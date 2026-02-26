"""Integration tests for Python literal output style.

Tests that grammars compiled with output_style="python" produce output that:
1. Is accepted by the grammar (via LLMatcher)
2. Can be parsed by ast.literal_eval
3. When round-tripped through json.dumps, validates against the original JSON schema
"""

import ast
import json
from typing import Any, Dict, List

import pytest
import jsonschema
from llguidance import LLMatcher, LLTokenizer, grammar_from

_tokenizer = None


def tokenizer() -> LLTokenizer:
    global _tokenizer
    if _tokenizer is None:
        _tokenizer = LLTokenizer("byte")
    return _tokenizer


def check_python_grammar(grm: str, passing: List[str], failing: List[str]) -> None:
    """Check that passing strings are accepted and failing strings are rejected."""
    for s in passing:
        m = LLMatcher(tokenizer(), grm, log_level=0)
        assert not m.is_error(), f"Grammar error: {m}"
        tokens = tokenizer().tokenize_str(s)
        for t in tokens:
            mask = m.compute_logit_bias()
            assert mask[t] != 0, f"Token rejected in passing string {s!r}"
            m.consume_token(t)
        assert m.is_accepting(), f"String not accepted: {s!r}"

    for s in failing:
        m = LLMatcher(tokenizer(), grm, log_level=0)
        assert not m.is_error()
        tokens = tokenizer().tokenize_str(s)
        rejected = False
        for t in tokens:
            mask = m.compute_logit_bias()
            if mask[t] == 0:
                rejected = True
                break
            m.consume_token(t)
        if not rejected:
            assert not m.is_accepting(), f"String should have been rejected: {s!r}"


def validate_python_output(output: str, schema: Dict[str, Any]) -> None:
    """Validate that output is a valid Python literal that satisfies the JSON schema."""
    # Step 1: Parse with ast.literal_eval
    parsed = ast.literal_eval(output)

    # Step 2: Convert to JSON and validate against schema
    json_str = json.dumps(parsed)
    json_value = json.loads(json_str)
    jsonschema.validate(instance=json_value, schema=schema)


def flexible_grammar(schema: Any) -> str:
    """Compile a schema with python output_style and flexible quote style."""
    if isinstance(schema, str):
        schema = json.loads(schema)
    return LLMatcher.grammar_from_json_schema(
        schema,
        overrides={"output_style": "python", "python_quote_style": "flexible"},
    )


# === Task 6.1: Basic type tests with ast.literal_eval ===


class TestPythonBooleans:
    def test_true_accepted(self):
        grm = grammar_from("python", '{"type": "boolean"}')
        check_python_grammar(grm, ["True", "False"], ["true", "false", "none", "None"])

    def test_const_true(self):
        grm = grammar_from("python", '{"const": true}')
        check_python_grammar(grm, ["True"], ["False", "true"])

    def test_const_false(self):
        grm = grammar_from("python", '{"const": false}')
        check_python_grammar(grm, ["False"], ["True", "false"])

    def test_literal_eval_true(self):
        output = "True"
        parsed = ast.literal_eval(output)
        assert parsed is True
        assert isinstance(parsed, bool)

    def test_literal_eval_false(self):
        output = "False"
        parsed = ast.literal_eval(output)
        assert parsed is False
        assert isinstance(parsed, bool)


class TestPythonNull:
    def test_none_accepted(self):
        grm = grammar_from("python", '{"type": "null"}')
        check_python_grammar(grm, ["None"], ["null", "Null", "none"])

    def test_literal_eval_none(self):
        output = "None"
        parsed = ast.literal_eval(output)
        assert parsed is None


class TestPythonNumbers:
    def test_integer(self):
        grm = grammar_from("python", '{"type": "integer"}')
        check_python_grammar(grm, ["0", "42", "-7"], [])

    def test_number(self):
        grm = grammar_from("python", '{"type": "number"}')
        check_python_grammar(grm, ["0", "3.14", "-2.5"], [])

    def test_literal_eval_integer(self):
        parsed = ast.literal_eval("42")
        assert parsed == 42
        assert isinstance(parsed, int)

    def test_literal_eval_float(self):
        parsed = ast.literal_eval("3.14")
        assert parsed == 3.14
        assert isinstance(parsed, float)


class TestPythonStrings:
    def test_default_double_quoted(self):
        """Default (double) style: only double-quoted strings accepted."""
        grm = grammar_from("python", '{"type": "string"}')
        check_python_grammar(grm, ['"hello"'], ["'hello'"])

    def test_flexible_both_quotes(self):
        """Flexible style: both single and double quoted accepted."""
        grm = flexible_grammar('{"type": "string"}')
        check_python_grammar(grm, ["'hello'", '"hello"'], [])

    def test_empty_strings(self):
        grm = grammar_from("python", '{"type": "string"}')
        check_python_grammar(grm, ['""'], ["''"])

    def test_double_quote_contains_escaped_double_quote(self):
        """Double-quoted string containing escaped double-quote."""
        grm = grammar_from("python", '{"type": "string"}')
        check_python_grammar(grm, ['"he said \\"hi\\""'], [])
        validate_python_output('"he said \\"hi\\""', {"type": "string"})

    def test_unescaped_single_quote_in_double_quoted(self):
        r"""Double-quoted string with unescaped single-quote: "it's" """
        grm = grammar_from("python", '{"type": "string"}')
        check_python_grammar(grm, ['"it\'s"'], [])
        validate_python_output('"it\'s"', {"type": "string"})

    def test_flexible_double_quote_contains_single_quote(self):
        """Flexible mode: double-quoted string containing unescaped single-quote."""
        grm = flexible_grammar('{"type": "string"}')
        check_python_grammar(grm, ['"she said \'hi\'"'], [])
        validate_python_output('"she said \'hi\'"', {"type": "string"})

    def test_flexible_escaped_double_quote_in_double_quoted(self):
        r"""Flexible mode: double-quoted string with escaped double-quote."""
        grm = flexible_grammar('{"type": "string"}')
        check_python_grammar(grm, ['"it\\"s"'], [])
        validate_python_output('"it\\"s"', {"type": "string"})

    def test_escaped_backslash(self):
        r"""String with escaped backslash: "back\\slash" """
        grm = grammar_from("python", '{"type": "string"}')
        check_python_grammar(grm, ['"back\\\\slash"'], [])
        validate_python_output('"back\\\\slash"', {"type": "string"})

    def test_escaped_newline(self):
        r"""String with escaped newline: "new\nline" """
        grm = grammar_from("python", '{"type": "string"}')
        check_python_grammar(grm, ['"new\\nline"'], [])
        validate_python_output('"new\\nline"', {"type": "string"})

    def test_string_with_spaces(self):
        grm = grammar_from("python", '{"type": "string"}')
        check_python_grammar(grm, ['"abc def"'], ["'abc def'"])

    def test_flexible_string_with_spaces(self):
        grm = flexible_grammar('{"type": "string"}')
        check_python_grammar(grm, ["'abc def'", '"abc def"'], [])

    def test_literal_eval_single_quoted(self):
        output = "'hello world'"
        parsed = ast.literal_eval(output)
        assert parsed == "hello world"
        assert isinstance(parsed, str)

    def test_literal_eval_double_quoted(self):
        output = '"hello world"'
        parsed = ast.literal_eval(output)
        assert parsed == "hello world"
        assert isinstance(parsed, str)


# === Task 6.2: Round-trip validation tests ===


class TestRoundTrip:
    def test_boolean_roundtrip(self):
        schema = {"type": "boolean"}
        validate_python_output("True", schema)
        validate_python_output("False", schema)

    def test_null_roundtrip(self):
        schema = {"type": "null"}
        validate_python_output("None", schema)

    def test_integer_roundtrip(self):
        schema = {"type": "integer", "minimum": 0, "maximum": 100}
        validate_python_output("42", schema)

    def test_number_roundtrip(self):
        schema = {"type": "number"}
        validate_python_output("3.14", schema)

    def test_string_roundtrip(self):
        schema = {"type": "string"}
        validate_python_output("'hello'", schema)
        validate_python_output('"hello"', schema)

    def test_array_roundtrip(self):
        schema = {"type": "array", "items": {"type": "integer"}}
        validate_python_output("[1, 2, 3]", schema)

    def test_object_roundtrip(self):
        schema = {
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "age": {"type": "integer"},
            },
            "required": ["name", "age"],
        }
        validate_python_output('{"name": "Alice", "age": 30}', schema)

    def test_object_with_boolean_and_null_roundtrip(self):
        schema = {
            "type": "object",
            "properties": {
                "active": {"type": "boolean"},
                "deleted": {"type": "null"},
            },
            "required": ["active", "deleted"],
        }
        validate_python_output('{"active": True, "deleted": None}', schema)


# === Task 6.3: Complex schema tests ===


class TestComplexSchemas:
    def test_nested_object_with_array(self):
        schema = {
            "type": "object",
            "properties": {
                "items": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "flag": {"type": "boolean"},
                            "value": {"type": "integer"},
                        },
                        "required": ["flag", "value"],
                    },
                }
            },
            "required": ["items"],
        }
        grm = grammar_from("python", json.dumps(schema))
        check_python_grammar(
            grm,
            ['{"items": [{"flag": True, "value": 1}]}'],
            [],
        )
        validate_python_output(
            '{"items": [{"flag": True, "value": 1}]}', schema
        )

    def test_enum_mixed_types(self):
        schema = {"enum": [1, "hello", True, None]}
        grm = grammar_from("python", json.dumps(schema))
        check_python_grammar(
            grm,
            ["1", '"hello"', "True", "None"],
            ["false", "null", "2"],
        )
        validate_python_output("1", schema)
        validate_python_output('"hello"', schema)
        validate_python_output("True", schema)
        validate_python_output("None", schema)

    def test_anyof_string_or_integer(self):
        schema = {"anyOf": [{"type": "string"}, {"type": "integer"}]}
        grm = grammar_from("python", json.dumps(schema))
        check_python_grammar(grm, ['"hello"', "42"], [])
        validate_python_output('"hello"', schema)
        validate_python_output("42", schema)

    def test_object_grammar_accepts_python_literals(self):
        """Full end-to-end: grammar_from("python", ...) + check acceptance + ast.literal_eval + jsonschema validate."""
        schema = {
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "age": {"type": "integer"},
                "active": {"type": "boolean"},
                "notes": {"type": "null"},
            },
            "required": ["name", "age", "active", "notes"],
            "additionalProperties": False,
        }
        grm = grammar_from("python", json.dumps(schema))
        test_output = '{"name": "Alice", "age": 30, "active": True, "notes": None}'
        check_python_grammar(grm, [test_output], [])
        validate_python_output(test_output, schema)

    def test_pattern_simple(self):
        """Pattern with a simple regex - digits only."""
        schema = {"type": "string", "pattern": "^\\d{3}-\\d{4}$"}
        grm = grammar_from("python", json.dumps(schema))
        check_python_grammar(grm, ['"123-4567"'], ['"abc-defg"', '"12-345"'])
        validate_python_output('"123-4567"', schema)

    def test_pattern_with_min_max_length(self):
        """Pattern + length constraints."""
        schema = {"type": "string", "pattern": "^[a-z]+$", "minLength": 2, "maxLength": 5}
        grm = grammar_from("python", json.dumps(schema))
        check_python_grammar(grm, ['"ab"', '"abcde"'], ['"a"', '"abcdef"'])
        validate_python_output('"abc"', schema)

    def test_pattern_allows_single_quotes(self):
        r"""Pattern that matches strings containing literal single-quotes.

        The regex [a-z']+ allows single-quote chars in the string value.
        With default double-quote style, no escaping is needed for '.
        """
        schema = {"type": "string", "pattern": "^[a-z']+$"}
        grm = grammar_from("python", json.dumps(schema))
        # double-quoted with unescaped ': "it's"
        check_python_grammar(grm, ['"it\'s"'], [])
        validate_python_output('"it\'s"', schema)

    def test_pattern_allows_single_quotes_flexible(self):
        r"""Flexible mode: pattern allowing single-quotes can use double-quoting."""
        schema = {"type": "string", "pattern": "^[a-z']+$"}
        grm = flexible_grammar(schema)
        # double-quoted avoids escaping
        check_python_grammar(grm, ["\"it's\""], [])
        validate_python_output("\"it's\"", schema)

    def test_pattern_allows_double_quotes(self):
        r"""Pattern that matches strings containing literal double-quotes.

        The regex [a-z"]+ allows double-quote chars in the string value.
        With default double-quote style, \" escapes are needed.
        """
        schema = {"type": "string", "pattern": "^[a-z\"]+$"}
        grm = grammar_from("python", json.dumps(schema))
        # double-quoted: escaping needed for "
        check_python_grammar(grm, ['"say\\"hi"'], [])
        validate_python_output('"say\\"hi"', schema)

    def test_pattern_with_escaped_backslash(self):
        r"""Pattern that matches strings containing literal backslashes.

        JSON schema pattern \\\\  matches a literal backslash in the string.
        The Python-quoted output needs \\  inside the quotes for each real backslash.
        Round-trip: python literal 'a\\b' -> ast.literal_eval -> "a\b" -> json.dumps -> "a\\b"
        """
        schema = {"type": "string", "pattern": "^[a-z\\\\]+$"}
        grm = grammar_from("python", json.dumps(schema))
        check_python_grammar(grm, ['"a\\\\b"'], [])
        validate_python_output('"a\\\\b"', schema)

    def test_pattern_with_newline(self):
        r"""Pattern that matches strings containing literal newlines.

        JSON schema pattern \n matches a newline char.
        The Python-quoted output needs \n inside the quotes.
        Round-trip: python literal 'a\nb' -> ast.literal_eval -> "a\nb" -> json.dumps -> "a\nb"
        """
        schema = {"type": "string", "pattern": "^[a-z\\n]+$"}
        grm = grammar_from("python", json.dumps(schema))
        check_python_grammar(grm, ['"a\\nb"'], [])
        validate_python_output('"a\\nb"', schema)

    def test_pattern_in_object_property(self):
        """Pattern-constrained string inside an object - full pipeline test."""
        schema = {
            "type": "object",
            "properties": {
                "email": {
                    "type": "string",
                    "pattern": "^[a-z]+@[a-z]+\\.[a-z]+$",
                },
                "code": {
                    "type": "string",
                    "pattern": "^[A-Z]{2}\\d{3}$",
                },
            },
            "required": ["email", "code"],
            "additionalProperties": False,
        }
        grm = grammar_from("python", json.dumps(schema))
        test_output = '{"email": "foo@bar.com", "code": "AB123"}'
        check_python_grammar(grm, [test_output], [])
        validate_python_output(test_output, schema)

    def test_pattern_with_both_quotes_and_backslash(self):
        r"""Pattern allowing single-quotes, double-quotes, AND backslashes.

        This is the hardest case: the string value can contain all three
        problematic characters. The grammar must pick a quoting style and
        escape appropriately. Round-trip must still work.
        """
        # Allow a-z, ', ", and \ in any combination
        schema = {"type": "string", "pattern": "^[a-z'\"\\\\]+$"}
        grm = grammar_from("python", json.dumps(schema))
        # A string with just letters works in either quote style
        check_python_grammar(grm, ['"hello"'], [])
        validate_python_output('"hello"', schema)

    def test_x_guidance_output_style(self):
        """Test that x-guidance with output_style=python works."""
        schema = {
            "type": "object",
            "properties": {"x": {"type": "boolean"}},
            "required": ["x"],
            "additionalProperties": False,
        }
        grm = LLMatcher.grammar_from_json_schema(
            schema, overrides={"output_style": "python"}
        )
        check_python_grammar(grm, ['{"x": True}', '{"x": False}'], [])
        validate_python_output('{"x": True}', schema)


class TestPythonQuoteStyle:
    """Tests for the python_quote_style configuration."""

    def test_double_style_general_string(self):
        """Double style: general strings use double quotes only."""
        grm = LLMatcher.grammar_from_json_schema(
            {"type": "string"},
            overrides={"output_style": "python", "python_quote_style": "double"},
        )
        check_python_grammar(grm, ['"hello"'], ["'hello'"])

    def test_double_style_object_keys(self):
        """Double style: dict keys prefer double quotes."""
        schema = {
            "type": "object",
            "properties": {"name": {"type": "boolean"}},
            "required": ["name"],
            "additionalProperties": False,
        }
        grm = LLMatcher.grammar_from_json_schema(
            schema,
            overrides={"output_style": "python", "python_quote_style": "double"},
        )
        check_python_grammar(grm, ['{"name": True}'], [])

    def test_flexible_style_both_accepted(self):
        """Flexible style: both quote styles accepted for general strings."""
        grm = flexible_grammar({"type": "string"})
        check_python_grammar(grm, ["'hello'", '"hello"'], [])

    def test_const_string_deterministic_double(self):
        """Const string without quotes: uses preferred double quote."""
        schema = {"const": "hello"}
        grm = grammar_from("python", json.dumps(schema))
        check_python_grammar(grm, ['"hello"'], ["'hello'"])

    def test_const_string_with_single_quote_uses_double(self):
        """Const string containing ' but not ": uses double to avoid escape (Q003)."""
        schema = {"const": "it's"}
        grm = grammar_from("python", json.dumps(schema))
        check_python_grammar(grm, ["\"it's\""], ["'it\\'s'"])

    def test_const_string_with_double_quote_uses_single(self):
        """Const string containing " but not ': uses single to avoid escape (Q003)."""
        schema = {"const": 'say "hi"'}
        grm = grammar_from("python", json.dumps(schema))
        check_python_grammar(grm, ['\'say "hi"\''], [])

    def test_const_string_with_both_quotes_uses_preferred(self):
        """Const string containing both ' and ": uses preferred double style."""
        schema = {"const": "it's a \"test\""}
        grm = grammar_from("python", json.dumps(schema))
        # Double is preferred when both are present
        check_python_grammar(grm, ['"it\'s a \\"test\\""'], [])

    def test_enum_strings_deterministic(self):
        """Enum values are each quoted deterministically."""
        schema = {"enum": ["hello", "it's", 'say "hi"']}
        grm = grammar_from("python", json.dumps(schema))
        # "hello" → "hello" (preferred double)
        check_python_grammar(grm, ['"hello"'], [])
        # "it's" → "it's" (double, no conflict)
        check_python_grammar(grm, ["\"it's\""], [])
        # 'say "hi"' → 'say "hi"' (single, Q003 avoidance)
        check_python_grammar(grm, ['\'say "hi"\''], [])

    def test_x_guidance_python_quote_style(self):
        """x-guidance with python_quote_style works."""
        grm = LLMatcher.grammar_from_json_schema(
            {"type": "string"},
            overrides={"output_style": "python", "python_quote_style": "flexible"},
        )
        check_python_grammar(grm, ["'hello'", '"hello"'], [])
