from typing import List, Optional, Dict, Union, Any
from pydantic import BaseModel

RegexSpec = Union[str, int]
GrammarId = Union[str, int]
NodeId = int

class NodeProps(BaseModel):
    max_tokens: Optional[int]
    name: Optional[str]
    capture_name: Optional[str]

class _String(NodeProps):
    literal: str

class String(BaseModel):
    String: _String

class _Gen(NodeProps):
    body_rx: RegexSpec
    stop_rx: RegexSpec
    stop_capture_name: Optional[str]
    lazy: Optional[bool]
    temperature: Optional[float]

class Gen(BaseModel):
    Gen: _Gen

class _Lexeme(NodeProps):
    rx: RegexSpec
    contextual: Optional[bool]
    temperature: Optional[float]
    json_string: Optional[bool]
    json_allowed_escapes: Optional[str]
    json_raw: Optional[bool]

class Lexeme(BaseModel):
    Lexeme: _Lexeme

class _GenGrammar(NodeProps):
    grammar: GrammarId
    temperature: Optional[float]

class GenGrammar(BaseModel):
    GenGrammar: _GenGrammar

class _SpecialToken(NodeProps):
    token: str

class SpecialToken(BaseModel):
    SpecialToken: _SpecialToken

class _Select(NodeProps):
    among: List[GrammarId]

class Select(BaseModel):
    Select: _Select

class _Join(NodeProps):
    sequence: List[NodeId]

class Join(BaseModel):
    Join: _Join

Node = Union[String, Gen, Lexeme, GenGrammar, SpecialToken, Select, Join]

class GrammarWithLexer(BaseModel):
    name: Optional[str]
    nodes: List[Node]
    json_schema: Optional[Dict[str, Any]]
    lark_grammar: Optional[str]
    greedy_lexer: bool = False
    greedy_skip_rx: Optional[RegexSpec]
    contextual: Optional[bool]
    rx_nodes: List[Any] # TODO: Define this type
    allow_initial_skip: bool = False
    no_forcing: bool = False
    allow_invalid_utf8: bool = False

class TopLevelGrammar(BaseModel):
    grammars: List[GrammarWithLexer]
    max_tokens: Optional[int]
    test_trace: bool = False
