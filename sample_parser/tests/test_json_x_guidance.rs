mod common_lark_utils;
use common_lark_utils::lark_str_test;
use rstest::rstest;
use serde_json::json;

#[rstest]
// Ok
#[case::with_spaces(r#"{"a": 1, "b": 2}"#, true)]
// Bad
#[case::no_spaces(r#"{"a":1,"b":2}"#, false)]
#[case::two_spaces_around_colon(r#"{"a"  :  1 , "b":2}"#, false)]
#[case::spaces_before_comma(r#"{"a":1 ,  "b":2}"#, false)]
#[case::two_spaces_after_colon(r#"{"a":1,"b":  2}"#, false)]
#[case::three_spaces_after_comma(r#"{"a":1,   "b":2}"#, false)]
#[case::four_spaces_after_colon(r#"{"a":1,"b":    2}"#, false)]
fn test_simple_separators(#[case] input: &str, #[case] should_succeed: bool) {
    let options = json!({
        "item_separator": ", ",
        "key_separator": ": ",
        "whitespace_flexible": false,
    });
    let lark = format!(
        r#"
        start: %json {{
            "x-guidance": {options}
        }}
    "#
    );
    lark_str_test(&lark, should_succeed, input, true);
}

#[rstest]
// Ok
#[case::with_spaces(r#"{"a": 1, "b": 2}"#, true)]
#[case::no_spaces(r#"{"a":1,"b":2}"#, true)]
#[case::two_spaces_around_colon(r#"{"a"  :  1 , "b":2}"#, true)]
#[case::spaces_before_comma(r#"{"a":1 ,  "b":2}"#, true)]
#[case::two_spaces_after_colon(r#"{"a":1,"b":  2}"#, true)]
// Bad
#[case::three_spaces_after_comma(r#"{"a":1,   "b":2}"#, false)]
#[case::four_spaces_after_colon(r#"{"a":1,"b":    2}"#, false)]
fn test_pattern_separators(#[case] input: &str, #[case] should_succeed: bool) {
    let options = json!({
        "item_separator": r"\s{0,2},\s{0,2}",
        "key_separator": r"\s{0,2}:\s{0,2}",
        "whitespace_flexible": false,
    });
    let lark = format!(
        r#"
        start: %json {{
            "x-guidance": {options}
        }}
    "#
    );
    lark_str_test(&lark, should_succeed, input, true);
}

#[rstest]
// Ok
#[case::with_spaces(r#"{"a": 1, "b": 2}"#, true)]
#[case::no_spaces(r#"{"a":1,"b":2}"#, true)]
#[case::two_spaces_around_colon(r#"{"a"  :  1 , "b":2}"#, true)]
#[case::spaces_before_comma(r#"{"a":1 ,  "b":2}"#, true)]
#[case::two_spaces_after_colon(r#"{"a":1,"b":  2}"#, true)]
#[case::three_spaces_after_comma(r#"{"a":1,   "b":2}"#, true)]
#[case::four_spaces_after_colon(r#"{"a":1,"b":    2}"#, true)]
fn test_flexible_separators(#[case] input: &str, #[case] should_succeed: bool) {
    let options = json!({
        "item_separator": r",",
        "key_separator": r":",
        "whitespace_flexible": true,
    });
    let lark = format!(
        r#"
        start: %json {{
            "x-guidance": {options}
        }}
    "#
    );
    lark_str_test(&lark, should_succeed, input, true);
}
