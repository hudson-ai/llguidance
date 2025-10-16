mod common_lark_utils;
use common_lark_utils::lark_str_test;
use serde_json::json;

#[test]
fn test_simple_separators() {
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
    // Ok
    lark_str_test(&lark, true, r#"{"a": 1, "b": 2}"#, true);
    // Bad
    lark_str_test(&lark, false, r#"{"a":1,"b":2}"#, true);
    lark_str_test(&lark, false, r#"{"a"  :  1 , "b":2}"#, true);
    lark_str_test(&lark, false, r#"{"a":1 ,  "b":2}"#, true);
    lark_str_test(&lark, false, r#"{"a":1,"b":  2}"#, true);
    lark_str_test(&lark, false, r#"{"a":1,   "b":2}"#, true);
    lark_str_test(&lark, false, r#"{"a":1,"b":    2}"#, true);
}

#[test]
fn test_pattern_separators() {
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
    // Ok
    lark_str_test(&lark, true, r#"{"a": 1, "b": 2}"#, true);
    lark_str_test(&lark, true, r#"{"a":1,"b":2}"#, true);
    lark_str_test(&lark, true, r#"{"a"  :  1 , "b":2}"#, true);
    lark_str_test(&lark, true, r#"{"a":1 ,  "b":2}"#, true);
    lark_str_test(&lark, true, r#"{"a":1,"b":  2}"#, true);
    // Bad
    lark_str_test(&lark, false, r#"{"a":1,   "b":2}"#, true);
    lark_str_test(&lark, false, r#"{"a":1,"b":    2}"#, true);
}

#[test]
fn test_flexible_separators() {
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
    // Ok
    lark_str_test(&lark, true, r#"{"a": 1, "b": 2}"#, true);
    lark_str_test(&lark, true, r#"{"a":1,"b":2}"#, true);
    lark_str_test(&lark, true, r#"{"a"  :  1 , "b":2}"#, true);
    lark_str_test(&lark, true, r#"{"a":1 ,  "b":2}"#, true);
    lark_str_test(&lark, true, r#"{"a":1,"b":  2}"#, true);
    lark_str_test(&lark, true, r#"{"a":1,   "b":2}"#, true);
    lark_str_test(&lark, true, r#"{"a":1,"b":    2}"#, true);
}
