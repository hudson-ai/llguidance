/// Runs the official JSON Schema Test Suite (draft2020-12) against our compiler
/// with ratchet-based regression detection.
///
/// Each test case is compiled and run, then categorized by distance from correctness:
///   pass                      - Instance result matches expectation
///   false_negative            - Instance was rejected but should have been accepted
///   compile_error_all_invalid - Schema failed to compile, all instances invalid
///   skip_compile              - Schema failed to compile on unimplemented feature
///   compile_error_valid       - Schema failed to compile, but has valid instances
///   false_positive            - Instance was accepted but should have been rejected
///
/// Usage:
///   cargo run -p json_schema_test_suite --release -- --expected expected.json [test_suite_dir]
///
/// Future work: Add Draft 4/6/7/2019-09 test suites. Infrastructure supports it;
/// just needs `$schema` injection for older drafts that don't include it in test schemas.
/// Draft 4 is particularly valuable (FHIR, OpenAPI).
use anyhow::{bail, Result};
use llguidance::{
    api::{GrammarInit, TopLevelGrammar},
    TokenParser,
};
use sample_parser::{get_parser_factory, get_tok_env};
use serde::Deserialize;
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Deserialize)]
struct TestGroup {
    description: String,
    schema: Value,
    tests: Vec<TestCase>,
}

#[derive(Deserialize)]
struct TestCase {
    description: String,
    data: Value,
    valid: bool,
}

/// Result categories ordered by badness (lower = better).
const CATEGORIES: &[&str] = &[
    "pass",
    "false_negative",
    "compile_error_all_invalid",
    "skip_compile",
    "compile_error_valid",
    "false_positive",
];

fn category_badness(cat: &str) -> usize {
    CATEGORIES
        .iter()
        .position(|&c| c == cat)
        .unwrap_or(usize::MAX)
}

fn make_parser(lark: &str) -> anyhow::Result<TokenParser> {
    let grm = TopLevelGrammar::from_lark(lark.to_string());
    let mut parser = get_parser_factory().create_parser_from_init(
        GrammarInit::Serialized(grm),
        0, // quiet
        1, // quiet
    )?;
    parser.start_without_prompt();
    Ok(parser)
}

fn json_schema_check(schema: &Value, json_obj: &Value, expect_valid: bool) {
    let lark_grammar = format!(r#"start: %json {}"#, serde_json::to_string(schema).unwrap());
    let json_string = serde_json::to_string(json_obj).unwrap();
    let trie = get_tok_env().tok_trie();
    let tokens = get_tok_env().tokenize(&json_string);

    let mut p = make_parser(&lark_grammar).unwrap();

    for (i, tok) in tokens.iter().enumerate() {
        let m = p.compute_mask().unwrap();
        if m.is_allowed(*tok) {
            let n = p.consume_token(*tok).unwrap();
            for _ in 0..n {
                p.compute_mask().unwrap();
            }
        } else {
            let curr_tok_str = trie.token_dbg(*tok);
            assert!(
                !expect_valid,
                "Unexpected token: {curr_tok_str} at token index {i}",
            );
            return;
        }
    }

    assert_eq!(p.is_accepting(), expect_valid, "Final state mismatch");
}

fn ensure_test_suite(dir: Option<&str>) -> PathBuf {
    if let Some(d) = dir {
        let p = PathBuf::from(d);
        assert!(
            p.join("tests").join("draft2020-12").exists(),
            "No draft2020-12 tests found in {d}"
        );
        return p;
    }
    // Check common locations
    let candidates = [
        PathBuf::from("JSON-Schema-Test-Suite"),
        PathBuf::from("/tmp/JSON-Schema-Test-Suite"),
    ];
    for c in &candidates {
        if c.join("tests").join("draft2020-12").exists() {
            return c.clone();
        }
    }
    // Clone it
    let tmp = &candidates[1];
    eprintln!("Cloning JSON-Schema-Test-Suite...");
    let status = Command::new("git")
        .args([
            "clone",
            "--depth",
            "1",
            "https://github.com/json-schema-org/JSON-Schema-Test-Suite",
            tmp.to_str().unwrap(),
        ])
        .status()
        .expect("Failed to run git clone");
    assert!(status.success(), "git clone failed");
    tmp.clone()
}

/// Nested results: file → group → test → category
type Results = BTreeMap<String, BTreeMap<String, BTreeMap<String, String>>>;

/// Run all tests in a file, recording per-test-case results.
fn run_test_file(path: &Path, prefix: &str, results: &mut Results) {
    let filename = path.file_name().unwrap().to_str().unwrap();
    let stem = filename.strip_suffix(".json").unwrap_or(filename);
    let file_key = format!("{prefix}{stem}");

    let content = std::fs::read_to_string(path).unwrap();
    let groups: Vec<TestGroup> = serde_json::from_str(&content).unwrap();

    let file_results = results.entry(file_key).or_default();

    for group in &groups {
        let group_results = file_results.entry(group.description.clone()).or_default();

        let lark_grammar = format!(
            r#"start: %json {}"#,
            serde_json::to_string(&group.schema).unwrap()
        );
        let parser_result = make_parser(&lark_grammar);

        match parser_result {
            Err(e) => {
                let msg = format!("{e}");
                let is_unimplemented =
                    msg.contains("Unimplemented keys") || msg.contains("not supported");
                let has_valid = group.tests.iter().any(|t| t.valid);

                for test in &group.tests {
                    let category = if is_unimplemented {
                        "skip_compile"
                    } else if has_valid {
                        "compile_error_valid"
                    } else {
                        "compile_error_all_invalid"
                    };
                    group_results.insert(test.description.clone(), category.to_string());
                }
            }
            Ok(_) => {
                for test in &group.tests {
                    let result = std::panic::catch_unwind(|| {
                        json_schema_check(&group.schema, &test.data, test.valid);
                    });
                    let category = match result {
                        Ok(()) => "pass",
                        Err(_) => {
                            if test.valid {
                                "false_negative"
                            } else {
                                "false_positive"
                            }
                        }
                    };
                    group_results.insert(test.description.clone(), category.to_string());
                }
            }
        }
    }
}

fn flatten(results: &Results) -> BTreeMap<String, String> {
    let mut flat = BTreeMap::new();
    for (file, groups) in results {
        for (group, tests) in groups {
            for (test, cat) in tests {
                flat.insert(format!("{file} / {group} / {test}"), cat.clone());
            }
        }
    }
    flat
}

fn compare_results(
    current: &Results,
    baseline: &Results,
) -> (Vec<String>, Vec<String>, Vec<String>, Vec<String>) {
    let cur_flat = flatten(current);
    let base_flat = flatten(baseline);
    let mut regressions = Vec::new();
    let mut improvements = Vec::new();
    let mut new_tests = Vec::new();
    let mut missing_tests = Vec::new();

    for (test_id, cur_cat) in &cur_flat {
        match base_flat.get(test_id) {
            Some(base_cat) => {
                if cur_cat != base_cat {
                    let cur_bad = category_badness(cur_cat);
                    let base_bad = category_badness(base_cat);
                    if cur_bad > base_bad {
                        regressions.push(format!("{test_id}: {base_cat} → {cur_cat}"));
                    } else {
                        improvements.push(format!("{test_id}: {base_cat} → {cur_cat}"));
                    }
                }
            }
            None => {
                new_tests.push(format!("{test_id}: {cur_cat}"));
            }
        }
    }

    for (test_id, base_cat) in &base_flat {
        if !cur_flat.contains_key(test_id) {
            missing_tests.push(format!("{test_id}: was {base_cat}"));
        }
    }

    (regressions, improvements, new_tests, missing_tests)
}

fn print_category_summary(results: &Results) {
    let mut counts: BTreeMap<&str, usize> = BTreeMap::new();
    for cat in CATEGORIES {
        counts.insert(cat, 0);
    }
    let mut total = 0;
    for groups in results.values() {
        for tests in groups.values() {
            for cat in tests.values() {
                *counts.entry(cat.as_str()).or_insert(0) += 1;
                total += 1;
            }
        }
    }
    eprintln!("\n=== JSON Schema Test Suite (draft2020-12) ===");
    eprintln!("Total: {total}");
    for (cat, count) in &counts {
        if *count > 0 {
            eprintln!("  {cat:30} {count}");
        }
    }
}

fn main() -> Result<()> {
    // Suppress panic messages from catch_unwind (expected for false_negative/false_positive cases)
    std::panic::set_hook(Box::new(|_| {}));

    let args: Vec<String> = std::env::args().collect();

    let mut expected_path: Option<String> = None;
    let mut suite_dir: Option<String> = None;
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--expected" => {
                i += 1;
                expected_path = Some(args[i].clone());
            }
            arg if !arg.starts_with('-') => {
                suite_dir = Some(arg.to_string());
            }
            other => bail!("Unknown argument: {other}"),
        }
        i += 1;
    }

    let suite_root = ensure_test_suite(suite_dir.as_deref());
    let suite_dir = suite_root.join("tests").join("draft2020-12");
    let mut results: Results = BTreeMap::new();

    // Core tests
    let mut files: Vec<PathBuf> = std::fs::read_dir(&suite_dir)?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "json"))
        .collect();
    files.sort();
    for file in &files {
        run_test_file(file, "", &mut results);
    }

    // Optional format tests
    let format_dir = suite_dir.join("optional").join("format");
    if format_dir.exists() {
        let mut format_files: Vec<PathBuf> = std::fs::read_dir(&format_dir)?
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p.extension().is_some_and(|ext| ext == "json"))
            .collect();
        format_files.sort();
        for file in &format_files {
            run_test_file(file, "optional/format/", &mut results);
        }
    }

    print_category_summary(&results);

    // Compare against baseline if provided
    let Some(expected) = expected_path else {
        // No baseline — just print results
        let json = serde_json::to_string_pretty(&results)?;
        println!("{json}");
        return Ok(());
    };

    let baseline_file = PathBuf::from(&expected);
    if !baseline_file.exists() {
        eprintln!("\nNo baseline found at {expected}");
        eprintln!("Writing current results as new baseline.");
        let json = serde_json::to_string_pretty(&results)?;
        std::fs::write(&baseline_file, &json)?;
        eprintln!("Wrote {expected}");
        return Ok(());
    }

    let baseline_content = std::fs::read_to_string(&baseline_file)?;
    let baseline: Results = serde_json::from_str(&baseline_content)?;

    let (regressions, improvements, new_tests, missing_tests) =
        compare_results(&results, &baseline);

    let has_changes = !regressions.is_empty()
        || !improvements.is_empty()
        || !new_tests.is_empty()
        || !missing_tests.is_empty();

    if has_changes {
        if !regressions.is_empty() {
            eprintln!("\n--- REGRESSIONS ({}) ---", regressions.len());
            for r in &regressions {
                eprintln!("  {r}");
            }
        }
        if !improvements.is_empty() {
            eprintln!("\n--- IMPROVEMENTS ({}) ---", improvements.len());
            for i in &improvements {
                eprintln!("  {i}");
            }
        }
        if !new_tests.is_empty() {
            eprintln!("\n--- NEW TESTS ({}) ---", new_tests.len());
            for n in &new_tests {
                eprintln!("  {n}");
            }
        }
        if !missing_tests.is_empty() {
            eprintln!("\n--- MISSING TESTS ({}) ---", missing_tests.len());
            for m in &missing_tests {
                eprintln!("  {m}");
            }
        }
        bail!(
            "Baseline mismatch: {} regressions, {} improvements, {} new, {} missing",
            regressions.len(),
            improvements.len(),
            new_tests.len(),
            missing_tests.len()
        );
    }

    eprintln!("\nAll results match baseline. ✓");
    Ok(())
}
