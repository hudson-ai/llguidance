/// Runs the official JSON Schema Test Suite (draft2020-12) against our compiler
/// with ratchet-based regression detection.
///
/// Each test case gets a result category. Results are compared against a committed
/// baseline JSON file. CI fails on regressions (got worse) AND improvements (got
/// better — forces baseline update to keep it current).
///
/// Categories (ordered by "badness"):
///   pass                   - Instance result matches expectation
///   false_negative         - Instance was rejected but should have been accepted
///   skip_compile           - Schema uses unimplemented features
///   skip                   - Entire file or group skipped
///   compile_error_all_invalid - Schema failed to compile, all instances invalid
///   compile_error_valid    - Schema failed to compile, but has valid instances
///   false_positive         - Instance was accepted but should have been rejected
///
/// Run with: cargo test -p sample_parser --test test_json_schema_test_suite -- --nocapture
///
/// Future work: Add Draft 4/6/7/2019-09 test suites. Infrastructure supports it;
/// just needs `$schema` injection for older drafts that don't include it in test schemas.
/// Draft 4 is particularly valuable (FHIR, OpenAPI).
use serde::Deserialize;
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

mod common_lark_utils;
use common_lark_utils::json_schema_check;

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

/// Files we know we don't support (features not implemented in llguidance).
const SKIP_FILES: &[&str] = &[
    // Not implemented
    "not.json",
    "if-then-else.json",
    "dependentRequired.json",
    "dependentSchemas.json",
    "contains.json",
    "minContains.json",
    "maxContains.json",
    "unevaluatedItems.json",
    "unevaluatedProperties.json",
    "propertyNames.json",
    "uniqueItems.json",
    "dynamicRef.json",
    "vocabulary.json",
    "infinite-loop-detection.json",
    // Remote refs require a retriever
    "refRemote.json",
];

/// Groups we skip by description substring (e.g., features we don't handle).
const SKIP_GROUPS: &[&str] = &[
    // $anchor not fully supported in all contexts
    "Location-independent identifier",
    // we don't support $id changing resolution scope in nested schemas
    "$id inside an enum is not a real identifier",
];

/// Result categories ordered by badness (lower = better).
const CATEGORIES: &[&str] = &[
    "pass",
    "false_negative",
    "skip_compile",
    "skip",
    "compile_error_all_invalid",
    "compile_error_valid",
    "false_positive",
];

fn category_badness(cat: &str) -> usize {
    CATEGORIES.iter().position(|&c| c == cat).unwrap_or(usize::MAX)
}

fn ensure_test_suite() -> PathBuf {
    let local = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("JSON-Schema-Test-Suite");
    if local.join("tests").join("draft2020-12").exists() {
        return local;
    }
    let tmp = PathBuf::from("/tmp/JSON-Schema-Test-Suite");
    if tmp.join("tests").join("draft2020-12").exists() {
        return tmp;
    }
    // Clone it
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
    tmp
}

fn baseline_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("expected_json_schema_test_suite.json")
}

fn should_skip_file(filename: &str) -> bool {
    SKIP_FILES.contains(&filename)
}

fn should_skip_group(desc: &str) -> bool {
    SKIP_GROUPS.iter().any(|skip| desc.contains(skip))
}

/// Nested results: file → group → test → category
type Results = BTreeMap<String, BTreeMap<String, BTreeMap<String, String>>>;

/// Run all tests in a file, recording per-test-case results.
fn run_test_file(path: &Path, results: &mut Results) {
    let filename = path.file_name().unwrap().to_str().unwrap().to_string();

    let content = std::fs::read_to_string(path).unwrap();
    let groups: Vec<TestGroup> = serde_json::from_str(&content).unwrap();

    let file_results = results.entry(filename.clone()).or_default();

    for group in &groups {
        let group_results = file_results.entry(group.description.clone()).or_default();

        if should_skip_file(&filename) || should_skip_group(&group.description) {
            for test in &group.tests {
                group_results.insert(test.description.clone(), "skip".to_string());
            }
            continue;
        }

        // Try to compile the schema
        let lark_grammar = format!(
            r#"start: %json {}"#,
            serde_json::to_string(&group.schema).unwrap()
        );
        let parser_result = common_lark_utils::make_parser(&lark_grammar, true);

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

/// Flatten nested results into (test_path, category) pairs for comparison.
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

/// Compare current results against baseline. Returns (regressions, improvements, new, missing).
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

#[test]
fn json_schema_test_suite_draft2020_12() {
    let suite_root = ensure_test_suite();
    let suite_dir = suite_root.join("tests").join("draft2020-12");
    let mut results: Results = BTreeMap::new();

    // Collect JSON test files from main suite dir
    let mut files: Vec<PathBuf> = std::fs::read_dir(&suite_dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().map_or(false, |ext| ext == "json"))
        .collect();
    files.sort();

    for file in &files {
        run_test_file(file, &mut results);
    }

    // Also run optional format tests
    let format_dir = suite_dir.join("optional").join("format");
    if format_dir.exists() {
        let mut format_files: Vec<PathBuf> = std::fs::read_dir(&format_dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p.extension().map_or(false, |ext| ext == "json"))
            .collect();
        format_files.sort();
        for file in &format_files {
            run_test_file(file, &mut results);
        }
    }

    // Print per-category summary
    print_category_summary(&results);

    // Write current results for baseline generation
    let current_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("target")
        .join("json_schema_test_suite_results.json");
    let json = serde_json::to_string_pretty(&results).unwrap();
    std::fs::write(&current_path, &json).ok();
    eprintln!("\nCurrent results written to: {}", current_path.display());

    // Compare against baseline if it exists
    let baseline_file = baseline_path();
    if !baseline_file.exists() {
        eprintln!(
            "\nNo baseline found at {}",
            baseline_file.display()
        );
        eprintln!("To create one, copy the current results:");
        eprintln!(
            "  cp {} {}",
            current_path.display(),
            baseline_file.display()
        );
        // Don't fail — allow first run without baseline
        return;
    }

    let baseline_content = std::fs::read_to_string(&baseline_file).unwrap();
    let baseline: Results =
        serde_json::from_str(&baseline_content).expect("Failed to parse baseline JSON");

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
        eprintln!("\nBaseline mismatch. Update baseline with:");
        eprintln!(
            "  cp {} {}",
            current_path.display(),
            baseline_file.display()
        );
        panic!(
            "Baseline mismatch: {} regressions, {} improvements, {} new, {} missing",
            regressions.len(),
            improvements.len(),
            new_tests.len(),
            missing_tests.len()
        );
    }

    eprintln!("\nAll results match baseline. ✓");
}
