#!/usr/bin/env python3

import json
import glob

output_path = "tmp/xgr/"


class Stats:
    def __init__(self) -> None:
        self.ttfm_us = 0
        self.max_ttfm_us = 0
        self.masks_us = 0
        self.max_mask_us = 0
        self.num_tokens = 0
        self.num_schemas = 0
        self.num_schemas_ok = 0
        self.num_compilation_errors = 0
        self.num_validation_errors = 0
        self.num_tests = 0
        self.num_valid_tests = 0
        self.num_invalid_tests = 0

def log_fraction_plot(times: list[int]):
    times.sort()
    cutoff = 1
    mult = 1.3
    count = 0
    csv = "cutoff time,frac left\n"
    total = len(times)
    for t in times:
        while t > cutoff:
            csv += f"{cutoff/1000.0},{(total - count)/total}\n"
            cutoff = int(cutoff * mult) + 1
        count += 1
    return csv

def main():
    files = glob.glob(output_path + "*.json")
    files = sorted(files)
    stats = Stats()
    ttfm_us = []
    all_masks_us = []
    for f in files:
        with open(f) as f:
            data = json.load(f)
        if "num_tests" not in data:
            continue
        stats.num_schemas += 1
        stats.num_tests += data["num_tests"]
        if "compile_error" in data:
            stats.num_compilation_errors += 1
        else:
            stats.ttfm_us += data["ttfm_us"]
            ttfm_us.append(data["ttfm_us"])
            stats.max_ttfm_us = max(data["max_ttfm_us"], stats.max_ttfm_us)
            stats.masks_us += data["masks_us"]
            stats.max_mask_us = max(data["max_mask_us"], stats.max_mask_us)
            stats.num_tokens += data["num_tokens"]
            if "validation_error" in data:
                stats.num_validation_errors += 1
            else:
                stats.num_schemas_ok += 1
            stats.num_valid_tests += data["num_valid_tests"]
            stats.num_invalid_tests += data["num_invalid_tests"]
            all_masks_us.extend(data["all_mask_us"])
    print(json.dumps(stats.__dict__, indent=2))
    with open("tmp/xgr_ttfm_us.csv", "w") as f:
        f.write(log_fraction_plot(ttfm_us))
    with open("tmp/xgr_masks_us.csv", "w") as f:
        f.write(log_fraction_plot(all_masks_us))
    


main()
