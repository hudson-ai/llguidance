#!/usr/bin/env python3

import sys
import json
import glob
import os
import random
import time
import resource

import xgrammar as xgr
import numpy as np
from transformers import AutoTokenizer, AutoConfig

output_path = "tmp/xgr/"


def time_us(prev: float) -> int:
    return int((time.monotonic() - prev) * 1000000)


def process_file(file: str):
    id = os.path.basename(file)
    output_name = output_path + id
    if os.path.exists(output_name):
        return None

    with open(output_name, "w") as f:
        f.write(json.dumps({ "pending_file": 1 }, indent=2))

    with open(file) as f:
        pos_data = json.loads(f.read())

    schema = json.dumps(pos_data["schema"])
    token_bitmask = xgr.allocate_token_bitmask(1, tokenizer_info.vocab_size)

    all_mask_us = []
    status = {
        "id": id,
        "ttfm_us": 0,
        "max_ttfm_us": 0,
        "masks_us": 0,
        "max_mask_us": 0,
        "num_tokens": 0,
        "num_tests": len(pos_data["tests"]),
        "all_mask_us": all_mask_us,
        "num_valid_tests": 0,
        "num_invalid_tests": 0,
    }

    try:
        t0 = time.monotonic()
        compiled_grammar = compiler.compile_json_schema(
            schema, any_whitespace=True, strict_mode=False
        )
        matcher = xgr.GrammarMatcher(compiled_grammar)
    except Exception as e:
        status["compile_error"] = repr(e)
        with open(output_name, "w") as f:
            f.write(json.dumps(status, indent=2))
        return status

    status["ttfm_us"] = time_us(t0)
    status["max_ttfm_us"] = status["ttfm_us"]

    masks_us = 0
    max_mask_us = 0
    num_tokens = 0

    for i, test in enumerate(pos_data["tests"]):
        instance = json.dumps(test["data"], indent=None)
        tokens = tokenizer.encode(instance, add_special_tokens=False)

        t1 = time.monotonic()
        accepted = True
        for tidx, t in enumerate(tokens):
            t2 = time.monotonic()
            matcher.fill_next_token_bitmask(token_bitmask)
            ok = matcher.accept_token(t)
            mask_time = time_us(t2)
            num_tokens += 1
            masks_us += mask_time
            all_mask_us.append(mask_time)
            if mask_time > max_mask_us:
                max_mask_us = mask_time
            if not ok:
                accepted = False
                break

        if accepted and not test["valid"]:
            status["validation_error"] = f"test #{i}: should reject but didn't"
        elif not accepted and test["valid"]:
            status["validation_error"] = f"test #{i}: should accept but didn't"
        else:
            if test["valid"]:
                status["num_valid_tests"] += 1
            else:
                status["num_invalid_tests"] += 1

    status["masks_us"] = masks_us
    status["max_mask_us"] = max_mask_us
    status["num_tokens"] = num_tokens

    with open(output_name, "w") as f:
        f.write(json.dumps(status, indent=2))
    return status

def main():
    global tokenizer_info, compiler, tokenizer

    limit_gb = 32
    limit_bytes = limit_gb * 1024 * 1024 * 1024
    resource.setrlimit(resource.RLIMIT_AS, (limit_bytes, limit_bytes))

    # Get tokenizer info
    model_id = "meta-llama/Llama-3.1-8B-Instruct"
    tokenizer = AutoTokenizer.from_pretrained(model_id)
    config = AutoConfig.from_pretrained(model_id)
    # This can be larger than tokenizer.vocab_size due to paddings
    full_vocab_size = config.vocab_size
    tokenizer_info = xgr.TokenizerInfo.from_huggingface(
        tokenizer, vocab_size=full_vocab_size
    )
    compiler = xgr.GrammarCompiler(tokenizer_info, max_threads=1)

    files = []
    for arg in sys.argv[1:]:
        if arg.endswith(".json"):
            files.append(arg)
        else:
            files.extend(glob.glob(arg + "/*.json"))
    print(len(files), file=sys.stderr)
    random.shuffle(files)

    os.makedirs(output_path, exist_ok=True)

    for f in files:
        print(f, file=sys.stderr)
        process_file(f)

main()