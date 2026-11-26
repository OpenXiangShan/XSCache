"""Check the post-clean XSPerf dump against the directed software oracle.

Usage: python3 src/test/python/check_queue_duplicate_observation.py <log> <result.json>
The Scala runner prints QOBS_EXPECT from its software queue, independently of
the hardware events. XSPerf dump includes its current cycle; QOBS_DUMP_EXTRA
accounts for the final idle cycle's empty occupancy sample.
"""
import json
from pathlib import Path
import re
import sys


def check(log):
    expected = {}
    actual = {}
    for name, value in re.findall(r"QOBS_EXPECT (\w+) (\d+)", log):
        assert name not in expected, name
        expected[name] = int(value)
    for name, value in re.findall(r"QOBS_DUMP_EXTRA (\w+) (\d+)", log):
        expected[name] += int(value)
    for name, value in re.findall(r": (pfobs_Q_test\w+),\s+(\d+)", log):
        assert name not in actual, name
        actual[name] = int(value)
    assert len(expected) == 150, len(expected)
    assert "QOBS_RESULT pass=true" in log
    assert "Assertion failed" not in log
    differences = {
        k: [expected.get(k), actual.get(k)]
        for k in expected.keys() | actual.keys()
        if expected.get(k) != actual.get(k)
    }
    for lane, entries in enumerate((4, 8)):
        prefix = f"pfobs_Q_test{lane}_"
        get = lambda event: actual[prefix + event]
        assert get("offered") == get("inserted")
        assert get("dedup_drop") == 0
        assert 0 < get("equivalent_hit") <= get("pa_hit") <= get("offered")
        assert 0 < get("overwrite") <= get("inserted")
        assert get("occupancy_samples") == sum(get(f"occupancy_{n}") for n in range(entries + 1))
        assert get("occupancy_sum") == sum(n * get(f"occupancy_{n}") for n in range(entries + 1))
        # The clean edge retained two warmup entries and the test drains all
        # entries before dump: final = initial + inserted - dequeued - overwrite.
        assert 0 == 2 + get("inserted") - get("dequeued") - get("overwrite")
        if lane == 1:
            for event in ("offered", "pa_hit", "equivalent_hit", "inserted", "overwrite", "dequeued", "dedup_drop"):
                assert get(event) == sum(get(f"src{source}_{event}") for source in range(16))
    return {
        "pass": not differences,
        "expected_count": len(expected),
        "actual_count": len(actual),
        "differences": differences,
        "result": re.findall(r"QOBS_RESULT.*", log),
        "occupancy_and_source_conservation": True,
    }


if __name__ == "__main__":
    result = check(Path(sys.argv[1]).read_text())
    Path(sys.argv[2]).write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))
    raise SystemExit(0 if result["pass"] else 1)
