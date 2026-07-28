#!/usr/bin/env python3
"""Generate a single-line text file for editor performance testing.

The default output is 500 KB of space-separated words on one line with no
newline at all, which exercises the long-line paths in the renderer (base
token budgeting, wrapping, grapheme segmentation) and in the input path
(backward scan to line start on every keystroke).

Usage:
    scripts/gen-single-line-bench.py [--bytes 500000] [--out PATH]
"""

import argparse
import random

WORDS = [
    "fresh", "buffer", "token", "render", "viewport", "cursor", "piece", "tree",
    "wrap", "grapheme", "layout", "segment", "offset", "column", "insert",
    "delete", "scroll", "frame", "cache", "line", "width", "measure", "iterate",
    "span", "trace", "alloc", "vector", "slice", "range", "index",
]


def build_line(size: int, seed: int) -> str:
    rng = random.Random(seed)
    parts = []
    length = 0
    while length < size:
        word = rng.choice(WORDS)
        parts.append(word)
        length += len(word) + 1
    line = " ".join(parts)[:size]
    # The slice can land mid-word and come up short; pad to an exact size.
    return line + "x" * (size - len(line))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bytes", type=int, default=500_000, help="file size in bytes")
    parser.add_argument("--seed", type=int, default=1337, help="RNG seed")
    parser.add_argument(
        "--out", default="bench-single-line-500k.txt", help="output path"
    )
    args = parser.parse_args()

    line = build_line(args.bytes, args.seed)
    with open(args.out, "w", encoding="utf-8") as handle:
        handle.write(line)
    print(f"{args.out}: {len(line)} bytes, 1 line, no trailing newline")


if __name__ == "__main__":
    main()
