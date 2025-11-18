#!/usr/bin/env python3
"""
Parse cargo flamegraph SVG output and generate a sorted summary of hottest codepaths.

Usage:
    python parse_flamegraph.py <flamegraph.svg> [options]

Options:
    --top N         Show top N entries (default: 50)
    --min-percent P Minimum percentage threshold (default: 0.0)
    --group-by STR  Group by: 'function', 'module', 'crate' (default: function)
    --demangle      Simplify Rust/C++ symbol names
"""

import re
import sys
import argparse
from collections import defaultdict
from html import unescape


def parse_title(title_content: str) -> tuple[str, int, float] | None:
    """
    Parse a flamegraph title element content.

    Returns (function_name, samples, percentage) or None if parsing fails.
    """
    # Match pattern: "function_name (N samples, X.XX%)" or "(N,NNN samples, X.XX%)"
    # The samples can have commas as thousand separators
    match = re.match(
        r'^(.+?)\s+\(([0-9,]+)\s+samples?,\s+([0-9.]+)%\)$',
        title_content.strip()
    )
    if not match:
        return None

    func_name = match.group(1)
    samples_str = match.group(2).replace(',', '')
    percentage_str = match.group(3)

    try:
        samples = int(samples_str)
        percentage = float(percentage_str)
    except ValueError:
        return None

    return (func_name, samples, percentage)


def extract_titles_from_svg(svg_path: str) -> list[tuple[str, int, float]]:
    """
    Extract all title elements from the SVG file.

    Returns list of (function_name, samples, percentage) tuples.
    """
    with open(svg_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Find all <title>...</title> elements
    title_pattern = re.compile(r'<title>([^<]+)</title>')
    matches = title_pattern.findall(content)

    results = []
    for title_content in matches:
        # Unescape HTML entities like &lt; &gt; etc.
        unescaped = unescape(title_content)
        parsed = parse_title(unescaped)
        if parsed:
            results.append(parsed)

    return results


def demangle_name(name: str) -> str:
    """
    Simplify Rust/C++ symbol names for readability.

    - Remove template parameters
    - Simplify common patterns
    """
    # Remove template parameters (angle brackets and their contents)
    # Handle nested templates by repeatedly removing innermost
    prev = None
    result = name
    while prev != result:
        prev = result
        result = re.sub(r'<[^<>]*>', '', result)

    # Remove common noise
    result = re.sub(r'\s+', ' ', result)  # Collapse whitespace
    result = result.strip()

    return result


def extract_module(name: str) -> str:
    """Extract the module/namespace path from a function name."""
    # For Rust: foo::bar::baz -> foo::bar
    # For C++: foo::bar::baz -> foo::bar
    parts = name.split('::')
    if len(parts) > 1:
        return '::'.join(parts[:-1])
    return name


def extract_crate(name: str) -> str:
    """Extract the top-level crate/namespace from a function name."""
    # For Rust: foo::bar::baz -> foo
    # Also handle things like v8::internal::... -> v8
    parts = name.split('::')
    if parts:
        return parts[0]
    return name


def group_entries(
    entries: list[tuple[str, int, float]],
    group_by: str,
    demangle: bool
) -> dict[str, tuple[int, float]]:
    """
    Group entries by function, module, or crate.

    Returns dict mapping group key to (total_samples, max_percentage).
    """
    groups: dict[str, tuple[int, float]] = defaultdict(lambda: (0, 0.0))

    for func_name, samples, percentage in entries:
        if demangle:
            func_name = demangle_name(func_name)

        if group_by == 'module':
            key = extract_module(func_name)
        elif group_by == 'crate':
            key = extract_crate(func_name)
        else:  # function
            key = func_name

        current_samples, current_max_pct = groups[key]
        groups[key] = (
            current_samples + samples,
            max(current_max_pct, percentage)
        )

    return dict(groups)


def format_output(
    grouped: dict[str, tuple[int, float]],
    top_n: int,
    min_percent: float,
    sort_by: str = 'samples'
) -> str:
    """
    Format the grouped entries as plain text output.

    Returns formatted string.
    """
    # Filter by minimum percentage
    filtered = [
        (name, samples, pct)
        for name, (samples, pct) in grouped.items()
        if pct >= min_percent
    ]

    # Sort by samples (descending)
    if sort_by == 'samples':
        filtered.sort(key=lambda x: x[1], reverse=True)
    else:  # percentage
        filtered.sort(key=lambda x: x[2], reverse=True)

    # Take top N
    filtered = filtered[:top_n]

    if not filtered:
        return "No entries found matching criteria."

    # Calculate column widths
    max_samples = max(e[1] for e in filtered)
    samples_width = len(f"{max_samples:,}")

    lines = []
    lines.append(f"{'Samples':>{samples_width}}  {'%':>6}  Function/Path")
    lines.append('-' * (samples_width + 2 + 6 + 2 + 50))

    for name, samples, pct in filtered:
        lines.append(f"{samples:>{samples_width},}  {pct:>5.2f}%  {name}")

    return '\n'.join(lines)


def main():
    parser = argparse.ArgumentParser(
        description='Parse cargo flamegraph SVG and output sorted hottest codepaths.'
    )
    parser.add_argument(
        'svg_file',
        help='Path to the flamegraph SVG file'
    )
    parser.add_argument(
        '--top', '-n',
        type=int,
        default=50,
        help='Show top N entries (default: 50)'
    )
    parser.add_argument(
        '--min-percent', '-m',
        type=float,
        default=0.0,
        help='Minimum percentage threshold (default: 0.0)'
    )
    parser.add_argument(
        '--group-by', '-g',
        choices=['function', 'module', 'crate'],
        default='function',
        help='Group results by function, module, or crate (default: function)'
    )
    parser.add_argument(
        '--demangle', '-d',
        action='store_true',
        help='Simplify Rust/C++ symbol names by removing template parameters'
    )
    parser.add_argument(
        '--sort-by', '-s',
        choices=['samples', 'percent'],
        default='samples',
        help='Sort by samples or percentage (default: samples)'
    )

    args = parser.parse_args()

    try:
        entries = extract_titles_from_svg(args.svg_file)
    except FileNotFoundError:
        print(f"Error: File not found: {args.svg_file}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        sys.exit(1)

    if not entries:
        print("No flamegraph data found in the SVG file.", file=sys.stderr)
        sys.exit(1)

    # Calculate total samples from the entries
    total_samples = sum(samples for _, samples, _ in entries)

    print(f"Parsed {len(entries)} stack frames")
    print(f"Total samples: {total_samples:,}")
    print()

    grouped = group_entries(entries, args.group_by, args.demangle)
    output = format_output(grouped, args.top, args.min_percent, args.sort_by)
    print(output)


if __name__ == '__main__':
    main()
