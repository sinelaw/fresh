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
    --stacks        Show hottest complete stack traces
    --tree          Show stack tree with hottest paths highlighted
    --max-frames N  Max frames to show per stack before abbreviating (default: 0=no limit)
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


def extract_stacks_from_svg(svg_path: str) -> list[tuple[list[str], int, float]]:
    """
    Extract full stack traces from the SVG file by reconstructing from rect positions.

    In flamegraph SVGs, each rect's y position represents stack depth.
    We group frames by x position and width to reconstruct call stacks.

    Returns list of (stack_frames, samples, percentage) tuples for leaf frames.
    """
    with open(svg_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Parse all <g> elements with their rects and titles
    # Pattern to match: <g>...<title>NAME (SAMPLES samples, PCT%)</title>...<rect ... y="Y" ... fg:x="X" fg:w="W"/>...</g>
    g_pattern = re.compile(
        r'<g[^>]*>.*?<title>([^<]+)</title>.*?<rect[^>]*\sy="([^"]+)"[^>]*fg:x="([^"]+)"[^>]*fg:w="([^"]+)"[^>]*/?>',
        re.DOTALL
    )

    frames_data: list[tuple[str, int, float, int, int, int]] = []  # (name, samples, pct, x, w, y)

    for match in g_pattern.finditer(content):
        title_content = unescape(match.group(1))
        y_str = match.group(2)
        x_str = match.group(3)
        w_str = match.group(4)

        # Parse title
        title_match = re.match(
            r'^(.+?)\s+\(([0-9,]+)\s+samples?,\s+([0-9.]+)%\)$',
            title_content.strip()
        )
        if not title_match:
            continue

        func_name = title_match.group(1)
        samples_str = title_match.group(2).replace(',', '')
        percentage_str = title_match.group(3)

        try:
            samples = int(samples_str)
            percentage = float(percentage_str)
            x = int(x_str)
            w = int(w_str)
            y = int(float(y_str))
        except ValueError:
            continue

        frames_data.append((func_name, samples, percentage, x, w, y))

    if not frames_data:
        return []

    # Sort by x position and then by y (depth) descending to process top-to-bottom
    # In flamegraphs, lower y = deeper in stack (bottom of graph)
    frames_data.sort(key=lambda f: (f[3], -f[5]))  # sort by x, then by y descending

    # Build stacks by finding leaf frames and tracing up to root
    # A leaf frame has no children (no frame with same x and smaller y that fits within its width)

    # Group frames by their x position range
    # For each frame, find all frames that could be its ancestors (same x, larger y, covering its x range)

    # Create a lookup by y level
    by_y: dict[int, list[tuple[str, int, float, int, int, int]]] = defaultdict(list)
    for frame in frames_data:
        by_y[frame[5]].append(frame)

    y_levels = sorted(by_y.keys())  # bottom to top of graph

    # For each leaf (or all frames), reconstruct the stack
    results: list[tuple[list[str], int, float]] = []

    for frame in frames_data:
        func_name, samples, percentage, x, w, y = frame

        # Build stack trace by finding ancestor at each y level above this one
        stack = [func_name]
        current_x = x
        current_w = w

        # Go up through y levels (increasing y values in typical flamegraph)
        for level_y in y_levels:
            if level_y <= y:  # Skip same level and below
                continue

            # Find frame at this level that contains our x range
            for candidate in by_y[level_y]:
                cand_name, _, _, cand_x, cand_w, _ = candidate
                # Check if candidate contains our frame (x range overlaps and candidate is wider or equal)
                if cand_x <= current_x and (cand_x + cand_w) >= (current_x + current_w):
                    stack.append(cand_name)
                    break

        # Reverse to get root-to-leaf order
        stack.reverse()
        results.append((stack, samples, percentage))

    return results


def build_stack_tree(
    stacks: list[tuple[list[str], int, float]],
    demangle: bool
) -> dict:
    """
    Build a tree structure from stack traces.

    Returns a nested dict structure:
    {
        'name': 'root',
        'self_samples': 0,
        'total_samples': N,
        'children': {
            'frame_name': { ... recursive ... }
        }
    }
    """
    root: dict = {
        'name': 'root',
        'self_samples': 0,
        'total_samples': 0,
        'children': {}
    }

    for frames, samples, _pct in stacks:
        if demangle:
            frames = [demangle_name(f) for f in frames]

        root['total_samples'] += samples
        node = root

        for i, frame in enumerate(frames):
            if frame not in node['children']:
                node['children'][frame] = {
                    'name': frame,
                    'self_samples': 0,
                    'total_samples': 0,
                    'children': {}
                }

            child = node['children'][frame]
            child['total_samples'] += samples

            # If this is the last frame, it's a leaf - add to self samples
            if i == len(frames) - 1:
                child['self_samples'] += samples

            node = child

    return root


def find_leaf_stacks(
    stacks: list[tuple[list[str], int, float]]
) -> list[tuple[list[str], int, float]]:
    """
    Filter stacks to only include leaf frames (actual hot code, not intermediate frames).

    A leaf frame is one where no other frame has it as a proper prefix.
    This gives us the actual functions consuming CPU, not just callers.
    """
    # Sort stacks by length descending - longer stacks are more likely to be leaves
    stacks_sorted = sorted(stacks, key=lambda x: len(x[0]), reverse=True)

    # Build a set of stack tuples for fast lookup
    stack_set = {tuple(s[0]) for s in stacks}

    leaves = []
    for frames, samples, pct in stacks_sorted:
        stack_tuple = tuple(frames)
        # Check if this stack is a prefix of any other stack
        is_leaf = True
        for other in stack_set:
            if len(other) > len(stack_tuple):
                # Check if stack_tuple is a prefix of other
                if other[:len(stack_tuple)] == stack_tuple:
                    is_leaf = False
                    break
        if is_leaf:
            leaves.append((frames, samples, pct))

    return leaves


def format_hottest_stacks(
    stacks: list[tuple[list[str], int, float]],
    top_n: int,
    min_percent: float,
    demangle: bool,
    max_frames: int = 0
) -> str:
    """
    Format the hottest complete stack traces.

    Returns formatted string showing full call paths for leaf frames only.
    """
    # Filter to only leaf stacks (actual hot code)
    leaf_stacks = find_leaf_stacks(stacks)

    # Sort by samples (descending) to get hottest stacks
    sorted_stacks = sorted(leaf_stacks, key=lambda x: x[1], reverse=True)

    # Filter by minimum percentage
    filtered = [
        (frames, samples, pct)
        for frames, samples, pct in sorted_stacks
        if pct >= min_percent
    ][:top_n]

    if not filtered:
        return "No stacks found matching criteria."

    lines = []
    lines.append("=" * 80)
    lines.append("HOTTEST STACK TRACES (leaf frames only)")
    lines.append("=" * 80)
    lines.append("")

    for i, (frames, samples, pct) in enumerate(filtered, 1):
        if demangle:
            frames = [demangle_name(f) for f in frames]

        lines.append(f"#{i}: {samples:,} samples ({pct:.2f}%)")
        lines.append("-" * 40)

        # Show stack trace - compact format for readability
        # Show first few and last few frames if stack is long
        if max_frames == 0 or len(frames) <= max_frames:
            for depth, frame in enumerate(frames):
                indent = "  " * min(depth, 4)  # Cap indentation
                if depth == len(frames) - 1:
                    lines.append(f"{indent}→ {frame}")
                else:
                    lines.append(f"{indent}{frame}")
        else:
            # Show abbreviated stack
            head_count = max_frames // 3
            tail_count = max_frames - head_count
            omitted = len(frames) - head_count - tail_count

            for depth, frame in enumerate(frames[:head_count]):
                indent = "  " * min(depth, 4)
                lines.append(f"{indent}{frame}")

            lines.append("    ... ({} frames omitted) ...".format(omitted))

            for depth, frame in enumerate(frames[-tail_count:], len(frames) - tail_count):
                indent = "  " * min(depth, 4)
                if depth == len(frames) - 1:
                    lines.append(f"{indent}→ {frame}")
                else:
                    lines.append(f"{indent}{frame}")

        lines.append("")

    return '\n'.join(lines)


def format_stack_tree(
    tree: dict,
    top_n: int,
    min_percent: float,
    total_samples: int
) -> str:
    """
    Format the stack tree showing hottest paths.

    Returns formatted string with tree structure.
    """
    lines = []
    lines.append("=" * 80)
    lines.append("STACK TREE (hottest paths)")
    lines.append("=" * 80)
    lines.append("")
    lines.append(f"Total samples: {total_samples:,}")
    lines.append("")

    def format_node(node: dict, depth: int, path_samples: int, shown_count: list) -> None:
        """Recursively format tree nodes, prioritizing hottest paths."""
        if shown_count[0] >= top_n * 3:  # Limit total output
            return

        children = list(node['children'].values())
        if not children:
            return

        # Sort children by total_samples (hottest first)
        children.sort(key=lambda x: x['total_samples'], reverse=True)

        for child in children:
            pct = (child['total_samples'] / total_samples * 100) if total_samples > 0 else 0
            self_pct = (child['self_samples'] / total_samples * 100) if total_samples > 0 else 0

            if pct < min_percent:
                continue

            indent = "  " * depth

            # Show self time if significant
            if child['self_samples'] > 0 and self_pct >= 0.1:
                self_info = f" [self: {child['self_samples']:,} ({self_pct:.1f}%)]"
            else:
                self_info = ""

            # Truncate long names
            name = child['name']
            max_name_len = 60
            if len(name) > max_name_len:
                name = name[:max_name_len-3] + "..."

            lines.append(f"{indent}{name}")
            lines.append(f"{indent}  └─ {child['total_samples']:,} samples ({pct:.1f}%){self_info}")

            shown_count[0] += 1

            # Recurse into children
            format_node(child, depth + 1, child['total_samples'], shown_count)

    shown_count = [0]
    format_node(tree, 0, total_samples, shown_count)

    if not lines[-1]:  # Remove trailing empty line
        lines = lines[:-1]

    return '\n'.join(lines)


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
    parser.add_argument(
        '--stacks',
        action='store_true',
        help='Show hottest complete stack traces'
    )
    parser.add_argument(
        '--tree',
        action='store_true',
        help='Show stack tree with hottest paths highlighted'
    )
    parser.add_argument(
        '--max-frames',
        type=int,
        default=0,
        help='Max frames to show per stack before abbreviating (0=no limit, default: 0)'
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

    # Show stacks or tree view if requested
    if args.stacks or args.tree:
        stacks = extract_stacks_from_svg(args.svg_file)

        if args.stacks:
            output = format_hottest_stacks(stacks, args.top, args.min_percent, args.demangle, args.max_frames)
            print(output)
            print()

        if args.tree:
            tree = build_stack_tree(stacks, args.demangle)
            output = format_stack_tree(tree, args.top, args.min_percent, total_samples)
            print(output)
            print()

        # Also show function summary unless it would be redundant
        if not args.stacks:
            grouped = group_entries(entries, args.group_by, args.demangle)
            output = format_output(grouped, args.top, args.min_percent, args.sort_by)
            print(output)
    else:
        # Default: show grouped function summary
        grouped = group_entries(entries, args.group_by, args.demangle)
        output = format_output(grouped, args.top, args.min_percent, args.sort_by)
        print(output)


if __name__ == '__main__':
    main()
