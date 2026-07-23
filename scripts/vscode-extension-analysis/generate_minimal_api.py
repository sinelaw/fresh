#!/usr/bin/env python3
"""
Generates the minimal API surface recommendation from analyzed extension data.
Called by analyze-vscode-extensions.sh after analysis is complete.
"""

import os
import re
import sys
import json
from collections import defaultdict
from pathlib import Path


def find_source_files(ext_dir: str):
    """Find all TS/JS source files, excluding build artifacts."""
    excludes = {'node_modules', '.git', 'out', 'dist', '__mocks__', '.vscode-test'}
    for root, dirs, files in os.walk(ext_dir):
        dirs[:] = [d for d in dirs if d not in excludes]
        for f in files:
            if f.endswith(('.ts', '.js', '.tsx', '.jsx')) and not f.endswith(('.d.ts',)):
                yield os.path.join(root, f)


def extract_vscode_api(file_path: str):
    """Extract all vscode API references from a source file."""
    try:
        with open(file_path, 'r', errors='replace') as f:
            content = f.read()
    except Exception:
        return set(), set(), set(), set()

    # vscode.namespace.method patterns
    method_calls = set(re.findall(r'\bvscode\.([a-z][A-Za-z]*)\.([a-zA-Z]+)', content))

    # vscode.Type patterns (capitalized = types/classes)
    type_refs = set(re.findall(r'\bvscode\.([A-Z][A-Za-z]*)\b', content))

    # Destructured imports: import { X, Y } from 'vscode'
    imports = set()
    for m in re.finditer(r'import\s*\{([^}]+)\}\s*from\s*[\'"]vscode[\'"]', content):
        for name in m.group(1).split(','):
            name = name.strip().split(' as ')[0].strip()
            if name:
                imports.add(name)

    # Event subscriptions
    events = set(re.findall(r'\bvscode\.([a-z][A-Za-z]*)\.on(?:Did|Will)([A-Za-z]+)', content))

    return method_calls, type_refs, imports, events


def main():
    if len(sys.argv) < 3:
        print("Usage: generate_minimal_api.py <analysis_dir> <clone_dir>")
        sys.exit(1)

    analysis_dir = sys.argv[1]
    clone_dir = sys.argv[2]

    # Collect per-extension data
    ext_methods = defaultdict(lambda: defaultdict(set))  # ext -> {(ns, method)}
    ext_types = defaultdict(set)    # ext -> {Type}
    ext_imports = defaultdict(set)  # ext -> {import}
    ext_events = defaultdict(set)   # ext -> {(ns, event)}
    ext_contributes = {}            # ext -> {contribute_key: count}

    for ext_name in os.listdir(clone_dir):
        ext_path = os.path.join(clone_dir, ext_name)
        if not os.path.isdir(ext_path):
            continue

        for src_file in find_source_files(ext_path):
            methods, types, imports, events = extract_vscode_api(src_file)
            for ns, method in methods:
                ext_methods[ext_name].add((ns, method))
            ext_types[ext_name].update(types)
            ext_imports[ext_name].update(imports)
            for ns, event in events:
                ext_events[ext_name].add((ns, event))

        # Parse contributes from package.json
        pkg_path = os.path.join(ext_path, 'package.json')
        if os.path.exists(pkg_path):
            try:
                with open(pkg_path) as f:
                    pkg = json.load(f)
                contributes = pkg.get('contributes', {})
                ext_contributes[ext_name] = {
                    k: len(v) if isinstance(v, list) else 1
                    for k, v in contributes.items()
                }
            except Exception:
                pass

    # Count how many extensions use each API
    method_usage = defaultdict(set)  # (ns, method) -> {ext_names}
    type_usage = defaultdict(set)
    import_usage = defaultdict(set)
    event_usage = defaultdict(set)
    contribute_usage = defaultdict(set)

    for ext, methods in ext_methods.items():
        for m in methods:
            method_usage[m].add(ext)

    for ext, types in ext_types.items():
        for t in types:
            type_usage[t].add(ext)

    for ext, imports in ext_imports.items():
        for i in imports:
            import_usage[i].add(ext)

    for ext, events in ext_events.items():
        for e in events:
            event_usage[e].add(ext)

    for ext, contribs in ext_contributes.items():
        for c in contribs:
            contribute_usage[c].add(ext)

    total_exts = len(set(os.listdir(clone_dir)) & set(
        d for d in os.listdir(clone_dir) if os.path.isdir(os.path.join(clone_dir, d))
    ))

    # Generate report
    out_path = os.path.join(analysis_dir, 'minimal_api_surface.txt')
    with open(out_path, 'w') as f:
        f.write("╔═══════════════════════════════════════════════════════════════╗\n")
        f.write("║    MINIMAL VS CODE API SURFACE FOR TOP EXTENSION SUPPORT     ║\n")
        f.write("╚═══════════════════════════════════════════════════════════════╝\n\n")
        f.write(f"Based on analysis of {total_exts} top VS Code extensions\n\n")

        # ── TIER 1: Universal (used by 50%+ extensions) ──
        threshold_universal = max(2, total_exts // 2)
        threshold_common = 2

        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
        f.write(f"  TIER 1: ESSENTIAL API (used by {threshold_universal}+ of {total_exts} extensions)\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

        f.write("  Methods:\n")
        for (ns, method), exts in sorted(method_usage.items(), key=lambda x: -len(x[1])):
            if len(exts) >= threshold_universal:
                f.write(f"    vscode.{ns}.{method}  ({len(exts)} exts: {', '.join(sorted(exts))})\n")

        f.write("\n  Types/Classes:\n")
        for typ, exts in sorted(type_usage.items(), key=lambda x: -len(x[1])):
            if len(exts) >= threshold_universal:
                f.write(f"    vscode.{typ}  ({len(exts)} exts: {', '.join(sorted(exts))})\n")

        f.write("\n  Contributes (package.json):\n")
        for contrib, exts in sorted(contribute_usage.items(), key=lambda x: -len(x[1])):
            if len(exts) >= threshold_universal:
                f.write(f"    contributes.{contrib}  ({len(exts)} exts: {', '.join(sorted(exts))})\n")

        # ── TIER 2: Common (used by 2+ extensions) ──
        f.write("\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
        f.write(f"  TIER 2: COMMON API (used by {threshold_common}+ extensions)\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

        f.write("  Methods:\n")
        for (ns, method), exts in sorted(method_usage.items(), key=lambda x: -len(x[1])):
            if threshold_common <= len(exts) < threshold_universal:
                f.write(f"    vscode.{ns}.{method}  ({len(exts)} exts: {', '.join(sorted(exts))})\n")

        f.write("\n  Types/Classes:\n")
        for typ, exts in sorted(type_usage.items(), key=lambda x: -len(x[1])):
            if threshold_common <= len(exts) < threshold_universal:
                f.write(f"    vscode.{typ}  ({len(exts)} exts: {', '.join(sorted(exts))})\n")

        f.write("\n  Contributes (package.json):\n")
        for contrib, exts in sorted(contribute_usage.items(), key=lambda x: -len(x[1])):
            if threshold_common <= len(exts) < threshold_universal:
                f.write(f"    contributes.{contrib}  ({len(exts)} exts: {', '.join(sorted(exts))})\n")

        # ── TIER 3: Extension-specific ──
        f.write("\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
        f.write("  TIER 3: EXTENSION-SPECIFIC API (used by 1 extension)\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

        f.write("  Methods:\n")
        for (ns, method), exts in sorted(method_usage.items(), key=lambda x: (x[0][0], x[0][1])):
            if len(exts) == 1:
                f.write(f"    vscode.{ns}.{method}  ({list(exts)[0]})\n")

        f.write("\n  Types/Classes:\n")
        for typ, exts in sorted(type_usage.items()):
            if len(exts) == 1:
                f.write(f"    vscode.{typ}  ({list(exts)[0]})\n")

        # ── Events ──
        f.write("\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
        f.write("  EVENT SUBSCRIPTIONS\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

        for (ns, event), exts in sorted(event_usage.items(), key=lambda x: -len(x[1])):
            prefix = "onDid" if True else "onWill"  # simplified
            f.write(f"    vscode.{ns}.on*{event}  ({len(exts)} exts: {', '.join(sorted(exts))})\n")

        # ── Summary: Minimal namespace list ──
        f.write("\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
        f.write("  MINIMAL NAMESPACE SUMMARY\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

        all_namespaces = defaultdict(set)
        for (ns, _), exts in method_usage.items():
            all_namespaces[ns].update(exts)

        f.write("  To support all analyzed extensions, implement these namespaces:\n\n")
        for ns, exts in sorted(all_namespaces.items(), key=lambda x: -len(x[1])):
            methods_in_ns = [(m, e) for (n, m), e in method_usage.items() if n == ns]
            f.write(f"    vscode.{ns}  (used by {len(exts)} exts, {len(methods_in_ns)} methods)\n")

        # ── Per-extension breakdown ──
        f.write("\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
        f.write("  PER-EXTENSION API DEPENDENCY COUNT\n")
        f.write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

        for ext in sorted(ext_methods.keys()):
            n_methods = len(ext_methods[ext])
            n_types = len(ext_types.get(ext, set()))
            n_events = len(ext_events.get(ext, set()))
            contribs = ext_contributes.get(ext, {})
            f.write(f"    {ext}: {n_methods} methods, {n_types} types, {n_events} events, "
                    f"{len(contribs)} contribute keys\n")

    print(f"  Written: {out_path}")


if __name__ == '__main__':
    main()
