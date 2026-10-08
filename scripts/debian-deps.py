#!/usr/bin/env python3
"""Check that Debian's archive can satisfy the Debian build's Rust dependencies.

Debian builds Fresh against its own `librust-*-dev` packages, not Cargo.lock.
For every dependency of a workspace crate in the Debian build (Linux, the
system QuickJS backend, the given features), the archive must have the crate
at a version matching Fresh's requirement and with the requested features.
Crates the archive has bring their own dependencies, which the archive keeps
consistent, so they are not followed; a crate it lacks would have to be
packaged, so its dependencies are checked as well.

Usage (from the workspace root):

    scripts/debian-deps.py PACKAGES [--features runtime,plugins,embed-plugins]

PACKAGES is a Debian `Packages` index (plain or `.xz`), for example
https://deb.debian.org/debian/dists/testing/main/binary-amd64/Packages.xz.
Exits with status 1 if anything is missing. See
docs/internal/debian-quickjs-spike.md.
"""

import argparse
import json
import lzma
import os
import re
import subprocess
import sys
from collections import defaultdict

TARGET = "x86_64-unknown-linux-gnu"


def parse_version(v):
    nums = re.findall(r"\d+", v.split("-")[0].split("+")[0])
    return tuple(int(x) for x in (nums + ["0", "0", "0"])[:3])


def matches(version, req):
    """Whether `version` satisfies a Cargo version requirement."""
    v = parse_version(version)
    for part in req.split(","):
        m = re.match(r"\s*(\^|~|>=|<=|>|<|=)?\s*([\d.*]+)", part)
        if not m:
            continue
        op, rv = m.group(1) or "^", m.group(2)
        if "*" in rv:
            fixed = [int(x) for x in rv.split(".") if x != "*"]
            if list(v[: len(fixed)]) != fixed:
                return False
            continue
        rn = [int(x) for x in rv.split(".")]
        r = tuple((rn + [0, 0, 0])[:3])
        if op == "^":
            # The leftmost non-zero component (or the last one given) must match.
            significant = next((i for i, x in enumerate(rn) if x != 0), len(rn) - 1)
            if v < r or v[: significant + 1] != r[: significant + 1]:
                return False
        elif op == "~":
            k = min(2, len(rn))
            if v < r or v[:k] != r[:k]:
                return False
        elif op == "=":
            if v[: len(rn)] != r[: len(rn)]:
                return False
        elif op == ">=" and v < r:
            return False
        elif op == ">" and v <= r:
            return False
        elif op == "<" and v >= r:
            return False
        elif op == "<=" and v > r:
            return False
    return True


def read_archive(path):
    """crate name (Debian spelling) -> [(upstream version, provides)]."""
    opener = lzma.open if path.endswith(".xz") else open
    with opener(path, "rt", encoding="utf-8", errors="replace") as fh:
        text = fh.read()
    archive = defaultdict(list)
    for stanza in text.split("\n\n"):
        m = re.search(r"^Package: (librust-\S+-dev)$", stanza, re.M)
        if not m:
            continue
        package = m.group(1)
        version = re.search(r"^Version: (\S+)", stanza, re.M).group(1)
        version = re.sub(r"^\d+:", "", version)  # epoch
        version = re.sub(r"-[^-]+$", "", version)  # Debian revision
        version = re.sub(r"[+~].*$", "", version)
        provides = {package}
        line = re.search(r"^Provides: (.*)$", stanza, re.M)
        if line:
            provides |= {item.strip().split(" ")[0] for item in line.group(1).split(",")}
        crate = re.match(r"librust-(.+?)(-\d+(?:\.\d+)*)?-dev$", package)
        if crate:
            archive[crate.group(1)].append((version, provides))
    return archive


def debian_name(name):
    return name.lower().replace("_", "-")


def provides_feature(crate, provides, feature):
    pattern = re.compile(
        rf"librust-{re.escape(crate)}(-[\d.]+)?\+{re.escape(debian_name(feature))}-dev$"
    )
    return any(pattern.match(p) for p in provides)


def target_cfgs(env):
    """The `cfg` keys and key/value pairs that hold for TARGET in this build."""
    out = subprocess.run(
        ["rustc", "--print", "cfg", "--target", TARGET, *env["RUSTFLAGS"].split()],
        capture_output=True, text=True, check=True,
    ).stdout
    return set(out.split())


def cfg_holds(expr, cfgs):
    """Evaluate a dependency's `target` (a triple or a `cfg(...)` predicate)."""
    expr = expr.strip()
    if not expr.startswith("cfg("):
        return expr == TARGET
    tokens = re.findall(r'[A-Za-z_][A-Za-z0-9_]*|"[^"]*"|[(),=]', expr[4:-1])
    pos = 0

    def predicate():
        nonlocal pos
        word = tokens[pos]
        pos += 1
        if word in ("all", "any", "not") and pos < len(tokens) and tokens[pos] == "(":
            pos += 1
            values = []
            while tokens[pos] != ")":
                values.append(predicate())
                if tokens[pos] == ",":
                    pos += 1
            pos += 1
            if word == "all":
                return all(values)
            if word == "any":
                return any(values)
            return not values[0]
        if pos < len(tokens) and tokens[pos] == "=":
            value = tokens[pos + 1]
            pos += 2
            return f"{word}={value}" in cfgs
        return word in cfgs

    return predicate()


def cargo(args, env):
    return subprocess.run(["cargo", *args], capture_output=True, text=True, env=env, check=True).stdout


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("packages", help="Debian Packages index (plain or .xz)")
    parser.add_argument("--features", default="runtime,plugins,embed-plugins")
    args = parser.parse_args()

    env = dict(os.environ)
    env["RUSTFLAGS"] = (env.get("RUSTFLAGS", "") + " --cfg fresh_js_system").strip()

    # The Debian build's graph, as parent -> child edges (normal and build).
    tree = cargo(
        ["tree", "-p", "fresh-editor", "--no-default-features", "--features", args.features,
         "-e", "normal,build", "--target", TARGET, "--prefix", "depth", "--no-dedupe",
         "-f", "{p}"],
        env,
    )
    edges = defaultdict(set)
    workspace = set()
    stack = []
    for line in tree.splitlines():
        m = re.match(r"(\d+)(\S.*)", line)
        if not m:
            continue
        depth, spec = int(m.group(1)), m.group(2)
        name, version = spec.split()[:2]
        node = (name, version.lstrip("v"))
        if "(/" in spec:
            workspace.add(node)
        del stack[depth:]
        if stack:
            edges[stack[-1]].add(node)
        stack.append(node)

    metadata = json.loads(cargo(["metadata", "--format-version", "1", "--filter-platform", TARGET], env))
    packages = {(p["name"], p["version"]): p for p in metadata["packages"]}
    archive = read_archive(args.packages)
    cfgs = target_cfgs(env)

    problems = defaultdict(set)
    missing = defaultdict(set)  # crate -> crates that need it
    checked = 0
    todo = sorted(workspace)
    seen = set()
    while todo:
        parent = todo.pop()
        if parent in seen:
            continue
        seen.add(parent)
        for child in sorted(edges[parent]):
            if child in workspace:
                todo.append(child)
                continue
            deps = [
                d for d in packages[parent]["dependencies"]
                if d["name"] == child[0] and d["kind"] != "dev" and matches(child[1], d["req"])
                and (d["target"] is None or cfg_holds(d["target"], cfgs))
            ]
            if not deps:
                continue
            req = ", ".join(sorted({d["req"] for d in deps}))
            features = sorted(
                {f for d in deps for f in d["features"]}
                | ({"default"} if any(d["uses_default_features"] for d in deps) else set())
            )
            crate = debian_name(child[0])
            candidates = archive.get(crate, [])
            checked += 1
            if not candidates:
                missing[child[0]].add(parent[0])
                todo.append(child)
                continue
            in_range = [c for c in candidates if matches(c[0], req)]
            if not in_range:
                have = ", ".join(sorted({c[0] for c in candidates}, key=parse_version))
                problems[child[0]].add(f"{parent[0]} requires {req}; Debian has {have}")
                continue
            if not any(all(provides_feature(crate, p, f) for f in features) for _, p in in_range):
                version, provides = in_range[0]
                lacking = [f for f in features if not provides_feature(crate, provides, f)]
                problems[child[0]].add(
                    f"{parent[0]} needs feature(s) {', '.join(lacking)}; Debian's {version} lacks them"
                )

    for name, parents in missing.items():
        problems[name].add(f"not in Debian (needed by {', '.join(sorted(parents))})")

    print(f"{checked} dependency edges checked (features: {args.features})")
    for name in sorted(problems):
        for problem in sorted(problems[name]):
            print(f"  {name}: {problem}")
    if problems:
        print(f"{len(problems)} crate(s) Debian cannot supply as required")
        return 1
    print("Debian can supply every dependency")
    return 0


if __name__ == "__main__":
    sys.exit(main())
