#!/usr/bin/env python3
"""Build Fresh from Debian's packaged Rust crates.

Debian builds Rust programs offline against the `librust-*-dev` packages,
which unpack into /usr/share/cargo/registry, instead of crates.io and
Cargo.lock. This script covers the three steps that differ from an upstream
build. Run it from the workspace root; see docs/internal/debian-quickjs-spike.md.

  check PACKAGES [--features F]
      Check that a Debian `Packages` index (plain or .xz, e.g.
      https://deb.debian.org/debian/dists/testing/main/binary-amd64/Packages.xz)
      can satisfy every dependency of the Debian build (the system QuickJS
      backend and the given features): each one must be in the archive at a
      version matching Fresh's requirement, with the requested features.
      Crates the archive has bring their own, archive-consistent dependencies,
      so they are not followed; a crate it lacks would have to be packaged, so
      its dependencies are checked too. Exits 1 if anything is missing.

  packages PACKAGES
      Print the librust-*-dev packages to install: those providing every
      dependency of every workspace crate, optional ones included, that the
      archive can satisfy (cargo resolves the whole workspace before building).

  patch-manifests [REGISTRY]
      Rewrite the workspace's manifests in place so cargo can resolve them
      from REGISTRY (default /usr/share/cargo/registry) alone, as a Debian
      source package's patches would. Removes dev-dependencies, dependencies
      for other targets (Windows, macOS, the rquickjs backend), optional
      dependencies the registry lacks with the feature entries naming them,
      and workspace members that cannot build from it and that nothing needs
      except optionally (fresh-gui). A required dependency the registry lacks
      is an error. Needs python3-tomlkit.
"""

import argparse
import json
import lzma
import os
import re
import subprocess
import sys
from collections import defaultdict

SYSTEM_CFG = "--cfg fresh_js_system"
DEP_TABLES = ("dependencies", "build-dependencies", "dev-dependencies")


# ── versions and cfgs ────────────────────────────────────────────────────


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


def build_env():
    env = dict(os.environ)
    if "fresh_js_system" not in env.get("RUSTFLAGS", ""):
        env["RUSTFLAGS"] = (env.get("RUSTFLAGS", "") + " " + SYSTEM_CFG).strip()
    return env


def host_target():
    out = subprocess.run(["rustc", "-vV"], capture_output=True, text=True, check=True).stdout
    return re.search(r"^host: (\S+)", out, re.M).group(1)


def target_cfgs(env, target):
    """The `cfg` keys and key/value pairs that hold for `target` in this build."""
    out = subprocess.run(
        ["rustc", "--print", "cfg", "--target", target, *env["RUSTFLAGS"].split()],
        capture_output=True, text=True, check=True,
    ).stdout
    return set(out.split())


def cfg_holds(expr, cfgs, target):
    """Evaluate a dependency's `target` (a triple or a `cfg(...)` predicate)."""
    expr = expr.strip()
    if not expr.startswith("cfg("):
        return expr == target
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


# ── the Debian archive ───────────────────────────────────────────────────


def debian_name(name):
    return name.lower().replace("_", "-")


class Archive:
    """The librust-*-dev packages in a Debian `Packages` index."""

    def __init__(self, path):
        opener = lzma.open if path.endswith(".xz") else open
        with opener(path, "rt", encoding="utf-8", errors="replace") as fh:
            text = fh.read()
        self.crates = defaultdict(list)  # crate -> [(version, package, provides)]
        self.providers = defaultdict(list)  # package or virtual name -> [package]
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
            for name in provides:
                self.providers[name].append(package)
            crate = re.match(r"librust-(.+?)(-\d+(?:\.\d+)*)?-dev$", package)
            if crate:
                self.crates[crate.group(1)].append((version, package, provides))

    def lookup(self, crate_name, req, features):
        """("ok", version, [packages]) or ("missing"|"version"|"feature", detail)."""
        crate = debian_name(crate_name)
        candidates = self.crates.get(crate, [])
        if not candidates:
            return ("missing", None)
        in_range = [c for c in candidates if matches(c[0], req)]
        if not in_range:
            return ("version", sorted({c[0] for c in candidates}, key=parse_version))
        for version, package, provides in in_range:
            names = [self.feature_name(crate, version, provides, f) for f in features]
            if all(names):
                packages = {package} | {self.providers[n][0] for n in names}
                return ("ok", version, sorted(packages))
        version, _, provides = in_range[0]
        lacking = [f for f in features if not self.feature_name(crate, version, provides, f)]
        return ("feature", (version, lacking))

    def feature_name(self, crate, version, provides, feature):
        """The name to install for `crate` `version` with `feature`, if any.

        debcargo either folds a feature into the crate's package (listed in its
        Provides) or packages it separately, as
        librust-<crate>[-<version prefix>]+<feature>-dev.
        """
        feature = debian_name(feature)
        parts = version.split(".")
        for prefix in ["", *(".".join(parts[:i]) for i in range(1, len(parts) + 1))]:
            name = f"librust-{crate}{'-' + prefix if prefix else ''}+{feature}-dev"
            if name in provides or name in self.providers:
                return name
        return None


# ── check ────────────────────────────────────────────────────────────────


def cmd_check(args):
    env = build_env()
    target = host_target()
    cfgs = target_cfgs(env, target)

    # The Debian build's graph, as parent -> child edges (normal and build).
    tree = cargo(
        ["tree", "-p", "fresh-editor", "--no-default-features", "--features", args.features,
         "-e", "normal,build", "--target", target, "--prefix", "depth", "--no-dedupe",
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

    metadata = json.loads(cargo(["metadata", "--format-version", "1", "--filter-platform", target], env))
    packages = {(p["name"], p["version"]): p for p in metadata["packages"]}
    archive = Archive(args.packages)

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
                and (d["target"] is None or cfg_holds(d["target"], cfgs, target))
            ]
            if not deps:
                continue
            req = ", ".join(sorted({d["req"] for d in deps}))
            features = sorted(
                {f for d in deps for f in d["features"]}
                | ({"default"} if any(d["uses_default_features"] for d in deps) else set())
            )
            checked += 1
            status, detail = archive.lookup(child[0], req, features)[:2]
            if status == "missing":
                missing[child[0]].add(parent[0])
                todo.append(child)  # it would have to be packaged, so its deps count too
            elif status == "version":
                problems[child[0]].add(f"{parent[0]} requires {req}; Debian has {', '.join(detail)}")
            elif status == "feature":
                version, lacking = detail
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


# ── packages ─────────────────────────────────────────────────────────────


def cmd_packages(args):
    env = build_env()
    target = host_target()
    cfgs = target_cfgs(env, target)
    metadata = json.loads(cargo(["metadata", "--format-version", "1", "--no-deps"], env))
    archive = Archive(args.packages)
    wanted = set()
    for package in metadata["packages"]:
        found, unsatisfied_required = set(), False
        for d in package["dependencies"]:
            if d["kind"] == "dev" or d.get("path"):
                continue
            if d["target"] and not cfg_holds(d["target"], cfgs, target):
                continue
            features = set(d["features"]) | ({"default"} if d["uses_default_features"] else set())
            result = archive.lookup(d["name"], d["req"], sorted(features))
            if result[0] == "ok":
                found.update(result[2])
            elif d["optional"]:
                # patch-manifests drops it.
                print(f"{package['name']}: skip optional {d['name']} {d['req']} ({result[0]})",
                      file=sys.stderr)
            else:
                unsatisfied_required = True
                print(f"{package['name']}: {d['name']} {d['req']} is required ({result[0]}); "
                      f"patch-manifests leaves {package['name']} out if nothing requires it",
                      file=sys.stderr)
        if not unsatisfied_required:
            wanted |= found
    print("\n".join(sorted(wanted)))
    return 0


# ── patch-manifests ──────────────────────────────────────────────────────


def cmd_patch_manifests(args):
    from pathlib import Path

    import tomlkit

    env = build_env()
    target = host_target()
    cfgs = target_cfgs(env, target)

    available = defaultdict(list)
    for entry in os.listdir(args.registry):
        m = re.match(r"(.+)-(\d+\.\d+\.\d+.*)$", entry)
        if m:
            available[m.group(1)].append(m.group(2))

    def in_registry(name, req):
        return any(matches(v, req) for v in available.get(name, []))

    def applies(t):
        return t is None or cfg_holds(t, cfgs, target)

    ws = Path(".")
    root_path = ws / "Cargo.toml"
    root = tomlkit.parse(root_path.read_text())
    workspace_deps = root["workspace"].get("dependencies", {})
    members = {}
    for pattern in root["workspace"]["members"]:
        for d in sorted(ws.glob(pattern)):
            if (d / "Cargo.toml").exists():
                doc = tomlkit.parse((d / "Cargo.toml").read_text())
                members[doc["package"]["name"]] = (pattern, d, doc)

    def dep_info(name, spec):
        """(package name, version requirement or None for a path dep, optional)."""
        if isinstance(spec, str):
            return name, spec, False
        optional = bool(spec.get("optional", False))
        if spec.get("workspace"):
            spec = workspace_deps[name]
            if isinstance(spec, str):
                return name, spec, optional
        return spec.get("package", name), (None if "path" in spec else spec.get("version")), optional

    def dep_tables(doc):
        for kind in DEP_TABLES:
            if kind in doc:
                yield None, kind, doc[kind]
        for t, tdoc in doc.get("target", {}).items():
            for kind in DEP_TABLES:
                if kind in tdoc:
                    yield t, kind, tdoc[kind]

    # Members that cannot build from the registry; fine to leave out if
    # nothing needs them except optionally.
    dropped = set()
    for name, (_, _, doc) in members.items():
        for t, kind, table in dep_tables(doc):
            if kind == "dev-dependencies" or not applies(t):
                continue
            for dname, spec in table.items():
                pkg, req, optional = dep_info(dname, spec)
                if req is not None and not optional and not in_registry(pkg, req):
                    dropped.add(name)
    for name in dropped:
        for other, (_, _, doc) in members.items():
            for t, kind, table in dep_tables(doc):
                if (name in table and kind != "dev-dependencies" and applies(t)
                        and not dep_info(name, table[name])[2]):
                    print(f"{other} needs {name}, which cannot build from the registry", file=sys.stderr)
                    return 1

    errors = []
    for name, (_, path, doc) in members.items():
        if name in dropped:
            continue
        removed, removed_optional = set(), set()
        for t, kind, table in list(dep_tables(doc)):
            for dname in list(table.keys()):
                pkg, req, optional = dep_info(dname, table[dname])
                why = None
                if kind == "dev-dependencies":
                    why = "dev-dependency"
                elif not applies(t):
                    why = f"for another target ({t})"
                elif pkg in dropped:
                    why = "workspace member left out"
                elif req is not None and not in_registry(pkg, req):
                    if optional:
                        why = f"optional, not in the registry ({pkg} {req})"
                    else:
                        errors.append(f"{name}: {pkg} {req} is required and not in the registry")
                if why:
                    del table[dname]
                    removed.add(dname)
                    if optional and kind != "dev-dependencies":
                        removed_optional.add(dname)
                    print(f"{name}: drop {dname}: {why}")
        for t in list(doc.get("target", {}).keys()):
            if all(len(doc["target"][t].get(kind, {})) == 0 for kind in DEP_TABLES):
                del doc["target"][t]
        if "target" in doc and len(doc["target"]) == 0:
            del doc["target"]
        for kind in DEP_TABLES:
            if kind in doc and len(doc[kind]) == 0:
                del doc[kind]

        # Feature entries naming a dependency that is gone from every table.
        remaining = set()
        for _, _, table in dep_tables(doc):
            remaining |= set(table.keys())
        removed -= remaining
        features = doc.get("features")
        if features is not None:
            explicit = {e[4:] for v in features.values() for e in v if e.startswith("dep:")}
            for fname in list(features.keys()):
                kept = [
                    e for e in features[fname]
                    if not (e.startswith("dep:") and e[4:] in removed)
                    and re.match(r"^([\w-]+)", e).group(1) not in removed
                ]
                if len(kept) != len(features[fname]):
                    array = tomlkit.array()
                    array.extend(kept)
                    features[fname] = array
            # An optional dependency without `dep:` also named a feature; keep it, empty.
            for dname in (removed_optional & removed) - explicit:
                if dname not in features:
                    features[dname] = tomlkit.array()
        (path / "Cargo.toml").write_text(tomlkit.dumps(doc))

    if dropped:
        paths = {members[d][0] for d in dropped}
        for key in ("members", "default-members"):
            if key in root["workspace"]:
                keep = tomlkit.array()
                keep.extend(m for m in root["workspace"][key] if m not in paths)
                root["workspace"][key] = keep
        root_path.write_text(tomlkit.dumps(root))
        for name in sorted(dropped):
            print(f"workspace: leave out {name}")
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    return 0


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = parser.add_subparsers(dest="command", required=True)
    p = sub.add_parser("check")
    p.add_argument("packages", help="Debian Packages index (plain or .xz)")
    p.add_argument("--features", default="runtime,plugins,embed-plugins")
    p = sub.add_parser("packages")
    p.add_argument("packages", help="Debian Packages index (plain or .xz)")
    p = sub.add_parser("patch-manifests")
    p.add_argument("registry", nargs="?", default="/usr/share/cargo/registry")
    args = parser.parse_args()
    return {"check": cmd_check, "packages": cmd_packages, "patch-manifests": cmd_patch_manifests}[
        args.command
    ](args)


if __name__ == "__main__":
    sys.exit(main())
