#!/usr/bin/env python3
"""Convert a TextMate grammar (.tmLanguage plist) to a .sublime-syntax file.

Regenerates the vendored JavaScript/TypeScript grammars in
crates/fresh-editor-core/src/grammars/ from microsoft/TypeScript-TmLanguage.
syntect only loads .sublime-syntax, and upstream ships TextMate plists.

Usage:
    tmlanguage-to-sublime-syntax.py <TypeScript-TmLanguage checkout> <commit> <out dir>

Writes typescript.sublime-syntax (from TypeScript.tmLanguage), and
typescriptreact.sublime-syntax and javascript.sublime-syntax (both from
TypeScriptReact.tmLanguage; JavaScript is derived the way VS Code derives its
own JavaScript grammar: same rules, `.tsx` scope suffixes renamed to `.js`).

The mapping is the one Sublime Text's own converter uses:
  match/name/captures          -> match/scope/captures
  begin/end                    -> match + push [meta_scope, pop rule, ...patterns]
  name / contentName on begin  -> meta_scope / meta_content_scope
  include #x / $self / $base   -> include x / main / main
The end rule goes first in the pushed context (TextMate tries `end` before the
nested patterns at the same position) unless `applyEndPatternLast` is set.
"""

import plistlib
import sys
from pathlib import Path

import yaml

SUPPORTED_RULE_KEYS = {
    "name",
    "contentName",
    "match",
    "begin",
    "end",
    "captures",
    "beginCaptures",
    "endCaptures",
    "patterns",
    "include",
    "applyEndPatternLast",
    "comment",
    "repository",
}


class Converter:
    def __init__(self, grammar, rename_scope):
        self.grammar = grammar
        self.rename_scope = rename_scope

    def scope(self, name):
        return self.rename_scope(name)

    def captures(self, caps):
        out = {}
        for key, cap in sorted(caps.items(), key=lambda kv: int(kv[0])):
            if "patterns" in cap:
                raise SystemExit(f"capture {key} has sub-patterns; not supported")
            if "name" in cap:
                out[int(key)] = self.scope(cap["name"])
        return out

    def include(self, target):
        if target in ("$self", "$base"):
            return "main"
        if target.startswith("#"):
            return target[1:]
        raise SystemExit(f"external include {target!r} not supported")

    def rules(self, patterns):
        out = []
        for rule in patterns:
            out.extend(self.rule(rule))
        return out

    def rule(self, rule):
        unknown = set(rule) - SUPPORTED_RULE_KEYS
        if unknown:
            raise SystemExit(f"unsupported rule keys {sorted(unknown)}")
        if "repository" in rule:
            raise SystemExit("nested repositories are not supported")
        if "include" in rule:
            return [{"include": self.include(rule["include"])}]
        if "match" in rule:
            converted = {"match": rule["match"]}
            if "name" in rule:
                converted["scope"] = self.scope(rule["name"])
            caps = self.captures(rule.get("captures", {}))
            if caps:
                converted["captures"] = caps
            return [converted]
        if "begin" in rule:
            shared = rule.get("captures", {})
            push = []
            if "name" in rule:
                push.append({"meta_scope": self.scope(rule["name"])})
            if "contentName" in rule:
                push.append({"meta_content_scope": self.scope(rule["contentName"])})
            end = {"match": rule["end"]}
            end_caps = self.captures(rule.get("endCaptures", shared))
            if end_caps:
                end["captures"] = end_caps
            end["pop"] = True
            inner = self.rules(rule.get("patterns", []))
            if rule.get("applyEndPatternLast"):
                push.extend(inner + [end])
            else:
                push.extend([end] + inner)
            converted = {"match": rule["begin"]}
            begin_caps = self.captures(rule.get("beginCaptures", shared))
            if begin_caps:
                converted["captures"] = begin_caps
            converted["push"] = push
            return [converted]
        if "patterns" in rule:
            return self.rules(rule["patterns"])
        return []

    def convert(self, name, file_extensions, header):
        g = self.grammar
        contexts = {"main": self.rules(g["patterns"])}
        for key, rule in g["repository"].items():
            if key == "main":
                raise SystemExit("repository key 'main' collides with the main context")
            contexts[key] = self.rule(rule)
        doc = {
            "name": name,
            "file_extensions": file_extensions,
            "scope": self.scope(g["scopeName"]),
            "contexts": contexts,
        }
        return "%YAML 1.2\n---\n" + header + dump(doc)


class Dumper(yaml.SafeDumper):
    pass


def _str(dumper, value):
    style = "|" if "\n" in value else None
    return dumper.represent_scalar("tag:yaml.org,2002:str", value, style=style)


Dumper.add_representer(str, _str)


def dump(doc):
    return yaml.dump(
        doc,
        Dumper=Dumper,
        sort_keys=False,
        allow_unicode=True,
        width=float("inf"),
        default_flow_style=False,
    )


def header(source_file, commit, notes):
    lines = [
        f"GENERATED by scripts/tmlanguage-to-sublime-syntax.py from {source_file}",
        f"of microsoft/TypeScript-TmLanguage at commit {commit}:",
        f"https://github.com/microsoft/TypeScript-TmLanguage/blob/{commit}/{source_file}",
        "Do not edit by hand; re-run the script against a newer commit instead.",
        "",
        "LICENSE: MIT, Copyright (c) Microsoft Corporation. The notice is vendored",
        "next to this file as `typescript-tmlanguage.LICENSE.txt` and must stay there.",
    ] + notes
    return "".join(f"# {line}\n" if line else "#\n" for line in lines)


def main():
    if len(sys.argv) != 4:
        raise SystemExit(__doc__)
    src, commit, out = Path(sys.argv[1]), sys.argv[2], Path(sys.argv[3])

    def load(file_name):
        with open(src / file_name, "rb") as f:
            return plistlib.load(f)

    identity = lambda s: s
    outputs = [
        (
            "typescript.sublime-syntax",
            Converter(load("TypeScript.tmLanguage"), identity).convert(
                "TypeScript",
                ["ts", "mts", "cts"],
                header("TypeScript.tmLanguage", commit, []),
            ),
        ),
        (
            "typescriptreact.sublime-syntax",
            Converter(load("TypeScriptReact.tmLanguage"), identity).convert(
                "TypeScriptReact",
                ["tsx"],
                header("TypeScriptReact.tmLanguage", commit, []),
            ),
        ),
        (
            "javascript.sublime-syntax",
            Converter(
                load("TypeScriptReact.tmLanguage"),
                lambda s: s.replace(".tsx", ".js"),
            ).convert(
                "JavaScript",
                ["js", "jsx", "mjs", "cjs", "es6"],
                header(
                    "TypeScriptReact.tmLanguage",
                    commit,
                    [
                        "",
                        "JavaScript is the TypeScriptReact grammar with `.tsx` scope",
                        "suffixes renamed to `.js`, which is how VS Code builds its own.",
                    ],
                ),
            ),
        ),
    ]
    for file_name, text in outputs:
        (out / file_name).write_text(text)
        print(f"wrote {out / file_name} ({len(text)} bytes)")


if __name__ == "__main__":
    main()
