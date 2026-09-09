//! Shebang (`#!`) interpreter → language detection.
//!
//! Used as the final fallback in [`GrammarRegistry::find_by_path`](super::GrammarRegistry::find_by_path)
//! when neither the filename/extension nor syntect's first-line regexes match.
//! Syntect only recognises interpreters whose grammars ship a `first_line_match`
//! regex (sh/bash, python, ruby, perl, php); many languages Fresh bundles
//! grammars for — fish, Lua, PowerShell, Tcl, … — define none, so a
//! `#!/usr/bin/fish` script would otherwise fall through to plain text (#2357).
//! Parsing the interpreter ourselves closes that gap.

use fresh_languages::Language;

/// Catalog language id, as accepted by
/// [`GrammarRegistry::find_by_name`](super::GrammarRegistry::find_by_name).
///
/// Tree-sitter-backed languages source their id from
/// [`fresh_languages::Language::id`] so the two stay in sync; the syntect-only
/// grammars below have no [`Language`] variant and are named explicitly.
type LanguageId = &'static str;

// Syntect-only grammar ids — no `fresh_languages::Language` variant exists for
// these. `interpreter_targets_resolve` (in `types.rs`) asserts every id the
// table can return is present in the built-in catalog.
const FISH: LanguageId = "fish";
const PERL: LanguageId = "perl";
const POWERSHELL: LanguageId = "powershell";
const TCL: LanguageId = "tcl";
const GROOVY: LanguageId = "groovy";
const ELIXIR: LanguageId = "elixir";
const R: LanguageId = "r";
const JULIA: LanguageId = "julia";
const NUSHELL: LanguageId = "nushell";
const DART: LanguageId = "dart";

/// Resolve a file's first line to a catalog language id when it is a shebang
/// whose interpreter maps to a grammar Fresh ships. Handles:
/// - direct interpreters: `#!/bin/sh`, `#! /bin/sh` (leading space)
/// - `env` indirection: `#!/usr/bin/env python3`, including `env -S deno run`
///   and `env VAR=val interp`
/// - version suffixes: `python3.11` → python, `lua5.4` → lua, `ruby2.7` → ruby
///
/// Returns `None` for non-shebangs and for interpreters with no Fresh grammar
/// (e.g. `awk`), leaving the buffer as plain text exactly as before.
pub(super) fn language_for_shebang(first_line: &str) -> Option<LanguageId> {
    let rest = first_line.strip_prefix("#!")?;
    let mut tokens = rest.split_whitespace();
    let mut base = interpreter_basename(tokens.next()?);

    // `env` runs the first non-option, non-assignment argument as the real
    // interpreter (`env -S`, `env -i`, `env FOO=bar python` all land here).
    if base == "env" {
        base = loop {
            let tok = tokens.next()?;
            if tok.starts_with('-') || tok.contains('=') {
                continue;
            }
            break interpreter_basename(tok);
        };
    }

    language_for_interpreter(base)
}

/// The final path component of an interpreter token (`/usr/bin/env` → `env`).
fn interpreter_basename(token: &str) -> &str {
    token.rsplit(['/', '\\']).next().unwrap_or(token)
}

/// Map an interpreter basename to a catalog language id, tolerating version
/// suffixes (`python3`, `lua5.4`).
fn language_for_interpreter(base: &str) -> Option<LanguageId> {
    let lower = base.to_ascii_lowercase();
    if let Some(lang) = interpreter_table(&lower) {
        return Some(lang);
    }
    // `python3.11` → `python`, `ruby2.7` → `ruby`, `php8` → `php`.
    let stem = lower.trim_end_matches(|c: char| c.is_ascii_digit() || c == '.');
    if stem.len() != lower.len() && !stem.is_empty() {
        return interpreter_table(stem);
    }
    None
}

/// Interpreter basename → catalog language id.
///
/// Tree-sitter-backed targets reuse [`Language::id`] (visibly marking them and
/// keeping the id in sync with the canonical table); syntect-only grammars use
/// the named constants above.
fn interpreter_table(name: &str) -> Option<LanguageId> {
    Some(match name {
        "sh" | "bash" | "dash" | "ash" | "ksh" | "mksh" | "pdksh" | "zsh" => Language::Bash.id(),
        "python" | "pypy" => Language::Python.id(),
        "ruby" | "jruby" => Language::Ruby.id(),
        "php" => Language::Php.id(),
        "node" | "nodejs" => Language::JavaScript.id(),
        "deno" | "bun" | "ts-node" | "tsx" => Language::TypeScript.id(),
        "lua" | "luajit" => Language::Lua.id(),
        "perl" => PERL,
        "fish" => FISH,
        "pwsh" | "powershell" => POWERSHELL,
        "tcl" | "tclsh" | "wish" => TCL,
        "groovy" => GROOVY,
        "elixir" => ELIXIR,
        "r" | "rscript" => R,
        "julia" => JULIA,
        "nu" => NUSHELL,
        "dart" => DART,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_interpreters_and_indirection() {
        // Direct interpreters, with and without the space-after-#! form (#2357).
        assert_eq!(language_for_shebang("#!/bin/sh\n"), Some("bash"));
        assert_eq!(language_for_shebang("#! /bin/sh\n"), Some("bash"));
        assert_eq!(language_for_shebang("#!/usr/bin/fish\n"), Some("fish"));
        assert_eq!(language_for_shebang("#!/usr/bin/lua\n"), Some("lua"));
        assert_eq!(
            language_for_shebang("#!/usr/bin/pwsh\n"),
            Some("powershell")
        );
        // `env` indirection, options, version suffixes, and assignments.
        assert_eq!(
            language_for_shebang("#!/usr/bin/env python3\n"),
            Some("python")
        );
        assert_eq!(
            language_for_shebang("#!/usr/bin/env -S deno run\n"),
            Some("typescript")
        );
        assert_eq!(
            language_for_shebang("#!/usr/bin/env FOO=bar ruby\n"),
            Some("ruby")
        );
        assert_eq!(
            language_for_shebang("#!/usr/bin/python3.11\n"),
            Some("python")
        );
        assert_eq!(language_for_shebang("#!/usr/bin/env Rscript\n"), Some("r"));
        assert_eq!(
            language_for_shebang("#!/usr/bin/env node\n"),
            Some("javascript")
        );
        assert_eq!(
            language_for_shebang("#!/usr/bin/env elixir\n"),
            Some("elixir")
        );
        // Non-shebangs and unknown interpreters stay None (→ plain text).
        assert_eq!(language_for_shebang("not a shebang\n"), None);
        assert_eq!(language_for_shebang("#!/usr/bin/awk -f\n"), None);
        assert_eq!(language_for_shebang("#!/usr/bin/env\n"), None);
    }
}
