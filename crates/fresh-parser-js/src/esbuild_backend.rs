//! The esbuild implementation of the TypeScript functions, for builds without
//! the `oxc` feature (Debian's: esbuild is a Debian package, oxc is not).
//!
//! Each function runs the `esbuild` executable (`$FRESH_ESBUILD`, else
//! `esbuild` on `PATH`) and adapts its output to what the oxc backend
//! produces:
//!
//! - **Transpiling** is esbuild's transform mode: types stripped, module
//!   syntax left as written.
//! - **Bundling** and **stripping imports/exports** use esbuild's bundler with
//!   ESM output, every non-relative import (`fresh:plugin/…`) left external
//!   and tree-shaking off — plugins define functions they only name in strings
//!   (`registerHandler("name", fn)` aside, `registerCommand(…, "name")`), which
//!   tree-shaking would delete. The external imports and the trailing
//!   `export { … }` block are then removed, as the oxc bundler drops them.
//! - **Declarations** (`.d.ts`) are not available; see
//!   [`crate::CAN_EMIT_DECLARATIONS`].
//!
//! esbuild starts in a few milliseconds, but Fresh loads every plugin at
//! startup, so results are cached on disk (`$XDG_CACHE_HOME/fresh/esbuild`):
//! by source text for transforms, and by the content of every file a bundle
//! read for bundles.

use super::SyntaxError;
use anyhow::{anyhow, Context, Result};
use std::ffi::OsString;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;

/// Flags every invocation shares: quiet except errors, plain text, no
/// project `tsconfig.json` from the plugin's directory, UTF-8 kept as is.
const COMMON_ARGS: &[&str] = &[
    "--log-level=error",
    "--color=false",
    "--tsconfig-raw={}",
    "--charset=utf8",
];

/// Bundler flags for both bundling and import/export stripping.
const BUNDLE_ARGS: &[&str] = &["--bundle", "--format=esm", "--tree-shaking=false"];

fn esbuild_executable() -> OsString {
    std::env::var_os("FRESH_ESBUILD").unwrap_or_else(|| OsString::from("esbuild"))
}

/// esbuild's loader for a file name, matching how the oxc backend picks a
/// source type from the extension.
fn loader_for(filename: &str) -> &'static str {
    let ext = Path::new(filename)
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("");
    match ext {
        "js" | "mjs" | "cjs" => "js",
        "jsx" => "jsx",
        "tsx" => "tsx",
        _ => "ts",
    }
}

/// Run esbuild with `args`, feeding `stdin`. `Ok(Ok(stdout))` on success,
/// `Ok(Err(stderr))` when esbuild reports errors, `Err` when it could not run.
fn run_raw(args: &[String], stdin: Option<&str>) -> Result<std::result::Result<String, String>> {
    let exe = esbuild_executable();
    let mut child = Command::new(&exe)
        .args(args)
        .stdin(if stdin.is_some() {
            Stdio::piped()
        } else {
            Stdio::null()
        })
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                anyhow!(
                    "esbuild not found ({}): this build of Fresh compiles TypeScript \
                     with the `esbuild` executable. Install it (Debian: `apt install \
                     esbuild`) or set FRESH_ESBUILD to its path.",
                    exe.to_string_lossy()
                )
            } else {
                anyhow!("could not run esbuild ({}): {e}", exe.to_string_lossy())
            }
        })?;

    // Feed stdin from a thread so a large output can't deadlock against a
    // large input.
    let writer = match (stdin, child.stdin.take()) {
        (Some(text), Some(mut pipe)) => {
            let text = text.to_owned();
            Some(std::thread::spawn(move || pipe.write_all(text.as_bytes())))
        }
        _ => None,
    };
    let mut stdout = String::new();
    let mut stderr = String::new();
    if let Some(mut out) = child.stdout.take() {
        out.read_to_string(&mut stdout)
            .context("reading esbuild output")?;
    }
    if let Some(mut err) = child.stderr.take() {
        let _ = err.read_to_string(&mut stderr);
    }
    let status = child.wait().context("waiting for esbuild")?;
    if let Some(writer) = writer {
        let _ = writer.join();
    }
    Ok(if status.success() {
        Ok(stdout)
    } else {
        Err(stderr)
    })
}

/// Run esbuild; esbuild's errors become one `anyhow` error listing them.
fn run(args: &[String], stdin: Option<&str>) -> Result<String> {
    match run_raw(args, stdin)? {
        Ok(stdout) => Ok(stdout),
        Err(stderr) => {
            let errors = parse_errors(&stderr);
            if errors.is_empty() {
                Err(anyhow!("esbuild failed: {}", stderr.trim()))
            } else {
                let lines: Vec<String> = errors
                    .iter()
                    .map(|e| format!("{}:{}: {}", e.line, e.column, e.message))
                    .collect();
                Err(anyhow!("TypeScript errors: {}", lines.join("; ")))
            }
        }
    }
}

/// Parse esbuild's error report:
///
/// ```text
/// ✘ [ERROR] Unexpected ";"
///
///     init.ts:2:6:
/// ```
///
/// The line is 1-based and the column 0-based; both come back 1-based.
fn parse_errors(stderr: &str) -> Vec<SyntaxError> {
    let mut out: Vec<SyntaxError> = Vec::new();
    let mut pending: Option<String> = None;
    for line in stderr.lines() {
        if let Some(msg) = line.strip_prefix("✘ [ERROR] ") {
            if let Some(prev) = pending.take() {
                out.push(SyntaxError {
                    message: prev,
                    line: 0,
                    column: 0,
                });
            }
            pending = Some(msg.trim().to_string());
            continue;
        }
        if let Some(msg) = &pending {
            let loc = line.trim();
            if let Some(loc) = loc.strip_suffix(':') {
                let mut parts = loc.rsplitn(3, ':');
                let col = parts.next().and_then(|c| c.parse::<u32>().ok());
                let ln = parts.next().and_then(|l| l.parse::<u32>().ok());
                if let (Some(col), Some(ln)) = (col, ln) {
                    out.push(SyntaxError {
                        message: msg.clone(),
                        line: ln,
                        column: col + 1,
                    });
                    pending = None;
                }
            }
        }
    }
    if let Some(prev) = pending {
        out.push(SyntaxError {
            message: prev,
            line: 0,
            column: 0,
        });
    }
    out
}

/// Remove what esbuild's ESM bundle keeps of module syntax: external
/// `import`/`export … from` statements (which esbuild places at the start of
/// a module's section, right after its `// path` comment) and the trailing
/// `export { … };` block.
fn strip_esm_module_syntax(js: &str) -> String {
    let lines: Vec<&str> = js.lines().collect();

    // The trailing export block: `export {` … `};` as the last statement.
    let mut end = lines.len();
    while end > 0 && lines[end - 1].trim().is_empty() {
        end -= 1;
    }
    if end > 0 {
        let last = lines[end - 1];
        if last.starts_with("export {") && last.ends_with("};") {
            end -= 1;
        } else if last == "};" {
            if let Some(start) = (0..end - 1).rev().find(|&i| {
                let l = lines[i];
                !l.starts_with("  ") || l.starts_with("export {")
            }) {
                if lines[start] == "export {" {
                    end = start;
                }
            }
        }
    }

    let is_module_syntax =
        |l: &str| (l.starts_with("import ") || l.starts_with("export * ")) && l.ends_with(';');
    let mut out = String::with_capacity(js.len());
    let mut at_section_start = true;
    for line in &lines[..end] {
        if line.starts_with("// ") {
            at_section_start = true;
        } else if at_section_start && is_module_syntax(line) {
            continue;
        } else if !line.trim().is_empty() {
            at_section_start = false;
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

// ── cache ────────────────────────────────────────────────────────────────

fn fnv1a(parts: &[&[u8]]) -> u64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    for part in parts {
        for b in part.iter() {
            h ^= *b as u64;
            h = h.wrapping_mul(0x0000_0100_0000_01b3);
        }
        // Separator, so ("ab", "c") and ("a", "bc") differ.
        h ^= 0xff;
        h = h.wrapping_mul(0x0000_0100_0000_01b3);
    }
    h
}

/// esbuild's version, part of every cache key so an upgrade invalidates it.
fn esbuild_version() -> &'static str {
    static VERSION: OnceLock<String> = OnceLock::new();
    VERSION.get_or_init(|| {
        run(&["--version".to_string()], None)
            .map(|v| v.trim().to_string())
            .unwrap_or_default()
    })
}

fn cache_dir() -> Option<PathBuf> {
    static DIR: OnceLock<Option<PathBuf>> = OnceLock::new();
    DIR.get_or_init(|| {
        let base = std::env::var_os("XDG_CACHE_HOME")
            .map(PathBuf::from)
            .filter(|p| p.is_absolute())
            .or_else(|| std::env::var_os("HOME").map(|h| PathBuf::from(h).join(".cache")))?;
        let dir = base.join("fresh").join("esbuild");
        std::fs::create_dir_all(&dir).ok()?;
        Some(dir)
    })
    .clone()
}

fn cache_path(kind: &str, key: u64) -> Option<PathBuf> {
    cache_dir().map(|d| d.join(format!("{kind}-{key:016x}.js")))
}

fn cache_read(path: &Path) -> Option<String> {
    std::fs::read_to_string(path).ok()
}

/// Write atomically (temp file + rename) so concurrent Fresh instances never
/// read a partial entry.
fn cache_write(path: &Path, contents: &str) {
    let tmp = path.with_extension(format!("tmp{}", std::process::id()));
    if std::fs::write(&tmp, contents).is_ok() && std::fs::rename(&tmp, path).is_err() {
        let _ = std::fs::remove_file(&tmp);
    }
}

/// Run esbuild on `source`, cached by everything that determines the output.
fn run_cached(kind: &str, args: &[String], source: &str) -> Result<String> {
    let mut key_parts: Vec<&[u8]> = vec![
        env!("CARGO_PKG_VERSION").as_bytes(),
        esbuild_version().as_bytes(),
        kind.as_bytes(),
    ];
    for a in args {
        key_parts.push(a.as_bytes());
    }
    key_parts.push(source.as_bytes());
    let path = cache_path(kind, fnv1a(&key_parts));
    if let Some(hit) = path.as_deref().and_then(cache_read) {
        return Ok(hit);
    }
    let out = run(args, Some(source))?;
    if let Some(path) = path {
        cache_write(&path, &out);
    }
    Ok(out)
}

// ── the backend functions ────────────────────────────────────────────────

fn transform_args(filename: &str) -> Vec<String> {
    let mut args = vec![
        format!("--loader={}", loader_for(filename)),
        format!("--sourcefile={filename}"),
    ];
    args.extend(COMMON_ARGS.iter().map(|s| s.to_string()));
    args
}

/// Transpile TypeScript source code to JavaScript.
pub fn transpile_typescript(source: &str, filename: &str) -> Result<String> {
    run_cached("transform", &transform_args(filename), source)
}

/// Not available with esbuild; callers check [`crate::CAN_EMIT_DECLARATIONS`].
pub fn emit_isolated_declarations(_source: &str, filename: &str) -> Result<String> {
    Err(anyhow!(
        "{filename}: .d.ts emit needs the oxc backend (this build uses esbuild)"
    ))
}

/// Strip import statements and export keywords from `source`, leaving plain
/// script code (still TypeScript-free only after [`transpile_typescript`],
/// though esbuild has already removed the types). On an esbuild error the
/// source comes back unchanged, as with the oxc backend, so the transpile
/// step reports the error.
pub fn strip_imports_and_exports(source: &str) -> String {
    let mut args = vec![
        "--loader=ts".to_string(),
        "--sourcefile=module.ts".to_string(),
        "--external:*".to_string(),
    ];
    args.extend(BUNDLE_ARGS.iter().map(|s| s.to_string()));
    args.extend(COMMON_ARGS.iter().map(|s| s.to_string()));
    match run_cached("strip", &args, source) {
        Ok(js) => strip_esm_module_syntax(&js),
        Err(_) => source.to_string(),
    }
}

/// Bundle a module and its relative imports into one script. Non-relative
/// imports (`fresh:plugin/…`) are dropped, as the oxc bundler does.
pub fn bundle_module(entry_path: &Path) -> Result<String> {
    let entry = entry_path
        .canonicalize()
        .unwrap_or_else(|_| entry_path.to_path_buf());
    let entry_str = entry.to_string_lossy().into_owned();

    // A bundle depends on every file it read; a cache entry records them
    // with their content hashes, and only counts as a hit if all still match.
    let dir = cache_dir();
    let index = dir.as_ref().map(|d| {
        d.join(format!(
            "bundle-{:016x}.idx",
            fnv1a(&[
                env!("CARGO_PKG_VERSION").as_bytes(),
                esbuild_version().as_bytes(),
                entry_str.as_bytes(),
            ])
        ))
    });
    if let Some(hit) = index.as_deref().and_then(read_bundle_cache) {
        return Ok(hit);
    }

    let metafile = std::env::temp_dir().join(format!(
        "fresh-esbuild-meta-{}-{:016x}.json",
        std::process::id(),
        fnv1a(&[entry_str.as_bytes()])
    ));
    let mut args = vec![
        entry_str.clone(),
        "--packages=external".to_string(),
        format!("--metafile={}", metafile.display()),
    ];
    args.extend(BUNDLE_ARGS.iter().map(|s| s.to_string()));
    args.extend(COMMON_ARGS.iter().map(|s| s.to_string()));
    let result = run(&args, None).with_context(|| format!("bundling {}", entry.display()));
    let inputs = std::fs::read_to_string(&metafile)
        .ok()
        .and_then(|m| metafile_inputs(&m));
    let _ = std::fs::remove_file(&metafile);
    let js = strip_esm_module_syntax(&result?);

    if let (Some(index), Some(inputs)) = (index, inputs) {
        write_bundle_cache(&index, &inputs, &js);
    }
    Ok(js)
}

/// The input files listed in an esbuild metafile, as absolute paths.
fn metafile_inputs(metafile: &str) -> Option<Vec<PathBuf>> {
    let meta: serde_json::Value = serde_json::from_str(metafile).ok()?;
    let inputs = meta.get("inputs")?.as_object()?;
    let cwd = std::env::current_dir().ok()?;
    Some(
        inputs
            .keys()
            .map(|k| {
                let p = PathBuf::from(k);
                if p.is_absolute() {
                    p
                } else {
                    cwd.join(p)
                }
            })
            .collect(),
    )
}

fn file_hash(path: &Path) -> Option<u64> {
    std::fs::read(path).ok().map(|bytes| fnv1a(&[&bytes]))
}

/// Cache index format: one `<hash-hex>\t<path>` line per input, a blank line,
/// then the bundle.
fn write_bundle_cache(index: &Path, inputs: &[PathBuf], js: &str) {
    let mut text = String::new();
    for input in inputs {
        let Some(hash) = file_hash(input) else { return };
        text.push_str(&format!("{hash:016x}\t{}\n", input.display()));
    }
    text.push('\n');
    text.push_str(js);
    cache_write(index, &text);
}

fn read_bundle_cache(index: &Path) -> Option<String> {
    let text = std::fs::read_to_string(index).ok()?;
    let (header, js) = text.split_once("\n\n")?;
    for line in header.lines() {
        let (hash, path) = line.split_once('\t')?;
        let expected = u64::from_str_radix(hash, 16).ok()?;
        if file_hash(Path::new(path))? != expected {
            return None;
        }
    }
    Some(js.to_string())
}

/// Syntax errors in `source`, with 1-based positions.
pub fn syntax_errors(source: &str, filename: &str) -> Vec<SyntaxError> {
    match run_raw(&transform_args(filename), Some(source)) {
        Ok(Ok(_)) => Vec::new(),
        Ok(Err(stderr)) => {
            let errors = parse_errors(&stderr);
            if errors.is_empty() {
                vec![SyntaxError {
                    message: stderr.trim().to_string(),
                    line: 0,
                    column: 0,
                }]
            } else {
                errors
            }
        }
        Err(e) => vec![SyntaxError {
            message: e.to_string(),
            line: 0,
            column: 0,
        }],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_esbuild_error_reports() {
        let report = "✘ [ERROR] Unexpected \";\"\n\n    init.ts:2:6:\n      2 │ let = ;\n        ╵       ^\n\n";
        assert_eq!(
            parse_errors(report),
            vec![SyntaxError {
                message: "Unexpected \";\"".into(),
                line: 2,
                column: 7,
            }]
        );
    }

    #[test]
    fn strips_external_imports_and_the_export_block() {
        let bundle = "// p/lib/util.ts\nvar editor = getEditor();\n\n// p/main.ts\nimport { b } from \"fresh:plugin/b\";\nexport * from \"fresh:plugin/c\";\nfunction f() {\n  return b;\n}\nvar k = `\nimport x from \"y\";\n`;\nexport {\n  f as default,\n  k\n};\n";
        let out = strip_esm_module_syntax(bundle);
        assert!(!out.contains("from \"fresh:plugin"), "{out}");
        assert!(!out.contains("export {"), "{out}");
        // An `import` line inside a template literal is not module syntax.
        assert!(out.contains("import x from \"y\";"), "{out}");
        assert!(out.contains("function f()"), "{out}");
    }

    // The tests below run the real `esbuild` (the Debian CI job installs it).

    #[test]
    fn transpile_strips_types_and_keeps_module_syntax() {
        let js = transpile_typescript(
            "import { a } from './a';\nconst x: number = a;\nexport function f(n: string): string { return n; }\n",
            "t.ts",
        )
        .unwrap();
        assert!(js.contains("const x = a;"), "{js}");
        assert!(js.contains("function f(n)"), "{js}");
        assert!(js.contains("import { a }"), "{js}");
        assert!(!js.contains(": number"), "{js}");
    }

    #[test]
    fn strip_removes_module_syntax_but_keeps_unreferenced_code() {
        let src = "import { foo } from \"./lib\";\nimport bar from \"../bar\";\nexport const API_VERSION = 1;\nexport function greet() { return \"hi\"; }\nexport interface User { name: string; }\nfunction onlyNamedInAString() {}\nregisterHandler(\"onlyNamedInAString\", 0);\nconst x = foo() + bar();";
        let out = strip_imports_and_exports(src);
        assert!(!out.contains("import "), "{out}");
        assert!(!out.contains("export "), "{out}");
        assert!(out.contains("API_VERSION = 1"), "{out}");
        assert!(out.contains("function greet()"), "{out}");
        // Tree-shaking is off: a function referenced only by name survives.
        assert!(out.contains("function onlyNamedInAString()"), "{out}");
        assert!(out.contains("foo() + bar()"), "{out}");
    }

    #[test]
    fn bundle_inlines_relative_imports_and_drops_external_ones() {
        let dir = std::env::temp_dir().join(format!("fresh-esbuild-test-{}", std::process::id()));
        std::fs::create_dir_all(dir.join("lib")).unwrap();
        std::fs::write(
            dir.join("lib/util.ts"),
            "export function twice(n: number): number { return n * 2; }\n",
        )
        .unwrap();
        std::fs::write(
            dir.join("main.ts"),
            "import type { T } from \"fresh:plugin/other\";\nimport { twice } from \"./lib/util.ts\";\nfunction handler(): void { editor.debug(String(twice(2))); }\nregisterHandler(\"handler\", handler);\nexport const answer = 42;\n",
        )
        .unwrap();
        let js = bundle_module(&dir.join("main.ts")).unwrap();
        let _ = std::fs::remove_dir_all(&dir);
        assert!(js.contains("function twice(n)"), "{js}");
        assert!(js.contains("function handler()"), "{js}");
        assert!(!js.contains("fresh:plugin"), "{js}");
        assert!(!js.contains("export "), "{js}");
        assert!(!js.contains(": number"), "{js}");
    }

    #[test]
    fn syntax_errors_have_one_based_positions() {
        let errors = syntax_errors("let a = 1;\nlet = ;\n", "init.ts");
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert_eq!((errors[0].line, errors[0].column), (2, 7));
        assert!(syntax_errors("let a: number = 1;\n", "init.ts").is_empty());
    }

    #[test]
    fn loader_follows_the_extension() {
        assert_eq!(loader_for("a.ts"), "ts");
        assert_eq!(loader_for("a.js"), "js");
        assert_eq!(loader_for("script"), "ts");
    }
}
