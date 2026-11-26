//! Build script for Fresh editor
//!
//! Generates TypeScript type definitions from Rust op definitions.
//! JSON Schema for configuration is now generated via `cargo run --features dev-bins --bin generate_schema`.

use std::fs;
use std::path::Path;

// The changelog parser lives in the crate, where it has unit tests; the build
// script compiles the same file to run it.
#[path = "src/release_notes/parse.rs"]
#[allow(dead_code)]
mod release_notes_parse;

fn main() {
    // Embed git commit hash (gracefully falls back to "unknown" outside git).
    //
    // Release builds only. Declaring `.git/HEAD` and `.git/refs` as build
    // inputs means *any* git write -- a commit, a branch switch, a rebase, a
    // `git pull` -- invalidates this build script, which recompiles
    // `fresh-editor` and relinks every one of its 59 integration-test
    // binaries. Measured on a warm `target/` with 10 of those tests built:
    // a genuine no-op rebuild is 0.44s, but one `touch .git/refs/heads/*`
    // turns it into 24.26s, and that is the state you are in after every
    // single commit. At the full 59 targets it is several minutes of
    // relinking for a hash that only ever reaches a `tracing::info!` line.
    //
    // So debug builds report "dev" and register no git dependency; release
    // builds are unchanged and still embed the real short hash.
    let git_hash = if std::env::var("PROFILE").as_deref() == Ok("debug") {
        "dev".to_string()
    } else {
        if Path::new("../../.git/HEAD").exists() {
            println!("cargo::rerun-if-changed=../../.git/HEAD");
            println!("cargo::rerun-if-changed=../../.git/refs");
        }
        std::process::Command::new("git")
            .args(["rev-parse", "--short", "HEAD"])
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .output()
            .ok()
            .filter(|o| o.status.success())
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .unwrap_or_else(|| "unknown".to_string())
    };
    println!("cargo::rustc-env=FRESH_GIT_HASH={}", git_hash);

    // Discover the integration-test roots (see generate_test_roots).
    generate_test_roots();

    // The welcome screen's "What's new", from the changelog's latest release.
    generate_release_notes();

    // ---- Assemble the self-contained web client page ----------------------
    // web-ui/index.html plus css/*.css and js/*.js, concatenated in filename
    // order into $OUT_DIR/webui-index.html (`assemble_webui`). Only the `web`
    // feature compiles webui/mod.rs, so only that build needs the page.
    if std::env::var_os("CARGO_FEATURE_WEB").is_some() {
        assemble_webui();
    }

    // On Windows, embed the application icon, version info, and (for GUI
    // builds) the application manifest into the .exe.  All payload files
    // live under crates/fresh-gui/resources/windows/ — this build script
    // only points winresource at them.
    #[cfg(target_os = "windows")]
    {
        let ico_path = Path::new("../../docs/icons/windows/app.ico");
        if ico_path.exists() {
            let mut res = winresource::WindowsResource::new();
            res.set_icon(ico_path.to_str().unwrap());

            // Version info block (Explorer "Properties" dialog, SmartScreen).
            let version = env!("CARGO_PKG_VERSION");
            res.set("FileVersion", version);
            res.set("ProductVersion", version);
            res.set("ProductName", "Fresh");
            res.set("FileDescription", "Fresh — fast terminal text editor");
            res.set("CompanyName", "Fresh");
            res.set("LegalCopyright", "Licensed under GPL-3.0-or-later");
            res.set("OriginalFilename", "fresh.exe");
            res.set("InternalName", "fresh");

            // Manifest is GUI-only: it declares Per-Monitor DPI awareness
            // and Common Controls v6, both of which are only useful when
            // the binary actually opens a window.  The TUI-only build
            // skips the manifest so it stays a "normal console app".
            if std::env::var("CARGO_FEATURE_GUI").is_ok() {
                let manifest = "../fresh-gui/resources/windows/fresh.manifest";
                if Path::new(manifest).exists() {
                    res.set_manifest_file(manifest);
                } else {
                    eprintln!(
                        "Warning: gui feature enabled but manifest missing at {}",
                        manifest
                    );
                }
            }

            if let Err(e) = res.compile() {
                eprintln!("Warning: Failed to embed Windows resources: {}", e);
            }
        }
    }

    #[cfg(feature = "embed-plugins")]
    {
        println!("cargo::rerun-if-changed=plugins");
        #[cfg(feature = "precompiled-plugins")]
        let precompiled = precompile_plugins();
        #[cfg(not(feature = "precompiled-plugins"))]
        let precompiled: Option<std::path::PathBuf> = None;
        stage_embedded_plugins(precompiled.as_deref());
    }
}

/// Copy the latest release's notes, as written, from `CHANGELOG.md` into
/// `$OUT_DIR/release_notes.rs`, which `src/release_notes/mod.rs` includes.
///
/// The crate carries `CHANGELOG.md` as a symlink to the repository's, which
/// `cargo package` resolves into a real file, so a build from the published
/// crate (distro and musl builds) reads the same notes. The repository path is
/// the fallback for a checkout where the symlink did not survive (git on
/// Windows without symlink support writes it as a one-line text file, which
/// parses to nothing). With neither, the build still succeeds: it compiles
/// `LATEST = None` and the welcome screen leaves "What's new" out.
fn generate_release_notes() {
    let candidates = ["CHANGELOG.md", "../../CHANGELOG.md"];
    let mut release = None;
    for path in candidates {
        // Declared whether or not it exists now, so creating it later
        // reruns this.
        println!("cargo::rerun-if-changed={path}");
        if release.is_none() {
            if let Ok(text) = fs::read_to_string(path) {
                release = release_notes_parse::latest_release(&text);
            }
        }
    }
    let body = match release {
        Some(r) => format!(
            "pub const LATEST: Option<(&str, &str)> = Some(({:?}, {:?}));\n",
            r.version, r.markdown
        ),
        None => {
            println!("cargo::warning=CHANGELOG.md not found or has no release; the welcome screen will have no \"What's new\"");
            "pub const LATEST: Option<(&str, &str)> = None;\n".to_string()
        }
    };
    let out = Path::new(&std::env::var("OUT_DIR").unwrap()).join("release_notes.rs");
    let header = "// Generated by build.rs from CHANGELOG.md. Do not edit.\n";
    let content = format!("{header}{body}");
    // Write only on change, so an unrelated rerun does not recompile.
    if fs::read_to_string(&out).ok().as_deref() != Some(content.as_str()) {
        fs::write(&out, content).expect("write release_notes.rs");
    }
}

/// Compile every bundled plugin (the top-level `plugins/*.ts`, which is what
/// the plugin runtime loads from that directory) to the JavaScript the runtime
/// would produce, and write `precompiled_plugins.rs`: an array of
/// `fresh_parser_js::precompiled::Entry` that `services::plugins::embedded`
/// extracts and registers. Returns the directory of the JavaScript files when
/// the plugins were compiled, in which case the build embeds no TypeScript
/// for them (see [`stage_embedded_plugins`]).
///
/// With the `oxc` feature the plugins compile in-process with oxc, the
/// compiler the runtime of such a build uses. Without it (Debian's build),
/// compiling needs the `esbuild` executable; without that the table is empty
/// and the plugins compile at run time as before, with a cargo warning, so
/// that such builds on machines without esbuild still work. A packager sets
/// `FRESH_REQUIRE_PRECOMPILED_PLUGINS=1` (debian/rules does) to make a missing
/// esbuild, or any plugin that fails to compile, fail the build instead. When
/// the plugins were compiled, the build sets `cfg(fresh_plugins_precompiled)`.
#[cfg(feature = "precompiled-plugins")]
fn precompile_plugins() -> Option<std::path::PathBuf> {
    use fresh_parser_js as parser;

    println!("cargo::rerun-if-env-changed=FRESH_ESBUILD");
    println!("cargo::rerun-if-env-changed=FRESH_REQUIRE_PRECOMPILED_PLUGINS");
    println!("cargo::rustc-check-cfg=cfg(fresh_plugins_precompiled)");
    let out_dir = std::path::PathBuf::from(std::env::var("OUT_DIR").expect("OUT_DIR"));
    let base = out_dir.join("precompiled-plugins");
    if base.exists() {
        fs::remove_dir_all(&base).expect("clear precompiled-plugins dir");
    }
    let js_dir = base.join("js");
    let dts_dir = base.join("d.ts");
    fs::create_dir_all(&js_dir).expect("create precompiled-plugins dir");
    fs::create_dir_all(&dts_dir).expect("create precompiled-plugins dir");

    let table_head = "/// Generated by build.rs: the bundled plugins, compiled.\n\
         pub static PRECOMPILED: &[fresh_plugin_runtime::fresh_parser_js::precompiled::Entry] = {\n\
         #[allow(unused_imports)] // when the table is empty\n\
         use fresh_plugin_runtime::fresh_parser_js::precompiled::Entry;\n\
         &[\n";
    let table_tail = "]};\n";
    let required = std::env::var_os("FRESH_REQUIRE_PRECOMPILED_PLUGINS").is_some_and(|v| v == "1");
    // Without precompiled plugins: fail if asked to have them, else warn and
    // write an empty table, so the plugins compile at run time as before.
    let give_up = |why: String| {
        if required {
            panic!("FRESH_REQUIRE_PRECOMPILED_PLUGINS is set but {why}");
        }
        println!(
            "cargo::warning=precompiled-plugins: {why}, so the bundled plugins are not \
             precompiled and will be compiled at run time"
        );
        fs::write(
            out_dir.join("precompiled_plugins.rs"),
            format!("{table_head}{table_tail}"),
        )
        .expect("write table");
        None
    };
    // `oxc` turns on the build-dependency's oxc too: no executable needed.
    if !cfg!(feature = "oxc") {
        let esbuild = std::env::var_os("FRESH_ESBUILD").unwrap_or_else(|| "esbuild".into());
        let esbuild_runs = std::process::Command::new(&esbuild)
            .arg("--version")
            .output()
            .is_ok_and(|o| o.status.success());
        if !esbuild_runs {
            return give_up(format!("{} cannot be run", esbuild.to_string_lossy()));
        }
    }

    let mut plugins: Vec<_> = fs::read_dir("plugins")
        .expect("read plugins/")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.is_file() && is_plugin_source(p.file_name().unwrap().to_str().unwrap()))
        .collect();
    plugins.sort();

    let mut table = String::from(table_head);
    for path in &plugins {
        let name = path.file_name().unwrap().to_str().unwrap();
        let source = fs::read_to_string(path).expect("read plugin");
        let js = match parser::compile_plugin(path, &source) {
            Ok(js) => js,
            Err(e) => return give_up(format!("plugins/{name} failed to compile: {e:#}")),
        };
        let file = Path::new(name).with_extension("js");
        let js_path = js_dir.join(&file);
        fs::write(&js_path, js).expect("write precompiled plugin");
        // The runtime emits declarations only where it could itself.
        let declarations = parser::CAN_EMIT_DECLARATIONS
            .then(|| parser::emit_isolated_declarations(&source, name).ok())
            .flatten()
            .map(|dts| {
                let dts_path = dts_dir.join(name).with_extension("d.ts");
                fs::write(&dts_path, dts).expect("write plugin declarations");
                format!("Some(include_str!({:?}))", dts_path.display().to_string())
            })
            .unwrap_or_else(|| "None".to_string());
        table.push_str(&format!(
            "    Entry {{ file: {:?}, js: include_str!({:?}), dependencies: &{:?}, declarations: {} }},\n",
            file.display().to_string(),
            js_path.display().to_string(),
            parser::extract_plugin_dependencies(&source),
            declarations,
        ));
    }
    table.push_str(table_tail);
    fs::write(out_dir.join("precompiled_plugins.rs"), table).expect("write table");
    println!("cargo::rustc-cfg=fresh_plugins_precompiled");
    Some(js_dir)
}

/// Whether `name`, a file directly in `plugins/`, is a plugin's source.
#[cfg(feature = "embed-plugins")]
fn is_plugin_source(name: &str) -> bool {
    name.ends_with(".ts") && !name.ends_with(".d.ts") && !name.contains(".i18n.")
}

/// Copy the part of `plugins/` that `services::plugins::embedded` embeds to
/// `$OUT_DIR/embedded-plugins`, and write `plugins_hash.txt`, a hash of what
/// is extracted, which names the directory it is extracted to. Left out:
/// `tests/` and `examples/`, which nothing reads at run time, and, when the
/// plugins were compiled (their JavaScript, in `precompiled`, is extracted
/// instead), their TypeScript and the `lib/` modules it bundles.
/// `lib/fresh.d.ts` stays: user plugins and `init.ts` get their types from it.
#[cfg(feature = "embed-plugins")]
fn stage_embedded_plugins(precompiled: Option<&Path>) {
    use std::hash::Hasher;

    let out_dir = std::path::PathBuf::from(std::env::var("OUT_DIR").expect("OUT_DIR"));
    let staged = out_dir.join("embedded-plugins");
    if staged.exists() {
        fs::remove_dir_all(&staged).expect("clear embedded-plugins");
    }
    let skip = |rel: &Path| {
        if rel == Path::new("tests") || rel == Path::new("examples") {
            return true;
        }
        if precompiled.is_none() {
            return false;
        }
        let name = rel.file_name().and_then(|n| n.to_str()).unwrap_or("");
        match rel.parent().and_then(|p| p.to_str()) {
            Some("") => is_plugin_source(name),
            Some("lib") => name.ends_with(".ts") && !name.ends_with(".d.ts"),
            _ => false,
        }
    };
    copy_tree(Path::new("plugins"), &staged, Path::new(""), &skip).expect("stage plugins");

    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    hash_directory(&staged, &staged, &mut hasher).expect("hash staged plugins");
    if let Some(js_dir) = precompiled {
        hash_directory(js_dir, js_dir, &mut hasher).expect("hash precompiled plugins");
    }
    fs::write(
        out_dir.join("plugins_hash.txt"),
        format!("{:016x}", hasher.finish()),
    )
    .expect("write plugins_hash.txt");
}

/// Copy `src` to `dst`, leaving out every entry whose path relative to the
/// root (`rel` is `src`'s) `skip` matches.
#[cfg(feature = "embed-plugins")]
fn copy_tree(
    src: &Path,
    dst: &Path,
    rel: &Path,
    skip: &dyn Fn(&Path) -> bool,
) -> std::io::Result<()> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let rel = rel.join(entry.file_name());
        if skip(&rel) {
            continue;
        }
        let path = entry.path();
        let target = dst.join(entry.file_name());
        if path.is_dir() {
            copy_tree(&path, &target, &rel, skip)?;
        } else {
            fs::copy(&path, &target)?;
        }
    }
    Ok(())
}

/// Hash the names (relative to `root`) and contents of every file under `dir`.
#[cfg(feature = "embed-plugins")]
fn hash_directory(
    root: &Path,
    dir: &Path,
    hasher: &mut impl std::hash::Hasher,
) -> std::io::Result<()> {
    use std::hash::Hash;

    let mut entries: Vec<_> = fs::read_dir(dir)?.filter_map(|e| e.ok()).collect();
    // Sort for deterministic ordering
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        path.strip_prefix(root).unwrap_or(&path).hash(hasher);
        if path.is_dir() {
            hash_directory(root, &path, hasher)?;
        } else {
            fs::read(&path)?.hash(hasher);
        }
    }

    Ok(())
}

/// Emit the `mod` declarations for every integration-test root under `tests/`.
///
/// `autotests = false` stops cargo building one binary per `tests/*.rs` -- the
/// change that collapsed 59 near-identical link steps into one. But it is
/// all-or-nothing: with discovery off, a new root is not a target and, unless
/// something names it, compiles into nothing. No error, no warning, its tests
/// simply never run. That silence caught real files twice on this branch.
///
/// So discovery moves here. `tests/all_tests.rs` includes the file this
/// writes, and a new `tests/whatever.rs` is picked up with nothing to edit.
///
/// The paths have to be absolute: `#[path]` inside an `include!`d file
/// resolves against *that* file's directory, which is `$OUT_DIR`, not against
/// the file doing the including. A relative hop back to `tests/` is not an
/// option either -- `$OUT_DIR`'s depth moves with the profile and with
/// `CARGO_TARGET_DIR`. The cost is that `file!()` reports an absolute path for
/// these roots. Nothing depends on it being relative: the insta snapshots live
/// under `tests/common/`, which `all_tests.rs` reaches by a plain `mod
/// common;`, and `include_str!` inside a root resolves against the root's own
/// directory either way.
fn generate_test_roots() {
    // Adding or removing a file changes the directory, which is what has to
    // retrigger this. The script itself is one `read_dir`, and cargo only
    // rebuilds the test binary when the bytes below actually change -- so an
    // ordinary edit to a test file costs nothing extra.
    println!("cargo::rerun-if-changed=tests");

    // Forward slashes, even on Windows: this path is about to be written into
    // a Rust string literal, and `D:\a\fresh\...` makes `\a` and `\f` look
    // like character escapes -- 245 of them, which is how this first failed.
    // Windows accepts `/` as a separator, so normalising is enough, and the
    // literal is emitted with `{:?}` below so anything else that needs
    // escaping still is.
    let manifest = env!("CARGO_MANIFEST_DIR").replace('\\', "/");
    let tests_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests");

    let mut roots: Vec<String> = fs::read_dir(&tests_dir)
        .expect("read tests/")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            if !entry.path().is_file() {
                return None;
            }
            let name = entry.file_name().to_string_lossy().into_owned();
            let stem = name.strip_suffix(".rs")?.to_string();
            if stem == "all_tests" {
                return None;
            }
            Some(stem)
        })
        .collect();
    // Sorted so the generated bytes are stable and the file does not churn.
    roots.sort();

    let mut out = String::from("// Generated by build.rs from tests/*.rs. Do not edit.\n");
    for root in &roots {
        let path = format!("{manifest}/tests/{root}.rs");
        out.push_str(&format!("#[path = {path:?}]\nmod {root};\n"));
    }
    // Also emitted as data, so a test can check this list against the
    // directory and catch a build script that did not re-run.
    out.push_str("\n#[allow(dead_code)]\npub const GENERATED_ROOTS: &[&str] = &[\n");
    for root in &roots {
        out.push_str(&format!("    \"{root}\",\n"));
    }
    out.push_str("];\n");

    let dest = Path::new(&std::env::var("OUT_DIR").expect("OUT_DIR")).join("test_roots.rs");
    fs::write(&dest, out).expect("write test_roots.rs");
}

/// Build the single self-contained web client page from its split sources.
///
/// `web-ui/index.html` is the document; `web-ui/css/*.css` and
/// `web-ui/js/*.js` are concatenated in filename order into its `/*@CSS@*/`
/// and `/*@JS@*/` markers. Output: `$OUT_DIR/webui-index.html`, embedded by
/// `webui/mod.rs` via `include_str!` so `fresh --web` stays self-contained.
fn assemble_webui() {
    // Inside the crate, not at the workspace root: `cargo package` vendors only
    // files under the package directory.
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("web-ui");
    assert!(
        root.is_dir(),
        "the `web` feature needs the web-ui/ sources at {}",
        root.display()
    );
    // Only what the page is built from: watching web-ui/ itself would scan
    // test/ (its node_modules, a failed case's screenshots) and rebuild for
    // them. The css/, js/ and skins/ folders and index.html are watched where
    // they are read.
    let concat_dir = |dir: &str| -> String {
        let d = root.join(dir);
        println!("cargo::rerun-if-changed={}", d.display());
        let mut files: Vec<_> = fs::read_dir(&d)
            .unwrap_or_else(|e| panic!("read web-ui/{dir}: {e}"))
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| p.is_file())
            .collect();
        files.sort();
        let mut out = String::new();
        for f in files {
            println!("cargo::rerun-if-changed={}", f.display());
            out.push_str(&fs::read_to_string(&f).expect("read web-ui source"));
            out.push('\n');
        }
        out
    };
    // Built-in skins: `[{"dir": "<NN-name>", "files": {"<name>": "<text>"}}, …]`,
    // the same shape a skin folder the user installs is read into. Placed at
    // the JS's `/*@SKINS@*/[]` marker.
    let skins_root = root.join("skins");
    println!("cargo::rerun-if-changed={}", skins_root.display());
    let mut skin_dirs: Vec<_> = fs::read_dir(&skins_root)
        .expect("web-ui/skins")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.is_dir())
        .collect();
    skin_dirs.sort();
    let mut skins = Vec::new();
    for dir in skin_dirs {
        let dname = dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or_default()
            .to_string();
        assert!(
            dir.join("skin.css").is_file(),
            "web-ui/skins/{dname} has no skin.css"
        );
        println!("cargo::rerun-if-changed={}", dir.display());
        let mut files: Vec<_> = fs::read_dir(&dir)
            .expect("skin folder")
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| p.is_file())
            .collect();
        files.sort();
        let mut entries = Vec::new();
        for f in files {
            let name = f
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or_default()
                .to_string();
            assert!(
                name.ends_with(".css") || name.ends_with(".html") || name.ends_with(".json"),
                "web-ui/skins/{dname}/{name}: a built-in skin bundles only .css/.html/.json (inline images as data: URLs)"
            );
            println!("cargo::rerun-if-changed={}", f.display());
            let text = fs::read_to_string(&f).expect("skin file");
            if name.ends_with(".css") {
                if let Err(e) = css_comments_balanced(&text) {
                    panic!("web-ui/skins/{dname}/{name}: {e} (the browser would drop the rule it falls in)");
                }
            }
            entries.push(format!("{}:{}", js_string(&name), js_string(&text)));
        }
        skins.push(format!(
            "{{\"dir\":{},\"files\":{{{}}}}}",
            js_string(&dname),
            entries.join(",")
        ));
    }
    // `</` escaped so no skin file can close the page's <script> (`<\/` reads
    // back as `</` in a JS string).
    let skins = format!("[{}]", skins.join(",")).replace("</", "<\\/");

    let index = root.join("index.html");
    println!("cargo::rerun-if-changed={}", index.display());
    let shell = fs::read_to_string(&index).expect("web-ui/index.html");
    assert!(
        shell.contains("/*@CSS@*/") && shell.contains("/*@JS@*/"),
        "web-ui/index.html must contain the /*@CSS@*/ and /*@JS@*/ markers"
    );
    let page = shell.replacen("/*@CSS@*/", &concat_dir("css"), 1).replacen(
        "/*@JS@*/",
        &concat_dir("js")
            .replace("</script", "<\\/script")
            .replacen("/*@SKINS@*/[]", &skins, 1),
        1,
    );
    // The page's build: a hash of the page itself, stamped into it and handed
    // to the server, which sends it on every connection. A page left open
    // across an upgrade sees another build and reloads, so a client never
    // talks to a server whose API it was not built for.
    assert!(
        page.contains("@BUILD@"),
        "web-ui/index.html must contain the @BUILD@ marker (its fresh-build meta)"
    );
    let build = format!("{:016x}", fnv1a64(page.as_bytes()));
    let page = page.replacen("@BUILD@", &build, 1);
    let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR");
    fs::write(Path::new(&out_dir).join("webui-index.html"), page).expect("write webui-index.html");
    fs::write(Path::new(&out_dir).join("webui-build.txt"), build).expect("write webui-build.txt");
}

/// FNV-1a, 64-bit: a stable hash with no dependency, for the page's build.
fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    for &b in bytes {
        h ^= u64::from(b);
        h = h.wrapping_mul(0x0000_0100_0000_01b3);
    }
    h
}

/// Whether every `/*` in a stylesheet is closed and no `*/` stands outside a
/// comment (strings skipped). A broken comment makes the browser drop the
/// rule around it without a word, so a built-in skin is checked here.
fn css_comments_balanced(css: &str) -> Result<(), String> {
    let b = css.as_bytes();
    let mut i = 0;
    let line = |at: usize| css[..at].matches('\n').count() + 1;
    while i < b.len() {
        match b[i] {
            b'"' | b'\'' => {
                let q = b[i];
                i += 1;
                while i < b.len() && b[i] != q && b[i] != b'\n' {
                    i += if b[i] == b'\\' { 2 } else { 1 };
                }
                i += 1;
            }
            b'/' if b.get(i + 1) == Some(&b'*') => match css[i + 2..].find("*/") {
                Some(end) => i += 2 + end + 2,
                None => return Err(format!("line {}: a comment is never closed", line(i))),
            },
            b'*' if b.get(i + 1) == Some(&b'/') => {
                return Err(format!("line {}: `*/` closes no comment", line(i)));
            }
            _ => i += 1,
        }
    }
    Ok(())
}

/// `s` as a double-quoted JSON (and so JS) string literal.
fn js_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{2028}' => out.push_str("\\u2028"),
            '\u{2029}' => out.push_str("\\u2029"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}
