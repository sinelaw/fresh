//! Build script for Fresh editor
//!
//! Generates TypeScript type definitions from Rust op definitions.
//! JSON Schema for configuration is now generated via `cargo run --features dev-bins --bin generate_schema`.

use std::fs;
use std::path::Path;

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

    // ---- Assemble the self-contained web-ui page --------------------------
    // Sources live in web-ui/ split by concern: shell.html (the document
    // skeleton) plus css/*.css and js/*.js, each directory concatenated in
    // FILENAME order (the numeric prefixes define CSS cascade / JS
    // declaration order — order is load-bearing, later files deliberately
    // override earlier ones). The result replaces the /*@CSS@*/ and /*@JS@*/
    // markers in shell.html and is written to $OUT_DIR/webui-index.html,
    // which webui/mod.rs embeds via include_str! — the served page stays a
    // single fully self-contained file.
    //
    // Only the `web` feature compiles webui/mod.rs, so only that build needs
    // the page — skip the read-and-concatenate work otherwise.
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
            res.set("LegalCopyright", "Licensed under GPL-2.0");
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

    // Generate plugins content hash for cache invalidation
    #[cfg(feature = "embed-plugins")]
    {
        println!("cargo::rerun-if-changed=plugins");
        if let Err(e) = generate_plugins_hash() {
            eprintln!("Warning: Failed to generate plugins hash: {}", e);
        }
    }
}

/// Generate a hash of all plugin files for cache invalidation
#[cfg(feature = "embed-plugins")]
fn generate_plugins_hash() -> Result<(), Box<dyn std::error::Error>> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hasher;

    let plugins_dir = Path::new("plugins");
    let mut hasher = DefaultHasher::new();

    // Hash all files in the plugins directory recursively
    hash_directory(plugins_dir, &mut hasher)?;

    let hash = format!("{:016x}", hasher.finish());

    let out_dir = std::env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("plugins_hash.txt");
    fs::write(&dest_path, &hash)?;

    println!("cargo::warning=Generated plugins hash: {}", hash);
    Ok(())
}

#[cfg(feature = "embed-plugins")]
fn hash_directory(dir: &Path, hasher: &mut impl std::hash::Hasher) -> std::io::Result<()> {
    use std::hash::Hash;

    if !dir.exists() {
        return Ok(());
    }

    let mut entries: Vec<_> = fs::read_dir(dir)?.filter_map(|e| e.ok()).collect();
    // Sort for deterministic ordering
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        // Hash the relative path
        path.strip_prefix("plugins").unwrap_or(&path).hash(hasher);

        if path.is_dir() {
            hash_directory(&path, hasher)?;
        } else {
            // Hash file contents
            let contents = fs::read(&path)?;
            contents.hash(hasher);
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

    // Declared in Cargo.toml as their own targets instead, because they are
    // feature-gated; pulling them in here would build them unconditionally.
    const SEPARATE_TARGETS: &[&str] = &["scene_parity"];

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
            if stem == "all_tests" || SEPARATE_TARGETS.contains(&stem.as_str()) {
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

/// Build the single self-contained web-ui page from its split sources.
///
/// `web-ui/shell.html` is the document skeleton; `web-ui/css/*.css` and
/// `web-ui/js/*.js` are concatenated in filename order (numeric prefixes
/// define the order, which is load-bearing) into its `/*@CSS@*/` and
/// `/*@JS@*/` markers. Output: `$OUT_DIR/webui-index.html`, embedded by
/// `webui/mod.rs` via `include_str!` so `fresh --web` stays fully
/// self-contained.
fn assemble_webui() {
    // Inside the crate, not at the workspace root: `cargo package` vendors only
    // files under the package directory, so a path reaching outside it would
    // leave the published crate unable to build with `--features web`.
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("web-ui");
    assert!(
        root.is_dir(),
        "the `web` feature needs the web-ui/ sources at {}",
        root.display()
    );
    println!(
        "cargo::rerun-if-changed={}",
        root.join("shell.html").display()
    );
    // The directory entries are also watched so ADDING/REMOVING a part
    // retriggers; per-file lines below catch content edits.
    println!("cargo::rerun-if-changed={}", root.join("css").display());
    println!("cargo::rerun-if-changed={}", root.join("js").display());

    let concat_dir = |dir: &str| -> String {
        let mut files: Vec<_> = fs::read_dir(root.join(dir))
            .unwrap_or_else(|e| panic!("web-ui/{dir}: {e}"))
            .filter_map(|e| e.ok().map(|e| e.path()))
            .collect();
        files.sort();
        let mut out = String::new();
        for f in &files {
            println!("cargo::rerun-if-changed={}", f.display());
            out.push_str(&fs::read_to_string(f).unwrap_or_else(|e| panic!("{}: {e}", f.display())));
        }
        out
    };

    let shell = fs::read_to_string(root.join("shell.html")).expect("web-ui/shell.html");
    let css = concat_dir("css");
    let js = concat_dir("js");
    assert!(
        shell.contains("/*@CSS@*/") && shell.contains("/*@JS@*/"),
        "web-ui/shell.html must contain the /*@CSS@*/ and /*@JS@*/ markers"
    );
    let page = shell
        .replacen("/*@CSS@*/", &css, 1)
        .replacen("/*@JS@*/", &js, 1);

    let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR");
    fs::write(Path::new(&out_dir).join("webui-index.html"), page).expect("write webui-index.html");
}
