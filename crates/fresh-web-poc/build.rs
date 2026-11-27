//! Embed the web client (`web/`) into the binary, and build its syntax
//! highlighter (`crates/fresh-highlight-wasm`) as WebAssembly to embed next
//! to it.
use std::path::{Path, PathBuf};
use std::process::Command;

fn walk(dir: &Path, base: &Path, out: &mut Vec<String>) {
    let mut entries: Vec<_> = std::fs::read_dir(dir).unwrap().flatten().collect();
    entries.sort_by_key(|e| e.path());
    for e in entries {
        let p = e.path();
        if p.is_dir() {
            walk(&p, base, out);
        } else {
            let rel = p
                .strip_prefix(base)
                .unwrap()
                .to_string_lossy()
                .replace('\\', "/");
            out.push(format!(
                "    ({:?}, include_bytes!({:?})),\n",
                rel,
                p.canonicalize().unwrap().to_string_lossy()
            ));
        }
    }
}

const WASM_TARGET: &str = "wasm32-unknown-unknown";

/// Variables cargo hands this script that describe the host build, and would
/// make the nested build compile for the host's flags, into the host's target
/// directory (whose lock the outer build holds) or for the host's target.
const HOST_BUILD_VARS: &[&str] = &[
    "CARGO_ENCODED_RUSTFLAGS",
    "RUSTFLAGS",
    "CARGO_BUILD_RUSTFLAGS",
    "CARGO_TARGET_DIR",
    "CARGO_BUILD_TARGET_DIR",
    "CARGO_BUILD_TARGET",
    "RUSTC_WORKSPACE_WRAPPER",
];

/// Build `fresh-highlight-wasm` for the browser with a nested `cargo build`,
/// into a target directory of this script's own, and return the module's
/// path.
fn build_highlighter(workspace: &Path, out_dir: &Path) -> PathBuf {
    let rustc = std::env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());
    let sysroot = Command::new(&rustc)
        .args(["--print", "sysroot"])
        .output()
        .unwrap_or_else(|e| panic!("cannot run {rustc}: {e}"));
    let sysroot = PathBuf::from(String::from_utf8_lossy(&sysroot.stdout).trim());
    if !sysroot.join("lib/rustlib").join(WASM_TARGET).is_dir() {
        panic!(
            "the {WASM_TARGET} target is not installed for {}, and the web client's \
             highlighter is built for it: run `rustup target add {WASM_TARGET}`",
            sysroot.display()
        );
    }

    let target_dir = out_dir.join("highlight-wasm");
    let cargo = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());
    let mut build = Command::new(cargo);
    build
        .current_dir(workspace)
        .args([
            "build",
            "--package",
            "fresh-highlight-wasm",
            "--profile",
            "wasm",
        ])
        .args(["--target", WASM_TARGET])
        .arg("--target-dir")
        .arg(&target_dir);
    for var in HOST_BUILD_VARS {
        build.env_remove(var);
    }
    let status = build
        .status()
        .unwrap_or_else(|e| panic!("cannot run cargo to build fresh-highlight-wasm: {e}"));
    if !status.success() {
        panic!("building fresh-highlight-wasm for {WASM_TARGET} failed ({status})");
    }
    target_dir
        .join(WASM_TARGET)
        .join("wasm")
        .join("fresh_highlight_wasm.wasm")
}

fn main() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());

    let web = manifest.join("web");
    println!("cargo:rerun-if-changed=web");
    let mut items = Vec::new();
    walk(&web, &web, &mut items);
    let src = format!(
        "pub static ASSETS: &[(&str, &[u8])] = &[\n{}];\n",
        items.concat()
    );
    std::fs::write(out_dir.join("assets.rs"), src).unwrap();

    let workspace = manifest.join("../..");
    for input in [
        "crates/fresh-highlight",
        "crates/fresh-highlight-wasm",
        "Cargo.toml",
        "Cargo.lock",
    ] {
        println!("cargo:rerun-if-changed={}", workspace.join(input).display());
    }
    let wasm = build_highlighter(&workspace, &out_dir);
    std::fs::write(
        out_dir.join("highlight_wasm.rs"),
        format!(
            "pub static HIGHLIGHT_WASM: &[u8] = include_bytes!({:?});\n",
            wasm.to_string_lossy()
        ),
    )
    .unwrap();
}
