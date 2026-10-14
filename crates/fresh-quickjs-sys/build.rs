//! Find the system QuickJS, compile the inline-function shim against it, and
//! generate bindings from its header.
//!
//! Debian's `libquickjs` installs `/usr/include/quickjs/quickjs.h` and a static
//! `/usr/lib/<multiarch>/quickjs/libquickjs.a`, with no pkg-config file, so the
//! defaults below are those paths. Override them with `QUICKJS_INCLUDE_DIR` and
//! `QUICKJS_LIB_DIR` (for example to point at an unpacked .deb).
//!
//! All of this only runs with the `system` feature and `--cfg fresh_js_system`;
//! otherwise the build script does nothing and the crate is empty. The cfg is
//! read at run time from CARGO_CFG_FRESH_JS_SYSTEM: with `--target`, cargo
//! compiles build scripts without the target's RUSTFLAGS, so `#[cfg]` here
//! would not see it (see Cargo.toml).

#[cfg(feature = "system")]
use std::env;
#[cfg(feature = "system")]
use std::path::{Path, PathBuf};
#[cfg(feature = "system")]
use std::process::Command;

#[cfg(not(feature = "system"))]
fn main() {}

#[cfg(feature = "system")]
fn main() {
    println!("cargo:rerun-if-env-changed=CARGO_CFG_FRESH_JS_SYSTEM");
    if env::var_os("CARGO_CFG_FRESH_JS_SYSTEM").is_none() {
        return;
    }
    for var in [
        "QUICKJS_INCLUDE_DIR",
        "QUICKJS_LIB_DIR",
        "DEB_HOST_MULTIARCH",
        "CC",
    ] {
        println!("cargo:rerun-if-env-changed={var}");
    }
    for file in ["wrapper.h", "shim.h", "shim.c"] {
        println!("cargo:rerun-if-changed={file}");
    }

    let include_dir = env::var_os("QUICKJS_INCLUDE_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("/usr/include/quickjs"));
    let lib_dir = env::var_os("QUICKJS_LIB_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(default_lib_dir);

    require(&include_dir.join("quickjs.h"), "QUICKJS_INCLUDE_DIR");
    require(&lib_dir.join("libquickjs.a"), "QUICKJS_LIB_DIR");

    cc::Build::new()
        .file("shim.c")
        .include(&include_dir)
        .include(".")
        // quickjs.h's own inline helpers trip these; nothing in shim.c does.
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-cast-function-type")
        .compile("fresh_quickjs_shim");

    println!("cargo:rustc-link-search=native={}", lib_dir.display());
    println!("cargo:rustc-link-lib=static=quickjs");
    println!("cargo:rustc-link-lib=m");
    // For Debian's dh-cargo-built-using, which records statically linked
    // libraries in (Static-)Built-Using and stops at any it cannot place.
    // libquickjs.a comes from the libquickjs package, and QuickJS's MIT
    // license does not require shipping its source with the binary (the 0).
    // The shim is built from this crate's own GPL-3+ source (the 1).
    println!("dh-cargo:deb-built-using=quickjs=0~=libquickjs .*");
    println!(
        "dh-cargo:deb-built-using=fresh_quickjs_shim=1={}",
        env::var("CARGO_MANIFEST_DIR").unwrap_or_default()
    );

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .clang_arg(format!("-I{}", include_dir.display()))
        .clang_arg("-I.")
        .allowlist_function("JS_.*|__JS_.*|fqjs_.*|js_free")
        .allowlist_type("JS.*")
        .allowlist_var("JS_.*")
        // Static-inline functions have no symbol in libquickjs.a; the shim
        // exports the ones we need under fqjs_* names instead.
        .generate_inline_functions(false)
        .layout_tests(false)
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .generate()
        .expect("bindgen failed on quickjs.h");

    let out = PathBuf::from(env::var("OUT_DIR").unwrap()).join("bindings.rs");
    bindings.write_to_file(out).expect("writing bindings.rs");
}

#[cfg(feature = "system")]
fn require(path: &Path, var: &str) {
    if !path.exists() {
        panic!(
            "fresh-quickjs-sys: {} not found. Install Debian's `libquickjs` package \
             (apt install libquickjs) or set {var} to the directory that contains it.",
            path.display()
        );
    }
}

/// `/usr/lib/<multiarch>/quickjs`, with the multiarch triplet taken from
/// dpkg-buildpackage's `DEB_HOST_MULTIARCH` when set, else from the C compiler.
#[cfg(feature = "system")]
fn default_lib_dir() -> PathBuf {
    let multiarch = env::var("DEB_HOST_MULTIARCH").ok().or_else(|| {
        let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());
        let out = Command::new(cc).arg("-print-multiarch").output().ok()?;
        let s = String::from_utf8(out.stdout).ok()?.trim().to_string();
        (!s.is_empty()).then_some(s)
    });
    match multiarch {
        Some(m) => PathBuf::from(format!("/usr/lib/{m}/quickjs")),
        None => PathBuf::from("/usr/lib/quickjs"),
    }
}
