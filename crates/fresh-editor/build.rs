//! Build script for Fresh editor
//!
//! Generates TypeScript type definitions from Rust op definitions.
//! JSON Schema for configuration is now generated via `cargo run --features dev-bins --bin generate_schema`.

use std::fs;
use std::path::Path;

fn main() {
    // Embed git commit hash (gracefully falls back to "unknown" outside git)
    let git_hash = std::process::Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_else(|| "unknown".to_string());
    println!("cargo::rustc-env=FRESH_GIT_HASH={}", git_hash);
    if Path::new("../../.git/HEAD").exists() {
        println!("cargo::rerun-if-changed=../../.git/HEAD");
        println!("cargo::rerun-if-changed=../../.git/refs");
    }

    // Rerun if locales change
    println!("cargo::rerun-if-changed=locales");

    // Rerun if themes change
    println!("cargo::rerun-if-changed=themes");

    // On Windows, embed the application icon into the .exe
    #[cfg(target_os = "windows")]
    {
        let ico_path = Path::new("../../docs/icons/windows/app.ico");
        if ico_path.exists() {
            let mut res = winresource::WindowsResource::new();
            res.set_icon(ico_path.to_str().unwrap());
            if let Err(e) = res.compile() {
                eprintln!("Warning: Failed to embed Windows icon: {}", e);
            }
        }
    }

    // Always generate locale_options.rs - it's required by config.rs at compile time
    // This must run even during publish verification since the include!() macro needs it
    if let Err(e) = generate_locale_options() {
        eprintln!("Warning: Failed to generate locale options: {}", e);
    }

    // Always generate builtin_themes.rs - embeds all themes from themes/ directory
    if let Err(e) = generate_builtin_themes() {
        eprintln!("Warning: Failed to generate builtin themes: {}", e);
    }

    // Pre-compile syntect defaults + embedded grammars into a binary packdump.
    // At runtime this replaces the expensive into_builder() + build() cycle (~12s)
    // with a simple deserialization (~300ms).
    println!("cargo::rerun-if-changed=src/grammars");
    if let Err(e) = generate_syntax_packdump() {
        eprintln!("Warning: Failed to generate syntax packdump: {}", e);
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

/// Generate a Rust file with all theme files embedded from themes/ directory recursively
fn generate_builtin_themes() -> Result<(), Box<dyn std::error::Error>> {
    let themes_dir = Path::new("themes");
    let mut themes: Vec<(String, String, String)> = Vec::new(); // (name, pack, relative_path)

    // Recursively collect all .json files
    collect_theme_files(themes_dir, "", &mut themes)?;

    // Sort by pack then name for consistent output
    themes.sort_by(|a, b| (&a.1, &a.0).cmp(&(&b.1, &b.0)));

    // Generate Rust code
    let out_dir = std::env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("builtin_themes.rs");

    let theme_entries: Vec<String> = themes
        .iter()
        .map(|(name, pack, rel_path)| {
            format!(
                r#"    BuiltinTheme {{
        name: "{}",
        pack: "{}",
        json: include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/themes/{}")),
    }}"#,
                name, pack, rel_path
            )
        })
        .collect();

    let content = format!(
        r#"// Auto-generated by build.rs from themes/**/*.json files
// DO NOT EDIT MANUALLY

/// All builtin themes embedded at compile time.
pub const BUILTIN_THEMES: &[BuiltinTheme] = &[
{}
];
"#,
        theme_entries.join(",\n")
    );

    fs::write(&dest_path, content)?;

    println!("cargo::warning=Generated {} builtin themes", themes.len());

    Ok(())
}

/// Recursively collect theme files from a directory
fn collect_theme_files(
    dir: &Path,
    pack: &str,
    themes: &mut Vec<(String, String, String)>,
) -> std::io::Result<()> {
    if !dir.exists() {
        return Ok(());
    }

    let mut entries: Vec<_> = fs::read_dir(dir)?.filter_map(|e| e.ok()).collect();
    // Sort for deterministic ordering
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();

        if path.is_dir() {
            // Recurse into subdirectory, updating pack name
            let dir_name = path.file_name().unwrap().to_string_lossy();
            let new_pack = if pack.is_empty() {
                dir_name.to_string()
            } else {
                format!("{}/{}", pack, dir_name)
            };
            collect_theme_files(&path, &new_pack, themes)?;
        } else if path.extension().is_some_and(|ext| ext == "json") {
            // Found a theme file
            let name = path.file_stem().unwrap().to_string_lossy().to_string();
            // Build path with forward slashes for cross-platform include_str!
            let rel_path = path
                .strip_prefix("themes")
                .unwrap()
                .components()
                .map(|c| c.as_os_str().to_string_lossy())
                .collect::<Vec<_>>()
                .join("/");
            themes.push((name, pack.to_string(), rel_path));
        }
    }

    Ok(())
}

/// Generate a Rust file with the list of available locales from the locales directory
fn generate_locale_options() -> Result<(), Box<dyn std::error::Error>> {
    let locales_dir = Path::new("locales");

    // Read all .json files in the locales directory
    let mut locales: Vec<String> = fs::read_dir(locales_dir)?
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension()? == "json" {
                path.file_stem()?.to_str().map(|s| s.to_string())
            } else {
                None
            }
        })
        .collect();

    // Sort alphabetically for consistent output
    locales.sort();

    // Generate Rust code
    let out_dir = std::env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("locale_options.rs");

    let locale_entries: Vec<String> = locales.iter().map(|l| format!("Some(\"{}\")", l)).collect();

    let content = format!(
        r#"// Auto-generated by build.rs from locales/*.json files
// DO NOT EDIT MANUALLY

/// Available locale options for the settings dropdown
/// None (null) means auto-detect from environment
pub const GENERATED_LOCALE_OPTIONS: &[Option<&str>] = &[
    None, // Auto-detect
    {}
];
"#,
        locale_entries.join(",\n    ")
    );

    // Note: OUT_DIR files don't need write_if_changed since cargo handles them specially,
    // but it doesn't hurt to use it for consistency
    fs::write(&dest_path, content)?;

    println!(
        "cargo::warning=Generated locale options with {} locales",
        locales.len()
    );

    Ok(())
}

/// Pre-compile syntect defaults + all embedded grammars into a single binary packdump.
///
/// This moves the expensive `SyntaxSetBuilder::build()` + grammar parsing from
/// runtime to build time, reducing startup from ~12s to ~300ms.
fn generate_syntax_packdump() -> Result<(), Box<dyn std::error::Error>> {
    use syntect::dumps::dump_to_uncompressed_file;
    use syntect::parsing::{SyntaxDefinition, SyntaxSet};

    let out_dir = std::env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("default_syntaxes.packdump");

    // Start with syntect's built-in defaults
    let defaults = SyntaxSet::load_defaults_newlines();
    let mut builder = defaults.into_builder();

    // Add all embedded grammars — must match the list in types.rs add_embedded_grammars()
    let grammar_files: &[(&str, &str)] = &[
        ("src/grammars/toml.sublime-syntax", "TOML"),
        ("src/grammars/odin/Odin.sublime-syntax", "Odin"),
        ("src/grammars/zig.sublime-syntax", "Zig"),
        ("src/grammars/git-rebase.sublime-syntax", "Git Rebase Todo"),
        (
            "src/grammars/git-commit.sublime-syntax",
            "Git Commit Message",
        ),
        ("src/grammars/gitignore.sublime-syntax", "Gitignore"),
        ("src/grammars/gitconfig.sublime-syntax", "Git Config"),
        (
            "src/grammars/gitattributes.sublime-syntax",
            "Git Attributes",
        ),
        ("src/grammars/typst.sublime-syntax", "Typst"),
        ("src/grammars/dockerfile.sublime-syntax", "Dockerfile"),
        ("src/grammars/ini.sublime-syntax", "INI"),
        ("src/grammars/cmake.sublime-syntax", "CMake"),
        ("src/grammars/scss.sublime-syntax", "SCSS"),
        ("src/grammars/less.sublime-syntax", "LESS"),
        ("src/grammars/powershell.sublime-syntax", "PowerShell"),
        ("src/grammars/kotlin.sublime-syntax", "Kotlin"),
        ("src/grammars/swift.sublime-syntax", "Swift"),
        ("src/grammars/dart.sublime-syntax", "Dart"),
        ("src/grammars/elixir.sublime-syntax", "Elixir"),
        ("src/grammars/fsharp.sublime-syntax", "FSharp"),
        ("src/grammars/nix.sublime-syntax", "Nix"),
        ("src/grammars/hcl.sublime-syntax", "HCL"),
        ("src/grammars/protobuf.sublime-syntax", "Protocol Buffers"),
        ("src/grammars/graphql.sublime-syntax", "GraphQL"),
        ("src/grammars/julia.sublime-syntax", "Julia"),
        ("src/grammars/nim.sublime-syntax", "Nim"),
        ("src/grammars/gleam.sublime-syntax", "Gleam"),
        ("src/grammars/vlang.sublime-syntax", "V"),
        ("src/grammars/solidity.sublime-syntax", "Solidity"),
        ("src/grammars/kdl.sublime-syntax", "KDL"),
        ("src/grammars/nushell.sublime-syntax", "Nushell"),
        ("src/grammars/starlark.sublime-syntax", "Starlark"),
        ("src/grammars/justfile.sublime-syntax", "Justfile"),
        ("src/grammars/earthfile.sublime-syntax", "Earthfile"),
        ("src/grammars/gomod.sublime-syntax", "Go Module"),
        ("src/grammars/vue.sublime-syntax", "Vue"),
        ("src/grammars/svelte.sublime-syntax", "Svelte"),
        ("src/grammars/astro.sublime-syntax", "Astro"),
        ("src/grammars/hyprlang.sublime-syntax", "Hyprlang"),
    ];

    let mut loaded = 0;
    for (path, name) in grammar_files {
        let content = fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("Failed to read grammar {}: {}", path, e));
        match SyntaxDefinition::load_from_str(&content, true, Some(name)) {
            Ok(syntax) => {
                builder.add(syntax);
                loaded += 1;
            }
            Err(e) => {
                eprintln!("Warning: Failed to parse grammar {}: {}", name, e);
            }
        }
    }

    let syntax_set = builder.build();
    dump_to_uncompressed_file(&syntax_set, &dest_path)?;

    println!(
        "cargo::warning=Generated syntax packdump: {} syntaxes ({} embedded)",
        syntax_set.syntaxes().len(),
        loaded
    );

    Ok(())
}
