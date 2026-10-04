//! Kotlin's built-in language server is JetBrains' official `kotlin-lsp`
//! (<https://github.com/Kotlin/kotlin-lsp>), rooted at the build's root.

use fresh::config::Config;
use fresh::services::lsp::manager::detect_workspace_root;
use fresh::types::LspServerConfig;
use tempfile::TempDir;

fn kotlin_server(config: &Config) -> &LspServerConfig {
    let servers = config
        .lsp
        .get("kotlin")
        .expect("Kotlin should have an LSP entry")
        .as_slice();
    assert_eq!(
        servers.len(),
        1,
        "one Kotlin server, not old + new side by side"
    );
    &servers[0]
}

/// fwcd's `kotlin-language-server` is deprecated in favour of JetBrains'
/// `kotlin-lsp`; a default pointing at the old binary reports a missing server
/// to anyone who installed the current one.
#[test]
fn kotlin_uses_the_official_kotlin_lsp_over_stdio() {
    let config = Config::default();
    let server = kotlin_server(&config);
    assert_eq!(server.command, "kotlin-lsp");
    // Without `--stdio`, kotlin-lsp listens on TCP instead.
    assert_eq!(server.args.as_deref(), Some(&["--stdio".to_string()][..]));
    assert!(server.enabled);
    assert!(
        !server.auto_start,
        "built-in servers never start on their own"
    );
}

/// Workspace detection takes the nearest directory holding any marker, so only
/// files that sit at a build's root may be markers; otherwise a subproject of a
/// multi-module Gradle build becomes the workspace.
#[test]
fn kotlin_root_markers_point_at_the_build_root() {
    let config = Config::default();
    let server = kotlin_server(&config);
    for marker in ["settings.gradle.kts", "settings.gradle", "pom.xml"] {
        assert!(
            server.root_markers.iter().any(|m| m == marker),
            "missing {marker}"
        );
    }
    for marker in ["build.gradle.kts", "build.gradle"] {
        assert!(
            !server.root_markers.iter().any(|m| m == marker),
            "{marker} would root a multi-module build at the subproject"
        );
    }
}

/// A file deep in a subproject of a multi-module Gradle build gets the build's
/// root as its workspace, not the subproject.
#[test]
fn a_kotlin_file_in_a_gradle_subproject_is_rooted_at_the_build() {
    let temp = TempDir::new().unwrap();
    let root = temp.path().join("build");
    let app = root.join("app");
    let src = app.join("src").join("main").join("kotlin");
    std::fs::create_dir_all(&src).unwrap();
    std::fs::write(root.join("settings.gradle.kts"), "include(\"app\")\n").unwrap();
    std::fs::write(app.join("build.gradle.kts"), "").unwrap();
    let file = src.join("Main.kt");
    std::fs::write(&file, "fun main() {}\n").unwrap();

    let config = Config::default();
    let server = kotlin_server(&config);
    assert_eq!(detect_workspace_root(&file, &server.root_markers), root);
}
