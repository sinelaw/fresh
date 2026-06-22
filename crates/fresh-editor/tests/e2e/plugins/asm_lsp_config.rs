//! E2E tests for the asm-lsp helper plugin's `.asm-lsp.toml` config offer.
//!
//! asm-lsp ignores the LSP languageId and defaults to GAS/x86-64, so the
//! plugin offers to create a project config when an assembly file is opened
//! and no config exists. These tests drive the offer popup end-to-end and
//! assert on the rendered screen plus the file the accepted offer writes.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::Path;

const NASM_SOURCE: &str =
    "; NASM hello\nsection .text\nglobal _start\n_start:\n    mov rax, 1\n    syscall\n";
const GAS_SOURCE: &str =
    "# GAS hello\n.text\n.globl _start\n_start:\n    movq $1, %rax\n    syscall\n";

/// Create a project dir with the real asm-lsp plugin and one assembly file.
/// XDG_CONFIG_HOME is pointed at an empty dir inside the project so the
/// plugin's user-global `.asm-lsp.toml` lookup never sees the host's config.
fn setup_project(temp: &Path, file_name: &str, content: &str) -> std::path::PathBuf {
    crate::common::tracing::init_tracing_from_env();
    let project_root = temp.join("project");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "asm-lsp");

    let xdg_dir = temp.join("xdg-config");
    fs::create_dir(&xdg_dir).unwrap();
    std::env::set_var("XDG_CONFIG_HOME", &xdg_dir);

    fs::write(project_root.join(file_name), content).unwrap();
    project_root
}

#[test]
fn test_asm_lsp_config_offer_creates_nasm_config() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path(), "hello.asm", NASM_SOURCE);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.open_file(&project_root.join("hello.asm")).unwrap();

    // The plugin sniffs the buffer and offers to create a config, with the
    // dialect guessed from the Intel-syntax content.
    harness
        .wait_for_screen_contains("Assembly LSP: no .asm-lsp.toml found")
        .unwrap();
    harness
        .wait_for_screen_contains("Create with detected: nasm / x86/x86-64")
        .unwrap();

    // Accept the first (detected) option.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let config_path = project_root.join(".asm-lsp.toml");
    harness.wait_until(|_| config_path.exists()).unwrap();
    let written = fs::read_to_string(&config_path).unwrap();
    assert!(
        written.contains("assembler = \"nasm\""),
        "expected nasm assembler in generated config:\n{written}"
    );
    assert!(
        written.contains("instruction_set = \"x86/x86-64\""),
        "expected x86/x86-64 instruction set in generated config:\n{written}"
    );
    // asm-lsp's default diagnostics run gcc/clang (GAS syntax only), so the
    // generated config must switch them off for Intel-syntax dialects.
    assert!(
        written.contains("default_diagnostics = false"),
        "expected default diagnostics disabled for nasm:\n{written}"
    );
}

#[test]
fn test_asm_lsp_config_offer_detects_gas() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path(), "hello.s", GAS_SOURCE);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.open_file(&project_root.join("hello.s")).unwrap();

    harness
        .wait_for_screen_contains("Create with detected: gas / x86/x86-64")
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let config_path = project_root.join(".asm-lsp.toml");
    harness.wait_until(|_| config_path.exists()).unwrap();
    let written = fs::read_to_string(&config_path).unwrap();
    assert!(
        written.contains("assembler = \"gas\""),
        "expected gas assembler in generated config:\n{written}"
    );
    // gcc/clang understand GAS syntax, so default diagnostics stay on.
    assert!(
        !written.contains("default_diagnostics"),
        "gas config should keep asm-lsp's default diagnostics:\n{written}"
    );
}

#[test]
fn test_asm_lsp_config_offer_is_scoped_to_triggering_buffer() {
    // The offer is raised in response to opening one assembly file, so it
    // must be scoped to that buffer: switching to an unrelated buffer should
    // hide it (it isn't a global notification floating over every buffer),
    // and switching back should reveal it again since it's still unanswered.
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path(), "hello.asm", NASM_SOURCE);
    let other_path = project_root.join("notes.txt");
    fs::write(&other_path, "just some prose, not assembly\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.open_file(&project_root.join("hello.asm")).unwrap();
    harness
        .wait_for_screen_contains("Assembly LSP: no .asm-lsp.toml found")
        .unwrap();

    // Switch to the unrelated buffer — the offer must not follow us there.
    harness.open_file(&other_path).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("just some prose"))
        .unwrap();
    for _ in 0..10 {
        harness.tick_and_render().unwrap();
    }
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Assembly LSP: no .asm-lsp.toml found"),
        "config offer must not render over an unrelated buffer:\n{screen}"
    );

    // Back to the assembly buffer — the still-unanswered offer reappears.
    harness.open_file(&project_root.join("hello.asm")).unwrap();
    harness
        .wait_for_screen_contains("Assembly LSP: no .asm-lsp.toml found")
        .unwrap();
}

#[test]
fn test_asm_lsp_config_offer_skipped_when_config_exists() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = setup_project(temp.path(), "hello.asm", NASM_SOURCE);
    fs::write(
        project_root.join(".asm-lsp.toml"),
        "[default_config]\nassembler = \"nasm\"\ninstruction_set = \"x86/x86-64\"\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.open_file(&project_root.join("hello.asm")).unwrap();

    // Wait until the file is on screen and plugin hooks have run, then give
    // the popup pipeline a generous number of extra ticks before asserting
    // it stayed away.
    harness
        .wait_until(|h| h.screen_to_string().contains("NASM hello"))
        .unwrap();
    for _ in 0..20 {
        harness.tick_and_render().unwrap();
    }
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Assembly LSP: no .asm-lsp.toml found"),
        "config offer must not appear when .asm-lsp.toml already exists:\n{screen}"
    );
}
