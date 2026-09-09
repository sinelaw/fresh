//! E2E tests for `plugins/deno_lsp.ts` — the Deno server selection that used
//! to be hard-coded in `configure_lsp_servers`.
//!
//! A project rooted at a `deno.json`/`deno.jsonc` is served by `deno lsp`
//! (#1191), but only when the Deno runtime is installed (#2981). `deno` here
//! is a stub on `PATH`: the plugin only needs the executable to exist, and
//! nothing in these tests starts a server.

use crate::common::global_state::{pin_config_globals, pin_path_with_dir_first};
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use std::fs;
use std::path::{Path, PathBuf};

fn project(files: &[(&str, &str)]) -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("project_root");
    fs::create_dir(&root).unwrap();
    let root = fs::canonicalize(&root).unwrap();

    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "deno_lsp");

    for (name, contents) in files {
        fs::write(root.join(name), contents).unwrap();
    }

    (temp_dir, root)
}

/// Put a stub `deno` on `PATH` for the duration of the returned guard.
///
/// `PATH` is process-global and these run as threads of one binary, so the
/// stub goes on through [`pin_path_with_dir_first`] — the same pin the
/// fake-`ssh` shims use. That single owner is what keeps a sibling's *restore*
/// from landing mid-test and taking this stub (or the sibling's shim) with it:
/// each snapshot is taken while no one else can be holding one.
struct DenoOnPath {
    _dir: tempfile::TempDir,
    _pin: crate::common::global_state::PathPin,
}

impl DenoOnPath {
    fn install() -> Self {
        let dir = tempfile::TempDir::new().unwrap();
        let deno = dir
            .path()
            .join(if cfg!(windows) { "deno.exe" } else { "deno" });
        fs::write(&deno, "#!/bin/sh\nexit 0\n").unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(&deno).unwrap().permissions();
            perms.set_mode(0o755);
            fs::set_permissions(&deno, perms).unwrap();
        }

        let pin = pin_path_with_dir_first(dir.path());
        Self {
            _dir: dir,
            _pin: pin,
        }
    }
}

/// Whether the host running the tests has its own `deno`.
fn deno_is_installed() -> bool {
    let Ok(path) = std::env::var("PATH") else {
        return false;
    };
    std::env::split_paths(&path).any(|dir| {
        dir.join(if cfg!(windows) { "deno.exe" } else { "deno" })
            .is_file()
    })
}

fn harness_in(root: &Path) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        root.to_path_buf(),
    )
    .unwrap();
    harness.open_file(&root.join("index.ts")).unwrap();
    harness.render().unwrap();
    harness
}

fn configured_command(harness: &EditorTestHarness, language: &str) -> Option<String> {
    harness
        .editor()
        .config()
        .lsp
        .get(language)
        .and_then(|configs| configs.as_slice().first())
        .map(|config| config.command.clone())
}

/// Pump the editor until `condition` holds, or give up.
///
/// Deliberately not `harness.wait_until`, which loops forever: "the plugin
/// never applied" is a plausible outcome here and should read as a failed
/// assertion rather than a hang.
fn settle(harness: &mut EditorTestHarness, condition: impl Fn(&EditorTestHarness) -> bool) -> bool {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    while std::time::Instant::now() < deadline {
        if condition(harness) {
            return true;
        }
        harness.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    condition(harness)
}

/// Let the plugin runtime start and make its choice, whatever that is.
///
/// The negative cases have nothing to wait *for* — "unchanged" and "has not
/// run yet" look alike — so they wait for the plugin to have loaded at all.
fn settle_plugins(harness: &mut EditorTestHarness) {
    settle(harness, |h| {
        h.editor().config().lsp.contains_key("typescript")
    });
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
    while std::time::Instant::now() < deadline {
        harness.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
}

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn deno_project_with_the_runtime_installed_uses_deno_lsp() {
    let _pin = pin_config_globals();
    let _deno = DenoOnPath::install();
    let (_temp, root) = project(&[
        ("deno.json", "{ \"tasks\": {} }\n"),
        ("index.ts", "export const x: number = 1;\n"),
    ]);

    let mut harness = harness_in(&root);
    // The two registrations can land in different ticks, so wait for both.
    let switched = settle(&mut harness, |h| {
        configured_command(h, "typescript").as_deref() == Some("deno")
            && configured_command(h, "javascript").as_deref() == Some("deno")
    });
    assert!(
        switched,
        "the plugin should switch TypeScript and JavaScript over to `deno lsp`; got {:?} / {:?}\n\
         window root: {:?}\nplugin errors: {:?}\nPATH: {:?}",
        configured_command(&harness, "typescript"),
        configured_command(&harness, "javascript"),
        harness.editor().active_window().root,
        harness.editor().active_window().plugin_errors,
        std::env::var("PATH").unwrap_or_default(),
    );

    // The entry the plugin installs is the one the core version installed:
    // `deno lsp`, enabled, not auto-started.
    let deno = harness.editor().config().lsp.get("typescript").unwrap();
    let deno = deno.as_slice().first().unwrap();
    assert_eq!(deno.args.as_deref(), Some(&["lsp".to_string()][..]));
    assert!(deno.enabled);
    assert!(!deno.auto_start, "the plugin asks for `autoStart: false`");
}

/// The #2981 case: the marker file is there, Deno is not.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn deno_project_without_the_runtime_keeps_the_configured_server() {
    let _pin = pin_config_globals();
    let (_temp, root) = project(&[
        ("deno.jsonc", "{ \"tasks\": {} }\n"),
        ("package.json", "{ \"name\": \"p\" }\n"),
        ("index.ts", "export const x: number = 1;\n"),
    ]);

    // Deliberately no `deno` on PATH. A host that has one installed has
    // nothing to say here.
    if deno_is_installed() {
        return;
    }

    let mut harness = harness_in(&root);
    settle_plugins(&mut harness);

    let command = configured_command(&harness, "typescript");
    assert_ne!(
        command.as_deref(),
        Some("deno"),
        "a project without the Deno runtime must keep its configured server \
         (bug #2981); got {command:?}",
    );
}

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn a_plain_typescript_project_is_untouched() {
    let _pin = pin_config_globals();
    let _deno = DenoOnPath::install();
    let (_temp, root) = project(&[
        ("package.json", "{ \"name\": \"p\" }\n"),
        ("index.ts", "export const x: number = 1;\n"),
    ]);

    let mut harness = harness_in(&root);
    settle_plugins(&mut harness);

    let command = configured_command(&harness, "typescript");
    assert_ne!(
        command.as_deref(),
        Some("deno"),
        "a project with no deno.json must keep its configured server; got {command:?}",
    );
}

/// A language the user turned off stays off. `registerLspServer` can only
/// register an enabled server, so the plugin must not touch it at all.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn a_language_the_user_disabled_is_left_alone() {
    let _pin = pin_config_globals();
    let _deno = DenoOnPath::install();
    let (_temp, root) = project(&[
        ("deno.json", "{ \"tasks\": {} }\n"),
        ("index.ts", "export const x: number = 1;\n"),
    ]);

    let mut config = fresh::config::Config::default();
    for server in config.lsp.get_mut("typescript").unwrap().as_mut_slice() {
        server.enabled = false;
    }
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, root.clone()).unwrap();
    harness.open_file(&root.join("index.ts")).unwrap();
    harness.render().unwrap();

    // JavaScript is still enabled, so it is the one that switches; waiting on
    // it also means the plugin has made its choice for TypeScript.
    let switched = settle(&mut harness, |h| {
        configured_command(h, "javascript").as_deref() == Some("deno")
    });
    assert!(
        switched,
        "JavaScript, still enabled, should switch to `deno lsp`"
    );

    let typescript = harness.editor().config().lsp.get("typescript").unwrap();
    let typescript = typescript.as_slice().first().unwrap();
    assert_ne!(
        typescript.command, "deno",
        "a disabled language must not be switched"
    );
    assert!(
        !typescript.enabled,
        "a disabled language must not be re-enabled"
    );
}

/// Leaving a Deno project for a plain one puts the configured server back.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn leaving_a_deno_project_restores_the_configured_server() {
    let _pin = pin_config_globals();
    let _deno = DenoOnPath::install();
    let (_temp, deno_root) = project(&[
        ("deno.json", "{ \"tasks\": {} }\n"),
        ("index.ts", "export const x: number = 1;\n"),
    ]);
    let plain = tempfile::TempDir::new().unwrap();
    let plain_root = fs::canonicalize(plain.path()).unwrap();
    fs::write(plain_root.join("package.json"), "{ \"name\": \"p\" }\n").unwrap();

    let mut harness = harness_in(&deno_root);
    let original = configured_command(&harness, "typescript");
    let switched = settle(&mut harness, |h| {
        configured_command(h, "typescript").as_deref() == Some("deno")
    });
    assert!(
        switched,
        "the Deno project should switch to `deno lsp` first"
    );

    let plain_window = harness
        .editor_mut()
        .create_window_at(plain_root.clone(), "plain".to_string());
    harness.editor_mut().set_active_window(plain_window);

    let restored = settle(&mut harness, |h| {
        configured_command(h, "typescript") == original
    });
    assert!(
        restored,
        "the plain project should get its configured server back; got {:?}",
        configured_command(&harness, "typescript"),
    );
}
