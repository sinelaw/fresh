//! Security regression for issue #2063: opening a `.cs` file must not run
//! `dotnet restore` (which evaluates attacker-controlled MSBuild targets in the
//! project's `.csproj`/`.sln` — arbitrary command execution) while the
//! workspace is untrusted.
//!
//! Core's spawn gate can't catch this on its own: `dotnet` is a trusted system
//! binary on `$PATH` and the danger is in the *argument* (the project path),
//! which the path-based Restricted gate never inspects. So `csharp_support.ts`
//! self-gates the restore on Workspace Trust, exactly as env-manager gates
//! activation. This test pins that behavior.
//!
//! Hermetic and non-vacuous: a fake `dotnet` on `$PATH` records every `restore`
//! invocation to a marker file, so we observe *actual* execution (CONTRIBUTING
//! §2 "observe, not inspect") without needing the real .NET SDK. Both halves
//! run so neither assertion can pass vacuously:
//!   1. Restricted → opening the `.cs` file records NO restore. (the fix)
//!   2. Trusted    → opening the same file DOES record a restore. (proves the
//!      plumbing works, so the negative half isn't vacuous, and the feature
//!      still works once the user trusts the folder.)
//!
//! Unix-only: the fake `dotnet` is a `#!/bin/sh` script, and the `$PATH`
//! shape is POSIX (same constraint as the fake-devcontainer tests).
#![cfg(unix)]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use fresh::config::Config;
use fresh::services::workspace_trust::TrustLevel;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// A malicious C# project: a target hooked `BeforeTargets="Restore"` runs an
/// arbitrary command during `dotnet restore`. We don't rely on the real SDK to
/// evaluate it — the fake `dotnet` stands in — but shipping the realistic
/// payload documents exactly what the gate is protecting against.
const EVIL_CSPROJ: &str = r#"<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
  </PropertyGroup>
  <Target Name="Pwn" BeforeTargets="Restore;CollectPackageReferences">
    <Exec Command="echo pwned" />
  </Target>
</Project>
"#;

const PROGRAM_CS: &str = "public class Program {\n    public static void Main() {}\n}\n";

/// Install `csharp_support` + its lib into the project's `plugins/` dir so the
/// real plugin loads when the harness boots against `project`.
fn setup_project(project: &Path, marker: &Path) {
    fs::write(project.join("evil.csproj"), EVIL_CSPROJ).unwrap();
    fs::write(project.join("Program.cs"), PROGRAM_CS).unwrap();

    let plugins_dir = project.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "csharp_support");
    copy_plugin_lib(&plugins_dir);

    // Fake `dotnet` on $PATH: records each `restore` to `marker`, answers
    // `--version` so the plugin's isDotnetAvailable() check passes. The marker
    // path is baked in (not passed via env) so the observation can't be foiled
    // by env capture rewriting the child's environment.
    let fake_bin = project.join("fake-bin");
    fs::create_dir_all(&fake_bin).unwrap();
    let dotnet = fake_bin.join("dotnet");
    fs::write(
        &dotnet,
        format!(
            "#!/bin/sh\n\
             case \"$1\" in\n\
             \x20 restore) printf 'restore %s\\n' \"$*\" >> '{}' ;;\n\
             \x20 --version) echo '8.0.100' ;;\n\
             esac\n\
             exit 0\n",
            marker.display()
        ),
    )
    .unwrap();
    let mut perms = fs::metadata(&dotnet).unwrap().permissions();
    use std::os::unix::fs::PermissionsExt;
    perms.set_mode(0o755);
    fs::set_permissions(&dotnet, perms).unwrap();

    // Prepend the fake bin so `spawnProcess("dotnet", …)` resolves to it.
    let old = std::env::var("PATH").unwrap_or_default();
    if !old.split(':').any(|p| Path::new(p) == fake_bin) {
        std::env::set_var("PATH", format!("{}:{}", fake_bin.display(), old));
    }
}

/// Boot a harness rooted at `project`, pin its trust level, publish it to the
/// plugin snapshot (so `editor.workspaceTrustLevel()` reads it), and open
/// `Program.cs` — the deliberate open that fires the `after_file_open` hook.
fn open_cs_at_trust(project: &Path, level: TrustLevel) -> EditorTestHarness {
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), project.into())
            .unwrap();
    harness
        .editor()
        .authority()
        .workspace_trust
        .set_level(level);
    harness.editor_mut().update_plugin_state_snapshot();
    harness.editor_mut().fire_plugins_loaded_hook();
    harness.render().unwrap();

    harness.open_file(&project.join("Program.cs")).unwrap();
    harness
}

/// Pump the async plugin bridge for a bounded spell so any restore the plugin
/// was going to run has had every chance to reach the fake `dotnet`.
fn pump(harness: &mut EditorTestHarness) {
    for _ in 0..40 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(25));
    }
}

fn restore_ran(marker: &Path) -> bool {
    fs::read_to_string(marker)
        .map(|s| s.contains("restore"))
        .unwrap_or(false)
}

#[test]
fn opening_cs_file_gates_dotnet_restore_on_trust() {
    let tmp = tempfile::TempDir::new().unwrap();
    let project: PathBuf = tmp.path().join("proj");
    fs::create_dir_all(&project).unwrap();
    let marker = tmp.path().join("restore_invocations.log");
    setup_project(&project, &marker);

    // ---- 1. Restricted: opening the .cs file must NOT run restore. ----
    let mut restricted = open_cs_at_trust(&project, TrustLevel::Restricted);
    pump(&mut restricted);
    assert!(
        !restore_ran(&marker),
        "SECURITY: `dotnet restore` ran while the workspace was Restricted \
         (issue #2063); marker:\n{:?}",
        fs::read_to_string(&marker).ok()
    );
    drop(restricted);

    // ---- 2. Trusted: opening the same file DOES run restore. ----
    // Proves the fake `dotnet` + plugin wiring works (so half 1 isn't vacuous)
    // and that the feature still works once the user trusts the folder.
    let mut trusted = open_cs_at_trust(&project, TrustLevel::Trusted);
    trusted
        .wait_until(|_| restore_ran(&marker))
        .expect("trusted workspace should run dotnet restore on opening a .cs file");
}
