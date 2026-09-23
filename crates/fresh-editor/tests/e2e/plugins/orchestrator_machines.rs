//! Machines are added in one place and used everywhere.
//!
//! The Machines dialog is where a machine becomes usable: it lists the saved
//! machines and, beside them, the `~/.ssh/config` hosts not added yet. Every
//! other machine picker (New Workspace, Import sessions) lists only saved
//! machines, and has a `+ Add machine…` button that opens Add Machine and
//! comes back with the new machine picked.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crate::common::launch_form::{focus_stop, open_new_workspace_form, FORM_TITLE};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

const WIDTH: u16 = 160;
const HEIGHT: u16 = 50;

/// An editor whose resolved home holds `ssh_config` as its `~/.ssh/config`
/// (none when empty), with the plugins loaded.
fn editor_with_ssh_config(data_home: &tempfile::TempDir, ssh_config: &str) -> EditorTestHarness {
    fresh::i18n::set_locale("en");
    if !ssh_config.is_empty() {
        let ssh = data_home.path().join("home").join(".ssh");
        fs::create_dir_all(&ssh).unwrap();
        fs::write(ssh.join("config"), ssh_config).unwrap();
    }
    let dir_context = fresh::config_io::DirectoryContext::for_testing(data_home.path());
    let mut harness = EditorTestHarness::create(
        WIDTH,
        HEIGHT,
        HarnessOptions::new()
            .with_project_root()
            .with_shared_dir_context(dir_context),
    )
    .unwrap();
    harness.tick_and_render().unwrap();
    harness
}

/// Run a palette command once its plugin has registered it: Import sessions
/// comes from a different plugin than Machines, and they load independently.
fn run_palette(harness: &mut EditorTestHarness, command_name: &str) {
    harness
        .wait_until(|h| {
            let reg = h.editor().command_registry().read().unwrap();
            reg.get_all()
                .iter()
                .any(|c| c.get_localized_name().ends_with(command_name))
        })
        .unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text(command_name).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(command_name))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

fn open_machines_dialog(harness: &mut EditorTestHarness) {
    run_palette(harness, "Orchestrator: Machines");
    harness
        .wait_until(|h| h.screen_to_string().contains("┌ Machines"))
        .unwrap();
}

/// In Add Machine, on an editor with no `~/.ssh/config` (so there is no Host
/// picker and focus starts on Name): fill Name and Target, then Save.
fn fill_and_save_new_machine(harness: &mut EditorTestHarness, name: &str, target: &str) {
    harness
        .wait_until(|h| h.screen_to_string().contains("┌ Add Machine"))
        .unwrap();
    harness.type_text(name).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.type_text(target).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("┌ Add Machine"))
        .unwrap();
}

/// **The Machines dialog lists a config host that is not saved, as such.**
/// It reads the editor's resolved home, not the process's `$HOME`: this
/// harness redirects home, so a `$HOME` read would find no host at all.
#[test]
fn a_config_host_is_listed_as_not_added() {
    let data_home = tempfile::tempdir().unwrap();
    let mut harness = editor_with_ssh_config(
        &data_home,
        "Host plantedbox\n  HostName 10.0.0.9\n  User deploy\n",
    );
    open_machines_dialog(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("plantedbox"))
        .unwrap_or_else(|_| {
            panic!(
                "the host from the editor's own `~/.ssh/config` should be listed. Screen:\n{}",
                harness.screen_to_string()
            )
        });
    // It resolves the entry, not just the alias, and says it is not a machine.
    harness.assert_screen_contains("deploy@10.0.0.9");
    harness.assert_screen_contains("not added");
}

/// **An IPv6 literal is all colons, so a bare `host:port` is ambiguous.**
/// `sshResolvedTarget` joined the hostname and the port with a `:`, which for
/// `::1` and `22` gives `::1:22` — and `parseSshTarget`, reading the port as
/// whatever follows the last colon, then took the host to be `::1:` with port
/// `22`. The resolved target brackets the literal, which is the form `ssh`
/// itself takes and the form `parseSshTarget` can split unambiguously.
#[test]
fn an_ipv6_config_host_is_bracketed_so_its_port_survives() {
    let data_home = tempfile::tempdir().unwrap();
    let mut harness = editor_with_ssh_config(
        &data_home,
        "Host v6box\n  HostName 2001:db8::1\n  User deploy\n  Port 2222\n",
    );
    open_machines_dialog(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("v6box"))
        .unwrap();
    harness.assert_screen_contains("deploy@[2001:db8::1]:2222");
}

/// **On a config host the action is to add it.** Enter opens Add Machine
/// prefilled from the entry, and Save turns the row into a saved machine.
#[test]
fn enter_on_a_config_host_adds_it_as_a_machine() {
    let data_home = tempfile::tempdir().unwrap();
    let mut harness = editor_with_ssh_config(
        &data_home,
        "Host plantedbox\n  HostName 10.0.0.9\n  User deploy\n",
    );
    open_machines_dialog(&mut harness);
    // Local is first; the config host follows it.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Add as machine"))
        .unwrap_or_else(|_| {
            panic!(
                "a config host's primary action is to add it. Screen:\n{}",
                harness.screen_to_string()
            )
        });
    harness.assert_screen_not_contains("New workspace here");

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("┌ Add Machine"))
        .unwrap();
    // Name and Target come from the alias.
    harness.assert_screen_contains("[plantedbox");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::CONTROL)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("┌ Machines") && s.contains("plantedbox") && !s.contains("not added")
        })
        .unwrap_or_else(|_| {
            panic!(
                "the host should now be a saved machine. Screen:\n{}",
                harness.screen_to_string()
            )
        });
}

/// **`+ Add machine…` in New Workspace comes back with the machine picked.**
/// The form is set aside, not closed, so what was typed in it survives.
#[test]
fn the_form_adds_a_machine_and_picks_it() {
    let data_home = tempfile::tempdir().unwrap();
    let mut harness = editor_with_ssh_config(&data_home, "");
    open_new_workspace_form(&mut harness);
    // With no saved machine, the Machine control has no remote to offer.
    harness.assert_screen_not_contains("newbox");

    focus_stop(&mut harness, "▸ [ + Add machine");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    fill_and_save_new_machine(&mut harness, "newbox", "deploy@10.1.1.1");

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains(FORM_TITLE) && s.contains("[newbox")
        })
        .unwrap_or_else(|_| {
            panic!(
                "the form should come back on the new machine. Screen:\n{}",
                harness.screen_to_string()
            )
        });
    harness.assert_screen_contains("deploy@10.1.1.1");
}

/// **`+ Add machine…` in Import sessions comes back with the machine picked.**
/// A saved machine is one the scan can dial, so it is a scan target at once.
#[test]
fn import_sessions_adds_a_machine_and_picks_it() {
    let data_home = tempfile::tempdir().unwrap();
    let mut harness = editor_with_ssh_config(&data_home, "");
    run_palette(&mut harness, "Import sessions");
    harness
        .wait_until(|h| h.screen_to_string().contains("┌ Import sessions"))
        .unwrap();

    focus_stop(&mut harness, "▸ [ + Add machine");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    fill_and_save_new_machine(&mut harness, "newbox", "deploy@10.1.1.1");

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("┌ Import sessions") && s.contains("[newbox")
        })
        .unwrap_or_else(|_| {
            panic!(
                "Import sessions should come back on the new machine. Screen:\n{}",
                harness.screen_to_string()
            )
        });
}

/// **Cancelling Add Machine from Import sessions goes back to it unchanged.**
#[test]
fn import_sessions_survives_a_cancelled_add() {
    let data_home = tempfile::tempdir().unwrap();
    let mut harness = editor_with_ssh_config(&data_home, "");
    run_palette(&mut harness, "Import sessions");
    harness
        .wait_until(|h| h.screen_to_string().contains("┌ Import sessions"))
        .unwrap();

    focus_stop(&mut harness, "▸ [ + Add machine");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("┌ Add Machine"))
        .unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            !s.contains("┌ Add Machine") && s.contains("┌ Import sessions")
        })
        .unwrap_or_else(|_| {
            panic!(
                "Esc in Add Machine should return to Import sessions. Screen:\n{}",
                harness.screen_to_string()
            )
        });
}
