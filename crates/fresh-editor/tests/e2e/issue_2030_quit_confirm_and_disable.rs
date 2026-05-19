//! Regression tests for issue #2030: quit confirmation and Ctrl+Q
//! cannot be disabled via config.
//!
//! Two sub-cases:
//!
//! A. `editor.confirm_quit` (new opt-in setting) — when true, `quit()`
//!    must surface a confirmation prompt even when no buffer is
//!    modified, so a stray `Ctrl+Q` doesn't tear down the workspace.
//!
//! B. Custom keybinding override for `Ctrl+Q` — when the user binds
//!    `Ctrl+Q` to `"none"` (or its `"noop"` alias) in their config,
//!    the binding must actually disable the default Quit action. The
//!    pre-fix resolver fell through to the default Normal binding's
//!    application-wide Quit in every non-Normal context, ignoring the
//!    user override (see `keybindings.rs:1820-1882`). And `"none"`
//!    wasn't even a recognized action name — `from_str` returned
//!    `None`, so the binding silently failed to load.
//!
//! Observability: each test drives a key event, then either inspects
//! the rendered prompt (the user-visible artifact of "I'm asking you
//! to confirm") or `should_quit()` (which is the user-visible "did
//! the editor actually quit" outcome — losing the editor would
//! itself be the failure mode).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, Keybinding};
use std::collections::HashMap;

/// A — with `editor.confirm_quit = true`, pressing `Ctrl+Q` on a
/// clean session must surface a confirmation prompt and *not* quit
/// until the user confirms.
#[test]
fn confirm_quit_setting_prompts_before_quitting_clean_session() {
    let mut config = Config::default();
    config.editor.confirm_quit = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 40, config).unwrap();

    // Precondition: no modified buffers, so without `confirm_quit`
    // this would quit immediately.
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();

    assert!(
        !harness.editor().should_quit(),
        "confirm_quit must intercept Ctrl+Q and surface a prompt instead of quitting; \
         should_quit was true"
    );

    let screen = harness.screen_to_string();
    assert!(
        screen.to_lowercase().contains("quit"),
        "confirm_quit must render a prompt referencing quit; screen:\n{screen}"
    );

    // The default response on Enter is cancel — must not quit.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    assert!(
        !harness.editor().should_quit(),
        "cancelling the prompt must not quit"
    );
}

/// A.2 — confirming the prompt (typing the localized "quit"/yes key)
/// must actually quit.
#[test]
fn confirm_quit_setting_quits_after_user_confirms() {
    let mut config = Config::default();
    config.editor.confirm_quit = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 40, config).unwrap();

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(!harness.editor().should_quit());

    // Confirm. The default English binding is "y" (yes).
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    assert!(
        harness.editor().should_quit(),
        "confirming the prompt must set should_quit"
    );
}

/// B — custom `Ctrl+Q → "noop"` keybinding must disable the default
/// `Action::Quit` even from contexts where Quit would otherwise be
/// application-wide.
#[test]
fn custom_noop_keybinding_disables_ctrl_q() {
    let mut config = Config::default();
    config.keybindings.push(Keybinding {
        key: "Q".to_string(),
        modifiers: vec!["Ctrl".to_string()],
        keys: Vec::new(),
        action: "noop".to_string(),
        args: HashMap::new(),
        when: None,
    });

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 40, config).unwrap();

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(
        !harness.editor().should_quit(),
        "custom Ctrl+Q → noop in Normal context must disable the default Quit \
         (the resolver's app-wide fallthrough used to ignore the user override)"
    );
}

/// B.2 — `"none"` is the spelling users most often try; accept it as
/// an alias for `"noop"` so `{\"action\": \"none\"}` actually loads
/// rather than silently failing to parse.
#[test]
fn custom_none_keybinding_disables_ctrl_q() {
    let mut config = Config::default();
    config.keybindings.push(Keybinding {
        key: "Q".to_string(),
        modifiers: vec!["Ctrl".to_string()],
        keys: Vec::new(),
        action: "none".to_string(),
        args: HashMap::new(),
        when: None,
    });

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 40, config).unwrap();

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(
        !harness.editor().should_quit(),
        "custom Ctrl+Q → \"none\" must disable Quit (Action::from_str now \
         accepts \"none\" as an alias for \"noop\")"
    );
}

/// B.3 — the keybinding editor UI deletes a default binding by
/// pushing a `Keybinding { action: "noop", when: Some("normal"), … }`
/// into the user's config (see
/// `app/keybinding_editor/editor.rs:879-944`). Exercise that exact
/// shape — lowercase key, lowercase modifier, explicit
/// `when: "normal"` — to lock in the UI-driven path that this issue
/// originally reported ("Can't disable quit key binding doesn't seem
/// to work if just delete it").
#[test]
fn keybinding_editor_delete_of_default_ctrl_q_disables_quit() {
    let mut config = Config::default();
    config.keybindings.push(Keybinding {
        key: "q".to_string(),
        modifiers: vec!["ctrl".to_string()],
        keys: Vec::new(),
        action: "noop".to_string(),
        args: HashMap::new(),
        when: Some("normal".to_string()),
    });

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 40, config).unwrap();

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(
        !harness.editor().should_quit(),
        "deleting Ctrl+Q via the keybinding editor (which pushes a `noop` \
         entry into the user's config in `normal` context) must actually \
         disable Quit"
    );
}
