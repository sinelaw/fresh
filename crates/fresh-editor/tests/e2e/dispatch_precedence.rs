//! Precedence GOLDEN tests: routing OUTCOMES over the real dispatch
//! entry points (`handle_mouse` / `handle_key`), pinning the facts the
//! chrome unit tests express only as rank relations. Every test here
//! is of the regression shape this architecture exists to prevent —
//! "the event reached the wrong surface" — so a refactor of the walks,
//! ranks, or guards cannot silently invert them.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::event::{
    PopupContentData, PopupData, PopupKindHint, PopupListItemData, PopupPositionData,
};

/// A centered, focused list popup with enough items to scroll.
fn show_scrolling_popup(harness: &mut EditorTestHarness) {
    let items = (0..40)
        .map(|i| PopupListItemData {
            text: format!("POPUP_ITEM_{i:02}"),
            detail: None,
            icon: None,
            data: None,
        })
        .collect();
    let popup = PopupData {
        kind: PopupKindHint::List,
        title: Some("Precedence Probe".to_string()),
        description: None,
        transient: false,
        content: PopupContentData::List { items, selected: 0 },
        position: PopupPositionData::Centered,
        width: 30,
        max_height: 8,
        bordered: true,
    };
    harness.editor_mut().show_popup(popup);
    harness.render().unwrap();
}

fn write_numbered_file(harness: &EditorTestHarness, lines: usize) -> std::path::PathBuf {
    let path = harness.editor().working_dir().join("precedence.txt");
    let mut s = String::new();
    for i in 1..=lines {
        s.push_str(&format!(
            "LINE{i} with a deliberately long tail {}\n",
            "x".repeat(200)
        ));
    }
    std::fs::write(&path, s).unwrap();
    path
}

/// The wheel over a popup scrolls the POPUP, never the buffer hidden
/// beneath it — the popup's opaque box takes the wheel in the walk
/// (rank/z above the split content), so the buffer viewport must not
/// move.
#[test]
fn wheel_over_popup_scrolls_popup_not_buffer() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let path = write_numbered_file(&harness, 200);
    harness.open_file(&path).unwrap();
    harness.render().unwrap();
    show_scrolling_popup(&mut harness);

    let (col, row) = harness
        .find_text_on_screen("POPUP_ITEM_02")
        .expect("popup visible");
    for _ in 0..5 {
        harness.mouse_scroll_down(col, row).unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("LINE1 "),
        "buffer viewport must not scroll under a wheel aimed at the popup.\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("POPUP_ITEM_00"),
        "the popup itself must have scrolled.\nScreen:\n{screen}"
    );
}

/// The HORIZONTAL wheel over a popup is absorbed (the popup has no
/// horizontal axis) — it must not pan the buffer hidden beneath. Pins
/// the R2 fix: without the popup's `on_hwheel` absorb arm the delta
/// chained down to the split's h-scroll.
#[test]
fn hwheel_over_popup_does_not_pan_buffer() {
    use crossterm::event::{MouseEvent, MouseEventKind};
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let path = write_numbered_file(&harness, 50);
    harness.open_file(&path).unwrap();
    harness.render().unwrap();
    show_scrolling_popup(&mut harness);

    let (col, row) = harness
        .find_text_on_screen("POPUP_ITEM_02")
        .expect("popup visible");
    for _ in 0..5 {
        harness
            .editor_mut()
            .handle_mouse(MouseEvent {
                kind: MouseEventKind::ScrollRight,
                column: col,
                row,
                modifiers: KeyModifiers::NONE,
            })
            .unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("LINE1 "),
        "the buffer must not pan horizontally under a horizontal wheel aimed at \
         the popup.\nScreen:\n{screen}"
    );
}

/// A click on a popup's chrome (its title row — inside the box, on no
/// interactive item) is ABSORBED by the popup's opaque box: it must
/// not fall through and move the buffer cursor beneath.
#[test]
fn click_on_popup_chrome_does_not_move_buffer_cursor() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let path = write_numbered_file(&harness, 50);
    harness.open_file(&path).unwrap();
    harness.render().unwrap();
    let cursor_before = harness.cursor_position();
    show_scrolling_popup(&mut harness);

    let (col, row) = harness
        .find_text_on_screen("Precedence Probe")
        .expect("popup title visible");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();

    assert_eq!(
        harness.cursor_position(),
        cursor_before,
        "a click on popup chrome must be absorbed by the opaque box, not move \
         the buffer cursor beneath.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// Workspace-trust keys BEAT an open prompt — the deliberate
/// convergence fix of the keyboard arc (`WORKSPACE_TRUST` (870) ranks
/// above `PROMPT` (850); dispatch now agrees with `get_key_context`,
/// which always resolved WT higher). With the command palette open
/// UNDER the trust prompt, a trust mnemonic must act on the trust
/// dialog and never reach the palette's input.
#[test]
fn workspace_trust_keys_beat_open_prompt() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    // Executable-content markers + a real store so an undecided
    // project raises the prompt (the trust_activation tests' recipe).
    let dir = harness.editor().working_dir().to_path_buf();
    std::fs::write(dir.join("Cargo.toml"), "[package]\nname = \"x\"\n").unwrap();
    let store_path = harness.editor().dir_context().project_state_dir(&dir);
    let store = fresh::services::workspace_trust::TrustStore::for_project_dir(&store_path);
    harness
        .editor()
        .authority()
        .workspace_trust
        .set_store(Some(store));

    // Open the command palette FIRST, then raise the trust prompt on
    // top of it.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.editor_mut().maybe_prompt_workspace_trust(true);
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("SECURITY WARNING"))
        .expect("trust prompt raised over the open palette");

    // A printable trust mnemonic: if the prompt outranked the trust
    // dialog (the pre-arc dispatch order), this would land in the
    // palette's input instead.
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("> t"),
        "the trust mnemonic must not reach the palette input beneath the trust \
         dialog.\nScreen:\n{screen}"
    );
}
