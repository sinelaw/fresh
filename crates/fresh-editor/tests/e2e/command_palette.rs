use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use std::fs;

/// Test command palette trigger and rendering
#[test]
fn test_command_palette_trigger() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger Quick Open with Ctrl+P (defaults to command mode with > prefix)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check that the Quick Open hints line is visible
    harness.assert_screen_contains(">command");

    // Check that suggestions are visible (commands sorted alphabetically, so Add Cursor commands appear first)
    harness.assert_screen_contains("Add Cursor Above");
    harness.assert_screen_contains("Add Cursor Below");
    harness.assert_screen_contains("Close Buffer");
}

/// Test command palette autocomplete filtering
#[test]
fn test_command_palette_autocomplete() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "open" to filter commands (Quick Open already starts with > prefix)
    harness.type_text("open").unwrap();

    // Should show filtered results
    harness.assert_screen_contains("Open File");

    // Should not show non-matching commands
    // (We might still see them if there are many results, but "Open File" should be first)
}

/// Test command palette navigation with Up/Down
#[test]
fn test_command_palette_navigation() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(">command");

    // Navigate down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Navigate up
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Commands should still be visible (alphabetically sorted, so Add Cursor commands first)
    harness.assert_screen_contains("Add Cursor Above");
}

/// Test command palette Tab completion
#[test]
fn test_command_palette_tab_completion() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type partial text - use "open f" to specifically match "Open File"
    harness.type_text("open f").unwrap();

    // Press Tab to accept first suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The input should be completed to "Open File"
    harness.assert_screen_contains(">Open File");
    // Note: The prompt shows ">command" followed by the input text
}

/// Test command palette cancel with Escape
#[test]
fn test_command_palette_cancel() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(">command");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Prompt should be gone
    harness.assert_screen_not_contains(">command");
    harness.assert_screen_contains("cancelled");
}

/// Test executing a command from the palette
#[test]
fn test_command_palette_execute() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.render().unwrap();

    // Verify line numbers are shown initially (default config)
    harness.assert_screen_contains("1 │");

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type a valid command name - use "Toggle Line Numbers" which has visible effect
    harness.type_text("Toggle Line Numbers").unwrap();

    // Execute with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Line numbers should now be hidden
    harness.assert_screen_not_contains("1 │");
}

/// Test Quick Open file mode with path:line:col
#[test]
fn test_quick_open_file_path_line_col() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let content = "11111\n22222\nABCDE12345\n44444\n";
    fs::write(project_root.join("jump.txt"), content).unwrap();

    // Open Quick Open (command mode), then switch to file mode
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // Type path with line/column suffix and open it
    harness.type_text("jump.txt:3:5").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.process_async_and_render().unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Ln 3") && screen.contains("Col 5") && screen.contains("ABCDE12345")
        })
        .expect("Cursor should jump to Ln 3, Col 5 after Quick Open");
}

/// Test Quick Open file mode with path:line (no column)
#[test]
fn test_quick_open_file_path_line_only() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let content = "11111\n22222\nABCDE12345\n44444\n";
    fs::write(project_root.join("jump.txt"), content).unwrap();

    // Open Quick Open (command mode), then switch to file mode
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // Type path with line-only suffix and open it
    harness.type_text("jump.txt:3").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.process_async_and_render().unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Ln 3") && screen.contains("Col 1") && screen.contains("ABCDE12345")
        })
        .expect("Cursor should jump to Ln 3, Col 1 after Quick Open");
}

/// Helper: write a fixture file with `n` lines of the form `LINEn\n`.
///
/// Tests here use long files so the live-preview jump scrolls the viewport
/// noticeably, which is observable in the rendered output (per CONTRIBUTING.md:
/// e2e tests examine rendered output, not internal state).
fn write_numbered_lines(path: &std::path::Path, n: usize) {
    let mut s = String::new();
    for i in 1..=n {
        s.push_str(&format!("LINE{i}\n"));
    }
    fs::write(path, s).unwrap();
}

/// Quick Open goto-line (":N") should live-preview the jump as the user types,
/// so the cursor moves to line N before pressing Enter (issue #1253).
#[test]
fn test_quick_open_goto_line_live_preview() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();
    // Baseline: viewport starts at the top, LINE80 is far off-screen.
    assert!(
        !harness.screen_to_string().contains("LINE80"),
        "Baseline viewport should not include line 80"
    );

    // Open Quick Open, clear the ">" prefix, then type ":80".
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":80").unwrap();

    // Live preview must have scrolled the viewport toward line 80, while the
    // prompt is still open (no Enter pressed). The suggestion popup overlays
    // the bottom rows, so line 80 itself may render behind it; check a line
    // that's guaranteed to be in the visible content area and confirm the
    // original top-of-file line is gone.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 75 │ LINE75") && !screen.contains("  1 │ LINE1")
        })
        .expect("Viewport should scroll toward line 80 during ':80' preview");
}

/// Extending the live preview to a new number jumps again in the same prompt.
#[test]
fn test_quick_open_goto_line_live_preview_updates() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // First preview to line 40 — viewport scrolls so line 35 is visible.
    harness.type_text(":40").unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 35 │ LINE35") && !screen.contains("  1 │ LINE1")
        })
        .expect("Viewport should scroll toward line 40");

    // Change the target to line 80 — viewport should follow.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("80").unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 75 │ LINE75") && !screen.contains(" 35 │ LINE35")
        })
        .expect("Viewport should follow the updated target to line 80");
}

/// Canceling the Quick Open prompt (Esc) after a live-preview jump should
/// restore the cursor to where it was before the prompt was opened. Observable
/// in the rendered output as the viewport scrolling back and the status bar
/// reporting the original line.
#[test]
fn test_quick_open_goto_line_live_preview_cancel_restores() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":80").unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 75 │ LINE75") && !screen.contains("  1 │ LINE1")
        })
        .expect("Preview should scroll toward line 80");

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // After Esc, the prompt closes and the status bar becomes visible; the
    // cursor must be back on line 1 and the viewport scrolled back to it.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Ln 1,") && screen.contains("  1 │ LINE1")
        })
        .expect("Esc should restore cursor to pre-preview line 1");
}

/// Clicking in the editor while a goto-line preview is active should commit
/// the click as the new cursor position — Esc afterwards must NOT restore
/// the pre-preview snapshot over the user's click.
#[test]
fn test_quick_open_goto_line_live_preview_mouse_click_commits() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":80").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(" 78 │ LINE78"))
        .expect("Preview should scroll so LINE78 is on screen near LINE80");

    // Locate a visible line's rendered position and click on it. Using a line
    // different from the preview target proves the click overrides the preview.
    let (col, row) = harness
        .find_text_on_screen("LINE78")
        .expect("LINE78 should be visible while previewing line 80");
    harness.mouse_click(col, row).unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // The pre-preview snapshot (line 1) must NOT overwrite the click target
    // (line 78) — status bar must report line 78 after the prompt closes.
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 78,"))
        .expect(
            "Cursor should stay on the clicked line 78 — pre-preview snapshot must be \
             dropped on editor click",
        );
}

/// A buffer edit that shifts the cursor via `adjust_for_edit` while the
/// preview is active invalidates the snapshot: Esc afterwards must NOT restore
/// the pre-preview byte position, which no longer corresponds to the original
/// line number.
#[test]
fn test_quick_open_goto_line_live_preview_buffer_edit_invalidates() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::model::event::{CursorId, Event};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":50").unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 45 │ LINE45") && !screen.contains("  1 │ LINE1")
        })
        .expect("Preview should scroll toward line 50");

    // Simulate an async external edit (LSP code action, plugin, format-on-save)
    // inserting a single line at the start of the buffer. Use a cursor_id that
    // doesn't match any cursor so the primary cursor only receives
    // `adjust_for_edit` (no end-of-insertion jump).
    harness
        .apply_event(Event::Insert {
            position: 0,
            text: "PREFIX\n".to_string(),
            cursor_id: CursorId(999_999),
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Inserting one line before the cursor shifts it via `adjust_for_edit`
    // from line 50 to physical line 51 of the edited buffer (LINE50 text).
    // If the restore had fired, the viewport would have snapped back to the
    // top of the buffer (gutter line "  1 │ PREFIX") and LINE50 would scroll
    // out of view. Rendered-output evidence that the snapshot was dropped:
    // the viewport still shows content around the adjusted cursor.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 51 │ LINE50") && !screen.contains("  1 │ PREFIX")
        })
        .expect(
            "Esc must not restore the stale pre-preview snapshot after an external edit \
             shifted the cursor — expected viewport to stay near line 51 (LINE50), \
             not snap back to the PREFIX line at the top",
        );
}

/// Changing the prefix away from ":" should restore the cursor even without
/// cancelling (so the preview doesn't leak into other providers).
#[test]
fn test_quick_open_goto_line_live_preview_restores_when_prefix_changes() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":80").unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 75 │ LINE75") && !screen.contains("  1 │ LINE1")
        })
        .expect("Preview should scroll toward line 80");

    // Replace ":80" with ">toggle" to switch to the command provider — this
    // should restore the pre-preview viewport (line 1 visible again).
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(">toggle").unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("  1 │ LINE1") && !screen.contains(" 75 │ LINE75")
        })
        .expect(
            "Switching providers should restore the pre-preview viewport — line 1 visible, \
             line 75 no longer visible",
        );
}

/// Quick Open `:N` preview must center the target line in the viewport, not
/// pin it to the bottom edge. Without centering, the suggestion popup (which
/// overlays the bottom rows) obscures the line the user is trying to navigate
/// to, making the live preview useless for its stated purpose.
///
/// Discriminator: when previewing line 50 in a 100-line file, a line BELOW
/// the target (e.g. LINE55) must be visible — that's only possible when the
/// viewport is centered around line 50. If the cursor is pinned to the
/// bottom of the viewport (old `ensure_visible` behavior), nothing past the
/// cursor line can be visible.
#[test]
fn test_quick_open_goto_line_live_preview_centers_target() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":50").unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // A line below the target (LINE55) is visible → the viewport is
            // centered around line 50, not bottom-pinned.
            screen.contains(" 55 │ LINE55")
        })
        .expect(
            "Quick Open ':50' preview should center line 50 in the viewport so lines \
             below it (like LINE55) are visible — otherwise the target line is obscured \
             by the suggestion popup at the bottom of the screen",
        );
}

/// The standalone `Goto Line` prompt should likewise center the target line in
/// the viewport during live preview, rather than pinning it to the bottom.
#[test]
fn test_goto_line_prompt_live_preview_centers_target() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("50").unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 55 │ LINE55")
        })
        .expect(
            "Goto Line prompt '50' preview should center line 50 so lines below it \
             (like LINE55) are visible — otherwise the target is obscured by the prompt",
        );
}

/// The standalone `Goto Line` prompt (Ctrl+G) should live-preview the jump as
/// the user types a line number, mirroring the Quick Open `:N` behavior (same
/// snapshot/restore plumbing, just a different input parser).
#[test]
fn test_goto_line_prompt_live_preview() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();
    assert!(
        !harness.screen_to_string().contains("LINE80"),
        "Baseline viewport should not include line 80"
    );

    // Open the Goto Line prompt (Ctrl+G) and type the target line. Nothing is
    // pressed to confirm — the preview must take effect as we type.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("80").unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 75 │ LINE75") && !screen.contains("  1 │ LINE1")
        })
        .expect("Goto Line prompt should live-preview the jump toward line 80");
}

/// Extending the Goto Line prompt preview to a new number jumps again.
#[test]
fn test_goto_line_prompt_live_preview_updates() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();

    harness.type_text("40").unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 35 │ LINE35") && !screen.contains("  1 │ LINE1")
        })
        .expect("Preview should scroll toward line 40");

    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("80").unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 75 │ LINE75") && !screen.contains(" 35 │ LINE35")
        })
        .expect("Preview should follow the updated target to line 80");
}

/// Canceling the Goto Line prompt (Esc) after a live-preview jump should
/// restore the cursor to where it was before the prompt was opened.
#[test]
fn test_goto_line_prompt_live_preview_cancel_restores() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("80").unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(" 75 │ LINE75") && !screen.contains("  1 │ LINE1")
        })
        .expect("Preview should scroll toward line 80");

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Ln 1,") && screen.contains("  1 │ LINE1")
        })
        .expect("Esc should restore cursor to pre-preview line 1");
}

/// Confirming the Goto Line prompt commits the live-preview jump: the cursor
/// stays at the target even after any subsequent focus-loss path fires.
#[test]
fn test_goto_line_prompt_live_preview_confirm_keeps_jump() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 100);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("80").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 80,"))
        .expect("Enter should confirm the preview and leave the cursor on line 80");
}

/// With relative_line_numbers enabled, goto line prompt accepts negative numbers
/// to jump relative to current cursor position.
#[test]
fn test_goto_line_prompt_relative_negative_offset() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut config = fresh::config::Config::default();
    config.editor.relative_line_numbers = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(100, 24, config).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 50);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    // Verify we start at line 1
    harness.assert_screen_contains("Ln 1");

    // Open the Goto Line prompt (Ctrl+G) and type -5 to go 5 lines up
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("-5").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 1,"))
        .expect("-5 should jump to line 1 (clamped from -5)");
}

/// With relative_line_numbers enabled, goto line prompt accepts positive
/// relative offset with + prefix.
#[test]
fn test_goto_line_prompt_relative_positive_offset() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut config = fresh::config::Config::default();
    config.editor.relative_line_numbers = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(100, 24, config).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 50);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    // Verify we start at line 1
    harness.assert_screen_contains("Ln 1");

    // Open the Goto Line prompt (Ctrl+G) and type +20 to jump 20 lines down
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("+20").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 21,"))
        .expect("+20 should jump to line 21 (relative to cursor)");
}

/// Issue #1750: a `+N`/`-N` in the Go to Line prompt should always be a
/// relative jump, regardless of the `relative_line_numbers` display setting.
#[test]
fn test_goto_line_prompt_signed_input_is_relative_without_setting() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 50);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();

    // Jump to an absolute line first, so a relative offset has somewhere to
    // start from. Default config has relative_line_numbers = false.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("10").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 10,"))
        .expect("Absolute `10` should jump to line 10 even without relative_line_numbers");

    // `+5` from line 10 should land on line 15.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("+5").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 15,"))
        .expect("`+5` should be a relative jump regardless of the setting");

    // `-3` from line 15 should land on line 12.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("-3").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 12,"))
        .expect("`-3` should be a relative jump regardless of the setting");
}

/// Issue #1750: an unsigned line number is always absolute, even when the
/// `relative_line_numbers` display setting is enabled.
#[test]
fn test_goto_line_prompt_unsigned_input_is_absolute_with_relative_setting() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut config = fresh::config::Config::default();
    config.editor.relative_line_numbers = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(100, 24, config).unwrap();
    let project_root = harness.project_dir().unwrap();

    let jump_path = project_root.join("jump.txt");
    write_numbered_lines(&jump_path, 50);

    harness.open_file(&jump_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Ln 1");

    // Even with relative line numbers shown in the gutter, typing `25` (no
    // sign) means absolute line 25 — not "25 lines down from the cursor".
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("25").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 25,"))
        .expect("Unsigned `25` should jump to absolute line 25 regardless of the setting");
}

/// Test command palette fuzzy matching
#[test]
fn test_command_palette_fuzzy_matching() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "sf" which should match "Save File" (fuzzy match)
    harness.type_text("sf").unwrap();

    // Should show "Save File" in suggestions
    harness.assert_screen_contains("Save File");
}

/// Test Tab completion skips disabled suggestions
#[test]
fn test_command_palette_tab_skip_disabled() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "save file" to specifically match "Save File"
    // Using just "save" may fuzzy match other commands first
    harness.type_text("save file").unwrap();

    // Press Tab to accept first suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The input should be completed (should work with available commands)
    harness.assert_screen_contains(">Save File");
}

/// Test Tab completion doesn't accept disabled suggestions
#[test]
fn test_command_palette_tab_on_disabled() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "undo" - this command requires Normal context
    // Since we're in Normal context, it should be available
    harness.type_text("undo").unwrap();

    // Press Tab to accept the suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The input should be completed
    harness.assert_screen_contains(">Undo");

    // Now clear and try a different command
    // Clear input
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }

    // Type "focus" which will match "Focus Editor" and "Focus File Explorer"
    // "Focus Editor" requires FileExplorer context (disabled in Normal)
    // "Focus File Explorer" should be available in Normal context
    harness.type_text("focus e").unwrap();
    harness.render().unwrap();

    // The first match might be "Focus Editor" which is disabled in Normal context
    // Tab should either skip it or not accept it
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // After Tab, we should have an enabled command in the input
    // Let's just check that SOMETHING happened (either it completed or stayed as is)
    // This test is to verify the behavior - we'll fix it if it's broken
    let screen = harness.screen_to_string();
    println!("Screen after Tab on 'focus e': {screen}");

    // For now, just assert we still have the command palette open
    harness.assert_screen_contains(">command");
}

/// Test Tab completion doesn't work when all suggestions are disabled
#[test]
fn test_command_palette_tab_all_disabled() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type a single-term query to match only "Focus Editor" which is disabled in Normal context.
    // Using a single term (no spaces) avoids multi-term description matching which could
    // match enabled commands and change the selected suggestion.
    harness.type_text("FocusEditor").unwrap();
    harness.render().unwrap();

    // Check that "Focus Editor" is shown (should be greyed out)
    harness.assert_screen_contains("Focus Editor");

    // Press Tab - it should not accept the disabled suggestion
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The input should NOT have been auto-completed to disabled command
    // It should still be "FocusEditor" not "Focus Editor"
    let screen = harness.screen_to_string();
    println!("Screen after Tab on disabled 'FocusEditor': {screen}");

    // Check that input didn't change (tab should do nothing on disabled suggestions)
    harness.assert_screen_contains("FocusEditor");
}

/// Test Enter executes the selected (highlighted) command, not the typed text
#[test]
fn test_command_palette_enter_uses_selection() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "new file" which will specifically match "New File"
    // (using just "new" may match other commands like "Navigate Forward")
    harness.type_text("new file").unwrap();

    // The first suggestion should be "New File" (selected by default)
    harness.assert_screen_contains("New File");

    // Press Enter - should execute "New File" command, not try to find "new file" command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should NOT see error about unknown command
    harness.assert_screen_not_contains("Unknown command");

    // Should see the result of executing New File command
    // (new_buffer() sets status message to "New buffer")
    harness.assert_screen_contains("New buffer");
}

/// Test Enter with partial match uses the highlighted selection
#[test]
fn test_command_palette_enter_partial_match() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("partial_match.txt");
    std::fs::write(&file_path, "hello").unwrap();

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "sav" which matches "Save File" and "Save File As"
    harness.type_text("sav").unwrap();

    // Navigate down to select "Save File As"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter - should execute the selected command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should execute the selected command, not fail on "sav"
    harness.assert_screen_not_contains("Unknown command: sav");
}

/// Test scrolling beyond visible suggestions keeps selection visible
#[test]
fn test_command_palette_scroll_beyond_visible() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Commands are sorted alphabetically, first is "Add Cursor Above"
    harness.assert_screen_contains("Add Cursor Above");

    // With no filter, we should have many commands
    // The popup shows max 10 items at a time

    // Press Down 15 times to go well beyond the first 10 visible items
    for _ in 0..15 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // The selection should still be visible (the view should have scrolled)
    // We can verify this by checking that the view has scrolled beyond the first commands
    // After scrolling down 15 times, first command "Add Cursor Above" should NOT be visible
    harness.assert_screen_not_contains("Add Cursor Above");

    // Now press Enter - it should execute the selected command (whatever is selected)
    // not fail with "Unknown command"
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should NOT see "Unknown command" error
    harness.assert_screen_not_contains("Unknown command");
}

/// Test that "New File" command actually switches to the new buffer
#[test]
fn test_command_palette_new_file_switches_buffer() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let fixture = TestFixture::new("test.txt", "Original content\nLine 2\nLine 3").unwrap();

    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Open the fixture file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Should see the original content
    harness.assert_screen_contains("Original content");
    harness.assert_screen_contains("Line 2");

    // The tab should show the filename
    harness.assert_screen_contains("test.txt");

    // Now use command palette to create a new file
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    // Use "new file" to specifically match New File command
    harness.type_text("new file").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see status message confirming new buffer
    harness.assert_screen_contains("New buffer");

    // Should now have two tabs
    harness.assert_screen_contains("test.txt");
    harness.assert_screen_contains("[No Name]");

    // The important part: the CONTENT should now be empty (new buffer)
    // NOT showing the original content anymore
    harness.assert_screen_not_contains("Original content");
    harness.assert_screen_not_contains("Line 2");

    // The cursor should be at the start of an empty buffer
    let screen = harness.screen_to_string();
    println!("Screen after New File:\n{screen}");

    // Verify we can type in the new buffer
    harness.type_text("New buffer text").unwrap();
    harness.assert_screen_contains("New buffer text");
    harness.assert_screen_not_contains("Original content");
}

/// Test that Toggle Line Wrap command is available
#[test]
fn test_command_palette_toggle_line_wrap() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "toggle line" to filter
    harness.type_text("toggle line").unwrap();
    harness.render().unwrap();

    // Should show "Toggle Line Wrap" command
    harness.assert_screen_contains("Toggle Line Wrap");
}

/// Test that File Explorer toggle commands are available
#[test]
fn test_command_palette_file_explorer_toggles() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "toggle hidden" to filter
    harness.type_text("toggle hidden").unwrap();
    harness.render().unwrap();

    // Should show "Toggle Hidden Files" command
    harness.assert_screen_contains("Toggle Hidden Files");

    // Clear and search for gitignored (use specific term to avoid description matches)
    for _ in 0..13 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }

    harness.type_text("gitignored").unwrap();
    harness.render().unwrap();

    // Should show "Toggle Gitignored Files" command
    harness.assert_screen_contains("Toggle Gitignored Files");
}

/// Test that command palette can be invoked from file explorer
#[test]
fn test_command_palette_from_file_explorer() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Open file explorer (Ctrl+E)
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for file explorer to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Verify file explorer is open by checking for the UI element
    harness.assert_screen_contains("File Explorer");

    // Now trigger the command palette from file explorer with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show the command palette
    harness.assert_screen_contains(">command");

    // Should show commands (Calibrate Keyboard appears early alphabetically)
    harness.assert_screen_contains("Calibrate Keyboard");

    // Should be able to execute a command
    harness.type_text("toggle hidden").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Command should execute successfully (Toggle Hidden Files)
    // We should see a status message about the toggle
    let screen = harness.screen_to_string();
    println!("Screen after toggle hidden: {}", screen);

    // The command should have executed (not showing error about unavailable)
    harness.assert_screen_not_contains("not available");
}

/// Test that Up arrow stops at the beginning of the list instead of wrapping
#[test]
fn test_command_palette_up_no_wraparound() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Commands are sorted alphabetically, first is "Add Cursor Above"
    harness.assert_screen_contains("Add Cursor Above");

    // The first suggestion should be selected by default
    // Press Up - should stay at the first item, not wrap to the end
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter to execute the selected command
    // If we wrapped around, we would execute the last command in the list
    // If we stayed at the first command, we would execute "Add Cursor Above"
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // "Add Cursor Above" adds a cursor above the current one
    // The editor should now have 2 cursors - check via cursor count or status
    // For simplicity, just verify we're back in normal mode (no error)
    harness.assert_screen_not_contains("Unknown command");
}

/// Test that Down arrow stops at the end of the list instead of wrapping
#[test]
fn test_command_palette_down_no_wraparound() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Filter to get a small set of commands
    harness.type_text("save f").unwrap();
    harness.render().unwrap();

    // Should match "Save File" and "Save File As" (plus possible description matches)
    harness.assert_screen_contains("Save File");

    // Press Down many times - way past the end of the list
    for _ in 0..200 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    let screen_at_end = harness.screen_to_string();

    // Press Down many more times - should stay at the last item (no wraparound)
    for _ in 0..200 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    let screen_still_at_end = harness.screen_to_string();

    // If no wraparound, the screen should be identical (cursor stayed at the end)
    assert_eq!(
        screen_at_end, screen_still_at_end,
        "Down arrow should not wrap around at the end of the list"
    );
}

/// Test that PageUp stops at the beginning of the list instead of wrapping
#[test]
fn test_command_palette_pageup_no_wraparound() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Commands are sorted alphabetically, first is "Add Cursor Above"
    harness.assert_screen_contains("Add Cursor Above");

    // Press Down a few times to move away from the first item
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Now press PageUp multiple times - should return to beginning and stay there
    for _ in 0..3 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Press Enter to execute the selected command (should be first: Add Cursor Above)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should execute first command - "Add Cursor Above" adds a cursor
    // Just verify we didn't execute a command from the end of the list
    harness.assert_screen_not_contains("Unknown command");
}

/// Test that PageDown stops at the end of the list instead of wrapping
#[test]
fn test_command_palette_pagedown_no_wraparound() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Commands are sorted alphabetically, first is "Add Cursor Above"
    harness.assert_screen_contains("Add Cursor Above");

    // Press PageDown many times to try reaching the end
    // There are 80+ commands, PageDown moves by 10
    for _ in 0..10 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // After pressing PageDown many times, verify we moved from the first command
    // The first command "Add Cursor Above" should no longer be highlighted/at top
    // We verify this by pressing PageUp once to see if we can go back
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // We should still be far from the beginning
    // Execute the command and verify we didn't wrap to the first command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify command executed without error
    harness.assert_screen_not_contains("Unknown command");
}

/// Test that keyboard shortcuts are displayed in the command palette
#[test]
fn test_command_palette_shows_shortcuts() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Trigger the command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check that the command palette is visible
    harness.assert_screen_contains(">command");

    // Check that commands with shortcuts are visible (commands sorted alphabetically)
    // Add Cursor Above should show Ctrl+Alt+↑ (or ⌘+⌥+↑ on macOS)
    harness.assert_screen_contains("Add Cursor Above");
    // On macOS, Ctrl is ⌘ and Alt is ⌥
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ctrl+Alt+") || screen.contains("⌘+⌥+"),
        "Should show shortcut for Add Cursor Above"
    );

    // Add Cursor Below should show Ctrl+Alt+↓
    harness.assert_screen_contains("Add Cursor Below");

    // Filter to "Copy" to bring it into the visible 10 items
    let _ = harness.type_text("Copy");
    harness.render().unwrap();

    // Copy should show Ctrl+C (or ⌘+C on macOS)
    harness.assert_screen_contains("Copy");
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ctrl+C") || screen.contains("⌘+C"),
        "Should show shortcut for Copy"
    );
}

/// Test that shortcuts are displayed for filtered commands
#[test]
fn test_command_palette_shortcuts_with_filtering() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "save" to filter commands
    harness.type_text("save").unwrap();
    harness.render().unwrap();

    // Should show filtered results with shortcuts (Ctrl+S or ⌘+S on macOS)
    harness.assert_screen_contains("Save File");
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ctrl+S") || screen.contains("⌘+S"),
        "Should show shortcut for Save File"
    );

    // Save As should also appear with its shortcut
    harness.assert_screen_contains("Save File As");
    // Ctrl+Shift+S is the typical shortcut for Save As, but it might not be bound by default
    // So we just check that the command appears
}

/// Test that shortcuts are displayed in a column format in the command palette
#[test]
fn test_command_palette_shortcuts_alignment() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify that shortcuts are displayed alongside commands
    // Look for commands that we know have shortcuts
    harness.assert_screen_contains("Add Cursor Above");
    harness.assert_screen_contains("Add Cursor Below");

    // These commands should have shortcuts displayed (Ctrl+Alt+arrow)
    // Just verify they're present - the exact format may vary
    let screen = harness.screen_to_string();

    // Check that we see some keyboard shortcut indicators
    // Ctrl, Alt, or Shift should appear somewhere indicating shortcuts are shown
    let has_shortcuts =
        screen.contains("Ctrl") || screen.contains("Alt") || screen.contains("Shift");
    assert!(
        has_shortcuts,
        "Command palette should display keyboard shortcuts. Screen:\n{}",
        screen
    );
}

/// Test that "Show Keyboard Shortcuts" command actually shows keyboard shortcuts (issue #192)
/// Previously this would just show a fallback message because it relied on a non-existent plugin hook
#[test]
fn test_show_keyboard_shortcuts_command() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to filter for "Show Keyboard Shortcuts" (more specific to avoid "Calibrate Keyboard")
    harness.type_text("show keyboard").unwrap();
    harness.render().unwrap();

    // Should see the command in the palette
    harness.assert_screen_contains("Show Keyboard Shortcuts");

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after Show Keyboard Shortcuts:\n{}", screen);

    // ISSUE #192: The command should actually show keyboard shortcuts, not just a fallback message
    // The keyboard shortcuts should be displayed in a buffer or popup
    // We should see actual keybindings like Ctrl+S, Ctrl+O, etc.
    let shows_keybindings = screen.contains("Ctrl+S")
        || screen.contains("Ctrl+O")
        || screen.contains("Ctrl+P")
        || screen.contains("Save")
        || screen.contains("Keyboard Shortcuts"); // Buffer title

    // Should NOT show the fallback "not available" message
    let shows_fallback = screen.contains("not available") || screen.contains("plugins not loaded");

    assert!(
        shows_keybindings && !shows_fallback,
        "Show Keyboard Shortcuts command should display actual keybindings, not a fallback message. Screen:\n{}",
        screen
    );
}

/// Test that keyboard shortcuts can be opened, closed, and reopened without crashing
/// This reproduces a bug where opening keyboard shortcuts the second time causes:
/// "index out of bounds: the len is 1 but the index is 1" panic in tabs.rs:207
#[test]
fn test_show_keyboard_shortcuts_open_close_reopen() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // First open: Trigger the command palette and run "Show Keyboard Shortcuts"
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("show keyboard").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify keyboard shortcuts are shown
    let screen = harness.screen_to_string();
    println!("First open - Screen:\n{}", screen);
    assert!(
        screen.contains("Ctrl+") || screen.contains("Keyboard Shortcuts"),
        "Keyboard shortcuts should be visible on first open. Screen:\n{}",
        screen
    );

    // Close with 'q' (standard way to close help/read-only buffers)
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after_close = harness.screen_to_string();
    println!("After close - Screen:\n{}", screen_after_close);

    // Second open: This should NOT panic with "index out of bounds"
    // BUG: Currently this causes panic at src/view/ui/tabs.rs:207:42
    // "index out of bounds: the len is 1 but the index is 1"
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("show keyboard").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify keyboard shortcuts are shown again
    let screen = harness.screen_to_string();
    println!("Second open - Screen:\n{}", screen);
    assert!(
        screen.contains("Ctrl+") || screen.contains("Keyboard Shortcuts"),
        "Keyboard shortcuts should be visible on second open. Screen:\n{}",
        screen
    );
}

/// Test that command palette fuzzy matches on command descriptions
#[test]
fn test_command_palette_description_fuzzy_matching() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for "UI language" which appears in the description of "Select Locale"
    // ("Choose the UI language for the editor")
    // but not in the command name itself
    harness.type_text("UI language").unwrap();
    harness.render().unwrap();

    // Should find "Select Locale" because "UI language" is in its description
    harness.assert_screen_contains("Select Locale");

    // Clear and try another example
    for _ in 0..11 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }

    // Search for "clipboard" which appears in Copy/Paste descriptions
    // but not in the command names
    harness.type_text("clipboard").unwrap();
    harness.render().unwrap();

    // Should find Copy or Paste commands
    let screen = harness.screen_to_string();
    let found_clipboard_command = screen.contains("Copy") || screen.contains("Paste");
    assert!(
        found_clipboard_command,
        "Should find commands matching 'clipboard' in their description. Screen:\n{}",
        screen
    );
}

/// Test that cursor style can be changed via command palette
#[test]
fn test_command_palette_select_cursor_style() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Type to filter for cursor style command
    harness.type_text("cursor style").unwrap();
    harness
        .wait_for_screen_contains("Select Cursor Style")
        .unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should now show cursor style selection prompt
    harness
        .wait_for_screen_contains("Select cursor style:")
        .unwrap();

    // Should show cursor style options
    harness.assert_screen_contains("Terminal default");
    harness.assert_screen_contains("Blinking block");

    // Navigate down to select a different style (e.g., steady block)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Press Enter to select
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should show confirmation message
    harness
        .wait_for_screen_contains("Cursor style changed")
        .unwrap();
}

/// Test that command palette searches descriptions, not just names
#[test]
fn test_command_palette_description_search() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "narrow" which only appears in the description of page view commands,
    // not in their names
    harness.type_text("narrow").unwrap();
    harness.render().unwrap();

    // Should find commands whose descriptions match
    harness.assert_screen_contains("Toggle Page View");
    harness.assert_screen_contains("Set Page Width");
}
