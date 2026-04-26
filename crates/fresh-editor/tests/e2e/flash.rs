//! End-to-end tests for the `flash` plugin (label-based jump
//! navigation).  The plugin lives at
//! `crates/fresh-editor/plugins/flash.ts` and these tests load it via
//! the same `copy_plugin` mechanism vi_mode tests use, so they
//! exercise the actual production plugin source.
//!
//! These are also the production-code regression test for plugin
//! API #1 (`editor.getNextKey()`) when used by a plugin that does
//! NOT also use `defineMode` bindings.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::PathBuf;

/// Build a harness with the `flash` plugin loaded into an isolated
/// per-test project directory.  Returns the harness, the TempDir
/// guard (must outlive the harness), and the project root path —
/// callers should put any test fixtures **under that root** so that
/// the editor displays them with short relative paths in the status
/// bar.  Long absolute paths (like macOS `/private/var/folders/…`
/// temp paths) push the rest of the status bar off the visible
/// area, including the plugin's own `Flash[…]` text the tests wait
/// on.
fn flash_harness(width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir, PathBuf) {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "flash");
    copy_plugin_lib(&plugins_dir);

    let harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    (harness, temp_dir, project_root)
}

/// Write `content` to `name` inside the harness project root and
/// return the resulting path.  Use this in place of
/// `TestFixture::new` so the file lives **under** the editor's
/// working directory and renders with a short relative path.
fn write_fixture(project_root: &std::path::Path, name: &str, content: &str) -> PathBuf {
    let path = project_root.join(name);
    fs::write(&path, content).unwrap();
    path
}

/// Open the command palette, type `Flash: Jump`, press Enter, and
/// wait for the plugin's status banner (`Flash[]`) to appear on
/// screen.  CONTRIBUTING rule #2 — observe only rendered output —
/// so we don't peek at `editor_mode()` or the command registry;
/// the visible `Flash[]` banner is the single signal that the
/// plugin is in flash mode AND has armed its first `getNextKey`.
fn arm_flash(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Flash: Jump").unwrap();
    harness.wait_for_screen_contains("Flash: Jump").unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The empty-pattern status banner is the readiness signal.  It
    // is set inside the plugin's main loop AFTER `setEditorMode` and
    // `beginKeyCapture` have been queued, so seeing it on screen
    // proves the editor has processed all three.  No model peek
    // required.
    harness.wait_for_screen_contains("Flash[]").unwrap();
}

/// Type a pattern one character at a time, waiting after each char
/// for the plugin to acknowledge by updating its status banner to
/// `Flash[<pattern-so-far>]`.  This avoids the natural race between
/// the harness's synchronous key dispatch and the plugin's async
/// `getNextKey` re-arm — without it, fast `type_text` can outrun the
/// plugin and chars 2+ fall through into the buffer.
fn type_pattern(harness: &mut EditorTestHarness, pattern: &str) {
    let mut so_far = String::new();
    for c in pattern.chars() {
        so_far.push(c);
        let needle = format!("Flash[{}]", so_far);
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains(&needle))
            .unwrap();
    }
}

#[test]
fn flash_jumps_to_label() {
    // Three "hello" lines.  After arming flash and typing pattern
    // "hello", labels are assigned in distance order from the
    // cursor (currently at line 1 col 1).  We press 's' (the second
    // pool letter after the labeler's skip rule) to jump.  To keep
    // the assertion screen-only (CONTRIBUTING rule #2), we then
    // insert a marker character and observe where it lands in the
    // rendered buffer.
    let (mut harness, _temp, project_root) = flash_harness(120, 24);
    let path = write_fixture(
        &project_root,
        "test.txt",
        "hello world\nhello there\nhello again\n",
    );
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    arm_flash(&mut harness);
    type_pattern(&mut harness, "hello");
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("Flash["))
        .unwrap();

    // Insert a marker so we can observe where the cursor landed.
    harness
        .send_key(KeyCode::Char('@'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("@hello there"))
        .unwrap();
    // Negative check: the `@` did NOT land on line 1 or line 3.
    let screen = harness.screen_to_string();
    assert!(!screen.contains("@hello world"), "screen:\n{}", screen);
    assert!(!screen.contains("@hello again"), "screen:\n{}", screen);
}

#[test]
fn flash_escape_cancels_no_movement() {
    let (mut harness, _temp, project_root) = flash_harness(120, 24);
    let path = write_fixture(
        &project_root,
        "test.txt",
        "hello world\nhello there\nhello again\n",
    );
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    arm_flash(&mut harness);
    type_pattern(&mut harness, "hello");
    harness.render().unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("Flash["))
        .unwrap();

    // Cursor must still be at the start of line 1.  Insert a marker
    // and observe the rendered text.
    harness
        .send_key(KeyCode::Char('@'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("@hello world"))
        .unwrap();
}

#[test]
fn flash_backspace_shrinks_pattern() {
    // After Backspace the prior label set should be re-assigned.
    // Verify by typing a too-narrow pattern first ("there"), then
    // Backspacing back to a multi-match prefix and pressing a label.
    let (mut harness, _temp, project_root) = flash_harness(120, 24);
    let path = write_fixture(
        &project_root,
        "test.txt",
        "hello world\nhello there\nhello again\n",
    );
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    arm_flash(&mut harness);
    type_pattern(&mut harness, "there"); // 1 match (line 2)
    for n in (0..5).rev() {
        let needle = format!("Flash[{}]", &"there"[..n]);
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains(&needle))
            .unwrap();
    }
    type_pattern(&mut harness, "hello"); // 3 matches again
    harness.render().unwrap();

    // Press the label for the third (farthest) match.
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("Flash["))
        .unwrap();

    // Marker assertion: cursor landed at start of line 3.
    harness
        .send_key(KeyCode::Char('@'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("@hello again"))
        .unwrap();
}

/// Regression for the silent-conceal bug, 2026-04: flash relies on
/// `addConceal` to substitute the next-char glyph with the label
/// letter (overlay-style rendering, no layout shift).  An earlier
/// version of fresh's renderer gated `apply_conceal_ranges` on
/// Compose mode only, so flash's conceal calls landed in state but
/// never reached the rendered buffer — labels appeared on screen as
/// the original character with the magenta style applied, not as
/// the assigned label letter.  Cursor-position assertions still
/// passed (the labeler logic was correct), so no existing test
/// caught it.
///
/// This test asserts the rendered glyph itself: at the screen
/// position right after the first `s` match in the buffer, the
/// rendered cell must contain the label letter `a`, not the
/// original `e`.
#[test]
fn flash_label_substitutes_rendered_glyph() {
    // Same buffer shape as `flash_jumps_to_label` so the harness
    // setup that's already known to work doesn't surprise us.
    let (mut harness, _temp, project_root) = flash_harness(120, 24);
    let path = write_fixture(
        &project_root,
        "test.txt",
        "hello world\nhello there\nhello again\n",
    );
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    arm_flash(&mut harness);
    // Pattern `h` — three matches at the start of each line.  With
    // cursor at byte 0, the labeler assigns labels in distance order
    // from "asdfghjkl..." minus the next-char skip set.  The next
    // char after each `h` is `e` (in "hello"), so the skip set is
    // {e}.  Available pool: a, s, d, f, g, h, j, k, l, ...
    // Three matches → labels a, s, d.
    type_pattern(&mut harness, "h");
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // The labels overlay-substitute the next-char glyph (the `e`
    // after each `h`).  The literal label letters depend on the
    // labeler's stability rule (which carries empty-pattern mode's
    // labels through the first-character transition), so we don't
    // hard-code which letter lands where.  What we assert is the
    // *substitution itself*: at every "hello" occurrence the `e`
    // immediately after the matched `h` must be replaced by SOME
    // label letter from the pool.  If conceal isn't applied, the
    // original `hello` text comes through unchanged.
    let pool: &[char] = &[
        'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'q', 'w', 'r', 't', 'y', 'u', 'i', 'o', 'p',
        'z', 'x', 'c', 'v', 'b', 'n', 'm',
    ];
    let mut substituted_count = 0;
    for c in pool {
        let needle: String = format!("h{}llo", c);
        if screen.contains(&needle) {
            substituted_count += screen.matches(&needle).count();
        }
    }
    assert!(
        substituted_count >= 1,
        "expected at least one match to render with the next-char \
         `e` replaced by a pool label letter (e.g. `hsllo`, `hallo`, …) \
         — that's flash's overlay-style cell substitution.  None \
         seen, so addConceal didn't paint.  Screen:\n{}",
        screen,
    );
    // The original glyph `hello` must NOT survive at the labelled
    // positions.  We can't easily count "labelled occurrences" from
    // the screen alone, but we can check there are FEWER plain
    // `hello`s than there are matches (3): if none were
    // substituted, all three would still read `hello`.
    let plain_hello = screen.matches("hello").count();
    assert!(
        plain_hello < 3,
        "expected the substitution to remove at least one plain \
         `hello`, but {} remain — conceal didn't apply.  Screen:\n{}",
        plain_hello,
        screen,
    );
}

#[test]
fn flash_jumps_across_splits() {
    // Two vertical splits, each with a different buffer that contains
    // the literal string "alpha".  Pattern "alpha" → 2 matches: one in
    // each split.  The active split's match sorts first (label "a"),
    // the other split's match second (label "s").  Pressing "s" must
    // (a) focus the other split and (b) place the cursor on its match.
    let (mut harness, _temp, project_root) = flash_harness(120, 30);

    // Place both files **under** the harness project root so the
    // editor renders short relative paths (`left.txt` / `right.txt`)
    // in the status bar.  See the comment on `flash_harness`.
    let f1 = write_fixture(&project_root, "left.txt", "alpha left side\n");
    let f2 = write_fixture(&project_root, "right.txt", "alpha right side\n");

    // Open left file in initial split, then create a vertical split
    // and open right file in the new (active) split.
    harness.open_file(&f1).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.open_file(&f2).unwrap();
    harness.render().unwrap();

    // Sanity: both files visible.
    harness.wait_for_screen_contains("left.txt").unwrap();
    harness.wait_for_screen_contains("right.txt").unwrap();

    // Cursor is currently in right.txt at byte 0.  After arming flash
    // and typing "alpha", labels are 'a' (right split, distance 0) and
    // 's' (left split, other-split tier).
    arm_flash(&mut harness);
    type_pattern(&mut harness, "alpha");
    harness.render().unwrap();

    // Press 's' to jump to the OTHER split's match.
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("Flash["))
        .unwrap();

    // Insert a marker — it should land at the start of the LEFT
    // split's "alpha left side", i.e. on the same line as that text
    // in the left split.  Screen-only assertion (CONTRIBUTING #2).
    harness
        .send_key(KeyCode::Char('@'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("@alpha left side"))
        .unwrap();
    // Negative check: the right split's `alpha right side` is still
    // there but unmarked.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("alpha right side"),
        "right split's content should remain visible; screen:\n{}",
        screen,
    );
    assert!(
        !screen.contains("@alpha right side"),
        "marker should NOT have landed in the right split; screen:\n{}",
        screen,
    );
}

#[test]
fn flash_enter_jumps_to_closest() {
    let (mut harness, _temp, project_root) = flash_harness(120, 24);
    let path = write_fixture(
        &project_root,
        "test.txt",
        "hello world\nhello there\nhello again\n",
    );
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    arm_flash(&mut harness);
    type_pattern(&mut harness, "hello");
    harness.render().unwrap();

    // Closest match is at cursor position (byte 0); Enter should
    // exit cleanly without changing position.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("Flash["))
        .unwrap();

    // Marker assertion — should land before the very first char of
    // line 1.  (CONTRIBUTING #2: screen-only.)
    harness
        .send_key(KeyCode::Char('@'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("@hello world"))
        .unwrap();
}
