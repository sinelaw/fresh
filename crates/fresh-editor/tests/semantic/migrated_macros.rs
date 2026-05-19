//! DECLARATIVE migration of `tests/e2e/macros.rs`.
//!
//! Exercises the production macro recording & playback subsystem
//! (`Action::PromptRecordMacro`, `Action::ToggleMacroRecording`,
//! `Action::PlayLastMacro`, `Action::PlayMacro`, and the
//! per-window `MacroState`). Macro is **not** redefined as "any
//! action sequence" — every scenario below drives the same code
//! path the production keymap reaches when the user picks "Record
//! Macro" / "Play Last Macro" from the command palette.
//!
//! # Prompt-driven register selection vs. `ToggleMacroRecording`
//!
//! The e2e originals opened the command palette ("Record Macro")
//! to reach the register prompt, then typed `0` + Enter into the
//! prompt. The Action layer exposes two equivalent entry points:
//!
//! - `Action::PromptRecordMacro` ⇒ opens the prompt; the
//!   `RecordMacro` prompt-type's confirm handler then calls
//!   `toggle_macro_recording(c)` with the typed register char.
//! - `Action::ToggleMacroRecording(c)` ⇒ calls
//!   `toggle_macro_recording(c)` directly with no prompt.
//!
//! Both reach `Editor::toggle_macro_recording`, which is the
//! actual subsystem entry point. The first scenario exercises
//! the prompt-driven path (`PromptRecordMacro` + InputEvent
//! keystrokes into the prompt) so the prompt routing and
//! register-selection contract are pinned; the rest dispatch
//! `ToggleMacroRecording('0')` directly so the
//! macro-record/replay invariant under test is not entangled
//! with prompt-input plumbing.
//!
//! # Coverage map vs. `tests/e2e/macros.rs`
//!
//! | e2e test                                             | migrated to                                                                       |
//! |------------------------------------------------------|-----------------------------------------------------------------------------------|
//! | `test_macro_record_and_play_last`                    | `migrated_macro_record_and_play_last_via_prompt` + `_via_toggle_action`           |
//! | `test_macro_with_multiple_cursors_no_overflow`       | `migrated_macro_with_multiple_cursors_no_overflow`                                |
//! | `test_play_last_macro_when_none_recorded`            | `migrated_play_last_macro_when_none_recorded`                                     |
//! | `test_macro_move_line_end_uses_current_line_length`  | `migrated_macro_move_line_end_uses_current_line_length`                           |
//! | `test_macro_playback_is_undoable`                    | `migrated_macro_playback_appends_replay`                                          |
//!
//! Note on `test_macro_playback_is_undoable`: the e2e test
//! asserted only "one Ctrl+Z removes at least some of the
//! playback" (`abc_count_after < abc_count`). The current
//! production behaviour is finer: each replayed `InsertChar`
//! lands as its own undo unit (one `apply_event` ⇒ one event-log
//! entry per char), so 3 Ctrl+Z's undo the replayed "abc". We
//! pin the **observed** granularity (per-action) so a future
//! grouping change (replay-as-one-bulk-edit) doesn't silently
//! flip the test from "asserting per-char granularity" to
//! "asserting bulk granularity" without anyone noticing.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use crate::common::scenario::input_event::{InputEvent, KeyMods, KeySpec};
use fresh::test_api::Action;

// ─────────────────────────────────────────────────────────────────────
// 1. Record + play last — both via prompt and via the direct action
// ─────────────────────────────────────────────────────────────────────

/// Original: `test_macro_record_and_play_last`. Uses the prompt
/// path for register selection, just like the e2e original via
/// the command palette: `PromptRecordMacro` opens the register
/// prompt, then `SendKey('0')` + `SendKey(Enter)` close it and
/// start recording on register 0.
#[test]
fn migrated_macro_record_and_play_last_via_prompt() {
    assert_buffer_scenario(BufferScenario {
        description:
            "PromptRecordMacro + register-prompt SendKeys records, then PlayLastMacro replays"
                .into(),
        initial_text: String::new(),
        // Step 1: open register prompt. Step 2 (events): type
        // '0' + Enter into the prompt to start recording on
        // register 0. Step 3 (events): record "hello". Step 4:
        // stop recording. Step 5: newline + PlayLastMacro.
        actions: vec![Action::PromptRecordMacro],
        events: vec![
            // Select register 0 via the prompt-input keystrokes.
            // These route through `Editor::handle_key` → prompt
            // input handler (the production path).
            InputEvent::SendKey {
                code: KeySpec::Char('0'),
                modifiers: KeyMods::NONE,
            },
            InputEvent::SendKey {
                code: KeySpec::Enter,
                modifiers: KeyMods::NONE,
            },
            // Record "hello".
            InputEvent::Action(Action::InsertChar('h')),
            InputEvent::Action(Action::InsertChar('e')),
            InputEvent::Action(Action::InsertChar('l')),
            InputEvent::Action(Action::InsertChar('l')),
            InputEvent::Action(Action::InsertChar('o')),
            // Stop recording.
            InputEvent::Action(Action::ToggleMacroRecording('0')),
            // Move to a new line, then replay.
            InputEvent::Action(Action::InsertNewline),
            InputEvent::Action(Action::PlayLastMacro),
        ],
        expected_text: "hello\nhello".into(),
        expected_primary: CursorExpect::at("hello\nhello".len()),
        ..Default::default()
    });
}

/// Same scenario as the prompt-driven test, but skipping the
/// prompt: `ToggleMacroRecording('0')` is dispatched directly.
/// Proves the recording subsystem doesn't depend on the prompt
/// having been open — the prompt is just register-selection
/// plumbing.
#[test]
fn migrated_macro_record_and_play_last_via_toggle_action() {
    assert_buffer_scenario(BufferScenario {
        description: "ToggleMacroRecording('0') start/stop bracket records 'hello'; PlayLastMacro replays".into(),
        initial_text: String::new(),
        actions: vec![
            Action::ToggleMacroRecording('0'),
            Action::InsertChar('h'),
            Action::InsertChar('e'),
            Action::InsertChar('l'),
            Action::InsertChar('l'),
            Action::InsertChar('o'),
            Action::ToggleMacroRecording('0'),
            Action::InsertNewline,
            Action::PlayLastMacro,
        ],
        expected_text: "hello\nhello".into(),
        expected_primary: CursorExpect::at("hello\nhello".len()),
        ..Default::default()
    });
}

// ─────────────────────────────────────────────────────────────────────
// 2. Multi-cursor recording must not stack-overflow on playback
// ─────────────────────────────────────────────────────────────────────

/// Original: `test_macro_with_multiple_cursors_no_overflow`.
/// Reproduces the bug where recording with multiple cursors
/// active, then playing via `PlayLastMacro`, used to infinite-
/// recurse. The fix is `MacroState::is_playing` gating in
/// `play_macro`. The load-bearing claim is "the runner doesn't
/// stack-overflow during PlayLastMacro and the macro had at
/// least one observable effect (an 'X' in the buffer)".
///
/// Declarative shape: seed three lines, move up to line 2,
/// record on register 0 with `AddCursorAbove` + `InsertChar('X')`,
/// stop recording, drop secondary cursors, play. Final buffer
/// must contain at least one 'X'.
///
/// The exact final text is asserted strictly: starting from
/// "l1\nl2\nl3" with the cursor on line 2 we add a cursor
/// above (cursor on line 1 too), insert 'X' (appears at both
/// cursors ⇒ "l1X\nl2X\nl3"), then stop recording, drop
/// secondaries (cursor returns to line 2's 'X'-end position),
/// and replay (insertion of 'X' at the surviving cursor only —
/// no AddCursorAbove this time because the replay loop guards
/// against re-entering the multi-cursor path). The final text
/// is "l1X\nl2XX\nl3".
///
/// The e2e original only asserted `screen.contains("X")`; we
/// pin the stronger structural property to also catch
/// regressions in single-cursor replay semantics.
#[test]
fn migrated_macro_with_multiple_cursors_no_overflow() {
    assert_buffer_scenario(BufferScenario {
        description: "Multi-cursor macro replay does not stack-overflow and inserts an 'X'".into(),
        initial_text: "l1\nl2\nl3".into(),
        actions: vec![
            // Cursor starts at byte 0 (line 1). Move to end of
            // line 2 so AddCursorAbove gives a sane cursor on
            // line 1.
            Action::MoveDocumentEnd, // end of l3
            Action::MoveUp,          // end of l2
            // Start recording on register 0.
            Action::ToggleMacroRecording('0'),
            Action::AddCursorAbove,
            Action::InsertChar('X'),
            Action::ToggleMacroRecording('0'),
            // Drop secondary cursors before playback (e2e
            // equivalent: Escape).
            Action::RemoveSecondaryCursors,
            // Replay — this used to stack-overflow.
            Action::PlayLastMacro,
            // Drop secondaries that replay may have re-added so
            // the assertion shape is single-cursor.
            Action::RemoveSecondaryCursors,
        ],
        // Recording phase: two cursors (lines 1 + 2 ends)
        // insert 'X' ⇒ "l1X\nl2X\nl3". After
        // RemoveSecondaryCursors the surviving cursor is on
        // line 2. Replay re-runs AddCursorAbove (fans out a
        // cursor onto line 1 again) + InsertChar('X') (applies
        // at both cursors). Net effect: one additional 'X' on
        // each of line 1 and line 2 ⇒ "l1XX\nl2XX\nl3". A
        // final RemoveSecondaryCursors collapses to a single
        // cursor again.
        //
        // The structural buffer-content claim is the e2e
        // original's claim (`screen.contains("X")`) tightened
        // to equality on the exact post-replay text.
        expected_text: "l1XX\nl2XX\nl3".into(),
        // Don't pin the exact primary cursor position or count
        // — the load-bearing claim is "no stack overflow + at
        // least one 'X' inserted", and the precise post-collapse
        // cursor state is an implementation detail of
        // RemoveSecondaryCursors / replay's fan-out re-entry.
        // `skip_cursor_check` is the explicit opt-out.
        expected_primary: CursorExpect::default(),
        skip_cursor_check: true,
        ..Default::default()
    });
}

// ─────────────────────────────────────────────────────────────────────
// 3. PlayLastMacro with no recording is a no-op
// ─────────────────────────────────────────────────────────────────────

/// Original: `test_play_last_macro_when_none_recorded`. The e2e
/// test screen-scraped for "No macro" / "no macro" status text;
/// the semantic equivalent is the contract underneath that
/// message: PlayLastMacro on a fresh editor must not modify the
/// buffer or move the cursor. (The status-message string itself
/// is i18n-localized — we don't pin its exact text.)
#[test]
fn migrated_play_last_macro_when_none_recorded() {
    assert_buffer_scenario(BufferScenario {
        description: "PlayLastMacro on a fresh editor leaves buffer + cursor unchanged".into(),
        initial_text: "seed".into(),
        // Cursor starts at byte 0; PlayLastMacro must leave it
        // there. The buffer must remain "seed".
        actions: vec![Action::PlayLastMacro],
        expected_text: "seed".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

// ─────────────────────────────────────────────────────────────────────
// 4. MoveLineEnd during replay uses the *current* line length
//    (stale-cache regression)
// ─────────────────────────────────────────────────────────────────────

/// Original: `test_macro_move_line_end_uses_current_line_length`.
/// Pins the visual-line cache-recompute fix in `play_macro` (see
/// `recompute_layout` call between replayed actions).
///
/// Macro on an empty line: type "ab", MoveLeft, type ".",
/// MoveLineEnd, type "!" ⇒ "a.b!".
///
/// Replay on the same line (already "a.b!"): insert "ab" at start
/// ⇒ "aba.b!", MoveLeft, insert "." ⇒ "a.ba.b!", MoveLineEnd
/// ⇒ cursor at byte 7 (after the trailing "!"), insert "!" ⇒
/// "a.ba.b!!". With the stale-cache bug, MoveLineEnd would
/// consult the cached line_end_byte (4 — the original "a.b!"
/// length) and the final result would be "a.ba!.b!" instead.
#[test]
fn migrated_macro_move_line_end_uses_current_line_length() {
    assert_buffer_scenario(BufferScenario {
        description: "MoveLineEnd during macro replay consults current line length, not stale cache".into(),
        initial_text: String::new(),
        actions: vec![
            // Record "ab" + MoveLeft + "." + MoveLineEnd + "!"
            // on register 0.
            Action::ToggleMacroRecording('0'),
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::MoveLeft,
            Action::InsertChar('.'),
            Action::MoveLineEnd,
            Action::InsertChar('!'),
            Action::ToggleMacroRecording('0'),
            // Move to start of the (now non-empty) line and replay.
            Action::MoveLineStart,
            Action::PlayLastMacro,
        ],
        expected_text: "a.ba.b!!".into(),
        // After replay the cursor sits just past the last '!'
        // inserted by MoveLineEnd + '!'.
        expected_primary: CursorExpect::at("a.ba.b!!".len()),
        ..Default::default()
    });
}

// ─────────────────────────────────────────────────────────────────────
// 5. Macro playback granularity (was: "is undoable")
// ─────────────────────────────────────────────────────────────────────

/// Original: `test_macro_playback_is_undoable`. The e2e test
/// asserted only that the post-undo `abc` count was *less than*
/// the post-replay count — i.e. "at least some" of the replay
/// was undone. The actual production granularity is finer: each
/// replayed `InsertChar` lands as its own undo unit, so 3
/// Ctrl+Z's undo the replayed "abc".
///
/// We pin both the playback-appends-replay claim and the
/// per-char undo granularity in a single scenario so a future
/// grouping change (replay-as-one-bulk-edit) is loud rather
/// than silent.
#[test]
fn migrated_macro_playback_appends_replay() {
    // Sub-scenario A: post-playback the buffer is "abc\nabc".
    assert_buffer_scenario(BufferScenario {
        description: "Macro replay appends 'abc' on the new line".into(),
        initial_text: String::new(),
        actions: vec![
            Action::ToggleMacroRecording('0'),
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::InsertChar('c'),
            Action::ToggleMacroRecording('0'),
            Action::InsertNewline,
            Action::PlayLastMacro,
        ],
        expected_text: "abc\nabc".into(),
        expected_primary: CursorExpect::at("abc\nabc".len()),
        ..Default::default()
    });

    // Sub-scenario B: one Undo removes one replayed char (per-char granularity).
    assert_buffer_scenario(BufferScenario {
        description: "First Undo after macro replay removes the last replayed char only".into(),
        initial_text: String::new(),
        actions: vec![
            Action::ToggleMacroRecording('0'),
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::InsertChar('c'),
            Action::ToggleMacroRecording('0'),
            Action::InsertNewline,
            Action::PlayLastMacro,
            Action::Undo,
        ],
        expected_text: "abc\nab".into(),
        expected_primary: CursorExpect::at("abc\nab".len()),
        ..Default::default()
    });

    // Sub-scenario C: three Undos drain the rest of the replay,
    // leaving only the recording + the newline.
    assert_buffer_scenario(BufferScenario {
        description: "Three Undos after macro replay leave the original recording + newline".into(),
        initial_text: String::new(),
        actions: vec![
            Action::ToggleMacroRecording('0'),
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::InsertChar('c'),
            Action::ToggleMacroRecording('0'),
            Action::InsertNewline,
            Action::PlayLastMacro,
            Action::Undo,
            Action::Undo,
            Action::Undo,
        ],
        expected_text: "abc\n".into(),
        expected_primary: CursorExpect::at("abc\n".len()),
        ..Default::default()
    });
}

// ─────────────────────────────────────────────────────────────────────
// Anti-tests — prove the positive assertions are load-bearing
// ─────────────────────────────────────────────────────────────────────

/// Anti-test for `migrated_macro_record_and_play_last_*`. If
/// `ToggleMacroRecording` is never called, `PlayLastMacro` must
/// NOT replay anything — proving the "hello" appearing twice in
/// the positive test is caused by the recording, not by some
/// accidental InsertChar replay path.
#[test]
fn anti_no_recording_means_play_last_is_inert() {
    assert_buffer_scenario(BufferScenario {
        description:
            "anti: PlayLastMacro with no prior recording must not change the buffer".into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertChar('h'),
            Action::InsertChar('e'),
            Action::InsertChar('l'),
            Action::InsertChar('l'),
            Action::InsertChar('o'),
            Action::InsertNewline,
            Action::PlayLastMacro,
        ],
        expected_text: "hello\n".into(),
        expected_primary: CursorExpect::at("hello\n".len()),
        ..Default::default()
    });
}

/// Anti-test for `migrated_macro_move_line_end_uses_current_line_length`.
/// If `MoveLineEnd` is dropped from the recording, the trailing
/// "!" lands at the current cursor position (between '.' and
/// 'b'), producing "a.!b" rather than "a.b!". This proves the
/// positive test's MoveLineEnd step is load-bearing.
#[test]
fn anti_macro_without_move_line_end_does_not_reach_true_end() {
    assert_buffer_scenario(BufferScenario {
        description:
            "anti: same recording without MoveLineEnd lands '!' mid-line (not at true end)".into(),
        initial_text: String::new(),
        actions: vec![
            Action::ToggleMacroRecording('0'),
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::MoveLeft,
            Action::InsertChar('.'),
            // No MoveLineEnd here.
            Action::InsertChar('!'),
            Action::ToggleMacroRecording('0'),
        ],
        // "ab" ⇒ MoveLeft (cursor between 'a' and 'b') ⇒ "."
        // (cursor between '.' and 'b') ⇒ "!" (still between
        // '.' and 'b') ⇒ "a.!b".
        expected_text: "a.!b".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

/// Anti-test for `migrated_macro_playback_appends_replay`. If
/// playback were silently a no-op (subsystem broken, never
/// replays the recorded actions), the post-PlayLastMacro buffer
/// would equal the post-Newline buffer. Pin the positive claim:
/// recording + newline + playback produces a strictly longer
/// buffer than recording + newline alone — caught here by
/// asserting the playback path yields "abc\nabc" while a no-op
/// path would yield "abc\n".
#[test]
fn anti_play_last_macro_is_not_silently_a_noop() {
    // Without PlayLastMacro the buffer is just "abc\n".
    assert_buffer_scenario(BufferScenario {
        description: "anti: recording + newline (no PlayLastMacro) yields only 'abc\\n'".into(),
        initial_text: String::new(),
        actions: vec![
            Action::ToggleMacroRecording('0'),
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::InsertChar('c'),
            Action::ToggleMacroRecording('0'),
            Action::InsertNewline,
        ],
        expected_text: "abc\n".into(),
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    });
}
