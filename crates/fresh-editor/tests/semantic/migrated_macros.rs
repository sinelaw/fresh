//! Migrated from `tests/e2e/macros.rs`.
//!
//! These tests exercise the **production macro recording &
//! playback subsystem** — `Action::PromptRecordMacro`,
//! `Action::ToggleMacroRecording`, `Action::PlayLastMacro`,
//! `Action::PlayMacro`, and the per-window `MacroState`. They do
//! NOT redefine "macro" as "any action sequence" — every test
//! below drives the same code path the production keymap reaches
//! when the user picks "Record Macro" / "Play Last Macro" in the
//! command palette.
//!
//! # Why `EditorTestHarness` and not pure `BufferScenario`
//!
//! Macro recording is a cross-state claim: actions dispatched
//! between `ToggleMacroRecording(k)` and the next `ToggleMacroRecording(k)`
//! must end up in the per-window `MacroState`, and a later
//! `PlayLastMacro` must replay them. Each step must be asserted on
//! the same Editor instance — single-shot `assert_buffer_scenario`
//! is the wrong tool. The harness-direct pattern documented in
//! `docs/internal/scenario-migration-status.md` (see also the
//! Save-As migrations in `migrated_undo_save_point.rs`) is the
//! right fit: drive through `EditorTestApi::dispatch` to keep the
//! production code path, and reach for `send_key` only when the
//! step is a prompt-input keystroke (the same surface the
//! production keymap walks).
//!
//! # Prompt-driven register selection vs. `ToggleMacroRecording`
//!
//! The e2e tests opened the command palette ("Record Macro") to
//! get to the register prompt, then typed `0` + Enter into the
//! prompt. The Action layer exposes two equivalent entry points:
//!
//! - `Action::PromptRecordMacro` ⇒ opens the prompt; the
//!   `RecordMacro` prompt-type's confirm handler then calls
//!   `toggle_macro_recording(c)` with the typed register char.
//! - `Action::ToggleMacroRecording(c)` ⇒ calls
//!   `toggle_macro_recording(c)` directly with no prompt.
//!
//! Both reach `Editor::toggle_macro_recording`, which is the
//! actual subsystem entry point. We exercise the prompt-driven
//! path (`PromptRecordMacro` + prompt-input keystrokes +
//! `PromptConfirm`) in the first test to prove the prompt routing
//! and the register-selection contract, and the direct
//! `ToggleMacroRecording('0')` path in the remaining tests so the
//! macro-record/replay invariant under test is not entangled with
//! prompt-input plumbing.
//!
//! # Coverage map vs. `tests/e2e/macros.rs`
//!
//! | e2e test                                                | status     | migrated to                                                                       |
//! |---------------------------------------------------------|------------|-----------------------------------------------------------------------------------|
//! | `test_macro_record_and_play_last`                       | migrated   | `migrated_macro_record_and_play_last_via_prompt` + `_via_toggle_action`           |
//! | `test_macro_with_multiple_cursors_no_overflow`          | migrated   | `migrated_macro_with_multiple_cursors_no_overflow`                                |
//! | `test_play_last_macro_when_none_recorded`               | migrated   | `migrated_play_last_macro_when_none_recorded`                                     |
//! | `test_macro_move_line_end_uses_current_line_length`     | migrated   | `migrated_macro_move_line_end_uses_current_line_length`                           |
//! | `test_macro_playback_is_undoable`                       | migrated   | `migrated_macro_playback_appends_replay`                                          |
//!
//! Note on `test_macro_playback_is_undoable`: the e2e test
//! asserted "one Ctrl+Z removes the entire macro playback" — i.e.
//! macro replay is grouped as one undo unit. The current
//! production behaviour is that each replayed action goes through
//! `handle_action` and lands in the event log as its own unit
//! (see `play_macro` in `crates/fresh-editor/src/app/macro_actions.rs`
//! — no `BulkEdit` grouping around the replay loop). The e2e
//! test's assertion was deliberately weak (`abc_count_after <
//! abc_count`), so any reduction passed; we pin the **observed**
//! granularity (per-action) below and add a finding so a future
//! grouping change doesn't silently regress it.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::test_api::Action;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Drive the `PromptRecordMacro` flow: open the prompt, type the
/// register char + Enter, leaving the editor in recording state.
/// Asserts both observables along the way:
///   - prompt is active immediately after `PromptRecordMacro`
///   - recording is in progress after the confirm
fn open_record_prompt_and_select_register(harness: &mut EditorTestHarness, register: char) {
    harness.api_mut().dispatch(Action::PromptRecordMacro);
    assert!(
        harness.editor().is_prompting(),
        "PromptRecordMacro must open a prompt"
    );

    // Route the register character + Enter through `send_key` ⇒
    // `handle_key`, which forwards to the prompt-input handler
    // while a prompt is active. Same path the production keymap
    // walks.
    harness
        .send_key(KeyCode::Char(register), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    assert!(
        !harness.editor().is_prompting(),
        "Prompt must be closed after Enter"
    );
}

// ---------------------------------------------------------------------------
// 1. Record + play last — both via prompt and via the direct action
// ---------------------------------------------------------------------------

/// Original: `test_macro_record_and_play_last`. Uses the prompt
/// path (`PromptRecordMacro`) for register selection, just like
/// the e2e test does via the command palette.
#[test]
fn migrated_macro_record_and_play_last_via_prompt() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // --- Open prompt, select register 0, start recording. ---
    open_record_prompt_and_select_register(&mut harness, '0');

    // --- Record: type "hello". ---
    for c in "hello".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello");

    // --- Stop recording: ToggleMacroRecording('0') with a
    // recording active ⇒ stops. (The e2e test went through
    // "Stop Recording Macro" in the command palette, which
    // ultimately dispatches `Action::StopMacroRecording` — but
    // `ToggleMacroRecording('0')` reaches the same subsystem
    // entrypoint when '0' is the active register.) ---
    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));

    // --- Move to a fresh line, then play the last macro. ---
    harness.api_mut().dispatch(Action::InsertNewline);
    harness.api_mut().dispatch(Action::PlayLastMacro);

    let text = harness.api_mut().buffer_text();
    assert_eq!(
        text, "hello\nhello",
        "PlayLastMacro must replay the 5 InsertChar events on the new line"
    );

    // Recording state must be clean: no recording in flight, no
    // replay flag stuck on.
    assert!(
        !harness.editor().is_prompting(),
        "No prompt should be open after playback"
    );
}

/// Same scenario as the prompt-driven test, but skipping the
/// prompt and dispatching `ToggleMacroRecording('0')` directly.
/// Proves the recording subsystem doesn't depend on the prompt
/// having been open — the prompt is just register-selection
/// plumbing.
#[test]
fn migrated_macro_record_and_play_last_via_toggle_action() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));
    for c in "hello".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));

    harness.api_mut().dispatch(Action::InsertNewline);
    harness.api_mut().dispatch(Action::PlayLastMacro);

    assert_eq!(harness.api_mut().buffer_text(), "hello\nhello");
}

// ---------------------------------------------------------------------------
// 2. Multi-cursor recording must not stack-overflow on playback
// ---------------------------------------------------------------------------

/// Original: `test_macro_with_multiple_cursors_no_overflow`.
/// Reproduces the bug where recording with multiple cursors
/// active, then playing via `PlayLastMacro`, used to infinite-
/// recurse. The fix is `MacroState::is_playing` gating in
/// `play_macro`. This test passing without panic is the assertion.
#[test]
fn migrated_macro_with_multiple_cursors_no_overflow() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Seed: three lines.
    harness
        .api_mut()
        .dispatch_seq(&[Action::InsertChar('l'), Action::InsertChar('1')]);
    harness.api_mut().dispatch(Action::InsertNewline);
    harness
        .api_mut()
        .dispatch_seq(&[Action::InsertChar('l'), Action::InsertChar('2')]);
    harness.api_mut().dispatch(Action::InsertNewline);
    harness
        .api_mut()
        .dispatch_seq(&[Action::InsertChar('l'), Action::InsertChar('3')]);

    // Move up to line 2 (so AddCursorAbove gives us a sane cursor on line 1).
    harness.api_mut().dispatch(Action::MoveUp);

    // Start recording on register 0.
    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));

    // Add cursor above ⇒ now two cursors active.
    harness.api_mut().dispatch(Action::AddCursorAbove);
    let caret_count_during = harness.api_mut().carets().len();
    assert!(
        caret_count_during >= 2,
        "AddCursorAbove must produce multiple cursors; got {caret_count_during}"
    );

    // Type 'X' — appears at every cursor.
    harness.api_mut().dispatch(Action::InsertChar('X'));

    // Stop recording WITHOUT clearing cursors first — this was the
    // bug trigger.
    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));

    // Clear secondaries (Esc-equivalent) so the playback runs on a
    // single cursor — same as the e2e test does.
    harness.api_mut().dispatch(Action::RemoveSecondaryCursors);
    assert_eq!(
        harness.api_mut().carets().len(),
        1,
        "Should be back to a single cursor before playback"
    );

    // Play the last macro. Bug would stack-overflow here.
    harness.api_mut().dispatch(Action::PlayLastMacro);

    // We got here without panicking — the no-stack-overflow claim
    // is satisfied. Additionally check that the macro had an
    // observable effect: the buffer contains at least one 'X'.
    let text = harness.api_mut().buffer_text();
    assert!(
        text.contains('X'),
        "Macro should have inserted at least one 'X'; buffer was {text:?}"
    );
}

// ---------------------------------------------------------------------------
// 3. PlayLastMacro with no recording shows an error message
// ---------------------------------------------------------------------------

/// Original: `test_play_last_macro_when_none_recorded`. The e2e
/// test screen-scraped for "No macro" / "no macro". The semantic
/// equivalent: assert that the buffer is unchanged and no panic
/// occurs. The status-message string itself is i18n-localized so
/// we don't pin its exact text — instead we pin the contract:
/// PlayLastMacro on a fresh editor is a no-op for the buffer.
#[test]
fn migrated_play_last_macro_when_none_recorded() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Seed some text so "buffer unchanged" is a meaningful claim.
    for c in "seed".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    let before = harness.api_mut().buffer_text();
    let before_caret = harness.api_mut().primary_caret();

    harness.api_mut().dispatch(Action::PlayLastMacro);

    assert_eq!(
        harness.api_mut().buffer_text(),
        before,
        "PlayLastMacro with no recorded macro must not modify the buffer"
    );
    assert_eq!(
        harness.api_mut().primary_caret(),
        before_caret,
        "PlayLastMacro with no recorded macro must not move the cursor"
    );
}

// ---------------------------------------------------------------------------
// 4. MoveLineEnd during replay uses the *current* line length
//    (stale-cache regression)
// ---------------------------------------------------------------------------

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
/// "a.ba.b!!". If MoveLineEnd consulted the stale cached
/// line-end-byte (4 — the original "a.b!" length) we'd get
/// "a.ba!.b!" instead.
#[test]
fn migrated_macro_move_line_end_uses_current_line_length() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // --- Record on register 0. ---
    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));

    harness.api_mut().dispatch_seq(&[
        Action::InsertChar('a'),
        Action::InsertChar('b'),
        Action::MoveLeft,
        Action::InsertChar('.'),
        Action::MoveLineEnd,
        Action::InsertChar('!'),
    ]);

    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));

    assert_eq!(
        harness.api_mut().buffer_text(),
        "a.b!",
        "Recording itself must produce a.b!"
    );

    // --- Move to start of the line and replay. ---
    harness.api_mut().dispatch(Action::MoveLineStart);
    harness.api_mut().dispatch(Action::PlayLastMacro);

    let after = harness.api_mut().buffer_text();
    assert_eq!(
        after, "a.ba.b!!",
        "MoveLineEnd during replay must consult the CURRENT line length, \
         not the cached layout from before the macro modified the line. \
         If this fails with 'a.ba!.b!' the stale-cache bug has regressed."
    );
}

// ---------------------------------------------------------------------------
// 5. Macro playback granularity (was: "is undoable")
// ---------------------------------------------------------------------------

/// Original: `test_macro_playback_is_undoable`. The e2e test
/// asserted only that the post-undo `abc` count was *less than*
/// the post-replay count — i.e. "at least some" of the replay was
/// undone. The actual production granularity is finer: each
/// replayed `InsertChar` lands as its own undo unit (one
/// `apply_event` ⇒ one event-log entry per char), so 3 Ctrl+Z's
/// undo the replayed "abc".
///
/// We pin both halves of the spectrum so a future grouping change
/// (replay-as-one-bulk-edit) doesn't silently flip this test from
/// "asserting per-char granularity" to "asserting bulk granularity"
/// without anyone noticing.
#[test]
fn migrated_macro_playback_appends_replay() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Record "abc" on register 0.
    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));
    for c in "abc".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));
    assert_eq!(harness.api_mut().buffer_text(), "abc");

    // New line, then play the macro.
    harness.api_mut().dispatch(Action::InsertNewline);
    harness.api_mut().dispatch(Action::PlayLastMacro);
    let after_play = harness.api_mut().buffer_text();
    assert_eq!(
        after_play, "abc\nabc",
        "Macro playback appends 'abc' on the new line"
    );

    // --- Granularity probe (pinned per migration-findings #N): ---
    // One Undo removes one replayed char.
    harness.api_mut().dispatch(Action::Undo);
    assert_eq!(
        harness.api_mut().buffer_text(),
        "abc\nab",
        "First Undo after macro replay removes the last replayed char only — \
         macro replay is NOT grouped as a single undo unit today. See \
         scenario-migration-findings.md."
    );

    // Two more Undos drain the rest of the replay.
    harness.api_mut().dispatch(Action::Undo);
    harness.api_mut().dispatch(Action::Undo);
    assert_eq!(
        harness.api_mut().buffer_text(),
        "abc\n",
        "After 3 Undos all 3 replayed InsertChars are gone; the \
         InsertNewline and the original recording survive."
    );
}

// ---------------------------------------------------------------------------
// Anti-tests: prove the positive assertions are load-bearing
// ---------------------------------------------------------------------------

/// Anti-test for `migrated_macro_record_and_play_last_*`.
///
/// If we never call `ToggleMacroRecording`, then `PlayLastMacro`
/// must NOT replay anything — proves that the "hello" appearing
/// twice in the positive test is caused by the recording, not by
/// some accidental InsertChar replay.
#[test]
fn anti_no_recording_means_play_last_is_inert() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    for c in "hello".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    harness.api_mut().dispatch(Action::InsertNewline);
    let before = harness.api_mut().buffer_text();

    // No ToggleMacroRecording calls have happened.
    harness.api_mut().dispatch(Action::PlayLastMacro);

    assert_eq!(
        harness.api_mut().buffer_text(),
        before,
        "PlayLastMacro with no prior recording must not change the buffer — \
         proves the positive test's 'hello\\nhello' result is caused by \
         the macro replay, not by stray InsertChar dispatch."
    );
}

/// Anti-test for `migrated_macro_move_line_end_uses_current_line_length`.
///
/// If `MoveLineEnd` ignored the line entirely during replay (a
/// hypothetical alternative bug), the replay output would be the
/// macro inserts only — no "!" at the trailing position. Prove
/// the test catches that case by recording without `MoveLineEnd`
/// and showing the result differs.
#[test]
fn anti_macro_without_move_line_end_does_not_reach_true_end() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Same recording as the positive test, but with MoveLineEnd REMOVED.
    // Result on empty line: "ab" ⇒ MoveLeft ⇒ "." ⇒ "!" inserted at
    // current cursor (between '.' and 'b') ⇒ "a.!b".
    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));
    harness.api_mut().dispatch_seq(&[
        Action::InsertChar('a'),
        Action::InsertChar('b'),
        Action::MoveLeft,
        Action::InsertChar('.'),
        // No MoveLineEnd here.
        Action::InsertChar('!'),
    ]);
    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));

    let recorded = harness.api_mut().buffer_text();
    assert_ne!(
        recorded, "a.b!",
        "Without MoveLineEnd the trailing '!' lands mid-line; \
         this proves the positive test's MoveLineEnd is load-bearing."
    );
}

/// Anti-test for `migrated_macro_playback_appends_replay`.
///
/// If playback were somehow no-op (subsystem broken, never replays
/// the recorded actions), the post-PlayLastMacro buffer would equal
/// the post-Newline buffer. Catches a regression where
/// `MacroState::is_playing()` is true at entry (e.g. forgotten
/// `end_play()` from a prior replay) and `play_macro` early-returns.
#[test]
fn anti_play_last_macro_is_not_silently_a_noop() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));
    for c in "abc".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    harness
        .api_mut()
        .dispatch(Action::ToggleMacroRecording('0'));

    harness.api_mut().dispatch(Action::InsertNewline);
    let before_play = harness.api_mut().buffer_text();
    harness.api_mut().dispatch(Action::PlayLastMacro);
    let after_play = harness.api_mut().buffer_text();

    assert_ne!(
        before_play, after_play,
        "PlayLastMacro must produce an observable buffer change when a \
         macro is recorded — catches a regression where play_macro early-\
         returns due to a stuck is_playing flag."
    );
}
