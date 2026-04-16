//! Self-contained macro record/playback state.
//!
//! `MacroState` owns the four fields that used to live directly on `Editor`:
//! the map of register-key to recorded actions, the in-flight recording
//! buffer, the last register played (for "play last macro"), and the
//! playing flag that suppresses recursive playback.
//!
//! Nothing else in the codebase reaches into `MacroState`'s internals —
//! every cross-subsystem effect (status messages, handle_action replay,
//! buffer creation for `show_macro_in_buffer`) is handled by the
//! orchestrator on `Editor` using the narrow public API below.

use std::collections::HashMap;

use crate::input::keybindings::Action;

/// State for a macro recording in progress.
#[derive(Debug, Clone)]
pub(crate) struct RecordingState {
    /// The register key this macro is being recorded to.
    pub key: char,
    /// Actions recorded so far.
    pub actions: Vec<Action>,
}

/// Owner of all macro record/playback state.
#[derive(Debug, Default)]
pub(crate) struct MacroState {
    /// Register-key -> recorded actions.
    macros: HashMap<char, Vec<Action>>,
    /// Current in-flight recording, if any.
    recording: Option<RecordingState>,
    /// Last register that was recorded/played (for "play last macro").
    last_register: Option<char>,
    /// True while a macro is being replayed — suppresses recursive recording.
    playing: bool,
}

impl MacroState {
    // ---- Queries -----------------------------------------------------------

    /// The register key of the in-flight recording, if any.
    ///
    /// Callers use `recording_key().is_some()` as the "is recording?" query —
    /// we deliberately don't expose a separate `is_recording` boolean since
    /// callers almost always want the key too.
    pub(crate) fn recording_key(&self) -> Option<char> {
        self.recording.as_ref().map(|r| r.key)
    }

    /// Whether a macro is currently being replayed.
    pub(crate) fn is_playing(&self) -> bool {
        self.playing
    }

    /// Register key of the most recently recorded/played macro.
    pub(crate) fn last_register(&self) -> Option<char> {
        self.last_register
    }

    /// Recorded actions for `key`, or `None` if no such macro exists.
    pub(crate) fn get(&self, key: char) -> Option<&[Action]> {
        self.macros.get(&key).map(Vec::as_slice)
    }

    /// Whether any macros have been recorded.
    pub(crate) fn is_empty(&self) -> bool {
        self.macros.is_empty()
    }

    /// Total number of recorded macros.
    pub(crate) fn count(&self) -> usize {
        self.macros.len()
    }

    /// All register keys, sorted — suitable for listing in a view buffer.
    pub(crate) fn keys_sorted(&self) -> Vec<char> {
        let mut keys: Vec<char> = self.macros.keys().copied().collect();
        keys.sort();
        keys
    }

    // ---- Lifecycle ---------------------------------------------------------

    /// Begin recording into `key`. Any existing recording is discarded —
    /// callers should call [`Self::stop_recording`] first if they want the
    /// previous recording saved.
    pub(crate) fn start_recording(&mut self, key: char) {
        self.recording = Some(RecordingState {
            key,
            actions: Vec::new(),
        });
    }

    /// Stop the in-flight recording and save it.
    ///
    /// Returns `Some((key, action_count))` on success, `None` if no recording
    /// was in progress.
    pub(crate) fn stop_recording(&mut self) -> Option<(char, usize)> {
        let state = self.recording.take()?;
        let action_count = state.actions.len();
        let key = state.key;
        self.macros.insert(key, state.actions);
        self.last_register = Some(key);
        Some((key, action_count))
    }

    /// Mark replay as started. Callers must call [`Self::end_play`] exactly
    /// once afterwards, even on error.
    pub(crate) fn begin_play(&mut self) {
        self.playing = true;
    }

    /// Mark replay as finished.
    pub(crate) fn end_play(&mut self) {
        self.playing = false;
    }

    // ---- Recording ---------------------------------------------------------

    /// Append `action` to the in-flight recording, if any, unless the action
    /// is a macro-control action itself (those don't belong in the recorded
    /// stream — replaying them would trigger infinite regress).
    ///
    /// Does nothing if no recording is in progress or if a macro is currently
    /// playing back (we never record replay actions).
    pub(crate) fn record_if_recording(&mut self, action: &Action) {
        if self.playing {
            return;
        }
        let Some(state) = self.recording.as_mut() else {
            return;
        };
        if is_macro_control_action(action) {
            return;
        }
        state.actions.push(action.clone());
    }

    /// Append an already-transformed action to the recording (e.g. a
    /// `PromptConfirmWithText` the orchestrator produced by snapshotting the
    /// current prompt text). Skips the `is_macro_control_action` filter so
    /// that the transformed variant lands in the recording even though the
    /// raw form it replaces was filterable.
    pub(crate) fn record_transformed(&mut self, action: Action) {
        if self.playing {
            return;
        }
        let Some(state) = self.recording.as_mut() else {
            return;
        };
        state.actions.push(action);
    }
}

/// Actions that manage macros themselves — recording a "start recording"
/// action inside the macro would cause recursive recording on replay.
fn is_macro_control_action(action: &Action) -> bool {
    matches!(
        action,
        Action::StartMacroRecording
            | Action::StopMacroRecording
            | Action::PlayMacro(_)
            | Action::ToggleMacroRecording(_)
            | Action::ShowMacro(_)
            | Action::ListMacros
            | Action::PromptRecordMacro
            | Action::PromptPlayMacro
            | Action::PlayLastMacro
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_are_empty_and_idle() {
        let m = MacroState::default();
        assert!(!m.is_playing());
        assert_eq!(m.recording_key(), None);
        assert_eq!(m.last_register(), None);
        assert!(m.is_empty());
        assert_eq!(m.count(), 0);
    }

    #[test]
    fn start_then_stop_round_trips_recording() {
        let mut m = MacroState::default();
        m.start_recording('q');
        assert_eq!(m.recording_key(), Some('q'));

        // Record a few non-control actions.
        m.record_if_recording(&Action::MoveLeft);
        m.record_if_recording(&Action::MoveRight);

        let saved = m.stop_recording();
        assert_eq!(saved, Some(('q', 2)));
        assert_eq!(m.recording_key(), None);
        assert_eq!(m.last_register(), Some('q'));
        assert_eq!(m.get('q').map(|a| a.len()), Some(2));
    }

    #[test]
    fn stop_recording_when_not_recording_returns_none() {
        let mut m = MacroState::default();
        assert_eq!(m.stop_recording(), None);
    }

    #[test]
    fn recording_filters_control_actions() {
        let mut m = MacroState::default();
        m.start_recording('q');
        m.record_if_recording(&Action::MoveLeft);
        m.record_if_recording(&Action::StartMacroRecording); // filtered
        m.record_if_recording(&Action::StopMacroRecording); // filtered
        m.record_if_recording(&Action::PlayMacro('x')); // filtered
        m.record_if_recording(&Action::ListMacros); // filtered
        m.record_if_recording(&Action::MoveRight);

        let (_key, count) = m.stop_recording().unwrap();
        assert_eq!(count, 2); // only Left + Right were recorded
    }

    #[test]
    fn recording_suppressed_while_playing() {
        let mut m = MacroState::default();
        m.start_recording('q');
        m.begin_play();
        m.record_if_recording(&Action::MoveLeft);
        m.end_play();
        let (_key, count) = m.stop_recording().unwrap();
        assert_eq!(count, 0);
    }

    #[test]
    fn record_transformed_bypasses_control_filter() {
        // PromptConfirm is not a control action, but the orchestrator
        // transforms it into PromptConfirmWithText. That transformed form
        // should land in the recording even though it's a manually-produced
        // variant.
        let mut m = MacroState::default();
        m.start_recording('q');
        m.record_transformed(Action::PromptConfirmWithText("hello".to_string()));
        let (_key, count) = m.stop_recording().unwrap();
        assert_eq!(count, 1);
    }

    #[test]
    fn starting_second_recording_discards_first() {
        // Deliberate behaviour: start_recording clobbers any in-flight
        // recording. Callers that want to save the previous one must call
        // stop_recording first.
        let mut m = MacroState::default();
        m.start_recording('a');
        m.record_if_recording(&Action::MoveLeft);
        m.start_recording('b');
        assert_eq!(m.recording_key(), Some('b'));
        let (key, count) = m.stop_recording().unwrap();
        assert_eq!(key, 'b');
        assert_eq!(count, 0);
    }

    #[test]
    fn keys_sorted_returns_stable_order() {
        let mut m = MacroState::default();
        m.start_recording('c');
        m.stop_recording();
        m.start_recording('a');
        m.stop_recording();
        m.start_recording('b');
        m.stop_recording();
        assert_eq!(m.keys_sorted(), vec!['a', 'b', 'c']);
    }

    #[test]
    fn last_register_tracks_most_recent_recording() {
        let mut m = MacroState::default();
        m.start_recording('a');
        m.stop_recording();
        assert_eq!(m.last_register(), Some('a'));
        m.start_recording('b');
        m.stop_recording();
        assert_eq!(m.last_register(), Some('b'));
    }

}
