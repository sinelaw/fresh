//! Keys held back while a plugin's modal input handler is still running.
//!
//! A plugin that owns a modal input mode (vi) changes how the *next*
//! key is read from inside the handler of *this* one: `d` switches vi to
//! operator-pending, `f` starts capturing the target character. The handler
//! runs on the plugin thread, so those changes reach the editor a round trip
//! later. A key that arrives in between (a fast typist, a paste, a burst
//! from a remote terminal) used to be resolved against the old mode: `d w`
//! ran `w` as a plain motion and left `-- OPERATOR (d) --` showing
//! (sinelaw/fresh#3796).
//!
//! So while such a handler is in flight, further keys queue here and run in
//! order once it has settled, which is when every command it sent (the mode
//! switch among them) has been applied. A handler that waits for a key of its
//! own (`getNextKey`, as `f` and `r` do) gets the queued keys one at a time,
//! as it asks for them, and the rest wait on until it settles: without that,
//! the key after `f`'s target landed in the capture `f` was about to close
//! and was dropped. A handler that opens a prompt or an action popup
//! releases the queue, since the keys are meant for it now.
//!
//! Only modes defined with `holdKeys` hold keys: anywhere else a slow
//! handler (a markdown Enter, say) would stall typing for nothing. A handler that never settles releases it
//! after [`KEY_HOLD_TIMEOUT`] without a key asked for, so a stuck plugin
//! delays typing but cannot swallow it.

use super::Editor;

/// Longest a key waits behind an unsettled handler. Same bound as a macro
/// replay's wait on a plugin step: generous for a loaded machine, short
/// enough that a wedged plugin is noticed rather than mistaken for a hang.
const KEY_HOLD_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(2);

/// The handler that keys are waiting on (`Editor::plugin_key_hold`).
pub(crate) struct PluginKeyHold {
    /// The answerable-call id the handler settles under.
    request_id: u64,
    /// Whether a prompt was open when the handler started, so that only one
    /// the handler opened counts as it handing the keyboard to the user.
    prompt_open_at_start: bool,
    /// Set when an action popup opened while the hold was on: the handler
    /// handed the keyboard to the user, as with a prompt.
    popup_opened: bool,
    /// When the hold started, or last handed the handler a key it asked for.
    /// Wall-clock, not the editor's time source: the bound is on how long a
    /// real keystroke may be held, which a paused test clock must not stretch
    /// into forever.
    since: std::time::Instant,
}

/// A key that arrived while a hold was active, in the form it arrived in.
pub(crate) enum HeldKey {
    Press(fresh_input_parser::KeyPress),
    Key(crossterm::event::KeyCode, crossterm::event::KeyModifiers),
}

impl Editor {
    /// Whether a key has to wait for a plugin handler: queue it and return
    /// `true` if so.
    pub(super) fn hold_key_for_plugin(&mut self, key: HeldKey) -> bool {
        if self.plugin_key_hold.is_none() {
            return false;
        }
        self.held_plugin_keys.push_back(key);
        true
    }

    /// Answerable-call id for a plugin action a key is dispatching, if that
    /// key's successors must wait for it: the key came from the keyboard and
    /// the active plugin input mode was defined with `holdKeys` (its handler
    /// may change how the next key reads). `None` for everything else, which
    /// dispatches as before.
    pub(super) fn plugin_key_hold_request(&self) -> Option<u64> {
        (self.dispatching_key
            && self.plugin_key_hold.is_none()
            && self.macro_replay.is_none()
            && self
                .window_or_input_mode()
                .and_then(|mode| self.mode_registry.get(mode))
                .is_some_and(|mode| mode.hold_keys))
        .then(crate::server::command_access::next_request_id)
    }

    /// An action popup opened: if keys are held for a handler, release them,
    /// since they are the popup's now.
    pub(super) fn release_plugin_key_hold_for_popup(&mut self) {
        if let Some(hold) = self.plugin_key_hold.as_mut() {
            hold.popup_opened = true;
        }
    }

    /// Start holding keys behind the handler dispatched under `request_id`.
    pub(super) fn begin_plugin_key_hold(&mut self, request_id: u64) {
        let window = self.active_window();
        self.plugin_key_hold = Some(PluginKeyHold {
            request_id,
            prompt_open_at_start: window.prompt.is_some(),
            popup_opened: false,
            since: std::time::Instant::now(),
        });
    }

    /// Whether keys are being held for a plugin handler, for the event
    /// loop's wake-up deadline: nothing wakes it when the handler settles.
    pub fn holds_keys_for_plugin(&self) -> bool {
        self.plugin_key_hold.is_some()
    }

    /// Whether the hold (if any) is over: the handler settled, opened a
    /// prompt, or ran out of time.
    fn plugin_key_hold_released(&mut self) -> bool {
        let Some(hold) = self.plugin_key_hold.as_ref() else {
            return true;
        };
        let request_id = hold.request_id;
        let settled =
            !crate::server::command_access::take_completed_where(|id| id == request_id).is_empty();
        let prompted = (self.active_window().prompt.is_some() && !hold.prompt_open_at_start)
            || hold.popup_opened;
        let timed_out = hold.since.elapsed() >= KEY_HOLD_TIMEOUT;
        if timed_out && !settled && !prompted {
            tracing::warn!(
                "plugin handler did not settle within {:?}; releasing held keys",
                KEY_HOLD_TIMEOUT
            );
        }
        settled || prompted || timed_out
    }

    /// Run the keys held behind a handler: hand them one at a time to the
    /// handler while it waits on `getNextKey`, and once it has let go, run
    /// them as ordinary keys until one of them starts a new hold. Called once
    /// per event-loop pass, after the plugin commands have been applied;
    /// returns whether any key ran.
    pub(super) fn run_held_plugin_keys(&mut self) -> bool {
        let mut ran = false;
        loop {
            if self.plugin_key_hold.is_some() && !self.plugin_key_hold_released() {
                // Still running. A key it is waiting for goes to it now; the
                // rest stay queued behind it.
                if self.active_window().pending_next_key_callbacks.is_empty() {
                    break;
                }
                let Some(key) = self.held_plugin_keys.pop_front() else {
                    break;
                };
                let (code, modifiers) = match key {
                    HeldKey::Press(press) => (press.code, press.modifiers),
                    HeldKey::Key(code, modifiers) => (code, modifiers),
                };
                self.try_resolve_next_key_callback(&crossterm::event::KeyEvent::new(
                    code, modifiers,
                ));
                if let Some(hold) = self.plugin_key_hold.as_mut() {
                    hold.since = std::time::Instant::now();
                }
                ran = true;
                continue;
            }
            self.plugin_key_hold = None;
            let Some(key) = self.held_plugin_keys.pop_front() else {
                break;
            };
            ran = true;
            let result = match key {
                HeldKey::Press(press) => self.handle_key_press(press),
                HeldKey::Key(code, modifiers) => self.handle_key(code, modifiers),
            };
            if let Err(e) = result {
                tracing::warn!("key held for a plugin handler failed: {}", e);
            }
        }
        ran
    }
}
