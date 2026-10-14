//! A loud answer for a plugin's buffer id that belongs to another window.
//!
//! Buffer-addressed plugin commands act on the active window: a buffer id
//! from any other window is looked up in the wrong map and silently does
//! nothing, which is indistinguishable from a closed buffer. A plugin that
//! cached an id and kept it across a window switch (sinelaw/fresh#3326)
//! never finds out. [`Editor::plugin_buffer_in_active_window`] is the check
//! a buffer-addressed handler runs first: it says so at `warn`, naming the
//! command, and under `FRESH_STRICT_PLUGIN_IDS` it panics, so a test run
//! turns the mistake into a failure.

use super::Editor;
use fresh_core::BufferId;

impl Editor {
    /// Whether `buffer_id` is a buffer of the active window — the only window
    /// a buffer-addressed command acts on. An id another window owns is
    /// reported (see the module note); an id no window owns is an ordinary
    /// closed buffer and is not.
    pub(crate) fn plugin_buffer_in_active_window(
        &self,
        buffer_id: BufferId,
        command: &str,
    ) -> bool {
        let active = self.active_window;
        let owned_by = |id: &fresh_core::WindowId| {
            self.windows
                .get(id)
                .is_some_and(|w| w.buffers.iter().any(|(b, _)| *b == buffer_id))
        };
        if owned_by(&active) {
            return true;
        }
        if let Some(owner) = self
            .windows
            .keys()
            .find(|id| **id != active && owned_by(id))
        {
            tracing::warn!(
                buffer = buffer_id.0,
                owner_window = owner.0,
                active_window = active.0,
                command,
                "plugin command names a buffer of another window; commands act on the \
                 active window only, so this one is a no-op. Hold (window_id, buffer_id) \
                 together and re-read them on `active_buffer_changed`."
            );
            if std::env::var_os("FRESH_STRICT_PLUGIN_IDS").is_some() {
                panic!(
                    "plugin command {command} names buffer {} of window {} while window {} \
                     is active (FRESH_STRICT_PLUGIN_IDS is set)",
                    buffer_id.0, owner.0, active.0
                );
            }
        }
        false
    }
}
