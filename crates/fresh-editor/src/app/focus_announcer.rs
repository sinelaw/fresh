//! The one place that tells plugins what the user is looking at.
//!
//! Plugins key their state on `buffer_activated` / `buffer_deactivated` /
//! `active_window_changed`, and every path that changed the active buffer used
//! to remember to fire the hook by hand — ten sites, and the window switch was
//! the one that did not (sinelaw/fresh#3326). Now every such path calls
//! [`Editor::announce_focus`], which compares the active
//! `(window, split, buffer)` triple against the last one it announced and
//! fires exactly the hooks the difference calls for. Calling it twice is free;
//! forgetting it on a new path is caught by the frame's safety net, which
//! calls it after every plugin-command drain.
//!
//! Order within one announcement, when everything changed:
//! `active_window_changed`, `buffer_deactivated` (the buffer the user left),
//! `buffer_activated` (the one they are on). The plugin state snapshot is
//! refreshed first, so a handler reads the state it is being told about.

use super::Editor;
use crate::model::event::LeafId;
use crate::services::plugins::hooks::{BufferRef, HookArgs};
use fresh_core::{BufferId, WindowId};

/// What was last announced: the active window, its active pane, and the
/// buffer in it.
pub(crate) type FocusTriple = (WindowId, LeafId, BufferId);

impl Editor {
    /// Fire the focus hooks for whatever changed since the last announcement.
    /// A no-op when nothing did.
    pub(crate) fn announce_focus(&mut self) {
        self.announce_focus_inner(false);
    }

    /// As [`Self::announce_focus`], but `buffer_activated` fires even when the
    /// buffer id is unchanged — for the open path that re-points the active
    /// scratch buffer at a file in place, which changes what the user is
    /// looking at without changing which buffer it is.
    pub(crate) fn announce_focus_forced(&mut self) {
        self.announce_focus_inner(true);
    }

    /// The triple as it stands, or `None` while the active window has no split
    /// layout yet (a window seeded but not materialised).
    pub(crate) fn current_focus(&self) -> Option<FocusTriple> {
        let window = self.active_window;
        let win = self.windows.get(&window)?;
        let (mgr, _) = win.buffers.splits()?;
        Some((window, mgr.active_split(), win.active_buffer()))
    }

    fn announce_focus_inner(&mut self, force_buffer: bool) {
        let Some(current) = self.current_focus() else {
            return;
        };
        let previous = self.last_announced_focus;
        if previous == Some(current) && !force_buffer {
            return;
        }
        self.last_announced_focus = Some(current);
        let (window, _split, buffer) = current;

        // Refresh before any hook so a handler reads the state it is told
        // about — `getActiveBufferId`, `getCwd`, the window list.
        #[cfg(feature = "plugins")]
        self.update_plugin_state_snapshot();

        let prev_window = previous.map(|p| p.0);
        let prev_buffer = previous.map(|p| p.2);
        // The buffer the user left belongs to the window it was left in.
        let pw_for_buffer = prev_window.map(|w| w.0).unwrap_or(window.0);
        let plugins = self.plugin_manager.read().unwrap();
        if let Some(pw) = prev_window {
            if pw != window {
                plugins.run_hook(
                    "active_window_changed",
                    HookArgs::ActiveWindowChanged {
                        previous_id: Some(pw.0),
                        active_id: window.0,
                    },
                );
            }
        }
        if prev_buffer != Some(buffer) || force_buffer {
            if let Some(pb) = prev_buffer {
                if pb != buffer {
                    plugins.run_hook(
                        "buffer_deactivated",
                        HookArgs::BufferDeactivated {
                            buffer_id: pb,
                            window_id: pw_for_buffer,
                        },
                    );
                }
            }
            plugins.run_hook(
                "buffer_activated",
                HookArgs::BufferActivated {
                    buffer_id: buffer,
                    window_id: window.0,
                },
            );
            // The composed hook: one event for "what the user is looking at
            // changed", with the window attached and the reason named.
            let reason = if force_buffer && prev_buffer == Some(buffer) {
                "open"
            } else if prev_window.is_some_and(|pw| pw != window) {
                "window"
            } else {
                "buffer"
            };
            plugins.run_hook(
                "active_buffer_changed",
                HookArgs::ActiveBufferChanged {
                    window_id: window.0,
                    buffer_id: buffer,
                    previous: previous.map(|(w, _, b)| BufferRef {
                        window_id: w.0,
                        buffer_id: b,
                    }),
                    reason: reason.to_string(),
                },
            );
        }
    }
}
