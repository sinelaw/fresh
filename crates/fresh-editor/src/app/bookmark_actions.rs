//! Bookmark orchestrators on `Editor`.
//!
//! Cross-cutting effects — cursor movement, status messages, lazy
//! cleanup of bookmarks pointing at closed buffers — for the bookmark
//! subsystem. Plain data state lives in `super::bookmarks::BookmarkState`.

use rust_i18n::t;

use crate::model::event::Event;

use super::Editor;

impl Editor {
    pub(super) fn set_bookmark(&mut self, key: char) {
        let buffer_id = self.active_buffer();
        let position = self.active_cursors().primary().position;
        self.bookmarks.set(
            key,
            super::bookmarks::Bookmark {
                buffer_id,
                position,
            },
        );
        self.set_status_message(t!("bookmark.set", key = key).to_string());
    }

    /// Jump to a bookmark
    pub(super) fn jump_to_bookmark(&mut self, key: char) {
        let Some(bookmark) = self.bookmarks.get(key) else {
            self.set_status_message(t!("bookmark.not_set", key = key).to_string());
            return;
        };

        // Switch to the buffer if needed, or forget the bookmark if it's gone.
        if bookmark.buffer_id != self.active_buffer() {
            if self.buffers.contains_key(&bookmark.buffer_id) {
                self.set_active_buffer(bookmark.buffer_id);
            } else {
                self.set_status_message(t!("bookmark.buffer_gone", key = key).to_string());
                self.bookmarks.remove(key);
                return;
            }
        }

        // Move cursor to bookmark position
        let cursor = *self.active_cursors().primary();
        let cursor_id = self.active_cursors().primary_id();
        let state = self.active_state_mut();
        let new_pos = bookmark.position.min(state.buffer.len());

        let event = Event::MoveCursor {
            cursor_id,
            old_position: cursor.position,
            new_position: new_pos,
            old_anchor: cursor.anchor,
            new_anchor: None,
            old_sticky_column: cursor.sticky_column,
            new_sticky_column: 0,
        };

        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
        // Bookmarks can point anywhere in the file; the viewport must scroll
        // to follow the jump even when the bookmark target is in the same
        // buffer that's already visible (#1689).
        self.ensure_active_cursor_visible_for_navigation(true);
        self.set_status_message(t!("bookmark.jumped", key = key).to_string());
    }

    /// Clear a bookmark
    pub(super) fn clear_bookmark(&mut self, key: char) {
        if self.bookmarks.remove(key) {
            self.set_status_message(t!("bookmark.cleared", key = key).to_string());
        } else {
            self.set_status_message(t!("bookmark.not_set", key = key).to_string());
        }
    }

    /// List all bookmarks
    pub(super) fn list_bookmarks(&mut self) {
        if self.bookmarks.is_empty() {
            self.set_status_message(t!("bookmark.none_set").to_string());
            return;
        }

        let mut bookmark_list: Vec<(char, super::bookmarks::Bookmark)> =
            self.bookmarks.iter().collect();
        bookmark_list.sort_by_key(|(k, _)| *k);

        let list_str: String = bookmark_list
            .iter()
            .map(|(k, bm)| {
                let buffer_name = self
                    .buffer_metadata
                    .get(&bm.buffer_id)
                    .map(|m| m.display_name.as_str())
                    .unwrap_or("unknown");
                format!("'{}': {} @ {}", k, buffer_name, bm.position)
            })
            .collect::<Vec<_>>()
            .join(", ");

        self.set_status_message(t!("bookmark.list", list = list_str).to_string());
    }
}
