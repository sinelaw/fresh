//! Clipboard and multi-cursor operations for the Editor.
//!
//! This module contains clipboard operations and multi-cursor actions:
//! - Copy/cut/paste operations
//! - Copy with formatting (HTML with syntax highlighting)
//! - Multi-cursor add above/below/at next match

use ratatui::style::{Modifier, Style};
use rust_i18n::t;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

use crate::input::multi_cursor::{
    add_cursor_above, add_cursor_at_next_match, add_cursor_below, line_end_positions_in_selection,
    AddCursorResult,
};
use crate::model::buffer_position::byte_to_2d;
use crate::model::cursor::Cursor;
use crate::model::event::{BufferId, CursorId, Event};
use crate::primitives::ansi::strip_ansi_codes;
use crate::primitives::word_navigation::{
    find_vi_word_end, find_word_start_left, find_word_start_right,
};
use crate::services::async_bridge::AsyncMessage;
use crate::view::virtual_text::{VirtualTextId, VirtualTextPosition};

use super::Editor;

/// Per-paste timeout. The async-paste path renders a placeholder
/// marker and lets the user keep editing; if the background arboard
/// read doesn't return within this window, the marker is removed and
/// the paste is silently cancelled. 500 ms is comfortably longer than
/// any reasonable clipboard round trip and short enough that users
/// recognise a stalled paste before they've moved on.
pub(crate) const PASTE_ASYNC_DEADLINE: Duration = Duration::from_millis(500);

/// Inline-wait budget at the top of `paste()`. Before going async, we
/// race the arboard read against this duration; if the clipboard
/// responds within the window (the common case on a responsive
/// system, ~3ms), we paste inline and skip the placeholder entirely
/// — the user sees zero perceptible latency, indistinguishable from
/// the old synchronous path. Only when arboard takes longer than
/// this do we fall through to the placeholder/event-bridge path.
///
/// 50ms catches typical X11/Wayland clipboard round trips even on
/// slower systems (the prior 20ms budget was missing them — anything
/// in the 20-50ms band fell into the slow placeholder+bridge path,
/// which a slow renderer compounds into hundreds of ms of perceived
/// latency since each render frame is gated on the render itself).
/// It's at the edge of the ~50ms human latency-perception threshold,
/// so a worst-case inline wait still feels nearly instant; on a hung
/// clipboard it's a short, bounded stall before the async path takes
/// over.
pub(crate) const PASTE_INLINE_WAIT: Duration = Duration::from_millis(50);

/// Hard cap on concurrent pending pastes. Each entry costs one virtual
/// text + one marker + one OS thread; in practice the deadline keeps
/// the count near zero. The cap exists only to bound damage from a
/// runaway macro / wedged process holding the clipboard forever.
const MAX_PENDING_PASTES: usize = 64;

/// Single anchor a paste will land at when its read returns. Stored
/// per-cursor at dispatch time (selections having been deleted first
/// so the anchor sits at the eventual insertion point).
#[derive(Debug, Clone, Copy)]
pub struct PasteAnchor {
    /// Virtual text rendering the visual "▍" placeholder; also owns
    /// the underlying marker that tracks the position through edits.
    pub virtual_text_id: VirtualTextId,
}

/// In-flight async paste. Lives in `Editor::paste_pending` keyed by
/// `request_id` between dispatching the background read and receiving
/// the matching `AsyncMessage::ClipboardPasteResult`. Multiple may be
/// pending at once (each Ctrl+V allocates a new id) and each captures
/// the OS clipboard contents at the moment its own thread starts.
#[derive(Debug, Clone)]
pub struct PendingPaste {
    /// Wall-clock cutoff. The tick walks `paste_pending` and removes
    /// any entry past this point; arboard threads that come back
    /// afterwards find no matching entry and are dropped. (The
    /// request id is the map key, not stored here.)
    pub deadline: Instant,
    /// Buffer the anchors live in. Used at resolve time so a paste
    /// initiated in buffer A still lands in A even if the user
    /// switched to buffer B during the wait. If the buffer was closed
    /// in the meantime the entire entry is discarded.
    pub buffer_id: BufferId,
    /// One anchor per cursor at dispatch time (after any selection
    /// deletes were applied). Insertions happen in descending position
    /// order at resolve time so earlier offsets stay valid.
    pub anchors: Vec<PasteAnchor>,
    /// Cursor count captured at dispatch — column-mode paste (one line
    /// per cursor) is decided against this snapshot, not against the
    /// live cursor list which may have changed during the wait.
    pub cursor_count_at_dispatch: usize,
    /// Buffer line-ending captured at dispatch, used to convert the
    /// clipboard's LF-normalised text back to the buffer's format
    /// before insertion.
    pub line_ending: crate::model::buffer::LineEnding,
    /// Wall-clock when paste() was called, used by the `paste_timing`
    /// trace target to measure end-to-end latency from Ctrl+V to the
    /// pasted text appearing on screen.
    pub dispatched_at: Instant,
}

static NEXT_PASTE_REQUEST_ID: AtomicU64 = AtomicU64::new(1);

pub(crate) fn allocate_paste_request_id() -> u64 {
    NEXT_PASTE_REQUEST_ID.fetch_add(1, Ordering::Relaxed)
}

// These are the clipboard and multi-cursor operations on Editor.
//
// MOTIVATION FOR SEPARATION:
// - Buffer operations need: multi-cursor, selections, event sourcing, undo/redo
// - Prompt operations need: simple string manipulation, no selection tracking
// - Sharing code would force prompts to use Buffer (expensive) or buffers to
//   lose features (selections, multi-cursor, undo)
//
// Both use the same clipboard storage (self.clipboard) ensuring copy/paste
// works across buffer editing and prompt input.

impl Editor {
    /// Copy the current selection to clipboard
    ///
    /// If no selection exists, copies the entire current line (like VSCode/Rider/Zed).
    /// For block selections, copies only the rectangular region.
    pub fn copy_selection(&mut self) {
        // Check if any cursor has a block selection (takes priority)
        let has_block_selection = self
            .active_cursors()
            .iter()
            .any(|(_, cursor)| cursor.has_block_selection());

        if has_block_selection {
            // Block selection: copy rectangular region. Strip ANSI escape
            // codes so the plain copy matches the styled text the user sees
            // rather than the raw escape sequences (see `copy_selection_with_theme`
            // for the formatting-preserving variant).
            let text = strip_ansi_codes(&self.copy_block_selection_text());
            if !text.is_empty() {
                self.clipboard.copy(text);
                self.active_window_mut().status_message = Some(t!("clipboard.copied").to_string());
            }
            return;
        }

        // Check if any cursor has a normal selection
        let has_selection = self
            .active_cursors()
            .iter()
            .any(|(_, cursor)| cursor.selection_range().is_some());

        if has_selection {
            // Original behavior: copy selected text
            let ranges: Vec<_> = self
                .active_cursors()
                .iter()
                .filter_map(|(_, cursor)| cursor.selection_range())
                .collect();

            let mut text = String::new();
            let state = self.active_state_mut();
            for range in ranges {
                if !text.is_empty() {
                    text.push('\n');
                }
                let range_text = state.get_text_range(range.start, range.end);
                text.push_str(&range_text);
            }

            // Strip ANSI escape codes: ANSI-aware buffers render escapes as
            // zero-width styling, so the user sees colored text — the plain
            // copy should carry that visible text, not the control codes.
            let text = strip_ansi_codes(&text);
            if !text.is_empty() {
                self.clipboard.copy(text);
                self.active_window_mut().status_message = Some(t!("clipboard.copied").to_string());
            }
        } else {
            // No selection: copy entire line(s) for each cursor
            let estimated_line_length = 80;
            let mut text = String::new();

            // Collect cursor positions first
            let positions: Vec<_> = self
                .active_cursors()
                .iter()
                .map(|(_, c)| c.position)
                .collect();
            let state = self.active_state_mut();

            for pos in positions {
                let mut iter = state.buffer.line_iterator(pos, estimated_line_length);
                if let Some((_start, content)) = iter.next_line() {
                    if !text.is_empty() {
                        text.push('\n');
                    }
                    text.push_str(&content);
                }
            }

            let text = strip_ansi_codes(&text);
            if !text.is_empty() {
                self.clipboard.copy(text);
                self.active_window_mut().status_message =
                    Some(t!("clipboard.copied_line").to_string());
            }
        }
    }

    /// Extract text from block (rectangular) selection
    ///
    /// For block selection, we need to extract a rectangular region defined by:
    /// - The block anchor (stored as Position2D with line and column)
    /// - The current cursor position (byte offset, converted to 2D)
    ///
    /// This works for both small and large files by using line_iterator
    /// for iteration and only using 2D positions for column extraction.
    pub(crate) fn copy_block_selection_text(&mut self) -> String {
        let estimated_line_length = 120;

        // Collect block selection info from all cursors
        let block_infos: Vec<_> = self
            .active_cursors()
            .iter()
            .filter_map(|(_, cursor)| {
                if !cursor.has_block_selection() {
                    return None;
                }
                let block_anchor = cursor.block_anchor?;
                let anchor_byte = cursor.anchor?; // byte offset of anchor
                let cursor_byte = cursor.position;
                Some((block_anchor, anchor_byte, cursor_byte))
            })
            .collect();

        let mut result = String::new();

        for (block_anchor, anchor_byte, cursor_byte) in block_infos {
            // Get current cursor position as 2D
            let cursor_2d = {
                let state = self.active_state();
                byte_to_2d(&state.buffer, cursor_byte)
            };

            // Calculate column bounds (min and max columns for the rectangle)
            let min_col = block_anchor.column.min(cursor_2d.column);
            let max_col = block_anchor.column.max(cursor_2d.column);

            // Calculate line bounds using byte positions
            let start_byte = anchor_byte.min(cursor_byte);
            let end_byte = anchor_byte.max(cursor_byte);

            // Use line_iterator to iterate through lines
            let state = self.active_state_mut();
            let mut iter = state
                .buffer
                .line_iterator(start_byte, estimated_line_length);

            // Collect lines within the block selection range
            let mut lines_text = Vec::new();
            loop {
                let line_start = iter.current_position();

                // Stop if we've passed the end of the selection
                if line_start > end_byte {
                    break;
                }

                if let Some((_offset, line_content)) = iter.next_line() {
                    // Extract the column range from this line
                    // Remove trailing newline for column calculation
                    let content_without_newline = line_content.trim_end_matches(&['\n', '\r'][..]);
                    let chars: Vec<char> = content_without_newline.chars().collect();

                    // Extract characters from min_col to max_col (exclusive)
                    let extracted: String = chars
                        .iter()
                        .skip(min_col)
                        .take(max_col.saturating_sub(min_col))
                        .collect();

                    lines_text.push(extracted);

                    // If this line extends past end_byte, we're done
                    if line_start + line_content.len() > end_byte {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Join the extracted text from each line
            if !result.is_empty() && !lines_text.is_empty() {
                result.push('\n');
            }
            result.push_str(&lines_text.join("\n"));
        }

        result
    }

    /// Copy selection with a specific theme's formatting
    ///
    /// If theme_name is empty, opens a prompt to select a theme.
    /// Otherwise, copies the selected text as HTML with inline CSS styles.
    pub fn copy_selection_with_theme(&mut self, theme_name: &str) {
        // Check if there's a selection first
        let has_selection = self
            .active_cursors()
            .iter()
            .any(|(_, cursor)| cursor.selection_range().is_some());

        if !has_selection {
            self.active_window_mut().status_message =
                Some(t!("clipboard.no_selection").to_string());
            return;
        }

        // Empty theme = open theme picker prompt
        if theme_name.is_empty() {
            self.start_copy_with_formatting_prompt();
            return;
        }
        use crate::services::styled_html::render_styled_html;

        // Get the requested theme from registry
        let theme = match self.theme_registry.get_cloned(theme_name) {
            Some(t) => t,
            None => {
                self.active_window_mut().status_message =
                    Some(format!("Theme '{}' not found", theme_name));
                return;
            }
        };

        // Collect ranges and their byte offsets
        let ranges: Vec<_> = self
            .active_cursors()
            .iter()
            .filter_map(|(_, cursor)| cursor.selection_range())
            .collect();

        if ranges.is_empty() {
            self.active_window_mut().status_message =
                Some(t!("clipboard.no_selection").to_string());
            return;
        }

        // Get the overall range for highlighting
        let min_offset = ranges.iter().map(|r| r.start).min().unwrap_or(0);
        let max_offset = ranges.iter().map(|r| r.end).max().unwrap_or(0);

        // Collect text and highlight spans from state
        let (text, highlight_spans) = {
            let state = self.active_state_mut();

            // Collect text from all ranges
            let mut text = String::new();
            for range in &ranges {
                if !text.is_empty() {
                    text.push('\n');
                }
                let range_text = state.get_text_range(range.start, range.end);
                text.push_str(&range_text);
            }

            if text.is_empty() {
                (text, Vec::new())
            } else {
                // Get highlight spans for the selected region
                let highlight_spans = state.highlighter.highlight_viewport(
                    &state.buffer,
                    min_offset,
                    max_offset,
                    &theme,
                    0, // No context needed since we're copying exact selection
                );
                (text, highlight_spans)
            }
        };

        if text.is_empty() {
            self.active_window_mut().status_message = Some(t!("clipboard.no_text").to_string());
            return;
        }

        // Adjust highlight spans to be relative to the copied text
        let adjusted_spans: Vec<_> = if ranges.len() == 1 {
            let base_offset = ranges[0].start;
            highlight_spans
                .into_iter()
                .filter_map(|span| {
                    if span.range.end <= base_offset || span.range.start >= ranges[0].end {
                        return None;
                    }
                    let start = span.range.start.saturating_sub(base_offset);
                    let end = (span.range.end - base_offset).min(text.len());
                    if start < end {
                        Some(crate::primitives::highlighter::HighlightSpan {
                            range: start..end,
                            color: span.color,
                            bg: None,
                            category: span.category,
                        })
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            Vec::new()
        };

        // Render the styled text to HTML
        let html = render_styled_html(&text, &adjusted_spans, &theme);

        // Copy the HTML to clipboard (with plain text fallback)
        if self.clipboard.copy_html(&html, &text) {
            self.active_window_mut().status_message =
                Some(t!("clipboard.copied_with_theme", theme = theme_name).to_string());
        } else {
            self.clipboard.copy(text);
            self.active_window_mut().status_message =
                Some(t!("clipboard.copied_plain").to_string());
        }
    }

    /// Start the theme selection prompt for copy with formatting
    fn start_copy_with_formatting_prompt(&mut self) {
        use crate::view::prompt::PromptType;

        let available_themes = self.theme_registry.list();
        // Resolve the config value (portable form) to a canonical registry
        // key so the picker can pre-highlight the current theme.
        let resolved_current = self
            .theme_registry
            .resolve_key(&self.config.theme.0)
            .unwrap_or_else(|| self.config.theme.0.clone());
        let current_theme_key = resolved_current.as_str();

        // Find the index of the current theme (match by key first, then name)
        let current_index = available_themes
            .iter()
            .position(|info| info.key == *current_theme_key)
            .or_else(|| {
                let normalized = crate::view::theme::normalize_theme_name(current_theme_key);
                available_themes.iter().position(|info| {
                    crate::view::theme::normalize_theme_name(&info.name) == normalized
                })
            })
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = available_themes
            .iter()
            .map(|info| {
                let is_current = Some(info) == available_themes.get(current_index);
                let description = if is_current {
                    Some(format!("{} (current)", info.key))
                } else {
                    Some(info.key.clone())
                };
                crate::input::commands::Suggestion {
                    description_spans: None,
                    text: info.name.clone(),
                    description,
                    value: Some(info.key.clone()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.active_window_mut().prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Copy with theme: ".to_string(),
            PromptType::CopyWithFormattingTheme,
            suggestions,
        ));

        if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                prompt.input = current_theme_key.to_string();
                prompt.cursor_pos = prompt.input.len();
            }
        }
    }

    /// Cut the current selection to clipboard
    ///
    /// If no selection exists, cuts the entire current line (like VSCode/Rider/Zed).
    pub fn cut_selection(&mut self) {
        // Check if any cursor has a selection
        let has_selection = self
            .active_cursors()
            .iter()
            .any(|(_, cursor)| cursor.selection_range().is_some());

        // Copy first (this handles both selection and whole-line cases)
        self.copy_selection();

        if has_selection {
            // Delete selected text from all cursors
            // IMPORTANT: Sort deletions by position to ensure we process from end to start
            let mut deletions: Vec<_> = self
                .active_cursors()
                .iter()
                .filter_map(|(_, c)| c.selection_range())
                .collect();
            // Sort by start position so reverse iteration processes from end to start
            deletions.sort_by_key(|r| r.start);

            let primary_id = self.active_cursors().primary_id();
            let state = self.active_state_mut();
            let events: Vec<_> = deletions
                .iter()
                .rev()
                .map(|range| {
                    let deleted_text = state.get_text_range(range.start, range.end);
                    Event::Delete {
                        range: range.clone(),
                        deleted_text,
                        cursor_id: primary_id,
                    }
                })
                .collect();

            // Apply events with atomic undo using bulk edit for O(n) performance
            if events.len() > 1 {
                // Use optimized bulk edit for multi-cursor cut
                if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, "Cut".to_string()) {
                    self.active_event_log_mut().append(bulk_edit);
                }
            } else if let Some(event) = events.into_iter().next() {
                self.log_and_apply_event(&event);
            }

            if !deletions.is_empty() {
                self.active_window_mut().status_message = Some(t!("clipboard.cut").to_string());
            }
        } else {
            // No selection: delete entire line(s) for each cursor
            let estimated_line_length = 80;

            // Collect line ranges for each cursor
            // IMPORTANT: Sort deletions by position to ensure we process from end to start
            let positions: Vec<_> = self
                .active_cursors()
                .iter()
                .map(|(_, c)| c.position)
                .collect();
            let mut deletions: Vec<_> = {
                let state = self.active_state_mut();
                positions
                    .into_iter()
                    .filter_map(|pos| {
                        let mut iter = state.buffer.line_iterator(pos, estimated_line_length);
                        let line_start = iter.current_position();
                        iter.next_line().map(|(_start, content)| {
                            let line_end = line_start + content.len();
                            line_start..line_end
                        })
                    })
                    .collect()
            };
            // Sort by start position so reverse iteration processes from end to start
            deletions.sort_by_key(|r| r.start);

            let primary_id = self.active_cursors().primary_id();
            let state = self.active_state_mut();
            let events: Vec<_> = deletions
                .iter()
                .rev()
                .map(|range| {
                    let deleted_text = state.get_text_range(range.start, range.end);
                    Event::Delete {
                        range: range.clone(),
                        deleted_text,
                        cursor_id: primary_id,
                    }
                })
                .collect();

            // Apply events with atomic undo using bulk edit for O(n) performance
            if events.len() > 1 {
                // Use optimized bulk edit for multi-cursor cut
                if let Some(bulk_edit) =
                    self.apply_events_as_bulk_edit(events, "Cut line".to_string())
                {
                    self.active_event_log_mut().append(bulk_edit);
                }
            } else if let Some(event) = events.into_iter().next() {
                self.log_and_apply_event(&event);
            }

            if !deletions.is_empty() {
                self.active_window_mut().status_message =
                    Some(t!("clipboard.cut_line").to_string());
            }
        }
    }

    /// Paste the clipboard content at all cursor positions
    ///
    /// Handles:
    /// - Single cursor paste
    /// - Multi-cursor paste (pastes at each cursor)
    /// - Selection replacement (deletes selection before inserting)
    /// - Atomic undo (single undo step for entire operation)
    pub fn paste(&mut self) {
        // Defensive fast-paths. Prompt/terminal/file-explorer paste
        // routes go through their own actions (PromptPaste,
        // TerminalPaste, FileExplorerPaste); the buffer paste path
        // below assumes there's a real buffer view in front of us. If
        // we somehow landed here under one of those modes anyway,
        // hand off to the synchronous service-level paste.
        if self.active_window().prompt.is_some() || self.active_window().terminal_mode {
            if let Some(text) = self.clipboard.paste() {
                self.paste_text(text);
            }
            return;
        }

        // No bridge (early bootstrap / test harness): there is no
        // event loop to deliver the async result through, so a
        // background read would never come back. The no-bridge
        // configuration also implies no display, so the synchronous
        // arboard call won't actually block.
        let sender = match self.async_bridge.as_ref() {
            Some(bridge) => bridge.sender(),
            None => {
                if let Some(text) = self.clipboard.paste() {
                    self.paste_text(text);
                }
                return;
            }
        };

        // System clipboard disabled (internal-only test mode, or user
        // opted out via config). Spinning up a thread for arboard is
        // pointless when we already know we won't touch the OS.
        if !self.clipboard.uses_system_clipboard() || self.clipboard.is_internal_only() {
            if let Some(text) = self.clipboard.paste_internal() {
                self.paste_text(text);
            }
            return;
        }

        // Bound concurrent pendings. A clipboard owner stuck for an
        // unusual length of time, combined with Ctrl+V autorepeat,
        // could otherwise grow the map without limit. The deadline
        // keeps the count near zero in normal use.
        if self.paste_pending.len() >= MAX_PENDING_PASTES {
            tracing::warn!(
                "MAX_PENDING_PASTES ({}) reached, ignoring Ctrl+V",
                MAX_PENDING_PASTES
            );
            return;
        }

        let buffer_id = self.active_buffer();
        let line_ending = self.active_state().buffer.line_ending();

        // Kick the arboard read off on its own thread RIGHT AWAY,
        // before touching the buffer. Two channels: a private
        // `inline_tx` (bounded to 1) we race against a short timer
        // for the fast path, and the editor's `AsyncBridge` for the
        // slow path. The background thread tries `inline_tx` first
        // and falls back to the bridge only if the inline receiver
        // is gone (we dropped it after timing out).
        //
        // Each thread does its own `arboard::Clipboard::new().get_text()`,
        // so back-to-back Ctrl+V with different OS-clipboard contents
        // in between still picks each one up — the contents captured
        // are whatever the OS clipboard held when this thread reached
        // `get_text`.
        let request_id = allocate_paste_request_id();
        let dispatch_at = Instant::now();
        let (inline_tx, inline_rx) = std::sync::mpsc::sync_channel::<Option<String>>(1);
        let bridge_sender = sender.clone();
        let thread_request_id = request_id;
        // The system-clipboard reader (overridable in tests) and the
        // internal-clipboard snapshot captured *now*. The thread returns
        // `system.or(internal)`: on a host where the OS clipboard is
        // unreadable (Termux, where arboard has no Android backend; a
        // headless TTY; an opt-out) the system read yields `None` and the
        // paste falls back to Fresh's own internal clipboard — restoring
        // the in-editor copy/paste round-trip that the pre-async
        // synchronous path provided (regression from #2155).
        let reader = self
            .system_clipboard_reader
            .unwrap_or(crate::services::clipboard::read_system_clipboard);
        let internal_fallback = self.clipboard.paste_internal();
        std::thread::Builder::new()
            .name("clipboard-paste".into())
            .spawn(move || {
                let arboard_start = Instant::now();
                let text = reader().or(internal_fallback);
                let arboard_ms = arboard_start.elapsed().as_millis();
                let len = text.as_ref().map(|s| s.len()).unwrap_or(0);
                // Try the inline channel first. If the main thread
                // is still inside its `recv_timeout`, the send
                // succeeds and the fast path applies the paste. If
                // the main thread already gave up and dropped
                // `inline_rx`, fall through to the bridge for the
                // async (placeholder) path.
                match inline_tx.send(text.clone()) {
                    Ok(()) => {
                        tracing::info!(
                            target: "paste_timing",
                            "[req {}] arboard returned in {}ms ({} bytes), delivered via INLINE",
                            thread_request_id, arboard_ms, len
                        );
                    }
                    Err(_) => {
                        tracing::info!(
                            target: "paste_timing",
                            "[req {}] arboard returned in {}ms ({} bytes), inline gone — sending via bridge",
                            thread_request_id, arboard_ms, len
                        );
                        if let Err(e) = bridge_sender.send(AsyncMessage::ClipboardPasteResult {
                            request_id: thread_request_id,
                            text,
                        }) {
                            tracing::trace!("clipboard paste result delivery failed: {}", e);
                        }
                    }
                }
            })
            .ok();

        // Now race a short inline wait against the spawned read.
        // Doing the selection-delete *after* this wait would be
        // wrong: a fast inline paste needs the selection cleared
        // first so it can replace it via `paste_text`'s normal
        // logic. So delete the selection now (it's a synchronous
        // local operation, ~µs) and only THEN race the wait.
        let cursor_selections: Vec<(CursorId, std::ops::Range<usize>)> = self
            .active_cursors()
            .iter()
            .filter_map(|(id, c)| c.selection_range().map(|r| (id, r)))
            .collect();

        if !cursor_selections.is_empty() {
            let mut delete_events = Vec::with_capacity(cursor_selections.len());
            for (cursor_id, range) in &cursor_selections {
                let deleted_text = self
                    .active_state_mut()
                    .get_text_range(range.start, range.end);
                delete_events.push(Event::Delete {
                    range: range.clone(),
                    deleted_text,
                    cursor_id: *cursor_id,
                });
            }
            delete_events.sort_by(|a, b| {
                let pa = if let Event::Delete { range, .. } = a {
                    range.start
                } else {
                    0
                };
                let pb = if let Event::Delete { range, .. } = b {
                    range.start
                } else {
                    0
                };
                pb.cmp(&pa)
            });
            if let Err(e) = self.apply_events_to_buffer_as_bulk_edit(
                buffer_id,
                delete_events,
                "Paste (clear selection)".to_string(),
            ) {
                tracing::warn!("paste selection delete failed: {}", e);
                return;
            }
        }

        // Inline wait: if arboard came back within budget, paste
        // synchronously and skip the placeholder entirely — the
        // user sees the paste appear in the same frame as the
        // keystroke, indistinguishable from the old synchronous
        // path. If the read is still in flight after the budget,
        // drop `inline_rx` (which signals the thread to deliver via
        // the bridge instead) and continue to the placeholder path.
        match inline_rx.recv_timeout(PASTE_INLINE_WAIT) {
            Ok(text) => {
                tracing::info!(
                    target: "paste_timing",
                    "[req {}] fast path: inline result in {}ms, no placeholder needed",
                    request_id,
                    dispatch_at.elapsed().as_millis()
                );
                if let Some(t) = text {
                    self.paste_text(t);
                }
                return;
            }
            Err(_) => {
                tracing::info!(
                    target: "paste_timing",
                    "[req {}] inline wait timed out after {}ms — falling back to placeholder",
                    request_id,
                    dispatch_at.elapsed().as_millis()
                );
                // Dropping `inline_rx` here would race the thread
                // (it might be mid-send). Keep it alive until after
                // we've drained any last-second arrival.
                if let Ok(text) = inline_rx.try_recv() {
                    tracing::info!(
                        target: "paste_timing",
                        "[req {}] caught race — fast path after timeout",
                        request_id
                    );
                    if let Some(t) = text {
                        self.paste_text(t);
                    }
                    return;
                }
                drop(inline_rx);
            }
        }

        // Slow path: plant placeholders and register the pending
        // paste so the eventual bridge delivery lands at the anchor.
        let mut positions: Vec<usize> = self
            .active_cursors()
            .iter()
            .map(|(_, c)| c.position)
            .collect();
        positions.sort_unstable();
        positions.dedup();
        let cursor_count = positions.len();

        if positions.is_empty() {
            return;
        }

        let placeholder_style = Style::default().add_modifier(Modifier::DIM);
        let anchors: Vec<PasteAnchor> = {
            let Some(state) = self.buffers_mut().get_mut(&buffer_id) else {
                return;
            };
            positions
                .iter()
                .map(|&pos| {
                    let id = state.virtual_texts.add(
                        &mut state.marker_list,
                        pos,
                        "▍".to_string(),
                        placeholder_style,
                        VirtualTextPosition::BeforeChar,
                        -100,
                    );
                    PasteAnchor {
                        virtual_text_id: id,
                    }
                })
                .collect()
        };

        let deadline = Instant::now() + PASTE_ASYNC_DEADLINE;
        tracing::info!(
            target: "paste_timing",
            "[req {}] slow path: placeholder planted, registering for async delivery",
            request_id
        );

        self.paste_pending.insert(
            request_id,
            PendingPaste {
                deadline,
                buffer_id,
                anchors,
                cursor_count_at_dispatch: cursor_count,
                line_ending,
                dispatched_at: dispatch_at,
            },
        );

        // Signal the input dispatcher to skip the immediate render
        // for this keystroke, AND set a hard render-suppression
        // deadline that the main loop checks. The placeholder is in
        // the buffer; the next render that fires after the deadline
        // (or after the paste resolves, whichever is first) will
        // pick it up. For a common fast-ish clipboard the resolve
        // beats the deadline by a wide margin and that single
        // post-resolve render is the only frame the user sees —
        // instead of paying for two full `terminal.draw` cycles.
        // The suppression window is bounded by the paste deadline
        // so a wedged clipboard can't permanently veto rendering.
        self.paste_slow_path_just_armed = true;
        self.paste_render_suppress_until = Some(deadline);
    }

    /// Consume the "paste just went async" flag set by the slow
    /// placeholder path of `paste()`. Returns whether it was set
    /// (so the caller can suppress the otherwise-automatic render).
    pub(crate) fn take_paste_slow_path_armed(&mut self) -> bool {
        std::mem::take(&mut self.paste_slow_path_just_armed)
    }

    /// True when the main loop should hold off on rendering a frame
    /// because an async paste is in flight and its placeholder
    /// shouldn't get its own (expensive) render before the paste
    /// itself resolves. The suppression auto-expires at the paste
    /// deadline so a hung clipboard can't permanently veto renders.
    pub fn should_suppress_render(&self) -> bool {
        match self.paste_render_suppress_until {
            Some(until) => Instant::now() < until,
            None => false,
        }
    }

    /// Resolve an in-flight async paste keyed by `request_id`.
    ///
    /// - Drops the result if no entry matches: a deadline-fired
    ///   timeout already cleaned up the anchors, or a different
    ///   paste cycle is in flight.
    /// - If `text` is `Some` and the target buffer still exists,
    ///   inserts at every anchor's current position (column-mode
    ///   distributed using the dispatch-time cursor count).
    /// - Cleans up the placeholder virtual texts in all cases so the
    ///   visible "▍" markers go away.
    pub(crate) fn resolve_pending_paste(&mut self, request_id: u64, text: Option<String>) {
        let Some(pending) = self.paste_pending.remove(&request_id) else {
            tracing::info!(
                target: "paste_timing",
                "[req {}] resolve called but no matching entry (already cancelled/stale)",
                request_id
            );
            return;
        };
        let total_ms = pending.dispatched_at.elapsed().as_millis();
        let text_len = text.as_ref().map(|s| s.len()).unwrap_or(0);
        tracing::info!(
            target: "paste_timing",
            "[req {}] resolving after {}ms ({} bytes from clipboard)",
            request_id, total_ms, text_len
        );

        // Clear the render-suppression window if this was the last
        // pending paste (so the about-to-be-applied insertion can
        // render in this frame). If other pastes are still in flight
        // the suppression stays so we keep batching their renders.
        if self.paste_pending.is_empty() {
            self.paste_render_suppress_until = None;
        }

        // Bail out if the buffer is gone (closed during the wait).
        // The buffer's drop took its `virtual_texts` and `marker_list`
        // with it, so the anchors are already cleaned up.
        if self.buffers().get(&pending.buffer_id).is_none() {
            tracing::debug!(
                "paste request {} resolved against closed buffer {:?}, discarding",
                request_id,
                pending.buffer_id
            );
            return;
        }

        // Resolve each anchor's current position via the marker tree.
        // Skip any anchor whose marker was deleted by an intervening
        // edit (e.g. the user deleted through the placeholder).
        let mut anchor_positions: Vec<(usize, usize)> = {
            let state = self
                .buffers()
                .get(&pending.buffer_id)
                .expect("checked above");
            pending
                .anchors
                .iter()
                .enumerate()
                .filter_map(|(i, a)| {
                    let mid = state.virtual_texts.marker_id_of(a.virtual_text_id)?;
                    let pos = state.marker_list.get_position(mid)?;
                    Some((i, pos))
                })
                .collect()
        };

        if let Some(raw_text) = text.filter(|s| !s.is_empty()) {
            // Normalise to LF (mirrors `paste_text`) so column-mode
            // line splitting is unambiguous, then convert back to the
            // buffer's line ending captured at dispatch.
            let normalized = raw_text.replace("\r\n", "\n").replace('\r', "\n");
            let mut lines_for_distribution: Vec<&str> = normalized.split('\n').collect();
            if lines_for_distribution.len() > 1 && lines_for_distribution.last() == Some(&"") {
                lines_for_distribution.pop();
            }
            let use_column_paste = pending.cursor_count_at_dispatch > 1
                && lines_for_distribution.len() > 1
                && lines_for_distribution.len() == pending.cursor_count_at_dispatch
                && anchor_positions.len() == pending.cursor_count_at_dispatch;

            let paste_text_full = match pending.line_ending {
                crate::model::buffer::LineEnding::LF => normalized.clone(),
                crate::model::buffer::LineEnding::CRLF => normalized.replace('\n', "\r\n"),
                crate::model::buffer::LineEnding::CR => normalized.replace('\n', "\r"),
            };

            // Sort anchors by position descending so each insertion
            // doesn't shift subsequent ones forward. The original
            // index is retained for column-mode line lookup.
            anchor_positions.sort_by_key(|b| std::cmp::Reverse(b.1));

            let total = pending.cursor_count_at_dispatch;
            let mut events = Vec::with_capacity(anchor_positions.len());
            for (original_index, pos) in &anchor_positions {
                let text_for_anchor = if use_column_paste {
                    // Topmost cursor (smallest position) gets the
                    // first line — matches `paste_text`'s mapping so
                    // a block-selected round-trip preserves shape.
                    lines_for_distribution[total - 1 - (total - 1 - *original_index)].to_string()
                } else {
                    paste_text_full.clone()
                };
                events.push(Event::Insert {
                    position: *pos,
                    text: text_for_anchor,
                    // No cursor moves on this insert: the user has
                    // been editing freely, and yanking their cursor
                    // to the paste site (which might be far away)
                    // would be the freeze bug in a different form.
                    cursor_id: CursorId::UNDO_SENTINEL,
                });
            }

            if let Err(e) = self.apply_events_to_buffer_as_bulk_edit(
                pending.buffer_id,
                events,
                "Paste".to_string(),
            ) {
                tracing::warn!("paste insertion failed: {}", e);
            } else {
                self.set_status_message(t!("clipboard.pasted").to_string());
            }
        } else {
            // Deadline fired or read returned empty. Leave the buffer
            // untouched; cleanup of the placeholder markers below.
            tracing::debug!(
                "paste request {} resolved with no text — removing anchors",
                request_id
            );
        }

        // Remove the placeholder virtual texts (and their markers).
        let Some(state) = self.buffers_mut().get_mut(&pending.buffer_id) else {
            return;
        };
        for anchor in pending.anchors {
            state
                .virtual_texts
                .remove(&mut state.marker_list, anchor.virtual_text_id);
        }
    }

    /// Walk pending pastes, cancelling any whose deadline has passed.
    /// Returns true when at least one entry was cancelled (the caller
    /// should redraw to refresh the now-empty placeholder cells).
    pub(crate) fn check_paste_deadline(&mut self) -> bool {
        let now = Instant::now();
        let expired_ids: Vec<u64> = self
            .paste_pending
            .iter()
            .filter_map(|(id, pending)| (now >= pending.deadline).then_some(*id))
            .collect();
        if expired_ids.is_empty() {
            return false;
        }
        for id in expired_ids {
            tracing::debug!(
                "paste request {} hit {}ms deadline, cancelling",
                id,
                PASTE_ASYNC_DEADLINE.as_millis()
            );
            self.resolve_pending_paste(id, None);
        }
        true
    }

    /// Earliest deadline across all in-flight pastes, used by the
    /// tick loop to know when to wake.
    ///
    /// Returns the SOONER of:
    ///  - the actual cancel deadline of the earliest pending paste
    ///    (`PASTE_ASYNC_DEADLINE` from dispatch), and
    ///  - a 1 ms drain hint, so the loop wakes ~1ms after the
    ///    background `clipboard-paste` thread sends its result on
    ///    the `AsyncBridge`. The bridge is an mpsc channel with no
    ///    wake mechanism, so the editor only sees the result when
    ///    `editor_tick` next runs — without the 1 ms hint the loop
    ///    could sleep for up to 50ms (idle poll) or 16ms (frame
    ///    budget) per iteration, and a slow render env (which gates
    ///    the next render on `FRAME_DURATION`) compounds that into
    ///    a several-hundred-millisecond perceived paste latency.
    ///
    /// CPU cost is bounded: the deadline cap of
    /// `PASTE_ASYNC_DEADLINE` (500 ms) means at most ~500 extra tick
    /// iterations per paste cycle. Each iteration is a `try_recv_all`
    /// on the bridge plus a few cheap checks; no rendering work
    /// happens unless something actually changed.
    pub(crate) fn next_paste_deadline(&self) -> Option<Instant> {
        let cancel_deadline = self.paste_pending.values().map(|p| p.deadline).min()?;
        let drain_hint = Instant::now() + Duration::from_millis(1);
        Some(cancel_deadline.min(drain_hint))
    }

    /// Whether at least one async paste is in flight. Exposed mainly
    /// for tests and instrumentation; the input loop no longer keys
    /// off this — input is dispatched immediately and the anchor
    /// catches the eventual paste.
    pub fn is_paste_pending(&self) -> bool {
        !self.paste_pending.is_empty()
    }

    /// Cancel any pending pastes whose anchors live in the given
    /// buffer. Called by the buffer-close path so we don't try to
    /// insert into a freed buffer when the result arrives. The
    /// buffer's `virtual_texts` and `marker_list` are about to be
    /// dropped along with the buffer, so we just forget the entries
    /// — no virtual-text removal needed.
    pub fn cancel_pending_pastes_for_buffer(&mut self, buffer_id: BufferId) {
        self.paste_pending
            .retain(|_, pending| pending.buffer_id != buffer_id);
        if self.paste_pending.is_empty() {
            self.paste_render_suppress_until = None;
        }
    }

    /// Route a terminal-initiated bracketed paste to a focused
    /// floating panel (Orchestrator picker / New-Session form / plugin
    /// overlay) or focused dock when one owns the keyboard.
    ///
    /// Bracketed paste arrives as a single `Event::Paste` rather than
    /// per-key events, so — unlike typed characters and `Ctrl+V` — it
    /// never passes through `dispatch_floating_widget_key`. Without this
    /// routing it falls straight through to `paste_text`, which targets
    /// the buffer underneath the modal (the user-reported bug: pasting
    /// into the New-Session dialog dumped the text into the obscured
    /// file instead of the focused field).
    ///
    /// Returns `true` when a panel owns the keyboard (the paste was
    /// either inserted into its focused `Text` widget, or deliberately
    /// swallowed because focus isn't on a text field — a modal with no
    /// text input focused must ignore the paste, not leak it into the
    /// hidden buffer). Returns `false` when no panel owns the keyboard,
    /// so the caller falls back to the normal `paste_text` path.
    pub(crate) fn paste_bracketed_into_focused_panel(&mut self, text: &str) -> bool {
        // The Settings dialog is a capture-all modal overlay that owns the
        // keyboard above any panel. A bracketed paste must reach its focused
        // text input (or be swallowed when no field is focused) rather than
        // leaking into the buffer obscured behind it — the same class of bug
        // the floating-panel routing below fixes (issue #2268). Gate on
        // `visible`, not mere presence: `close_settings` only hides the
        // state (it isn't dropped), and a lingering hidden dialog must not
        // swallow pastes meant for the buffer.
        if self.settings_state.as_ref().is_some_and(|s| s.visible) {
            if let Some(settings) = self.settings_state.as_mut() {
                if settings.paste_into_focused_text(text) {
                    self.set_status_message(t!("clipboard.pasted").to_string());
                }
            }
            return true;
        }

        // Mirror the keyboard-dispatch precedence in `handle_key`: a
        // focused centered modal wins over a focused dock.
        let slot = if self
            .floating_widget_panel
            .as_ref()
            .is_some_and(|f| f.focused)
        {
            super::PanelSlot::Floating
        } else if self.dock.as_ref().is_some_and(|d| d.focused) {
            super::PanelSlot::Dock
        } else {
            return false;
        };
        let Some(panel_id) = self.panel(slot).map(|f| f.panel_key.clone()) else {
            return false;
        };
        if self.panel_focused_widget_is_text(&panel_id) {
            // Single-line `TextEdit` strips embedded newlines; multi-line
            // stores plain `\n`. Normalise CRLF / CR → LF first, matching
            // the `Action::Paste` widget-routing path.
            let normalized = text.replace("\r\n", "\n").replace('\r', "\n");
            self.handle_widget_insert_str(&panel_id, &normalized);
            self.set_status_message(t!("clipboard.pasted").to_string());
        }
        true
    }

    /// Paste text directly into the editor
    ///
    /// Handles:
    /// - Line ending normalization (CRLF/CR → buffer's format)
    /// - Single cursor paste
    /// - Multi-cursor paste (pastes at each cursor)
    /// - Column-mode paste: when the cursor count equals the number of
    ///   clipboard lines, each cursor receives a distinct line (matches
    ///   VSCode/Notepad++ behavior, see issue #1057). This makes a
    ///   block-selected copy/paste round-trip preserve its rectangular shape.
    /// - Selection replacement (deletes selection before inserting)
    /// - Atomic undo (single undo step for entire operation)
    /// - Routing to prompt if one is open
    pub fn paste_text(&mut self, paste_text: String) {
        if paste_text.is_empty() {
            return;
        }

        // Normalize line endings: first convert all to LF, then to buffer's format
        // This handles Windows clipboard (CRLF), old Mac (CR), and Unix (LF)
        let normalized = paste_text.replace("\r\n", "\n").replace('\r', "\n");

        // If a prompt is open, paste into the prompt (prompts use LF internally)
        if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
            prompt.insert_str(&normalized);
            self.update_prompt_suggestions();
            self.active_window_mut().status_message = Some(t!("clipboard.pasted").to_string());
            return;
        }

        // If in terminal mode, send paste to the terminal PTY
        if self.active_window().terminal_mode {
            self.active_window_mut()
                .send_terminal_input(normalized.as_bytes());
            return;
        }

        // Collect cursor info sorted in reverse order by position
        let mut cursor_data: Vec<_> = self
            .active_cursors()
            .iter()
            .map(|(cursor_id, cursor)| {
                let selection = cursor.selection_range();
                let insert_position = selection
                    .as_ref()
                    .map(|r| r.start)
                    .unwrap_or(cursor.position);
                (cursor_id, selection, insert_position)
            })
            .collect();
        cursor_data.sort_by_key(|(_, _, pos)| std::cmp::Reverse(*pos));

        // Decide whether to distribute one clipboard line per cursor
        // (column-mode paste). We split on LF (after normalization above) and
        // ignore a single trailing empty entry from a trailing newline so that
        // "a\nb\nc" and "a\nb\nc\n" both yield 3 lines.
        let mut lines_for_distribution: Vec<&str> = normalized.split('\n').collect();
        if lines_for_distribution.len() > 1 && lines_for_distribution.last() == Some(&"") {
            lines_for_distribution.pop();
        }
        let use_column_paste = cursor_data.len() > 1
            && lines_for_distribution.len() > 1
            && lines_for_distribution.len() == cursor_data.len();

        // Convert to buffer's line ending format (only used in non-column mode;
        // a single column-paste line never contains an embedded newline).
        let paste_text_full = match self.active_state().buffer.line_ending() {
            crate::model::buffer::LineEnding::LF => normalized.clone(),
            crate::model::buffer::LineEnding::CRLF => normalized.replace('\n', "\r\n"),
            crate::model::buffer::LineEnding::CR => normalized.replace('\n', "\r"),
        };

        // Get deleted text for each selection
        let cursor_data_with_text: Vec<_> = {
            let state = self.active_state_mut();
            cursor_data
                .into_iter()
                .map(|(cursor_id, selection, insert_position)| {
                    let deleted_text = selection
                        .as_ref()
                        .map(|r| state.get_text_range(r.start, r.end));
                    (cursor_id, selection, insert_position, deleted_text)
                })
                .collect()
        };

        // Build events for each cursor.
        //
        // cursor_data_with_text is sorted by position DESCENDING (so events
        // applied in vector order don't invalidate earlier offsets). For column
        // paste we want the topmost cursor (smallest position) to receive the
        // first clipboard line, so we index into `lines_for_distribution` from
        // the back when iterating.
        let total = cursor_data_with_text.len();
        let mut events = Vec::new();
        for (i, (cursor_id, selection, insert_position, deleted_text)) in
            cursor_data_with_text.into_iter().enumerate()
        {
            if let (Some(range), Some(text)) = (selection, deleted_text) {
                events.push(Event::Delete {
                    range,
                    deleted_text: text,
                    cursor_id,
                });
            }
            let text = if use_column_paste {
                lines_for_distribution[total - 1 - i].to_string()
            } else {
                paste_text_full.clone()
            };
            events.push(Event::Insert {
                position: insert_position,
                text,
                cursor_id,
            });
        }

        // Apply events with atomic undo using bulk edit for O(n) performance
        if events.len() > 1 {
            // Use optimized bulk edit for multi-cursor paste
            if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, "Paste".to_string()) {
                self.active_event_log_mut().append(bulk_edit);
            }
        } else if let Some(event) = events.into_iter().next() {
            self.log_and_apply_event(&event);
        }

        self.active_window_mut().status_message = Some(t!("clipboard.pasted").to_string());
    }

    /// Set clipboard content for testing purposes
    /// This sets the internal clipboard and enables internal-only mode to avoid
    /// system clipboard interference between parallel tests
    #[doc(hidden)]
    pub fn set_clipboard_for_test(&mut self, text: String) {
        self.clipboard.set_internal(text);
        self.clipboard.set_internal_only(true);
    }

    /// Override the async paste path's system-clipboard reader for tests.
    ///
    /// Lets a test deterministically simulate a host whose OS clipboard is
    /// unreadable (e.g. Termux, where arboard has no backend) by passing
    /// `|| None`, while leaving the system clipboard nominally *enabled* —
    /// the exact configuration that exposed the lost internal-clipboard
    /// fallback (#2343). Without this seam a test would read the real host
    /// clipboard, which is neither deterministic nor isolated.
    #[doc(hidden)]
    pub fn set_system_clipboard_reader_for_test(&mut self, reader: fn() -> Option<String>) {
        self.system_clipboard_reader = Some(reader);
    }

    /// Paste from internal clipboard only (for testing)
    /// This bypasses the system clipboard to avoid interference from CI environments
    #[doc(hidden)]
    pub fn paste_for_test(&mut self) {
        // Get content from internal clipboard only (ignores system clipboard)
        let paste_text = match self.clipboard.paste_internal() {
            Some(text) => text,
            None => return,
        };

        // Use the same paste logic as the regular paste method
        self.paste_text(paste_text);
    }

    /// Get clipboard content for testing purposes
    /// Returns the internal clipboard content
    #[doc(hidden)]
    pub fn clipboard_content_for_test(&self) -> String {
        self.clipboard.get_internal().to_string()
    }

    /// Copy a buffer's file path to the clipboard.
    ///
    /// When `relative` is true the path is made relative to the workspace root;
    /// if the file lives outside the workspace the absolute path is used as a
    /// safe fallback (the user still gets a usable path rather than nothing).
    /// When `relative` is false the absolute path is always copied.
    ///
    /// If the buffer has no associated file (unsaved scratch buffer) or the
    /// buffer id is unknown, a status message is shown and the clipboard is
    /// left untouched.
    pub fn copy_buffer_path(&mut self, buffer_id: crate::model::event::BufferId, relative: bool) {
        let path = self
            .buffers()
            .get(&buffer_id)
            .and_then(|state| state.buffer.file_path().map(|p| p.to_path_buf()));
        let Some(path) = path else {
            self.active_window_mut().status_message =
                Some(t!("clipboard.no_file_path").to_string());
            return;
        };

        let path_str = if relative {
            path.strip_prefix(self.working_dir())
                .unwrap_or(&path)
                .to_string_lossy()
                .into_owned()
        } else {
            path.to_string_lossy().into_owned()
        };

        self.clipboard.copy(path_str.clone());
        self.active_window_mut().status_message =
            Some(t!("clipboard.copied_path", path = &path_str).to_string());
    }

    /// Copy the active buffer's file path. See [`Self::copy_buffer_path`].
    pub fn copy_active_buffer_path(&mut self, relative: bool) {
        let buffer_id = self.active_buffer();
        self.copy_buffer_path(buffer_id, relative);
    }

    /// Add a cursor at the next occurrence of the selected text
    /// If no selection, first selects the entire word at cursor position.
    ///
    /// When an active substring search has placed the cursor at a match
    /// (cursor inside `search_state.matches[i]..matches[i] + match_lengths[i]`),
    /// the search match is selected instead of the surrounding word.  This
    /// way subsequent presses look for the search substring rather than the
    /// whole word, which would skip other substring occurrences (issue #1697).
    pub fn add_cursor_at_next_match(&mut self) {
        if let Some(range) = self.active_window().search_match_at_primary_cursor() {
            let primary_id = self.active_cursors().primary_id();
            let primary = self.active_cursors().primary();
            let event = Event::MoveCursor {
                cursor_id: primary_id,
                old_position: primary.position,
                new_position: range.end,
                old_anchor: primary.anchor,
                new_anchor: Some(range.start),
                old_sticky_column: primary.sticky_column,
                new_sticky_column: None,
            };
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
            return;
        }

        let cursors = self.active_cursors().clone();
        let state = self.active_state_mut();
        match add_cursor_at_next_match(state, &cursors) {
            AddCursorResult::Success {
                cursor,
                total_cursors,
            } => {
                // Create AddCursor event with the next cursor ID
                let next_id = CursorId(self.active_cursors().count());
                let event = Event::AddCursor {
                    cursor_id: next_id,
                    position: cursor.position,
                    anchor: cursor.anchor,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);

                self.active_window_mut().status_message =
                    Some(t!("clipboard.added_cursor_match", count = total_cursors).to_string());
            }
            AddCursorResult::WordSelected {
                word_start,
                word_end,
            } => {
                // Select the word by updating the primary cursor
                let primary_id = self.active_cursors().primary_id();
                let primary = self.active_cursors().primary();
                let event = Event::MoveCursor {
                    cursor_id: primary_id,
                    old_position: primary.position,
                    new_position: word_end,
                    old_anchor: primary.anchor,
                    new_anchor: Some(word_start),
                    old_sticky_column: primary.sticky_column,
                    new_sticky_column: None,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);
            }
            AddCursorResult::Failed { message } => {
                self.active_window_mut().status_message = Some(message);
            }
        }
    }

    /// Add a cursor above the primary cursor at the same column
    pub fn add_cursor_above(&mut self) {
        let cursors = self.active_cursors().clone();
        let state = self.active_state_mut();
        match add_cursor_above(state, &cursors) {
            AddCursorResult::Success {
                cursor,
                total_cursors,
            } => {
                // Create AddCursor event with the next cursor ID
                let next_id = CursorId(self.active_cursors().count());
                let event = Event::AddCursor {
                    cursor_id: next_id,
                    position: cursor.position,
                    anchor: cursor.anchor,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);

                self.active_window_mut().status_message =
                    Some(t!("clipboard.added_cursor_above", count = total_cursors).to_string());
            }
            AddCursorResult::Failed { message } => {
                self.active_window_mut().status_message = Some(message);
            }
            AddCursorResult::WordSelected { .. } => unreachable!(),
        }
    }

    /// Add a cursor below the primary cursor at the same column
    pub fn add_cursor_below(&mut self) {
        let cursors = self.active_cursors().clone();
        let state = self.active_state_mut();
        match add_cursor_below(state, &cursors) {
            AddCursorResult::Success {
                cursor,
                total_cursors,
            } => {
                // Create AddCursor event with the next cursor ID
                let next_id = CursorId(self.active_cursors().count());
                let event = Event::AddCursor {
                    cursor_id: next_id,
                    position: cursor.position,
                    anchor: cursor.anchor,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);

                self.active_window_mut().status_message =
                    Some(t!("clipboard.added_cursor_below", count = total_cursors).to_string());
            }
            AddCursorResult::Failed { message } => {
                self.active_window_mut().status_message = Some(message);
            }
            AddCursorResult::WordSelected { .. } => unreachable!(),
        }
    }

    /// Place a cursor at the end of every line covered by ANY existing
    /// cursor's selection (or each cursor's own line if it has no selection).
    /// Matches VSCode's "Add Cursor to Line Ends" / Sublime's "Split Selection
    /// into Lines": every existing cursor contributes, no cursor is silently
    /// dropped. Two cursors on the same line collapse to a single cursor.
    /// All selections are cleared.
    pub fn add_cursors_to_line_ends(&mut self) {
        let cursors = self.active_cursors().clone();
        let state = self.active_state_mut();
        let positions = line_end_positions_in_selection(state, &cursors);

        if positions.is_empty() {
            self.active_window_mut().status_message =
                Some(t!("clipboard.added_cursors_to_line_ends_failed").to_string());
            return;
        }

        // Sort the existing cursors in document order and map them index-wise
        // onto the new positions. This preserves cursor IDs where possible —
        // important for undo/redo — and minimises the move distance for each
        // surviving cursor.
        let mut existing: Vec<(CursorId, Cursor)> =
            cursors.iter().map(|(id, c)| (id, *c)).collect();
        existing.sort_by_key(|(_, c)| c.position);

        let mut events: Vec<Event> = Vec::new();
        let reuse = existing.len().min(positions.len());

        for i in 0..reuse {
            let (cursor_id, cur) = existing[i];
            let target = positions[i];
            events.push(Event::MoveCursor {
                cursor_id,
                old_position: cur.position,
                new_position: target,
                old_anchor: cur.anchor,
                new_anchor: None,
                old_sticky_column: cur.sticky_column,
                new_sticky_column: None,
            });
        }

        // If two cursors collapsed onto the same line, dedup left us with
        // fewer positions than cursors — drop the extras.
        for &(cursor_id, cur) in existing.iter().skip(reuse) {
            events.push(Event::RemoveCursor {
                cursor_id,
                position: cur.position,
                anchor: cur.anchor,
            });
        }

        // Add fresh cursors for any extra line ends, with IDs strictly above
        // the highest existing one so we never collide with a cursor an undo
        // could re-insert later.
        let next_free_id = cursors
            .iter()
            .map(|(id, _)| id.0)
            .max()
            .map(|m| m + 1)
            .unwrap_or(0);
        for (i, &pos) in positions.iter().enumerate().skip(reuse) {
            let new_id = CursorId(next_free_id + i - reuse);
            events.push(Event::AddCursor {
                cursor_id: new_id,
                position: pos,
                anchor: None,
            });
        }

        let total = positions.len();
        let batch = Event::Batch {
            events,
            description: "Add cursors to line ends".to_string(),
        };
        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);

        self.active_window_mut().status_message =
            Some(t!("clipboard.added_cursors_to_line_ends", count = total).to_string());
    }

    // =========================================================================
    // Vi-style yank operations (copy range without requiring selection)
    // =========================================================================

    /// Yank (copy) from cursor to next word start
    pub fn yank_word_forward(&mut self) {
        let cursor_positions: Vec<_> = self
            .active_cursors()
            .iter()
            .map(|(_, c)| c.position)
            .collect();
        let ranges: Vec<_> = {
            let state = self.active_state();
            cursor_positions
                .into_iter()
                .filter_map(|start| {
                    let end = find_word_start_right(&state.buffer, start);
                    if end > start {
                        Some(start..end)
                    } else {
                        None
                    }
                })
                .collect()
        };

        if ranges.is_empty() {
            return;
        }

        // Copy text from all ranges
        let mut text = String::new();
        let state = self.active_state_mut();
        for range in ranges {
            if !text.is_empty() {
                text.push('\n');
            }
            let range_text = state.get_text_range(range.start, range.end);
            text.push_str(&range_text);
        }

        if !text.is_empty() {
            let len = text.len();
            self.clipboard.copy(text);
            self.active_window_mut().status_message =
                Some(t!("clipboard.yanked", count = len).to_string());
        }
    }

    /// Yank (copy) from cursor to vim word end (inclusive)
    pub fn yank_vi_word_end(&mut self) {
        let cursor_positions: Vec<_> = self
            .active_cursors()
            .iter()
            .map(|(_, c)| c.position)
            .collect();
        let ranges: Vec<_> = {
            let state = self.active_state();
            cursor_positions
                .into_iter()
                .filter_map(|start| {
                    let word_end = find_vi_word_end(&state.buffer, start);
                    let end = (word_end + 1).min(state.buffer.len());
                    if end > start {
                        Some(start..end)
                    } else {
                        None
                    }
                })
                .collect()
        };

        if ranges.is_empty() {
            return;
        }

        let mut text = String::new();
        let state = self.active_state_mut();
        for range in ranges {
            if !text.is_empty() {
                text.push('\n');
            }
            let range_text = state.get_text_range(range.start, range.end);
            text.push_str(&range_text);
        }

        if !text.is_empty() {
            let len = text.len();
            self.clipboard.copy(text);
            self.active_window_mut().status_message =
                Some(t!("clipboard.yanked", count = len).to_string());
        }
    }

    /// Yank (copy) from previous word start to cursor
    pub fn yank_word_backward(&mut self) {
        let cursor_positions: Vec<_> = self
            .active_cursors()
            .iter()
            .map(|(_, c)| c.position)
            .collect();
        let ranges: Vec<_> = {
            let state = self.active_state();
            cursor_positions
                .into_iter()
                .filter_map(|end| {
                    let start = find_word_start_left(&state.buffer, end);
                    if start < end {
                        Some(start..end)
                    } else {
                        None
                    }
                })
                .collect()
        };

        if ranges.is_empty() {
            return;
        }

        let mut text = String::new();
        let state = self.active_state_mut();
        for range in ranges {
            if !text.is_empty() {
                text.push('\n');
            }
            let range_text = state.get_text_range(range.start, range.end);
            text.push_str(&range_text);
        }

        if !text.is_empty() {
            let len = text.len();
            self.clipboard.copy(text);
            self.active_window_mut().status_message =
                Some(t!("clipboard.yanked", count = len).to_string());
        }
    }

    /// Yank (copy) from cursor to end of line
    pub fn yank_to_line_end(&mut self) {
        let estimated_line_length = 80;

        // First collect cursor positions with immutable borrow
        let cursor_positions: Vec<_> = self
            .active_cursors()
            .iter()
            .map(|(_, cursor)| cursor.position)
            .collect();

        // Now compute ranges with mutable borrow (line_iterator needs &mut self)
        let state = self.active_state_mut();
        let mut ranges = Vec::new();
        for pos in cursor_positions {
            let mut iter = state.buffer.line_iterator(pos, estimated_line_length);
            let line_start = iter.current_position();
            if let Some((_start, content)) = iter.next_line() {
                // Don't include the line ending in yank
                let content_len = content.trim_end_matches(&['\n', '\r'][..]).len();
                let line_end = line_start + content_len;
                if pos < line_end {
                    ranges.push(pos..line_end);
                }
            }
        }

        if ranges.is_empty() {
            return;
        }

        let mut text = String::new();
        for range in ranges {
            if !text.is_empty() {
                text.push('\n');
            }
            let range_text = state.get_text_range(range.start, range.end);
            text.push_str(&range_text);
        }

        if !text.is_empty() {
            let len = text.len();
            self.clipboard.copy(text);
            self.active_window_mut().status_message =
                Some(t!("clipboard.yanked", count = len).to_string());
        }
    }

    /// Yank (copy) from start of line to cursor
    pub fn yank_to_line_start(&mut self) {
        let estimated_line_length = 80;

        // First collect cursor positions with immutable borrow
        let cursor_positions: Vec<_> = self
            .active_cursors()
            .iter()
            .map(|(_, cursor)| cursor.position)
            .collect();

        // Now compute ranges with mutable borrow (line_iterator needs &mut self)
        let state = self.active_state_mut();
        let mut ranges = Vec::new();
        for pos in cursor_positions {
            let iter = state.buffer.line_iterator(pos, estimated_line_length);
            let line_start = iter.current_position();
            if pos > line_start {
                ranges.push(line_start..pos);
            }
        }

        if ranges.is_empty() {
            return;
        }

        let mut text = String::new();
        for range in ranges {
            if !text.is_empty() {
                text.push('\n');
            }
            let range_text = state.get_text_range(range.start, range.end);
            text.push_str(&range_text);
        }

        if !text.is_empty() {
            let len = text.len();
            self.clipboard.copy(text);
            self.active_window_mut().status_message =
                Some(t!("clipboard.yanked", count = len).to_string());
        }
    }
}
