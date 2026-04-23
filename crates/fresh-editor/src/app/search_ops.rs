//! Search & replace orchestrators on `Editor`.
//!
//! This file consolidates the search-and-replace method cluster that
//! previously lived in `render.rs`. The pure pieces — regex construction,
//! replacement expansion — continue to live in `super::regex_replace` and
//! are called through; these methods handle the cross-cutting effects
//! (cursor movement, overlay creation, status messages, interactive
//! replace state machine).
//!
//! Part of the editor-modules refactor (docs/internal/
//! editor-modules-refactor-plan.md). Kept as `impl Editor` for now;
//! future phases may convert to free functions taking borrowed slices.

use anyhow::Result as AnyhowResult;
use rust_i18n::t;

use crate::model::event::Event;
use crate::view::prompt::{Prompt, PromptType};

use super::types::{InteractiveReplaceState, SearchState};
use super::Editor;

enum SearchDirection {
    Forward,
    Backward,
}

impl Editor {
    /// Clear all search highlights from the active buffer and reset search state
    pub(super) fn clear_search_highlights(&mut self) {
        self.clear_search_overlays();
        // Also clear search state
        self.search_state = None;
    }

    /// Clear only the visual search overlays, preserving search state for F3/Shift+F3
    /// This is used when the buffer is modified - highlights become stale but F3 should still work
    pub(super) fn clear_search_overlays(&mut self) {
        let ns = self.search_namespace.clone();
        let state = self.active_state_mut();
        state.overlays.clear_namespace(&ns, &mut state.marker_list);
    }

    /// Update search highlights in visible viewport only (for incremental search)
    /// This is called as the user types in the search prompt for real-time feedback
    pub(super) fn update_search_highlights(&mut self, query: &str) {
        // If query is empty, clear highlights and return
        if query.is_empty() {
            self.clear_search_highlights();
            return;
        }

        // Get theme colors and search settings before borrowing state
        let search_bg = self.theme.search_match_bg;
        let search_fg = self.theme.search_match_fg;
        let case_sensitive = self.search_case_sensitive;
        let whole_word = self.search_whole_word;
        let use_regex = self.search_use_regex;
        let ns = self.search_namespace.clone();

        // Build regex pattern if regex mode is enabled, or escape for literal search
        let regex_pattern = if use_regex {
            if whole_word {
                format!(r"\b{}\b", query)
            } else {
                query.to_string()
            }
        } else {
            let escaped = regex::escape(query);
            if whole_word {
                format!(r"\b{}\b", escaped)
            } else {
                escaped
            }
        };

        // Build regex with case sensitivity
        let regex = regex::RegexBuilder::new(&regex_pattern)
            .case_insensitive(!case_sensitive)
            .build();

        let regex = match regex {
            Ok(r) => r,
            Err(_) => {
                // Invalid regex, clear highlights and return
                self.clear_search_highlights();
                return;
            }
        };

        // Get viewport from active split's SplitViewState
        let active_split = self.split_manager.active_split();
        let (top_byte, visible_height) = self
            .split_view_states
            .get(&active_split)
            .map(|vs| (vs.viewport.top_byte, vs.viewport.height.saturating_sub(2)))
            .unwrap_or((0, 20));

        let state = self.active_state_mut();

        // Clear any existing search highlights
        state.overlays.clear_namespace(&ns, &mut state.marker_list);

        // Get the visible content by iterating through visible lines
        let visible_start = top_byte;
        let mut visible_end = top_byte;

        {
            let mut line_iter = state.buffer.line_iterator(top_byte, 80);
            for _ in 0..visible_height {
                if let Some((line_start, line_content)) = line_iter.next_line() {
                    visible_end = line_start + line_content.len();
                } else {
                    break;
                }
            }
        }

        // Ensure we don't go past buffer end
        visible_end = visible_end.min(state.buffer.len());

        // Get the visible text
        let visible_text = state.get_text_range(visible_start, visible_end);

        // Find all matches using regex
        for mat in regex.find_iter(&visible_text) {
            let absolute_pos = visible_start + mat.start();
            let match_len = mat.end() - mat.start();

            // Add overlay for this match
            let search_style = ratatui::style::Style::default().fg(search_fg).bg(search_bg);
            let overlay = crate::view::overlay::Overlay::with_namespace(
                &mut state.marker_list,
                absolute_pos..(absolute_pos + match_len),
                crate::view::overlay::OverlayFace::Style {
                    style: search_style,
                },
                ns.clone(),
            )
            .with_priority_value(10); // Priority - above syntax highlighting

            state.overlays.add(overlay);
        }
    }

    /// Build a compiled regex from the current search settings and query.
    fn build_search_regex(&self, query: &str) -> Result<regex::Regex, String> {
        super::regex_replace::build_search_regex(
            query,
            self.search_use_regex,
            self.search_whole_word,
            self.search_case_sensitive,
        )
    }

    /// Perform a search and update search state.
    ///
    /// For large files (lazy-loaded buffers), this starts an incremental
    /// chunked search that runs a few pieces per render frame so the UI
    /// stays responsive.  For normal-sized files the search runs inline.
    ///
    /// Matches are capped at `MAX_SEARCH_MATCHES` to bound memory usage,
    /// and overlays are only created for the visible viewport.
    /// Move the primary cursor to `position`, clear its selection anchor,
    /// update the cached line number (used by the status bar), and scroll
    /// the active split so the cursor is visible.
    ///
    /// Delegates to [`Editor::jump_active_cursor_to`] so the viewport
    /// invariant (cursor must end up visible) is enforced uniformly with
    /// every other navigation flow (LSP goto-def, jump-to-line, etc.). If
    /// the match was off-screen and required a scroll, the viewport is
    /// vertically centered on the match to provide surrounding context
    /// (issue #1251); matches already visible are not re-scrolled.
    fn move_cursor_to_match(&mut self, position: usize) {
        self.jump_active_cursor_to(position, super::navigation::JumpOptions::navigation());
    }

    pub(super) fn perform_search(&mut self, query: &str) {
        if query.is_empty() {
            self.search_state = None;
            self.set_status_message(t!("search.cancelled").to_string());
            return;
        }

        let search_range = self.pending_search_range.take();

        // Build the regex early so we can bail on invalid patterns
        let regex = match self.build_search_regex(query) {
            Ok(r) => r,
            Err(e) => {
                self.search_state = None;
                self.set_status_message(t!("error.invalid_regex", error = e).to_string());
                return;
            }
        };

        // For large files, start an incremental (non-blocking) search scan
        let is_large = self.active_state().buffer.is_large_file();
        if is_large && search_range.is_none() {
            self.start_search_scan(query, regex);
            return;
        }

        // --- Normal (small-file) path: search inline with match cap ---

        let buffer_content = {
            let state = self.active_state_mut();
            let total_bytes = state.buffer.len();
            match state.buffer.get_text_range_mut(0, total_bytes) {
                Ok(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
                Err(e) => {
                    tracing::warn!("Failed to load buffer for search: {}", e);
                    self.set_status_message(t!("error.buffer_not_loaded").to_string());
                    return;
                }
            }
        };

        let (search_start, search_end) = if let Some(ref range) = search_range {
            (range.start, range.end)
        } else {
            (0, buffer_content.len())
        };

        let search_slice = &buffer_content[search_start..search_end];

        // Collect matches with a cap to bound memory
        let mut match_ranges: Vec<(usize, usize)> = Vec::new();
        let mut capped = false;
        for m in regex.find_iter(search_slice) {
            if match_ranges.len() >= SearchState::MAX_MATCHES {
                capped = true;
                break;
            }
            match_ranges.push((search_start + m.start(), m.end() - m.start()));
        }

        if match_ranges.is_empty() {
            self.search_state = None;
            let msg = if search_range.is_some() {
                format!("No matches found for '{}' in selection", query)
            } else {
                format!("No matches found for '{}'", query)
            };
            self.set_status_message(msg);
            return;
        }

        self.finalize_search(query, match_ranges, capped, search_range);
    }

    /// Common finalization after all matches have been collected (inline or
    /// from the incremental scan).  Sets `search_state`, moves the cursor to
    /// the nearest match, creates overlays, and updates the status message.
    ///
    /// For small files, overlays are created for ALL matches so that marker-
    /// based position tracking keeps F3 correct across edits.  For large
    /// files (`viewport_only == true`), only visible-viewport overlays are
    /// created to avoid multi-GB overlay allocations.
    pub(super) fn finalize_search(
        &mut self,
        query: &str,
        match_ranges: Vec<(usize, usize)>,
        capped: bool,
        search_range: Option<std::ops::Range<usize>>,
    ) {
        let matches: Vec<usize> = match_ranges.iter().map(|(pos, _)| *pos).collect();
        let match_lengths: Vec<usize> = match_ranges.iter().map(|(_, len)| *len).collect();
        let is_large = self.active_state().buffer.is_large_file();

        // Find the first match at or after the current cursor position
        let cursor_pos = self.active_cursors().primary().position;
        let current_match_index = matches
            .iter()
            .position(|&pos| pos >= cursor_pos)
            .unwrap_or(0);

        // Move cursor to the first match
        let match_pos = matches[current_match_index];
        self.move_cursor_to_match(match_pos);

        let num_matches = matches.len();

        self.search_state = Some(SearchState {
            query: query.to_string(),
            matches,
            match_lengths: match_lengths.clone(),
            current_match_index: Some(current_match_index),
            wrap_search: search_range.is_none(),
            search_range,
            capped,
        });

        if is_large {
            // Large file: viewport-only overlays to avoid O(matches) memory
            self.refresh_search_overlays();
        } else {
            // Small file: overlays for ALL matches so markers auto-track edits
            let search_bg = self.theme.search_match_bg;
            let search_fg = self.theme.search_match_fg;
            let ns = self.search_namespace.clone();
            let state = self.active_state_mut();
            state.overlays.clear_namespace(&ns, &mut state.marker_list);

            for (&pos, &len) in match_ranges
                .iter()
                .map(|(p, _)| p)
                .zip(match_lengths.iter())
            {
                let search_style = ratatui::style::Style::default().fg(search_fg).bg(search_bg);
                let overlay = crate::view::overlay::Overlay::with_namespace(
                    &mut state.marker_list,
                    pos..(pos + len),
                    crate::view::overlay::OverlayFace::Style {
                        style: search_style,
                    },
                    ns.clone(),
                )
                .with_priority_value(10);
                state.overlays.add(overlay);
            }
        }

        let cap_suffix = if capped { "+" } else { "" };
        let msg = if self.search_state.as_ref().unwrap().search_range.is_some() {
            format!(
                "Found {}{} match{} for '{}' in selection",
                num_matches,
                cap_suffix,
                if num_matches == 1 { "" } else { "es" },
                query
            )
        } else {
            format!(
                "Found {}{} match{} for '{}'",
                num_matches,
                cap_suffix,
                if num_matches == 1 { "" } else { "es" },
                query
            )
        };
        self.set_status_message(msg);
    }

    /// Create search-highlight overlays only for matches visible in the current
    /// viewport.  Uses binary search on the sorted `search_state.matches` vec
    /// so it is O(log N + visible_matches) regardless of total match count.
    pub(super) fn refresh_search_overlays(&mut self) {
        let _span = tracing::info_span!("refresh_search_overlays").entered();
        let search_bg = self.theme.search_match_bg;
        let search_fg = self.theme.search_match_fg;
        let ns = self.search_namespace.clone();

        // Determine the visible byte range from the active viewport
        let active_split = self.split_manager.active_split();
        let (top_byte, visible_height) = self
            .split_view_states
            .get(&active_split)
            .map(|vs| (vs.viewport.top_byte, vs.viewport.height.saturating_sub(2)))
            .unwrap_or((0, 20));

        // Remember the viewport we computed overlays for so we can detect
        // scrolling in check_search_overlay_refresh().
        self.search_overlay_top_byte = Some(top_byte);

        let state = self.active_state_mut();

        // Clear existing search overlays
        state.overlays.clear_namespace(&ns, &mut state.marker_list);

        // Walk visible lines to find the visible byte range
        let visible_start = top_byte;
        let mut visible_end = top_byte;
        {
            let mut line_iter = state.buffer.line_iterator(top_byte, 80);
            for _ in 0..visible_height {
                if let Some((line_start, line_content)) = line_iter.next_line() {
                    visible_end = line_start + line_content.len();
                } else {
                    break;
                }
            }
        }
        visible_end = visible_end.min(state.buffer.len());

        // Collect viewport matches into a local vec to avoid holding an
        // immutable borrow on self.search_state while we need &mut self for
        // the buffer state.
        let _ = state;

        let viewport_matches: Vec<(usize, usize)> = match &self.search_state {
            Some(ss) => {
                let start_idx = ss.matches.partition_point(|&pos| pos < visible_start);
                ss.matches[start_idx..]
                    .iter()
                    .zip(ss.match_lengths[start_idx..].iter())
                    .take_while(|(&pos, _)| pos <= visible_end)
                    .map(|(&pos, &len)| (pos, len))
                    .collect()
            }
            None => return,
        };

        let state = self.active_state_mut();

        for (pos, len) in &viewport_matches {
            let search_style = ratatui::style::Style::default().fg(search_fg).bg(search_bg);
            let overlay = crate::view::overlay::Overlay::with_namespace(
                &mut state.marker_list,
                *pos..(*pos + *len),
                crate::view::overlay::OverlayFace::Style {
                    style: search_style,
                },
                ns.clone(),
            )
            .with_priority_value(10);
            state.overlays.add(overlay);
        }
    }

    /// Check whether the viewport has scrolled since we last created search
    /// overlays. If so, refresh them. Called from `editor_tick()`.
    ///
    /// Only applies to large files where overlays are viewport-scoped.
    /// Small files already have overlays for ALL matches (created by
    /// `finalize_search`), so replacing them with viewport-only overlays
    /// would lose matches outside the visible area.
    pub(super) fn check_search_overlay_refresh(&mut self) -> bool {
        if self.search_state.is_none() {
            return false;
        }
        // Only refresh viewport-scoped overlays for large files
        if !self.active_state().buffer.is_large_file() {
            return false;
        }
        let active_split = self.split_manager.active_split();
        let current_top = self
            .split_view_states
            .get(&active_split)
            .map(|vs| vs.viewport.top_byte);
        if current_top != self.search_overlay_top_byte {
            self.refresh_search_overlays();
            true
        } else {
            false
        }
    }

    /// Start an incremental search scan for a large file.
    /// Splits the piece tree into ≤1 MB chunks and sets up the scan state
    /// that `process_search_scan()` (called from `editor_tick()`) will
    /// consume a few chunks per frame.
    fn start_search_scan(&mut self, query: &str, regex: regex::Regex) {
        let buffer_id = self.active_buffer();
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let leaves = state.buffer.piece_tree_leaves();
            // Build a bytes::Regex from the same pattern for the chunked scanner
            let bytes_regex = regex::bytes::RegexBuilder::new(regex.as_str())
                .case_insensitive(!self.search_case_sensitive)
                .build()
                .expect("regex already validated");
            let scan = state.buffer.search_scan_init(
                bytes_regex,
                super::SearchState::MAX_MATCHES,
                query.len(),
            );
            self.search_scan.start(
                buffer_id,
                leaves,
                scan,
                query.to_string(),
                None,
                self.search_case_sensitive,
                self.search_whole_word,
                self.search_use_regex,
            );
            self.set_status_message(t!("goto.scanning_progress", percent = 0).to_string());
        }
    }

    /// Get current match positions from search overlays (which use markers
    /// that auto-track edits).  Only useful for small files where we create
    /// overlays for ALL matches.
    fn get_search_match_positions(&self) -> Vec<usize> {
        let ns = &self.search_namespace;
        let state = self.active_state();

        let mut positions: Vec<usize> = state
            .overlays
            .all()
            .iter()
            .filter(|o| o.namespace.as_ref() == Some(ns))
            .filter_map(|o| state.marker_list.get_position(o.start_marker))
            .collect();

        positions.sort_unstable();
        positions.dedup();
        positions
    }

    /// Find the next match.
    ///
    /// For small files, overlay markers are used as the source of truth
    /// (they auto-track buffer edits).  For large files, `search_state.matches`
    /// is used directly and viewport overlays are refreshed after the cursor
    /// moves.
    pub(super) fn find_next(&mut self) {
        self.find_match_in_direction(SearchDirection::Forward);
    }

    /// Find the previous match.
    ///
    /// For small files, overlay markers are used as the source of truth
    /// (they auto-track buffer edits).  For large files, `search_state.matches`
    /// is used directly and viewport overlays are refreshed.
    pub(super) fn find_previous(&mut self) {
        self.find_match_in_direction(SearchDirection::Backward);
    }

    /// Navigate to the next or previous search match relative to the current
    /// cursor position. This matches standard editor behavior (VS Code,
    /// IntelliJ, etc.) where find always searches from the cursor, not from
    /// a stored match index.
    fn find_match_in_direction(&mut self, direction: SearchDirection) {
        let overlay_positions = self.get_search_match_positions();
        let is_large = self.active_state().buffer.is_large_file();

        if let Some(ref mut search_state) = self.search_state {
            // Use overlay positions for small files (they auto-track edits),
            // otherwise reference search_state.matches directly to avoid cloning.
            let use_overlays =
                !is_large && !overlay_positions.is_empty() && search_state.search_range.is_none();
            let match_positions: &[usize] = if use_overlays {
                &overlay_positions
            } else {
                &search_state.matches
            };

            if match_positions.is_empty() {
                return;
            }

            let cursor_pos = {
                let active_split = self.split_manager.active_split();
                self.split_view_states
                    .get(&active_split)
                    .map(|vs| vs.cursors.primary().position)
                    .unwrap_or(0)
            };

            let target_index = match direction {
                SearchDirection::Forward => {
                    // First match strictly after the cursor position.
                    let idx = match match_positions.binary_search(&(cursor_pos + 1)) {
                        Ok(i) | Err(i) => {
                            if i < match_positions.len() {
                                Some(i)
                            } else {
                                None
                            }
                        }
                    };
                    match idx {
                        Some(i) => i,
                        None if search_state.wrap_search => 0,
                        None => {
                            self.set_status_message(t!("search.no_matches").to_string());
                            return;
                        }
                    }
                }
                SearchDirection::Backward => {
                    // Last match strictly before the cursor position.
                    let idx = if cursor_pos == 0 {
                        None
                    } else {
                        match match_positions.binary_search(&(cursor_pos - 1)) {
                            Ok(i) => Some(i),
                            Err(i) => {
                                if i > 0 {
                                    Some(i - 1)
                                } else {
                                    None
                                }
                            }
                        }
                    };
                    match idx {
                        Some(i) => i,
                        None if search_state.wrap_search => match_positions.len() - 1,
                        None => {
                            self.set_status_message(t!("search.no_matches").to_string());
                            return;
                        }
                    }
                }
            };

            search_state.current_match_index = Some(target_index);
            let match_pos = match_positions[target_index];
            let matches_len = match_positions.len();

            self.move_cursor_to_match(match_pos);

            self.set_status_message(
                t!(
                    "search.match_of",
                    current = target_index + 1,
                    total = matches_len
                )
                .to_string(),
            );

            if is_large {
                self.refresh_search_overlays();
            }
        } else {
            let find_key = self
                .get_keybinding_for_action("find")
                .unwrap_or_else(|| "Ctrl+F".to_string());
            self.set_status_message(t!("search.no_active", find_key = find_key).to_string());
        }
    }

    /// Find the next occurrence of the current selection (or word under cursor).
    /// This is a "quick find" that doesn't require opening the search panel.
    /// The search term is stored so subsequent Alt+N/Alt+P/F3 navigation works.
    ///
    /// If there's already an active search, this continues with the same search term.
    /// Otherwise, it starts a new search with the current selection or word under cursor.
    pub(super) fn find_selection_next(&mut self) {
        // If there's already a search active AND cursor is at a match position,
        // just continue to next match. Otherwise, clear and start fresh.
        if let Some(ref search_state) = self.search_state {
            let cursor_pos = self.active_cursors().primary().position;
            if search_state.matches.binary_search(&cursor_pos).is_ok() {
                self.find_next();
                return;
            }
            // Cursor moved away from a match - clear search state
        }
        self.search_state = None;

        // No active search - start a new one with selection or word under cursor
        let (search_text, selection_start) = self.get_selection_or_word_for_search_with_pos();

        match search_text {
            Some(text) if !text.is_empty() => {
                // Record cursor position before search
                let cursor_before = self.active_cursors().primary().position;

                // Perform the search to set up search state
                self.perform_search(&text);

                // Check if we need to move to next match
                if let Some(ref search_state) = self.search_state {
                    let cursor_after = self.active_cursors().primary().position;

                    // If we started at a match (selection_start matches a search result),
                    // and perform_search didn't move us (or moved us to the same match),
                    // then we need to find_next
                    let started_at_match = selection_start
                        .map(|start| search_state.matches.binary_search(&start).is_ok())
                        .unwrap_or(false);

                    let landed_at_start = selection_start
                        .map(|start| cursor_after == start)
                        .unwrap_or(false);

                    // Only call find_next if:
                    // 1. We started at a match AND landed back at it, OR
                    // 2. We didn't move at all
                    if ((started_at_match && landed_at_start) || cursor_before == cursor_after)
                        && search_state.matches.len() > 1
                    {
                        self.find_next();
                    }
                }
            }
            _ => {
                self.set_status_message(t!("search.no_text").to_string());
            }
        }
    }

    /// Find the previous occurrence of the current selection (or word under cursor).
    /// This is a "quick find" that doesn't require opening the search panel.
    ///
    /// If there's already an active search, this continues with the same search term.
    /// Otherwise, it starts a new search with the current selection or word under cursor.
    pub(super) fn find_selection_previous(&mut self) {
        // If there's already a search active AND cursor is at a match position,
        // just continue to previous match. Otherwise, clear and start fresh.
        if let Some(ref search_state) = self.search_state {
            let cursor_pos = self.active_cursors().primary().position;
            if search_state.matches.binary_search(&cursor_pos).is_ok() {
                self.find_previous();
                return;
            }
            // Cursor moved away from a match - clear search state
        }
        self.search_state = None;

        // No active search - start a new one with selection or word under cursor
        let (search_text, selection_start) = self.get_selection_or_word_for_search_with_pos();

        match search_text {
            Some(text) if !text.is_empty() => {
                // Record cursor position before search
                let cursor_before = self.active_cursors().primary().position;

                // Perform the search to set up search state
                self.perform_search(&text);

                // If we found matches, navigate to previous
                if let Some(ref search_state) = self.search_state {
                    let cursor_after = self.active_cursors().primary().position;

                    // Check if we started at a match
                    let started_at_match = selection_start
                        .map(|start| search_state.matches.binary_search(&start).is_ok())
                        .unwrap_or(false);

                    let landed_at_start = selection_start
                        .map(|start| cursor_after == start)
                        .unwrap_or(false);

                    // For find previous, we always need to call find_previous at least once.
                    // If we landed at our starting match, we need to go back once to get previous.
                    // If we landed at a different match (because cursor was past start of selection),
                    // we still want to find_previous to get to where we should be.
                    if started_at_match && landed_at_start {
                        // We're at the same match we started at, go to previous
                        self.find_previous();
                    } else if cursor_before != cursor_after {
                        // perform_search moved us, now go back to find the actual previous
                        // from our original position (which is before where we landed)
                        self.find_previous();
                    } else {
                        // Cursor didn't move, just find previous
                        self.find_previous();
                    }
                }
            }
            _ => {
                self.set_status_message(t!("search.no_text").to_string());
            }
        }
    }

    /// Get the text to search for from selection or word under cursor,
    /// along with the start position of that text (for determining if we're at a match).
    fn get_selection_or_word_for_search_with_pos(&mut self) -> (Option<String>, Option<usize>) {
        use crate::primitives::word_navigation::{find_word_end, find_word_start};

        // First get selection range and cursor position with immutable borrow
        let (selection_range, cursor_pos) = {
            let primary = self.active_cursors().primary();
            (primary.selection_range(), primary.position)
        };

        // Check if there's a selection
        if let Some(range) = selection_range {
            let state = self.active_state_mut();
            let text = state.get_text_range(range.start, range.end);
            if !text.is_empty() {
                return (Some(text), Some(range.start));
            }
        }

        // No selection - try to get word under cursor
        let (word_start, word_end) = {
            let state = self.active_state();
            let word_start = find_word_start(&state.buffer, cursor_pos);
            let word_end = find_word_end(&state.buffer, cursor_pos);
            (word_start, word_end)
        };

        if word_start < word_end {
            let state = self.active_state_mut();
            (
                Some(state.get_text_range(word_start, word_end)),
                Some(word_start),
            )
        } else {
            (None, None)
        }
    }

    /// Perform a replace-all operation
    /// Build a compiled byte-regex for replace operations using current search settings.
    /// Returns None when regex mode is off (plain text matching should be used).
    fn build_replace_regex(&self, search: &str) -> Option<regex::bytes::Regex> {
        super::regex_replace::build_regex(
            search,
            self.search_use_regex,
            self.search_whole_word,
            self.search_case_sensitive,
        )
    }

    /// Get the length of a regex match at a given position in the buffer.
    fn get_regex_match_len(&mut self, regex: &regex::bytes::Regex, pos: usize) -> Option<usize> {
        let state = self.active_state_mut();
        let remaining = state.buffer.len().saturating_sub(pos);
        if remaining == 0 {
            return None;
        }
        let bytes = state.buffer.get_text_range_mut(pos, remaining).ok()?;
        regex.find(&bytes).map(|m| m.len())
    }

    /// Expand capture group references (e.g. $1, $2, ${name}) in the replacement string
    /// for a regex match at the given buffer position. Returns the expanded replacement.
    fn expand_regex_replacement(
        &mut self,
        regex: &regex::bytes::Regex,
        pos: usize,
        match_len: usize,
        replacement: &str,
    ) -> String {
        let state = self.active_state_mut();
        if let Ok(bytes) = state.buffer.get_text_range_mut(pos, match_len) {
            return super::regex_replace::expand_replacement(regex, &bytes, replacement);
        }
        replacement.to_string()
    }

    /// Replaces all occurrences of the search query with the replacement text
    ///
    /// OPTIMIZATION: Uses BulkEdit for O(n) tree operations instead of O(n²)
    /// This directly edits the piece tree without loading the entire buffer into memory
    pub(super) fn perform_replace(&mut self, search: &str, replacement: &str) {
        if search.is_empty() {
            self.set_status_message(t!("replace.empty_query").to_string());
            return;
        }

        let compiled_regex = self.build_replace_regex(search);

        // Find all matches first (before making any modifications)
        // Each match is (position, length, expanded_replacement)
        let matches: Vec<(usize, usize, String)> = if let Some(ref regex) = compiled_regex {
            // Regex mode: load buffer content as bytes and find all matches
            // with capture group expansion in the replacement template
            let buffer_bytes = {
                let state = self.active_state_mut();
                let total_bytes = state.buffer.len();
                match state.buffer.get_text_range_mut(0, total_bytes) {
                    Ok(bytes) => bytes,
                    Err(e) => {
                        tracing::warn!("Failed to load buffer for replace: {}", e);
                        self.set_status_message(t!("error.buffer_not_loaded").to_string());
                        return;
                    }
                }
            };
            super::regex_replace::collect_regex_matches(regex, &buffer_bytes, replacement)
                .into_iter()
                .map(|m| (m.offset, m.len, m.replacement))
                .collect()
        } else {
            // Plain text mode - replacement is used literally
            let state = self.active_state();
            let buffer_len = state.buffer.len();
            let mut matches = Vec::new();
            let mut current_pos = 0;

            while current_pos < buffer_len {
                if let Some(offset) = state.buffer.find_next_in_range(
                    search,
                    current_pos,
                    Some(current_pos..buffer_len),
                ) {
                    matches.push((offset, search.len(), replacement.to_string()));
                    current_pos = offset + search.len();
                } else {
                    break;
                }
            }
            matches
        };

        let count = matches.len();

        if count == 0 {
            self.set_status_message(t!("search.no_occurrences", search = search).to_string());
            return;
        }

        // Get cursor info for the event
        let cursor_id = self.active_cursors().primary_id();

        // Create Delete+Insert events for each match
        // Events will be processed in reverse order by apply_events_as_bulk_edit
        let mut events = Vec::with_capacity(count * 2);
        for (match_pos, match_len, expanded_replacement) in &matches {
            // Get the actual matched text for the delete event
            let deleted_text = self
                .active_state_mut()
                .get_text_range(*match_pos, match_pos + match_len);
            // Delete the matched text
            events.push(Event::Delete {
                range: *match_pos..match_pos + match_len,
                deleted_text,
                cursor_id,
            });
            // Insert the replacement (with capture groups expanded)
            events.push(Event::Insert {
                position: *match_pos,
                text: expanded_replacement.clone(),
                cursor_id,
            });
        }

        // Apply all replacements using BulkEdit for O(n) performance
        let description = format!("Replace all '{}' with '{}'", search, replacement);
        if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description) {
            self.active_event_log_mut().append(bulk_edit);
        }

        // Clear search state since positions are now invalid
        self.search_state = None;

        // Clear any search highlight overlays
        let ns = self.search_namespace.clone();
        let state = self.active_state_mut();
        state.overlays.clear_namespace(&ns, &mut state.marker_list);

        // Set status message
        self.set_status_message(
            t!(
                "search.replaced",
                count = count,
                search = search,
                replace = replacement
            )
            .to_string(),
        );
    }

    /// Start interactive replace mode (query-replace)
    pub(super) fn start_interactive_replace(&mut self, search: &str, replacement: &str) {
        if search.is_empty() {
            self.set_status_message(t!("replace.query_empty").to_string());
            return;
        }

        let compiled_regex = self.build_replace_regex(search);

        // Find the first match lazily (don't find all matches upfront)
        let start_pos = self.active_cursors().primary().position;
        let (first_match_pos, first_match_len) = if let Some(ref regex) = compiled_regex {
            let state = self.active_state();
            let buffer_len = state.buffer.len();
            // Try from cursor to end, then wrap from beginning
            let found = state
                .buffer
                .find_next_regex_in_range(regex, start_pos, Some(start_pos..buffer_len))
                .or_else(|| {
                    if start_pos > 0 {
                        state
                            .buffer
                            .find_next_regex_in_range(regex, 0, Some(0..start_pos))
                    } else {
                        None
                    }
                });
            let Some(pos) = found else {
                self.set_status_message(t!("search.no_occurrences", search = search).to_string());
                return;
            };
            // Determine the match length by re-matching at the found position
            let match_len = self.get_regex_match_len(regex, pos).unwrap_or(search.len());
            (pos, match_len)
        } else {
            let state = self.active_state();
            let Some(pos) = state.buffer.find_next(search, start_pos) else {
                self.set_status_message(t!("search.no_occurrences", search = search).to_string());
                return;
            };
            (pos, search.len())
        };

        // Initialize interactive replace state with just the current match
        self.interactive_replace_state = Some(InteractiveReplaceState {
            search: search.to_string(),
            replacement: replacement.to_string(),
            current_match_pos: first_match_pos,
            current_match_len: first_match_len,
            start_pos: first_match_pos,
            has_wrapped: false,
            replacements_made: 0,
            regex: compiled_regex,
        });

        // Move cursor to first match
        self.move_cursor_to_match(first_match_pos);

        // Show the query-replace prompt
        self.prompt = Some(Prompt::new(
            "Replace? (y)es (n)o (a)ll (c)ancel: ".to_string(),
            PromptType::QueryReplaceConfirm,
        ));
    }

    /// Handle interactive replace key press (y/n/a/c)
    pub(super) fn handle_interactive_replace_key(&mut self, c: char) -> AnyhowResult<()> {
        let state = self.interactive_replace_state.clone();
        let Some(mut ir_state) = state else {
            return Ok(());
        };

        match c {
            'y' | 'Y' => {
                // Replace current match
                self.replace_current_match(&ir_state)?;
                ir_state.replacements_made += 1;

                // Find next match lazily (after the replacement)
                let search_pos = ir_state.current_match_pos + ir_state.replacement.len();
                if let Some((next_match, match_len, wrapped)) =
                    self.find_next_match_for_replace(&ir_state, search_pos)
                {
                    ir_state.current_match_pos = next_match;
                    ir_state.current_match_len = match_len;
                    if wrapped {
                        ir_state.has_wrapped = true;
                    }
                    self.interactive_replace_state = Some(ir_state.clone());
                    self.move_to_current_match(&ir_state);
                } else {
                    self.finish_interactive_replace(ir_state.replacements_made);
                }
            }
            'n' | 'N' => {
                // Skip current match and find next
                let search_pos = ir_state.current_match_pos + ir_state.current_match_len;
                if let Some((next_match, match_len, wrapped)) =
                    self.find_next_match_for_replace(&ir_state, search_pos)
                {
                    ir_state.current_match_pos = next_match;
                    ir_state.current_match_len = match_len;
                    if wrapped {
                        ir_state.has_wrapped = true;
                    }
                    self.interactive_replace_state = Some(ir_state.clone());
                    self.move_to_current_match(&ir_state);
                } else {
                    self.finish_interactive_replace(ir_state.replacements_made);
                }
            }
            'a' | 'A' | '!' => {
                // Replace all remaining matches with SINGLE confirmation
                // Undo behavior: ONE undo step undoes ALL remaining replacements
                //
                // OPTIMIZATION: Uses BulkEdit for O(n) tree operations instead of O(n²)
                // This directly edits the piece tree without loading the entire buffer

                // Collect ALL match positions and lengths including the current match
                // Start from the current match position
                let all_matches: Vec<(usize, usize)> = {
                    let mut matches = Vec::new();
                    let mut temp_state = ir_state.clone();
                    temp_state.has_wrapped = false; // Reset wrap state to find current match

                    // First, include the current match
                    matches.push((ir_state.current_match_pos, ir_state.current_match_len));
                    let mut current_pos = ir_state.current_match_pos + ir_state.current_match_len;

                    // Find all remaining matches
                    while let Some((next_match, match_len, wrapped)) =
                        self.find_next_match_for_replace(&temp_state, current_pos)
                    {
                        matches.push((next_match, match_len));
                        current_pos = next_match + match_len;
                        if wrapped {
                            temp_state.has_wrapped = true;
                        }
                    }
                    matches
                };

                let total_count = all_matches.len();

                if total_count > 0 {
                    // Get cursor info for the event
                    let cursor_id = self.active_cursors().primary_id();

                    // Create Delete+Insert events for each match
                    let mut events = Vec::with_capacity(total_count * 2);
                    for &(match_pos, match_len) in &all_matches {
                        let deleted_text = self
                            .active_state_mut()
                            .get_text_range(match_pos, match_pos + match_len);
                        // Expand capture group references if in regex mode
                        let replacement_text = if let Some(ref regex) = ir_state.regex {
                            self.expand_regex_replacement(
                                regex,
                                match_pos,
                                match_len,
                                &ir_state.replacement,
                            )
                        } else {
                            ir_state.replacement.clone()
                        };
                        events.push(Event::Delete {
                            range: match_pos..match_pos + match_len,
                            deleted_text,
                            cursor_id,
                        });
                        events.push(Event::Insert {
                            position: match_pos,
                            text: replacement_text,
                            cursor_id,
                        });
                    }

                    // Apply all replacements using BulkEdit for O(n) performance
                    let description = format!(
                        "Replace all {} occurrences of '{}' with '{}'",
                        total_count, ir_state.search, ir_state.replacement
                    );
                    if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description) {
                        self.active_event_log_mut().append(bulk_edit);
                    }

                    ir_state.replacements_made += total_count;
                }

                self.finish_interactive_replace(ir_state.replacements_made);
            }
            'c' | 'C' | 'q' | 'Q' | '\x1b' => {
                // Cancel/quit interactive replace
                self.finish_interactive_replace(ir_state.replacements_made);
            }
            _ => {
                // Unknown key - ignored (prompt shows valid options)
            }
        }

        Ok(())
    }

    /// Find the next match for interactive replace (lazy search with wrap-around)
    /// Returns (match_position, match_length, wrapped)
    pub(super) fn find_next_match_for_replace(
        &mut self,
        ir_state: &InteractiveReplaceState,
        start_pos: usize,
    ) -> Option<(usize, usize, bool)> {
        if let Some(ref regex) = ir_state.regex {
            // Regex mode
            let regex = regex.clone();
            let state = self.active_state();
            let buffer_len = state.buffer.len();

            if ir_state.has_wrapped {
                let search_range = Some(start_pos..ir_state.start_pos);
                if let Some(match_pos) =
                    state
                        .buffer
                        .find_next_regex_in_range(&regex, start_pos, search_range)
                {
                    let match_len = self.get_regex_match_len(&regex, match_pos).unwrap_or(0);
                    return Some((match_pos, match_len, true));
                }
                None
            } else {
                let search_range = Some(start_pos..buffer_len);
                if let Some(match_pos) =
                    state
                        .buffer
                        .find_next_regex_in_range(&regex, start_pos, search_range)
                {
                    let match_len = self.get_regex_match_len(&regex, match_pos).unwrap_or(0);
                    return Some((match_pos, match_len, false));
                }

                // Wrap to beginning
                let wrap_range = Some(0..ir_state.start_pos);
                let state = self.active_state();
                if let Some(match_pos) =
                    state.buffer.find_next_regex_in_range(&regex, 0, wrap_range)
                {
                    let match_len = self.get_regex_match_len(&regex, match_pos).unwrap_or(0);
                    return Some((match_pos, match_len, true));
                }

                None
            }
        } else {
            // Plain text mode
            let search_len = ir_state.search.len();
            let state = self.active_state();

            if ir_state.has_wrapped {
                let search_range = Some(start_pos..ir_state.start_pos);
                if let Some(match_pos) =
                    state
                        .buffer
                        .find_next_in_range(&ir_state.search, start_pos, search_range)
                {
                    return Some((match_pos, search_len, true));
                }
                None
            } else {
                let buffer_len = state.buffer.len();
                let search_range = Some(start_pos..buffer_len);
                if let Some(match_pos) =
                    state
                        .buffer
                        .find_next_in_range(&ir_state.search, start_pos, search_range)
                {
                    return Some((match_pos, search_len, false));
                }

                let wrap_range = Some(0..ir_state.start_pos);
                if let Some(match_pos) =
                    state
                        .buffer
                        .find_next_in_range(&ir_state.search, 0, wrap_range)
                {
                    return Some((match_pos, search_len, true));
                }

                None
            }
        }
    }

    /// Replace the current match in interactive replace mode
    pub(super) fn replace_current_match(
        &mut self,
        ir_state: &InteractiveReplaceState,
    ) -> AnyhowResult<()> {
        let match_pos = ir_state.current_match_pos;
        let match_len = ir_state.current_match_len;
        let range = match_pos..(match_pos + match_len);

        // Expand capture group references if in regex mode
        let replacement_text = if let Some(ref regex) = ir_state.regex {
            self.expand_regex_replacement(regex, match_pos, match_len, &ir_state.replacement)
        } else {
            ir_state.replacement.clone()
        };

        // Get the deleted text for the event
        let deleted_text = self
            .active_state_mut()
            .get_text_range(range.start, range.end);

        // Capture current cursor state for undo
        let cursor_id = self.active_cursors().primary_id();
        let cursor = *self.active_cursors().primary();
        let old_position = cursor.position;
        let old_anchor = cursor.anchor;
        let old_sticky_column = cursor.sticky_column;

        // Create events: MoveCursor, Delete, Insert
        // The MoveCursor saves the cursor position so undo can restore it
        let events = vec![
            Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: match_pos,
                old_anchor,
                new_anchor: None,
                old_sticky_column,
                new_sticky_column: 0,
            },
            Event::Delete {
                range: range.clone(),
                deleted_text,
                cursor_id,
            },
            Event::Insert {
                position: match_pos,
                text: replacement_text,
                cursor_id,
            },
        ];

        // Wrap in batch for atomic undo
        let batch = Event::Batch {
            events,
            description: format!(
                "Query replace '{}' with '{}'",
                ir_state.search, ir_state.replacement
            ),
        };

        // Apply the batch through the event log
        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);

        Ok(())
    }

    /// Move cursor to the current match in interactive replace
    pub(super) fn move_to_current_match(&mut self, ir_state: &InteractiveReplaceState) {
        self.move_cursor_to_match(ir_state.current_match_pos);

        // Update the prompt message (show [Wrapped] if we've wrapped around)
        let msg = if ir_state.has_wrapped {
            "[Wrapped] Replace? (y)es (n)o (a)ll (c)ancel: ".to_string()
        } else {
            "Replace? (y)es (n)o (a)ll (c)ancel: ".to_string()
        };
        if let Some(ref mut prompt) = self.prompt {
            if prompt.prompt_type == PromptType::QueryReplaceConfirm {
                prompt.message = msg;
                prompt.input.clear();
                prompt.cursor_pos = 0;
            }
        }
    }

    /// Finish interactive replace and show summary
    pub(super) fn finish_interactive_replace(&mut self, replacements_made: usize) {
        self.interactive_replace_state = None;
        self.prompt = None; // Clear the query-replace prompt

        // Clear search highlights
        let ns = self.search_namespace.clone();
        let state = self.active_state_mut();
        state.overlays.clear_namespace(&ns, &mut state.marker_list);

        self.set_status_message(t!("search.replaced_count", count = replacements_made).to_string());
    }
}
