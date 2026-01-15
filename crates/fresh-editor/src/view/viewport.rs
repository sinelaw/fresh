use crate::model::buffer::Buffer;
use crate::model::cursor::Cursor;
use crate::primitives::display_width::{char_width, str_width};
use crate::primitives::line_wrapping::{char_position_to_segment, wrap_line, WrapConfig};
use crate::view::ui::view_pipeline::ViewLine;
/// The viewport - what portion of the buffer is visible
#[derive(Debug, Clone)]
pub struct Viewport {
    /// Byte position of the first visible line
    /// **This is the authoritative source of truth for all viewport operations**
    /// The line number for this byte is obtained from Buffer's LineCache
    pub top_byte: usize,

    /// View line offset within the current top_byte position
    /// Used when virtual lines precede source content at top_byte.
    /// For example, if top_byte=0 and there are 120 virtual lines before
    /// source line 1, top_view_line_offset=100 means skip the first 100
    /// virtual lines and start rendering from virtual line 101.
    pub top_view_line_offset: usize,

    /// Left column offset (horizontal scroll position)
    pub left_column: usize,

    /// Terminal dimensions
    pub width: u16,
    pub height: u16,

    /// Scroll offset (lines to keep visible above/below cursor)
    pub scroll_offset: usize,

    /// Horizontal scroll offset (columns to keep visible left/right of cursor)
    pub horizontal_scroll_offset: usize,

    /// Whether line wrapping is enabled
    /// When true, horizontal scrolling is disabled
    pub line_wrap_enabled: bool,

    /// Whether viewport needs synchronization with cursor positions
    /// When true, ensure_visible needs to be called before rendering
    /// This allows batching multiple cursor movements into a single viewport update
    needs_sync: bool,

    /// Whether to skip viewport sync on next resize
    /// This is set when restoring a session to prevent the restored scroll position
    /// from being overwritten by ensure_visible during the first render
    skip_resize_sync: bool,

    /// Whether to skip ensure_visible on next render
    /// This is set after scroll actions (Ctrl+Up/Down) to prevent the scroll
    /// from being immediately undone by ensure_visible
    skip_ensure_visible: bool,
}

impl Viewport {
    /// Create a new viewport
    pub fn new(width: u16, height: u16) -> Self {
        Self {
            top_byte: 0,
            top_view_line_offset: 0,
            left_column: 0,
            width,
            height,
            scroll_offset: 3,
            horizontal_scroll_offset: 5,
            line_wrap_enabled: false,
            needs_sync: false,
            skip_resize_sync: false,
            skip_ensure_visible: false,
        }
    }

    /// Mark viewport to skip sync on next resize (used after session restore)
    pub fn set_skip_resize_sync(&mut self) {
        self.skip_resize_sync = true;
    }

    /// Check and clear the skip_resize_sync flag
    /// Returns true if sync should be skipped
    pub fn should_skip_resize_sync(&mut self) -> bool {
        let skip = self.skip_resize_sync;
        self.skip_resize_sync = false;
        skip
    }

    /// Mark viewport to skip ensure_visible on next render
    /// This is used after scroll actions to prevent the scroll from being undone
    pub fn set_skip_ensure_visible(&mut self) {
        tracing::trace!("set_skip_ensure_visible: setting flag to true");
        self.skip_ensure_visible = true;
    }

    /// Check if ensure_visible should be skipped (does NOT consume the flag)
    /// Returns true if ensure_visible should be skipped
    pub fn should_skip_ensure_visible(&self) -> bool {
        self.skip_ensure_visible
    }

    /// Clear the skip_ensure_visible flag
    /// This should be called after all ensure_visible calls in a render pass
    pub fn clear_skip_ensure_visible(&mut self) {
        self.skip_ensure_visible = false;
    }

    /// Set the scroll offset
    pub fn set_scroll_offset(&mut self, offset: usize) {
        self.scroll_offset = offset;
    }

    /// Update terminal dimensions
    pub fn resize(&mut self, width: u16, height: u16) {
        self.width = width;
        self.height = height;
    }

    /// Get the number of visible lines
    pub fn visible_line_count(&self) -> usize {
        self.height as usize
    }

    /// Calculate the gutter width based on buffer length
    /// Format: "[indicator]{:>N} │ " where N is the number of digits for line numbers
    /// - Indicator column: 1 char (space, or symbols like ●/✗/⚠)
    /// - Line numbers: N digits (min 4), right-aligned
    /// - Separator: " │ " = 3 chars (space, box char, space)
    ///
    /// Total width = 1 + N + 3 = N + 4 (where N >= 4 minimum, so min 8 total)
    /// This is a heuristic that assumes approximately 80 chars per line
    pub fn gutter_width(&self, buffer: &Buffer) -> usize {
        let buffer_len = buffer.len();
        let estimated_lines = (buffer_len / 80).max(1);
        let digits = if estimated_lines == 0 {
            1
        } else {
            ((estimated_lines as f64).log10().floor() as usize) + 1
        };
        // 1 (indicator) + minimum 4 digits for readability + 3 (" │ ")
        1 + digits.max(4) + 3
    }

    /// Scroll up by N lines (byte-based)
    /// LineCache automatically tracks line numbers
    pub fn scroll_up(&mut self, buffer: &mut Buffer, lines: usize) {
        let mut iter = buffer.line_iterator(self.top_byte, 80);
        for _ in 0..lines {
            if iter.prev().is_none() {
                break;
            }
        }
        let new_position = iter.current_position();
        self.set_top_byte_with_limit(buffer, new_position);
    }

    /// Scroll down by N lines (byte-based)
    /// LineCache automatically tracks line numbers
    pub fn scroll_down(&mut self, buffer: &mut Buffer, lines: usize) {
        let mut iter = buffer.line_iterator(self.top_byte, 80);
        for _ in 0..lines {
            if iter.next_line().is_none() {
                break;
            }
        }
        let new_position = iter.current_position();
        self.set_top_byte_with_limit(buffer, new_position);
    }

    /// Scroll through ViewLines (view-transform aware)
    ///
    /// This method scrolls through display lines rather than source lines,
    /// correctly handling view transforms that inject headers or other content.
    ///
    /// # Arguments
    /// * `view_lines` - The current display lines (from ViewLineIterator)
    /// * `line_offset` - Positive to scroll down, negative to scroll up
    ///
    /// # Returns
    /// The new top_byte position after scrolling
    pub fn scroll_view_lines(&mut self, view_lines: &[ViewLine], line_offset: isize) {
        let viewport_height = self.visible_line_count();
        if view_lines.is_empty() || viewport_height == 0 {
            return;
        }

        // Find the current view line index that corresponds to top_byte
        let current_idx = self.find_view_line_for_byte(view_lines, self.top_byte);

        // Calculate target index
        let target_idx = if line_offset >= 0 {
            current_idx.saturating_add(line_offset as usize)
        } else {
            current_idx.saturating_sub(line_offset.unsigned_abs())
        };

        // Apply scroll limit: don't scroll past the point where viewport can't be filled
        let max_top_idx = view_lines.len().saturating_sub(viewport_height);
        let clamped_idx = target_idx.min(max_top_idx);

        // Get the source byte for the target view line
        if let Some(new_top_byte) = self.get_source_byte_for_view_line(view_lines, clamped_idx) {
            tracing::trace!(
                "scroll_view_lines: offset={}, current_idx={}, target_idx={}, clamped_idx={}, new_top_byte={}",
                line_offset, current_idx, target_idx, clamped_idx, new_top_byte
            );
            self.top_byte = new_top_byte;
        }
    }

    /// Find the view line index that contains a source byte position
    /// Returns the line where the byte falls within its range, not just the first line
    /// starting at or after the byte.
    fn find_view_line_for_byte(&self, view_lines: &[ViewLine], target_byte: usize) -> usize {
        // Find the line that contains the target byte by checking if target is
        // between this line's start and the next line's start
        let mut best_match = 0;

        for (idx, line) in view_lines.iter().enumerate() {
            if let Some(first_source) = line.char_source_bytes.iter().find_map(|m| *m) {
                if first_source <= target_byte {
                    // This line starts at or before target, so it might contain it
                    best_match = idx;
                } else {
                    // This line starts after target, so previous line contains it
                    break;
                }
            }
        }

        best_match
    }

    /// Get the source byte position for a view line index
    /// For injected lines (headers), walks forward to find the next source line
    fn get_source_byte_for_view_line(&self, view_lines: &[ViewLine], idx: usize) -> Option<usize> {
        // Start from the requested index and walk forward to find a line with source mapping
        for line in view_lines.iter().skip(idx) {
            if let Some(source_byte) = line.char_source_bytes.iter().find_map(|m| *m) {
                return Some(source_byte);
            }
        }
        // If all remaining lines are injected, try to get the last known source position
        // by walking backwards
        for line in view_lines.iter().take(idx).rev() {
            if let Some(source_byte) = line.char_source_bytes.iter().find_map(|m| *m) {
                // This is the last source position before our target
                // We want to stay at that position
                return Some(source_byte);
            }
        }
        // No source bytes found at all - keep current position
        Some(self.top_byte)
    }

    /// Ensure cursor is visible using view lines (Layout-aware)
    ///
    /// This method uses view lines to check visibility, correctly handling
    /// view transforms that inject headers or other virtual content.
    ///
    /// # Arguments
    /// * `view_lines` - The current display lines (from ViewLineIterator)
    /// * `cursor` - The cursor to ensure is visible
    /// * `gutter_width` - Width of the gutter (for cursor positioning)
    ///
    /// Returns true if scrolling occurred.
    pub fn ensure_visible_in_layout(
        &mut self,
        view_lines: &[ViewLine],
        cursor: &Cursor,
        gutter_width: usize,
    ) -> bool {
        // Check if we should skip sync due to session restore
        // This prevents the restored scroll position from being overwritten
        if self.should_skip_resize_sync() {
            return false;
        }

        // Check if we should skip ensure_visible due to scroll action
        // This prevents scroll actions (Ctrl+Up/Down) from being immediately undone
        if self.should_skip_ensure_visible() {
            tracing::trace!("ensure_visible_in_layout: SKIPPING due to skip_ensure_visible flag");
            return false;
        }
        tracing::trace!(
            "ensure_visible_in_layout: NOT skipping, skip_ensure_visible={}",
            self.skip_ensure_visible
        );

        let viewport_height = self.visible_line_count();
        if view_lines.is_empty() || viewport_height == 0 {
            return false;
        }

        // Find the cursor's absolute view line position (in the full view_lines array)
        let cursor_view_line = self.find_view_line_for_byte(view_lines, cursor.position);

        // The effective top view line is the offset we've scrolled through
        let effective_top = self.top_view_line_offset;
        let effective_bottom = effective_top + viewport_height;

        // Check if cursor is within visible range
        let cursor_is_visible =
            cursor_view_line >= effective_top && cursor_view_line < effective_bottom;

        if !cursor_is_visible {
            // Cursor is outside visible range - scroll to make it visible
            let target_top = if cursor_view_line < effective_top {
                // Cursor is above viewport - scroll up to show it
                cursor_view_line
            } else {
                // Cursor is below viewport - scroll down to put cursor near bottom
                cursor_view_line.saturating_sub(viewport_height - 1)
            };

            // Apply scroll limit
            let max_top = view_lines.len().saturating_sub(viewport_height);
            let new_offset = target_top.min(max_top);

            tracing::trace!(
                "ensure_visible_in_layout: scrolling from offset {} to {}, cursor_view_line={}",
                self.top_view_line_offset,
                new_offset,
                cursor_view_line
            );

            self.top_view_line_offset = new_offset;
            // Also update top_byte to match the new scroll position
            if let Some(new_top_byte) = self.get_source_byte_for_view_line(view_lines, new_offset) {
                self.top_byte = new_top_byte;
            }
            return true;
        }

        // Special case: When cursor is at the first view line of the viewport,
        // check if there are virtual lines above the cursor that should be visible.
        // Scroll up to show them, but keep the cursor visible within the viewport.
        let cursor_position_in_viewport = cursor_view_line.saturating_sub(effective_top);
        if cursor_position_in_viewport == 0 && cursor_view_line > 0 {
            // Cursor is at the top of the viewport, and there are lines above it
            // Count how many virtual lines (lines without source content) precede the cursor
            let mut virtual_lines_above = 0;
            for i in (0..cursor_view_line).rev() {
                let has_source = view_lines[i].char_source_bytes.iter().any(|m| m.is_some());
                if has_source {
                    break; // Hit a source line, stop counting
                }
                virtual_lines_above += 1;
            }

            if virtual_lines_above > 0 {
                // Scroll up to show virtual lines, but ensure cursor stays visible
                // The cursor should be at the bottom of the visible area at most
                let max_scroll_up = virtual_lines_above.min(viewport_height.saturating_sub(1));
                let new_offset = effective_top.saturating_sub(max_scroll_up);

                if new_offset != self.top_view_line_offset {
                    tracing::trace!(
                        "ensure_visible_in_layout: showing {} virtual lines above cursor, scrolling from {} to {}",
                        virtual_lines_above,
                        self.top_view_line_offset,
                        new_offset
                    );
                    self.top_view_line_offset = new_offset;
                    // Also update top_byte to match the new scroll position
                    if let Some(new_top_byte) =
                        self.get_source_byte_for_view_line(view_lines, new_offset)
                    {
                        self.top_byte = new_top_byte;
                    }
                    return true;
                }
            }
        }

        // Handle horizontal scrolling for cursor column
        if cursor_view_line < view_lines.len() {
            let line = &view_lines[cursor_view_line];
            // Get the byte position of the first character in this line
            // Then calculate cursor column as visual width from line start
            let line_start = line.char_source_bytes.iter().find_map(|m| *m).unwrap_or(0);

            // Calculate the byte position where this line ends (start of next line or end of view)
            // If cursor is beyond this line's content, skip horizontal scroll - the cursor
            // is on a line not in view_lines (e.g., a newly inserted line)
            let line_end_byte = if cursor_view_line + 1 < view_lines.len() {
                // Next line exists, use its start as this line's end
                view_lines[cursor_view_line + 1]
                    .char_source_bytes
                    .iter()
                    .find_map(|m| *m)
                    .unwrap_or(usize::MAX)
            } else {
                // This is the last view line - check if cursor is beyond line content
                // The line's content length (including newline) determines the end
                let content_bytes = line.text.len();
                line_start.saturating_add(content_bytes)
            };

            // Only handle horizontal scroll if cursor is actually within this line
            if cursor.position < line_end_byte {
                let cursor_byte_offset = cursor.position.saturating_sub(line_start);

                // Calculate visual column by walking through characters and summing widths
                // until we've consumed cursor_byte_offset bytes
                let line_text = line.text.trim_end_matches('\n');
                let mut bytes_consumed = 0usize;
                let mut cursor_visual_col = 0usize;
                for ch in line_text.chars() {
                    if bytes_consumed >= cursor_byte_offset {
                        break;
                    }
                    cursor_visual_col += char_width(ch);
                    bytes_consumed += ch.len_utf8();
                }

                let line_visual_width = str_width(line_text);
                self.ensure_column_visible_simple(
                    cursor_visual_col,
                    line_visual_width,
                    gutter_width,
                );
            }
            // If cursor.position >= line_end_byte, cursor is on a line not in view_lines
            // Skip horizontal scroll handling - ensure_visible already handled it correctly
        }

        false
    }

    /// Simple column visibility check (doesn't need buffer)
    fn ensure_column_visible_simple(
        &mut self,
        column: usize,
        line_length: usize,
        gutter_width: usize,
    ) {
        // Skip if line wrapping is enabled (all columns visible via wrapping)
        if self.line_wrap_enabled {
            self.left_column = 0;
            return;
        }

        let scrollbar_width = 1;
        let visible_width = (self.width as usize)
            .saturating_sub(gutter_width)
            .saturating_sub(scrollbar_width);

        if visible_width == 0 {
            return;
        }

        let effective_offset = self.horizontal_scroll_offset.min(visible_width / 2);
        let ideal_left = self.left_column + effective_offset;
        let ideal_right = self.left_column + visible_width.saturating_sub(effective_offset);

        if column < ideal_left {
            self.left_column = column.saturating_sub(effective_offset);
        } else if column >= ideal_right {
            let target_position = visible_width
                .saturating_sub(effective_offset)
                .saturating_sub(1);
            self.left_column = column.saturating_sub(target_position);
        }

        // Limit scroll to line length
        if line_length > 0 {
            let max_left_column = line_length.saturating_sub(visible_width.saturating_sub(1));
            if self.left_column > max_left_column {
                self.left_column = max_left_column;
            }
        }
    }

    /// Set top_byte with automatic scroll limit enforcement
    /// This prevents scrolling past the end of the buffer by ensuring
    /// the viewport can be filled from the proposed position
    fn set_top_byte_with_limit(&mut self, buffer: &mut Buffer, proposed_top_byte: usize) {
        tracing::trace!(
            "DEBUG set_top_byte_with_limit: proposed_top_byte={}",
            proposed_top_byte
        );

        let viewport_height = self.visible_line_count();
        if viewport_height == 0 {
            self.top_byte = proposed_top_byte;
            return;
        }

        let buffer_len = buffer.len();
        if buffer_len == 0 {
            self.top_byte = 0;
            return;
        }

        // Try to iterate viewport_height lines from proposed_top_byte
        // If we can't reach viewport_height lines before hitting EOF,
        // then we need to adjust backward
        let mut iter = buffer.line_iterator(proposed_top_byte, 80);
        let mut lines_visible = 0;

        while let Some((_, _)) = iter.next_line() {
            lines_visible += 1;
            if lines_visible >= viewport_height {
                // We have a full viewport of content, use proposed position
                tracing::trace!(
                    "DEBUG: Full viewport available, setting top_byte={}",
                    proposed_top_byte
                );
                self.top_byte = proposed_top_byte;
                return;
            }
        }

        tracing::trace!(
            "DEBUG: After iteration, lines_visible={}, viewport_height={}",
            lines_visible,
            viewport_height
        );

        // If we have enough lines to fill the viewport, we're good
        if lines_visible >= viewport_height {
            tracing::trace!(
                "DEBUG: Enough lines to fill viewport, setting top_byte={}",
                proposed_top_byte
            );
            self.top_byte = proposed_top_byte;
            return;
        }

        // We don't have enough lines to fill the viewport from proposed_top_byte
        // Calculate how many lines we're short and scroll back
        let lines_short = viewport_height - lines_visible;
        tracing::trace!("DEBUG: lines_short={}, scrolling back", lines_short);

        let mut backtrack_iter = buffer.line_iterator(proposed_top_byte, 80);
        tracing::trace!(
            "DEBUG: Backtracking from byte {}",
            backtrack_iter.current_position()
        );
        for i in 0..lines_short {
            let pos_before = backtrack_iter.current_position();
            if backtrack_iter.prev().is_none() {
                tracing::trace!(
                    "DEBUG: Hit beginning of buffer at backtrack iteration {}",
                    i
                );
                break; // Hit the beginning of the buffer
            }
            let pos_after = backtrack_iter.current_position();
            tracing::trace!(
                "DEBUG: Backtrack iteration {}: {} -> {}",
                i,
                pos_before,
                pos_after
            );
        }

        let final_top_byte = backtrack_iter.current_position();
        tracing::trace!(
            "DEBUG: After backtracking, setting top_byte={}",
            final_top_byte
        );
        self.top_byte = final_top_byte;
    }

    /// Scroll to a specific line (byte-based)
    /// This seeks from the beginning to find the byte position of the line
    pub fn scroll_to(&mut self, buffer: &mut Buffer, line: usize) {
        // Seek from the beginning to find the byte position for this line
        let mut iter = buffer.line_iterator(0, 80);
        let mut current_line = 0;

        while current_line < line {
            if let Some((line_start, _)) = iter.next_line() {
                if current_line + 1 == line {
                    self.set_top_byte_with_limit(buffer, line_start);
                    return;
                }
                current_line += 1;
            } else {
                // Reached end of buffer before target line
                break;
            }
        }

        // If we didn't find the line, stay at the last valid position
        let target_position = iter.current_position();
        self.set_top_byte_with_limit(buffer, target_position);
    }

    /// Mark viewport as needing synchronization with cursor positions
    /// This defers the actual viewport update until sync_with_cursor is called
    pub fn mark_needs_sync(&mut self) {
        self.needs_sync = true;
    }

    /// Check if viewport needs synchronization
    pub fn needs_sync(&self) -> bool {
        self.needs_sync
    }

    /// Synchronize viewport with cursor position (deferred ensure_visible)
    /// This should be called before rendering to batch multiple cursor movements
    pub fn sync_with_cursor(&mut self, buffer: &mut Buffer, cursor: &Cursor) {
        if self.needs_sync {
            self.ensure_visible(buffer, cursor);
            self.needs_sync = false;
        }
    }

    /// Ensure a cursor is visible, scrolling if necessary (smart scroll)
    /// Now works entirely with byte offsets - no line number calculations needed!
    pub fn ensure_visible(&mut self, buffer: &mut Buffer, cursor: &Cursor) {
        // Check if we should skip sync due to session restore
        // This prevents the restored scroll position from being overwritten
        if self.should_skip_resize_sync() {
            tracing::trace!("ensure_visible: SKIPPING due to skip_resize_sync");
            return;
        }

        // Check if we should skip ensure_visible due to scroll action
        // This prevents scroll actions (Ctrl+Up/Down) from being immediately undone
        if self.should_skip_ensure_visible() {
            tracing::trace!("ensure_visible: SKIPPING due to skip_ensure_visible flag");
            return;
        }
        tracing::trace!(
            "ensure_visible: NOT skipping, skip_ensure_visible={}",
            self.skip_ensure_visible
        );

        // For large files with lazy loading, ensure data around cursor is loaded
        let viewport_lines = self.visible_line_count().max(1);

        tracing::trace!(
            "ensure_visible: cursor={}, top_byte={}, viewport_lines={}, line_wrap={}",
            cursor.position,
            self.top_byte,
            viewport_lines,
            self.line_wrap_enabled
        );

        // CRITICAL: Load data around cursor position explicitly before using iterators
        // Load enough data to cover viewport above and below cursor
        let estimated_viewport_bytes = viewport_lines * 200;
        let load_start = cursor.position.saturating_sub(estimated_viewport_bytes * 2);
        // Cap load_length to not go past EOF
        let buffer_len = buffer.len();
        let remaining_bytes = buffer_len.saturating_sub(load_start);
        let load_length = (estimated_viewport_bytes * 3).min(remaining_bytes);

        // Force-load the data by actually requesting it (not just prepare_viewport)
        if let Err(e) = buffer.get_text_range_mut(load_start, load_length) {
            tracing::warn!(
                "Failed to load data around cursor at {}: {}",
                cursor.position,
                e
            );
        }

        // Find the start of the line containing the cursor using iterator
        let cursor_iter = buffer.line_iterator(cursor.position, 80);
        let cursor_line_start = cursor_iter.current_position();

        // Check if cursor is visible by counting VISUAL ROWS between top_byte and cursor
        // When line wrapping is enabled, we need to count wrapped rows, not logical lines!
        // Apply scroll_offset to keep cursor away from edges
        let effective_offset = self.scroll_offset.min(viewport_lines / 2);

        let cursor_is_visible = if cursor_line_start < self.top_byte {
            // Cursor is above viewport
            false
        } else if self.line_wrap_enabled {
            // With line wrapping: count VISUAL ROWS (wrapped segments), not logical lines
            let gutter_width = self.gutter_width(buffer);
            let wrap_config = WrapConfig::new(self.width as usize, gutter_width, true);

            let mut iter = buffer.line_iterator(self.top_byte, 80);
            let mut visual_rows = 0;

            // Iterate through logical lines, but count their wrapped rows
            loop {
                let current_pos = iter.current_position();

                // If we reached the cursor's line, check if the cursor is within visible rows
                if current_pos >= cursor_line_start {
                    // The cursor's line starts within or after the viewport
                    if current_pos == cursor_line_start {
                        // We need to check if the cursor's SPECIFIC POSITION within the wrapped line is visible
                        // Get the line content
                        let line_content = if let Some((_, content)) = iter.next_line() {
                            content.trim_end_matches('\n').to_string()
                        } else {
                            // At EOF after trailing newline - empty line
                            String::new()
                        };

                        // Wrap the line (even if empty, it still takes 1 row)
                        let segments = wrap_line(&line_content, &wrap_config);
                        let segments_count = segments.len().max(1); // Empty line is 1 row

                        // Find which segment the cursor is in
                        let cursor_column = cursor.position.saturating_sub(cursor_line_start);
                        let (cursor_segment_idx, _) =
                            char_position_to_segment(cursor_column, &segments);

                        // Add the rows for this line up to and including the cursor's segment
                        // For empty lines, cursor_segment_idx is 0, so we add 1 row
                        visual_rows += cursor_segment_idx.min(segments_count - 1) + 1;

                        // Check if cursor's row is within viewport with scroll offset applied
                        // Cursor should be between effective_offset and (viewport_lines - effective_offset)
                        break visual_rows > effective_offset
                            && visual_rows <= viewport_lines.saturating_sub(effective_offset);
                    } else {
                        // We passed the cursor's line without finding it - shouldn't happen
                        break false;
                    }
                }

                // Get the next line
                if let Some((_line_start, line_content)) = iter.next_line() {
                    // Wrap this line to count how many visual rows it takes
                    let line_text = line_content.trim_end_matches('\n');
                    let segments = wrap_line(line_text, &wrap_config);
                    visual_rows += segments.len();

                    // If we've exceeded the viewport, cursor is not visible
                    if visual_rows >= viewport_lines {
                        break false;
                    }
                } else {
                    // Reached end of buffer
                    break false;
                }
            }
        } else {
            // Without line wrapping: count logical lines as before
            let mut iter = buffer.line_iterator(self.top_byte, 80);
            let mut lines_from_top = 0;

            while iter.current_position() < cursor_line_start && lines_from_top < viewport_lines {
                if iter.next_line().is_none() {
                    break;
                }
                lines_from_top += 1;
            }

            // Apply scroll offset: cursor should be between offset and (viewport_lines - offset)
            let visible = lines_from_top > effective_offset
                && lines_from_top < viewport_lines.saturating_sub(effective_offset);
            tracing::trace!(
                "ensure_visible (no wrap): lines_from_top={}, effective_offset={}, visible={}",
                lines_from_top,
                effective_offset,
                visible
            );
            visible
        };

        tracing::trace!(
            "ensure_visible: cursor_line_start={}, cursor_is_visible={}",
            cursor_line_start,
            cursor_is_visible
        );

        // If cursor is not visible, scroll to make it visible
        if !cursor_is_visible {
            // Position cursor at center of viewport when jumping
            let target_rows_from_top = viewport_lines / 2;

            if self.line_wrap_enabled {
                // When wrapping is enabled, count visual rows (wrapped segments) not logical lines
                let gutter_width = self.gutter_width(buffer);
                let wrap_config = WrapConfig::new(self.width as usize, gutter_width, true);

                let mut iter = buffer.line_iterator(cursor_line_start, 80);
                let mut visual_rows_counted = 0;

                // First, count how many rows the cursor's line takes up to the cursor position
                if let Some((_line_start, line_content)) = iter.next_line() {
                    let line_text = if line_content.ends_with('\n') {
                        &line_content[..line_content.len() - 1]
                    } else {
                        &line_content
                    };
                    let segments = wrap_line(line_text, &wrap_config);
                    let cursor_column = cursor.position.saturating_sub(cursor_line_start);
                    let (cursor_segment_idx, _) =
                        char_position_to_segment(cursor_column, &segments);
                    visual_rows_counted += cursor_segment_idx + 1;
                } else {
                    // At EOF after trailing newline - cursor is on empty line, needs 1 row
                    visual_rows_counted += 1;
                }

                // Now move backwards counting visual rows until we reach target
                iter = buffer.line_iterator(cursor_line_start, 80);
                while visual_rows_counted < target_rows_from_top {
                    if iter.prev().is_none() {
                        break; // Hit beginning of buffer
                    }

                    if let Some((_line_start, line_content)) = iter.next_line() {
                        let line_text = if line_content.ends_with('\n') {
                            &line_content[..line_content.len() - 1]
                        } else {
                            &line_content
                        };
                        let segments = wrap_line(line_text, &wrap_config);
                        visual_rows_counted += segments.len();

                        // Move back to where prev() left us
                        iter.prev();
                    }
                }

                let new_top_byte = iter.current_position();
                self.set_top_byte_with_limit(buffer, new_top_byte);
            } else {
                // Non-wrapped mode: count logical lines as before
                let mut iter = buffer.line_iterator(cursor_line_start, 80);

                for _ in 0..target_rows_from_top {
                    if iter.prev().is_none() {
                        break; // Hit beginning of buffer
                    }
                }

                let new_top_byte = iter.current_position();
                tracing::trace!(
                    "ensure_visible: SCROLLING from top_byte={} to new_top_byte={} (target_rows={})",
                    self.top_byte, new_top_byte, target_rows_from_top
                );
                self.set_top_byte_with_limit(buffer, new_top_byte);
            }
        }

        // Horizontal scrolling - skip if line wrapping is enabled
        // When wrapping is enabled, all columns are always visible via wrapping
        if !self.line_wrap_enabled {
            let cursor_column = cursor.position.saturating_sub(cursor_line_start);

            // Get the line content to know its length (for limiting horizontal scroll)
            let mut line_iter = buffer.line_iterator(cursor_line_start, 80);
            let line_length = if let Some((_start, content)) = line_iter.next_line() {
                // Line length without the newline character
                content.trim_end_matches('\n').len()
            } else {
                0
            };

            self.ensure_column_visible(cursor_column, line_length, buffer);
        } else {
            // With line wrapping enabled, reset any horizontal scroll
            self.left_column = 0;
        }
    }

    /// Ensure a line is visible with scroll offset applied
    /// This is a legacy method kept for backward compatibility with tests
    /// In practice, use ensure_visible() which works directly with cursors and bytes
    pub fn ensure_line_visible(&mut self, buffer: &mut Buffer, line: usize) {
        // Seek to the target line to get its byte position
        let mut seek_iter = buffer.line_iterator(0, 80);
        let mut current_line = 0;
        let mut target_line_byte = 0;

        while current_line < line {
            if let Some((line_start, _)) = seek_iter.next_line() {
                if current_line + 1 == line {
                    target_line_byte = line_start;
                    break;
                }
                current_line += 1;
            } else {
                // Reached end of buffer before target line
                return;
            }
        }

        // Check if the line is already visible by iterating from top_byte
        let visible_count = self.visible_line_count();
        let mut iter = buffer.line_iterator(self.top_byte, 80);
        let mut lines_from_top = 0;
        let mut target_is_visible = false;

        while let Some((line_byte, _)) = iter.next_line() {
            if line_byte == target_line_byte {
                target_is_visible = lines_from_top < visible_count;
                break;
            }
            lines_from_top += 1;
            if lines_from_top >= visible_count {
                break;
            }
        }

        // If not visible, scroll to show it with scroll offset
        if !target_is_visible {
            let effective_offset = self.scroll_offset.min(visible_count / 2);
            let target_line_from_top = effective_offset;

            // Move backwards from target to find new top_byte
            let mut iter = buffer.line_iterator(target_line_byte, 80);
            for _ in 0..target_line_from_top {
                if iter.prev().is_none() {
                    break;
                }
            }
            let position = iter.current_position();
            self.set_top_byte_with_limit(buffer, position);
        }
    }

    /// Ensure a column is visible with horizontal scroll offset applied
    ///
    /// # Arguments
    /// * `column` - The column position within the line (0-indexed)
    /// * `line_length` - The length of the line content (without newline)
    /// * `buffer` - The buffer (for calculating gutter width)
    pub fn ensure_column_visible(
        &mut self,
        column: usize,
        line_length: usize,
        buffer: &mut Buffer,
    ) {
        // Calculate visible width (accounting for line numbers gutter which is dynamic)
        let gutter_width = self.gutter_width(buffer);
        // Also account for scrollbar (always present, takes 1 column)
        let scrollbar_width = 1;
        let visible_width = (self.width as usize)
            .saturating_sub(gutter_width)
            .saturating_sub(scrollbar_width);

        if visible_width == 0 {
            return; // Terminal too narrow
        }

        // If viewport is too small for scroll offset, use what we can
        let effective_offset = self.horizontal_scroll_offset.min(visible_width / 2);

        // Calculate the ideal left and right boundaries with scroll offset
        let ideal_left = self.left_column + effective_offset;
        let ideal_right = self.left_column + visible_width.saturating_sub(effective_offset);

        if column < ideal_left {
            // Cursor is to the left of the ideal zone - scroll left
            self.left_column = column.saturating_sub(effective_offset);
        } else if column >= ideal_right {
            // Cursor is to the right of the ideal zone - scroll right
            // Place cursor at (visible_width - effective_offset - 1) to keep it in valid range [0, visible_width-1]
            let target_position = visible_width
                .saturating_sub(effective_offset)
                .saturating_sub(1);
            self.left_column = column.saturating_sub(target_position);
        }

        // BUGFIX: Limit left_column to ensure content is always visible
        // Don't scroll past the point where the end of the line would be off-screen to the left
        // This prevents the viewport from scrolling into "empty space" past the line content
        if line_length > 0 {
            // Calculate the maximum left_column that still shows some content
            // Account for cursor potentially being one position past the line content (at position line_length)
            // If the line is shorter than visible width, left_column should be 0
            // Otherwise, allow scrolling enough to show position line_length at the last visible column
            let max_left_column = line_length.saturating_sub(visible_width.saturating_sub(1));

            // Limit left_column to max_left_column
            if self.left_column > max_left_column {
                self.left_column = max_left_column;
            }
        }
    }

    /// Ensure multiple cursors are visible (smart scroll for multi-cursor)
    /// Prioritizes keeping the primary cursor visible
    pub fn ensure_cursors_visible(
        &mut self,
        buffer: &mut Buffer,
        cursors: &[(usize, &Cursor)], // (priority, cursor) - lower priority number = higher priority
    ) {
        if cursors.is_empty() {
            return;
        }

        // Sort cursors by priority (primary cursor first)
        let mut sorted_cursors: Vec<_> = cursors.to_vec();
        sorted_cursors.sort_by_key(|(priority, _)| *priority);

        // Get byte positions for all cursors (at line starts)
        let cursor_line_bytes: Vec<usize> = sorted_cursors
            .iter()
            .map(|(_, cursor)| {
                let iter = buffer.line_iterator(cursor.position, 80);
                iter.current_position()
            })
            .collect();

        // Count how many lines span between min and max cursors
        let min_byte = *cursor_line_bytes.iter().min().unwrap();
        let max_byte = *cursor_line_bytes.iter().max().unwrap();

        // Count lines between min and max using iterator
        let mut iter = buffer.line_iterator(min_byte, 80);
        let mut line_span = 0;
        while let Some((line_byte, _)) = iter.next_line() {
            if line_byte >= max_byte {
                break;
            }
            line_span += 1;
        }

        let visible_count = self.visible_line_count();

        // If all cursors fit in the viewport, center them
        if line_span < visible_count {
            let lines_to_go_back = visible_count / 2;
            let mut iter = buffer.line_iterator(min_byte, 80);
            for _ in 0..lines_to_go_back {
                if iter.prev().is_none() {
                    break;
                }
            }
            let position = iter.current_position();
            self.set_top_byte_with_limit(buffer, position);
        } else {
            // Can't fit all cursors, ensure primary is visible
            let primary_cursor = sorted_cursors[0].1;
            self.ensure_visible(buffer, primary_cursor);
        }
    }

    /// Get the cursor screen position (x, y) which is (col, row) for rendering
    /// This returns the position relative to the viewport, accounting for horizontal scrolling
    ///
    /// NOTE: This function is kept for popup positioning and multi-cursor display,
    /// but is NO LONGER used for primary cursor rendering, which now happens during
    /// the line rendering loop in split_rendering.rs to eliminate duplicate line iteration.
    pub fn cursor_screen_position(&self, buffer: &mut Buffer, cursor: &Cursor) -> (u16, u16) {
        // Find line start using iterator
        let cursor_iter = buffer.line_iterator(cursor.position, 80);
        let line_start = cursor_iter.current_position();
        let column = cursor.position.saturating_sub(line_start);

        // Count lines from top_byte to cursor to get screen row
        let mut iter = buffer.line_iterator(self.top_byte, 80);
        let mut screen_row = 0;

        while let Some((line_byte, _)) = iter.next_line() {
            if line_byte >= line_start {
                break;
            }
            screen_row += 1;
        }

        // Calculate screen column and additional wrapped rows if line wrapping is enabled
        let (screen_col, additional_rows) = if self.line_wrap_enabled {
            // Use new clean wrapping implementation
            let gutter_width = self.gutter_width(buffer);
            let config = WrapConfig::new(self.width as usize, gutter_width, true);

            // Get the line text for wrapping
            let mut line_iter = buffer.line_iterator(line_start, 80);
            let line_text = if let Some((_start, content)) = line_iter.next_line() {
                // Remove trailing newline if present
                content.trim_end_matches('\n').to_string()
            } else {
                String::new()
            };

            // Wrap the line
            let segments = wrap_line(&line_text, &config);

            // Find which segment the cursor is in
            let (segment_idx, col_in_segment) = char_position_to_segment(column, &segments);

            (col_in_segment as u16, segment_idx)
        } else {
            // No wrapping - account for horizontal scrolling
            let screen_col = column.saturating_sub(self.left_column) as u16;
            (screen_col, 0)
        };

        // Return (x, y) which is (col, row)
        // Add the additional wrapped rows to the screen row
        (screen_col, (screen_row + additional_rows) as u16)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::Buffer;
    use crate::model::cursor::Cursor;

    #[test]
    fn test_viewport_new() {
        let vp = Viewport::new(80, 24);
        assert_eq!(vp.width, 80);
        assert_eq!(vp.height, 24);
        assert_eq!(vp.top_byte, 0);
    }

    #[test]
    fn test_scroll_up_down() {
        // Create a buffer with more lines than the viewport to make scrolling possible
        let mut content = String::new();
        for i in 1..=50 {
            if i > 1 {
                content.push('\n');
            }
            content.push_str(&format!("line{}", i));
        }
        let mut buffer = Buffer::from_str_test(&content);
        let mut vp = Viewport::new(80, 24);

        vp.scroll_down(&mut buffer, 10);
        // Check that we scrolled down (top_byte should be > 0)
        assert!(vp.top_byte > 0);

        let prev_top = vp.top_byte;
        vp.scroll_up(&mut buffer, 5);
        // Check that we scrolled up (top_byte should be less than before)
        assert!(vp.top_byte < prev_top);

        vp.scroll_up(&mut buffer, 100);
        assert_eq!(vp.top_byte, 0); // Can't scroll past 0
    }

    #[test]
    fn test_ensure_line_visible() {
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20\nline21\nline22\nline23\nline24\nline25\nline26\nline27\nline28\nline29\nline30\nline31\nline32\nline33\nline34\nline35\nline36\nline37\nline38\nline39\nline40\nline41\nline42\nline43\nline44\nline45\nline46\nline47\nline48\nline49\nline50\nline51");
        let mut vp = Viewport::new(80, 24);
        vp.scroll_offset = 3;

        // Line within scroll offset should adjust viewport
        vp.ensure_line_visible(&mut buffer, 2);
        // top_byte should be close to the beginning since line 2 is near the top
        assert!(vp.top_byte < 100);

        // Line far below should scroll down
        vp.ensure_line_visible(&mut buffer, 50);
        assert!(vp.top_byte > 0);
        // Verify the line is now visible by checking we can iterate to it
        let mut iter = buffer.line_iterator(vp.top_byte, 80);
        let mut found = false;
        for _ in 0..vp.visible_line_count() {
            if iter.next_line().is_none() {
                break;
            }
            found = true;
        }
        assert!(found);
    }

    #[test]
    fn test_ensure_visible_with_cursor() {
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
        let mut vp = Viewport::new(80, 10);

        // Find byte position of line 15 using iterator
        let mut iter = buffer.line_iterator(0, 80);
        let mut cursor_pos = 0;
        for i in 0..15 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 14 {
                    cursor_pos = line_start;
                    break;
                }
            }
        }

        let cursor = Cursor::new(cursor_pos);
        vp.ensure_visible(&mut buffer, &cursor);

        // Verify cursor is now visible by checking we scrolled appropriately
        assert!(vp.top_byte > 0);
    }

    #[test]
    fn test_cursor_screen_position() {
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3");
        let vp = Viewport::new(80, 24);

        let cursor = Cursor::new(6); // Start of line 1 ("line2")
        let (x, y) = vp.cursor_screen_position(&mut buffer, &cursor);
        // x is column (horizontal), y is row (vertical)
        assert_eq!(x, 0); // Column 0 (start of line)
        assert_eq!(y, 1); // Row 1 (second line, since top_line is 0)
    }

    #[test]
    fn test_ensure_visible_cursor_above_viewport() {
        // Create buffer with many lines
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
        let mut vp = Viewport::new(80, 10); // 10 lines visible

        // Scroll down to show lines 10-19 (top_byte at line 10)
        // scroll_to uses 1-based line numbers, so line 10 = argument 10
        vp.scroll_to(&mut buffer, 10);
        let _old_top_byte = vp.top_byte;

        // Verify we scrolled to around line 10
        let top_line = buffer.get_line_number(vp.top_byte);
        assert!(
            top_line >= 9,
            "Should have scrolled down to at least line 10"
        );

        // Now move cursor to line 5 (above the viewport)
        let mut iter = buffer.line_iterator(0, 80);
        let mut line_5_byte = 0;
        for i in 0..5 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 4 {
                    line_5_byte = line_start;
                    break;
                }
            }
        }
        let cursor = Cursor::new(line_5_byte);

        // Before fix, this should fail because ensure_visible doesn't detect cursor is above viewport
        vp.ensure_visible(&mut buffer, &cursor);

        // Verify that viewport scrolled up to make cursor visible
        // The viewport should now be positioned so cursor (line 5) is visible
        let new_top_line = buffer.get_line_number(vp.top_byte);
        let cursor_line = buffer.get_line_number(line_5_byte);
        assert!(
            cursor_line >= new_top_line,
            "Cursor line should be at or below top of viewport"
        );
        assert!(
            new_top_line < top_line,
            "Viewport should have scrolled up from line {}",
            top_line
        );

        // Verify cursor is within visible area
        let lines_from_top = cursor_line.saturating_sub(new_top_line);
        assert!(
            lines_from_top < vp.visible_line_count(),
            "Cursor should be within visible area"
        );

        // Verify cursor is centered (or close to center)
        let expected_center = vp.visible_line_count() / 2;
        assert!(
            lines_from_top >= expected_center - 1 && lines_from_top <= expected_center + 1,
            "Cursor should be centered in viewport, expected around {}, got {}",
            expected_center,
            lines_from_top
        );
    }

    #[test]
    fn test_ensure_visible_cursor_below_viewport_centers() {
        // Create buffer with many lines
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
        let mut vp = Viewport::new(80, 10); // 10 lines visible

        // Start at top (line 1 visible)
        assert_eq!(vp.top_byte, 0);

        // Move cursor to line 15 (below viewport)
        let mut iter = buffer.line_iterator(0, 80);
        let mut line_15_byte = 0;
        for i in 0..15 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 14 {
                    line_15_byte = line_start;
                    break;
                }
            }
        }
        let cursor = Cursor::new(line_15_byte);

        vp.ensure_visible(&mut buffer, &cursor);

        // Verify cursor is centered
        let new_top_line = buffer.get_line_number(vp.top_byte);
        let cursor_line = buffer.get_line_number(line_15_byte);
        let lines_from_top = cursor_line.saturating_sub(new_top_line);

        let expected_center = vp.visible_line_count() / 2;
        assert!(
            lines_from_top >= expected_center - 1 && lines_from_top <= expected_center + 1,
            "Cursor should be centered in viewport when jumping down, expected around {}, got {}",
            expected_center,
            lines_from_top
        );
    }

    #[test]
    fn test_ensure_column_visible_resets_to_zero() {
        // Test that horizontal scroll is reset when cursor moves to column 0
        // This simulates what happens after pressing Enter on a long line
        let mut buffer = Buffer::from_str_test("a".repeat(100).as_str());
        let mut vp = Viewport::new(80, 24);
        vp.line_wrap_enabled = false;

        // First, scroll right by moving cursor to end of line
        let cursor_at_end = Cursor::new(100);
        vp.ensure_visible(&mut buffer, &cursor_at_end);

        println!("After moving to position 100:");
        println!("  left_column = {}", vp.left_column);

        // Verify we've scrolled right
        assert!(
            vp.left_column > 0,
            "Should have scrolled right, but left_column = {}",
            vp.left_column
        );

        // Now simulate pressing Enter: newline is added, cursor moves to start of new line
        // Add the newline to the buffer
        // Note: In real usage the buffer would be modified, but for this test we just
        // need to test ensure_column_visible with cursor at column 0

        // Test ensure_column_visible directly with column=0 and the current left_column
        // This simulates what should happen when cursor is at column 0 on a new line
        vp.ensure_column_visible(0, 0, &mut buffer); // column=0, line_length=0 (empty new line)

        println!("After ensure_column_visible(0, 0):");
        println!("  left_column = {}", vp.left_column);

        assert_eq!(
            vp.left_column, 0,
            "left_column should be reset to 0 when cursor is at column 0, but got {}",
            vp.left_column
        );
    }
}
