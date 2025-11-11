/// Line anchor system for efficient line number ↔ byte offset conversion
///
/// This module provides anchor-based line tracking that scales to huge files
/// by using estimated anchors and sparse networks of line markers.

use crate::buffer::Buffer;
use crate::marker::{MarkerId, MarkerList};
use crate::marker_tree::AnchorConfidence;

/// Manages line anchors for efficient line number ↔ byte offset conversion
pub struct LineAnchorManager {
    /// Estimated total lines in the file (from LSP or heuristic)
    estimated_total_lines: Option<usize>,
    /// File size in bytes
    file_size: usize,
    /// Average line length in bytes
    avg_line_length: usize,
    /// Large file threshold - files below this use exact line tracking
    large_file_threshold: usize,
}

impl LineAnchorManager {
    /// Create a new line anchor manager
    pub fn new(file_size: usize, large_file_threshold: usize) -> Self {
        // Default assumption: 100 bytes per line
        let avg_line_length = 100;
        let estimated_total_lines = if file_size > 0 {
            Some(file_size / avg_line_length)
        } else {
            None
        };

        Self {
            estimated_total_lines,
            file_size,
            avg_line_length,
            large_file_threshold,
        }
    }

    /// Update the file size (call after edits)
    pub fn update_file_size(&mut self, new_size: usize) {
        self.file_size = new_size;
        if self.estimated_total_lines.is_some() && self.avg_line_length > 0 {
            self.estimated_total_lines = Some(new_size / self.avg_line_length);
        }
    }

    /// Set the estimated total lines (from LSP or other source)
    pub fn set_estimated_total_lines(&mut self, lines: usize) {
        self.estimated_total_lines = Some(lines);
        if lines > 0 && self.file_size > 0 {
            self.avg_line_length = self.file_size / lines;
        }
    }

    /// Convert line number to byte offset
    ///
    /// Creates anchors as needed using estimation for huge jumps.
    /// Maximum scan distance: max(100 lines, 10KB)
    pub fn line_to_byte(&self, line_num: usize, buffer: &Buffer, markers: &mut MarkerList) -> usize {
        // Try to find an exact anchor at this line
        if let Some((_, start, _, _)) = self.find_anchor_at_line(line_num, markers) {
            return start;
        }

        // For small files (< threshold), always scan from the beginning to get exact positions
        // This avoids estimation errors on small files
        if self.file_size < self.large_file_threshold {
            // Check if we have an anchor at line 0
            if let Some((anchor_id, anchor_start, _, anchor_line)) = markers.nearest_line_anchor_before_line(line_num) {
                if anchor_line == 0 {
                    // Scan from line 0
                    return self.scan_forward_n_lines(
                        buffer,
                        markers,
                        anchor_id,
                        anchor_start,
                        anchor_line,
                        line_num,
                    );
                }
            }

            // No anchor at line 0, create one and scan from there
            let line0_end = self.scan_to_next_newline(buffer, 0);
            markers.create_line_anchor(0, line0_end, 0, crate::marker_tree::AnchorConfidence::Exact);
            let anchor_id = markers.nearest_line_anchor_before_line(line_num + 1).unwrap().0;
            return self.scan_forward_n_lines(
                buffer,
                markers,
                anchor_id,
                0,
                0,
                line_num,
            );
        }

        // Find nearest anchor before this line
        if let Some((anchor_id, anchor_start, _, anchor_line)) =
            markers.nearest_line_anchor_before_line(line_num)
        {
            let distance_lines = line_num.saturating_sub(anchor_line);

            // If distance is small, scan forward from anchor
            if distance_lines < 100 {
                return self.scan_forward_n_lines(
                    buffer,
                    markers,
                    anchor_id,
                    anchor_start,
                    anchor_line,
                    distance_lines,
                );
            }
        }

        // No nearby anchor - create estimated anchor at target line
        self.create_estimated_anchor_at_line(line_num, buffer, markers)
    }

    /// Convert byte offset to line number
    ///
    /// Creates anchors as needed using estimation for huge jumps.
    pub fn byte_to_line(&self, byte_offset: usize, buffer: &Buffer, markers: &mut MarkerList) -> usize {
        // Check if there's an anchor containing this byte
        let anchors = markers.query_line_anchors(byte_offset, byte_offset + 1);
        if let Some((_, _, _, line_num)) = anchors.first() {
            return *line_num;
        }

        // Find nearest anchor before this byte
        if let Some((anchor_id, anchor_start, _, anchor_line)) =
            markers.nearest_line_anchor_before(byte_offset)
        {
            let distance_bytes = byte_offset.saturating_sub(anchor_start);

            // If distance is small, scan forward
            if distance_bytes < 10000 {
                // ~100 lines at 100 bytes/line
                return self.scan_forward_to_byte(
                    buffer,
                    markers,
                    anchor_id,
                    anchor_start,
                    anchor_line,
                    byte_offset,
                );
            }
        }

        // No nearby anchor - create estimated anchor
        let estimated_line = byte_offset / self.avg_line_length;
        self.create_estimated_anchor_at_line(estimated_line, buffer, markers);
        estimated_line
    }

    /// Find an anchor at exactly this line number
    fn find_anchor_at_line(
        &self,
        line_num: usize,
        markers: &MarkerList,
    ) -> Option<(MarkerId, usize, usize, usize)> {
        // Query all anchors and find exact match
        // In practice, we won't have many anchors, so this is fast
        markers
            .query_line_anchors(0, usize::MAX)
            .into_iter()
            .find(|(_, _, _, estimated_line)| *estimated_line == line_num)
    }

    /// Create an estimated anchor at a target line
    ///
    /// Cost: O(avg_line_length) ≈ O(100 bytes)
    fn create_estimated_anchor_at_line(
        &self,
        target_line: usize,
        buffer: &Buffer,
        markers: &mut MarkerList,
    ) -> usize {
        // Estimate byte position
        let estimated_byte = target_line * self.avg_line_length;

        // If the estimated position is past the end of the file,
        // the requested line doesn't exist - return the buffer length
        if estimated_byte >= buffer.len() {
            return buffer.len();
        }

        // Find actual line boundary around estimated position
        let line_start = self.scan_to_prev_newline(buffer, estimated_byte);
        let line_end = self.scan_to_next_newline(buffer, estimated_byte);

        // Create anchor at this line boundary
        markers.create_line_anchor(line_start, line_end, target_line, AnchorConfidence::Estimated);

        line_start
    }

    /// Scan backward to find the previous newline (start of current line)
    fn scan_to_prev_newline(&self, buffer: &Buffer, from_byte: usize) -> usize {
        if from_byte == 0 {
            return 0;
        }

        // Scan backward up to avg_line_length bytes
        let search_start = from_byte.saturating_sub(self.avg_line_length);
        let slice = buffer.slice(search_start..from_byte);

        // Find last newline in slice
        if let Some(pos) = slice.rfind('\n') {
            search_start + pos + 1 // Position after the newline
        } else {
            // No newline found, this might be near start of file
            if search_start == 0 {
                0
            } else {
                // Keep scanning backward
                self.scan_to_prev_newline(buffer, search_start)
            }
        }
    }

    /// Scan forward to find the next newline (end of current line)
    fn scan_to_next_newline(&self, buffer: &Buffer, from_byte: usize) -> usize {
        // Scan forward up to avg_line_length bytes
        let search_end = (from_byte + self.avg_line_length).min(buffer.len());
        let slice = buffer.slice(from_byte..search_end);

        // Find first newline in slice
        if let Some(pos) = slice.find('\n') {
            from_byte + pos
        } else {
            // No newline found
            if search_end == buffer.len() {
                buffer.len()
            } else {
                // Keep scanning forward
                self.scan_to_next_newline(buffer, search_end)
            }
        }
    }

    /// Scan forward N lines from an anchor
    fn scan_forward_n_lines(
        &self,
        buffer: &Buffer,
        markers: &mut MarkerList,
        parent_anchor_id: MarkerId,
        start_byte: usize,
        start_line: usize,
        n_lines: usize,
    ) -> usize {
        let mut current_byte = start_byte;
        let mut current_line = start_line;

        for _ in 0..n_lines {
            // Find next newline
            let line_end = self.scan_to_next_newline(buffer, current_byte);
            if line_end >= buffer.len() {
                // Hit end of file before reaching target line
                // Return buffer.len() to signal line doesn't exist
                return buffer.len();
            }

            current_byte = line_end + 1; // Move past the newline
            current_line += 1;

            // Create relative anchor every ~10 lines for better distribution
            if current_line % 10 == 0 {
                let next_line_end = self.scan_to_next_newline(buffer, current_byte);
                markers.create_line_anchor(
                    current_byte,
                    next_line_end,
                    current_line,
                    AnchorConfidence::Relative(parent_anchor_id.0),
                );
            }
        }

        current_byte
    }

    /// Scan forward to a specific byte, counting lines
    fn scan_forward_to_byte(
        &self,
        buffer: &Buffer,
        markers: &mut MarkerList,
        parent_anchor_id: MarkerId,
        start_byte: usize,
        start_line: usize,
        target_byte: usize,
    ) -> usize {
        let mut current_byte = start_byte;
        let mut current_line = start_line;

        while current_byte < target_byte {
            // Find next newline
            let line_end = self.scan_to_next_newline(buffer, current_byte);
            if line_end >= target_byte {
                // Target is in this line
                break;
            }

            current_byte = line_end + 1;
            current_line += 1;

            // Create relative anchor periodically
            if current_line % 10 == 0 {
                let next_line_end = self.scan_to_next_newline(buffer, current_byte);
                markers.create_line_anchor(
                    current_byte,
                    next_line_end,
                    current_line,
                    AnchorConfidence::Relative(parent_anchor_id.0),
                );
            }
        }

        current_line
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_anchor_basic() {
        let buffer = Buffer::from_str_test("line1\nline2\nline3\n");
        let mut markers = MarkerList::new();
        let manager = LineAnchorManager::new(buffer.len(), crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Convert line 0 to byte
        let byte = manager.line_to_byte(0, &buffer, &mut markers);
        assert_eq!(byte, 0);

        // Convert line 1 to byte
        let byte = manager.line_to_byte(1, &buffer, &mut markers);
        assert_eq!(byte, 6); // After "line1\n"

        // Convert byte back to line
        let line = manager.byte_to_line(6, &buffer, &mut markers);
        assert_eq!(line, 1);
    }

    #[test]
    fn test_estimated_anchor_creation() {
        // Create a large buffer
        let text = "x".repeat(100) + "\n";
        let text = text.repeat(1000); // 1000 lines
        let buffer = Buffer::from_str_test(&text);
        let mut markers = MarkerList::new();
        let manager = LineAnchorManager::new(buffer.len(), crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Jump to line 500 (no nearby anchors)
        let byte = manager.line_to_byte(500, &buffer, &mut markers);

        // Should have created an anchor
        let anchors = markers.query_line_anchors(0, buffer.len());
        assert!(!anchors.is_empty(), "Should have created at least one anchor");

        // The byte position should be approximately correct
        // (within a few lines of the actual position)
        let expected_byte = 500 * 101; // 100 chars + 1 newline per line
        let diff = (byte as i64 - expected_byte as i64).abs();
        assert!(
            diff < 1000,
            "Estimated position should be close to actual (diff: {})",
            diff
        );
    }

    #[test]
    fn test_relative_anchor_creation() {
        let text = "line\n".repeat(200);
        let buffer = Buffer::from_str_test(&text);
        let mut markers = MarkerList::new();
        let manager = LineAnchorManager::new(buffer.len(), crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Create anchor at line 0
        let _ = manager.line_to_byte(0, &buffer, &mut markers);

        // Scan to line 50 (should create relative anchors along the way)
        let _ = manager.line_to_byte(50, &buffer, &mut markers);

        // Should have created multiple anchors (every 10 lines)
        let anchors = markers.query_line_anchors(0, buffer.len());
        assert!(
            anchors.len() >= 5,
            "Should have created multiple anchors along the path"
        );
    }

    #[test]
    fn test_small_threshold_with_large_file() {
        // Create a buffer larger than our threshold
        let text = "x".repeat(50) + "\n";
        let text = text.repeat(100); // 100 lines, ~5100 bytes
        let buffer = Buffer::from_str(&text, 1000); // Threshold of 1000 bytes (file is ~5100)
        let mut markers = MarkerList::new();

        // Use a threshold smaller than the buffer size
        let manager = LineAnchorManager::new(buffer.len(), 1000);

        // Jump to line 50 - should use estimation since file is larger than threshold
        let byte = manager.line_to_byte(50, &buffer, &mut markers);

        // Verify the byte position is approximately correct
        // The initial estimate uses avg_line_length of 100, but our lines are 51 bytes
        // So we expect the estimate to be off by roughly 50 lines * (100-51) = ~2450 bytes
        // After the first estimate creates an anchor, subsequent queries will be more accurate
        let expected_byte = 50 * 51; // 50 chars + 1 newline per line
        let diff = (byte as i64 - expected_byte as i64).abs();
        assert!(
            diff < 3000,
            "Estimated position should be within ~3000 bytes (diff: {})",
            diff
        );
    }
}
