use crate::buffer::Buffer;
use std::collections::BTreeMap;

/// Represents cached information about a single line
#[derive(Debug, Clone)]
pub struct LineInfo {
    /// 0-indexed line number
    pub line_number: usize,
    /// Byte offset where this line starts
    pub byte_offset: usize,
}

/// A cache that maintains line number to byte offset mappings.
/// This is incrementally built as we navigate through the buffer,
/// and invalidated when edits occur.
pub struct LineCache {
    /// Maps byte_offset -> LineInfo
    /// Using BTreeMap so we can efficiently find nearest entries
    pub(crate) entries: BTreeMap<usize, LineInfo>,

    /// The reference point: we know with certainty that byte 0 is line 0
    /// This could be extended to support multiple reference points
    reference_byte: usize,
    reference_line: usize,
}

impl LineCache {
    /// Create a new line cache with byte 0 as line 0
    pub fn new() -> Self {
        let mut entries = BTreeMap::new();
        entries.insert(0, LineInfo {
            line_number: 0,
            byte_offset: 0,
        });

        Self {
            entries,
            reference_byte: 0,
            reference_line: 0,
        }
    }

    /// Get line info for a specific byte offset if cached
    pub fn get(&self, byte_offset: usize) -> Option<&LineInfo> {
        self.entries.get(&byte_offset)
    }

    /// Get the nearest cached line at or before the given byte offset
    pub fn get_nearest_before(&self, byte_offset: usize) -> Option<&LineInfo> {
        self.entries.range(..=byte_offset).next_back().map(|(_, info)| info)
    }

    /// Ensure we have line information for the given byte offset.
    /// If not cached, this will iterate from the nearest known point to build the cache.
    /// Returns the line number for this byte offset.
    pub fn ensure_line_for_byte(&mut self, buffer: &Buffer, byte_offset: usize) -> usize {
        // Check if already cached
        if let Some(info) = self.entries.get(&byte_offset) {
            return info.line_number;
        }

        // Find nearest cached entry before this offset
        let (start_byte, start_line) = if let Some(info) = self.get_nearest_before(byte_offset) {
            (info.byte_offset, info.line_number)
        } else {
            // No cached entry before, start from reference point
            (self.reference_byte, self.reference_line)
        };

        // Iterate forward from start_byte to byte_offset, building cache
        let mut iter = buffer.line_iterator(start_byte);
        let mut current_line = start_line;

        // Cache the starting position if not already cached
        if !self.entries.contains_key(&start_byte) {
            self.entries.insert(start_byte, LineInfo {
                line_number: start_line,
                byte_offset: start_byte,
            });
        }

        while let Some((line_byte, _)) = iter.next() {
            if line_byte > byte_offset {
                break;
            }

            // Cache this line
            self.entries.insert(line_byte, LineInfo {
                line_number: current_line,
                byte_offset: line_byte,
            });

            if line_byte == byte_offset {
                return current_line;
            }

            current_line += 1;
        }

        // If we get here, byte_offset is beyond what we found
        // Return the last known line number
        current_line.saturating_sub(1)
    }

    /// Populate the cache with a range of lines starting from a byte offset.
    /// This is useful for pre-populating the viewport area.
    /// Returns the line number of the starting byte offset.
    pub fn populate_range(&mut self, buffer: &Buffer, start_byte: usize, line_count: usize) -> usize {
        let start_line = self.ensure_line_for_byte(buffer, start_byte);

        // Now iterate forward to populate more lines
        let mut iter = buffer.line_iterator(start_byte);
        let mut current_line = start_line;
        let mut lines_added = 0;

        while let Some((line_byte, _)) = iter.next() {
            if lines_added >= line_count {
                break;
            }

            // Cache this line if not already cached
            self.entries.entry(line_byte).or_insert_with(|| LineInfo {
                line_number: current_line,
                byte_offset: line_byte,
            });

            current_line += 1;
            lines_added += 1;
        }

        start_line
    }

    /// Invalidate all cache entries at or after a byte offset.
    /// This should be called when an edit occurs.
    pub fn invalidate_from(&mut self, byte_offset: usize) {
        // Remove all entries >= byte_offset
        let keys_to_remove: Vec<_> = self.entries
            .range(byte_offset..)
            .map(|(k, _)| *k)
            .collect();

        for key in keys_to_remove {
            self.entries.remove(&key);
        }
    }

    /// Handle an insertion at a byte offset.
    /// This shifts all byte offsets after the insertion point.
    pub fn handle_insertion(&mut self, insert_byte: usize, inserted_bytes: usize, inserted_newlines: usize) {
        // Collect entries that need to be updated (all entries after insert_byte)
        let entries_to_update: Vec<_> = self.entries
            .range(insert_byte..)
            .map(|(byte, info)| (*byte, info.clone()))
            .collect();

        // Remove old entries
        for (byte, _) in &entries_to_update {
            self.entries.remove(byte);
        }

        // Re-insert with updated offsets and line numbers
        for (old_byte, mut info) in entries_to_update {
            if old_byte > insert_byte {
                info.byte_offset += inserted_bytes;
                info.line_number += inserted_newlines;
            } else if old_byte == insert_byte {
                // The line at insert_byte itself doesn't move, but entries after it do
                // We'll invalidate and let it be recalculated
                continue;
            }
            self.entries.insert(info.byte_offset, info);
        }

        // Invalidate the line at insert_byte to force recalculation
        self.entries.remove(&insert_byte);
    }

    /// Handle a deletion at a byte offset range.
    /// This shifts all byte offsets after the deletion point.
    pub fn handle_deletion(&mut self, delete_start: usize, deleted_bytes: usize, deleted_newlines: usize) {
        let delete_end = delete_start + deleted_bytes;

        // Remove all entries in the deleted range
        let keys_in_range: Vec<_> = self.entries
            .range(delete_start..delete_end)
            .map(|(k, _)| *k)
            .collect();

        for key in keys_in_range {
            self.entries.remove(&key);
        }

        // Collect entries after the deletion that need to be updated
        let entries_to_update: Vec<_> = self.entries
            .range(delete_end..)
            .map(|(byte, info)| (*byte, info.clone()))
            .collect();

        // Remove old entries
        for (byte, _) in &entries_to_update {
            self.entries.remove(byte);
        }

        // Re-insert with updated offsets and line numbers
        for (_old_byte, mut info) in entries_to_update {
            info.byte_offset -= deleted_bytes;
            info.line_number -= deleted_newlines;
            self.entries.insert(info.byte_offset, info);
        }

        // Invalidate the line at delete_start to force recalculation
        self.entries.remove(&delete_start);
    }

    /// Clear all cached entries except the reference point
    pub fn clear(&mut self) {
        self.entries.clear();
        self.entries.insert(self.reference_byte, LineInfo {
            line_number: self.reference_line,
            byte_offset: self.reference_byte,
        });
    }

    /// Get the number of cached entries
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the cache is empty (only has reference point)
    pub fn is_empty(&self) -> bool {
        self.entries.len() <= 1
    }
}

impl Default for LineCache {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_cache_basic() {
        let buffer = Buffer::from_str("line1\nline2\nline3\n");
        let mut cache = LineCache::new();

        // Ensure line for byte 0
        let line = cache.ensure_line_for_byte(&buffer, 0);
        assert_eq!(line, 0);

        // Ensure line for byte 6 (start of "line2")
        let line = cache.ensure_line_for_byte(&buffer, 6);
        assert_eq!(line, 1);

        // Ensure line for byte 12 (start of "line3")
        let line = cache.ensure_line_for_byte(&buffer, 12);
        assert_eq!(line, 2);
    }

    #[test]
    fn test_line_cache_populate_range() {
        let buffer = Buffer::from_str("line1\nline2\nline3\nline4\n");
        let mut cache = LineCache::new();

        // Populate 3 lines starting from byte 0
        let start_line = cache.populate_range(&buffer, 0, 3);
        assert_eq!(start_line, 0);

        // Check that we have cached entries
        assert!(cache.get(0).is_some());
        assert!(cache.get(6).is_some());
        assert!(cache.get(12).is_some());
    }

    #[test]
    fn test_line_cache_invalidation() {
        let buffer = Buffer::from_str("line1\nline2\nline3\n");
        let mut cache = LineCache::new();

        // Populate cache
        cache.populate_range(&buffer, 0, 3);
        assert_eq!(cache.len(), 3);

        // Invalidate from byte 6
        cache.invalidate_from(6);

        // Should only have entry for byte 0 now
        assert!(cache.get(0).is_some());
        assert!(cache.get(6).is_none());
        assert!(cache.get(12).is_none());
    }
}
