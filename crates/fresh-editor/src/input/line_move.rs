//! Line/selection move helpers for editor actions.

use crate::model::buffer::Buffer;
use crate::model::cursor::Cursor;
use crate::model::event::{CursorId, Event};
use crate::state::EditorState;
use std::ops::Range;

#[derive(Debug, Clone, Copy)]
pub(crate) enum LineMoveDirection {
    Up,
    Down,
}

#[derive(Debug, Clone, Copy)]
struct LineByteRange {
    start: usize,
    end: usize,
}

fn line_bounds_at(
    buffer: &mut Buffer,
    pos: usize,
    estimated_line_length: usize,
) -> Option<LineByteRange> {
    let mut iter = buffer.line_iterator(pos, estimated_line_length);
    let line_start = iter.current_position();
    iter.next_line().map(|(_start, content)| LineByteRange {
        start: line_start,
        end: line_start + content.len(),
    })
}

fn prev_line_bounds(
    buffer: &mut Buffer,
    pos: usize,
    estimated_line_length: usize,
) -> Option<LineByteRange> {
    let mut iter = buffer.line_iterator(pos, estimated_line_length);
    iter.prev().map(|(start, content)| LineByteRange {
        start,
        end: start + content.len(),
    })
}

fn next_line_bounds(
    buffer: &mut Buffer,
    pos: usize,
    estimated_line_length: usize,
) -> Option<LineByteRange> {
    let mut iter = buffer.line_iterator(pos, estimated_line_length);
    if iter.current_position() < pos {
        let _ = iter.next_line();
    }
    iter.next_line().map(|(start, content)| LineByteRange {
        start,
        end: start + content.len(),
    })
}

/// Determine the byte range covering full lines for a cursor selection or position.
fn selection_line_range(
    buffer: &mut Buffer,
    cursor: &Cursor,
    estimated_line_length: usize,
) -> Option<LineByteRange> {
    let buffer_len = buffer.len();
    let (start_pos, end_pos) = if let Some(range) = cursor.selection_range() {
        if range.start == range.end {
            (range.start.min(buffer_len), range.start.min(buffer_len))
        } else {
            let start = range.start.min(buffer_len);
            let end = range.end.min(buffer_len);
            let end_for_line = if start < end {
                end.saturating_sub(1)
            } else {
                start
            };
            (start, end_for_line)
        }
    } else {
        let pos = cursor.position.min(buffer_len);
        (pos, pos)
    };

    let start_line = line_bounds_at(buffer, start_pos, estimated_line_length)?;
    let end_line = line_bounds_at(buffer, end_pos, estimated_line_length)?;
    Some(LineByteRange {
        start: start_line.start,
        end: end_line.end,
    })
}

fn merge_line_ranges(mut ranges: Vec<LineByteRange>) -> Vec<LineByteRange> {
    if ranges.is_empty() {
        return ranges;
    }

    ranges.sort_by_key(|range| range.start);
    let mut merged = Vec::with_capacity(ranges.len());
    let mut current = ranges[0];

    for range in ranges.into_iter().skip(1) {
        if range.start <= current.end {
            current.end = current.end.max(range.end);
        } else {
            merged.push(current);
            current = range;
        }
    }

    merged.push(current);
    merged
}

#[derive(Debug, Clone, Copy)]
struct MoveRegion {
    start: usize,
    end: usize,
    block_len: usize,
    adjacent_len: usize,
    direction: LineMoveDirection,
}

impl MoveRegion {
    fn block_start(&self) -> usize {
        match self.direction {
            LineMoveDirection::Up => self.start + self.adjacent_len,
            LineMoveDirection::Down => self.start,
        }
    }

    fn block_end(&self) -> usize {
        self.block_start().saturating_add(self.block_len)
    }
}

#[derive(Debug, Clone)]
struct LinePiece {
    start: usize,
    end: usize,
    content: String,
}

#[derive(Debug, Clone)]
struct LineMapping {
    old_start: usize,
    old_end: usize,
    old_has_newline: bool,
    new_start: usize,
    new_len: usize,
    new_has_newline: bool,
}

#[derive(Debug, Clone)]
struct AppliedRegion {
    region: MoveRegion,
    mappings: Vec<LineMapping>,
}

fn strip_line_ending(line: &str) -> &str {
    if line.ends_with("\r\n") {
        &line[..line.len().saturating_sub(2)]
    } else if line.ends_with('\n') || line.ends_with('\r') {
        &line[..line.len().saturating_sub(1)]
    } else {
        line
    }
}

fn map_position_in_region(
    pos: usize,
    selection_range: Option<&Range<usize>>,
    region: &AppliedRegion,
) -> Option<usize> {
    let region_bounds = &region.region;
    if pos < region_bounds.start || pos > region_bounds.end {
        return None;
    }

    let block_end = region_bounds.block_end();
    let treat_block_end_as_block = selection_range
        .filter(|range| range.start < range.end)
        .is_some_and(|range| range.end == block_end);

    if pos == block_end && treat_block_end_as_block {
        if let Some(mapping) = region
            .mappings
            .iter()
            .find(|mapping| mapping.old_end == block_end)
        {
            return Some(mapping.new_start + mapping.new_len);
        }
    }

    if let Some(mapping) = region.mappings.iter().find(|mapping| {
        pos >= mapping.old_start
            && (pos < mapping.old_end || (!mapping.old_has_newline && pos == mapping.old_end))
    }) {
        let column = pos.saturating_sub(mapping.old_start);
        let max_offset = if mapping.new_has_newline {
            mapping.new_len.saturating_sub(1)
        } else {
            mapping.new_len
        };
        let clamped = column.min(max_offset);
        return Some(mapping.new_start + clamped);
    }

    None
}

pub(crate) fn move_lines(
    state: &mut EditorState,
    events: &mut Vec<Event>,
    direction: LineMoveDirection,
    estimated_line_length: usize,
) {
    let buffer_len = state.buffer.len();
    if buffer_len == 0 {
        return;
    }

    let cursor_snapshots: Vec<(CursorId, Option<Range<usize>>, usize, Option<usize>, usize)> =
        state
            .cursors
            .iter()
            .map(|(cursor_id, cursor)| {
                (
                    cursor_id,
                    cursor.selection_range().map(|range| range.clone()),
                    cursor.position,
                    cursor.anchor,
                    cursor.sticky_column,
                )
            })
            .collect();

    let ranges: Vec<LineByteRange> = {
        let buffer = &mut state.buffer;
        cursor_snapshots
            .iter()
            .filter_map(|(_, selection, position, _, _)| {
                let cursor = if let Some(range) = selection {
                    Cursor::with_selection(range.start, range.end)
                } else {
                    Cursor::new(*position)
                };
                selection_line_range(buffer, &cursor, estimated_line_length)
            })
            .collect()
    };
    let merged_ranges = merge_line_ranges(ranges);
    if merged_ranges.is_empty() {
        return;
    }

    let mut move_regions = Vec::new();
    {
        let buffer = &mut state.buffer;
        for range in merged_ranges {
            let block_len = range.end.saturating_sub(range.start);
            if block_len == 0 {
                continue;
            }

            match direction {
                LineMoveDirection::Up => {
                    if let Some(prev) = prev_line_bounds(buffer, range.start, estimated_line_length)
                    {
                        let adjacent_len = prev.end.saturating_sub(prev.start);
                        if adjacent_len == 0 && prev.start == buffer_len {
                            continue;
                        }
                        move_regions.push(MoveRegion {
                            start: prev.start,
                            end: range.end,
                            block_len,
                            adjacent_len,
                            direction,
                        });
                    }
                }
                LineMoveDirection::Down => {
                    if let Some(next) = next_line_bounds(buffer, range.end, estimated_line_length) {
                        let adjacent_len = next.end.saturating_sub(next.start);
                        if adjacent_len == 0 && next.start == buffer_len {
                            continue;
                        }
                        move_regions.push(MoveRegion {
                            start: range.start,
                            end: next.end,
                            block_len,
                            adjacent_len,
                            direction,
                        });
                    }
                }
            }
        }
    }

    if move_regions.is_empty() {
        return;
    }

    move_regions.sort_by_key(|region| region.start);

    let primary_cursor_id = state.cursors.primary_id();
    let has_trailing_newline = {
        let mut iter = state
            .buffer
            .line_iterator(buffer_len, estimated_line_length);
        matches!(
            iter.next_line(),
            Some((start, content)) if start == buffer_len && content.is_empty()
        )
    };

    let mut applied_regions = Vec::new();
    let line_ending = state.buffer.line_ending().as_str();
    let line_ending_len = line_ending.len();
    for region in &move_regions {
        let block_start = match region.direction {
            LineMoveDirection::Up => region.start + region.adjacent_len,
            LineMoveDirection::Down => region.start,
        };
        let block_end = block_start.saturating_add(region.block_len);
        let region_includes_last = region.end == buffer_len;

        let old_text = state.get_text_range(region.start, region.end);
        if old_text.is_empty() && region.start != region.end {
            continue;
        }

        let mut iter = state
            .buffer
            .line_iterator(region.start, estimated_line_length);
        let mut lines = Vec::new();
        while let Some((line_start, line)) = iter.next_line() {
            let line_end = line_start.saturating_add(line.len());
            lines.push(LinePiece {
                start: line_start,
                end: line_end,
                content: strip_line_ending(&line).to_string(),
            });
            if line_end >= region.end {
                break;
            }
        }

        if lines.is_empty() {
            continue;
        }

        let mut adjacent = None;
        let mut block_lines = Vec::new();
        for line in lines {
            match region.direction {
                LineMoveDirection::Up => {
                    if line.end == block_start {
                        adjacent = Some(line);
                    } else {
                        block_lines.push(line);
                    }
                }
                LineMoveDirection::Down => {
                    if line.start == block_end {
                        adjacent = Some(line);
                    } else {
                        block_lines.push(line);
                    }
                }
            }
        }

        let adjacent = if let Some(line) = adjacent {
            line
        } else {
            match region.direction {
                LineMoveDirection::Up => {
                    if block_lines.is_empty() {
                        continue;
                    }
                    block_lines.remove(0)
                }
                LineMoveDirection::Down => {
                    if block_lines.is_empty() {
                        continue;
                    }
                    block_lines.pop().unwrap()
                }
            }
        };

        let mut new_text = String::new();
        let mut ordered = Vec::new();
        match region.direction {
            LineMoveDirection::Up => {
                ordered.extend(block_lines.iter());
                ordered.push(&adjacent);
            }
            LineMoveDirection::Down => {
                ordered.push(&adjacent);
                ordered.extend(block_lines.iter());
            }
        }

        let mut mappings = Vec::with_capacity(ordered.len());
        let mut offset = 0usize;
        for (idx, line) in ordered.iter().enumerate() {
            let new_start = region.start.saturating_add(offset);
            new_text.push_str(&line.content);
            let mut append_line_ending = if region_includes_last {
                if has_trailing_newline {
                    true
                } else {
                    idx + 1 < ordered.len()
                }
            } else {
                true
            };

            if idx + 1 == ordered.len()
                && has_trailing_newline
                && line.start == buffer_len
                && line.content.is_empty()
            {
                append_line_ending = false;
            }

            let new_len = line.content.len()
                + if append_line_ending {
                    line_ending_len
                } else {
                    0
                };
            let old_len = line.end.saturating_sub(line.start);
            let old_has_newline = old_len > line.content.len();
            mappings.push(LineMapping {
                old_start: line.start,
                old_end: line.end,
                old_has_newline,
                new_start,
                new_len,
                new_has_newline: append_line_ending,
            });

            if append_line_ending {
                new_text.push_str(line_ending);
            }

            offset = offset.saturating_add(new_len);
        }

        applied_regions.push(AppliedRegion {
            region: *region,
            mappings,
        });

        if new_text == old_text {
            continue;
        }

        events.push(Event::Delete {
            range: region.start..region.end,
            deleted_text: old_text,
            cursor_id: primary_cursor_id,
        });
        events.push(Event::Insert {
            position: region.start,
            text: new_text,
            cursor_id: primary_cursor_id,
        });
    }

    if applied_regions.is_empty() {
        return;
    }

    for (cursor_id, selection, position, anchor, sticky_column) in cursor_snapshots {
        let new_position = applied_regions
            .iter()
            .find_map(|region| map_position_in_region(position, selection.as_ref(), region))
            .unwrap_or(position);
        let new_anchor = anchor.map(|anchor_pos| {
            applied_regions
                .iter()
                .find_map(|region| map_position_in_region(anchor_pos, selection.as_ref(), region))
                .unwrap_or(anchor_pos)
        });

        if new_position != position || new_anchor != anchor {
            events.push(Event::MoveCursor {
                cursor_id,
                old_position: position,
                new_position,
                old_anchor: anchor,
                new_anchor,
                old_sticky_column: sticky_column,
                new_sticky_column: sticky_column,
            });
        }
    }
}
