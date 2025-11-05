use crate::buffer::{Buffer, LineNumber};
use crate::cursor::{Cursor, Cursors};
use crate::event::{Event, MarginContentData, MarginPositionData, OverlayFace as EventOverlayFace, PopupData, PopupPositionData};
use crate::highlighter::{Highlighter, Language};
use crate::margin::{MarginAnnotation, MarginContent, MarginManager, MarginPosition};
use crate::overlay::{Overlay, OverlayFace, OverlayManager, UnderlineStyle};
use crate::popup::{Popup, PopupContent, PopupListItem, PopupManager, PopupPosition};
use crate::viewport::Viewport;
use ratatui::style::{Color, Style};

/// The complete editor state - everything needed to represent the current editing session
pub struct EditorState {
    /// The text buffer
    pub buffer: Buffer,

    /// All cursors
    pub cursors: Cursors,

    /// The viewport
    pub viewport: Viewport,

    /// Syntax highlighter (optional - only created if language is detected)
    pub highlighter: Option<Highlighter>,

    /// Overlays for visual decorations (underlines, highlights, etc.)
    pub overlays: OverlayManager,

    /// Popups for floating windows (completion, documentation, etc.)
    pub popups: PopupManager,

    /// Margins for line numbers, annotations, gutter symbols, etc.
    pub margins: MarginManager,

    /// Cached line number for primary cursor (0-indexed)
    /// Maintained incrementally to avoid O(n) scanning on every render
    pub primary_cursor_line_number: LineNumber,

    /// Current mode (for modal editing, if implemented)
    pub mode: String,
}

impl EditorState {
    /// Create a new editor state with an empty buffer
    pub fn new(width: u16, height: u16) -> Self {
        // Account for tab bar (1 line) and status bar (1 line)
        let content_height = height.saturating_sub(2);
        tracing::info!(
            "EditorState::new: width={}, height={}, content_height={}",
            width,
            height,
            content_height
        );
        Self {
            buffer: Buffer::new(),
            cursors: Cursors::new(),
            viewport: Viewport::new(width, content_height),
            highlighter: None, // No file path, so no syntax highlighting
            overlays: OverlayManager::new(),
            popups: PopupManager::new(),
            margins: MarginManager::new(),
            primary_cursor_line_number: LineNumber::Absolute(0), // Start at line 0
            mode: "insert".to_string(),
        }
    }

    /// Create an editor state from a file
    pub fn from_file(path: &std::path::Path, width: u16, height: u16) -> std::io::Result<Self> {
        // Account for tab bar (1 line) and status bar (1 line)
        let content_height = height.saturating_sub(2);
        let buffer = Buffer::load_from_file(path)?;

        // Try to create a highlighter based on file extension
        let highlighter = Language::from_path(path).and_then(|lang| {
            Highlighter::new(lang)
                .map_err(|e| {
                    tracing::warn!("Failed to create highlighter: {}", e);
                    e
                })
                .ok()
        });

        Ok(Self {
            buffer,
            cursors: Cursors::new(),
            viewport: Viewport::new(width, content_height),
            highlighter,
            overlays: OverlayManager::new(),
            popups: PopupManager::new(),
            margins: MarginManager::new(),
            primary_cursor_line_number: LineNumber::Absolute(0), // Start at line 0
            mode: "insert".to_string(),
        })
    }

    /// Apply an event to the state - THE ONLY WAY TO MODIFY STATE
    /// This is the heart of the event-driven architecture
    pub fn apply(&mut self, event: &Event) {
        match event {
            Event::Insert {
                position,
                text,
                cursor_id,
            } => {
                // Count newlines in inserted text to update cursor line number
                let newlines_inserted = text.matches('\n').count();

                // Insert text into buffer
                self.buffer.insert(*position, text);

                // Invalidate highlight cache for edited range
                if let Some(highlighter) = &mut self.highlighter {
                    highlighter.invalidate_range(*position..*position + text.len());
                }

                // Adjust all cursors after the edit
                self.cursors.adjust_for_edit(*position, 0, text.len());

                // Move the cursor that made the edit to the end of the insertion
                if let Some(cursor) = self.cursors.get_mut(*cursor_id) {
                    cursor.position = position + text.len();
                    cursor.clear_selection();
                }

                // Update primary cursor line number if this was the primary cursor
                if *cursor_id == self.cursors.primary_id() {
                    self.primary_cursor_line_number = match self.primary_cursor_line_number {
                        LineNumber::Absolute(line) => {
                            LineNumber::Absolute(line + newlines_inserted)
                        }
                        LineNumber::Relative {
                            line,
                            from_cached_line,
                        } => LineNumber::Relative {
                            line: line + newlines_inserted,
                            from_cached_line,
                        },
                    };
                }

                // Smart scroll to keep cursor visible
                if let Some(cursor) = self.cursors.get(*cursor_id) {
                    self.viewport.ensure_visible(&mut self.buffer, cursor);
                }
            }

            Event::Delete {
                range,
                cursor_id,
                deleted_text,
            } => {
                let len = range.len();
                // Count newlines in deleted text to update cursor line number
                let newlines_deleted = deleted_text.matches('\n').count();

                // Delete from buffer
                self.buffer.delete(range.clone());

                // Invalidate highlight cache for edited range
                if let Some(highlighter) = &mut self.highlighter {
                    highlighter.invalidate_range(range.clone());
                }

                // Adjust all cursors after the edit
                self.cursors.adjust_for_edit(range.start, len, 0);

                // Move the cursor that made the edit to the start of deletion
                if let Some(cursor) = self.cursors.get_mut(*cursor_id) {
                    cursor.position = range.start;
                    cursor.clear_selection();
                }

                // Update primary cursor line number if this was the primary cursor
                if *cursor_id == self.cursors.primary_id() {
                    self.primary_cursor_line_number = match self.primary_cursor_line_number {
                        LineNumber::Absolute(line) => {
                            LineNumber::Absolute(line.saturating_sub(newlines_deleted))
                        }
                        LineNumber::Relative {
                            line,
                            from_cached_line,
                        } => LineNumber::Relative {
                            line: line.saturating_sub(newlines_deleted),
                            from_cached_line,
                        },
                    };
                }

                // Smart scroll to keep cursor visible
                if let Some(cursor) = self.cursors.get(*cursor_id) {
                    self.viewport.ensure_visible(&mut self.buffer, cursor);
                }
            }

            Event::MoveCursor {
                cursor_id,
                position,
                anchor,
            } => {
                if let Some(cursor) = self.cursors.get_mut(*cursor_id) {
                    cursor.position = *position;
                    cursor.anchor = *anchor;

                    // Smart scroll to keep cursor visible
                    self.viewport.ensure_visible(&mut self.buffer, cursor);
                }

                // Update primary cursor line number if this is the primary cursor
                // For MoveCursor events, we lose absolute line tracking and switch to Relative
                if *cursor_id == self.cursors.primary_id() {
                    self.primary_cursor_line_number = LineNumber::Relative {
                        line: 0,
                        from_cached_line: 0,
                    };
                }
            }

            Event::AddCursor {
                cursor_id,
                position,
                anchor,
            } => {
                let cursor = if let Some(anchor) = anchor {
                    Cursor::with_selection(*anchor, *position)
                } else {
                    Cursor::new(*position)
                };

                // Insert cursor with the specific ID from the event
                // This is important for undo/redo to work correctly
                self.cursors.insert_with_id(*cursor_id, cursor);

                self.cursors.normalize();
            }

            Event::RemoveCursor { cursor_id, .. } => {
                self.cursors.remove(*cursor_id);
            }

            Event::Scroll { line_offset } => {
                if *line_offset > 0 {
                    self.viewport
                        .scroll_down(&self.buffer, *line_offset as usize);
                } else {
                    self.viewport
                        .scroll_up(&self.buffer, line_offset.unsigned_abs());
                }
            }

            Event::SetViewport { top_line } => {
                self.viewport.scroll_to(&self.buffer, *top_line);
            }

            Event::ChangeMode { mode } => {
                self.mode = mode.clone();
            }

            Event::AddOverlay {
                overlay_id,
                range,
                face,
                priority,
                message,
            } => {
                // Convert event overlay face to overlay face
                let overlay_face = convert_event_face_to_overlay_face(face);
                let overlay = Overlay {
                    range: range.clone(),
                    face: overlay_face,
                    priority: *priority,
                    id: Some(overlay_id.clone()),
                    message: message.clone(),
                };
                self.overlays.add(overlay);
            }

            Event::RemoveOverlay { overlay_id } => {
                self.overlays.remove_by_id(overlay_id);
            }

            Event::RemoveOverlaysInRange { range } => {
                self.overlays.remove_in_range(range);
            }

            Event::ClearOverlays => {
                self.overlays = OverlayManager::new();
            }

            Event::ShowPopup { popup } => {
                let popup_obj = convert_popup_data_to_popup(popup);
                self.popups.show(popup_obj);
            }

            Event::HidePopup => {
                self.popups.hide();
            }

            Event::ClearPopups => {
                self.popups.clear();
            }

            Event::PopupSelectNext => {
                if let Some(popup) = self.popups.top_mut() {
                    popup.select_next();
                }
            }

            Event::PopupSelectPrev => {
                if let Some(popup) = self.popups.top_mut() {
                    popup.select_prev();
                }
            }

            Event::PopupPageDown => {
                if let Some(popup) = self.popups.top_mut() {
                    popup.page_down();
                }
            }

            Event::PopupPageUp => {
                if let Some(popup) = self.popups.top_mut() {
                    popup.page_up();
                }
            }

            Event::AddMarginAnnotation {
                line,
                position,
                content,
                annotation_id,
            } => {
                let margin_position = convert_margin_position(position);
                let margin_content = convert_margin_content(content);
                let annotation = if let Some(id) = annotation_id {
                    MarginAnnotation::with_id(*line, margin_position, margin_content, id.clone())
                } else {
                    MarginAnnotation::new(*line, margin_position, margin_content)
                };
                self.margins.add_annotation(annotation);
            }

            Event::RemoveMarginAnnotation { annotation_id } => {
                self.margins.remove_by_id(annotation_id);
            }

            Event::RemoveMarginAnnotationsAtLine { line, position } => {
                let margin_position = convert_margin_position(position);
                self.margins.remove_at_line(*line, margin_position);
            }

            Event::ClearMarginPosition { position } => {
                let margin_position = convert_margin_position(position);
                self.margins.clear_position(margin_position);
            }

            Event::ClearMargins => {
                self.margins.clear_all();
            }

            Event::SetLineNumbers { enabled } => {
                self.margins.set_line_numbers(*enabled);
            }

            // Split events are handled at the Editor level, not at EditorState level
            // These are no-ops here as they affect the split layout, not buffer state
            Event::SplitPane { .. }
            | Event::CloseSplit { .. }
            | Event::SetActiveSplit { .. }
            | Event::AdjustSplitRatio { .. }
            | Event::NextSplit
            | Event::PrevSplit => {
                // No-op: split events are handled by Editor, not EditorState
            }

            Event::Batch { events, .. } => {
                // Apply all events in the batch sequentially
                // This ensures multi-cursor operations are applied atomically
                for event in events {
                    self.apply(event);
                }
            }
        }
    }

    /// Apply multiple events in sequence
    pub fn apply_many(&mut self, events: &[Event]) {
        for event in events {
            self.apply(event);
        }
    }

    /// Get the primary cursor
    pub fn primary_cursor(&self) -> &Cursor {
        self.cursors.primary()
    }

    /// Get the primary cursor mutably (for reading state only, not for modification!)
    pub fn primary_cursor_mut(&mut self) -> &mut Cursor {
        self.cursors.primary_mut()
    }

    /// Get all cursor positions for rendering
    pub fn cursor_positions(&mut self) -> Vec<(u16, u16)> {
        let mut positions = Vec::new();
        for (_, cursor) in self.cursors.iter() {
            let pos = self
                .viewport
                .cursor_screen_position(&mut self.buffer, cursor);
            positions.push(pos);
        }
        positions
    }

    /// Resize the viewport
    pub fn resize(&mut self, width: u16, height: u16) {
        // Account for tab bar (1 line) and status bar (1 line)
        let content_height = height.saturating_sub(2);
        self.viewport.resize(width, content_height);

        // Ensure primary cursor is still visible after resize
        let primary = *self.cursors.primary();
        self.viewport.ensure_visible(&mut self.buffer, &primary);
    }
}

/// Convert event overlay face to the actual overlay face
fn convert_event_face_to_overlay_face(event_face: &EventOverlayFace) -> OverlayFace {
    match event_face {
        EventOverlayFace::Underline { color, style } => {
            let underline_style = match style {
                crate::event::UnderlineStyle::Straight => UnderlineStyle::Straight,
                crate::event::UnderlineStyle::Wavy => UnderlineStyle::Wavy,
                crate::event::UnderlineStyle::Dotted => UnderlineStyle::Dotted,
                crate::event::UnderlineStyle::Dashed => UnderlineStyle::Dashed,
            };
            OverlayFace::Underline {
                color: Color::Rgb(color.0, color.1, color.2),
                style: underline_style,
            }
        }
        EventOverlayFace::Background { color } => OverlayFace::Background {
            color: Color::Rgb(color.0, color.1, color.2),
        },
        EventOverlayFace::Foreground { color } => OverlayFace::Foreground {
            color: Color::Rgb(color.0, color.1, color.2),
        },
    }
}

/// Convert popup data to the actual popup object
fn convert_popup_data_to_popup(data: &PopupData) -> Popup {
    let content = match &data.content {
        crate::event::PopupContentData::Text(lines) => PopupContent::Text(lines.clone()),
        crate::event::PopupContentData::List { items, selected } => PopupContent::List {
            items: items
                .iter()
                .map(|item| PopupListItem {
                    text: item.text.clone(),
                    detail: item.detail.clone(),
                    icon: item.icon.clone(),
                    data: item.data.clone(),
                })
                .collect(),
            selected: *selected,
        },
    };

    let position = match data.position {
        PopupPositionData::AtCursor => PopupPosition::AtCursor,
        PopupPositionData::BelowCursor => PopupPosition::BelowCursor,
        PopupPositionData::AboveCursor => PopupPosition::AboveCursor,
        PopupPositionData::Fixed { x, y } => PopupPosition::Fixed { x, y },
        PopupPositionData::Centered => PopupPosition::Centered,
    };

    let popup = Popup {
        title: data.title.clone(),
        content,
        position,
        width: data.width,
        max_height: data.max_height,
        bordered: data.bordered,
        border_style: Style::default().fg(Color::Gray),
        background_style: Style::default().bg(Color::Rgb(30, 30, 30)),
        scroll_offset: 0,
    };

    popup
}

/// Convert margin position data to the actual margin position
fn convert_margin_position(position: &MarginPositionData) -> MarginPosition {
    match position {
        MarginPositionData::Left => MarginPosition::Left,
        MarginPositionData::Right => MarginPosition::Right,
    }
}

/// Convert margin content data to the actual margin content
fn convert_margin_content(content: &MarginContentData) -> MarginContent {
    match content {
        MarginContentData::Text(text) => MarginContent::Text(text.clone()),
        MarginContentData::Symbol { text, color } => {
            if let Some((r, g, b)) = color {
                MarginContent::colored_symbol(text.clone(), Color::Rgb(*r, *g, *b))
            } else {
                MarginContent::symbol(text.clone(), Style::default())
            }
        }
        MarginContentData::Empty => MarginContent::Empty,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event::CursorId;

    #[test]
    fn test_state_new() {
        let state = EditorState::new(80, 24);
        assert!(state.buffer.is_empty());
        assert_eq!(state.cursors.count(), 1);
        assert_eq!(state.cursors.primary().position, 0);
    }

    #[test]
    fn test_apply_insert() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = state.cursors.primary_id();

        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id,
        });

        assert_eq!(state.buffer.to_string(), "hello");
        assert_eq!(state.cursors.primary().position, 5);
        assert!(state.buffer.is_modified());
    }

    #[test]
    fn test_apply_delete() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = state.cursors.primary_id();

        // Insert then delete
        state.apply(&Event::Insert {
            position: 0,
            text: "hello world".to_string(),
            cursor_id,
        });

        state.apply(&Event::Delete {
            range: 5..11,
            deleted_text: " world".to_string(),
            cursor_id,
        });

        assert_eq!(state.buffer.to_string(), "hello");
        assert_eq!(state.cursors.primary().position, 5);
    }

    #[test]
    fn test_apply_move_cursor() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = state.cursors.primary_id();

        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id,
        });

        state.apply(&Event::MoveCursor {
            cursor_id,
            position: 2,
            anchor: None,
        });

        assert_eq!(state.cursors.primary().position, 2);
    }

    #[test]
    fn test_apply_add_cursor() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = CursorId(1);

        state.apply(&Event::AddCursor {
            cursor_id,
            position: 5,
            anchor: None,
        });

        assert_eq!(state.cursors.count(), 2);
    }

    #[test]
    fn test_apply_many() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = state.cursors.primary_id();

        let events = vec![
            Event::Insert {
                position: 0,
                text: "hello ".to_string(),
                cursor_id,
            },
            Event::Insert {
                position: 6,
                text: "world".to_string(),
                cursor_id,
            },
        ];

        state.apply_many(&events);

        assert_eq!(state.buffer.to_string(), "hello world");
    }

    #[test]
    fn test_cursor_adjustment_after_insert() {
        let mut state = EditorState::new(80, 24);
        let cursor_id = state.cursors.primary_id();

        // Add a second cursor at position 5
        state.apply(&Event::AddCursor {
            cursor_id: CursorId(1),
            position: 5,
            anchor: None,
        });

        // Insert at position 0 - should push second cursor forward
        state.apply(&Event::Insert {
            position: 0,
            text: "abc".to_string(),
            cursor_id,
        });

        // Second cursor should be at position 5 + 3 = 8
        if let Some(cursor) = state.cursors.get(CursorId(1)) {
            assert_eq!(cursor.position, 8);
        }
    }
}
