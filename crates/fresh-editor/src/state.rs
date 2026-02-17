use crate::model::buffer::{Buffer, LineNumber};
use crate::model::cursor::{Cursor, Cursors};
use crate::model::document_model::{
    DocumentCapabilities, DocumentModel, DocumentPosition, ViewportContent, ViewportLine,
};
use crate::model::event::{
    Event, MarginContentData, MarginPositionData, OverlayFace as EventOverlayFace, PopupData,
    PopupPositionData,
};
use crate::model::filesystem::FileSystem;
use crate::model::marker::MarkerList;
use crate::primitives::grammar::GrammarRegistry;
use crate::primitives::highlight_engine::HighlightEngine;
use crate::primitives::highlighter::Language;
use crate::primitives::indent::IndentCalculator;
use crate::primitives::reference_highlighter::ReferenceHighlighter;
use crate::primitives::text_property::TextPropertyManager;
use crate::view::bracket_highlight_overlay::BracketHighlightOverlay;
use crate::view::conceal::ConcealManager;
use crate::view::margin::{MarginAnnotation, MarginContent, MarginManager, MarginPosition};
use crate::view::overlay::{Overlay, OverlayFace, OverlayManager, UnderlineStyle};
use crate::view::popup::{
    Popup, PopupContent, PopupKind, PopupListItem, PopupManager, PopupPosition,
};
use crate::view::reference_highlight_overlay::ReferenceHighlightOverlay;
use crate::view::soft_break::SoftBreakManager;
use crate::view::virtual_text::VirtualTextManager;
use anyhow::Result;
use ratatui::style::{Color, Style};
use std::cell::RefCell;
use std::ops::Range;
use std::sync::Arc;

/// Display mode for a buffer
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ViewMode {
    /// Plain source rendering
    Source,
    /// Semi-WYSIWYG compose rendering
    Compose,
}

/// Per-buffer user settings that should be preserved across file reloads (auto-revert).
///
/// These are user overrides that apply to a specific buffer, separate from:
/// - File-derived state (syntax highlighting, language detection)
/// - View-specific state (scroll position, line wrap - those live in SplitViewState)
///
/// TODO: Consider moving view-related settings (line numbers, debug mode) to SplitViewState
/// to allow per-split preferences. Currently line numbers is in margins (coupled with plugin
/// gutters), and debug_highlight_mode is in EditorState, but both could arguably be per-view
/// rather than per-buffer.
#[derive(Debug, Clone)]
pub struct BufferSettings {
    /// Whether to show whitespace tab indicators (→) for this buffer
    /// Set based on language config; can be toggled per-buffer by user
    pub show_whitespace_tabs: bool,

    /// Whether pressing Tab should insert a tab character instead of spaces.
    /// Set based on language config; can be toggled per-buffer by user
    pub use_tabs: bool,

    /// Tab size (number of spaces per tab character) for rendering.
    /// Used for visual display of tab characters and indent calculations.
    /// Set based on language config; can be changed per-buffer by user
    pub tab_size: usize,
}

impl Default for BufferSettings {
    fn default() -> Self {
        Self {
            show_whitespace_tabs: true,
            use_tabs: false,
            tab_size: 4,
        }
    }
}

/// The complete editor state - everything needed to represent the current editing session
///
/// NOTE: Viewport is NOT stored here - it lives in SplitViewState.
/// This is because viewport is view-specific (each split can view the same buffer
/// at different scroll positions), while EditorState represents the buffer content.
pub struct EditorState {
    /// The text buffer
    pub buffer: Buffer,

    /// Syntax highlighter (tree-sitter or TextMate based on language)
    pub highlighter: HighlightEngine,

    /// Auto-indent calculator for smart indentation (RefCell for interior mutability)
    pub indent_calculator: RefCell<IndentCalculator>,

    /// Overlays for visual decorations (underlines, highlights, etc.)
    pub overlays: OverlayManager,

    /// Marker list for content-anchored overlay positions
    pub marker_list: MarkerList,

    /// Virtual text manager for inline hints (type hints, parameter hints, etc.)
    pub virtual_texts: VirtualTextManager,

    /// Conceal ranges for hiding/replacing byte ranges during rendering
    pub conceals: ConcealManager,

    /// Soft break points for marker-based line wrapping during rendering
    pub soft_breaks: SoftBreakManager,

    /// Popups for floating windows (completion, documentation, etc.)
    pub popups: PopupManager,

    /// Margins for line numbers, annotations, gutter symbols, etc.)
    pub margins: MarginManager,

    /// Cached line number for primary cursor (0-indexed)
    /// Maintained incrementally to avoid O(n) scanning on every render
    pub primary_cursor_line_number: LineNumber,

    /// Current mode (for modal editing, if implemented)
    pub mode: String,

    /// Text properties for virtual buffers (embedded metadata in text ranges)
    /// Used by virtual buffers to store location info, severity, etc.
    pub text_properties: TextPropertyManager,

    /// Whether to show cursors in this buffer (default true)
    /// Can be set to false for virtual buffers like diagnostics panels
    pub show_cursors: bool,

    /// Whether editing is disabled for this buffer (default false)
    /// When true, typing, deletion, cut/paste, undo/redo are blocked
    /// but navigation, selection, and copy are still allowed
    pub editing_disabled: bool,

    /// Per-buffer user settings (tab size, indentation style, etc.)
    /// These settings are preserved across file reloads (auto-revert)
    pub buffer_settings: BufferSettings,

    /// Semantic highlighter for word occurrence highlighting
    pub reference_highlighter: ReferenceHighlighter,

    /// Whether this buffer is a composite view (e.g., side-by-side diff)
    pub is_composite_buffer: bool,

    /// Debug mode: reveal highlight/overlay spans (WordPerfect-style)
    pub debug_highlight_mode: bool,

    /// Debounced semantic highlight cache
    pub reference_highlight_overlay: ReferenceHighlightOverlay,

    /// Bracket matching highlight overlay
    pub bracket_highlight_overlay: BracketHighlightOverlay,

    /// Cached LSP semantic tokens (converted to buffer byte ranges)
    pub semantic_tokens: Option<SemanticTokenStore>,

    /// The detected language for this buffer (e.g., "rust", "python", "text")
    pub language: String,
}

impl EditorState {
    /// Create a new editor state with an empty buffer
    ///
    /// Note: width/height parameters are kept for backward compatibility but
    /// are no longer used - viewport is now owned by SplitViewState.
    pub fn new(
        _width: u16,
        _height: u16,
        large_file_threshold: usize,
        fs: Arc<dyn FileSystem + Send + Sync>,
    ) -> Self {
        Self {
            buffer: Buffer::new(large_file_threshold, fs),
            highlighter: HighlightEngine::None, // No file path, so no syntax highlighting
            indent_calculator: RefCell::new(IndentCalculator::new()),
            overlays: OverlayManager::new(),
            marker_list: MarkerList::new(),
            virtual_texts: VirtualTextManager::new(),
            conceals: ConcealManager::new(),
            soft_breaks: SoftBreakManager::new(),
            popups: PopupManager::new(),
            margins: MarginManager::new(),
            primary_cursor_line_number: LineNumber::Absolute(0), // Start at line 0
            mode: "insert".to_string(),
            text_properties: TextPropertyManager::new(),
            show_cursors: true,
            editing_disabled: false,
            buffer_settings: BufferSettings::default(),
            reference_highlighter: ReferenceHighlighter::new(),
            is_composite_buffer: false,
            debug_highlight_mode: false,
            reference_highlight_overlay: ReferenceHighlightOverlay::new(),
            bracket_highlight_overlay: BracketHighlightOverlay::new(),
            semantic_tokens: None,
            language: "text".to_string(), // Default to plain text
        }
    }

    /// Set the syntax highlighting language based on a filename or extension
    /// This allows virtual buffers to get highlighting even without a real file path
    pub fn set_language_from_name(&mut self, name: &str, registry: &GrammarRegistry) {
        // Handle virtual buffer names like "*OLD:test.ts*" or "*OURS*.c"
        // 1. Strip surrounding * characters
        // 2. Extract filename after any prefix like "OLD:" or "NEW:"
        let cleaned_name = name.trim_matches('*');
        let filename = if let Some(pos) = cleaned_name.rfind(':') {
            // Extract part after the last colon (e.g., "OLD:test.ts" -> "test.ts")
            &cleaned_name[pos + 1..]
        } else {
            cleaned_name
        };

        let path = std::path::Path::new(filename);
        self.highlighter = HighlightEngine::for_file(path, registry);
        if let Some(language) = Language::from_path(path) {
            self.reference_highlighter.set_language(&language);
            self.language = language.to_string();
        } else {
            self.language = "text".to_string();
        }
        tracing::debug!(
            "Set highlighter for virtual buffer based on name: {} -> {} (backend: {}, language: {})",
            name,
            filename,
            self.highlighter.backend_name(),
            self.language
        );
    }

    /// Create an editor state from a file
    ///
    /// Note: width/height parameters are kept for backward compatibility but
    /// are no longer used - viewport is now owned by SplitViewState.
    pub fn from_file(
        path: &std::path::Path,
        _width: u16,
        _height: u16,
        large_file_threshold: usize,
        registry: &GrammarRegistry,
        fs: Arc<dyn FileSystem + Send + Sync>,
    ) -> anyhow::Result<Self> {
        let buffer = Buffer::load_from_file(path, large_file_threshold, fs)?;

        let highlighter = HighlightEngine::for_file(path, registry);
        let language = Language::from_path(path);
        let mut reference_highlighter = ReferenceHighlighter::new();
        let language_name = if let Some(lang) = &language {
            reference_highlighter.set_language(lang);
            lang.to_string()
        } else {
            "text".to_string()
        };

        let mut marker_list = MarkerList::new();
        if !buffer.is_empty() {
            marker_list.adjust_for_insert(0, buffer.len());
        }

        Ok(Self {
            buffer,
            highlighter,
            indent_calculator: RefCell::new(IndentCalculator::new()),
            overlays: OverlayManager::new(),
            marker_list,
            virtual_texts: VirtualTextManager::new(),
            conceals: ConcealManager::new(),
            soft_breaks: SoftBreakManager::new(),
            popups: PopupManager::new(),
            margins: MarginManager::new(),
            primary_cursor_line_number: LineNumber::Absolute(0),
            mode: "insert".to_string(),
            text_properties: TextPropertyManager::new(),
            show_cursors: true,
            editing_disabled: false,
            buffer_settings: BufferSettings::default(),
            reference_highlighter,
            is_composite_buffer: false,
            debug_highlight_mode: false,
            reference_highlight_overlay: ReferenceHighlightOverlay::new(),
            bracket_highlight_overlay: BracketHighlightOverlay::new(),
            semantic_tokens: None,
            language: language_name,
        })
    }

    /// Create an editor state from a file with language configuration.
    ///
    /// This version uses the provided languages configuration for syntax detection,
    /// allowing user-configured filename patterns to be respected for highlighting.
    ///
    /// Note: width/height parameters are kept for backward compatibility but
    /// are no longer used - viewport is now owned by SplitViewState.
    pub fn from_file_with_languages(
        path: &std::path::Path,
        _width: u16,
        _height: u16,
        large_file_threshold: usize,
        registry: &GrammarRegistry,
        languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
        fs: Arc<dyn FileSystem + Send + Sync>,
    ) -> anyhow::Result<Self> {
        let buffer = Buffer::load_from_file(path, large_file_threshold, fs)?;

        let highlighter = HighlightEngine::for_file_with_languages(path, registry, languages);

        let language = Language::from_path(path);
        let mut reference_highlighter = ReferenceHighlighter::new();
        let language_name = if let Some(lang) = &language {
            reference_highlighter.set_language(lang);
            lang.to_string()
        } else {
            crate::services::lsp::manager::detect_language(path, languages)
                .unwrap_or_else(|| "text".to_string())
        };

        let mut marker_list = MarkerList::new();
        if !buffer.is_empty() {
            marker_list.adjust_for_insert(0, buffer.len());
        }

        Ok(Self {
            buffer,
            highlighter,
            indent_calculator: RefCell::new(IndentCalculator::new()),
            overlays: OverlayManager::new(),
            marker_list,
            virtual_texts: VirtualTextManager::new(),
            conceals: ConcealManager::new(),
            soft_breaks: SoftBreakManager::new(),
            popups: PopupManager::new(),
            margins: MarginManager::new(),
            primary_cursor_line_number: LineNumber::Absolute(0),
            mode: "insert".to_string(),
            text_properties: TextPropertyManager::new(),
            show_cursors: true,
            editing_disabled: false,
            buffer_settings: BufferSettings::default(),
            reference_highlighter,
            is_composite_buffer: false,
            debug_highlight_mode: false,
            reference_highlight_overlay: ReferenceHighlightOverlay::new(),
            bracket_highlight_overlay: BracketHighlightOverlay::new(),
            semantic_tokens: None,
            language: language_name,
        })
    }

    /// Create an editor state from a buffer and highlighter
    ///
    /// This is useful when you have already loaded a buffer with a specific encoding
    /// and want to create an EditorState from it.
    pub fn from_buffer_with_highlighter(
        buffer: Buffer,
        highlighter: HighlightEngine,
        language_name: String,
        language: Option<Language>,
    ) -> Self {
        let mut reference_highlighter = ReferenceHighlighter::new();
        if let Some(lang) = &language {
            reference_highlighter.set_language(lang);
        }

        let mut marker_list = MarkerList::new();
        if !buffer.is_empty() {
            marker_list.adjust_for_insert(0, buffer.len());
        }

        Self {
            buffer,
            highlighter,
            indent_calculator: RefCell::new(IndentCalculator::new()),
            overlays: OverlayManager::new(),
            marker_list,
            virtual_texts: VirtualTextManager::new(),
            conceals: ConcealManager::new(),
            soft_breaks: SoftBreakManager::new(),
            popups: PopupManager::new(),
            margins: MarginManager::new(),
            primary_cursor_line_number: LineNumber::Absolute(0),
            mode: "insert".to_string(),
            text_properties: TextPropertyManager::new(),
            show_cursors: true,
            editing_disabled: false,
            buffer_settings: BufferSettings::default(),
            reference_highlighter,
            is_composite_buffer: false,
            debug_highlight_mode: false,
            reference_highlight_overlay: ReferenceHighlightOverlay::new(),
            bracket_highlight_overlay: BracketHighlightOverlay::new(),
            semantic_tokens: None,
            language: language_name,
        }
    }

    /// Handle an Insert event - adjusts markers, buffer, highlighter, cursors, and line numbers
    fn apply_insert(
        &mut self,
        cursors: &mut Cursors,
        position: usize,
        text: &str,
        cursor_id: crate::model::event::CursorId,
    ) {
        let newlines_inserted = text.matches('\n').count();

        // CRITICAL: Adjust markers BEFORE modifying buffer
        self.marker_list.adjust_for_insert(position, text.len());
        self.margins.adjust_for_insert(position, text.len());

        // Insert text into buffer
        self.buffer.insert(position, text);

        // Invalidate highlight cache for edited range
        self.highlighter
            .invalidate_range(position..position + text.len());

        // Note: reference_highlight_overlay uses markers that auto-adjust,
        // so no manual invalidation needed

        // Adjust all cursors after the edit
        cursors.adjust_for_edit(position, 0, text.len());

        // Move the cursor that made the edit to the end of the insertion
        if let Some(cursor) = cursors.get_mut(cursor_id) {
            cursor.position = position + text.len();
            cursor.clear_selection();
        }

        // Update primary cursor line number if this was the primary cursor
        if cursor_id == cursors.primary_id() {
            self.primary_cursor_line_number = match self.primary_cursor_line_number {
                LineNumber::Absolute(line) => LineNumber::Absolute(line + newlines_inserted),
                LineNumber::Relative {
                    line,
                    from_cached_line,
                } => LineNumber::Relative {
                    line: line + newlines_inserted,
                    from_cached_line,
                },
            };
        }
    }

    /// Handle a Delete event - adjusts markers, buffer, highlighter, cursors, and line numbers
    fn apply_delete(
        &mut self,
        cursors: &mut Cursors,
        range: &std::ops::Range<usize>,
        cursor_id: crate::model::event::CursorId,
        deleted_text: &str,
    ) {
        let len = range.len();
        let newlines_deleted = deleted_text.matches('\n').count();

        // CRITICAL: Adjust markers BEFORE modifying buffer
        self.marker_list.adjust_for_delete(range.start, len);
        self.margins.adjust_for_delete(range.start, len);

        // Delete from buffer
        self.buffer.delete(range.clone());

        // Invalidate highlight cache for edited range
        self.highlighter.invalidate_range(range.clone());

        // Note: reference_highlight_overlay uses markers that auto-adjust,
        // so no manual invalidation needed

        // Adjust all cursors after the edit
        cursors.adjust_for_edit(range.start, len, 0);

        // Move the cursor that made the edit to the start of deletion
        if let Some(cursor) = cursors.get_mut(cursor_id) {
            cursor.position = range.start;
            cursor.clear_selection();
        }

        // Update primary cursor line number if this was the primary cursor
        if cursor_id == cursors.primary_id() {
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
    }

    /// Apply an event to the state - THE ONLY WAY TO MODIFY STATE
    /// This is the heart of the event-driven architecture
    pub fn apply(&mut self, cursors: &mut Cursors, event: &Event) {
        match event {
            Event::Insert {
                position,
                text,
                cursor_id,
            } => self.apply_insert(cursors, *position, text, *cursor_id),

            Event::Delete {
                range,
                cursor_id,
                deleted_text,
            } => self.apply_delete(cursors, range, *cursor_id, deleted_text),

            Event::MoveCursor {
                cursor_id,
                new_position,
                new_anchor,
                new_sticky_column,
                ..
            } => {
                if let Some(cursor) = cursors.get_mut(*cursor_id) {
                    cursor.position = *new_position;
                    cursor.anchor = *new_anchor;
                    cursor.sticky_column = *new_sticky_column;
                }

                // Update primary cursor line number if this is the primary cursor
                // Try to get exact line number from buffer, or estimate for large files
                if *cursor_id == cursors.primary_id() {
                    self.primary_cursor_line_number =
                        match self.buffer.offset_to_position(*new_position) {
                            Some(pos) => LineNumber::Absolute(pos.line),
                            None => {
                                // Large file without line metadata - estimate line number
                                // Use default estimated_line_length of 80 bytes
                                let estimated_line = *new_position / 80;
                                LineNumber::Absolute(estimated_line)
                            }
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
                cursors.insert_with_id(*cursor_id, cursor);

                cursors.normalize();
            }

            Event::RemoveCursor { cursor_id, .. } => {
                cursors.remove(*cursor_id);
            }

            // View events (Scroll, SetViewport, Recenter) are now handled at Editor level
            // via SplitViewState. They should not reach EditorState.apply().
            Event::Scroll { .. } | Event::SetViewport { .. } | Event::Recenter => {
                // These events are intercepted in Editor::apply_event_to_active_buffer
                // and routed to SplitViewState. If we get here, something is wrong.
                tracing::warn!("View event {:?} reached EditorState.apply() - should be handled by SplitViewState", event);
            }

            Event::SetAnchor {
                cursor_id,
                position,
            } => {
                // Set the anchor (selection start) for a specific cursor
                // Also disable deselect_on_move so movement preserves the selection (Emacs mark mode)
                if let Some(cursor) = cursors.get_mut(*cursor_id) {
                    cursor.anchor = Some(*position);
                    cursor.deselect_on_move = false;
                }
            }

            Event::ClearAnchor { cursor_id } => {
                // Clear the anchor and reset deselect_on_move to cancel mark mode
                // Also clear block selection if active
                if let Some(cursor) = cursors.get_mut(*cursor_id) {
                    cursor.anchor = None;
                    cursor.deselect_on_move = true;
                    cursor.clear_block_selection();
                }
            }

            Event::ChangeMode { mode } => {
                self.mode = mode.clone();
            }

            Event::AddOverlay {
                namespace,
                range,
                face,
                priority,
                message,
                extend_to_line_end,
                url,
            } => {
                tracing::debug!(
                    "AddOverlay: namespace={:?}, range={:?}, face={:?}, priority={}",
                    namespace,
                    range,
                    face,
                    priority
                );
                // Convert event overlay face to overlay face
                let overlay_face = convert_event_face_to_overlay_face(face);
                tracing::trace!("Converted face: {:?}", overlay_face);

                let mut overlay = Overlay::with_priority(
                    &mut self.marker_list,
                    range.clone(),
                    overlay_face,
                    *priority,
                );
                overlay.namespace = namespace.clone();
                overlay.message = message.clone();
                overlay.extend_to_line_end = *extend_to_line_end;
                overlay.url = url.clone();

                let actual_range = overlay.range(&self.marker_list);
                tracing::debug!(
                    "Created overlay with markers - actual range: {:?}, handle={:?}",
                    actual_range,
                    overlay.handle
                );

                self.overlays.add(overlay);
            }

            Event::RemoveOverlay { handle } => {
                tracing::debug!("RemoveOverlay: handle={:?}", handle);
                self.overlays
                    .remove_by_handle(handle, &mut self.marker_list);
            }

            Event::RemoveOverlaysInRange { range } => {
                self.overlays.remove_in_range(range, &mut self.marker_list);
            }

            Event::ClearNamespace { namespace } => {
                tracing::debug!("ClearNamespace: namespace={:?}", namespace);
                self.overlays
                    .clear_namespace(namespace, &mut self.marker_list);
            }

            Event::ClearOverlays => {
                self.overlays.clear(&mut self.marker_list);
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
                self.margins.configure_for_line_numbers(*enabled);
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
                    self.apply(cursors, event);
                }
            }

            Event::BulkEdit {
                new_snapshot,
                new_cursors,
                ..
            } => {
                // Restore the target buffer state (piece tree + buffers) for this event.
                // - For original application: this is set after apply_events_as_bulk_edit
                // - For undo: snapshots are swapped, so new_snapshot is the original state
                // - For redo: new_snapshot is the state after edits
                // Restoring buffers alongside the tree is critical because
                // consolidate_after_save() can replace buffers between snapshot and restore.
                if let Some(snapshot) = new_snapshot {
                    self.buffer.restore_buffer_state(snapshot);
                }

                // Update cursor positions
                for (cursor_id, position, anchor) in new_cursors {
                    if let Some(cursor) = cursors.get_mut(*cursor_id) {
                        cursor.position = *position;
                        cursor.anchor = *anchor;
                    }
                }

                // Invalidate highlight cache for entire buffer
                self.highlighter.invalidate_all();

                // Update primary cursor line number
                let primary_pos = cursors.primary().position;
                self.primary_cursor_line_number = match self.buffer.offset_to_position(primary_pos)
                {
                    Some(pos) => crate::model::buffer::LineNumber::Absolute(pos.line),
                    None => crate::model::buffer::LineNumber::Absolute(0),
                };
            }
        }
    }

    /// Apply multiple events in sequence
    pub fn apply_many(&mut self, cursors: &mut Cursors, events: &[Event]) {
        for event in events {
            self.apply(cursors, event);
        }
    }

    /// Called when this buffer loses focus (e.g., switching to another buffer,
    /// opening a prompt, focusing file explorer, etc.)
    /// Dismisses transient popups like Hover and Signature Help.
    pub fn on_focus_lost(&mut self) {
        if self.popups.dismiss_transient() {
            tracing::debug!("Dismissed transient popup on buffer focus loss");
        }
    }
}

/// Convert event overlay face to the actual overlay face
fn convert_event_face_to_overlay_face(event_face: &EventOverlayFace) -> OverlayFace {
    match event_face {
        EventOverlayFace::Underline { color, style } => {
            let underline_style = match style {
                crate::model::event::UnderlineStyle::Straight => UnderlineStyle::Straight,
                crate::model::event::UnderlineStyle::Wavy => UnderlineStyle::Wavy,
                crate::model::event::UnderlineStyle::Dotted => UnderlineStyle::Dotted,
                crate::model::event::UnderlineStyle::Dashed => UnderlineStyle::Dashed,
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
        EventOverlayFace::Style { options } => {
            use ratatui::style::Modifier;

            // Build fallback style from RGB values
            let mut style = Style::default();

            // Extract foreground color (RGB fallback or default white)
            if let Some(ref fg) = options.fg {
                if let Some((r, g, b)) = fg.as_rgb() {
                    style = style.fg(Color::Rgb(r, g, b));
                }
            }

            // Extract background color (RGB fallback)
            if let Some(ref bg) = options.bg {
                if let Some((r, g, b)) = bg.as_rgb() {
                    style = style.bg(Color::Rgb(r, g, b));
                }
            }

            // Apply modifiers
            let mut modifiers = Modifier::empty();
            if options.bold {
                modifiers |= Modifier::BOLD;
            }
            if options.italic {
                modifiers |= Modifier::ITALIC;
            }
            if options.underline {
                modifiers |= Modifier::UNDERLINED;
            }
            if options.strikethrough {
                modifiers |= Modifier::CROSSED_OUT;
            }
            if !modifiers.is_empty() {
                style = style.add_modifier(modifiers);
            }

            // Extract theme keys
            let fg_theme = options
                .fg
                .as_ref()
                .and_then(|c| c.as_theme_key())
                .map(String::from);
            let bg_theme = options
                .bg
                .as_ref()
                .and_then(|c| c.as_theme_key())
                .map(String::from);

            // If theme keys are provided, use ThemedStyle for runtime resolution
            if fg_theme.is_some() || bg_theme.is_some() {
                OverlayFace::ThemedStyle {
                    fallback_style: style,
                    fg_theme,
                    bg_theme,
                }
            } else {
                OverlayFace::Style { style }
            }
        }
    }
}

/// Convert popup data to the actual popup object
fn convert_popup_data_to_popup(data: &PopupData) -> Popup {
    let content = match &data.content {
        crate::model::event::PopupContentData::Text(lines) => PopupContent::Text(lines.clone()),
        crate::model::event::PopupContentData::List { items, selected } => PopupContent::List {
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
        PopupPositionData::BottomRight => PopupPosition::BottomRight,
    };

    // Map the explicit kind hint to PopupKind for input handling
    let kind = match data.kind {
        crate::model::event::PopupKindHint::Completion => PopupKind::Completion,
        crate::model::event::PopupKindHint::List => PopupKind::List,
        crate::model::event::PopupKindHint::Text => PopupKind::Text,
    };

    Popup {
        kind,
        title: data.title.clone(),
        description: data.description.clone(),
        transient: data.transient,
        content,
        position,
        width: data.width,
        max_height: data.max_height,
        bordered: data.bordered,
        border_style: Style::default().fg(Color::Gray),
        background_style: Style::default().bg(Color::Rgb(30, 30, 30)),
        scroll_offset: 0,
        text_selection: None,
    }
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

impl EditorState {
    /// Prepare viewport for rendering (called before frame render)
    ///
    /// This pre-loads all data that will be needed for rendering the current viewport,
    /// ensuring that subsequent read-only access during rendering will succeed.
    ///
    /// Takes viewport parameters since viewport is now owned by SplitViewState.
    pub fn prepare_for_render(&mut self, top_byte: usize, height: u16) -> Result<()> {
        self.buffer.prepare_viewport(top_byte, height as usize)?;
        Ok(())
    }

    // ========== DocumentModel Helper Methods ==========
    // These methods provide convenient access to DocumentModel functionality
    // while maintaining backward compatibility with existing code.

    /// Get text in a range, driving lazy loading transparently
    ///
    /// This is a convenience wrapper around DocumentModel::get_range that:
    /// - Drives lazy loading automatically (never fails due to unloaded data)
    /// - Uses byte offsets directly
    /// - Returns String (not Result) - errors are logged internally
    /// - Returns empty string for invalid ranges
    ///
    /// This is the preferred API for getting text ranges. The caller never needs
    /// to worry about lazy loading or buffer preparation.
    ///
    /// # Example
    /// ```ignore
    /// let text = state.get_text_range(0, 100);
    /// ```
    pub fn get_text_range(&mut self, start: usize, end: usize) -> String {
        // TextBuffer::get_text_range_mut() handles lazy loading automatically
        match self
            .buffer
            .get_text_range_mut(start, end.saturating_sub(start))
        {
            Ok(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
            Err(e) => {
                tracing::warn!("Failed to get text range {}..{}: {}", start, end, e);
                String::new()
            }
        }
    }

    /// Get the content of a line by its byte offset
    ///
    /// Returns the line containing the given offset, along with its start position.
    /// This uses DocumentModel's viewport functionality for consistent behavior.
    ///
    /// # Returns
    /// `Some((line_start_offset, line_content))` if successful, `None` if offset is invalid
    pub fn get_line_at_offset(&mut self, offset: usize) -> Option<(usize, String)> {
        use crate::model::document_model::DocumentModel;

        // Find the start of the line containing this offset
        // Scan backwards to find the previous newline or start of buffer
        let mut line_start = offset;
        while line_start > 0 {
            if let Ok(text) = self.buffer.get_text_range_mut(line_start - 1, 1) {
                if text.first() == Some(&b'\n') {
                    break;
                }
                line_start -= 1;
            } else {
                break;
            }
        }

        // Get a single line viewport starting at the line start
        let viewport = self
            .get_viewport_content(
                crate::model::document_model::DocumentPosition::byte(line_start),
                1,
            )
            .ok()?;

        viewport
            .lines
            .first()
            .map(|line| (line.byte_offset, line.content.clone()))
    }

    /// Get text from current cursor position to end of line
    ///
    /// This is a common pattern in editing operations. Uses DocumentModel
    /// for consistent behavior across file sizes.
    pub fn get_text_to_end_of_line(&mut self, cursor_pos: usize) -> Result<String> {
        use crate::model::document_model::DocumentModel;

        // Get the line containing cursor
        let viewport = self.get_viewport_content(
            crate::model::document_model::DocumentPosition::byte(cursor_pos),
            1,
        )?;

        if let Some(line) = viewport.lines.first() {
            let line_start = line.byte_offset;
            let line_end = line_start + line.content.len();

            if cursor_pos >= line_start && cursor_pos <= line_end {
                let offset_in_line = cursor_pos - line_start;
                // Use get() to safely handle potential non-char-boundary offsets
                Ok(line.content.get(offset_in_line..).unwrap_or("").to_string())
            } else {
                Ok(String::new())
            }
        } else {
            Ok(String::new())
        }
    }

    /// Replace cached semantic tokens with a new store.
    pub fn set_semantic_tokens(&mut self, store: SemanticTokenStore) {
        self.semantic_tokens = Some(store);
    }

    /// Clear cached semantic tokens (e.g., when tokens are invalidated).
    pub fn clear_semantic_tokens(&mut self) {
        self.semantic_tokens = None;
    }

    /// Get the server-provided semantic token result_id if available.
    pub fn semantic_tokens_result_id(&self) -> Option<&str> {
        self.semantic_tokens
            .as_ref()
            .and_then(|store| store.result_id.as_deref())
    }
}

/// Implement DocumentModel trait for EditorState
///
/// This provides a clean abstraction layer between rendering/editing operations
/// and the underlying text buffer implementation.
impl DocumentModel for EditorState {
    fn capabilities(&self) -> DocumentCapabilities {
        let line_count = self.buffer.line_count();
        DocumentCapabilities {
            has_line_index: line_count.is_some(),
            uses_lazy_loading: false, // TODO: add large file detection
            byte_length: self.buffer.len(),
            approximate_line_count: line_count.unwrap_or_else(|| {
                // Estimate assuming ~80 bytes per line
                self.buffer.len() / 80
            }),
        }
    }

    fn get_viewport_content(
        &mut self,
        start_pos: DocumentPosition,
        max_lines: usize,
    ) -> Result<ViewportContent> {
        // Convert to byte offset
        let start_offset = self.position_to_offset(start_pos)?;

        // Use new efficient line iteration that tracks line numbers during iteration
        // by accumulating line_feed_cnt from pieces (single source of truth)
        let line_iter = self.buffer.iter_lines_from(start_offset, max_lines)?;
        let has_more = line_iter.has_more;

        let lines = line_iter
            .map(|line_data| ViewportLine {
                byte_offset: line_data.byte_offset,
                content: line_data.content,
                has_newline: line_data.has_newline,
                approximate_line_number: line_data.line_number,
            })
            .collect();

        Ok(ViewportContent {
            start_position: DocumentPosition::ByteOffset(start_offset),
            lines,
            has_more,
        })
    }

    fn position_to_offset(&self, pos: DocumentPosition) -> Result<usize> {
        match pos {
            DocumentPosition::ByteOffset(offset) => Ok(offset),
            DocumentPosition::LineColumn { line, column } => {
                if !self.has_line_index() {
                    anyhow::bail!("Line indexing not available for this document");
                }
                // Use piece tree's position conversion
                let position = crate::model::piece_tree::Position { line, column };
                Ok(self.buffer.position_to_offset(position))
            }
        }
    }

    fn offset_to_position(&self, offset: usize) -> DocumentPosition {
        if self.has_line_index() {
            if let Some(pos) = self.buffer.offset_to_position(offset) {
                DocumentPosition::LineColumn {
                    line: pos.line,
                    column: pos.column,
                }
            } else {
                // Line index exists but metadata unavailable - fall back to byte offset
                DocumentPosition::ByteOffset(offset)
            }
        } else {
            DocumentPosition::ByteOffset(offset)
        }
    }

    fn get_range(&mut self, start: DocumentPosition, end: DocumentPosition) -> Result<String> {
        let start_offset = self.position_to_offset(start)?;
        let end_offset = self.position_to_offset(end)?;

        if start_offset > end_offset {
            anyhow::bail!(
                "Invalid range: start offset {} > end offset {}",
                start_offset,
                end_offset
            );
        }

        let bytes = self
            .buffer
            .get_text_range_mut(start_offset, end_offset - start_offset)?;

        Ok(String::from_utf8_lossy(&bytes).into_owned())
    }

    fn get_line_content(&mut self, line_number: usize) -> Option<String> {
        if !self.has_line_index() {
            return None;
        }

        // Convert line number to byte offset
        let line_start_offset = self.buffer.line_start_offset(line_number)?;

        // Get line content using iterator
        let mut iter = self.buffer.line_iterator(line_start_offset, 80);
        if let Some((_start, content)) = iter.next_line() {
            let has_newline = content.ends_with('\n');
            let line_content = if has_newline {
                content[..content.len() - 1].to_string()
            } else {
                content
            };
            Some(line_content)
        } else {
            None
        }
    }

    fn get_chunk_at_offset(&mut self, offset: usize, size: usize) -> Result<(usize, String)> {
        let bytes = self.buffer.get_text_range_mut(offset, size)?;

        Ok((offset, String::from_utf8_lossy(&bytes).into_owned()))
    }

    fn insert(&mut self, pos: DocumentPosition, text: &str) -> Result<usize> {
        let offset = self.position_to_offset(pos)?;
        self.buffer.insert_bytes(offset, text.as_bytes().to_vec());
        Ok(text.len())
    }

    fn delete(&mut self, start: DocumentPosition, end: DocumentPosition) -> Result<()> {
        let start_offset = self.position_to_offset(start)?;
        let end_offset = self.position_to_offset(end)?;

        if start_offset > end_offset {
            anyhow::bail!(
                "Invalid range: start offset {} > end offset {}",
                start_offset,
                end_offset
            );
        }

        self.buffer.delete(start_offset..end_offset);
        Ok(())
    }

    fn replace(
        &mut self,
        start: DocumentPosition,
        end: DocumentPosition,
        text: &str,
    ) -> Result<()> {
        // Delete then insert
        self.delete(start, end)?;
        self.insert(start, text)?;
        Ok(())
    }

    fn find_matches(
        &mut self,
        pattern: &str,
        search_range: Option<(DocumentPosition, DocumentPosition)>,
    ) -> Result<Vec<usize>> {
        let (start_offset, end_offset) = if let Some((start, end)) = search_range {
            (
                self.position_to_offset(start)?,
                self.position_to_offset(end)?,
            )
        } else {
            (0, self.buffer.len())
        };

        // Get text in range
        let bytes = self
            .buffer
            .get_text_range_mut(start_offset, end_offset - start_offset)?;
        let text = String::from_utf8_lossy(&bytes);

        // Find all matches (simple substring search for now)
        let mut matches = Vec::new();
        let mut search_offset = 0;
        while let Some(pos) = text[search_offset..].find(pattern) {
            matches.push(start_offset + search_offset + pos);
            search_offset += pos + pattern.len();
        }

        Ok(matches)
    }
}

/// Cached semantic tokens for a buffer.
#[derive(Clone, Debug)]
pub struct SemanticTokenStore {
    /// Buffer version the tokens correspond to.
    pub version: u64,
    /// Server-provided result identifier (if any).
    pub result_id: Option<String>,
    /// Raw semantic token data (u32 array, 5 integers per token).
    pub data: Vec<u32>,
    /// All semantic token spans resolved to byte ranges.
    pub tokens: Vec<SemanticTokenSpan>,
}

/// A semantic token span resolved to buffer byte offsets.
#[derive(Clone, Debug)]
pub struct SemanticTokenSpan {
    pub range: Range<usize>,
    pub token_type: String,
    pub modifiers: Vec<String>,
}

#[cfg(test)]
mod tests {
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }
    use super::*;
    use crate::model::event::CursorId;

    #[test]
    fn test_state_new() {
        let state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        assert!(state.buffer.is_empty());
    }

    #[test]
    fn test_apply_insert() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();
        let cursor_id = cursors.primary_id();

        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "hello".to_string(),
                cursor_id,
            },
        );

        assert_eq!(state.buffer.to_string().unwrap(), "hello");
        assert_eq!(cursors.primary().position, 5);
        assert!(state.buffer.is_modified());
    }

    #[test]
    fn test_apply_delete() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();
        let cursor_id = cursors.primary_id();

        // Insert then delete
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "hello world".to_string(),
                cursor_id,
            },
        );

        state.apply(
            &mut cursors,
            &Event::Delete {
                range: 5..11,
                deleted_text: " world".to_string(),
                cursor_id,
            },
        );

        assert_eq!(state.buffer.to_string().unwrap(), "hello");
        assert_eq!(cursors.primary().position, 5);
    }

    #[test]
    fn test_apply_move_cursor() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();
        let cursor_id = cursors.primary_id();

        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "hello".to_string(),
                cursor_id,
            },
        );

        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id,
                old_position: 5,
                new_position: 2,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        assert_eq!(cursors.primary().position, 2);
    }

    #[test]
    fn test_apply_add_cursor() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();
        let cursor_id = CursorId(1);

        state.apply(
            &mut cursors,
            &Event::AddCursor {
                cursor_id,
                position: 5,
                anchor: None,
            },
        );

        assert_eq!(cursors.count(), 2);
    }

    #[test]
    fn test_apply_many() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();
        let cursor_id = cursors.primary_id();

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

        state.apply_many(&mut cursors, &events);

        assert_eq!(state.buffer.to_string().unwrap(), "hello world");
    }

    #[test]
    fn test_cursor_adjustment_after_insert() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();
        let cursor_id = cursors.primary_id();

        // Add a second cursor at position 5
        state.apply(
            &mut cursors,
            &Event::AddCursor {
                cursor_id: CursorId(1),
                position: 5,
                anchor: None,
            },
        );

        // Insert at position 0 - should push second cursor forward
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "abc".to_string(),
                cursor_id,
            },
        );

        // Second cursor should be at position 5 + 3 = 8
        if let Some(cursor) = cursors.get(CursorId(1)) {
            assert_eq!(cursor.position, 8);
        }
    }

    // DocumentModel trait tests
    mod document_model_tests {
        use super::*;
        use crate::model::document_model::{DocumentModel, DocumentPosition};

        #[test]
        fn test_capabilities_small_file() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("line1\nline2\nline3");

            let caps = state.capabilities();
            assert!(caps.has_line_index, "Small file should have line index");
            assert_eq!(caps.byte_length, "line1\nline2\nline3".len());
            assert_eq!(caps.approximate_line_count, 3, "Should have 3 lines");
        }

        #[test]
        fn test_position_conversions() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("hello\nworld\ntest");

            // Test ByteOffset -> offset
            let pos1 = DocumentPosition::ByteOffset(6);
            let offset1 = state.position_to_offset(pos1).unwrap();
            assert_eq!(offset1, 6);

            // Test LineColumn -> offset
            let pos2 = DocumentPosition::LineColumn { line: 1, column: 0 };
            let offset2 = state.position_to_offset(pos2).unwrap();
            assert_eq!(offset2, 6, "Line 1, column 0 should be at byte 6");

            // Test offset -> position (should return LineColumn for small files)
            let converted = state.offset_to_position(6);
            match converted {
                DocumentPosition::LineColumn { line, column } => {
                    assert_eq!(line, 1);
                    assert_eq!(column, 0);
                }
                _ => panic!("Expected LineColumn for small file"),
            }
        }

        #[test]
        fn test_get_viewport_content() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5");

            let content = state
                .get_viewport_content(DocumentPosition::ByteOffset(0), 3)
                .unwrap();

            assert_eq!(content.lines.len(), 3);
            assert_eq!(content.lines[0].content, "line1");
            assert_eq!(content.lines[1].content, "line2");
            assert_eq!(content.lines[2].content, "line3");
            assert!(content.has_more);
        }

        #[test]
        fn test_get_range() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("hello world");

            let text = state
                .get_range(
                    DocumentPosition::ByteOffset(0),
                    DocumentPosition::ByteOffset(5),
                )
                .unwrap();
            assert_eq!(text, "hello");

            let text2 = state
                .get_range(
                    DocumentPosition::ByteOffset(6),
                    DocumentPosition::ByteOffset(11),
                )
                .unwrap();
            assert_eq!(text2, "world");
        }

        #[test]
        fn test_get_line_content() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("line1\nline2\nline3");

            let line0 = state.get_line_content(0).unwrap();
            assert_eq!(line0, "line1");

            let line1 = state.get_line_content(1).unwrap();
            assert_eq!(line1, "line2");

            let line2 = state.get_line_content(2).unwrap();
            assert_eq!(line2, "line3");
        }

        #[test]
        fn test_insert_delete() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("hello world");

            // Insert text
            let bytes_inserted = state
                .insert(DocumentPosition::ByteOffset(6), "beautiful ")
                .unwrap();
            assert_eq!(bytes_inserted, 10);
            assert_eq!(state.buffer.to_string().unwrap(), "hello beautiful world");

            // Delete text
            state
                .delete(
                    DocumentPosition::ByteOffset(6),
                    DocumentPosition::ByteOffset(16),
                )
                .unwrap();
            assert_eq!(state.buffer.to_string().unwrap(), "hello world");
        }

        #[test]
        fn test_replace() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("hello world");

            state
                .replace(
                    DocumentPosition::ByteOffset(0),
                    DocumentPosition::ByteOffset(5),
                    "hi",
                )
                .unwrap();
            assert_eq!(state.buffer.to_string().unwrap(), "hi world");
        }

        #[test]
        fn test_find_matches() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("hello world hello");

            let matches = state.find_matches("hello", None).unwrap();
            assert_eq!(matches.len(), 2);
            assert_eq!(matches[0], 0);
            assert_eq!(matches[1], 12);
        }

        #[test]
        fn test_prepare_for_render() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5");

            // Should not panic - pass top_byte=0 and height=24 (typical viewport params)
            state.prepare_for_render(0, 24).unwrap();
        }

        #[test]
        fn test_helper_get_text_range() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("hello world");

            // Test normal range
            let text = state.get_text_range(0, 5);
            assert_eq!(text, "hello");

            // Test middle range
            let text2 = state.get_text_range(6, 11);
            assert_eq!(text2, "world");
        }

        #[test]
        fn test_helper_get_line_at_offset() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("line1\nline2\nline3");

            // Get first line (offset 0)
            let (offset, content) = state.get_line_at_offset(0).unwrap();
            assert_eq!(offset, 0);
            assert_eq!(content, "line1");

            // Get second line (offset in middle of line)
            let (offset2, content2) = state.get_line_at_offset(8).unwrap();
            assert_eq!(offset2, 6); // Line starts at byte 6
            assert_eq!(content2, "line2");

            // Get last line
            let (offset3, content3) = state.get_line_at_offset(12).unwrap();
            assert_eq!(offset3, 12);
            assert_eq!(content3, "line3");
        }

        #[test]
        fn test_helper_get_text_to_end_of_line() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("hello world\nline2");

            // From beginning of line
            let text = state.get_text_to_end_of_line(0).unwrap();
            assert_eq!(text, "hello world");

            // From middle of line
            let text2 = state.get_text_to_end_of_line(6).unwrap();
            assert_eq!(text2, "world");

            // From end of line
            let text3 = state.get_text_to_end_of_line(11).unwrap();
            assert_eq!(text3, "");

            // From second line
            let text4 = state.get_text_to_end_of_line(12).unwrap();
            assert_eq!(text4, "line2");
        }
    }

    // Virtual text integration tests
    mod virtual_text_integration_tests {
        use super::*;
        use crate::view::virtual_text::VirtualTextPosition;
        use ratatui::style::Style;

        #[test]
        fn test_virtual_text_add_and_query() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("hello world");

            // Initialize marker list for buffer
            if !state.buffer.is_empty() {
                state.marker_list.adjust_for_insert(0, state.buffer.len());
            }

            // Add virtual text at position 5 (after 'hello')
            let vtext_id = state.virtual_texts.add(
                &mut state.marker_list,
                5,
                ": string".to_string(),
                Style::default(),
                VirtualTextPosition::AfterChar,
                0,
            );

            // Query should return the virtual text
            let results = state.virtual_texts.query_range(&state.marker_list, 0, 11);
            assert_eq!(results.len(), 1);
            assert_eq!(results[0].0, 5); // Position
            assert_eq!(results[0].1.text, ": string");

            // Build lookup should work
            let lookup = state.virtual_texts.build_lookup(&state.marker_list, 0, 11);
            assert!(lookup.contains_key(&5));
            assert_eq!(lookup[&5].len(), 1);
            assert_eq!(lookup[&5][0].text, ": string");

            // Clean up
            state.virtual_texts.remove(&mut state.marker_list, vtext_id);
            assert!(state.virtual_texts.is_empty());
        }

        #[test]
        fn test_virtual_text_position_tracking_on_insert() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("hello world");

            // Initialize marker list for buffer
            if !state.buffer.is_empty() {
                state.marker_list.adjust_for_insert(0, state.buffer.len());
            }

            // Add virtual text at position 6 (the 'w' in 'world')
            let _vtext_id = state.virtual_texts.add(
                &mut state.marker_list,
                6,
                "/*param*/".to_string(),
                Style::default(),
                VirtualTextPosition::BeforeChar,
                0,
            );

            // Insert "beautiful " at position 6 using Event
            let mut cursors = Cursors::new();
            let cursor_id = cursors.primary_id();
            state.apply(
                &mut cursors,
                &Event::Insert {
                    position: 6,
                    text: "beautiful ".to_string(),
                    cursor_id,
                },
            );

            // Virtual text should now be at position 16 (6 + 10)
            let results = state.virtual_texts.query_range(&state.marker_list, 0, 30);
            assert_eq!(results.len(), 1);
            assert_eq!(results[0].0, 16); // Position should have moved
            assert_eq!(results[0].1.text, "/*param*/");
        }

        #[test]
        fn test_virtual_text_position_tracking_on_delete() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("hello beautiful world");

            // Initialize marker list for buffer
            if !state.buffer.is_empty() {
                state.marker_list.adjust_for_insert(0, state.buffer.len());
            }

            // Add virtual text at position 16 (the 'w' in 'world')
            let _vtext_id = state.virtual_texts.add(
                &mut state.marker_list,
                16,
                ": string".to_string(),
                Style::default(),
                VirtualTextPosition::AfterChar,
                0,
            );

            // Delete "beautiful " (positions 6-16) using Event
            let mut cursors = Cursors::new();
            let cursor_id = cursors.primary_id();
            state.apply(
                &mut cursors,
                &Event::Delete {
                    range: 6..16,
                    deleted_text: "beautiful ".to_string(),
                    cursor_id,
                },
            );

            // Virtual text should now be at position 6
            let results = state.virtual_texts.query_range(&state.marker_list, 0, 20);
            assert_eq!(results.len(), 1);
            assert_eq!(results[0].0, 6); // Position should have moved back
            assert_eq!(results[0].1.text, ": string");
        }

        #[test]
        fn test_multiple_virtual_texts_with_priorities() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("let x = 5");

            // Initialize marker list for buffer
            if !state.buffer.is_empty() {
                state.marker_list.adjust_for_insert(0, state.buffer.len());
            }

            // Add type hint after 'x' (position 5)
            state.virtual_texts.add(
                &mut state.marker_list,
                5,
                ": i32".to_string(),
                Style::default(),
                VirtualTextPosition::AfterChar,
                0, // Lower priority - renders first
            );

            // Add another hint at same position with higher priority
            state.virtual_texts.add(
                &mut state.marker_list,
                5,
                " /* inferred */".to_string(),
                Style::default(),
                VirtualTextPosition::AfterChar,
                10, // Higher priority - renders second
            );

            // Build lookup - should have both, sorted by priority (lower first)
            let lookup = state.virtual_texts.build_lookup(&state.marker_list, 0, 10);
            assert!(lookup.contains_key(&5));
            let vtexts = &lookup[&5];
            assert_eq!(vtexts.len(), 2);
            // Lower priority first (like layer ordering)
            assert_eq!(vtexts[0].text, ": i32");
            assert_eq!(vtexts[1].text, " /* inferred */");
        }

        #[test]
        fn test_virtual_text_clear() {
            let mut state = EditorState::new(
                80,
                24,
                crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
                test_fs(),
            );
            state.buffer = Buffer::from_str_test("test");

            // Initialize marker list for buffer
            if !state.buffer.is_empty() {
                state.marker_list.adjust_for_insert(0, state.buffer.len());
            }

            // Add multiple virtual texts
            state.virtual_texts.add(
                &mut state.marker_list,
                0,
                "hint1".to_string(),
                Style::default(),
                VirtualTextPosition::BeforeChar,
                0,
            );
            state.virtual_texts.add(
                &mut state.marker_list,
                2,
                "hint2".to_string(),
                Style::default(),
                VirtualTextPosition::AfterChar,
                0,
            );

            assert_eq!(state.virtual_texts.len(), 2);

            // Clear all
            state.virtual_texts.clear(&mut state.marker_list);
            assert!(state.virtual_texts.is_empty());

            // Query should return nothing
            let results = state.virtual_texts.query_range(&state.marker_list, 0, 10);
            assert!(results.is_empty());
        }
    }
}
