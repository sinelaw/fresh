use super::theme::CellThemeInfo;
use crate::model::event::{BufferId, ContainerId, LeafId, SplitDirection};
use ratatui::layout::Rect;
use std::collections::{HashMap, HashSet};

/// Mapping from visual row to buffer positions for mouse click handling
/// Each entry represents one visual row with byte position info for click handling
#[derive(Debug, Clone, Default)]
pub struct ViewLineMapping {
    /// Source byte offset for each character (None for injected/virtual content)
    pub char_source_bytes: Vec<Option<usize>>,
    /// Character index at each visual column (for O(1) mouse clicks)
    pub visual_to_char: Vec<usize>,
    /// Last valid byte position in this visual row (newline for real lines, last char for wrapped)
    /// Clicks past end of visible text position cursor here
    pub line_end_byte: usize,
    /// True iff this visual row was rendered for a plugin-injected
    /// virtual line (live-diff deletion overlays, markdown_compose
    /// borders, …) rather than for actual buffer content. Used by
    /// `move_visual_line` to skip past these rows without stranding
    /// the cursor on a position whose `line_end_byte` was inherited
    /// from the previous source row.
    pub is_plugin_virtual: bool,
}

impl ViewLineMapping {
    /// Get source byte at a given visual column (O(1) for mouse clicks)
    #[inline]
    pub fn source_byte_at_visual_col(&self, visual_col: usize) -> Option<usize> {
        let char_idx = self.visual_to_char.get(visual_col).copied()?;
        self.char_source_bytes.get(char_idx).copied().flatten()
    }

    /// Find the nearest source byte to a given visual column, searching outward.
    /// Returns the source byte at the closest valid visual column.
    pub fn nearest_source_byte(&self, goal_col: usize) -> Option<usize> {
        let width = self.visual_to_char.len();
        if width == 0 {
            return None;
        }
        // Search outward from goal_col: try +1, -1, +2, -2, ...
        for delta in 1..width {
            if goal_col + delta < width {
                if let Some(byte) = self.source_byte_at_visual_col(goal_col + delta) {
                    return Some(byte);
                }
            }
            if delta <= goal_col {
                if let Some(byte) = self.source_byte_at_visual_col(goal_col - delta) {
                    return Some(byte);
                }
            }
        }
        None
    }

    /// Check if this visual row contains the given byte position
    #[inline]
    pub fn contains_byte(&self, byte_pos: usize) -> bool {
        // A row contains a byte if it's in the char_source_bytes range
        // The first valid source byte marks the start, line_end_byte marks the end
        if let Some(first_byte) = self.char_source_bytes.iter().find_map(|b| *b) {
            byte_pos >= first_byte && byte_pos <= self.line_end_byte
        } else {
            // Empty/virtual row - only matches if byte_pos equals line_end_byte
            byte_pos == self.line_end_byte
        }
    }

    /// Get the first source byte position in this row (if any)
    #[inline]
    pub fn first_source_byte(&self) -> Option<usize> {
        self.char_source_bytes.iter().find_map(|b| *b)
    }
}

/// Type alias for popup area layout information used in mouse hit testing.
/// Fields: (popup_index, rect, inner_rect, scroll_offset, num_items, scrollbar_rect, total_lines)
pub(crate) type PopupAreaLayout = (usize, Rect, Rect, usize, usize, Option<Rect>, usize);

/// Editor-chrome layout cache: full-frame and chrome-region rects
/// (status bar, menu bar, prompt overlay, popups) plus the screen-
/// indexed cell-theme map. Per-window layout (split-leaf rects, tab
/// rects, file-explorer rects, view-line mappings) lives on
/// [`WindowLayoutCache`] instead.
#[derive(Debug, Clone, Default)]
pub(crate) struct ChromeLayout {
    /// Popup areas for mouse hit testing
    /// scrollbar_rect is Some if popup has a scrollbar
    pub popup_areas: Vec<PopupAreaLayout>,
    /// Editor-level popup areas (e.g. plugin action popups) for mouse hit
    /// testing. Stored separately from buffer popups because they're owned by
    /// `Editor.global_popups` rather than the active buffer's state.
    /// Fields: (popup_index, rect, inner_rect, scroll_offset, num_items)
    pub global_popup_areas: Vec<(usize, Rect, Rect, usize, usize)>,
    /// Suggestions area for mouse hit testing
    /// (inner_rect, scroll_start_idx, visible_count, total_count)
    pub suggestions_area: Option<(Rect, usize, usize, usize)>,
    /// Full outer rect of the suggestions popup (including borders).
    /// Used to absorb clicks on the popup chrome so they don't reach the
    /// buffer below while the prompt is open.
    pub suggestions_outer_area: Option<Rect>,
    /// Hit-test rect for the floating-overlay prompt's scrollbar
    /// (issue #1796). `None` when no overlay is open or the result
    /// list fits in the visible window. Click/drag handlers in
    /// `mouse_input.rs` read this to update `prompt.scroll_offset`.
    pub suggestions_scrollbar_rect: Option<Rect>,
    /// Hit rects for the floating-overlay prompt's widget toolbar, as
    /// (widget_key, screen_rect) pairs. Populated when the prompt carries a
    /// `toolbar_widget`; a click inside one fires the matching
    /// `live_grep_toggle_<key>` action. Empty otherwise.
    pub prompt_toolbar_hits: Vec<(String, Rect)>,
    /// Screen rect of the floating-overlay prompt's results list (issue
    /// #2119). `None` when no overlay is open. The mouse-wheel handler reads
    /// this to scroll the result list (without moving the selection) when the
    /// pointer is over it.
    pub prompt_results_area: Option<Rect>,
    /// Screen rect of the floating-overlay prompt's preview pane (issue
    /// #2119). `None` when no overlay is open or the overlay is too narrow to
    /// show a preview. The mouse-wheel handler reads this to scroll the
    /// preview (rather than the result list) when the pointer is over it.
    pub prompt_preview_area: Option<Rect>,
    /// Settings modal layout for hit testing
    pub settings_layout: Option<crate::view::settings::SettingsLayout>,
    /// Workspace-trust dialog click layout (radios + OK/Quit) for hit testing.
    pub workspace_trust_dialog: Option<crate::view::workspace_trust_dialog::TrustDialogLayout>,
    /// Status-bar hit-test layout (area, clickable segments, plugin token
    /// areas, semantic segment model). See [`StatusBarChrome`].
    pub status_bar: StatusBarChrome,
    /// Search options layout for checkbox hit testing
    pub search_options_layout: Option<crate::view::ui::status_bar::SearchOptionsLayout>,
    /// Menu bar layout for hit testing
    pub menu_layout: Option<crate::view::ui::menu::MenuLayout>,
    /// Dimensions of the last rendered frame. See [`FrameDimensions`].
    pub last_frame: FrameDimensions,
    /// Per-cell theme key provenance recorded during rendering.
    /// Flat vec indexed as `row * width + col` where `width = last_frame.width`.
    pub cell_theme_map: Vec<CellThemeInfo>,
}

/// Width and height of the most recently rendered frame. Used to size the
/// cell-theme map and to clamp / replay layout against the latest frame
/// extent (macro replay, dock/overlay sizing). Grouped so the pair travels
/// together rather than as loose `last_frame_*` members of [`ChromeLayout`].
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct FrameDimensions {
    pub width: u16,
    pub height: u16,
}

/// Status-bar hit-test layout captured each frame by `render_status_bar`.
/// Grouped so the four related fields travel together rather than as loose
/// `status_bar_*` members of [`ChromeLayout`].
#[derive(Debug, Clone, Default)]
pub(crate) struct StatusBarChrome {
    /// Status bar area (row, x, width)
    pub area: Option<(u16, u16, u16)>,
    /// Every clickable built-in status-bar segment drawn last frame, as
    /// `(id, row, start_col, end_col)`. One generic list (mirroring
    /// `StatusBarLayout::clickable`) walked by both the hover hit-test and
    /// `handle_click_status_bar` — no per-indicator field. See
    /// `StatusBarClickable`.
    pub clickable: Vec<(
        crate::view::ui::status_bar::StatusBarClickable,
        u16,
        u16,
        u16,
    )>,
    /// Plugin-registered status-bar token areas, keyed by
    /// `"<plugin>:<token>"`. Populated by `render_status_bar`; consumed
    /// by `handle_click_status_bar` which fires the
    /// `status_bar_token_clicked` hook on a hit so the registering
    /// plugin can react (typically by re-opening a deferred prompt).
    /// See `docs/internal/trust-env-devcontainer-ux-plan.md` for the
    /// design context.
    pub plugin_token_areas: std::collections::HashMap<String, (u16, u16, u16)>,
    /// Semantic status-bar model (rendered elements + text + positions), captured
    /// by the renderer so `status_view` derives the web status bar directly
    /// instead of scraping the drawn cells.
    pub segments: Vec<crate::view::ui::status_bar::StatusSegmentInfo>,
}

impl StatusBarChrome {
    /// Screen area `(row, start_col, end_col)` of a given clickable status-bar
    /// segment from the last frame, if it was drawn. Used to anchor popups to
    /// their indicator (e.g. the LSP / remote / read-only menus).
    pub fn clickable_area(
        &self,
        id: crate::view::ui::status_bar::StatusBarClickable,
    ) -> Option<(u16, u16, u16)> {
        self.clickable
            .iter()
            .find(|(cid, _, _, _)| *cid == id)
            .map(|(_, row, start, end)| (*row, *start, *end))
    }
}

impl ChromeLayout {
    /// Reset the cell theme map for a new frame
    pub fn reset_cell_theme_map(&mut self) {
        let total = self.last_frame.width as usize * self.last_frame.height as usize;
        self.cell_theme_map.clear();
        self.cell_theme_map.resize(total, CellThemeInfo::default());
    }

    /// Look up the theme info for a screen position
    pub fn cell_theme_at(&self, col: u16, row: u16) -> Option<&CellThemeInfo> {
        let idx = row as usize * self.last_frame.width as usize + col as usize;
        self.cell_theme_map.get(idx)
    }

    /// Write theme-key runs a chrome renderer captured during paint into the
    /// per-cell map. The runs carry screen coordinates; cells outside the
    /// frame are skipped.
    pub fn apply_theme_runs(&mut self, runs: &[super::theme::ThemeRun]) {
        let width = self.last_frame.width;
        super::theme::apply_theme_runs(&mut self.cell_theme_map, width, runs);
    }
}

/// Per-window layout cache: hit-test rects for content scoped to a
/// single window (split panes, tabs, the file explorer, separators,
/// scrollbars) plus the per-leaf visual-row→source-byte mappings used
/// by mouse positioning and visual-line motion. Lives on `Window`;
/// editor-chrome rects live on [`ChromeLayout`].
#[derive(Debug, Clone, Default)]
pub(crate) struct WindowLayoutCache {
    /// File explorer area (if visible)
    pub file_explorer_area: Option<Rect>,
    /// Editor content area (excluding file explorer)
    pub editor_content_area: Option<Rect>,
    /// Individual split areas with their scrollbar areas and thumb positions
    /// (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end)
    pub split_areas: Vec<(LeafId, BufferId, Rect, Rect, usize, usize)>,
    /// Horizontal scrollbar areas per split
    /// (split_id, buffer_id, horizontal_scrollbar_rect, max_content_width, thumb_start_col, thumb_end_col)
    pub horizontal_scrollbar_areas: Vec<(LeafId, BufferId, Rect, usize, usize, usize)>,
    /// Split separator positions for drag resize
    /// (container_id, direction, x, y, length)
    pub separator_areas: Vec<(ContainerId, SplitDirection, u16, u16, u16)>,
    /// Tab layouts per split for mouse interaction
    pub tab_layouts: HashMap<LeafId, crate::view::ui::tabs::TabLayout>,
    /// Close split button hit areas
    /// (split_id, row, start_col, end_col)
    pub close_split_areas: Vec<(LeafId, u16, u16, u16)>,
    /// Maximize split button hit areas
    /// (split_id, row, start_col, end_col)
    pub maximize_split_areas: Vec<(LeafId, u16, u16, u16)>,
    /// View line mappings for accurate mouse click positioning per split
    /// Maps visual row index to character position mappings
    /// Used to translate screen coordinates to buffer byte positions
    pub view_line_mappings: HashMap<LeafId, Vec<ViewLineMapping>>,
}

impl WindowLayoutCache {
    /// Find which visual row contains the given byte position for a split
    pub fn find_visual_row(&self, split_id: LeafId, byte_pos: usize) -> Option<usize> {
        let mappings = self.view_line_mappings.get(&split_id)?;
        mappings.iter().position(|m| m.contains_byte(byte_pos))
    }

    /// Get the visual column of a byte position within its visual row
    pub fn byte_to_visual_column(&self, split_id: LeafId, byte_pos: usize) -> Option<usize> {
        let mappings = self.view_line_mappings.get(&split_id)?;
        let row_idx = self.find_visual_row(split_id, byte_pos)?;
        let row = mappings.get(row_idx)?;

        // Find the visual column that maps to this byte position
        for (visual_col, &char_idx) in row.visual_to_char.iter().enumerate() {
            if let Some(source_byte) = row.char_source_bytes.get(char_idx).and_then(|b| *b) {
                if source_byte == byte_pos {
                    return Some(visual_col);
                }
                // If we've passed the byte position, return previous column
                if source_byte > byte_pos {
                    return Some(visual_col.saturating_sub(1));
                }
            }
        }
        // Byte is at or past end of row - return the column just after the last
        // *source-backed* cell. Trailing cells that map to no source byte are
        // purely visual (e.g. indentation guides synthesised on a blank line
        // inside an indented block); counting them would push the cursor's
        // column right by one per guide, so a Down onto the next line would
        // land one column too far (issue #2564). On a normal line every cell is
        // source-backed, so this still returns the end-of-line column.
        let last_real_col = row
            .visual_to_char
            .iter()
            .enumerate()
            .rev()
            .find(|(_, &char_idx)| {
                row.char_source_bytes
                    .get(char_idx)
                    .is_some_and(|b| b.is_some())
            })
            .map(|(visual_col, _)| visual_col + 1)
            .unwrap_or(0);
        Some(last_real_col)
    }

    /// Move by visual line using the cached mappings
    /// Returns (new_position, new_visual_column) or None if at boundary
    pub fn move_visual_line(
        &self,
        split_id: LeafId,
        current_pos: usize,
        goal_visual_col: usize,
        direction: i8, // -1 = up, 1 = down
    ) -> Option<(usize, usize)> {
        let mappings = self.view_line_mappings.get(&split_id)?;
        let current_row = self.find_visual_row(split_id, current_pos)?;

        // Walk past purely-virtual rows (e.g. markdown_compose table top/
        // bottom borders and inter-row separators, live-diff deletion
        // virtual lines).  Those rows are plugin-injected and their
        // `line_end_byte` is inherited from the adjacent content row.
        // If MoveDown/MoveUp stopped on them the cursor would land on a
        // byte that's already at the row above's end, which in turn
        // causes Down-after-table to teleport back to an earlier
        // position (regression exposed by markdown_compose's table
        // border feature) or strands the cursor at the previous line's
        // EOL when a live-diff deletion hunk starts with a blank line
        // (regression exposed by the live-diff plugin).
        //
        // A row is "navigable" iff at least one of its visual columns
        // maps to a real source byte.  Skip entirely-virtual rows in
        // the move direction until we hit a navigable one or run off
        // the edge.
        let mut target_row = current_row;
        let navigable = |idx: usize| -> bool {
            mappings
                .get(idx)
                .map(|m| m.char_source_bytes.iter().any(|b| b.is_some()))
                .unwrap_or(false)
        };
        loop {
            target_row = if direction < 0 {
                target_row.checked_sub(1)?
            } else {
                let next = target_row + 1;
                if next >= mappings.len() {
                    return None;
                }
                next
            };
            // Either the next row has real source content, or we've reached
            // a legitimate non-source row that the rest of the editor
            // already treats as a cursor stop (trailing empty line at EOF,
            // implicit blank final line, empty source line between
            // paragraphs).  In either case stop walking.
            if navigable(target_row) {
                break;
            }
            let mapping = mappings.get(target_row)?;
            if mapping.is_plugin_virtual {
                // Plugin-injected virtual row (live-diff deletion lines,
                // markdown_compose table borders, …).  Its
                // `line_end_byte` is inherited from the previous row, so
                // stopping here would strand the cursor at the previous
                // source line's EOL.  Keep walking.
                continue;
            }
            // Empty mapping that isn't plugin-virtual: a real empty
            // source line (paragraph separator), the trailing empty
            // EOF row, or the implicit blank final line.  These are
            // legitimate cursor stops.
            break;
        }

        let target_mapping = mappings.get(target_row)?;

        // Try to get byte at goal visual column.  If the goal column is past
        // the end of visible content, land at line_end_byte (the newline or
        // end of buffer).  If the column exists but has no source byte (e.g.
        // padding on a wrapped continuation line), search outward for the
        // nearest valid source byte at minimal visual distance.
        let new_pos = if goal_visual_col >= target_mapping.visual_to_char.len() {
            target_mapping.line_end_byte
        } else {
            target_mapping
                .source_byte_at_visual_col(goal_visual_col)
                .or_else(|| target_mapping.nearest_source_byte(goal_visual_col))
                .unwrap_or(target_mapping.line_end_byte)
        };

        Some((new_pos, goal_visual_col))
    }

    /// Get the start byte position of the visual row containing the given byte position.
    /// If the cursor is already at the visual row start and this is a wrapped continuation,
    /// moves to the previous visual row's start (within the same logical line).
    /// Get the start byte position of the visual row containing the given byte position.
    /// When `allow_advance` is true and the cursor is already at the row start,
    /// moves to the previous visual row's start.
    pub fn visual_line_start(
        &self,
        split_id: LeafId,
        byte_pos: usize,
        allow_advance: bool,
    ) -> Option<usize> {
        let mappings = self.view_line_mappings.get(&split_id)?;
        let row_idx = self.find_visual_row(split_id, byte_pos)?;
        let row = mappings.get(row_idx)?;
        let row_start = row.first_source_byte()?;

        if allow_advance && byte_pos == row_start && row_idx > 0 {
            let prev_row = mappings.get(row_idx - 1)?;
            prev_row.first_source_byte()
        } else {
            Some(row_start)
        }
    }

    /// Get the end byte position of the visual row containing the given byte position.
    /// If the cursor is already at the visual row end and the next row is a wrapped continuation,
    /// moves to the next visual row's end (within the same logical line).
    /// Get the end byte position of the visual row containing the given byte position.
    /// When `allow_advance` is true and the cursor is already at the row end,
    /// advances to the next visual row's end.
    pub fn visual_line_end(
        &self,
        split_id: LeafId,
        byte_pos: usize,
        allow_advance: bool,
    ) -> Option<usize> {
        let mappings = self.view_line_mappings.get(&split_id)?;
        let row_idx = self.find_visual_row(split_id, byte_pos)?;
        let row = mappings.get(row_idx)?;

        if allow_advance && byte_pos == row.line_end_byte && row_idx + 1 < mappings.len() {
            let next_row = mappings.get(row_idx + 1)?;
            Some(next_row.line_end_byte)
        } else {
            Some(row.line_end_byte)
        }
    }
}

/// Self-contained state for the Live Grep floating overlay's preview
/// pane (issue #1796).
///
/// Owned directly by `Editor::overlay_preview_state` rather than
/// living in `Editor::split_view_states` keyed by a synthetic
/// `LeafId`. This isolation matters because ~20 sites across the
/// editor iterate `split_view_states` for cross-cutting work
/// (workspace save, viewport hooks, settings broadcasts, buffer
/// close cascades). The preview is a *transient render artefact*,
/// not a real split — none of those code paths should see it.
///
/// The phantom buffer is not in `SplitManager`'s tree either, so
/// it's invisible to focus rotation (`Alt+]`/`Alt+[`), tab drag
/// drop zones, hit testing, and `find_leaf_by_role` queries.
#[derive(Debug)]
pub struct OverlayPreviewState {
    /// Buffer currently displayed in the preview pane.
    pub buffer_id: BufferId,
    /// View state (cursor, viewport, folds, view mode, …) used by
    /// the renderer's per-leaf pipeline.
    pub view_state: crate::view::split::SplitViewState,
    /// Buffers we loaded only to feed the preview pane. On overlay
    /// close we close these via the standard `close_buffer` path.
    /// Buffers the user already had open are *not* in this set —
    /// dismissing the overlay never disturbs them.
    pub loaded_buffers: HashSet<BufferId>,
    /// When true, the preview pane renders empty (just its frame). Set
    /// when the current query has no selectable result so a stale match
    /// doesn't keep showing after the result list clears. Kept as a flag
    /// (rather than dropping the whole state) so `loaded_buffers` stays
    /// tracked for cleanup and the buffer can be re-shown on the next
    /// match without reloading.
    pub blanked: bool,
    /// The match byte-offset the preview viewport was last centred on
    /// (issue #2119). The renderer recentres only when this changes (a new
    /// selected result), so a mouse-wheel scroll of the preview isn't undone
    /// by the next frame's recenter.
    pub centered_byte: Option<usize>,
}
