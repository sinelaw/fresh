use super::theme::CellThemeInfo;
use crate::model::event::BufferId;
use ratatui::layout::Rect;
use std::collections::HashSet;

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
    /// One byte past the last character this row drew, when the row ends on a
    /// content character rather than on a separator. `None` when
    /// `line_end_byte` is already the row's end position — a row ending at its
    /// line ending, or at the whitespace a wrap consumed.
    ///
    /// A compose-mode soft break consumes the space it broke on, so the rows it
    /// wraps end on a content character and the position past it is carried by
    /// no cell. It still belongs to this row: it is where `End` goes and where
    /// the caret is drawn (`row_end_exclusive` in `render_line`). Without it
    /// the row below claims the byte, and a `Home` or `Up` after `End` acts on
    /// the wrong row.
    pub end_exclusive: Option<usize>,
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
        } else if self.is_plugin_virtual {
            // A plugin-injected row owns no byte of its own: its
            // `line_end_byte` was inherited from the row above (0 when it is
            // the first row on screen). Claiming that byte hides the real row
            // that owns it from `find_visual_row`, which returns the *first*
            // match — and every caller of that lookup is asking "which row is
            // the cursor drawn on", never "which row was injected here".
            //
            // A git-blame header above the buffer's first line is the case
            // where the two answers differ: it inherits `line_end_byte == 0`,
            // so a cursor at byte 0 resolved to the header row, and MoveDown
            // stepped from it onto line 1 — byte 0 again. Down did nothing
            // until the cursor was moved off the start of the buffer some
            // other way.
            false
        } else {
            // Empty row with no injected content (trailing line past the final
            // newline, blank source line) - only matches at line_end_byte.
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
/// indexed cell-theme map. Per-window geometry is the retained tree's:
/// pane boxes on `Window::pane_rects`, tab rectangles by key
/// (`tabs::rects`), each pane's rows on its `PaneHandle`.
///
/// ## THE paint-recorded (`screen_space`-class) roster — CLOSED LIST
///
/// Most chrome geometry is derived at event time from live state
/// (slice 7); the surfaces below are the ruled exceptions whose rects
/// are recorded at PAINT time because their geometry is a paint
/// product (content-measured popups, dialog layout math), each with
/// standing debug parity or documented rationale at its site:
///
///   - `popup_areas` / `global_popup_areas` (info/message popups)
///   - `suggestions_area` / `suggestions_outer_area` (the prompt's
///     suggestion list, both forms)
///   - `prompt_toolbar_boxes` (overlay toolbar box tree, in the
///     toolbar band's own coordinates — the tree gesture reports the
///     press in that space, so no origin travels with it)
///
/// This list is the ONE enumeration of the parallel geometry path
/// (recorded by ruling; `docs/internal/retained-mode-ui.md` §3.7 retires it).
/// ADDING A SURFACE HERE REQUIRES A RULING — the
/// event-time derivation is the default, and this class must not
/// grow surface by surface without one; retiring it entirely is the
/// paint-time compositing arc (sinelaw/fresh#3024).
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
