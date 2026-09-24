use super::theme::CellThemeInfo;
use crate::model::event::BufferId;
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

    /// Visual column just past the last *content* cell: the last
    /// source-backed cell whose byte is before `line_end_byte`. The
    /// newline cell (a line-ending indicator or cursor indicator) and
    /// trailing decoration-only cells don't count, so an empty line is 0
    /// whether or not it drew a cell for its newline, and a one-character
    /// line is 1.
    pub fn content_end_col(&self) -> usize {
        self.visual_to_char
            .iter()
            .enumerate()
            .rev()
            .find(|(_, &char_idx)| {
                self.char_source_bytes
                    .get(char_idx)
                    .is_some_and(|b| b.is_some_and(|b| b < self.line_end_byte))
            })
            .map(|(visual_col, _)| visual_col + 1)
            .unwrap_or(0)
    }
}

#[cfg(test)]
mod view_line_mapping_tests {
    use super::ViewLineMapping;

    fn mapping(cells: &[Option<usize>], line_end_byte: usize) -> ViewLineMapping {
        ViewLineMapping {
            char_source_bytes: cells.to_vec(),
            visual_to_char: (0..cells.len()).collect(),
            line_end_byte,
            is_plugin_virtual: false,
            end_exclusive: None,
        }
    }

    /// Issue #3351: a one-character line has one cell and must not read as
    /// empty; an empty line is empty whether or not its newline drew a cell.
    #[test]
    fn content_end_col_counts_content_cells_only() {
        // "}\n" at byte 10: one cell for `}`, the newline draws none.
        assert_eq!(mapping(&[Some(10)], 11).content_end_col(), 1);
        // Empty line at byte 10, newline drawn as an indicator cell.
        assert_eq!(mapping(&[Some(10)], 10).content_end_col(), 0);
        // Empty line, newline draws no cell.
        assert_eq!(mapping(&[], 10).content_end_col(), 0);
        // "ab" followed by decoration cells with no source byte.
        assert_eq!(
            mapping(&[Some(0), Some(1), None, None], 2).content_end_col(),
            2
        );
    }
}

/// What the frame leaves behind for the next one: its size, the screen-indexed
/// cell-theme map, and the suggestion list's window. Per-window geometry is the
/// retained tree's: pane boxes on `Window::pane_rects`, tab rectangles by key
/// (`tabs::rects`), each pane's rows on its `PaneHandle`.
///
/// ## THE paint-recorded (`screen_space`-class) roster — EMPTY
///
/// This was the one enumeration of the parallel geometry path: chrome whose
/// rectangles were recorded at PAINT time rather than derived at event time,
/// because their geometry was a paint product. **No rectangle is recorded here
/// any longer.** `popup_areas` and `global_popup_areas` became keyed nodes
/// (`shell::popup::{rects_of, inner_rects_of}`) — both took the outer rect off
/// the tree and then re-derived the content rect by hand, so they were a second
/// statement of an answer the tree already held. `prompt_toolbar_boxes` was
/// listed here and existed nowhere in the workspace, a roster entry outliving
/// its field. And `suggestions_area` / `suggestions_outer_area` were a copy of
/// `shell::prompt::{suggestions_list_rect, suggestions_rect}` kept for one
/// reader, the web `Scene`, which asks the tree directly now.
///
/// What remains under that name is `suggestions_window`, which is not a
/// rectangle and not a cache: see its own note. ADDING A SURFACE TO THE ROSTER
/// REQUIRES A RULING — event-time derivation is the default, and this class
/// must not grow back surface by surface (`docs/internal/retained-mode-ui.md`,
/// "The keyed geometry index"; the paint-time compositing arc is
/// sinelaw/fresh#3024).
#[derive(Debug, Clone, Default)]
pub(crate) struct ChromeLayout {
    /// The suggestion list's window as the last layout settled it: the first
    /// row shown, and how many.
    ///
    /// **The only thing here that is feedback rather than a cache.** The
    /// description measures the palette's columns against the rows that will
    /// be on screen, and which rows those are is the window the *previous*
    /// layout arrived at — a description reading back its own last frame, and
    /// the one thing about the popup a fresh read of the tree cannot supply
    /// while the tree is being described. The two rectangles that sat beside
    /// it were caches: the web `Scene` was their only reader and it asks
    /// `shell::prompt::{suggestions_rect, suggestions_list_rect}` for them
    /// now, the way it already asked `overlay_prompt::regions_of` for the
    /// card's bands two lines above. The count beside them was
    /// `prompt.suggestions.len()` copied.
    pub suggestions_window: Option<(usize, usize)>,
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
