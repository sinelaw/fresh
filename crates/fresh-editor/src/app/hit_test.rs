//! Pure pointer hit-testing: which UI target is under a screen cell.
//!
//! The mouse counterpart to [`crate::input::router`]. Everything here is a
//! free function over the layout the last render recorded — no `Editor`,
//! no layout caches, no mutation: the view names each rect and slice it
//! reads. `Editor` stays the imperative shell: it supplies
//! the recorded layout and acts on the returned [`HoverTarget`].
//!
//! Why this is worth having as its own layer:
//!
//!   - **The precedence ladder is the product.** "A click on the tab-bar
//!     row where a context menu overlaps belongs to the menu" is a rule,
//!     not a side effect. Ordering bugs here are the ones users report as
//!     "the button doesn't work", and they were previously only reachable
//!     by driving a whole editor. The tests at the bottom of this file
//!     pin the ordering directly.
//!   - **One ladder, not two.** Hover and click both ask "what is under
//!     the pointer?" over the same geometry; keeping the answer in one
//!     place is what stops them drifting (the same reasoning that put the
//!     overlay stack in `app/overlay.rs`).
//!
//! The one probe that cannot live here is the file-explorer *status
//! indicator*: its column depends on the theme, the plugin decoration
//! cache and the explorer renderer's slot layout. The shell resolves that
//! one and passes the result in as [`HoverHitView::explorer_status_indicator`],
//! so the *ordering* still lives here even though that probe's geometry
//! does not.

use ratatui::layout::Rect;

use std::collections::HashMap;

use super::types::{ContextMenu, ContextMenuHit, HoverTarget, PopupAreaLayout};
use crate::model::event::{BufferId, ContainerId, LeafId, SplitDirection};
use crate::view::ui::menu::MenuLayout;
use crate::view::ui::status_bar::{SearchOptionsLayout, StatusBarClickable};
use crate::view::ui::tabs::{TabHit, TabLayout};
use crate::view::ui::FileBrowserLayout;

/// True when `(col, row)` falls inside `rect`.
pub(crate) fn in_rect(col: u16, row: u16, rect: Rect) -> bool {
    col >= rect.x && col < rect.x + rect.width && row >= rect.y && row < rect.y + rect.height
}

/// Exactly the layout the ladder reads — borrowed slices and rects, not
/// whole layout caches.
///
/// Naming each field is deliberate: the shell has to state what it is
/// handing over, a new layout field cannot silently drift into the
/// decision, and tests construct precise values instead of fabricating a
/// blank cache and hoping the zeros are meaningful.
pub(crate) struct HoverHitView<'a> {
    // --- floating overlays (drawn on top of the chrome) ---
    /// The open native context menu (tab / "+" new-tab / file-explorer)
    /// and the frame it is clamped to. All three share one geometry core,
    /// so one hit-test covers them.
    pub context_menu: Option<(&'a ContextMenu, FrameSize)>,
    /// `(inner_rect, scroll_start_idx, visible_count, total_count)` of the
    /// suggestion list (command palette / autocomplete).
    pub suggestions: Option<(Rect, usize, usize, usize)>,
    /// Popup list areas, bottom-most first (the ladder walks them in
    /// reverse so the topmost popup wins).
    pub popups: &'a [PopupAreaLayout],
    /// The file-browser dialog while it is open.
    pub file_browser: Option<FileBrowserHit<'a>>,

    // --- permanent chrome ---
    /// The menu-bar layout, present only while the bar is visible.
    pub menu_bar: Option<&'a MenuLayout>,
    /// The open dropdown's index and layout, if a menu is open.
    pub open_menu: Option<(usize, &'a MenuLayout)>,
    /// The file-explorer panel's area.
    pub file_explorer_area: Option<Rect>,
    /// Pre-resolved file-explorer status-indicator hit (see module docs):
    /// `Some` only when the pointer is actually on an indicator slot.
    pub explorer_status_indicator: Option<HoverTarget>,
    /// `(container, direction, x, y, length)` per split separator.
    pub separators: &'a [(ContainerId, SplitDirection, u16, u16, u16)],
    /// `(split, row, start_col, end_col)` per close-split button.
    pub close_split_buttons: &'a [(LeafId, u16, u16, u16)],
    /// `(split, row, start_col, end_col)` per maximize-split button.
    pub maximize_split_buttons: &'a [(LeafId, u16, u16, u16)],
    /// Tab layouts per split.
    pub tabs: &'a HashMap<LeafId, TabLayout>,
    /// `(split, buffer, content_rect, scrollbar_rect, thumb_start,
    /// thumb_end)` per split — only the scrollbar parts are read here.
    pub split_scrollbars: &'a [(LeafId, BufferId, Rect, Rect, usize, usize)],
    /// The status bar's row, when it is drawn.
    pub status_bar_row: Option<u16>,
    /// `(id, row, start_col, end_col)` per clickable status-bar segment.
    pub status_bar_clickable: &'a [(StatusBarClickable, u16, u16, u16)],
    /// The search-options bar's checkbox layout.
    pub search_options: Option<&'a SearchOptionsLayout>,
}

/// The rendered frame's extent, used to clamp context-menu geometry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct FrameSize {
    pub width: u16,
    pub height: u16,
}

/// The target under `(col, row)`, or `None` when the pointer is over
/// ordinary buffer content (or nothing interactive).
///
/// Floating overlays are tested before permanent chrome because they
/// render on top of it.
pub(crate) fn hover_target(view: &HoverHitView<'_>, col: u16, row: u16) -> Option<HoverTarget> {
    floating_overlay_target(view, col, row).or_else(|| chrome_target(view, col, row))
}

/// Hit-test the floating overlay layers: context menus, the command
/// palette / autocomplete suggestions, popup lists, and the file-browser
/// dialog. These always render on top of the chrome and must be checked
/// first.
pub(crate) fn floating_overlay_target(
    view: &HoverHitView<'_>,
    col: u16,
    row: u16,
) -> Option<HoverTarget> {
    // The native context menus (tab / "+" new-tab / file-explorer) all
    // render on top and share one geometry core, so a single hit-test
    // over the open menu covers all three. An interior (item) row yields
    // a hover target; border rows and outside positions fall through to
    // the chrome below.
    if let Some((core, frame)) = view.context_menu {
        if let ContextMenuHit::Item(item_idx) = core.hit(col, row, frame.width, frame.height) {
            return Some(HoverTarget::ContextMenuItem(item_idx));
        }
    }

    // Suggestions (command palette, autocomplete).
    if let Some((inner_rect, start_idx, _visible_count, total_count)) = view.suggestions {
        if in_rect(col, row, inner_rect) {
            let relative_row = (row - inner_rect.y) as usize;
            let item_idx = start_idx + relative_row;
            if item_idx < total_count {
                return Some(HoverTarget::SuggestionItem(item_idx));
            }
        }
    }

    // Popups, topmost first (the last one drawn is on top).
    for (popup_idx, _popup_rect, inner_rect, scroll_offset, num_items, _, _) in
        view.popups.iter().rev()
    {
        if in_rect(col, row, *inner_rect) && *num_items > 0 {
            let relative_row = (row - inner_rect.y) as usize;
            let item_idx = scroll_offset + relative_row;
            if item_idx < *num_items {
                return Some(HoverTarget::PopupListItem(*popup_idx, item_idx));
            }
        }
    }

    // File-browser dialog.
    if let Some(fb) = view.file_browser.as_ref() {
        if let Some(hover) = file_browser_target(fb, col, row) {
            return Some(hover);
        }
    }

    None
}

/// Hit-test the permanent chrome: menu bar, file explorer panel, split
/// separators, split controls, tabs, scrollbars, status bar and search
/// options. Called only after floating overlays have been ruled out.
pub(crate) fn chrome_target(view: &HoverHitView<'_>, col: u16, row: u16) -> Option<HoverTarget> {
    // Menu bar (only clickable while visible).
    if let Some(menu_layout) = view.menu_bar {
        if let Some(menu_idx) = menu_layout.menu_at(col, row) {
            return Some(HoverTarget::MenuBarItem(menu_idx));
        }
    }

    // Open dropdown (and any nested submenus).
    if let Some((active_idx, menu_layout)) = view.open_menu {
        if let Some(hover) = menu_dropdown_target(menu_layout, active_idx, col, row) {
            return Some(hover);
        }
    }

    // File explorer: close button, then the (pre-resolved) status
    // indicator, then the resize border.
    if let Some(explorer_area) = view.file_explorer_area {
        let close_button_x = explorer_area.x + explorer_area.width.saturating_sub(3);
        if row == explorer_area.y
            && col >= close_button_x
            && col < explorer_area.x + explorer_area.width
        {
            return Some(HoverTarget::FileExplorerCloseButton);
        }

        // Renderer/theme-dependent probe, resolved by the shell — see the
        // module docs. It sits here so the *ordering* (after the close
        // button, before the border) stays in this ladder.
        if let Some(indicator) = view.explorer_status_indicator.clone() {
            return Some(indicator);
        }

        // The border is the rightmost drawn column of the explorer area,
        // not one past it.
        let border_x = explorer_area.x + explorer_area.width.saturating_sub(1);
        if col == border_x && row >= explorer_area.y && row < explorer_area.y + explorer_area.height
        {
            return Some(HoverTarget::FileExplorerBorder);
        }
    }

    // Split separators.
    for (split_id, direction, sep_x, sep_y, sep_length) in view.separators {
        let is_on_separator = match direction {
            SplitDirection::Horizontal => {
                row == *sep_y && col >= *sep_x && col < sep_x + sep_length
            }
            SplitDirection::Vertical => col == *sep_x && row >= *sep_y && row < sep_y + sep_length,
        };
        if is_on_separator {
            return Some(HoverTarget::SplitSeparator(*split_id, *direction));
        }
    }

    // Split control buttons sit on top of the tab row, so they win over
    // the tab hit-test below.
    for (split_id, btn_row, start_col, end_col) in view.close_split_buttons {
        if row == *btn_row && col >= *start_col && col < *end_col {
            return Some(HoverTarget::CloseSplitButton(*split_id));
        }
    }
    for (split_id, btn_row, start_col, end_col) in view.maximize_split_buttons {
        if row == *btn_row && col >= *start_col && col < *end_col {
            return Some(HoverTarget::MaximizeSplitButton(*split_id));
        }
    }

    // Tabs.
    for (split_id, tab_layout) in view.tabs {
        match tab_layout.hit_test(col, row) {
            Some(TabHit::CloseButton(target)) => {
                return Some(HoverTarget::TabCloseButton(target, *split_id));
            }
            Some(TabHit::TabName(target)) => {
                return Some(HoverTarget::TabName(target, *split_id));
            }
            Some(TabHit::ScrollLeft)
            | Some(TabHit::ScrollRight)
            | Some(TabHit::BarBackground)
            | Some(TabHit::NewTabButton)
            | None => {}
        }
    }

    // Vertical scrollbars: thumb vs track.
    for (split_id, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
        view.split_scrollbars
    {
        if in_rect(col, row, *scrollbar_rect) {
            let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
            let is_on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;
            return Some(if is_on_thumb {
                HoverTarget::ScrollbarThumb(*split_id)
            } else {
                HoverTarget::ScrollbarTrack(*split_id, relative_row as u16)
            });
        }
    }

    // Status bar: one generic hit-test over every clickable segment
    // recorded last frame (encoding, LSP, remote, …).
    if let Some(status_row) = view.status_bar_row {
        if row == status_row {
            for (id, indicator_row, start, end) in view.status_bar_clickable {
                if row == *indicator_row && col >= *start && col < *end {
                    return Some(HoverTarget::StatusBarClickable(*id));
                }
            }
        }
    }

    // Search-options bar checkboxes.
    if let Some(layout) = view.search_options {
        use crate::view::ui::status_bar::SearchOptionsHover;
        if let Some(hover) = layout.checkbox_at(col, row) {
            return Some(match hover {
                SearchOptionsHover::CaseSensitive => HoverTarget::SearchOptionCaseSensitive,
                SearchOptionsHover::WholeWord => HoverTarget::SearchOptionWholeWord,
                SearchOptionsHover::Regex => HoverTarget::SearchOptionRegex,
                SearchOptionsHover::ConfirmEach => HoverTarget::SearchOptionConfirmEach,
                SearchOptionsHover::None => return None,
            });
        }
    }

    None
}

/// Hit-test an open menu dropdown and its submenu chain. Submenus render
/// on top of their parent, so they are checked first.
pub(crate) fn menu_dropdown_target(
    menu_layout: &MenuLayout,
    menu_index: usize,
    col: u16,
    row: u16,
) -> Option<HoverTarget> {
    if let Some((depth, item_idx)) = menu_layout.submenu_item_at(col, row) {
        return Some(HoverTarget::SubmenuItem(depth, item_idx));
    }
    if let Some(item_idx) = menu_layout.item_at(col, row) {
        return Some(HoverTarget::MenuDropdownItem(menu_index, item_idx));
    }
    None
}

/// The file-browser dialog's layout plus the list state its row
/// hit-test needs. `scroll_offset` and `entry_count` come from
/// `file_open_state`, which is not layout data — passing them keeps the
/// row probe pure rather than reaching back into the editor.
pub(crate) struct FileBrowserHit<'a> {
    pub layout: &'a FileBrowserLayout,
    pub scroll_offset: usize,
    pub entry_count: usize,
}

/// Hit-test the file-browser dialog. The checkboxes are checked before
/// the navigation shortcuts because they overlap that band.
pub(crate) fn file_browser_target(fb: &FileBrowserHit<'_>, x: u16, y: u16) -> Option<HoverTarget> {
    let layout = fb.layout;
    if layout.is_on_show_hidden_checkbox(x, y) {
        return Some(HoverTarget::FileBrowserShowHiddenCheckbox);
    }
    if layout.is_on_detect_encoding_checkbox(x, y) {
        return Some(HoverTarget::FileBrowserDetectEncodingCheckbox);
    }
    if layout.is_in_nav(x, y) {
        if let Some(idx) = layout.nav_shortcut_at(x, y) {
            return Some(HoverTarget::FileBrowserNavShortcut(idx));
        }
    }
    if layout.is_in_header(x, y) {
        if let Some(mode) = layout.header_column_at(x) {
            return Some(HoverTarget::FileBrowserHeader(mode));
        }
    }
    // File list rows: `click_to_index` maps a screen row to an index in
    // the scrolled list; entries past the end are empty space.
    if layout.is_in_list(x, y) {
        if let Some(idx) = layout.click_to_index(y, fb.scroll_offset) {
            if idx < fb.entry_count {
                return Some(HoverTarget::FileBrowserEntry(idx));
            }
        }
    }
    if layout.is_in_scrollbar(x, y) {
        return Some(HoverTarget::FileBrowserScrollbar);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::SplitId;
    use crate::view::ui::status_bar::StatusBarClickable;

    fn leaf(n: usize) -> LeafId {
        LeafId(SplitId(n))
    }

    fn rect(x: u16, y: u16, width: u16, height: u16) -> Rect {
        Rect {
            x,
            y,
            width,
            height,
        }
    }

    /// Storage the borrowed view points at. Every field is named at the
    /// construction site below, so adding a field to [`HoverHitView`]
    /// fails to compile here rather than silently defaulting to "absent".
    struct Fixture {
        popups: Vec<PopupAreaLayout>,
        separators: Vec<(ContainerId, SplitDirection, u16, u16, u16)>,
        close_split_buttons: Vec<(LeafId, u16, u16, u16)>,
        maximize_split_buttons: Vec<(LeafId, u16, u16, u16)>,
        tabs: HashMap<LeafId, TabLayout>,
        split_scrollbars: Vec<(LeafId, BufferId, Rect, Rect, usize, usize)>,
        status_bar_clickable: Vec<(StatusBarClickable, u16, u16, u16)>,
    }

    impl Fixture {
        /// Nothing on screen. Tests add only the elements they exercise.
        fn empty() -> Self {
            Self {
                popups: Vec::new(),
                separators: Vec::new(),
                close_split_buttons: Vec::new(),
                maximize_split_buttons: Vec::new(),
                tabs: HashMap::new(),
                split_scrollbars: Vec::new(),
                status_bar_clickable: Vec::new(),
            }
        }

        fn view(&self) -> HoverHitView<'_> {
            HoverHitView {
                context_menu: None,
                suggestions: None,
                popups: &self.popups,
                file_browser: None,
                menu_bar: None,
                open_menu: None,
                file_explorer_area: None,
                explorer_status_indicator: None,
                separators: &self.separators,
                close_split_buttons: &self.close_split_buttons,
                maximize_split_buttons: &self.maximize_split_buttons,
                tabs: &self.tabs,
                split_scrollbars: &self.split_scrollbars,
                status_bar_row: None,
                status_bar_clickable: &self.status_bar_clickable,
                search_options: None,
            }
        }
    }

    /// A suggestion popup drawn over the tab row wins: floating overlays
    /// are painted on top of the chrome, so the pointer belongs to them.
    /// This is the precedence rule that used to live in two separate
    /// ladders and could drift between them.
    #[test]
    fn floating_overlay_beats_chrome_underneath() {
        let mut fx = Fixture::empty();
        // A close-split button occupying the same cell as a suggestion row.
        fx.close_split_buttons.push((leaf(1), 5, 10, 20));

        let mut v = fx.view();
        v.suggestions = Some((rect(0, 5, 40, 3), 0, 3, 3));
        assert_eq!(
            hover_target(&v, 12, 5),
            Some(HoverTarget::SuggestionItem(0)),
            "the overlay drawn on top must win over the chrome button beneath it"
        );

        // With no overlay, the same cell resolves to the chrome button.
        assert_eq!(
            hover_target(&fx.view(), 12, 5),
            Some(HoverTarget::CloseSplitButton(leaf(1)))
        );
    }

    /// Suggestion rows are offset by the list's scroll position, and a row
    /// past the end of the list is empty space (falls through).
    #[test]
    fn suggestion_row_accounts_for_scroll_and_end_of_list() {
        let fx = Fixture::empty();
        let mut v = fx.view();
        // Viewport of 3 rows scrolled down by 5, over a 7-entry list.
        v.suggestions = Some((rect(0, 10, 30, 3), 5, 3, 7));

        assert_eq!(
            hover_target(&v, 4, 10),
            Some(HoverTarget::SuggestionItem(5))
        );
        assert_eq!(
            hover_target(&v, 4, 11),
            Some(HoverTarget::SuggestionItem(6))
        );
        // Row 12 would be index 7 — past the 7 entries, so nothing.
        assert_eq!(hover_target(&v, 4, 12), None);
    }

    /// The topmost popup wins where two overlap (the stack is walked in
    /// reverse, because the last one drawn is on top).
    #[test]
    fn topmost_popup_wins_when_popups_overlap() {
        let mut fx = Fixture::empty();
        let area = rect(0, 0, 20, 4);
        fx.popups.push((0, area, area, 0, 4, None, 0));
        fx.popups.push((1, area, area, 0, 4, None, 0));

        assert_eq!(
            hover_target(&fx.view(), 3, 2),
            Some(HoverTarget::PopupListItem(1, 2)),
            "the later (topmost) popup owns the cell"
        );
    }

    /// Within the explorer the close button is checked before the resize
    /// border, and the border is the rightmost *drawn* column.
    #[test]
    fn explorer_close_button_beats_border_and_border_is_last_column() {
        let fx = Fixture::empty();
        let mut v = fx.view();
        v.file_explorer_area = Some(rect(0, 0, 30, 10));

        // Close button occupies the last 3 columns of the title row.
        assert_eq!(
            hover_target(&v, 28, 0),
            Some(HoverTarget::FileExplorerCloseButton)
        );
        // The border is column 29 (x + width - 1) on a non-title row.
        assert_eq!(
            hover_target(&v, 29, 5),
            Some(HoverTarget::FileExplorerBorder)
        );
        // One past the panel is outside it entirely.
        assert_eq!(hover_target(&v, 30, 5), None);
    }

    /// The pre-resolved explorer status indicator sits between the close
    /// button and the border — the shell computes its geometry, but this
    /// ladder owns where it ranks.
    #[test]
    fn explorer_status_indicator_ranks_between_close_button_and_border() {
        let fx = Fixture::empty();
        let indicator = HoverTarget::FileExplorerStatusIndicator("/x".into());
        let mut v = fx.view();
        v.file_explorer_area = Some(rect(0, 0, 30, 10));
        v.explorer_status_indicator = Some(indicator.clone());

        // It beats the border…
        assert_eq!(hover_target(&v, 29, 5), Some(indicator));
        // …but not the close button.
        assert_eq!(
            hover_target(&v, 28, 0),
            Some(HoverTarget::FileExplorerCloseButton)
        );
    }

    /// Split control buttons are drawn over the tab row, so they win
    /// against a tab occupying the same cell.
    #[test]
    fn split_controls_are_hit_on_the_tab_row() {
        let mut fx = Fixture::empty();
        fx.close_split_buttons.push((leaf(2), 0, 70, 73));
        fx.maximize_split_buttons.push((leaf(2), 0, 74, 77));

        let v = fx.view();
        assert_eq!(
            hover_target(&v, 71, 0),
            Some(HoverTarget::CloseSplitButton(leaf(2)))
        );
        assert_eq!(
            hover_target(&v, 75, 0),
            Some(HoverTarget::MaximizeSplitButton(leaf(2)))
        );
        // The gap between the two buttons belongs to neither.
        assert_eq!(hover_target(&v, 73, 0), None);
    }

    /// A vertical scrollbar distinguishes thumb from track, and the track
    /// carries the row offset the click-to-jump math needs.
    #[test]
    fn scrollbar_thumb_and_track_are_distinguished() {
        let mut fx = Fixture::empty();
        fx.split_scrollbars.push((
            leaf(3),
            BufferId(0),
            rect(0, 1, 79, 20),
            rect(79, 1, 1, 20),
            5,
            9,
        ));

        let v = fx.view();
        // Screen row 7 is relative row 6 — inside the thumb's 5..9.
        assert_eq!(
            hover_target(&v, 79, 7),
            Some(HoverTarget::ScrollbarThumb(leaf(3)))
        );
        // Screen row 2 is relative row 1 — above the thumb, so track.
        assert_eq!(
            hover_target(&v, 79, 2),
            Some(HoverTarget::ScrollbarTrack(leaf(3), 1))
        );
    }

    /// Separators are one cell thick along their length; the direction
    /// decides which axis is pinned.
    #[test]
    fn separator_orientation_decides_the_pinned_axis() {
        let mut fx = Fixture::empty();
        let container = ContainerId(SplitId(9));
        fx.separators
            .push((container, SplitDirection::Vertical, 40, 2, 10));

        let v = fx.view();
        // Vertical separator: fixed column 40, rows 2..12.
        assert_eq!(
            hover_target(&v, 40, 6),
            Some(HoverTarget::SplitSeparator(
                container,
                SplitDirection::Vertical
            ))
        );
        assert_eq!(
            hover_target(&v, 41, 6),
            None,
            "one column over is not the separator"
        );
        assert_eq!(
            hover_target(&v, 40, 12),
            None,
            "past the separator's length"
        );
    }

    /// Status-bar segments only match on their own row and column span.
    #[test]
    fn status_bar_segment_matches_its_own_span() {
        let mut fx = Fixture::empty();
        fx.status_bar_clickable
            .push((StatusBarClickable::Encoding, 23, 10, 18));

        let mut v = fx.view();
        v.status_bar_row = Some(23);

        assert_eq!(
            hover_target(&v, 12, 23),
            Some(HoverTarget::StatusBarClickable(
                StatusBarClickable::Encoding
            ))
        );
        assert_eq!(hover_target(&v, 18, 23), None, "end column is exclusive");
        assert_eq!(hover_target(&v, 12, 22), None, "wrong row");
    }

    /// Nothing under the pointer means nothing claimed — the caller then
    /// treats the cell as buffer content.
    #[test]
    fn empty_layout_claims_nothing() {
        let fx = Fixture::empty();
        assert_eq!(hover_target(&fx.view(), 5, 5), None);
    }
}
