//! Tab bar rendering for multiple buffers

use crate::app::types::CellThemeRecorder;
use crate::app::BufferMetadata;
use crate::model::event::{BufferId, LeafId};
use crate::primitives::display_width::str_width;
use crate::state::EditorState;
use crate::view::split::TabTarget;
use crate::view::ui::layout::point_in_rect;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Widget;
use ratatui::widgets::{Block, Paragraph};
use rust_i18n::t;
use std::collections::HashMap;

/// Returns true iff `t` is the editor's single preview tab. `preview_buffer`
/// is `window.preview`'s buffer id (the source of truth); groups are never
/// previews.
fn is_preview_tab(t: &TabTarget, preview_buffer: Option<BufferId>) -> bool {
    matches!(t, TabTarget::Buffer(id) if Some(*id) == preview_buffer)
}

/// Returns the preview-suffix string (leading space included) to append
/// to a preview tab's label, or an empty string if the tab is not a preview.
fn preview_suffix(t: &TabTarget, preview_buffer: Option<BufferId>) -> String {
    if is_preview_tab(t, preview_buffer) {
        format!(" {}", t!("buffer.preview_indicator"))
    } else {
        String::new()
    }
}

/// Hit area for a single tab
#[derive(Debug, Clone)]
pub struct TabHitArea {
    /// The tab target this tab represents (buffer or group)
    pub target: TabTarget,
    /// The resolved, disambiguated display label (the filename for file
    /// buffers, the group name for groups) — the *same* string the TUI draws
    /// and the tab width was computed from. Non-cell frontends (the web) render
    /// this directly so they can't diverge from the terminal on tab text.
    pub label: String,
    /// The area covering the tab name (clickable to switch to the target)
    pub tab_area: Rect,
    /// The area covering the close button
    pub close_area: Rect,
}

impl TabHitArea {
    /// Backwards-compatible access: returns the buffer id if this is a buffer tab.
    pub fn buffer_id(&self) -> Option<BufferId> {
        self.target.as_buffer()
    }
}

/// Layout information for hit testing tab interactions
///
/// Returned by `TabsRenderer::render_for_split()` to enable mouse hit testing
/// without duplicating position calculations.
#[derive(Debug, Clone, Default)]
pub struct TabLayout {
    /// Hit areas for each visible tab
    pub tabs: Vec<TabHitArea>,
    /// The full tab bar area
    pub bar_area: Rect,
    /// Hit area for the left scroll button (if shown)
    pub left_scroll_area: Option<Rect>,
    /// Hit area for the right scroll button (if shown)
    pub right_scroll_area: Option<Rect>,
    /// Hit area for the trailing "+" new-tab button (if visible)
    pub new_tab_area: Option<Rect>,
}

/// Hit test result for tab interactions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TabHit {
    /// Hit the tab name area (click to switch to this target)
    TabName(TabTarget),
    /// Hit the close button area
    CloseButton(TabTarget),
    /// Hit the tab bar background
    BarBackground,
    /// Hit the left scroll button
    ScrollLeft,
    /// Hit the right scroll button
    ScrollRight,
    /// Hit the trailing "+" new-tab button
    NewTabButton,
}

impl TabLayout {
    /// Create a new empty layout
    pub fn new(bar_area: Rect) -> Self {
        Self {
            tabs: Vec::new(),
            bar_area,
            left_scroll_area: None,
            right_scroll_area: None,
            new_tab_area: None,
        }
    }

    /// Perform a hit test to determine what element is at the given position
    pub fn hit_test(&self, x: u16, y: u16) -> Option<TabHit> {
        // Check scroll buttons first (they're at the edges)
        if let Some(left_area) = self.left_scroll_area {
            tracing::debug!(
                "Tab hit_test: checking left_scroll_area {:?} against ({}, {})",
                left_area,
                x,
                y
            );
            if point_in_rect(left_area, x, y) {
                tracing::debug!("Tab hit_test: HIT ScrollLeft");
                return Some(TabHit::ScrollLeft);
            }
        }
        if let Some(right_area) = self.right_scroll_area {
            tracing::debug!(
                "Tab hit_test: checking right_scroll_area {:?} against ({}, {})",
                right_area,
                x,
                y
            );
            if point_in_rect(right_area, x, y) {
                tracing::debug!("Tab hit_test: HIT ScrollRight");
                return Some(TabHit::ScrollRight);
            }
        }

        for tab in &self.tabs {
            // Check close button first (it's inside the tab area)
            if point_in_rect(tab.close_area, x, y) {
                return Some(TabHit::CloseButton(tab.target));
            }
            // Check tab area
            if point_in_rect(tab.tab_area, x, y) {
                return Some(TabHit::TabName(tab.target));
            }
        }

        // Check the trailing "+" new-tab button
        if let Some(new_tab_area) = self.new_tab_area {
            if point_in_rect(new_tab_area, x, y) {
                return Some(TabHit::NewTabButton);
            }
        }

        // Check bar background
        if point_in_rect(self.bar_area, x, y) {
            return Some(TabHit::BarBackground);
        }

        None
    }
}

/// Renders the tab bar showing open buffers
pub struct TabsRenderer;

/// The trailing "+" new-tab button cell text.
const NEW_TAB_BUTTON_TEXT: &str = " + ";
/// Display width (columns) of [`NEW_TAB_BUTTON_TEXT`].
pub const NEW_TAB_BUTTON_WIDTH: usize = 3;

/// Glyph drawn at the left edge when earlier tabs are scrolled off.
const SCROLL_INDICATOR_LEFT: &str = "<";
/// Glyph drawn at the right edge when later tabs are scrolled off.
const SCROLL_INDICATOR_RIGHT: &str = ">";
/// Column width of either scroll indicator.
const SCROLL_INDICATOR_WIDTH: usize = 1;

/// Width available for laying out / scrolling the real tabs, given the total
/// width of all tabs (including inter-tab separators) and the full tab-bar
/// width.
///
/// When the tabs plus an inline "+" button fit, the "+" is rendered inline
/// right after the last tab and the full bar width is available. When they
/// overflow, the "+" is pinned to the right edge of the bar and its column is
/// reserved here, so the tabs scroll within the remaining width and never slip
/// underneath the pinned button.
pub fn tabs_render_width(tabs_total: usize, bar_width: usize) -> usize {
    let sep_before_plus = if tabs_total > 0 { 1 } else { 0 };
    let inline_total = tabs_total + sep_before_plus + NEW_TAB_BUTTON_WIDTH;
    if inline_total > bar_width && bar_width > NEW_TAB_BUTTON_WIDTH {
        bar_width - NEW_TAB_BUTTON_WIDTH
    } else {
        bar_width
    }
}

/// Compute the scroll offset that brings the active tab into view with the
/// **least** movement from the current offset.
///
/// This is a plain scroll-into-view: if the active tab is already fully
/// visible the offset is left untouched, so activating a tab never yanks the
/// bar around. Only when the tab sits past an edge do we scroll — just far
/// enough to reveal it against that edge (its start against the left edge, or
/// its end against the right edge), never re-centering it.
///
/// `tab_widths` includes the 1-column separators between tabs. `current_offset`
/// is the split's live `tab_scroll_offset`.
pub fn scroll_to_show_tab(
    tab_widths: &[usize],
    active_idx: usize,
    current_offset: usize,
    max_width: usize,
) -> usize {
    if tab_widths.is_empty() || max_width == 0 || active_idx >= tab_widths.len() {
        return 0;
    }

    let total_width: usize = tab_widths.iter().sum();
    let tab_start: usize = tab_widths[..active_idx].iter().sum();
    let tab_width = tab_widths[active_idx];
    let tab_end = tab_start + tab_width;

    // Everything fits — nothing to scroll, park at the origin.
    if total_width <= max_width {
        return 0;
    }

    // Furthest we can scroll: at the right end a "<" indicator eats one column,
    // so only max_width-1 content columns remain visible there.
    let max_offset = total_width.saturating_sub(max_width.saturating_sub(1));

    // Visible content window for a candidate offset, reserving columns for the
    // scroll indicators the renderer will actually draw: a "<" when offset > 0,
    // a ">" when content extends past the right edge.
    let visible = |off: usize| -> (usize, usize) {
        let show_left = off > 0;
        let show_right = total_width.saturating_sub(off) > max_width;
        let available = max_width
            .saturating_sub(show_left as usize)
            .saturating_sub(show_right as usize);
        (off, off + available)
    };

    let offset = current_offset.min(max_offset);
    let (vis_start, vis_end) = visible(offset);

    let result = if tab_start >= vis_start && tab_end <= vis_end {
        // Already fully on screen — don't move at all.
        offset
    } else if tab_start < vis_start {
        // Off the left edge: reveal the tab start against the left edge.
        tab_start.min(max_offset)
    } else {
        // Off the right edge: align the tab end with the right edge. Reserve
        // both indicators (worst case) so the tab can't be clipped by an
        // indicator that appears at the new offset. This sidesteps the circular
        // dependency between the offset and which indicators are shown.
        let available_worst = max_width.saturating_sub(2);
        tab_end.saturating_sub(available_worst).min(max_offset)
    };

    tracing::debug!(
        "scroll_to_show_tab: idx={}, tab={}..{}, cur={}, result={}, total={}, max_width={}, max_offset={}",
        active_idx, tab_start, tab_end, current_offset, result, total_width, max_width, max_offset
    );
    result
}

/// Resolve display names for tab targets, disambiguating duplicates by appending a number.
/// For example, if there are three unnamed buffers, they become "[No Name]", "[No Name] 2", "[No Name] 3".
/// Similarly, duplicate filenames get numbered: "main.rs", "main.rs 2".
///
/// `group_names` provides the display name for each group tab (`TabTarget::Group`).
fn resolve_tab_names(
    tab_targets: &[TabTarget],
    buffers: &HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, BufferMetadata>,
    composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    group_names: &HashMap<LeafId, String>,
) -> HashMap<TabTarget, String> {
    let mut names: Vec<(TabTarget, String)> = Vec::new();

    for t in tab_targets.iter() {
        match t {
            TabTarget::Buffer(id) => {
                let is_regular_buffer = buffers.contains_key(id);
                let is_composite_buffer = composite_buffers.contains_key(id);
                if !is_regular_buffer && !is_composite_buffer {
                    continue;
                }
                if let Some(meta) = buffer_metadata.get(id) {
                    if meta.hidden_from_tabs {
                        continue;
                    }
                }

                let meta = buffer_metadata.get(id);
                let is_terminal = meta
                    .and_then(|m| m.virtual_mode())
                    .map(|mode| mode == "terminal")
                    .unwrap_or(false);

                let name = if is_composite_buffer {
                    meta.map(|m| m.display_name.as_str())
                } else if is_terminal {
                    meta.map(|m| m.display_name.as_str())
                } else {
                    buffers
                        .get(id)
                        .and_then(|state| state.buffer.file_path())
                        .and_then(|p| p.file_name())
                        .and_then(|n| n.to_str())
                        .or_else(|| meta.map(|m| m.display_name.as_str()))
                }
                .unwrap_or("[No Name]");

                names.push((*t, name.to_string()));
            }
            TabTarget::Group(leaf_id) => {
                if let Some(name) = group_names.get(leaf_id) {
                    names.push((*t, name.clone()));
                }
            }
        }
    }

    // Count occurrences of each name
    let mut name_counts: HashMap<&str, usize> = HashMap::new();
    for (_, name) in &names {
        *name_counts.entry(name.as_str()).or_insert(0) += 1;
    }

    // Assign disambiguated names — all duplicates get a number, including the first
    let mut result = HashMap::new();
    let mut name_indices: HashMap<String, usize> = HashMap::new();
    for (t, name) in &names {
        if name_counts.get(name.as_str()).copied().unwrap_or(0) > 1 {
            let idx = name_indices.entry(name.clone()).or_insert(0);
            *idx += 1;
            result.insert(*t, format!("{} {}", name, idx));
        } else {
            result.insert(*t, name.clone());
        }
    }

    result
}

/// Calculate tab widths for scroll offset calculations.
/// Returns (tab_widths, rendered_targets) where tab_widths includes separators.
/// This uses the same logic as render_for_split to ensure consistency.
pub fn calculate_tab_widths(
    tab_targets: &[TabTarget],
    buffers: &HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, BufferMetadata>,
    composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    group_names: &HashMap<LeafId, String>,
    preview_buffer: Option<BufferId>,
) -> (Vec<usize>, Vec<TabTarget>) {
    let mut tab_widths: Vec<usize> = Vec::new();
    let mut rendered_targets: Vec<TabTarget> = Vec::new();
    let resolved_names = resolve_tab_names(
        tab_targets,
        buffers,
        buffer_metadata,
        composite_buffers,
        group_names,
    );

    for t in tab_targets.iter() {
        // Skip targets we couldn't resolve a name for (hidden, missing, etc.)
        let Some(name) = resolved_names.get(t) else {
            continue;
        };

        // Calculate modified indicator (groups and composite buffers don't show it)
        let modified = match t {
            TabTarget::Buffer(id) => {
                if composite_buffers.contains_key(id) {
                    ""
                } else if let Some(state) = buffers.get(id) {
                    if state.buffer.is_modified() {
                        "*"
                    } else {
                        ""
                    }
                } else {
                    ""
                }
            }
            TabTarget::Group(_) => "",
        };

        let binary_indicator = match t {
            TabTarget::Buffer(id) => {
                if buffer_metadata.get(id).map(|m| m.binary).unwrap_or(false) {
                    " [BIN]"
                } else {
                    ""
                }
            }
            TabTarget::Group(_) => "",
        };

        let preview_indicator = preview_suffix(t, preview_buffer);

        // Same format as render_for_split: " {name}{modified}{preview_indicator}{binary_indicator} " + "× "
        let tab_name_text = format!(" {name}{modified}{preview_indicator}{binary_indicator} ");
        let close_text = "× ";
        let tab_width = str_width(&tab_name_text) + str_width(close_text);

        // Add separator if not first tab
        if !rendered_targets.is_empty() {
            tab_widths.push(1); // separator
        }

        tab_widths.push(tab_width);
        rendered_targets.push(*t);
    }

    (tab_widths, rendered_targets)
}

/// Compute the (name, close-button) styles for one tab from its state flags.
///
/// For the inactive split's active tab we keep BOLD to show which tab is
/// active inside that split, but use `tab_inactive_fg` instead of
/// `tab_active_fg`: pairing `tab_active_fg` with `tab_inactive_bg` assumes
/// `active_fg` was chosen against `active_bg`, which breaks on themes (e.g.
/// high-contrast) where `active_fg == inactive_bg` and the label disappears.
fn tab_styles(
    is_active: bool,
    is_active_split: bool,
    is_hovered_name: bool,
    is_hovered_close: bool,
    is_preview: bool,
    theme: &crate::view::theme::Theme,
) -> (Style, Style) {
    let mut base_style = if is_active {
        let fg = if is_active_split {
            theme.tab_active_fg
        } else {
            theme.tab_inactive_fg
        };
        let bg = if is_active_split {
            theme.tab_active_bg
        } else {
            theme.tab_inactive_bg
        };
        Style::default().fg(fg).bg(bg).add_modifier(Modifier::BOLD)
    } else if is_hovered_name {
        // Non-active tab with name hovered - use hover background
        Style::default()
            .fg(theme.tab_inactive_fg)
            .bg(theme.tab_hover_bg)
    } else {
        Style::default()
            .fg(theme.tab_inactive_fg)
            .bg(theme.tab_inactive_bg)
    };
    if is_preview {
        base_style = base_style.add_modifier(Modifier::ITALIC);
    }

    let close_style = if is_hovered_close {
        base_style.fg(theme.tab_close_hover_fg)
    } else {
        base_style
    };
    (base_style, close_style)
}

/// Output of [`build_tab_spans`]: `(spans, ranges, rendered_targets)`. See the
/// function's own docs for the per-field meaning.
type TabSpanLayout = (
    Vec<(Span<'static>, usize)>,
    Vec<(usize, usize, usize)>,
    Vec<TabTarget>,
);

/// Build the styled `(name, close-button)` spans for every resolvable tab.
///
/// Returns `(spans, ranges, rendered_targets)` where `spans` holds two entries
/// per tab (name then close button) as `(span, display_width)`, `ranges[i]` is
/// the `(start, end, close_start)` logical columns of rendered tab `i`, and
/// `rendered_targets[i]` is its target. Targets that don't resolve (hidden
/// buffers, missing groups) are skipped, so the returned vectors index by
/// *rendered* position, not by input position.
#[allow(clippy::too_many_arguments)]
fn build_tab_spans(
    tab_targets: &[TabTarget],
    resolved_names: &HashMap<TabTarget, String>,
    buffers: &HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, BufferMetadata>,
    composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    active_target: TabTarget,
    hovered_tab: Option<(TabTarget, bool)>,
    preview_buffer: Option<BufferId>,
    is_active_split: bool,
    theme: &crate::view::theme::Theme,
) -> TabSpanLayout {
    let mut all_tab_spans: Vec<(Span<'static>, usize)> = Vec::new();
    let mut tab_ranges: Vec<(usize, usize, usize)> = Vec::new();
    let mut rendered_targets: Vec<TabTarget> = Vec::new();

    for t in tab_targets.iter() {
        // Skip targets we couldn't resolve (hidden buffers, missing groups).
        let Some(name_owned) = resolved_names.get(t).cloned() else {
            continue;
        };
        let name = name_owned.as_str();
        rendered_targets.push(*t);

        // Composite buffers and groups never show as modified.
        let modified = match t {
            TabTarget::Buffer(id) if !composite_buffers.contains_key(id) => buffers
                .get(id)
                .filter(|state| state.buffer.is_modified())
                .map(|_| "*")
                .unwrap_or(""),
            _ => "",
        };
        let binary_indicator = match t {
            TabTarget::Buffer(id) if buffer_metadata.get(id).map(|m| m.binary).unwrap_or(false) => {
                " [BIN]"
            }
            _ => "",
        };

        // Preview (ephemeral) tabs render in italic and carry a translated
        // suffix (e.g. " (preview)") so the user knows the tab will be
        // replaced by the next single-click open.
        let is_preview = is_preview_tab(t, preview_buffer);
        let preview_indicator = preview_suffix(t, preview_buffer);
        let is_active = *t == active_target;
        let (is_hovered_name, is_hovered_close) = match hovered_tab {
            Some((hover_target, is_close)) if hover_target == *t => (!is_close, is_close),
            _ => (false, false),
        };

        let (base_style, close_style) = tab_styles(
            is_active,
            is_active_split,
            is_hovered_name,
            is_hovered_close,
            is_preview,
            theme,
        );

        // Tab content: " {name}{modified}{preview_indicator}{binary_indicator} ".
        let tab_name_text = format!(" {name}{modified}{preview_indicator}{binary_indicator} ");
        let tab_name_width = str_width(&tab_name_text);
        let close_text = "× ";
        let close_width = str_width(close_text);

        let start_pos: usize = all_tab_spans.iter().map(|(_, w)| w).sum();
        let close_start_pos = start_pos + tab_name_width;
        let end_pos = start_pos + tab_name_width + close_width;
        tab_ranges.push((start_pos, end_pos, close_start_pos));

        all_tab_spans.push((Span::styled(tab_name_text, base_style), tab_name_width));
        all_tab_spans.push((
            Span::styled(close_text.to_string(), close_style),
            close_width,
        ));
    }

    (all_tab_spans, tab_ranges, rendered_targets)
}

/// Clip the full tab-span list to the horizontally-scrolled viewport.
///
/// Produces the spans actually painted on the bar — left indicator, the
/// visible slice of tabs (truncating the boundary tab), right indicator and a
/// trailing fill out to `max_width` — plus the screen-x where the right scroll
/// indicator was drawn (if any), which the caller needs for its hit area.
fn build_visible_line(
    all_tab_spans: Vec<(Span<'static>, usize)>,
    area: Rect,
    offset: usize,
    max_width: usize,
    show_left: bool,
    show_right: bool,
    theme: &crate::view::theme::Theme,
) -> (Vec<Span<'static>>, Option<u16>) {
    let mut current_spans: Vec<Span<'static>> = Vec::new();
    let mut rendered_width = 0;
    let mut skip_chars_count = offset;

    if show_left {
        current_spans.push(Span::styled(
            SCROLL_INDICATOR_LEFT,
            Style::default().bg(theme.tab_separator_bg),
        ));
        rendered_width += SCROLL_INDICATOR_WIDTH;
    }

    let right_reserve = if show_right {
        SCROLL_INDICATOR_WIDTH
    } else {
        0
    };
    for (mut span, width) in all_tab_spans.into_iter() {
        if skip_chars_count >= width {
            skip_chars_count -= width;
            continue;
        }

        let visible_chars_in_span = width - skip_chars_count;
        if rendered_width + visible_chars_in_span > max_width.saturating_sub(right_reserve) {
            let remaining_width = max_width
                .saturating_sub(rendered_width)
                .saturating_sub(right_reserve);
            let truncated_content = span
                .content
                .chars()
                .skip(skip_chars_count)
                .take(remaining_width)
                .collect::<String>();
            span.content = std::borrow::Cow::Owned(truncated_content);
            current_spans.push(span);
            rendered_width += remaining_width;
            break;
        }

        let visible_content = span
            .content
            .chars()
            .skip(skip_chars_count)
            .collect::<String>();
        span.content = std::borrow::Cow::Owned(visible_content);
        current_spans.push(span);
        rendered_width += visible_chars_in_span;
        skip_chars_count = 0;
    }

    // Position of the right indicator (recorded before it's pushed) for hit
    // testing.
    let right_indicator_x = if show_right && rendered_width < max_width {
        Some(area.x + rendered_width as u16)
    } else {
        None
    };
    if show_right && rendered_width < max_width {
        current_spans.push(Span::styled(
            SCROLL_INDICATOR_RIGHT,
            Style::default().bg(theme.tab_separator_bg),
        ));
        rendered_width += SCROLL_INDICATOR_WIDTH;
    }
    if rendered_width < max_width {
        current_spans.push(Span::styled(
            " ".repeat(max_width.saturating_sub(rendered_width)),
            Style::default().bg(theme.tab_separator_bg),
        ));
    }

    (current_spans, right_indicator_x)
}

/// Map each rendered tab's logical column range to its on-screen hit areas
/// (tab body + close button), pushing them into `layout` and recording the
/// active/inactive theme cells. Tabs scrolled fully out of view are skipped.
#[allow(clippy::too_many_arguments)]
fn map_tab_hit_areas(
    layout: &mut TabLayout,
    rendered_targets: &[TabTarget],
    resolved_names: &HashMap<TabTarget, String>,
    tab_ranges: &[(usize, usize, usize)],
    area: Rect,
    offset: usize,
    available: usize,
    left_indicator_offset: usize,
    active_target: TabTarget,
    is_active_split: bool,
    mut rec: Option<&mut CellThemeRecorder>,
) {
    let visible_start = offset;
    let visible_end = offset + available;
    let base_x = area.x + left_indicator_offset as u16;

    for (idx, target) in rendered_targets.iter().enumerate() {
        let (logical_start, logical_end, logical_close_start) = tab_ranges[idx];

        // Skip tabs completely scrolled out of view.
        if logical_end <= visible_start || logical_start >= visible_end {
            continue;
        }

        let screen_start = if logical_start >= visible_start {
            base_x + (logical_start - visible_start) as u16
        } else {
            base_x
        };
        let screen_end = if logical_end <= visible_end {
            base_x + (logical_end - visible_start) as u16
        } else {
            base_x + available as u16
        };
        let screen_close_start =
            if logical_close_start >= visible_start && logical_close_start < visible_end {
                base_x + (logical_close_start - visible_start) as u16
            } else if logical_close_start < visible_start {
                // Close button scrolled off the left - clamp to the tab start.
                screen_start
            } else {
                // Close button scrolled off the right.
                screen_end
            };

        let tab_width = screen_end.saturating_sub(screen_start);
        let close_width = screen_end.saturating_sub(screen_close_start);

        // Record this tab's visible cells with its actual keys: the active tab
        // of the active split wears the active palette, every other tab the
        // inactive one (hover bg / close-hover fg are transient, not recorded).
        if let Some(r) = rec.as_deref_mut() {
            let (fg, bg) = if *target == active_target && is_active_split {
                ("ui.tab_active_fg", "ui.tab_active_bg")
            } else {
                ("ui.tab_inactive_fg", "ui.tab_inactive_bg")
            };
            r.run(
                screen_start,
                area.y,
                tab_width,
                Some(fg),
                Some(bg),
                "Tab Bar",
            );
        }

        layout.tabs.push(TabHitArea {
            target: *target,
            label: resolved_names.get(target).cloned().unwrap_or_default(),
            tab_area: Rect::new(screen_start, area.y, tab_width, 1),
            close_area: Rect::new(screen_close_start, area.y, close_width, 1),
        });
    }
}

impl TabsRenderer {
    /// Render the tab bar for a specific split showing only its open buffers
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render the tabs in
    /// * `split_buffers` - List of buffer IDs open in this split (in order)
    /// * `buffers` - All open buffers (for accessing state/metadata)
    /// * `buffer_metadata` - Metadata for buffers (contains display names for virtual buffers)
    /// * `active_buffer` - The currently active buffer ID for this split
    /// * `theme` - The active theme for colors
    /// * `is_active_split` - Whether this split is the active one
    /// * `hovered_tab` - Optional (buffer_id, is_close_button) if a tab is being hovered
    ///
    /// # Returns
    /// `TabLayout` containing hit areas for mouse interaction.
    #[allow(clippy::too_many_arguments)]
    pub fn render_for_split(
        buf: &mut ratatui::buffer::Buffer,
        area: Rect,
        tab_targets: &[TabTarget],
        buffers: &HashMap<BufferId, EditorState>,
        buffer_metadata: &HashMap<BufferId, BufferMetadata>,
        composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
        active_target: TabTarget,
        theme: &crate::view::theme::Theme,
        is_active_split: bool,
        tab_scroll_offset: usize,
        hovered_tab: Option<(TabTarget, bool)>, // (target, is_close_button)
        group_names: &HashMap<LeafId, String>,
        preview_buffer: Option<BufferId>,
        mut rec: Option<&mut CellThemeRecorder>,
        // When false, compute + return the TabLayout (tab rects) but paint no
        // cells — the web renders the tab bar natively from `tab_bar_view`. The
        // TUI always passes `true`.
        draw: bool,
    ) -> TabLayout {
        let mut layout = TabLayout::new(area);
        // Seed the whole bar with the separator surface (the block bg); each
        // visible tab / "+" overwrites its own cells below.
        if let Some(r) = rec.as_deref_mut() {
            r.run(
                area.x,
                area.y,
                area.width,
                None,
                Some("ui.tab_separator_bg"),
                "Tab Bar",
            );
        }
        let resolved_names = resolve_tab_names(
            tab_targets,
            buffers,
            buffer_metadata,
            composite_buffers,
            group_names,
        );

        // Phase 1: build each tab's styled name + close spans and logical
        // column ranges (unresolvable targets are skipped, so the vectors
        // index by rendered position).
        let (all_tab_spans, mut tab_ranges, rendered_targets) = build_tab_spans(
            tab_targets,
            &resolved_names,
            buffers,
            buffer_metadata,
            composite_buffers,
            active_target,
            hovered_tab,
            preview_buffer,
            is_active_split,
            theme,
        );

        // Phase 2: add separators between tabs (we do this after the loop to handle hidden buffers correctly)
        // We'll rebuild all_tab_spans with separators inserted, and fix up tab_ranges
        // to account for the separator widths
        let mut final_spans: Vec<(Span<'static>, usize)> = Vec::new();
        let mut separator_offset = 0usize;
        let spans_per_tab = 2; // name + close button
        for (tab_idx, chunk) in all_tab_spans.chunks(spans_per_tab).enumerate() {
            // Adjust tab_ranges for this tab to account for separators before it
            if separator_offset > 0 {
                let (start, end, close_start) = tab_ranges[tab_idx];
                tab_ranges[tab_idx] = (
                    start + separator_offset,
                    end + separator_offset,
                    close_start + separator_offset,
                );
            }

            for span in chunk {
                final_spans.push(span.clone());
            }
            // Add separator if not the last tab
            if tab_idx < rendered_targets.len().saturating_sub(1) {
                final_spans.push((
                    Span::styled(" ", Style::default().bg(theme.tab_separator_bg)),
                    1,
                ));
                separator_offset += 1;
            }
        }
        // Decide where the trailing "+" new-tab button goes. When the tabs
        // plus an inline "+" fit, the "+" is appended into the scroll flow and
        // sits right after the last tab. When they overflow, the "+" is pinned
        // to the right edge of the bar (`tabs_render_width` reserves its
        // column) and drawn on top after the main paragraph render below.
        let tabs_total: usize = final_spans.iter().map(|(_, w)| w).sum();
        let max_width = tabs_render_width(tabs_total, area.width as usize);
        let pin_plus = max_width < area.width as usize;

        let mut inline_plus_range: Option<(usize, usize)> = None;
        if !pin_plus {
            let plus_start = if !rendered_targets.is_empty() {
                // Separator between the last real tab and the "+" button
                final_spans.push((
                    Span::styled(" ", Style::default().bg(theme.tab_separator_bg)),
                    1,
                ));
                tabs_total + 1
            } else {
                tabs_total
            };
            final_spans.push((
                Span::styled(
                    NEW_TAB_BUTTON_TEXT.to_string(),
                    Style::default()
                        .fg(theme.tab_inactive_fg)
                        .bg(theme.tab_inactive_bg),
                ),
                NEW_TAB_BUTTON_WIDTH,
            ));
            inline_plus_range = Some((plus_start, plus_start + NEW_TAB_BUTTON_WIDTH));
        }

        // Phase 3: horizontal scroll geometry. Use the scroll offset directly
        // (ensure_active_tab_visible drives it); only clamp to avoid extremes.
        let total_width: usize = final_spans.iter().map(|(_, w)| w).sum();
        let max_offset = total_width.saturating_sub(max_width);
        let offset = tab_scroll_offset.min(total_width);
        tracing::trace!(
            "render_for_split: tab_scroll_offset={}, max_offset={}, offset={}, total={}, max_width={}",
            tab_scroll_offset, max_offset, offset, total_width, max_width
        );
        // Indicators reserve space based on scroll position.
        let show_left = offset > 0;
        let show_right = total_width.saturating_sub(offset) > max_width;
        let available = max_width
            .saturating_sub((show_left as usize + show_right as usize) * SCROLL_INDICATOR_WIDTH);

        // Phase 4: clip the spans to the viewport and paint the bar.
        let (current_spans, right_indicator_x) = build_visible_line(
            final_spans,
            area,
            offset,
            max_width,
            show_left,
            show_right,
            theme,
        );

        let line = Line::from(current_spans);
        let block = Block::default().style(Style::default().bg(theme.tab_separator_bg));
        let paragraph = Paragraph::new(line).block(block);
        if draw {
            paragraph.render(area, buf);
        }

        // Pinned "+" button: when the tabs overflow, draw the button on top of
        // the bar at the right edge. The main paragraph above filled the
        // reserved columns with the separator background; overwrite them with
        // the button cell here so it stays visible regardless of scroll.
        if pin_plus {
            let plus_w = NEW_TAB_BUTTON_WIDTH as u16;
            let plus_x = area.x + area.width.saturating_sub(plus_w);
            let plus_rect = Rect::new(plus_x, area.y, plus_w, 1);
            let plus_para = Paragraph::new(Line::from(vec![Span::styled(
                NEW_TAB_BUTTON_TEXT.to_string(),
                Style::default()
                    .fg(theme.tab_inactive_fg)
                    .bg(theme.tab_inactive_bg),
            )]));
            if draw {
                plus_para.render(plus_rect, buf);
            }
            layout.new_tab_area = Some(plus_rect);
        }

        // Compute and return hit areas for mouse interaction
        // We need to map the logical tab positions to screen positions accounting for:
        // 1. The scroll offset
        // 2. The left scroll indicator (if shown)
        // 3. The base area.x position
        let left_indicator_offset = if show_left { SCROLL_INDICATOR_WIDTH } else { 0 };

        // Set scroll button areas if shown
        if show_left {
            layout.left_scroll_area =
                Some(Rect::new(area.x, area.y, SCROLL_INDICATOR_WIDTH as u16, 1));
        }
        if let Some(right_x) = right_indicator_x {
            // Right scroll button is at the position where it was actually rendered
            layout.right_scroll_area =
                Some(Rect::new(right_x, area.y, SCROLL_INDICATOR_WIDTH as u16, 1));
        }

        // Phase 5: per-tab hit areas (and recorded theme cells).
        map_tab_hit_areas(
            &mut layout,
            &rendered_targets,
            &resolved_names,
            &tab_ranges,
            area,
            offset,
            available,
            left_indicator_offset,
            active_target,
            is_active_split,
            rec.as_deref_mut(),
        );

        // Map the inline "+" button's logical range to a screen rect using the
        // same visibility/clamping logic as the per-tab mapping above. (The
        // pinned variant set `new_tab_area` directly after the render.)
        if let Some((plus_logical_start, plus_logical_end)) = inline_plus_range {
            let visible_start = offset;
            let visible_end = offset + available;
            if plus_logical_end > visible_start && plus_logical_start < visible_end {
                let screen_start = if plus_logical_start >= visible_start {
                    area.x
                        + left_indicator_offset as u16
                        + (plus_logical_start - visible_start) as u16
                } else {
                    area.x + left_indicator_offset as u16
                };
                let screen_end = if plus_logical_end <= visible_end {
                    area.x
                        + left_indicator_offset as u16
                        + (plus_logical_end - visible_start) as u16
                } else {
                    area.x + left_indicator_offset as u16 + available as u16
                };
                let width = screen_end.saturating_sub(screen_start);
                if width > 0 {
                    layout.new_tab_area = Some(Rect::new(screen_start, area.y, width, 1));
                    if let Some(r) = rec.as_deref_mut() {
                        r.run(
                            screen_start,
                            area.y,
                            width,
                            Some("ui.tab_inactive_fg"),
                            Some("ui.tab_inactive_bg"),
                            "Tab Bar",
                        );
                    }
                }
            }
        }

        // Pinned "+" cells (drawn on top at the right edge when tabs overflow).
        if let (Some(plus_rect), Some(r)) = (layout.new_tab_area.filter(|_| pin_plus), rec) {
            r.run(
                plus_rect.x,
                area.y,
                plus_rect.width,
                Some("ui.tab_inactive_fg"),
                Some("ui.tab_inactive_bg"),
                "Tab Bar",
            );
        }

        layout
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::BufferId;

    #[test]
    fn tabs_render_width_inline_when_fits() {
        // Tabs + inline "+" fit: full width available, no reservation.
        assert_eq!(tabs_render_width(10, 40), 40);
        // Exactly fits inline: tabs(33) + sep(1) + plus(3) = 37 <= 40.
        assert_eq!(tabs_render_width(33, 40), 40);
        // No tabs: just the "+" — still inline.
        assert_eq!(tabs_render_width(0, 40), 40);
    }

    #[test]
    fn tabs_render_width_pins_when_overflow() {
        // tabs(37) + sep(1) + plus(3) = 41 > 40 → reserve 3.
        assert_eq!(tabs_render_width(37, 40), 37);
        // Heavy overflow still just reserves the button column.
        assert_eq!(tabs_render_width(200, 40), 37);
        // Degenerate: bar narrower than the button — fall back to full width.
        assert_eq!(tabs_render_width(100, 2), 2);
    }

    #[test]
    fn scroll_to_show_active_first_tab() {
        // Active is first tab, should scroll left to show it
        let widths = vec![5, 5, 5];
        let offset = scroll_to_show_tab(&widths, 0, 10, 20);
        // First tab starts at 0, should scroll to show it
        assert_eq!(offset, 0);
    }

    #[test]
    fn scroll_to_show_tab_already_visible() {
        // Tab is already visible, offset should stay the same
        let widths = vec![5, 5, 5];
        let offset = scroll_to_show_tab(&widths, 1, 0, 20);
        // Tab 1 starts at 5, ends at 10, visible in 0..20
        assert_eq!(offset, 0);
    }

    #[test]
    fn scroll_to_show_tab_on_right() {
        // Tab is to the right, need to scroll right
        let widths = vec![10, 10, 10];
        let offset = scroll_to_show_tab(&widths, 2, 0, 15);
        // Tab 2 starts at 20, ends at 30; need to scroll to show it
        assert!(offset > 0);
    }

    /// Helper: given a scroll offset, compute the visible content range
    /// accounting for scroll indicators (1 char each).
    fn visible_range(offset: usize, total_width: usize, max_width: usize) -> (usize, usize) {
        let show_left = offset > 0;
        let show_right = total_width.saturating_sub(offset) > max_width;
        let available = max_width
            .saturating_sub(if show_left { 1 } else { 0 })
            .saturating_sub(if show_right { 1 } else { 0 });
        (offset, offset + available)
    }

    /// Property: scroll_to_show_tab must produce an offset where the active tab
    /// is fully contained within the visible content range (after accounting for
    /// scroll indicator columns).
    #[test]
    fn scroll_to_show_tab_active_always_visible() {
        // Simulate the e2e scenario: 15 tabs with long names in a 40-char-wide bar.
        // tab_widths includes separators: [tab0, 1, tab1, 1, tab2, ...]
        // Active index for tab N is N*2 (matching ensure_active_tab_visible logic).
        let tab_content_width = 33; // " long_file_name_number_XX.txt × "
        let num_tabs = 15;
        let max_width = 40;

        let mut tab_widths = Vec::new();
        for i in 0..num_tabs {
            if i > 0 {
                tab_widths.push(1); // separator
            }
            tab_widths.push(tab_content_width);
        }
        let total_width: usize = tab_widths.iter().sum();

        for tab_idx in 0..num_tabs {
            let active_width_idx = if tab_idx == 0 { 0 } else { tab_idx * 2 };
            let tab_start: usize = tab_widths[..active_width_idx].iter().sum();
            let tab_end = tab_start + tab_widths[active_width_idx];

            let offset = scroll_to_show_tab(&tab_widths, active_width_idx, 0, max_width);
            let (vis_start, vis_end) = visible_range(offset, total_width, max_width);

            assert!(
                tab_start >= vis_start && tab_end <= vis_end,
                "Tab {} (width_idx={}, {}..{}) not fully visible in range {}..{} (offset={})",
                tab_idx,
                active_width_idx,
                tab_start,
                tab_end,
                vis_start,
                vis_end,
                offset
            );
        }
    }

    /// Property: same as above but with varying tab widths and screen sizes
    #[test]
    fn scroll_to_show_tab_property_varied_sizes() {
        let test_cases: Vec<(Vec<usize>, usize)> = vec![
            (vec![10, 15, 20, 10, 25], 30),
            (vec![5; 20], 20),
            (vec![40], 40),       // single tab exactly fills
            (vec![50], 40),       // single tab wider than screen
            (vec![3, 3, 3], 100), // all fit easily
        ];

        for (tab_widths, max_width) in test_cases {
            let total_width: usize = tab_widths.iter().sum();
            for active_idx in 0..tab_widths.len() {
                let tab_start: usize = tab_widths[..active_idx].iter().sum();
                let tab_end = tab_start + tab_widths[active_idx];
                let tab_w = tab_widths[active_idx];

                let offset = scroll_to_show_tab(&tab_widths, active_idx, 0, max_width);
                let (vis_start, vis_end) = visible_range(offset, total_width, max_width);

                // Only check if the tab can physically fit in the viewport
                if tab_w <= max_width.saturating_sub(2) || (active_idx == 0 && tab_w <= max_width) {
                    assert!(
                        tab_start >= vis_start && tab_end <= vis_end,
                        "Tab {} ({}..{}, w={}) not visible in {}..{} (offset={}, max_width={}, widths={:?})",
                        active_idx, tab_start, tab_end, tab_w, vis_start, vis_end, offset, max_width, tab_widths
                    );
                }
            }
        }
    }

    #[test]
    fn test_tab_layout_hit_test() {
        let bar_area = Rect::new(0, 0, 80, 1);
        let mut layout = TabLayout::new(bar_area);

        let buf1 = BufferId(1);
        let target1 = TabTarget::Buffer(buf1);

        layout.tabs.push(TabHitArea {
            target: target1,
            label: "buf1".to_string(),
            tab_area: Rect::new(0, 0, 16, 1),
            close_area: Rect::new(12, 0, 4, 1),
        });

        // Hit tab name
        assert_eq!(layout.hit_test(5, 0), Some(TabHit::TabName(target1)));

        // Hit close button
        assert_eq!(layout.hit_test(13, 0), Some(TabHit::CloseButton(target1)));

        // Hit bar background
        assert_eq!(layout.hit_test(50, 0), Some(TabHit::BarBackground));

        // Outside everything
        assert_eq!(layout.hit_test(50, 5), None);
    }
}
