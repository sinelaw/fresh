//! The tab strip's model: which name a tab shows (`resolve_tab_names`), how
//! it is elided, and the scroll offset that keeps the active tab in view
//! (`calculate_tab_widths`, `scroll_to_show_tab`).
//!
//! **The strips are the tree's** (`view::shell::tabs`): each tab is a node
//! that answers its own press and reports its own hover, the web reads their
//! rectangles back by key, and an embedded window's strips are the same nodes
//! in the embed's own tree (`shell_host::paint_embed`). Nothing here paints.

use crate::app::BufferMetadata;
use crate::model::event::{BufferId, LeafId};
use crate::primitives::display_width::{char_width, str_width};
use crate::state::EditorState;
use crate::view::split::TabTarget;
use fresh_i18n::t;
use std::collections::HashMap;
use std::path::{Component, Path, MAIN_SEPARATOR, MAIN_SEPARATOR_STR};

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

/// Display width (columns) of the trailing ` + ` new-tab button.
pub const NEW_TAB_BUTTON_WIDTH: usize = 3;

/// Columns reserved at the right edge of a split's tab row for the
/// right-side control cluster, drawn on top of the row afterwards by the
/// orchestration layer. When a split has any control button the cluster reads
/// `> □ ×`:
///
/// ```text
///   [gap] > □ ×
/// ```
///
/// where `□` (maximize) is present only when `show_maximize`, `×` (close) only
/// when `show_close`, and the `>` right-overflow slot is always reserved (the
/// glyph is drawn only when the tabs actually overflow, but the column is held
/// so the layout doesn't jump as you scroll). The `+` new-buffer button is
/// *not* part of this cluster: it sits inline right after the last (visible)
/// tab, exactly as in a single split (fresh#2768
/// follow-up). The tab bar lays out — and the tab-scroll math measures against
/// — the pane width *minus* this reserve, so the scrolling tabs, the inline
/// `+`, and the `<` left-overflow indicator never end up underneath the
/// cluster.
///
/// A pane with no control buttons (a single, unmaximized split) reserves
/// nothing: there is no cluster, and the strip places its own inline /
/// pinned `+` and `<`/`>` indicators exactly as an unsplit editor does.
pub fn split_control_reserve(show_maximize: bool, show_close: bool) -> u16 {
    if !show_maximize && !show_close {
        return 0;
    }
    // gap(1) + right-overflow slot(1) + maximize + close + trailing blank(1).
    1 + 1 + show_maximize as u16 + show_close as u16 + 1
}

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

/// Single-character ellipsis (U+2026) appended when a tab name is elided.
const TAB_NAME_ELLIPSIS: &str = "…";

/// Maximum display width, in columns, for the *name* portion of a tab label.
/// The surrounding pad, the modified/preview/binary indicators and the close
/// button are budgeted separately, so this caps only the filename/group name.
/// Without a cap a single very long name (e.g. 151 chars) consumes the whole
/// strip and hides every other tab (issue #2650).
pub const TAB_NAME_MAX_COLS: usize = 25;

/// Shorten a path-shaped label (`src/model/main.rs`) from the *front*, keeping
/// the file name and as many trailing directories as fit behind a leading
/// `…{sep}` marker — e.g. `…/model/main.rs`.
///
/// A tab whose name was disambiguated by path (see [`resolve_tab_names`])
/// carries its identity in the last component, so the generic
/// keep-the-leading-characters truncation would throw away exactly the part the
/// user is looking for. Returns `None` when `name` has no separator, or when
/// not even `…{sep}` plus the last component fits in `max_cols` — the caller
/// then falls back to plain truncation.
fn elide_path_label(name: &str, max_cols: usize) -> Option<String> {
    let parts: Vec<&str> = name.split(MAIN_SEPARATOR).collect();
    let (file, dirs) = parts.split_last()?;
    if dirs.is_empty() {
        return None;
    }
    let marker = format!("{TAB_NAME_ELLIPSIS}{MAIN_SEPARATOR}");
    let mut width = str_width(&marker) + str_width(file);
    if width > max_cols {
        return None;
    }
    // Grow rightwards-first: prepend whole directories while they fit.
    let mut kept: Vec<&str> = vec![file];
    for dir in dirs.iter().rev() {
        let extra = str_width(dir) + str_width(MAIN_SEPARATOR_STR);
        if width + extra > max_cols {
            break;
        }
        width += extra;
        kept.push(dir);
    }
    kept.reverse();
    Some(format!("{marker}{}", kept.join(MAIN_SEPARATOR_STR)))
}

/// Elide `name` to at most `max_cols` display columns, keeping the leading
/// characters and appending a single `…` when it is truncated. Width is
/// measured with `char_width`/`str_width` (not bytes), so multibyte / CJK /
/// emoji names are truncated on whole characters and never split mid-codepoint.
/// Returns `name` unchanged when it already fits.
///
/// Path-shaped labels are shortened from the front instead (see
/// [`elide_path_label`]) so the file name survives.
///
/// The label builder ([`calculate_tab_widths`]) and the strip's nodes run
/// the resolved name through this so their computed widths stay in lockstep; a
/// mismatch would drift hit-testing and the scroll math.
pub fn elided_tab_name(name: &str, max_cols: usize) -> String {
    if str_width(name) <= max_cols {
        return name.to_string();
    }
    if let Some(elided) = elide_path_label(name, max_cols) {
        return elided;
    }
    let budget = max_cols.saturating_sub(str_width(TAB_NAME_ELLIPSIS));
    let mut width = 0;
    let mut body = String::new();
    for ch in name.chars() {
        let w = char_width(ch);
        if width + w > budget {
            break;
        }
        width += w;
        body.push(ch);
    }
    body.push_str(TAB_NAME_ELLIPSIS);
    body
}

/// Full (uncapped) display width of one tab's label — the name portion plus the
/// close button, excluding the inter-tab separator. Mirrors the label format
/// both builders paint, so the "do all tabs fit?" pre-pass measures exactly what
/// the row would render at full names.
fn full_tab_label_width(
    t: &TabTarget,
    name: &str,
    buffers: &HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, BufferMetadata>,
    composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    preview_buffer: Option<BufferId>,
) -> usize {
    let modified = match t {
        TabTarget::Buffer(id) if !composite_buffers.contains_key(id) => buffers
            .get(id)
            .filter(|state| state.buffer.is_modified())
            .map(|_| "*")
            .unwrap_or(""),
        _ => "",
    };
    let binary = match t {
        TabTarget::Buffer(id) if buffer_metadata.get(id).map(|m| m.binary).unwrap_or(false) => {
            " [BIN]"
        }
        _ => "",
    };
    let preview_indicator = preview_suffix(t, preview_buffer);
    let tab_name_text = format!(" {name}{modified}{preview_indicator}{binary} ");
    str_width(&tab_name_text) + str_width("× ")
}

/// Decide the per-name elision cap for a split's tab bar.
///
/// When every tab fits at its FULL name within `available_width` (accounting for
/// the inter-tab separators and the pinned "+" reservation) the names are shown
/// untruncated (cap = `usize::MAX`, i.e. no elision). Only when the tabs would
/// overflow — the bar is "full" — is each name capped at [`TAB_NAME_MAX_COLS`]
/// so one long filename can't hide every other tab (issue #2650).
///
/// The label builder ([`calculate_tab_widths`]) and the strip derive
/// their cap from this with the same `available_width`, so their computed widths
/// stay in lockstep.
fn tab_name_cap(
    tab_targets: &[TabTarget],
    resolved_names: &HashMap<TabTarget, String>,
    buffers: &HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, BufferMetadata>,
    composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    preview_buffer: Option<BufferId>,
    available_width: usize,
) -> usize {
    let mut full_total = 0usize;
    let mut count = 0usize;
    for t in tab_targets.iter() {
        let Some(name) = resolved_names.get(t) else {
            continue;
        };
        full_total += full_tab_label_width(
            t,
            name,
            buffers,
            buffer_metadata,
            composite_buffers,
            preview_buffer,
        );
        count += 1;
    }
    let full_total_with_seps = full_total + count.saturating_sub(1);
    // `tabs_render_width` returns the columns actually available for tabs after
    // reserving the pinned "+" (when they overflow). If the full-name total fits
    // in that, nothing scrolls and we show full names.
    let render_w = tabs_render_width(full_total_with_seps, available_width);
    if full_total_with_seps <= render_w {
        usize::MAX
    } else {
        TAB_NAME_MAX_COLS
    }
}

/// Display components of `path`, outermost first.
///
/// Only `Normal` components are kept: a root (`/`) or Windows drive prefix
/// never helps tell two same-named files apart, and re-joining it would double
/// the separator. Non-UTF-8 components are shown lossily rather than dropped,
/// so a tab for such a file is still distinguishable.
fn label_components(path: &Path) -> Vec<String> {
    path.components()
        .filter_map(|c| match c {
            Component::Normal(s) => Some(s.to_string_lossy().into_owned()),
            _ => None,
        })
        .collect()
}

/// The last `depth` components of `parts`, joined with the platform separator.
/// A `depth` past the start of `parts` yields the whole path.
fn path_tail(parts: &[String], depth: usize) -> String {
    let start = parts.len().saturating_sub(depth);
    parts[start..].join(MAIN_SEPARATOR_STR)
}

/// The shortest trailing path fragment of `parts` that none of `others` shares
/// — `src/main.rs` when the collision is with `tests/main.rs`, `a/b/main.rs`
/// when it takes two directories to separate them.
///
/// Depth 1 is the file name, which every member of a same-name group shares by
/// construction, so the search starts at 2. Returns `None` when the paths stay
/// identical all the way up (or the file has no parent directory at all), which
/// leaves the caller to fall back to numbering.
fn shortest_unique_tail(parts: &[String], others: &[&[String]]) -> Option<String> {
    (2..=parts.len()).find_map(|depth| {
        let tail = path_tail(parts, depth);
        others
            .iter()
            .all(|other| path_tail(other, depth) != tail)
            .then_some(tail)
    })
}

/// Resolve display names for tab targets, disambiguating duplicates.
///
/// A tab is normally just the file's name. When several open tabs share that
/// name — the common case in a workspace full of `mod.rs` / `index.ts` — each
/// one is instead labelled with the shortest trailing path fragment that tells
/// it apart from the others (`model/mod.rs` vs `view/mod.rs`), so the tab bar
/// says which file it is without the user hovering or switching (issue #2851).
///
/// Tabs with no file path behind them (unnamed buffers, terminals, composite
/// buffers, groups) can't be separated that way, so those — and any file whose
/// path is identical to another's all the way up — keep the older numeric
/// suffix: three unnamed buffers become "[No Name] 1", "[No Name] 2",
/// "[No Name] 3".
///
/// `group_names` provides the display name for each group tab (`TabTarget::Group`).
pub(crate) fn resolve_tab_names(
    tab_targets: &[TabTarget],
    buffers: &HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, BufferMetadata>,
    composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    group_names: &HashMap<LeafId, String>,
) -> HashMap<TabTarget, String> {
    // (target, base name, path components when the tab is backed by a file).
    let mut names: Vec<(TabTarget, String, Option<Vec<String>>)> = Vec::new();

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

                // Only real file buffers carry a path we can disambiguate by;
                // terminals and composite buffers are named by their metadata.
                let file_path = (!is_composite_buffer && !is_terminal)
                    .then(|| buffers.get(id).and_then(|state| state.buffer.file_path()))
                    .flatten();

                let name = if is_composite_buffer || is_terminal {
                    meta.map(|m| m.display_name.as_str())
                } else {
                    file_path
                        .and_then(|p| p.file_name())
                        .and_then(|n| n.to_str())
                        .or_else(|| meta.map(|m| m.display_name.as_str()))
                }
                .unwrap_or("[No Name]");

                // Keep the path only when the label really is the file's name;
                // a buffer falling back to its display name isn't described by
                // its path, so a path fragment there would mislead.
                let parts = file_path
                    .filter(|p| p.file_name().and_then(|n| n.to_str()) == Some(name))
                    .map(label_components);

                names.push((*t, name.to_string(), parts));
            }
            TabTarget::Group(leaf_id) => {
                if let Some(name) = group_names.get(leaf_id) {
                    names.push((*t, name.clone(), None));
                }
            }
        }
    }

    // Count occurrences of each name
    let mut name_counts: HashMap<&str, usize> = HashMap::new();
    for (_, name, _) in &names {
        *name_counts.entry(name.as_str()).or_insert(0) += 1;
    }

    // Duplicates first try a path fragment; whatever that can't separate
    // (pathless buffers, or paths identical all the way up) is numbered below,
    // per base name, exactly as before.
    let mut result = HashMap::new();
    let mut numbered: Vec<(TabTarget, &str)> = Vec::new();
    for (t, name, parts) in &names {
        if name_counts.get(name.as_str()).copied().unwrap_or(0) <= 1 {
            result.insert(*t, name.clone());
            continue;
        }
        // The other tabs sharing this base name that we could compare against.
        let others: Vec<&[String]> = names
            .iter()
            .filter(|(other_t, other_name, _)| other_t != t && other_name == name)
            .filter_map(|(_, _, other_parts)| other_parts.as_deref())
            .collect();
        match parts
            .as_deref()
            .and_then(|p| shortest_unique_tail(p, &others))
        {
            Some(tail) => {
                result.insert(*t, tail);
            }
            None => numbered.push((*t, name.as_str())),
        }
    }

    // A tab left over on its own is already unambiguous — only number a base
    // name that still has several claimants.
    let mut leftover_counts: HashMap<&str, usize> = HashMap::new();
    for (_, name) in &numbered {
        *leftover_counts.entry(*name).or_insert(0) += 1;
    }
    let mut name_indices: HashMap<&str, usize> = HashMap::new();
    for (t, name) in numbered {
        if leftover_counts.get(name).copied().unwrap_or(0) > 1 {
            let idx = name_indices.entry(name).or_insert(0);
            *idx += 1;
            result.insert(t, format!("{} {}", name, idx));
        } else {
            result.insert(t, name.to_string());
        }
    }

    result
}

/// Calculate tab widths for scroll offset calculations.
/// Returns (tab_widths, rendered_targets) where tab_widths includes separators.
/// The strip's nodes (`view::shell::tabs`) measure the same way.
pub fn calculate_tab_widths(
    tab_targets: &[TabTarget],
    buffers: &HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, BufferMetadata>,
    composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    group_names: &HashMap<LeafId, String>,
    preview_buffer: Option<BufferId>,
    available_width: usize,
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

    // Full names when they all fit, otherwise cap each at TAB_NAME_MAX_COLS.
    // The strip's nodes apply the same cap, or widths drift.
    let name_cap = tab_name_cap(
        tab_targets,
        &resolved_names,
        buffers,
        buffer_metadata,
        composite_buffers,
        preview_buffer,
        available_width,
    );

    for t in tab_targets.iter() {
        // Skip targets we couldn't resolve a name for (hidden, missing, etc.)
        let Some(name) = resolved_names.get(t) else {
            continue;
        };
        let name = elided_tab_name(name, name_cap);

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

        // The strip's format: " {name}{modified}{preview_indicator}{binary_indicator} " + "× "
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

    // --- Tab name elision (issue #2650) ---------------------------------

    #[test]
    fn elided_tab_name_leaves_short_names_untouched() {
        assert_eq!(elided_tab_name("main.rs", TAB_NAME_MAX_COLS), "main.rs");
        // Exactly at the cap is not truncated.
        let exact = "a".repeat(TAB_NAME_MAX_COLS);
        assert_eq!(elided_tab_name(&exact, TAB_NAME_MAX_COLS), exact);
    }

    #[test]
    fn elided_tab_name_caps_long_name_and_ends_with_ellipsis() {
        let name = "a".repeat(151);
        let out = elided_tab_name(&name, TAB_NAME_MAX_COLS);
        assert!(
            str_width(&out) <= TAB_NAME_MAX_COLS,
            "elided width {} exceeds cap {}",
            str_width(&out),
            TAB_NAME_MAX_COLS
        );
        assert!(out.ends_with('…'), "elided label must end with U+2026");
    }

    #[test]
    fn elided_tab_name_multibyte_stays_within_cap_without_panic() {
        // Wide CJK glyphs (2 cols each) plus multi-codepoint emoji, well over
        // the cap: must truncate on whole characters and never split a
        // codepoint (which would panic) or exceed the display-width cap.
        let name = format!("{}🎉🎊🚀", "日本語のファイル".repeat(6));
        let out = elided_tab_name(&name, TAB_NAME_MAX_COLS);
        assert!(
            str_width(&out) <= TAB_NAME_MAX_COLS,
            "elided width {} exceeds cap {}",
            str_width(&out),
            TAB_NAME_MAX_COLS
        );
        assert!(out.ends_with('…'));
    }

    /// Build `TabTarget::Group` inputs (one per name) so the label builders can
    /// be exercised without constructing real buffers/`EditorState`.
    fn build_group_inputs(names: &[&str]) -> (Vec<TabTarget>, HashMap<LeafId, String>) {
        let mut group_names = HashMap::new();
        let mut targets = Vec::new();
        for (i, n) in names.iter().enumerate() {
            let leaf = LeafId(crate::model::event::SplitId(i));
            group_names.insert(leaf, n.to_string());
            targets.push(TabTarget::Group(leaf));
        }
        (targets, group_names)
    }

    #[test]
    fn long_tab_name_is_bounded_by_the_cap() {
        let long = "a".repeat(151);
        let (targets, group_names) = build_group_inputs(&[long.as_str()]);
        let buffers = HashMap::new();
        let meta = HashMap::new();
        let comp = HashMap::new();

        // A narrow bar (40) forces the long name to overflow -> capped.
        let bar = 40;
        // calculate_tab_widths: single tab, no separator.
        let (widths, rendered) =
            calculate_tab_widths(&targets, &buffers, &meta, &comp, &group_names, None, bar);
        assert_eq!(rendered.len(), 1);
        assert_eq!(widths.len(), 1);
        // Full tab width = leading+trailing pad (2) + name (<=cap) + "× " (2).
        assert!(
            widths[0] <= TAB_NAME_MAX_COLS + 4,
            "tab width {} exceeds cap {} + indicators",
            widths[0],
            TAB_NAME_MAX_COLS
        );
    }

    #[test]
    fn over_long_tabs_are_elided_and_scroll_into_view() {
        let long = "z".repeat(151);
        let (targets, group_names) =
            build_group_inputs(&[long.as_str(), "short.rs", long.as_str(), "other.txt"]);
        let buffers = HashMap::new();
        let meta = HashMap::new();
        let comp = HashMap::new();
        let max_width = 40;
        let (tab_widths, rendered) = calculate_tab_widths(
            &targets,
            &buffers,
            &meta,
            &comp,
            &group_names,
            None,
            max_width,
        );

        let total: usize = tab_widths.iter().sum();
        for i in 0..rendered.len() {
            let width_idx = if i == 0 { 0 } else { i * 2 };
            let w = tab_widths[width_idx];
            assert!(
                w <= TAB_NAME_MAX_COLS + 4,
                "tab {} width {} exceeds cap",
                i,
                w
            );
            // With names bounded, the active tab always fully scrolls into view.
            let offset = scroll_to_show_tab(&tab_widths, width_idx, 0, max_width);
            let (vis_start, vis_end) = visible_range(offset, total, max_width);
            let start: usize = tab_widths[..width_idx].iter().sum();
            let end = start + w;
            assert!(
                start >= vis_start && end <= vis_end,
                "tab {} ({}..{}) not fully visible in {}..{} (offset={})",
                i,
                start,
                end,
                vis_start,
                vis_end,
                offset
            );
        }
    }

    #[test]
    fn split_control_reserve_matches_cluster_width() {
        // No buttons (single pane): no reservation — the tab renderer draws its
        // own inline/pinned `+` and `<`/`>` indicators.
        assert_eq!(split_control_reserve(false, false), 0);
        // Maximized single pane: cluster is `> □` (no close, no `+` — the `+` is
        // drawn inline with the tabs), i.e. gap + `>` slot + □ + trail = 4.
        assert_eq!(split_control_reserve(true, false), 4);
        // Multiple splits, not maximized: full cluster `> □ ×`, i.e.
        // gap + `>` slot + □ + × + trail = 5.
        assert_eq!(split_control_reserve(true, true), 5);
    }

    #[test]
    fn tab_names_full_when_they_fit_and_capped_when_overflowing() {
        // A single name longer than the cap (30 cols) but shorter than a wide
        // bar: it fits, so it is shown in full (no elision).
        let name = "n".repeat(30);
        let (targets, group_names) = build_group_inputs(&[name.as_str()]);
        let buffers = HashMap::new();
        let meta = HashMap::new();
        let comp = HashMap::new();

        let (wide_widths, _) =
            calculate_tab_widths(&targets, &buffers, &meta, &comp, &group_names, None, 100);
        // pad(2) + full name(30) + "× "(2) = 34, untruncated.
        assert_eq!(wide_widths[0], 34, "wide bar should show the full name");

        // The same name in a narrow bar overflows, so it is capped.
        let (narrow_widths, _) =
            calculate_tab_widths(&targets, &buffers, &meta, &comp, &group_names, None, 20);
        assert!(
            narrow_widths[0] <= TAB_NAME_MAX_COLS + 4,
            "narrow bar should cap the name (width {})",
            narrow_widths[0]
        );
        assert!(
            narrow_widths[0] < wide_widths[0],
            "capped width {} must be narrower than full width {}",
            narrow_widths[0],
            wide_widths[0]
        );
    }

    // --- Path disambiguation of same-named tabs (issue #2851) -----------

    /// Build buffer inputs for `resolve_tab_names`: one file buffer per path,
    /// keyed by ascending `BufferId`.
    fn build_file_inputs(paths: &[&str]) -> (Vec<TabTarget>, HashMap<BufferId, EditorState>) {
        use crate::config::LARGE_FILE_THRESHOLD_BYTES;
        use crate::model::filesystem::StdFileSystem;

        let mut buffers = HashMap::new();
        let mut targets = Vec::new();
        for (i, p) in paths.iter().enumerate() {
            let id = BufferId(i);
            let mut state = EditorState::new(
                80,
                24,
                LARGE_FILE_THRESHOLD_BYTES as usize,
                std::sync::Arc::new(StdFileSystem),
            );
            state.buffer.set_file_path(std::path::PathBuf::from(p));
            buffers.insert(id, state);
            targets.push(TabTarget::Buffer(id));
        }
        (targets, buffers)
    }

    /// Resolve names for a set of file paths, returned in the input order.
    fn resolved_for(paths: &[&str]) -> Vec<String> {
        let (targets, buffers) = build_file_inputs(paths);
        let resolved = resolve_tab_names(
            &targets,
            &buffers,
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
        );
        targets
            .iter()
            .map(|t| resolved.get(t).cloned().unwrap_or_default())
            .collect()
    }

    /// Join components with the platform separator, so the expectations below
    /// read the same on Windows as on Unix.
    fn p(parts: &[&str]) -> String {
        parts.join(MAIN_SEPARATOR_STR)
    }

    #[test]
    fn unique_file_names_stay_bare() {
        assert_eq!(
            resolved_for(&["/w/src/main.rs", "/w/src/lib.rs"]),
            vec!["main.rs".to_string(), "lib.rs".to_string()]
        );
    }

    #[test]
    fn duplicate_file_names_get_their_distinguishing_directory() {
        // The old behaviour numbered these ("mod.rs 1" / "mod.rs 2"), which
        // says nothing about which file is which.
        assert_eq!(
            resolved_for(&["/w/src/model/mod.rs", "/w/src/view/mod.rs"]),
            vec![p(&["model", "mod.rs"]), p(&["view", "mod.rs"])]
        );
    }

    #[test]
    fn disambiguation_walks_up_until_the_paths_differ() {
        // The parent directory is shared, so one level isn't enough: each label
        // grows only until it is unique, and no further.
        assert_eq!(
            resolved_for(&[
                "/w/crates/a/src/lib.rs",
                "/w/crates/b/src/lib.rs",
                "/w/vendor/lib.rs",
            ]),
            vec![
                p(&["a", "src", "lib.rs"]),
                p(&["b", "src", "lib.rs"]),
                p(&["vendor", "lib.rs"]),
            ]
        );
    }

    #[test]
    fn only_the_colliding_names_grow_a_path() {
        // `unique.rs` shares its name with nobody, so it stays bare while the
        // `mod.rs` pair is disambiguated.
        assert_eq!(
            resolved_for(&["/w/a/mod.rs", "/w/b/mod.rs", "/w/a/unique.rs"]),
            vec![
                p(&["a", "mod.rs"]),
                p(&["b", "mod.rs"]),
                "unique.rs".to_string()
            ]
        );
    }

    #[test]
    fn pathless_duplicates_still_fall_back_to_numbering() {
        // Unnamed buffers have no path to disambiguate by, so they keep the
        // numeric suffix.
        use crate::config::LARGE_FILE_THRESHOLD_BYTES;
        use crate::model::filesystem::StdFileSystem;

        let mut buffers = HashMap::new();
        let mut targets = Vec::new();
        for i in 0..3 {
            let id = BufferId(i);
            buffers.insert(
                id,
                EditorState::new(
                    80,
                    24,
                    LARGE_FILE_THRESHOLD_BYTES as usize,
                    std::sync::Arc::new(StdFileSystem),
                ),
            );
            targets.push(TabTarget::Buffer(id));
        }
        let resolved = resolve_tab_names(
            &targets,
            &buffers,
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
        );
        let mut labels: Vec<String> = targets
            .iter()
            .map(|t| resolved[t].clone())
            .collect::<Vec<_>>();
        labels.sort();
        assert_eq!(labels, vec!["[No Name] 1", "[No Name] 2", "[No Name] 3"]);
    }

    #[test]
    fn a_file_at_the_filesystem_root_falls_back_to_numbering() {
        // `/main.rs` has no directory above it to name, so the pair can't be
        // told apart by path and keeps the numeric suffix.
        let labels = resolved_for(&["/main.rs", "/main.rs"]);
        assert_eq!(labels, vec!["main.rs 1", "main.rs 2"]);
    }

    #[test]
    fn duplicate_group_names_still_number() {
        // Groups carry no path at all.
        let (targets, group_names) = build_group_inputs(&["scratch", "scratch"]);
        let resolved = resolve_tab_names(
            &targets,
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
            &group_names,
        );
        let mut labels: Vec<String> = targets.iter().map(|t| resolved[t].clone()).collect();
        labels.sort();
        assert_eq!(labels, vec!["scratch 1", "scratch 2"]);
    }

    // --- Path-aware elision ---------------------------------------------

    #[test]
    fn elision_of_a_path_label_keeps_the_file_name() {
        let label = p(&["crates", "fresh-editor", "src", "view", "ui", "tabs.rs"]);
        let out = elided_tab_name(&label, TAB_NAME_MAX_COLS);
        assert!(
            str_width(&out) <= TAB_NAME_MAX_COLS,
            "elided width {} exceeds cap {TAB_NAME_MAX_COLS}: {out:?}",
            str_width(&out)
        );
        assert!(
            out.ends_with("tabs.rs"),
            "the file name must survive elision, got {out:?}"
        );
        assert!(
            out.starts_with('…'),
            "the dropped leading directories must be marked, got {out:?}"
        );
    }

    #[test]
    fn elision_keeps_as_many_directories_as_fit() {
        let label = p(&["aaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "view", "ui", "tabs.rs"]);
        let out = elided_tab_name(&label, TAB_NAME_MAX_COLS);
        assert_eq!(out, format!("…{}", p(&["", "view", "ui", "tabs.rs"])));
    }

    #[test]
    fn a_file_name_too_long_for_the_cap_is_truncated_from_the_end() {
        // Not even "…/" plus the file name fits, so the whole label falls back
        // to leading-character truncation rather than rendering a bare marker.
        let long_file = format!("{}.rs", "n".repeat(60));
        let label = p(&["src", &long_file]);
        let out = elided_tab_name(&label, TAB_NAME_MAX_COLS);
        assert!(str_width(&out) <= TAB_NAME_MAX_COLS);
        assert!(out.ends_with('…'), "got {out:?}");
        assert!(out.starts_with("src"), "got {out:?}");
    }
}
