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
use std::collections::HashMap;
use std::path::{Component, Path, MAIN_SEPARATOR, MAIN_SEPARATOR_STR};

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::BufferId;

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
