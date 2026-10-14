//! A move preview in a plugin's `Tree` (`WidgetMutation::PreviewTreeMove`).
//!
//! The same preview the file explorer plays while a drag is held over a
//! directory (`view::move_preview`), for a tree whose rows are a plugin's:
//! the orchestrator dock's, when a workspace is dragged over a folder. The
//! plugin names the row that moves, the row it lands under and the name to
//! type; the host lays the preview into the spec the panel is drawn from —
//! never into the plugin's own copy. The plugin ends it: dropped, or
//! committed (`EndTreeMovePreview`), in which case the host lets it play out
//! and then tells the plugin with `movepreviewend` so it makes the move.

use std::collections::HashMap;
use std::time::Instant;

use fresh_core::api::{OverlayColorSpec, OverlayOptions, TreeNode, WidgetSpec};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, StyledSegment, TextPropertyEntry};

use crate::view::move_preview::{typed_runs, typed_split, MovePreview, GHOST_BG};
use crate::widgets::{PanelKey, WidgetInstanceState};

/// The item key a preview's empty row carries: no plugin's, so it can be
/// told apart from every row the plugin made.
const PREVIEW_ROW_PREFIX: &str = "\u{0}move-preview:";

/// Whether `key` is a preview's empty row rather than one of the plugin's.
pub(crate) fn is_preview_row(key: &str) -> bool {
    key.starts_with(PREVIEW_ROW_PREFIX)
}

/// One plugin tree's preview in flight.
#[derive(Debug, Clone)]
pub(crate) struct TreeMovePreview {
    pub panel_key: PanelKey,
    pub widget_key: String,
    /// The row that moves.
    pub source_key: String,
    /// The row the empty row opens directly under; `None` for the top of the
    /// tree.
    pub after_key: Option<String>,
    /// What is typed across.
    pub label: String,
    pub clock: MovePreview,
    /// The plugin has committed the move: it is made (`movepreviewend`) once
    /// the preview has played out.
    pub committed: bool,
}

impl crate::app::Editor {
    /// Show `label`'s move in `widget_key`'s tree, replacing whatever move
    /// that tree was showing — unless it is this same move, which keeps
    /// playing rather than starting over.
    pub(crate) fn start_tree_move_preview(
        &mut self,
        panel_key: &PanelKey,
        widget_key: String,
        source_key: String,
        after_key: Option<String>,
        label: String,
    ) {
        let same_tree =
            |p: &TreeMovePreview| &p.panel_key == panel_key && p.widget_key == widget_key;
        if let Some(p) = self.tree_move_previews.iter().find(|p| same_tree(p)) {
            if p.source_key == source_key && p.after_key == after_key && p.label == label {
                return;
            }
        }
        // A move already dropped is made now rather than lost: the plugin is
        // waiting for its `movepreviewend`.
        if let Some(i) = self
            .tree_move_previews
            .iter()
            .position(|p| same_tree(p) && p.committed)
        {
            let dropped = self.tree_move_previews.remove(i);
            self.fire_move_preview_end(dropped);
        }
        self.tree_move_previews.retain(|p| !same_tree(p));
        self.tree_move_previews.push(TreeMovePreview {
            panel_key: panel_key.clone(),
            widget_key,
            source_key,
            after_key,
            clock: MovePreview::new([label.as_str()], self.time_source().now()),
            label,
            committed: false,
        });
        self.shell_description_stale = true;
    }

    /// End `widget_key`'s preview: drop it, or commit it to be made once it
    /// has played out (`step_tree_move_previews`) — at once with animations
    /// off, or when it already has.
    pub(crate) fn end_tree_move_preview(
        &mut self,
        panel_key: &PanelKey,
        widget_key: &str,
        commit: bool,
    ) {
        let Some(i) = self
            .tree_move_previews
            .iter()
            .position(|p| &p.panel_key == panel_key && p.widget_key == widget_key)
        else {
            return;
        };
        self.shell_description_stale = true;
        if !commit {
            self.tree_move_previews.remove(i);
            return;
        }
        self.tree_move_previews[i].committed = true;
        self.step_tree_move_previews();
    }

    /// Make every committed move whose preview has played out: the panel is
    /// drawn from its own spec again, and the plugin hears `movepreviewend`.
    /// True while a preview still owes frames.
    pub(crate) fn step_tree_move_previews(&mut self) -> bool {
        if self.tree_move_previews.is_empty() {
            return false;
        }
        let now = self.time_source().now();
        let animations = self.config.editor.animations;
        let (done, playing): (Vec<_>, Vec<_>) = std::mem::take(&mut self.tree_move_previews)
            .into_iter()
            .partition(|p| p.committed && (!animations || p.clock.finished(now)));
        self.tree_move_previews = playing;
        let owes_frames = animations
            && self
                .tree_move_previews
                .iter()
                .any(|p| p.clock.next_deadline(now).is_some());
        for p in done {
            self.fire_move_preview_end(p);
        }
        owes_frames
    }

    /// The move is made: the panel is drawn from its own spec again, and the
    /// plugin hears `movepreviewend`.
    fn fire_move_preview_end(&mut self, p: TreeMovePreview) {
        self.fire_widget_event(
            &p.panel_key,
            p.widget_key,
            "movepreviewend".to_string(),
            serde_json::json!({ "key": p.source_key }),
        );
        self.shell_description_stale = true;
    }

    /// A tree event raised on the panel as drawn — with a preview's empty
    /// row in it — told in the plugin's own rows: an `index` past the empty
    /// row is one less. `None` for a press on the empty row itself, which is
    /// no row of the plugin's and does nothing. Drag facts are left to name
    /// the empty row (the drag's own handling knows what it stands for).
    pub(crate) fn unsplice_tree_event(
        &self,
        slot: &crate::view::shell::widgets::Slot,
        mut event: crate::widgets::WidgetEvent,
        drag: bool,
    ) -> Option<crate::widgets::WidgetEvent> {
        if event.widget_kind != "tree" || self.tree_move_previews.is_empty() {
            return Some(event);
        }
        let Some(panel_key) = self.panel_key_of_slot(slot) else {
            return Some(event);
        };
        let Some(p) = self
            .tree_move_previews
            .iter()
            .find(|p| p.panel_key == panel_key && p.widget_key == event.widget_key)
            .filter(|_| self.config.editor.animations)
        else {
            return Some(event);
        };
        let Some(at) = self
            .widget_registry
            .get(&panel_key)
            .and_then(|panel| preview_insertion(&panel.spec, p))
            .map(|(at, _)| at)
        else {
            return Some(event);
        };
        let Some(index) = event.payload.get("index").and_then(|v| v.as_u64()) else {
            return Some(event);
        };
        let index = index as usize;
        if index == at {
            return drag.then_some(event);
        }
        if index > at {
            event.payload["index"] = serde_json::json!(index - 1);
        }
        Some(event)
    }

    /// When a plugin tree's preview next changes, for the loop to wake.
    pub(crate) fn tree_move_previews_deadline(&self) -> Option<Instant> {
        if !self.config.editor.animations {
            return None;
        }
        let now = self.time_source().now();
        self.tree_move_previews
            .iter()
            .filter_map(|p| p.clock.next_deadline(now))
            .min()
    }

    /// The spec and instance state `key`'s panel is drawn from, with any
    /// preview playing in one of its trees laid in. Untouched — the same
    /// handles — when none is.
    pub(crate) fn with_tree_move_previews(
        &self,
        key: &PanelKey,
        mut spec: std::rc::Rc<WidgetSpec>,
        mut states: HashMap<String, WidgetInstanceState>,
    ) -> (
        std::rc::Rc<WidgetSpec>,
        HashMap<String, WidgetInstanceState>,
    ) {
        if !self.config.editor.animations {
            return (spec, states);
        }
        let now = self.time_source().now();
        for p in self
            .tree_move_previews
            .iter()
            .filter(|p| &p.panel_key == key)
        {
            let typed = p.clock.typed(now);
            splice_tree(std::rc::Rc::make_mut(&mut spec), &mut states, p, typed);
        }
        (spec, states)
    }
}

/// Where `p`'s empty row goes in the tree it names inside `spec`, and at
/// what depth: directly under the `after_key` row, or at the top. `None` when
/// the tree, its source row, or the row it opens under is not there — a
/// preview that cannot show where the row lands shows nothing.
fn preview_insertion(spec: &WidgetSpec, p: &TreeMovePreview) -> Option<(usize, u32)> {
    let tree = crate::widgets::find_widget_by_key(spec, &p.widget_key)?;
    let WidgetSpec::Tree {
        nodes, item_keys, ..
    } = tree
    else {
        return None;
    };
    insertion_in(nodes, item_keys, p)
}

fn insertion_in(
    nodes: &[TreeNode],
    item_keys: &[String],
    p: &TreeMovePreview,
) -> Option<(usize, u32)> {
    item_keys.iter().position(|k| *k == p.source_key)?;
    match &p.after_key {
        Some(a) => {
            let i = item_keys.iter().position(|k| k == a)?;
            Some((i + 1, nodes[i].depth + 1))
        }
        None => Some((0, 0)),
    }
}

/// Lay `p`'s preview at `typed` characters into the tree it names inside
/// `spec`: the empty row in, the source row's name ghosting out, the row it
/// opens under open, and the tree's selection kept on the row it was on.
/// False when `spec` holds no such tree, or the tree no such source row.
pub(crate) fn splice_tree(
    spec: &mut WidgetSpec,
    states: &mut HashMap<String, WidgetInstanceState>,
    p: &TreeMovePreview,
    typed: usize,
) -> bool {
    let WidgetSpec::Tree {
        nodes,
        item_keys,
        key: Some(key),
        selected_index,
        expanded_keys,
        ..
    } = &mut *spec
    else {
        return spec
            .children_mut()
            .any(|c| splice_tree(c, states, p, typed));
    };
    if *key != p.widget_key {
        return false;
    }
    let Some((at, depth)) = insertion_in(nodes, item_keys, p) else {
        return false;
    };
    let src = item_keys
        .iter()
        .position(|k| *k == p.source_key)
        .expect("insertion_in found it");

    let nodes = std::sync::Arc::make_mut(nodes);
    let source = &mut nodes[src];
    // The column the name starts in on the source row, so the typed name
    // starts in the same one on the new row.
    let found = ghost(source, &p.label, typed);
    let phantom = TreeNode {
        text: typed_entry(found.unwrap_or(0), &p.label, typed),
        depth,
        has_children: false,
        flush: source.flush,
        draggable: false,
        checked: None,
        // A card keeps its height: the new row is as tall as the one moving.
        extra_lines: source
            .extra_lines
            .iter()
            .map(|_| TextPropertyEntry::text(""))
            .collect(),
        window_anchor: None,
        action: None,
        cells: Vec::new(),
    };
    nodes.insert(at, phantom);
    let phantom_key = format!("{PREVIEW_ROW_PREFIX}{}", p.source_key);
    std::sync::Arc::make_mut(item_keys).insert(at, phantom_key);

    // The selection is an index into `nodes`, so a row inserted above it
    // moves it down one — the spec's seed and the live state alike.
    let shift = |i: i32| match i >= at as i32 {
        true => i + 1,
        false => i,
    };
    *selected_index = shift(*selected_index);
    // A folded folder would hide the row opening in it.
    let opened = p.after_key.clone();
    if let Some(a) = &opened {
        if !expanded_keys.contains(a) {
            expanded_keys.push(a.clone());
        }
    }
    if let Some(WidgetInstanceState::Tree {
        selected_index,
        expanded_keys,
    }) = states.get_mut(&p.widget_key)
    {
        *selected_index = shift(*selected_index);
        if let Some(a) = opened {
            expanded_keys.insert(a);
        }
    }
    true
}

/// Ghost the first `typed` characters of `label` out of `node`'s row: the
/// first line of it that shows the label. Where the label starts on that
/// line, in display columns; `None` when no line shows it.
fn ghost(node: &mut TreeNode, label: &str, typed: usize) -> Option<usize> {
    if label.is_empty() {
        return None;
    }
    let (gone, _) = typed_split(label, typed);
    let lines = std::iter::once(&mut node.text).chain(node.extra_lines.iter_mut());
    for line in lines {
        // Segments resolved into text and overlays, so the label is found in
        // the text as drawn and the ghost lands over the plugin's own styling.
        line.normalize_widths();
        let Some(start) = line.text.find(label) else {
            continue;
        };
        if !gone.is_empty() {
            line.inline_overlays.push(InlineOverlay {
                start,
                end: start + gone.len(),
                style: OverlayOptions {
                    bg: Some(OverlayColorSpec::theme_key(GHOST_BG)),
                    ..Default::default()
                },
                properties: HashMap::new(),
                unit: OffsetUnit::Byte,
            });
        }
        return Some(crate::primitives::display_width::str_width(
            &line.text[..start],
        ));
    }
    None
}

/// The new row's text: blank up to display column `col`, then the typed part of
/// `label` in the addition's colours.
fn typed_entry(col: usize, label: &str, typed: usize) -> TextPropertyEntry {
    let mut entry = TextPropertyEntry::text("");
    entry.segments = std::iter::once(StyledSegment {
        text: " ".repeat(col),
        style: None,
        overlays: Vec::new(),
    })
    .chain(
        typed_runs(label, typed)
            .into_iter()
            .map(|(text, bg)| StyledSegment {
                text: text.to_string(),
                style: Some(OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key("editor.fg")),
                    bg: Some(OverlayColorSpec::theme_key(bg)),
                    ..Default::default()
                }),
                overlays: Vec::new(),
            }),
    )
    .collect();
    entry
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(text: &str, depth: u32) -> TreeNode {
        TreeNode {
            text: TextPropertyEntry::text(text),
            depth,
            has_children: false,
            flush: false,
            draggable: true,
            checked: None,
            extra_lines: Vec::new(),
            window_anchor: None,
            action: None,
            cells: Vec::new(),
        }
    }

    fn tree(selected: i32) -> WidgetSpec {
        let mut folder = node("Work", 0);
        folder.has_children = true;
        WidgetSpec::Tree {
            nodes: std::sync::Arc::new(vec![node("· alpha", 0), folder, node("· beta", 0)]),
            item_keys: std::sync::Arc::new(vec!["a".into(), "f".into(), "b".into()]),
            selected_index: selected,
            visible_rows: None,
            key: Some("sessions".into()),
            expanded_keys: Vec::new(),
            checkable: false,
            indent_cols: 2,
            item_height: 1,
            card_borders: false,
            toggle_on_click: false,
            columns: Vec::new(),
        }
    }

    fn preview(after: Option<&str>) -> TreeMovePreview {
        TreeMovePreview {
            panel_key: PanelKey {
                plugin: "p".into(),
                id: 1,
            },
            widget_key: "sessions".into(),
            source_key: "b".into(),
            after_key: after.map(str::to_string),
            label: "beta".into(),
            clock: MovePreview::new(["beta"], Instant::now()),
            committed: false,
        }
    }

    #[test]
    fn the_empty_row_opens_under_the_folder_and_types_where_the_name_was() {
        let mut spec = tree(2);
        let mut states = HashMap::new();
        assert!(splice_tree(&mut spec, &mut states, &preview(Some("f")), 2));
        let WidgetSpec::Tree {
            nodes,
            item_keys,
            selected_index,
            expanded_keys,
            ..
        } = &spec
        else {
            unreachable!()
        };
        assert_eq!(nodes.len(), 4);
        assert_eq!(item_keys[2], "\u{0}move-preview:b");
        let mut new_row = nodes[2].text.clone();
        new_row.normalize_widths();
        assert_eq!(
            new_row.text, "  be",
            "blank to the name's column, then typed"
        );
        assert_eq!(nodes[2].depth, 1);
        // The source keeps its text; its first two characters ghost.
        let src = &nodes[3].text;
        assert_eq!(src.text, "· beta");
        let o = src.inline_overlays.last().expect("a ghost overlay");
        assert_eq!(&src.text[o.start..o.end], "be");
        assert_eq!(*selected_index, 3, "the selection stays on beta");
        assert_eq!(expanded_keys, &vec!["f".to_string()]);
    }

    #[test]
    fn nothing_is_typed_or_ghosted_while_the_row_opens() {
        let mut spec = tree(0);
        let mut states = HashMap::new();
        assert!(splice_tree(&mut spec, &mut states, &preview(None), 0));
        let WidgetSpec::Tree {
            nodes,
            selected_index,
            ..
        } = &spec
        else {
            unreachable!()
        };
        assert_eq!(nodes[0].depth, 0, "at the top of the tree");
        assert!(nodes[3].text.inline_overlays.is_empty());
        assert_eq!(*selected_index, 1);
    }

    #[test]
    fn a_preview_whose_folder_is_gone_shows_nothing() {
        let mut spec = tree(0);
        assert!(!splice_tree(
            &mut spec,
            &mut HashMap::new(),
            &preview(Some("hidden")),
            1
        ));
    }

    #[test]
    fn a_tree_without_the_source_row_is_left_alone() {
        let mut spec = tree(0);
        let mut p = preview(None);
        p.source_key = "gone".into();
        assert!(!splice_tree(&mut spec, &mut HashMap::new(), &p, 1));
    }
}
