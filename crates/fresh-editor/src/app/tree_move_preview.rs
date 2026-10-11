//! A drop's move preview in a plugin's `Tree` (`WidgetMutation::PreviewTreeMove`).
//!
//! The same preview the file explorer plays before it asks about a move
//! (`view::move_preview`), for a tree whose rows are a plugin's: the
//! orchestrator dock's, when a workspace is dragged onto a folder. The plugin
//! names the row that moves, the row it lands under and the name to type;
//! the host lays the preview into the spec the panel is drawn from — never
//! into the plugin's own copy — and, when it has played out, tells the plugin
//! with `movepreviewend` so it makes the move.

use std::collections::HashMap;
use std::time::Instant;

use fresh_core::api::{OverlayColorSpec, OverlayOptions, TreeNode, WidgetSpec};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, StyledSegment, TextPropertyEntry};

use crate::view::move_preview::{typed_runs, typed_split, MovePreview, GHOST_BG};
use crate::widgets::{PanelKey, WidgetInstanceState};

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
}

impl crate::app::Editor {
    /// Start `label`'s preview in `widget_key`'s tree. A preview already
    /// playing in that tree ends first, so its move is not lost; with
    /// animations off this one ends at once too.
    pub(crate) fn start_tree_move_preview(
        &mut self,
        panel_key: &PanelKey,
        widget_key: String,
        source_key: String,
        after_key: Option<String>,
        label: String,
    ) {
        if let Some(i) = self
            .tree_move_previews
            .iter()
            .position(|p| &p.panel_key == panel_key && p.widget_key == widget_key)
        {
            let earlier = self.tree_move_previews.remove(i);
            self.end_tree_move_preview(earlier);
        }
        let preview = TreeMovePreview {
            panel_key: panel_key.clone(),
            widget_key,
            source_key,
            after_key,
            clock: MovePreview::new([label.as_str()], self.time_source().now()),
            label,
        };
        if !self.config.editor.animations {
            self.end_tree_move_preview(preview);
            return;
        }
        self.tree_move_previews.push(preview);
        self.shell_description_stale = true;
    }

    /// The preview is over: the panel is drawn from its own spec again, and
    /// the plugin hears `movepreviewend` to make the move.
    fn end_tree_move_preview(&mut self, p: TreeMovePreview) {
        self.fire_widget_event(
            &p.panel_key,
            p.widget_key,
            "movepreviewend".to_string(),
            serde_json::json!({ "key": p.source_key }),
        );
        self.shell_description_stale = true;
    }

    /// End every preview that has played out. True while any is in flight,
    /// because each tick of one is a frame.
    pub(crate) fn step_tree_move_previews(&mut self) -> bool {
        if self.tree_move_previews.is_empty() {
            return false;
        }
        let now = self.time_source().now();
        let (done, playing): (Vec<_>, Vec<_>) = std::mem::take(&mut self.tree_move_previews)
            .into_iter()
            .partition(|p| p.clock.finished(now));
        self.tree_move_previews = playing;
        for p in done {
            self.end_tree_move_preview(p);
        }
        true
    }

    /// When a plugin tree's preview next changes, for the loop to wake.
    pub(crate) fn tree_move_previews_deadline(&self) -> Option<Instant> {
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
    let Some(src) = item_keys.iter().position(|k| *k == p.source_key) else {
        return false;
    };
    let after = p
        .after_key
        .as_ref()
        .and_then(|a| item_keys.iter().position(|k| k == a));
    let (at, depth) = match after {
        Some(a) => (a + 1, nodes[a].depth + 1),
        None => (0, 0),
    };

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
    let phantom_key = format!("\u{0}move-preview:{}", p.source_key);
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
/// line, in characters; `None` when no line shows it.
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
        return Some(line.text[..start].chars().count());
    }
    None
}

/// The new row's text: blank up to column `col`, then the typed part of
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
    fn a_tree_without_the_source_row_is_left_alone() {
        let mut spec = tree(0);
        let mut p = preview(None);
        p.source_key = "gone".into();
        assert!(!splice_tree(&mut spec, &mut HashMap::new(), &p, 1));
    }
}
