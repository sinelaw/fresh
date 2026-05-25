//! Pure helpers used by `WidgetCommand` dispatch.
//!
//! These are factored out of the plugin-dispatch module so they can
//! be tested without spinning up an `Editor`. The widget runtime's
//! state mutations are intentionally pure functions of (current
//! widget state, requested action) → next state — the dispatcher
//! reads from the registry, calls these, and fires events.

use fresh_core::api::{TreeNode, WidgetSpec};
use fresh_core::text_property::TextPropertyEntry;

/// Locate a widget node in a spec tree by its stable `key`. Returns
/// the matched node, or `None` if no widget has that key.
///
/// Walks `Row`/`Col` children. Skips `Raw`/`HintBar`/`Spacer` (those
/// kinds either have no key worth dispatching to, or no interactive
/// behaviour at all).
pub fn find_widget_by_key<'a>(spec: &'a WidgetSpec, target: &str) -> Option<&'a WidgetSpec> {
    if target.is_empty() {
        return None;
    }
    if leaf_key_matches(spec, target) {
        return Some(spec);
    }
    spec.children().find_map(|c| find_widget_by_key(c, target))
}

/// True iff `spec` is a focusable leaf kind whose `key` equals
/// `target`. Container kinds (`Row`, `Col`, `LabeledSection`)
/// never match — tree walkers handle descent generically through
/// [`WidgetSpec::children`] and only call this on each node to
/// see whether the search terminates there.
fn leaf_key_matches(spec: &WidgetSpec, target: &str) -> bool {
    match spec {
        WidgetSpec::Toggle { key: Some(k), .. }
        | WidgetSpec::Button { key: Some(k), .. }
        | WidgetSpec::Text { key: Some(k), .. }
        | WidgetSpec::List { key: Some(k), .. }
        | WidgetSpec::Tree { key: Some(k), .. } => k == target,
        _ => false,
    }
}

/// In-place mutate a `Toggle`'s `checked` field by walking the
/// spec tree and matching on `widget_key`. Used by the
/// `WidgetMutate::SetChecked` IPC fast path.
///
/// Returns true when a matching Toggle was found and updated.
pub fn set_toggle_checked_in_spec(
    spec: &mut WidgetSpec,
    widget_key: &str,
    new_checked: bool,
) -> bool {
    if widget_key.is_empty() {
        return false;
    }
    if let WidgetSpec::Toggle { checked, key, .. } = spec {
        if key.as_deref() == Some(widget_key) {
            *checked = new_checked;
            return true;
        }
    }
    spec.children_mut()
        .any(|c| set_toggle_checked_in_spec(c, widget_key, new_checked))
}

/// In-place mutate a `List`'s `items` and `item_keys` fields.
/// Returns true when a matching List was found and updated.
pub fn set_list_items_in_spec(
    spec: &mut WidgetSpec,
    widget_key: &str,
    new_items: Vec<TextPropertyEntry>,
    new_item_keys: Vec<String>,
) -> bool {
    if widget_key.is_empty() {
        return false;
    }
    if let WidgetSpec::List {
        items,
        item_keys,
        key,
        ..
    } = spec
    {
        if key.as_deref() == Some(widget_key) {
            *items = new_items;
            *item_keys = new_item_keys;
            return true;
        }
    }
    // Descend only into the child that contains the target so
    // `new_items` / `new_item_keys` aren't dropped on dead-end
    // siblings (we take them by value, so each recursive call
    // moves them).
    for c in spec.children_mut() {
        if c.contains_key(widget_key) {
            return set_list_items_in_spec(c, widget_key, new_items, new_item_keys);
        }
    }
    false
}

/// In-place mutate a `Tree`'s `nodes` and `item_keys` fields.
/// Returns true when a matching Tree was found and updated.
///
/// Note: this does *not* touch instance state (selected_index,
/// scroll, expanded_keys). The renderer will clamp the previous
/// selection to a now-visible node and orphan-discard expanded
/// keys that no longer match any item key on the next render.
pub fn set_tree_nodes_in_spec(
    spec: &mut WidgetSpec,
    widget_key: &str,
    new_nodes: Vec<TreeNode>,
    new_item_keys: Vec<String>,
) -> bool {
    if widget_key.is_empty() {
        return false;
    }
    if let WidgetSpec::Tree {
        nodes,
        item_keys,
        key,
        ..
    } = spec
    {
        if key.as_deref() == Some(widget_key) {
            *nodes = new_nodes;
            *item_keys = new_item_keys;
            return true;
        }
    }
    for c in spec.children_mut() {
        if c.contains_key(widget_key) {
            return set_tree_nodes_in_spec(c, widget_key, new_nodes, new_item_keys);
        }
    }
    false
}

/// Append `new_nodes` (and their item keys) to an existing Tree's
/// node list, preserving everything already present. Returns true
/// when a matching Tree was found and extended.
///
/// Existing instance state (selection, scroll, expansion) is untouched —
/// the renderer keeps the user's current scroll position even as new
/// items appear at the tail.
///
/// Used by `WidgetMutation::AppendTreeNodes` to make streaming updates
/// cheap: a project-wide search reporting thousands of matches sends
/// only the per-batch delta instead of re-transmitting the full tree
/// on every pump tick. The plugin tracks how many nodes it last sent
/// and emits only the new tail.
pub fn append_tree_nodes_in_spec(
    spec: &mut WidgetSpec,
    widget_key: &str,
    new_nodes: Vec<TreeNode>,
    new_item_keys: Vec<String>,
) -> bool {
    if widget_key.is_empty() {
        return false;
    }
    if let WidgetSpec::Tree {
        nodes,
        item_keys,
        key,
        ..
    } = spec
    {
        if key.as_deref() == Some(widget_key) {
            nodes.extend(new_nodes);
            item_keys.extend(new_item_keys);
            return true;
        }
    }
    for c in spec.children_mut() {
        if c.contains_key(widget_key) {
            return append_tree_nodes_in_spec(c, widget_key, new_nodes, new_item_keys);
        }
    }
    false
}

/// Replace a `Raw` widget's `entries` in place. Returns true when a
/// matching `Raw` was found and updated.
///
/// Used by `WidgetMutation::SetRawEntries` to refresh small bits of
/// panel chrome (the matchStats label, separator counts) without a
/// full spec re-emit. Re-emitting was the killer in search-replace's
/// `batch.done`: re-walking and re-serializing a 5 000-node Tree just
/// to update a 30-byte label string blocked the JS thread for ~1
/// second while queued user input piled up in the request channel.
pub fn set_raw_entries_in_spec(
    spec: &mut WidgetSpec,
    widget_key: &str,
    new_entries: Vec<TextPropertyEntry>,
) -> bool {
    if widget_key.is_empty() {
        return false;
    }
    if let WidgetSpec::Raw { entries, key } = spec {
        if key.as_deref() == Some(widget_key) {
            *entries = new_entries;
            return true;
        }
    }
    for c in spec.children_mut() {
        if c.contains_key(widget_key) {
            return set_raw_entries_in_spec(c, widget_key, new_entries);
        }
    }
    false
}

/// Stamp `Some(checked)` onto every `TreeNode` whose item-key
/// appears in `keys`. Used by `WidgetMutation::SetCheckedKeys` —
/// the host writes the new checkbox state into the spec so the
/// next render reflects it without round-tripping through the
/// plugin. Nodes not in `keys` are unchanged. Nodes whose
/// existing `checked` is `None` are left as `None` (a node only
/// becomes checkable by the plugin emitting `Some(_)` in the
/// spec).
///
/// Returns true if the named tree was found.
pub fn set_tree_checked_keys_in_spec(
    spec: &mut WidgetSpec,
    widget_key: &str,
    checked: bool,
    keys: &[String],
) -> bool {
    if widget_key.is_empty() {
        return false;
    }
    if let WidgetSpec::Tree {
        nodes,
        item_keys,
        key,
        ..
    } = spec
    {
        if key.as_deref() == Some(widget_key) {
            let target: std::collections::HashSet<&str> = keys.iter().map(String::as_str).collect();
            for (i, node) in nodes.iter_mut().enumerate() {
                if node.checked.is_none() {
                    continue;
                }
                let item_key = item_keys.get(i).map(String::as_str).unwrap_or("");
                if !item_key.is_empty() && target.contains(item_key) {
                    node.checked = Some(checked);
                }
            }
            return true;
        }
    }
    spec.children_mut()
        .any(|c| set_tree_checked_keys_in_spec(c, widget_key, checked, keys))
}

/// Resolve the absolute `nodes` index of the parent of `child_idx`
/// in a `Tree`. The parent of a node at depth `d` is the most recent
/// earlier node at depth `d - 1`. Returns `None` for top-level nodes
/// (depth 0) and for out-of-range indices.
pub fn tree_parent_index(nodes: &[TreeNode], child_idx: usize) -> Option<usize> {
    let child = nodes.get(child_idx)?;
    if child.depth == 0 {
        return None;
    }
    let target_depth = child.depth - 1;
    nodes[..child_idx]
        .iter()
        .enumerate()
        .rev()
        .find(|(_, n)| n.depth == target_depth)
        .map(|(i, _)| i)
}

/// Recursive helper for `set_*_in_spec` — does this
/// subtree contain a widget (any kind) with `widget_key`?
trait ContainsKey {
    fn contains_key(&self, widget_key: &str) -> bool;
}

impl ContainsKey for WidgetSpec {
    fn contains_key(&self, widget_key: &str) -> bool {
        let direct = match self {
            WidgetSpec::Toggle { key, .. }
            | WidgetSpec::Button { key, .. }
            | WidgetSpec::Text { key, .. }
            | WidgetSpec::List { key, .. }
            | WidgetSpec::Tree { key, .. }
            | WidgetSpec::Raw { key, .. } => key.as_deref() == Some(widget_key),
            _ => false,
        };
        direct || self.children().any(|c| c.contains_key(widget_key))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn toggle_with_key(k: &str) -> WidgetSpec {
        WidgetSpec::Toggle {
            checked: false,
            label: "T".into(),
            focused: false,
            key: Some(k.into()),
        }
    }

    #[test]
    fn find_widget_by_key_finds_top_level_match() {
        let spec = toggle_with_key("a");
        assert!(find_widget_by_key(&spec, "a").is_some());
        assert!(find_widget_by_key(&spec, "b").is_none());
    }

    #[test]
    fn find_widget_by_key_recurses_into_row() {
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![toggle_with_key("a"), toggle_with_key("b")],
            key: None,
        };
        assert!(find_widget_by_key(&spec, "b").is_some());
    }

    #[test]
    fn find_widget_by_key_returns_none_for_empty_target() {
        let spec = toggle_with_key("a");
        assert!(find_widget_by_key(&spec, "").is_none());
    }

    fn node(text: &str, depth: u32, has_children: bool) -> TreeNode {
        TreeNode {
            text: TextPropertyEntry::text(text),
            depth,
            has_children,
            checked: None,
        }
    }

    #[test]
    fn tree_parent_index_top_level_returns_none() {
        let nodes = vec![node("root", 0, true)];
        assert!(tree_parent_index(&nodes, 0).is_none());
    }

    #[test]
    fn tree_parent_index_finds_immediate_parent() {
        let nodes = vec![
            node("root", 0, true),
            node("child", 1, false),
            node("child2", 1, false),
        ];
        assert_eq!(tree_parent_index(&nodes, 1), Some(0));
        assert_eq!(tree_parent_index(&nodes, 2), Some(0));
    }

    #[test]
    fn tree_parent_index_skips_intermediate_siblings() {
        // root, child, grandchild → grandchild's parent is child (idx 1).
        let nodes = vec![
            node("root", 0, true),
            node("child", 1, true),
            node("grand", 2, false),
        ];
        assert_eq!(tree_parent_index(&nodes, 2), Some(1));
    }

    #[test]
    fn tree_parent_index_finds_parent_across_unrelated_subtree() {
        // root_a, child_a, root_b, child_b — child_b's parent is root_b (idx 2),
        // not root_a.
        let nodes = vec![
            node("a", 0, true),
            node("a.0", 1, false),
            node("b", 0, true),
            node("b.0", 1, false),
        ];
        assert_eq!(tree_parent_index(&nodes, 3), Some(2));
    }

    #[test]
    fn set_tree_nodes_in_spec_replaces_nodes() {
        let mut spec = WidgetSpec::Tree {
            nodes: vec![node("old", 0, false)],
            item_keys: vec!["k0".into()],
            selected_index: -1,
            visible_rows: 5,
            expanded_keys: vec![],
            checkable: false,
            key: Some("t".into()),
        };
        let new_nodes = vec![node("new1", 0, false), node("new2", 0, false)];
        let new_keys = vec!["a".to_string(), "b".to_string()];
        let ok = set_tree_nodes_in_spec(&mut spec, "t", new_nodes.clone(), new_keys.clone());
        assert!(ok);
        match &spec {
            WidgetSpec::Tree {
                nodes, item_keys, ..
            } => {
                assert_eq!(nodes.len(), 2);
                assert_eq!(item_keys, &new_keys);
            }
            _ => unreachable!(),
        }
    }

    // ---- TextArea key tests were here — the editing primitives are
    // now `TextEdit` methods, covered by tests in
    // `crates/fresh-editor/src/primitives/text_edit.rs`.

    #[test]
    fn set_tree_checked_keys_in_spec_flips_only_named_keys() {
        let mut a = node("a", 0, false);
        a.checked = Some(true);
        let mut b = node("b", 0, false);
        b.checked = Some(true);
        let mut c = node("c", 0, false);
        c.checked = Some(true);
        let mut spec = WidgetSpec::Tree {
            nodes: vec![a, b, c],
            item_keys: vec!["k_a".into(), "k_b".into(), "k_c".into()],
            selected_index: -1,
            visible_rows: 5,
            expanded_keys: vec![],
            checkable: true,
            key: Some("t".into()),
        };
        let ok = set_tree_checked_keys_in_spec(
            &mut spec,
            "t",
            false,
            &["k_a".to_string(), "k_c".to_string()],
        );
        assert!(ok);
        match &spec {
            WidgetSpec::Tree { nodes, .. } => {
                assert_eq!(nodes[0].checked, Some(false));
                assert_eq!(nodes[1].checked, Some(true), "untouched");
                assert_eq!(nodes[2].checked, Some(false));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn set_tree_checked_keys_in_spec_skips_nodes_without_checkbox() {
        // Nodes with `checked: None` must stay None even when their
        // key is in the target list — the plugin's intent ("this
        // node has no checkbox") is preserved.
        let n_with = {
            let mut n = node("checked", 0, false);
            n.checked = Some(true);
            n
        };
        let n_without = node("no-checkbox", 0, false); // checked: None
        let mut spec = WidgetSpec::Tree {
            nodes: vec![n_with, n_without],
            item_keys: vec!["k0".into(), "k1".into()],
            selected_index: -1,
            visible_rows: 5,
            expanded_keys: vec![],
            checkable: true,
            key: Some("t".into()),
        };
        let _ok = set_tree_checked_keys_in_spec(
            &mut spec,
            "t",
            false,
            &["k0".to_string(), "k1".to_string()],
        );
        match &spec {
            WidgetSpec::Tree { nodes, .. } => {
                assert_eq!(nodes[0].checked, Some(false));
                assert_eq!(nodes[1].checked, None);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn set_tree_nodes_in_spec_returns_false_for_unknown_key() {
        let mut spec = WidgetSpec::Tree {
            nodes: vec![node("a", 0, false)],
            item_keys: vec!["k".into()],
            selected_index: -1,
            visible_rows: 5,
            expanded_keys: vec![],
            checkable: false,
            key: Some("real".into()),
        };
        assert!(!set_tree_nodes_in_spec(&mut spec, "wrong", vec![], vec![]));
    }
}
