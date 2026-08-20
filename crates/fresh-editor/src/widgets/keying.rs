//! The per-item keys a `List`/`Tree` will be *required* to carry.
//!
//! Identity in the widget runtime today is a manual walk that matches the
//! previous spec against the new one; when a panel moves onto `fresh-ui`
//! (wave M6 of `docs/internal/fresh-editor-ui-migration.md`) it becomes the
//! library's `(type, key)` reconciliation, and a list whose items have no keys
//! cannot be reconciled — every row would share the key `""`, so state that
//! belongs to a row (scroll position, expansion, selection) has nothing to
//! belong to.
//!
//! `item_keys` is optional today and defaults to empty. This audit is the
//! deprecation half of that change, deliberately shipped **ahead** of the
//! migration: it warns, per panel and once per offending widget, that keys
//! will be required — so the release that makes them required is not also the
//! release plugins first hear about it.
//!
//! Warnings are attributed to the plugin and the widget's own `key` where it
//! has one, because "some list somewhere is unkeyed" is not actionable.

use std::collections::HashSet;

use fresh_core::api::WidgetSpec;

use super::registry::PanelKey;

/// One unkeyed collection widget: what it is, and where to find it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Unkeyed {
    /// `"List"` or `"Tree"`.
    pub kind: &'static str,
    /// The widget's own `key`, when it has one — the handle a plugin author
    /// can search for. `None` means the widget is unkeyed too, and the path
    /// below is all there is to go on.
    pub widget_key: Option<String>,
    /// Position in the spec tree, as a `/`-joined path of kind names, so an
    /// anonymous widget can still be located.
    pub path: String,
    /// How many items carry no key. A short `item_keys` array is as broken as
    /// an absent one — the tail collapses onto `""`.
    pub unkeyed_items: usize,
}

/// Every `List`/`Tree` in `spec` whose items are not fully keyed.
pub fn unkeyed_collections(spec: &WidgetSpec) -> Vec<Unkeyed> {
    let mut out = Vec::new();
    walk(spec, &mut Vec::new(), &mut out);
    out
}

fn walk(spec: &WidgetSpec, path: &mut Vec<&'static str>, out: &mut Vec<Unkeyed>) {
    let here = kind_name(spec);
    path.push(here);
    match spec {
        WidgetSpec::List {
            items,
            item_specs,
            item_keys,
            key,
            ..
        } => {
            // `item_specs` overrides `items` when non-empty; the list still
            // counts in item units, so that is what needs keys.
            let count = if item_specs.is_empty() {
                items.len()
            } else {
                item_specs.len()
            };
            report(count, item_keys, "List", key, path, out);
            for child in item_specs {
                walk(child, path, out);
            }
        }
        WidgetSpec::Tree {
            nodes,
            item_keys,
            key,
            ..
        } => {
            // `TreeNode` is already flat (it carries its own `depth`), so the
            // node list *is* the item list `item_keys` parallels.
            report(nodes.len(), item_keys, "Tree", key, path, out);
        }
        WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
            for child in children {
                walk(child, path, out);
            }
        }
        WidgetSpec::LabeledSection { child, .. }
        | WidgetSpec::Overlay { child, .. }
        | WidgetSpec::Component { child, .. }
        | WidgetSpec::Popup { child, .. } => walk(child, path, out),
        _ => {}
    }
    path.pop();
}

fn report(
    count: usize,
    item_keys: &[String],
    kind: &'static str,
    key: &Option<String>,
    path: &[&'static str],
    out: &mut Vec<Unkeyed>,
) {
    if count == 0 {
        return;
    }
    // An empty string is not a key: it is what a missing one decays to, and
    // two of them are the same key.
    let keyed = item_keys
        .iter()
        .take(count)
        .filter(|k| !k.is_empty())
        .count();
    if keyed == count {
        return;
    }
    out.push(Unkeyed {
        kind,
        widget_key: key.clone(),
        path: path.join("/"),
        unkeyed_items: count - keyed,
    });
}

fn kind_name(spec: &WidgetSpec) -> &'static str {
    match spec {
        WidgetSpec::Row { .. } => "Row",
        WidgetSpec::Col { .. } => "Col",
        WidgetSpec::LabeledSection { .. } => "LabeledSection",
        WidgetSpec::Overlay { .. } => "Overlay",
        WidgetSpec::Component { .. } => "Component",
        WidgetSpec::Popup { .. } => "Popup",
        WidgetSpec::List { .. } => "List",
        WidgetSpec::Tree { .. } => "Tree",
        _ => "widget",
    }
}

/// Warn about each unkeyed collection in a freshly mounted or updated panel,
/// once per `(panel, widget)`.
///
/// Once per panel rather than once per process: a plugin that fixes one list
/// and leaves another should still hear about the second, and a panel that
/// re-mounts with the same defect should not re-flood the log.
pub fn warn_unkeyed(seen: &mut HashSet<(String, String)>, panel: &PanelKey, spec: &WidgetSpec) {
    for u in unkeyed_collections(spec) {
        let where_ = match &u.widget_key {
            Some(k) => format!("{} key={k:?}", u.kind),
            None => format!("{} at {}", u.kind, u.path),
        };
        if !seen.insert((panel.plugin.clone(), where_.clone())) {
            continue;
        }
        tracing::warn!(
            "plugin {:?}: {where_} has {} item(s) without a stable key. \
             `itemKeys` is optional today and will become required: per-item \
             state (scroll, selection, tree expansion) is keyed by it, and \
             items sharing the empty key cannot keep their own. Pass one \
             stable string per item.",
            panel.plugin,
            u.unkeyed_items,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn list(items: usize, keys: &[&str], key: Option<&str>) -> WidgetSpec {
        WidgetSpec::List {
            items: (0..items)
                .map(|i| fresh_core::text_property::TextPropertyEntry::text(format!("row {i}")))
                .collect(),
            item_specs: Vec::new(),
            item_keys: keys.iter().map(|s| s.to_string()).collect(),
            selected_index: -1,
            visible_rows: None,
            focusable: true,
            key: key.map(str::to_string),
        }
    }

    /// A fully keyed list is silent — the audit must not cry wolf about the
    /// plugins that already do the right thing.
    #[test]
    fn a_fully_keyed_list_is_not_reported() {
        assert!(unkeyed_collections(&list(2, &["a", "b"], None)).is_empty());
    }

    /// No keys at all: every item is unkeyed.
    #[test]
    fn an_unkeyed_list_reports_every_item() {
        let found = unkeyed_collections(&list(3, &[], Some("results")));
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].kind, "List");
        assert_eq!(found[0].widget_key.as_deref(), Some("results"));
        assert_eq!(found[0].unkeyed_items, 3);
    }

    /// **A short array is as broken as an absent one.** The tail decays to the
    /// empty key, and two empty keys are the same key — which is exactly the
    /// collision the reconciler cannot resolve.
    #[test]
    fn a_short_key_array_reports_its_tail() {
        let found = unkeyed_collections(&list(3, &["a"], None));
        assert_eq!(found[0].unkeyed_items, 2);
    }

    /// An empty string is not a key.
    #[test]
    fn empty_strings_do_not_count_as_keys() {
        let found = unkeyed_collections(&list(2, &["a", ""], None));
        assert_eq!(found[0].unkeyed_items, 1);
    }

    /// An empty list has nothing to key.
    #[test]
    fn an_empty_list_is_not_reported() {
        assert!(unkeyed_collections(&list(0, &[], None)).is_empty());
    }

    /// Nested containers are walked, and an anonymous widget is still locatable
    /// by its path.
    #[test]
    fn nesting_is_walked_and_anonymous_widgets_keep_a_path() {
        let spec = WidgetSpec::Col {
            children: vec![WidgetSpec::LabeledSection {
                label: "Results".into(),
                child: Box::new(list(1, &[], None)),
                width_pct: None,
                key: None,
            }],
            key: None,
        };
        let found = unkeyed_collections(&spec);
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].path, "Col/LabeledSection/List");
    }
}
