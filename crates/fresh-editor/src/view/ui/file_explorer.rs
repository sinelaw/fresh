use crate::input::fuzzy::FuzzyMatch;
use crate::primitives::display_width::str_width;
use crate::view::file_tree::view::VisibleRow;
use crate::view::file_tree::ExplorerSlotContext;
use crate::view::theme::Theme;

use std::collections::HashSet;
use std::path::PathBuf;

/// Whether anything unsaved lives under this folder.
///
/// All that is left of `FileExplorerRenderer`, whose `render`,
/// `render_loading`, `panel_title`, `panel_chrome_styles`,
/// `render_close_button`, `build_node_line` and `trailing_slot_screen_bounds`
/// went when the panel became a native region in the shell's tree. The type
/// outlived them as a namespace around this one predicate, which is a
/// function about paths and belongs beside the row that asks it.
fn folder_has_modified_files(
    folder_path: &PathBuf,
    files_with_unsaved_changes: &HashSet<PathBuf>,
) -> bool {
    for modified_file in files_with_unsaved_changes {
        if modified_file.starts_with(folder_path) {
            return true;
        }
    }
    false
}

/// Everything one row needs to describe itself.
pub struct RowDesc<'a> {
    /// The node, as the tree's projection saw it.
    pub node: &'a VisibleRow,
    /// The row's index in the tree's display order — its key, what
    /// hit-testing answers with, and the unit the window counts in.
    pub row: usize,
    pub is_cursor: bool,
    pub is_multi: bool,
    pub focused: bool,
    pub unsaved: &'a HashSet<PathBuf>,
    pub cut: &'a [PathBuf],
    /// The directory a held drag would drop into, if the pointer is over one.
    pub drop_target: Option<&'a std::path::Path>,
    pub fuzzy: Option<&'a FuzzyMatch>,
    pub decorations: &'a crate::view::file_tree::FileExplorerDecorationCache,
    pub slot_overrides: &'a crate::view::file_tree::FileExplorerSlotOverrideCache,
    pub slot_resolver: &'a crate::view::file_tree::ExplorerSlotResolver<'static>,
    pub theme: &'a Theme,
    pub collapsed: &'a str,
    pub expanded: &'a str,
}

/// One row of the tree, as the shell describes it.
///
/// This is [`FileExplorerRenderer::build_node_line`] with the arithmetic taken
/// out. It still decides *what the row says* — the indicator glyph and its
/// padding, the leading slot and its padding, the compacted ancestor chain,
/// the name and its fuzzy-match highlights, the trailing status slot and the
/// error marker — and it still decides what each piece looks like, but now as
/// a theme *name* rather than a resolved `Color`.
///
/// What it no longer decides is where anything sits. `content_width`,
/// `left_side_width`, `total_right_width` and the `padding` rule are gone: the
/// gap between the name and the status slot is a flex spacer with a floor, and
/// the tree measures it. So is `trailing_slot_screen_bounds`, the 45-line
/// second derivation that existed only so a hover could find the slot the
/// painter had already placed.
pub fn describe_row(d: RowDesc<'_>) -> crate::view::shell::file_explorer::Row {
    use crate::app::shell_host::shell_theme::{literal, pair, Paint};
    use crate::view::shell::file_explorer as fe;

    // A slot color as a name: the theme key it came from, so the key's text
    // attributes apply too, over the resolved color for a key the theme does
    // not know; or the color itself when no key named it.
    let slot_paint = |fg: ratatui::style::Color, key: &Option<String>| match key {
        Some(k) => Paint::asked(k.clone(), Paint::Lit(fg)).to_string(),
        None => literal(fg),
    };

    let node = d.node;
    let is_hidden = node
        .entry
        .metadata
        .as_ref()
        .map(|m| m.is_hidden)
        .unwrap_or(false);
    let neutral = fe::neutral_key(is_hidden, node.entry.is_symlink(), node.is_dir());
    // Three ways to wear the selected ground: the keyboard is on this row, the
    // row is in the reader's set, or a held drag would land here. The drop
    // target comes first because it outranks a cursor sitting elsewhere — it
    // is the only one of the three that is about the pointer.
    let selected = d.drop_target == Some(node.entry.path.as_path())
        || (d.focused && (d.is_cursor || d.is_multi));
    let ground = if selected {
        "editor.selection_bg"
    } else if d.is_cursor {
        "editor.current_line_bg"
    } else {
        "editor.bg"
    };

    let has_unsaved = if node.is_dir() {
        folder_has_modified_files(&node.entry.path, d.unsaved)
    } else {
        d.unsaved.contains(&node.entry.path)
    };
    let slots = d.slot_resolver.resolve(&ExplorerSlotContext {
        path: &node.entry.path,
        is_dir: node.is_dir(),
        has_unsaved,
        is_symlink: node.entry.is_symlink(),
        is_hidden,
        decorations: d.decorations,
        slot_overrides: d.slot_overrides,
        theme: d.theme,
        // The neutral colour the slot providers fall back to. They still work
        // in `Color`; only the description speaks in names.
        neutral_fg: d
            .theme
            .resolve_theme_key(neutral)
            .unwrap_or(d.theme.editor_fg),
    });

    let is_cut = d.cut.iter().any(|p| p == &node.entry.path);
    let name_fg = if is_cut {
        "editor.line_number_fg".to_string()
    } else if let Some(c) = slots.name_color_hint {
        slot_paint(c, &slots.name_color_key)
    } else if (d.is_cursor || d.is_multi) && d.focused {
        "editor.fg".to_string()
    } else {
        neutral.to_string()
    };

    let mut left: fe::Runs = Vec::new();
    if node.indent > 0 {
        left.push((" ".repeat(node.indent * 2), pair(neutral, ground)));
    }

    // The indicator column is sized from the configured glyphs so names stay
    // aligned when a user picks a wider one.
    let collapsed_w = str_width(d.collapsed);
    let expanded_w = str_width(d.expanded);
    let indicator_width = collapsed_w.max(expanded_w).max(1) + 1;
    if node.is_dir() {
        use crate::view::file_tree::NodeState;
        let (glyph, w) = if node.state == NodeState::Expanded {
            (format!("{} ", d.expanded), expanded_w + 1)
        } else if node.state == NodeState::Collapsed {
            (format!("{} ", d.collapsed), collapsed_w + 1)
        } else if node.state == NodeState::Loading {
            ("⟳ ".to_string(), 2)
        } else {
            ("! ".to_string(), 2)
        };
        left.push((glyph, pair("diagnostic.warning_fg", ground)));
        let pad = indicator_width.saturating_sub(w);
        if pad > 0 {
            left.push((" ".repeat(pad), pair(neutral, ground)));
        }
    } else {
        left.push((" ".repeat(indicator_width), pair(neutral, ground)));
    }

    if let Some(slot) = &slots.leading {
        let text_w = str_width(&slot.text);
        let pad = slot.width().saturating_sub(text_w) + 1;
        left.push((
            slot.text.clone(),
            pair(&slot_paint(slot.fg, &slot.fg_key), ground),
        ));
        left.push((" ".repeat(pad), pair(neutral, ground)));
    }

    // Ancestors that compact mode folded into this row, outermost first. Each
    // takes its own separator, so no cell between two names belongs to neither.
    let chain: Vec<fe::ChainPart> = node
        .chain
        .iter()
        .map(|seg| {
            // A drag over one of the folded names would land in *that*
            // directory, so that is the part that lights up. The row's own
            // ground cannot say it: the anchor is a directory further down,
            // and the row would otherwise show nothing at all.
            let ground = match d.drop_target == Some(seg.path.as_path()) {
                true => "editor.selection_bg",
                false => ground,
            };
            fe::ChainPart {
                runs: vec![
                    (seg.name.clone(), pair("syntax.keyword", ground)),
                    ("/".to_string(), pair("editor.line_number_fg", ground)),
                ],
                path: seg.path.clone(),
            }
        })
        .collect();

    // The anchor's own name, which is no segment's: a press on it is the row's.
    let mut name: fe::Runs = Vec::new();
    match d.fuzzy {
        Some(fm) => {
            let matched: std::collections::HashSet<usize> =
                fm.match_positions.iter().copied().collect();
            let hit = pair("search.match_fg", "search.match_bg");
            let base = pair(&name_fg, ground);
            let mut run = String::new();
            let mut run_is_match = false;
            for (i, c) in node.entry.name.chars().enumerate() {
                let is_match = matched.contains(&i);
                if i > 0 && is_match != run_is_match {
                    let theme = if run_is_match {
                        hit.clone()
                    } else {
                        base.clone()
                    };
                    name.push((std::mem::take(&mut run), theme));
                }
                run_is_match = is_match;
                run.push(c);
            }
            if !run.is_empty() {
                name.push((run, if run_is_match { hit } else { base }));
            }
        }
        None => name.push((node.entry.name.clone(), pair(&name_fg, ground))),
    }

    // The cell that holds the name off the status slot. Part of the label
    // rather than a floor under the row's flex gap, because a label too long
    // for the lane paints over that cell while the hit goes to the gap: here
    // the space is the first thing such a label loses, which is what should
    // give.
    if slots.trailing.is_some() {
        name.push((" ".to_string(), pair(neutral, ground)));
    }

    fe::Row {
        index: d.row,
        theme: pair("editor.fg", ground),
        left,
        chain,
        name,
        trailing: slots.trailing.as_ref().map(|slot| fe::Slot {
            text: slot.text.clone(),
            theme: pair(&slot_paint(slot.fg, &slot.fg_key), ground),
            path: node.entry.path.clone(),
        }),
        error: matches!(node.state, crate::view::file_tree::NodeState::Error(_))
            .then(|| (" [Error]".to_string(), pair("diagnostic.error_fg", ground))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // Only the tests build rows straight from the caches; `describe_row`
    // takes them through `ExplorerSlotContext`.
    use crate::model::filesystem::StdFileSystem;
    use crate::view::file_tree::{
        FileExplorerDecorationCache, FileExplorerSlotOverrideCache, FileTreeView, NodeId,
    };
    // The module itself no longer paints, so `Style` is a test-only type here:
    // `build_line` resolves theme *names* back to styles so these tests can go
    // on asserting about colours.
    use crate::services::fs::FsManager;
    use ratatui::style::Style;
    use std::collections::{HashMap, HashSet};
    use std::fs as std_fs;
    use std::sync::Arc;
    use tempfile::TempDir;

    async fn create_renderer_view() -> (TempDir, FileTreeView) {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        std_fs::create_dir(root.join("src")).unwrap();
        std_fs::write(root.join("README.md"), "hello").unwrap();
        std_fs::write(root.join("src/schema.ts"), "export const value = 1;\n").unwrap();

        let manager = Arc::new(FsManager::new(Arc::new(StdFileSystem)));
        let mut tree = crate::view::file_tree::FileTree::new(root.to_path_buf(), manager)
            .await
            .unwrap();
        let root_id = tree.root_id();
        tree.expand_node(root_id).await.unwrap();
        let src_id = tree
            .get_node(root_id)
            .unwrap()
            .children
            .iter()
            .copied()
            .find(|id| tree.get_node(*id).unwrap().entry.name == "src")
            .unwrap();
        tree.expand_node(src_id).await.unwrap();

        (temp_dir, FileTreeView::new(tree))
    }

    /// One row's pieces, with each theme *name* resolved back to the style it
    /// stands for — so these tests can go on asserting about colours while the
    /// description itself speaks in names.
    fn build_line(
        view: &FileTreeView,
        node_id: NodeId,
        indent: usize,
        decorations: &FileExplorerDecorationCache,
        slot_overrides: &FileExplorerSlotOverrideCache,
        theme: &Theme,
    ) -> Vec<(String, Style)> {
        let resolver = crate::view::file_tree::default_slot_providers().resolver();
        let projection = view.projection();
        let mut node =
            projection.rows[projection.index_of(node_id).expect("a visible node")].clone();
        node.indent = indent;
        let row = describe_row(RowDesc {
            node: &node,
            row: 0,
            is_cursor: false,
            is_multi: false,
            focused: false,
            unsaved: &HashSet::new(),
            cut: &[],
            drop_target: None,
            fuzzy: None,
            decorations,
            slot_overrides,
            slot_resolver: &resolver,
            theme,
            collapsed: ">",
            expanded: "▼",
        });
        let resolve = |name: &str| crate::app::shell_host::shell_theme::resolve(name, theme);
        // The label's parts in the order the row draws them.
        let label = row
            .left
            .into_iter()
            .chain(row.chain.into_iter().flat_map(|part| part.runs))
            .chain(row.name);
        label
            .map(|(t, name)| (t, resolve(&name)))
            .chain(row.trailing.map(|s| (s.text, resolve(&s.theme))))
            .chain(row.error.map(|(t, name)| (t, resolve(&name))))
            .collect()
    }

    #[tokio::test]
    async fn renderer_line_shows_plugin_decoration_badge() {
        let (_temp_dir, view) = create_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let schema_path = view.tree().root_path().join("src/schema.ts");
        let schema_id = view.tree().get_node_by_path(&schema_path).unwrap().id;
        let decorations = FileExplorerDecorationCache::rebuild(
            vec![crate::view::file_tree::FileExplorerDecoration {
                path: schema_path,
                symbol: "M".to_string(),
                color: fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_modified_fg".into(),
                ),
                priority: 50,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );

        let line = build_line(
            &view,
            schema_id,
            2,
            &decorations,
            &FileExplorerSlotOverrideCache::default(),
            &theme,
        );

        assert!(line
            .iter()
            .any(|(text, style)| text == "M" && style.fg == Some(theme.file_status_modified_fg)));
    }

    #[tokio::test]
    async fn directories_render_bubbled_plugin_status() {
        let (_temp_dir, view) = create_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let src_path = view.tree().root_path().join("src");
        let schema_path = src_path.join("schema.ts");
        let src_id = view.tree().get_node_by_path(&src_path).unwrap().id;
        let decorations = FileExplorerDecorationCache::rebuild(
            vec![crate::view::file_tree::FileExplorerDecoration {
                path: schema_path,
                symbol: "R".to_string(),
                color: fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_renamed_fg".into(),
                ),
                priority: 40,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );

        let line = build_line(
            &view,
            src_id,
            1,
            &decorations,
            &FileExplorerSlotOverrideCache::default(),
            &theme,
        );

        assert!(line
            .iter()
            .any(|(text, style)| text == "●" && style.fg == Some(theme.file_status_renamed_fg)));
    }

    #[tokio::test]
    async fn default_slot_providers_allow_explicit_slot_and_name_color_overrides() {
        let (_temp_dir, view) = create_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let schema_path = view.tree().root_path().join("src/schema.ts");
        let schema_id = view.tree().get_node_by_path(&schema_path).unwrap().id;
        let slot_overrides = FileExplorerSlotOverrideCache::rebuild(
            vec![fresh_core::file_explorer::FileExplorerSlotEntry {
                path: schema_path.clone(),
                leading: Some(fresh_core::file_explorer::FileExplorerLeadingSlot {
                    text: "PL".to_string(),
                    color: fresh_core::api::OverlayColorSpec::ThemeKey("syntax.string".into()),
                    min_width: 2,
                }),
                trailing: Some(fresh_core::file_explorer::FileExplorerTrailingSlot {
                    text: "X".to_string(),
                    color: fresh_core::api::OverlayColorSpec::ThemeKey("syntax.type".into()),
                    tooltip: Some(fresh_core::file_explorer::FileExplorerTooltip {
                        title: "Plugin".to_string(),
                        lines: vec!["Overridden".to_string()],
                    }),
                }),
                name_color: Some(fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_added_fg".into(),
                )),
                priority: 50,
                suppress_leading: false,
                suppress_trailing: false,
                suppress_name_color: false,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );

        let line = build_line(
            &view,
            schema_id,
            2,
            &FileExplorerDecorationCache::default(),
            &slot_overrides,
            &theme,
        );

        assert!(line.iter().any(|(text, _)| text == "PL"));
        assert!(line.iter().any(|(text, _)| text == "X"));
        assert!(line.iter().any(
            |(text, style)| text == "schema.ts" && style.fg == Some(theme.file_status_added_fg)
        ));
    }

    #[tokio::test]
    async fn default_slot_providers_fall_back_when_only_name_color_is_overridden() {
        let (_temp_dir, view) = create_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let schema_path = view.tree().root_path().join("src/schema.ts");
        let schema_id = view.tree().get_node_by_path(&schema_path).unwrap().id;
        let decorations = FileExplorerDecorationCache::rebuild(
            vec![crate::view::file_tree::FileExplorerDecoration {
                path: schema_path.clone(),
                symbol: "M".to_string(),
                color: fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_modified_fg".into(),
                ),
                priority: 50,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );
        let slot_overrides = FileExplorerSlotOverrideCache::rebuild(
            vec![fresh_core::file_explorer::FileExplorerSlotEntry {
                path: schema_path,
                leading: None,
                trailing: None,
                name_color: Some(fresh_core::api::OverlayColorSpec::ThemeKey(
                    "syntax.string".into(),
                )),
                priority: 50,
                suppress_leading: false,
                suppress_trailing: false,
                suppress_name_color: false,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );

        let line = build_line(&view, schema_id, 2, &decorations, &slot_overrides, &theme);

        assert!(line
            .iter()
            .any(|(text, style)| text == "schema.ts" && style.fg == Some(theme.syntax_string)));
        assert!(line
            .iter()
            .any(|(text, style)| text == "M" && style.fg == Some(theme.file_status_modified_fg)));
    }

    /// A directory a held drag is over wears the selection ground, so a reader
    /// can see where a drop would land — and it outranks the keyboard cursor,
    /// which is on some other row entirely while the pointer is dragging.
    #[tokio::test]
    async fn the_drop_target_shows_where_a_drag_would_land() {
        let (_temp_dir, view) = create_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let src_path = view.tree().root_path().join("src");
        let src_id = view.tree().get_node_by_path(&src_path).unwrap().id;

        let resolver = crate::view::file_tree::default_slot_providers().resolver();
        let projection = view.projection();
        let node = &projection.rows[projection.index_of(src_id).expect("a row")];
        let describe = |drop_target: Option<&std::path::Path>| {
            describe_row(RowDesc {
                node,
                row: 0,
                is_cursor: false,
                is_multi: false,
                focused: false,
                unsaved: &HashSet::new(),
                cut: &[],
                drop_target,
                fuzzy: None,
                decorations: &FileExplorerDecorationCache::default(),
                slot_overrides: &FileExplorerSlotOverrideCache::default(),
                slot_resolver: &resolver,
                theme: &theme,
                collapsed: ">",
                expanded: "▼",
            })
        };

        let plain = describe(None);
        let under_drag = describe(Some(&src_path));
        assert_ne!(
            plain.theme, under_drag.theme,
            "the row a drop would land in should not look like an idle one"
        );
        assert_eq!(
            describe(Some(&view.tree().root_path().join("README.md"))).theme,
            plain.theme,
            "and only that row should change"
        );
    }

    /// A compact row's label comes apart the way presses need it to: the indent
    /// and indicator are the row's, each folded directory is its own part with
    /// its own path, and the anchor's name is the row's again.
    #[tokio::test]
    async fn a_compact_rows_segments_are_its_own_parts() {
        let (_temp_dir, view) = create_chain_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let anchor_path = view.tree().root_path().join("chain/a/b/c");
        let anchor_id = view.tree().get_node_by_path(&anchor_path).unwrap().id;

        let resolver = crate::view::file_tree::default_slot_providers().resolver();
        let projection = view.projection();
        let mut node =
            projection.rows[projection.index_of(anchor_id).expect("a visible node")].clone();
        node.indent = 2;
        let folded: Vec<&str> = node.chain.iter().map(|s| s.name.as_str()).collect();
        assert_eq!(folded, vec!["chain", "a", "b"], "the folded ancestors");
        let row = describe_row(RowDesc {
            node: &node,
            row: 0,
            is_cursor: false,
            is_multi: false,
            focused: false,
            unsaved: &HashSet::new(),
            cut: &[],
            drop_target: None,
            fuzzy: None,
            decorations: &FileExplorerDecorationCache::default(),
            slot_overrides: &FileExplorerSlotOverrideCache::default(),
            slot_resolver: &resolver,
            theme: &theme,
            collapsed: ">",
            expanded: "▼",
        });

        let drawn =
            |runs: &[(String, String)]| runs.iter().map(|(t, _)| t.as_str()).collect::<String>();
        let root = view.tree().root_path();
        let segments: Vec<(String, &std::path::Path)> = row
            .chain
            .iter()
            .map(|part| (drawn(&part.runs), part.path.strip_prefix(root).unwrap()))
            .collect();
        assert_eq!(
            segments,
            vec![
                ("chain/".to_string(), std::path::Path::new("chain")),
                ("a/".to_string(), std::path::Path::new("chain/a")),
                ("b/".to_string(), std::path::Path::new("chain/a/b")),
            ]
        );
        assert_eq!(drawn(&row.left), "    ▼ ", "the indent and the indicator");
        // The anchor's own name is no segment's, so a press on it is the row's.
        assert_eq!(drawn(&row.name), "c");
    }

    /// A drag held over one of a compact row's folded names shows on *that*
    /// name. The row's ground cannot say it — the row is anchored at `c`,
    /// several directories below where the entry would land — so without this
    /// a drop into a folded directory is drawn exactly like no drop at all.
    #[tokio::test]
    async fn a_drag_over_a_folded_name_shows_on_that_name() {
        let (_temp_dir, view) = create_chain_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let root = view.tree().root_path().to_path_buf();
        let anchor_id = view
            .tree()
            .get_node_by_path(&root.join("chain/a/b/c"))
            .unwrap()
            .id;

        let resolver = crate::view::file_tree::default_slot_providers().resolver();
        let projection = view.projection();
        let node = &projection.rows[projection.index_of(anchor_id).expect("a visible node")];
        let describe = |drop_target: Option<&std::path::Path>| {
            describe_row(RowDesc {
                node,
                row: 0,
                is_cursor: false,
                is_multi: false,
                focused: false,
                unsaved: &HashSet::new(),
                cut: &[],
                drop_target,
                fuzzy: None,
                decorations: &FileExplorerDecorationCache::default(),
                slot_overrides: &FileExplorerSlotOverrideCache::default(),
                slot_resolver: &resolver,
                theme: &theme,
                collapsed: ">",
                expanded: "▼",
            })
        };
        let themes = |row: &crate::view::shell::file_explorer::Row| {
            row.chain
                .iter()
                .map(|part| part.runs.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>())
                .collect::<Vec<_>>()
        };

        let idle = describe(None);
        let over_a = describe(Some(&root.join("chain/a")));
        assert_eq!(
            themes(&idle)[0],
            themes(&over_a)[0],
            "`chain/` is not where the drop would land, so it is untouched"
        );
        assert_ne!(
            themes(&idle)[1],
            themes(&over_a)[1],
            "`a/` is, so it has to look different"
        );
        assert_eq!(themes(&idle)[2], themes(&over_a)[2], "and `b/` is not");
        assert_eq!(
            idle.theme, over_a.theme,
            "the row itself is not the drop target"
        );
    }

    async fn create_chain_renderer_view() -> (TempDir, FileTreeView) {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();
        std_fs::create_dir_all(root.join("chain/a/b/c")).unwrap();
        std_fs::write(root.join("chain/a/b/c/leaf.txt"), "leaf").unwrap();

        let manager = Arc::new(FsManager::new(Arc::new(StdFileSystem)));
        let tree = crate::view::file_tree::FileTree::new(root.to_path_buf(), manager)
            .await
            .unwrap();
        let mut view = FileTreeView::new(tree);
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        let chain_id = view
            .tree()
            .get_node_by_path(&root.join("chain"))
            .unwrap()
            .id;
        view.expand_with_chain(chain_id).await.unwrap();
        (temp_dir, view)
    }
}
