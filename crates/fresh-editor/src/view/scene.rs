//! Shared semantic UI projections — the single source of truth for *what* the
//! chrome is, computed once in the core and consumed by every frontend.
//!
//! The guiding principle (see docs/internal/UNIFIED_SCENE_DESIGN.md): the TUI and
//! the web/GUI must not re-implement the same logic. Everything semantic — which
//! menus exist, which items are enabled/checked, their accelerators, which menu
//! is open — is derived here, once. A frontend then only does the *rendering*
//! (this model → cells for the TUI; this model → HTML for the web) and the input
//! bridge (crossterm vs. DOM → the shared `handle_key`/`handle_mouse`).
//!
//! These projections derive `serde::Serialize` so the web bridge can ship them
//! as-is; the field names match the JSON the browser frontend already consumes.

use crate::app::Editor;
use fresh_core::LeafId;
use ratatui::layout::Rect;
use serde::Serialize;
use std::collections::HashMap;

/// A cell rectangle, serialized as `{x, y, w, h}` (matching the bridge's
/// historical `rect_json`).
#[derive(Debug, Clone, Copy, Serialize)]
pub struct RectView {
    pub x: u16,
    pub y: u16,
    pub w: u16,
    pub h: u16,
}

impl From<Rect> for RectView {
    fn from(r: Rect) -> Self {
        RectView {
            x: r.x,
            y: r.y,
            w: r.width,
            h: r.height,
        }
    }
}

/// One item in a menu, projected semantically (no cells). `kind` tags the
/// variant so the frontend can render actions, separators, submenus and labels
/// differently.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "lowercase")]
pub enum MenuItemView {
    Action {
        label: String,
        action: String,
        #[serde(skip_serializing_if = "HashMap::is_empty")]
        args: HashMap<String, serde_json::Value>,
        accel: Option<String>,
        enabled: bool,
        checked: Option<bool>,
    },
    Sep,
    Submenu {
        label: String,
        items: Vec<MenuItemView>,
    },
    Label {
        label: String,
    },
}

/// A top-level menu: its label, its menu-bar cell position (when laid out), and
/// its item tree.
#[derive(Debug, Clone, Serialize)]
pub struct MenuEntry {
    pub label: String,
    pub x: Option<u16>,
    pub w: Option<u16>,
    pub items: Vec<MenuItemView>,
}

/// The currently open dropdown's cell geometry (from the pipeline's MenuLayout),
/// so a frontend can position native rows at the exact cells the editor
/// hit-tests against.
#[derive(Debug, Clone, Serialize)]
pub struct DropdownView {
    pub rect: Option<RectView>,
    pub items: Vec<ItemArea>,
    pub submenus: Vec<SubmenuArea>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ItemArea {
    pub index: usize,
    pub rect: RectView,
}

#[derive(Debug, Clone, Serialize)]
pub struct SubmenuArea {
    pub depth: usize,
    pub index: usize,
    pub rect: RectView,
}

/// The full semantic menu model: the menu tree plus which menu/item is open and
/// highlighted. The editor is the single source of truth for open/highlight;
/// frontends render this and forward interactions back through `handle_mouse`.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MenuView {
    pub menus: Vec<MenuEntry>,
    pub menu_open: Option<usize>,
    pub menu_highlight: Option<usize>,
    pub submenu_path: Vec<usize>,
    pub dropdown: Option<DropdownView>,
}

fn item_view(editor: &Editor, item: &fresh_core::menu::MenuItem) -> MenuItemView {
    use fresh_core::menu::MenuItem::*;
    match item {
        Separator { .. } => MenuItemView::Sep,
        Action {
            label,
            action,
            args,
            when: _,
            checkbox,
        } => MenuItemView::Action {
            label: label.clone(),
            action: action.clone(),
            args: args.clone(),
            accel: editor.accelerator_for(action),
            // Same enabled/checked logic the TUI MenuRenderer uses — one source.
            enabled: crate::view::ui::menu::is_menu_item_enabled(
                item,
                &editor.menu_state().context,
            ),
            checked: checkbox.as_ref().map(|_| {
                crate::view::ui::menu::is_checkbox_checked(checkbox, &editor.menu_state().context)
            }),
        },
        Submenu { label, items } => MenuItemView::Submenu {
            label: label.clone(),
            items: items.iter().map(|i| item_view(editor, i)).collect(),
        },
        DynamicSubmenu { label, .. } => MenuItemView::Submenu {
            label: label.clone(),
            items: Vec::new(),
        },
        Label { info } => MenuItemView::Label {
            label: info.clone(),
        },
    }
}

fn union_rect(rects: &[Rect]) -> Option<Rect> {
    let mut acc: Option<Rect> = None;
    for r in rects {
        acc = Some(match acc {
            None => *r,
            Some(a) => {
                let x0 = a.x.min(r.x);
                let y0 = a.y.min(r.y);
                let x1 = (a.x + a.width).max(r.x + r.width);
                let y1 = (a.y + a.height).max(r.y + r.height);
                Rect::new(x0, y0, x1 - x0, y1 - y0)
            }
        });
    }
    acc
}

impl Editor {
    /// Build the semantic menu model. This is the *single* place the menu's
    /// structure, enabled/checked state and accelerators are derived; the TUI
    /// renderer and the web bridge both consume this rather than recomputing it.
    ///
    /// Geometry (`x`/`w`, dropdown rects) comes from the pipeline's `MenuLayout`,
    /// which is populated during render — so this reflects the most recent frame.
    pub fn menu_view(&self) -> MenuView {
        let chrome = self.active_chrome();
        let menu_areas: HashMap<usize, Rect> = chrome
            .menu_layout
            .as_ref()
            .map(|m| m.menu_areas.iter().cloned().collect())
            .unwrap_or_default();

        // Same expanded menu list the TUI renderer uses (config + plugin menus),
        // so the two frontends never diverge on which menus/items exist.
        let menus: Vec<MenuEntry> = self
            .all_menus_expanded()
            .iter()
            .enumerate()
            .map(|(i, m)| MenuEntry {
                label: m.label.clone(),
                x: menu_areas.get(&i).map(|r| r.x),
                w: menu_areas.get(&i).map(|r| r.width),
                items: m.items.iter().map(|it| item_view(self, it)).collect(),
            })
            .collect();

        let dropdown = chrome.menu_layout.as_ref().and_then(|ml| {
            if ml.item_areas.is_empty() {
                return None;
            }
            let rects: Vec<Rect> = ml.item_areas.iter().map(|(_, r)| *r).collect();
            Some(DropdownView {
                rect: union_rect(&rects).map(RectView::from),
                items: ml
                    .item_areas
                    .iter()
                    .map(|(index, r)| ItemArea {
                        index: *index,
                        rect: RectView::from(*r),
                    })
                    .collect(),
                submenus: ml
                    .submenu_areas
                    .iter()
                    .map(|(depth, index, r)| SubmenuArea {
                        depth: *depth,
                        index: *index,
                        rect: RectView::from(*r),
                    })
                    .collect(),
            })
        });

        let ms = self.menu_state();
        MenuView {
            menus,
            menu_open: ms.active_menu,
            menu_highlight: ms.highlighted_item,
            submenu_path: ms.submenu_path.clone(),
            dropdown,
        }
    }
}

// ─────────────────────────── tabs ───────────────────────────

/// One tab in a pane's tab bar (semantic; geometry from the pipeline's
/// TabLayout for click/close hit-testing).
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TabView {
    pub buffer_id: Option<usize>,
    pub label: String,
    pub active: bool,
    pub modified: bool,
    pub rect: RectView,
    pub close_rect: RectView,
}

/// A pane's tab bar: the bar rect (when laid out) and its tabs.
#[derive(Debug, Clone, Default, Serialize)]
pub struct TabBarView {
    pub bar: Option<RectView>,
    pub tabs: Vec<TabView>,
}

// ─────────────────────────── status bar ───────────────────────────

#[derive(Debug, Clone, Serialize)]
pub struct StatusSegment {
    pub name: &'static str,
    pub key: Option<String>,
    pub text: String,
    pub x: u16,
    pub w: u16,
    pub side: &'static str,
}

#[derive(Debug, Clone, Serialize)]
pub struct StatusView {
    pub rect: RectView,
    pub segments: Vec<StatusSegment>,
}

// ─────────────────────────── command palette / picker ───────────────────────────

#[derive(Debug, Clone, Serialize)]
pub struct SuggestionView {
    pub text: String,
    pub description: Option<String>,
    pub keybinding: Option<String>,
    pub disabled: bool,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PaletteView {
    pub query: String,
    pub message: String,
    pub prompt_type: &'static str,
    pub overlay: bool,
    pub title: String,
    pub status: String,
    pub selected: Option<usize>,
    pub scroll_start: usize,
    pub visible_count: usize,
    pub total: usize,
    pub outer_rect: Option<RectView>,
    pub list_rect: Option<RectView>,
    pub suggestions: Vec<SuggestionView>,
    /// Optional plugin-built toolbar for the overlay header (real `WidgetSpec`
    /// widgets — e.g. live-grep's scope toggles). Rendered natively; toggle/
    /// button clicks route back through `toggle_overlay_toolbar_widget`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub toolbar: Option<fresh_core::api::WidgetSpec>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub toolbar_focus: Option<String>,
}

/// Stable tag for a prompt type so the frontend can label the palette/picker.
fn prompt_type_tag(t: &crate::view::prompt::PromptType) -> &'static str {
    use crate::view::prompt::PromptType::*;
    match t {
        QuickOpen => "quickopen",
        LiveGrep => "livegrep",
        Search | ReplaceSearch | QueryReplaceSearch => "search",
        OpenFile | OpenFileWithEncoding { .. } => "openfile",
        SaveFileAs => "saveas",
        GotoLine | GotoByteOffset => "goto",
        _ => "input",
    }
}

impl Editor {
    /// Semantic tab bar for a pane (leaf). Single derivation of tab labels /
    /// active / modified shared by the TUI tab renderer and the web bridge.
    pub fn tab_bar_view(&self, leaf: LeafId) -> TabBarView {
        let active = self.active_buffer();
        let layout = self.active_layout();
        match layout.tab_layouts.get(&leaf) {
            None => TabBarView::default(),
            Some(tl) => TabBarView {
                bar: Some(RectView::from(tl.bar_area)),
                tabs: tl
                    .tabs
                    .iter()
                    .map(|tab| {
                        let bid = tab.target.as_buffer();
                        TabView {
                            buffer_id: bid.map(|b| b.0),
                            label: bid
                                .and_then(|b| self.buffer_display_name(b))
                                .unwrap_or_else(|| "untitled".into()),
                            active: bid == Some(active),
                            modified: bid.map(|b| self.buffer_is_modified(b)).unwrap_or(false),
                            rect: RectView::from(tab.tab_area),
                            close_rect: RectView::from(tab.close_area),
                        }
                    })
                    .collect(),
            },
        }
    }

    /// Semantic status bar: the whole bar tiled into labeled indicator segments
    /// plus the untracked text runs between them (file name / Ln,Col). The
    /// segment *text* is lifted from the rendered `buf` for now. Single
    /// derivation shared by both frontends.
    pub fn status_view(&self) -> Option<StatusView> {
        let chrome = self.active_chrome();
        let (sy, sx, sw) = chrome.status_bar_area?;
        let mid = sx.saturating_add(sw / 2);
        let side = |x: u16| if x < mid { "left" } else { "right" };

        // Read the status bar's semantic model captured by the renderer — no
        // cell scraping. Each rendered element (indicators + text) is a segment.
        let segments: Vec<StatusSegment> = chrome
            .status_bar_segments
            .iter()
            .filter(|s| !s.text.trim().is_empty())
            .map(|s| StatusSegment {
                name: s.name,
                key: s.key.clone(),
                text: s.text.trim().to_string(),
                x: s.x,
                w: s.w,
                side: side(s.x),
            })
            .collect();

        Some(StatusView {
            rect: RectView {
                x: sx,
                y: sy,
                w: sw,
                h: 1,
            },
            segments,
        })
    }

    /// Semantic command palette / picker, derived from the active prompt and the
    /// pipeline's suggestion-popup geometry. `None` unless a picker list (or a
    /// floating overlay) is showing. Single derivation shared by both frontends.
    pub fn palette_view(&self) -> Option<PaletteView> {
        let chrome = self.active_chrome();
        let sugg_outer = chrome.suggestions_outer_area;
        let sugg_area = chrome.suggestions_area;
        let prompt_results = chrome.prompt_results_area;
        let p = self.active_window().prompt.as_ref()?;
        if p.suggestions.is_empty() && !p.overlay {
            return None;
        }
        let (scroll_start, visible, total) = sugg_area.map(|(_, s, v, t)| (s, v, t)).unwrap_or((
            p.scroll_offset,
            p.suggestions.len(),
            p.suggestions.len(),
        ));
        Some(PaletteView {
            query: p.input.clone(),
            message: p.message.clone(),
            prompt_type: prompt_type_tag(&p.prompt_type),
            overlay: p.overlay,
            title: p.title.iter().map(|t| t.text.as_str()).collect(),
            status: p.status.clone(),
            selected: p.selected_suggestion,
            scroll_start,
            visible_count: visible,
            total,
            outer_rect: sugg_outer.map(RectView::from),
            list_rect: sugg_area
                .map(|(r, _, _, _)| r)
                .or(prompt_results)
                .map(RectView::from),
            suggestions: p
                .suggestions
                .iter()
                .map(|s| SuggestionView {
                    text: s.text.clone(),
                    description: s.description.clone(),
                    keybinding: s.keybinding.clone(),
                    disabled: s.disabled,
                })
                .collect(),
            toolbar: p.toolbar_widget.clone(),
            toolbar_focus: p.toolbar_focus.clone(),
        })
    }
}

// ─────────────────────────── popups (completion / hover / action / list / text) ───────────────────────────

#[derive(Debug, Clone, Serialize)]
pub struct PopupItemView {
    pub text: String,
    pub detail: Option<String>,
    pub icon: Option<String>,
    pub disabled: bool,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum PopupContentView {
    List {
        items: Vec<PopupItemView>,
        selected: usize,
    },
    Lines {
        lines: Vec<String>,
    },
}

/// A floating popup (completion menu, hover doc, action chooser, …) projected
/// semantically. Geometry (`rect`/`content_rect`) is the pipeline's popup layout
/// so the frontend can position the native box and forward clicks/scroll back
/// through `handle_mouse` — the existing popup hit-tester resolves them.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ScenePopup {
    pub kind: &'static str,
    pub title: Option<String>,
    pub description: Option<String>,
    pub rect: RectView,
    pub content_rect: RectView,
    pub scroll_offset: usize,
    pub content: PopupContentView,
}

fn project_popup(
    p: &crate::view::popup::Popup,
    outer: Rect,
    inner: Rect,
    scroll: usize,
) -> ScenePopup {
    use crate::view::popup::{PopupContent, PopupKind};
    let kind = match p.kind {
        PopupKind::Completion => "completion",
        PopupKind::Hover => "hover",
        PopupKind::Action => "action",
        PopupKind::List => "list",
        PopupKind::Text => "text",
    };
    let content = match &p.content {
        PopupContent::List { items, selected } => PopupContentView::List {
            items: items
                .iter()
                .map(|i| PopupItemView {
                    text: i.text.clone(),
                    detail: i.detail.clone(),
                    icon: i.icon.clone(),
                    disabled: i.disabled,
                })
                .collect(),
            selected: *selected,
        },
        PopupContent::Text(lines) | PopupContent::Custom(lines) => PopupContentView::Lines {
            lines: lines.clone(),
        },
        PopupContent::Markdown(styled) => PopupContentView::Lines {
            lines: styled
                .iter()
                .map(|l| l.spans.iter().map(|s| s.text.as_str()).collect::<String>())
                .collect(),
        },
    };
    ScenePopup {
        kind,
        title: p.title.clone(),
        description: p.description.clone(),
        rect: RectView::from(outer),
        content_rect: RectView::from(inner),
        scroll_offset: scroll,
        content,
    }
}

impl Editor {
    /// All visible popups across the per-buffer and global stacks, projected
    /// semantically. Single derivation shared by the web frontend (native HTML)
    /// and available to the TUI compositor; geometry comes from the pipeline's
    /// popup-area caches so clicks/scroll route through the existing hit-tester.
    pub fn popups_view(&self) -> Vec<ScenePopup> {
        let chrome = self.active_chrome();
        let mut out = Vec::new();
        let locals = self.active_state().popups.all();
        for (idx, outer, inner, scroll, _n, _sb, _t) in &chrome.popup_areas {
            if let Some(p) = locals.get(*idx) {
                out.push(project_popup(p, *outer, *inner, *scroll));
            }
        }
        let globals = self.global_popups.all();
        for (idx, outer, inner, scroll, _n) in &chrome.global_popup_areas {
            if let Some(p) = globals.get(*idx) {
                out.push(project_popup(p, *outer, *inner, *scroll));
            }
        }
        out
    }
}

// ─────────────────────────── file explorer (sidebar tree) ───────────────────────────

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FileRow {
    pub name: String,
    pub depth: usize,
    pub is_dir: bool,
    pub expanded: bool,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FileExplorerView {
    pub rect: RectView,
    pub title: String,
    pub scroll_offset: usize,
    pub viewport_height: usize,
    pub selected: Option<usize>,
    pub rows: Vec<FileRow>,
}

impl Editor {
    /// Semantic file-explorer sidebar: the flattened visible tree rows (the same
    /// `get_display_nodes()` the TUI renderer uses) plus selection/scroll and the
    /// sidebar rect. Rendered natively by the web frontend; row clicks route back
    /// through `handle_mouse` at the sidebar's content cells, which the existing
    /// file-explorer hit-test resolves to the same display index.
    pub fn file_explorer_view(&self) -> Option<FileExplorerView> {
        let rect = self.active_layout().file_explorer_area?;
        let view = self.file_explorer()?;
        let tree = view.tree();
        let rows = view
            .get_display_nodes()
            .into_iter()
            .filter_map(|(id, indent)| {
                tree.get_node(id).map(|n| FileRow {
                    name: n.entry.name.clone(),
                    depth: indent,
                    is_dir: n.is_dir(),
                    expanded: n.is_expanded(),
                })
            })
            .collect();
        let title = tree
            .get_node(tree.root_id())
            .map(|n| n.entry.name.clone())
            .unwrap_or_default();
        Some(FileExplorerView {
            rect: RectView::from(rect),
            title,
            scroll_offset: view.get_scroll_offset(),
            viewport_height: view.viewport_height,
            selected: view.get_selected_index(),
            rows,
        })
    }
}

// ─────────────────────────── workspace-trust dialog ───────────────────────────

#[derive(Debug, Clone, Serialize)]
pub struct TrustOptionView {
    pub label: String,
    pub description: String,
    pub selected: bool,
    pub data: &'static str,
    pub rect: RectView,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TrustDialogView {
    pub dialog: RectView,
    pub title: String,
    pub path: String,
    pub triggers: String,
    pub cancellable: bool,
    pub options: Vec<TrustOptionView>,
    pub ok: RectView,
    pub ok_label: String,
    pub quit: RectView,
    pub quit_label: String,
}

impl Editor {
    /// Semantic workspace-trust dialog (the blocking "trust this folder?" modal).
    /// `None` unless it's showing. Geometry comes from the pipeline's
    /// `TrustDialogLayout`; clicks on the options / OK / Quit route back through
    /// `handle_mouse` at those rects (the existing `handle_workspace_trust_mouse`).
    pub fn trust_dialog_view(&self) -> Option<TrustDialogView> {
        let layout = self.active_chrome().workspace_trust_dialog.clone()?;
        let selected = self.current_workspace_trust_selection();
        let data = ["trusted", "restricted", "blocked"];
        let options = crate::view::workspace_trust_dialog::options()
            .into_iter()
            .enumerate()
            .map(|(i, o)| TrustOptionView {
                label: o.label,
                description: o.description,
                selected: i == selected,
                data: data.get(i).copied().unwrap_or("restricted"),
                rect: RectView::from(layout.radios[i]),
            })
            .collect();
        let quit_label = if self.workspace_trust_cancellable() {
            rust_i18n::t!("trust.dialog.btn_cancel").into_owned()
        } else {
            rust_i18n::t!("trust.dialog.btn_quit").into_owned()
        };
        Some(TrustDialogView {
            dialog: RectView::from(layout.dialog),
            title: rust_i18n::t!("trust.dialog.security_warning").into_owned(),
            path: self.working_dir().display().to_string(),
            triggers: self.workspace_trust_markers().join(", "),
            cancellable: self.workspace_trust_cancellable(),
            options,
            ok: RectView::from(layout.ok),
            ok_label: rust_i18n::t!("trust.dialog.btn_ok").into_owned(),
            quit: RectView::from(layout.quit),
            quit_label,
        })
    }
}

// ─────────────────────────── plugin widget surfaces (floating / dock) ───────────────────────────

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct WidgetHitView {
    /// Index into this surface's `hits` — sent back on click so the editor runs
    /// the exact same hit it would for a TUI cell click.
    pub index: usize,
    pub widget_key: String,
    pub widget_kind: String,
    pub event_type: String,
    pub payload: serde_json::Value,
}

/// Host-owned instance state a frontend needs to render a widget correctly
/// (List/Tree selection + scroll). Keyed by widget `key`.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct WidgetInstanceView {
    pub selected_index: Option<i32>,
    pub scroll_offset: Option<u32>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub expanded_keys: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct WidgetSurfaceView {
    /// "dock" (left dock) or "floatingModal" (centered).
    pub kind: &'static str,
    pub plugin: String,
    pub panel_id: u64,
    pub rect: RectView,
    pub focus_key: String,
    /// The raw, already-serializable `WidgetSpec` tree — rendered natively.
    pub spec: fresh_core::api::WidgetSpec,
    pub instances: HashMap<String, WidgetInstanceView>,
    pub hits: Vec<WidgetHitView>,
}

impl Editor {
    /// Semantic model for plugin-mounted floating / dock widget panels (e.g. the
    /// orchestrator session dock). Each surface ships its `WidgetSpec` tree +
    /// instance state + on-screen rect + hit list; the frontend renders the spec
    /// natively and forwards a clicked hit's index back through `/widget`, which
    /// runs the same `deliver_widget_hit` path as a TUI cell click. `None`
    /// surfaces (unmounted panels) are simply omitted.
    pub fn widgets_view(&self) -> Vec<WidgetSurfaceView> {
        let mut out = Vec::new();
        for (kind, slot) in [
            ("dock", self.dock.as_ref()),
            ("floatingModal", self.floating_widget_panel.as_ref()),
        ] {
            let Some(fwp) = slot else { continue };
            let Some(rect) = fwp.last_inner_rect else {
                continue;
            };
            let Some(panel) = self.widget_registry.get(&fwp.panel_key) else {
                continue;
            };
            let mut instances = HashMap::new();
            for (key, st) in &panel.instance_states {
                use crate::widgets::WidgetInstanceState as W;
                let view = match st {
                    W::List {
                        scroll_offset,
                        selected_index,
                        ..
                    } => WidgetInstanceView {
                        selected_index: Some(*selected_index),
                        scroll_offset: Some(*scroll_offset),
                        expanded_keys: Vec::new(),
                    },
                    W::Tree {
                        scroll_offset,
                        selected_index,
                        expanded_keys,
                    } => WidgetInstanceView {
                        selected_index: Some(*selected_index),
                        scroll_offset: Some(*scroll_offset),
                        expanded_keys: expanded_keys.iter().cloned().collect(),
                    },
                    _ => continue,
                };
                instances.insert(key.clone(), view);
            }
            let hits = panel
                .hits
                .iter()
                .enumerate()
                .map(|(index, h)| WidgetHitView {
                    index,
                    widget_key: h.widget_key.clone(),
                    widget_kind: h.widget_kind.to_string(),
                    event_type: h.event_type.to_string(),
                    payload: h.payload.clone(),
                })
                .collect();
            out.push(WidgetSurfaceView {
                kind,
                plugin: fwp.panel_key.plugin.clone(),
                panel_id: fwp.panel_key.id,
                rect: RectView::from(rect),
                focus_key: panel.focus_key.clone(),
                spec: panel.spec.clone(),
                instances,
                hits,
            });
        }
        out
    }
}

// ─────────────────────────── context menus (right-click / new-tab) ───────────────────────────

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ContextMenuView {
    /// "tab" | "newTab" | "fileExplorer" — for styling / debugging.
    pub kind: &'static str,
    pub x: u16,
    pub y: u16,
    pub highlighted: usize,
    pub items: Vec<String>,
}

impl Editor {
    /// The active right-click / new-tab context menu (only one shows at a time),
    /// projected for native rendering. Items render at `y + 1 + i` (the bordered
    /// box); a click forwarded to `handle_mouse` at `(x + 1, y + 1 + i)` resolves
    /// to item `i` via the existing hover/hit-test (`item_idx = row - y - 1`).
    pub fn context_menu_view(&self) -> Option<ContextMenuView> {
        let w = self.active_window();
        let chrome = self.active_chrome();
        if let Some(m) = &w.file_explorer_context_menu {
            let (x, y) = m.clamped_position(chrome.last_frame_width, chrome.last_frame_height);
            return Some(ContextMenuView {
                kind: "fileExplorer",
                x,
                y,
                highlighted: m.highlighted,
                items: m.items().iter().map(|i| i.label()).collect(),
            });
        }
        if let Some(m) = &w.new_tab_menu {
            return Some(ContextMenuView {
                kind: "newTab",
                x: m.position.0,
                y: m.position.1,
                highlighted: m.highlighted,
                items: crate::app::types::NewTabMenuItem::all()
                    .iter()
                    .map(|i| i.label())
                    .collect(),
            });
        }
        if let Some(m) = &w.tab_context_menu {
            return Some(ContextMenuView {
                kind: "tab",
                x: m.position.0,
                y: m.position.1,
                highlighted: m.highlighted,
                items: crate::app::types::TabContextMenuItem::all()
                    .iter()
                    .map(|i| i.label())
                    .collect(),
            });
        }
        None
    }
}

// ─────────────────────────── auxiliary modals (keybindings / event-debug / theme-info) ───────────────────────────

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AuxLine {
    pub text: String,
    pub selected: bool,
}

/// A small/secondary modal projected as a titled list of text lines. Covers the
/// keybinding editor (binding list), the event-debug log, and the theme-info
/// popup — read-mostly surfaces whose interaction (nav / Esc / rebind) already
/// flows through `handle_key`. `rect` anchors the theme popup; `None` ⇒ the
/// frontend centers it.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AuxModalView {
    pub kind: &'static str,
    pub title: String,
    pub rect: Option<RectView>,
    pub lines: Vec<AuxLine>,
    pub footer: Option<String>,
}

impl Editor {
    /// The active auxiliary modal (keybinding editor / event-debug / theme-info),
    /// projected as a titled line list for native rendering. Only one shows at a
    /// time. Cells for these are suppressed on the web; keyboard drives them.
    pub fn aux_modals_view(&self) -> Option<AuxModalView> {
        // NOTE: the keybinding editor is intentionally NOT projected here — it's a
        // full interactive modal (search, context/source filters, an add/edit
        // sub-dialog, help overlay), Settings-grade rather than a line list. It
        // renders as cells (functional) until it gets a proper native projection,
        // grouped with the Settings UI.
        let w = self.active_window();
        // Event-debug log.
        if let Some(ed) = &w.event_debug {
            let mut lines: Vec<AuxLine> = ed
                .history
                .iter()
                .map(|r| AuxLine {
                    text: r.description.clone(),
                    selected: false,
                })
                .collect();
            if lines.is_empty() {
                lines.push(AuxLine {
                    text: rust_i18n::t!("event_debug.no_events").into_owned(),
                    selected: false,
                });
            }
            return Some(AuxModalView {
                kind: "eventDebug",
                title: rust_i18n::t!("event_debug.title").into_owned(),
                rect: None,
                lines,
                footer: Some(rust_i18n::t!("event_debug.help_text").into_owned()),
            });
        }
        // Theme-info popup (anchored at its click position).
        if let Some(ti) = &w.theme_info_popup {
            fn color_str(c: ratatui::style::Color) -> String {
                match c {
                    ratatui::style::Color::Rgb(r, g, b) => format!("#{r:02x}{g:02x}{b:02x}"),
                    other => format!("{other:?}"),
                }
            }
            let info = &ti.info;
            let mut lines = vec![AuxLine {
                text: format!("Region: {}", info.region),
                selected: false,
            }];
            if let Some(k) = &info.fg_key {
                let c = info
                    .fg_color
                    .map(|c| format!("  {}", color_str(c)))
                    .unwrap_or_default();
                lines.push(AuxLine {
                    text: format!("Foreground: {k}{c}"),
                    selected: false,
                });
            }
            if let Some(k) = &info.bg_key {
                let c = info
                    .bg_color
                    .map(|c| format!("  {}", color_str(c)))
                    .unwrap_or_default();
                lines.push(AuxLine {
                    text: format!("Background: {k}{c}"),
                    selected: false,
                });
            }
            if let Some(cat) = &info.syntax_category {
                lines.push(AuxLine {
                    text: format!("Category: {cat}"),
                    selected: false,
                });
            }
            return Some(AuxModalView {
                kind: "themeInfo",
                title: "Theme".to_string(),
                rect: Some(RectView {
                    x: ti.position.0,
                    y: ti.position.1,
                    w: 0,
                    h: 0,
                }),
                lines,
                footer: None,
            });
        }
        None
    }
}
