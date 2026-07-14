//! Shared semantic UI projections — the single source of truth for *what* the
//! chrome is, computed once in the core and consumed by every frontend.
//!
//! The guiding principle (see docs/internal/web-ui.md): the TUI and
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
    /// Whether this menu's `when` condition is satisfied. Derived once here via
    /// the shared `is_menu_visible` (the same the TUI uses), so the frontend
    /// doesn't re-decide visibility on its own.
    pub visible: bool,
    pub x: Option<u16>,
    pub w: Option<u16>,
    pub items: Vec<MenuItemView>,
}

/// The currently open dropdown's cell geometry (from the pipeline's MenuLayout),
/// so a frontend can position native rows at the exact cells the editor
/// hit-tests against.
#[derive(Debug, Clone, Serialize)]
pub struct DropdownView {
    /// The dropdown's full bordered box from the pipeline's `MenuLayout` —
    /// one row/column larger than the item union on every side, flush under
    /// the menu bar (falls back to the item union if the layout predates the
    /// recorded box).
    pub rect: Option<RectView>,
    pub items: Vec<ItemArea>,
    pub submenus: Vec<SubmenuArea>,
    /// Full bordered box per expanded submenu depth, same footprint the TUI
    /// border occupies.
    #[serde(rename = "submenuBoxes")]
    pub submenu_boxes: Vec<SubmenuBoxArea>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SubmenuBoxArea {
    pub depth: usize,
    pub rect: RectView,
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
                visible: crate::view::ui::menu::is_menu_visible(m, &self.menu_state().context),
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
                rect: ml
                    .dropdown_box
                    .map(RectView::from)
                    .or_else(|| union_rect(&rects).map(RectView::from)),
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
                submenu_boxes: ml
                    .submenu_boxes
                    .iter()
                    .map(|(depth, r)| SubmenuBoxArea {
                        depth: *depth,
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

/// One search-option toggle (Case / Whole Word / Regex / Confirm-each) as the
/// TUI lays it out on the options row: state + the cell span of its checkbox,
/// so a non-cell frontend can render a native toggle and route clicks back to
/// the exact cells `SearchOptionsLayout::checkbox_at` hit-tests.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SearchOptionView {
    pub name: &'static str,
    pub label: String,
    pub shortcut: Option<String>,
    pub active: bool,
    pub x: u16,
    pub w: u16,
}

/// The search-options row shown with search/replace prompts.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SearchOptionsView {
    pub row: u16,
    pub options: Vec<SearchOptionView>,
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
    /// Content rect of the live-grep / quick-open preview pane (the buffer
    /// interior, inside the left border). The preview is real rendered cells,
    /// so the bridge slices them from this rect and the frontend draws them
    /// like a pane interior. `None` when no preview is showing.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub preview_rect: Option<RectView>,
    pub suggestions: Vec<SuggestionView>,
    /// Optional plugin-built toolbar for the overlay header (real `WidgetSpec`
    /// widgets — e.g. live-grep's scope toggles). Rendered natively; toggle/
    /// button clicks route back through `toggle_overlay_toolbar_widget`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub toolbar: Option<fresh_core::api::WidgetSpec>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub toolbar_focus: Option<String>,
    /// Search-option toggles for search/replace prompts (the TUI's checkbox
    /// row above the prompt line). `None` for every other prompt.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub search_options: Option<SearchOptionsView>,
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
                            // The label the renderer resolved for this tab — the
                            // disambiguated filename (or group name), matching the
                            // TUI and the width the tab was laid out for. Reading
                            // the buffer's metadata display name here instead would
                            // leak the full workspace-relative path into the tab.
                            label: tab.label.clone(),
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
        let (sy, sx, sw) = chrome.status_bar.area?;

        // Read the status bar's semantic model captured by the renderer — no
        // cell scraping. Each rendered element (indicators + text) is a segment,
        // and `side` is the renderer's actual left/right tiling (carried on the
        // segment), not a midpoint guess from `x`.
        let segments: Vec<StatusSegment> = chrome
            .status_bar
            .segments
            .iter()
            .filter(|s| !s.text.trim().is_empty())
            .map(|s| StatusSegment {
                name: s.name,
                key: s.key.clone(),
                text: s.text.trim().to_string(),
                x: s.x,
                w: s.w,
                side: s.side,
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
        // EVERY active prompt projects. A picker list (non-empty suggestions)
        // or a floating overlay projects its full geometry; everything else —
        // plain input prompts (Add Ruler's column, goto-line, …) and prompts
        // whose body is drawn into the pane cells rather than `suggestions`
        // (the `OpenFile`/`SaveFileAs` file browser) — still needs its INPUT
        // LINE surfaced to non-cell frontends: the TUI draws that line on the
        // bottom prompt row in place of the status bar, so without projecting
        // it the web shows no prompt at all while the editor waits for input.
        // Such prompts have no native suggestion list; the frontend renders
        // just the input bar (null `list_rect`/`outer_rect` below).
        let (scroll_start, visible, total) = sugg_area.map(|(_, s, v, t)| (s, v, t)).unwrap_or((
            p.scroll_offset,
            p.suggestions.len(),
            p.suggestions.len(),
        ));
        // Search-option toggles: mirror the TUI's options row from the layout
        // the renderer recorded (`search_options_layout`) — same cell spans the
        // TUI hit-tests — plus the live state and the same label/shortcut
        // derivation `StatusBarRenderer::render_search_options` uses.
        let search_options = chrome.search_options_layout.as_ref().map(|lo| {
            use crate::input::keybindings::{Action, KeyContext};
            use rust_i18n::t;
            let win = self.active_window();
            let kb = self.keybinding_resolver();
            let shortcut = |a: &Action| {
                kb.get_keybinding_for_action(a, KeyContext::SearchPrompt)
                    .or_else(|| kb.get_keybinding_for_action(a, KeyContext::Prompt))
                    .or_else(|| kb.get_keybinding_for_action(a, KeyContext::Global))
            };
            let opt = |span: Option<(u16, u16)>,
                       name: &'static str,
                       label: String,
                       active: bool,
                       shortcut: Option<String>| {
                span.map(|(x0, x1)| SearchOptionView {
                    name,
                    label,
                    shortcut,
                    active,
                    x: x0,
                    w: x1.saturating_sub(x0).max(1),
                })
            };
            SearchOptionsView {
                row: lo.row,
                options: [
                    opt(
                        lo.case_sensitive,
                        "case",
                        t!("search.case_sensitive").to_string(),
                        win.search_case_sensitive,
                        shortcut(&Action::ToggleSearchCaseSensitive),
                    ),
                    opt(
                        lo.whole_word,
                        "word",
                        t!("search.whole_word").to_string(),
                        win.search_whole_word,
                        shortcut(&Action::ToggleSearchWholeWord),
                    ),
                    opt(
                        lo.regex,
                        "regex",
                        t!("search.regex").to_string(),
                        win.search_use_regex,
                        shortcut(&Action::ToggleSearchRegex),
                    ),
                    opt(
                        lo.confirm_each,
                        "confirm",
                        t!("search.confirm_each").to_string(),
                        win.search_confirm_each,
                        shortcut(&Action::ToggleSearchConfirmEach),
                    ),
                ]
                .into_iter()
                .flatten()
                .collect(),
            }
        });
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
            // Inner content of the preview pane: the stored area minus its
            // single left border column (matches `Block::borders(LEFT)` in
            // render_overlay_prompt). Only meaningful for overlay prompts.
            preview_rect: chrome.prompt_preview_area.and_then(|r| {
                (r.width > 1 && r.height > 0).then(|| {
                    RectView::from(Rect::new(
                        r.x.saturating_add(1),
                        r.y,
                        r.width.saturating_sub(1),
                        r.height,
                    ))
                })
            }),
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
            search_options,
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
            let (x, y) = m.clamped_position(chrome.last_frame.width, chrome.last_frame.height);
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

// ─────────────────────────── keybinding editor (full native modal) ───────────────────────────

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct KbSearchView {
    pub active: bool,
    pub focused: bool,
    pub mode: &'static str, // "text" | "recordKey"
    pub query: String,
    pub key_display: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum KbRow {
    Section {
        name: String,
        collapsed: bool,
        count: usize,
        selected: bool,
    },
    Binding {
        key: String,
        action: String,
        description: String,
        context: String,
        source: &'static str, // "keymap" | "custom" | "plugin" | ""
        selected: bool,
    },
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct KbEditDialog {
    pub title: String,
    pub focus_area: usize, // 0=key 1=action 2=context 3=buttons
    pub key_display: String,
    pub key_capturing: bool,
    pub action_text: String,
    pub action_error: Option<String>,
    pub autocomplete: Vec<String>,
    pub autocomplete_selected: Option<usize>,
    pub context: String,
    pub context_options: Vec<String>,
    pub conflicts: Vec<String>,
    pub save_focused: bool,
    pub cancel_focused: bool,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct KbConfirm {
    pub buttons: Vec<String>,
    pub selected: usize,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct KeybindingEditorView {
    pub title: String,
    pub config_path: String,
    pub keymaps: Vec<String>,
    pub search: KbSearchView,
    pub context_filter: String,
    pub context_filtered: bool,
    pub source_filter: String,
    pub source_filtered: bool,
    pub count: String,
    pub has_changes: bool,
    pub rows: Vec<KbRow>,
    pub selected: usize,
    pub scroll_offset: u16,
    pub viewport: u16,
    pub showing_help: bool,
    pub edit_dialog: Option<KbEditDialog>,
    pub confirm: Option<KbConfirm>,
}

impl Editor {
    /// Full semantic model of the keybinding editor modal (header + search +
    /// filters, the binding/section table, the add/edit sub-dialog, the confirm
    /// dialog and the help flag). Rendered natively; all interaction already
    /// flows through `handle_key` (the editor is keyboard-driven).
    pub fn keybinding_editor_view(&self) -> Option<KeybindingEditorView> {
        use crate::app::keybinding_editor::{
            BindingSource, ContextFilter, DisplayRow, SearchMode, SourceFilter,
        };
        let kb = self.keybinding_editor.as_ref()?;

        let rows = kb
            .display_rows
            .iter()
            .enumerate()
            .map(|(i, dr)| {
                let selected = i == kb.selected;
                match dr {
                    DisplayRow::SectionHeader {
                        plugin_name,
                        collapsed,
                        binding_count,
                    } => KbRow::Section {
                        name: plugin_name.clone().unwrap_or_else(|| "Builtin".to_string()),
                        collapsed: *collapsed,
                        count: *binding_count,
                        selected,
                    },
                    DisplayRow::Binding(bi) => {
                        let b = &kb.bindings[*bi];
                        KbRow::Binding {
                            key: b.key_display.clone(),
                            action: b.action.clone(),
                            description: b.action_display.clone(),
                            context: b.context.clone(),
                            source: match b.source {
                                BindingSource::Keymap => "keymap",
                                BindingSource::Custom => "custom",
                                BindingSource::Plugin => "plugin",
                                BindingSource::Unbound => "",
                            },
                            selected,
                        }
                    }
                }
            })
            .collect();

        let (context_filter, context_filtered) = match &kb.context_filter {
            ContextFilter::All => ("All".to_string(), false),
            ContextFilter::Specific(s) => (s.clone(), true),
        };
        let (source_filter, source_filtered) = match kb.source_filter {
            SourceFilter::All => ("All", false),
            SourceFilter::KeymapOnly => ("Keymap", true),
            SourceFilter::CustomOnly => ("Custom", true),
            SourceFilter::PluginOnly => ("Plugin", true),
        };

        let edit_dialog = kb.edit_dialog.as_ref().map(|d| KbEditDialog {
            title: if d.editing_index.is_some() {
                "Edit Binding".to_string()
            } else {
                "Add Binding".to_string()
            },
            focus_area: d.focus_area,
            key_display: d.key_display.clone(),
            key_capturing: d.capturing_special,
            action_text: d.action_text.clone(),
            action_error: d.action_error.clone(),
            autocomplete: if d.autocomplete_visible {
                d.autocomplete_suggestions.clone()
            } else {
                Vec::new()
            },
            autocomplete_selected: d.autocomplete_selected,
            context: d.context.clone(),
            context_options: d.context_options.clone(),
            conflicts: d.conflicts.clone(),
            save_focused: d.focus_area == 3 && d.selected_button == 0,
            cancel_focused: d.focus_area == 3 && d.selected_button == 1,
        });

        let confirm = kb.showing_confirm_dialog.then(|| KbConfirm {
            buttons: vec!["Save".into(), "Discard".into(), "Cancel".into()],
            selected: kb.confirm_selection,
        });

        Some(KeybindingEditorView {
            title: format!("Keybindings — {}", kb.active_keymap),
            config_path: kb.config_file_path.clone(),
            keymaps: kb.keymap_names.clone(),
            search: KbSearchView {
                active: kb.search_active,
                focused: kb.search_focused,
                mode: match kb.search_mode {
                    SearchMode::Text => "text",
                    SearchMode::RecordKey => "recordKey",
                },
                query: kb.search_query.clone(),
                key_display: kb.search_key_display.clone(),
            },
            context_filter,
            context_filtered,
            source_filter: source_filter.to_string(),
            source_filtered,
            count: format!("{} / {}", kb.filtered_indices.len(), kb.bindings.len()),
            has_changes: kb.has_changes,
            rows,
            selected: kb.selected,
            scroll_offset: kb.scroll.offset,
            viewport: kb.scroll.viewport,
            showing_help: kb.showing_help,
            edit_dialog,
            confirm,
        })
    }
}

// ─────────────────────────── settings UI (full native modal) ───────────────────────────

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum SettingControlView {
    Toggle {
        checked: bool,
    },
    Number {
        value: i64,
        min: Option<i64>,
        max: Option<i64>,
    },
    Dropdown {
        selected: usize,
        options: Vec<String>,
        open: bool,
    },
    Text {
        value: String,
        editing: bool,
        placeholder: String,
    },
    TextList {
        items: Vec<String>,
        focused: Option<usize>,
    },
    // Variant-level camelCase: the enum's `rename_all` renames variants but
    // not struct-variant *fields*, so the multi-word fields below need this to
    // match the camelCase JSON contract the frontend consumes.
    #[serde(rename_all = "camelCase")]
    DualList {
        included: Vec<String>,
        available: Vec<String>,
        /// Cursor row in each column and which column is active, so the web
        /// can mirror the TUI's selection highlight. Row indices line up with
        /// `included` / `available` (same order the dispatch hits use).
        included_cursor: usize,
        available_cursor: usize,
        active_column: &'static str, // "included" | "available"
    },
    // Variant-level camelCase (see DualList above).
    #[serde(rename_all = "camelCase")]
    Map {
        entries: Vec<MapEntryView>,
        /// Title for the value column (`Name │ <column>` header in the TUI),
        /// derived from the control's `display_field`. `None` = no header.
        column: Option<String>,
        /// Auto-managed maps (e.g. Languages, LSP) take no user-added
        /// entries; the TUI hides the add row for them.
        no_add: bool,
    },
    ObjectArray {
        entries: Vec<String>,
    },
    Json {
        value: String,
    },
    Complex {
        type_name: String,
    },
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MapEntryView {
    pub key: String,
    pub display: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SettingItemView {
    pub index: usize,
    pub path: String,
    pub name: String,
    pub description: Option<String>,
    pub section: Option<String>,
    pub section_start: bool,
    pub modified: bool,
    pub read_only: bool,
    pub nullable: bool,
    pub is_null: bool,
    pub selected: bool,
    pub control: SettingControlView,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SettingsCategoryView {
    pub index: usize,
    pub name: String,
    pub selected: bool,
    pub expandable: bool,
    pub expanded: bool,
    pub sections: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SettingsSearchResultView {
    pub name: String,
    pub category: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EntryDialogView {
    pub title: String,
    pub is_new: bool,
    pub items: Vec<SettingItemView>,
    pub selected_item: usize,
    pub focus_on_buttons: bool,
    pub focused_button: usize,
    pub no_delete: bool,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SettingsView {
    pub title: String,
    pub focus: &'static str, // "categories" | "settings" | "footer"
    pub target_layer: String,
    pub categories: Vec<SettingsCategoryView>,
    pub items: Vec<SettingItemView>,
    pub footer_buttons: Vec<String>,
    pub footer_selected: usize,
    pub search_active: bool,
    pub search_query: String,
    pub search_results: Vec<SettingsSearchResultView>,
    pub search_selected: usize,
    pub entry_dialog: Option<EntryDialogView>,
    pub showing_help: bool,
    pub showing_confirm: bool,
    pub showing_reset: bool,
}

fn setting_control_view(c: &crate::view::settings::items::SettingControl) -> SettingControlView {
    use crate::view::settings::items::SettingControl as C;
    match c {
        C::Toggle(s) => SettingControlView::Toggle { checked: s.checked },
        C::Number(s) => SettingControlView::Number {
            value: s.value,
            min: s.min,
            max: s.max,
        },
        C::Dropdown(s) => SettingControlView::Dropdown {
            selected: s.selected,
            options: s.options.clone(),
            open: s.open,
        },
        C::Text(s) => SettingControlView::Text {
            value: s.value(),
            editing: s.editing,
            placeholder: s.placeholder.clone(),
        },
        C::TextList(s) => SettingControlView::TextList {
            items: s.items.clone(),
            focused: s.focused_item,
        },
        C::DualList(s) => SettingControlView::DualList {
            // Use the control's own item enumerations so the row indices the
            // web sends back (ControlDualListIncluded/Available(idx,row)) match
            // exactly what `add_selected`/`remove_selected` index into.
            included: s
                .included_items()
                .iter()
                .map(|(_, n)| n.to_string())
                .collect(),
            available: s.available_items().iter().map(|(_, n)| n.clone()).collect(),
            included_cursor: s.included_cursor,
            available_cursor: s.available_cursor,
            active_column: match s.active_column {
                crate::view::controls::DualListColumn::Included => "included",
                crate::view::controls::DualListColumn::Available => "available",
            },
        },
        // Rows must read exactly as the TUI's: the same domain helper
        // (`get_display_value`, e.g. `/grammar` → "Assembly") formats the
        // value preview, and the value-column title mirrors the TUI's
        // `Name │ <Col>` header — never the raw JSON blob.
        C::Map(s) => SettingControlView::Map {
            entries: s
                .entries
                .iter()
                .map(|(k, v)| MapEntryView {
                    key: k.clone(),
                    display: s.get_display_value(v),
                })
                .collect(),
            column: s
                .display_field
                .as_deref()
                .map(crate::view::settings::widget_map::column_title),
            no_add: s.no_add,
        },
        // Same combo → action row text the TUI renders (keybinding-shaped
        // entries), collapsing to the bare display value when there is no
        // key combo (LSP server lists and other non-keybinding arrays).
        C::ObjectArray(s) => SettingControlView::ObjectArray {
            entries: s
                .bindings
                .iter()
                .map(|b| {
                    let field = s
                        .display_field
                        .as_deref()
                        .and_then(|p| p.strip_prefix('/'))
                        .or(s.display_field.as_deref())
                        .unwrap_or("action");
                    let combo = crate::view::controls::keybinding_list::format_key_combo(b);
                    let action = b
                        .get(field)
                        .and_then(|v| v.as_str())
                        .unwrap_or("(no action)");
                    if combo.trim().is_empty() {
                        action.to_string()
                    } else {
                        format!("{combo} → {action}")
                    }
                })
                .collect(),
        },
        C::Json(s) => SettingControlView::Json { value: s.value() },
        C::Complex { type_name } => SettingControlView::Complex {
            type_name: type_name.clone(),
        },
    }
}

fn setting_item_view(
    item: &crate::view::settings::items::SettingItem,
    i: usize,
    selected: bool,
) -> SettingItemView {
    SettingItemView {
        index: i,
        path: item.path.clone(),
        name: item.name.clone(),
        description: item.description.clone(),
        section: item.section.clone(),
        section_start: item.is_section_start,
        modified: item.modified,
        read_only: item.read_only,
        nullable: item.nullable,
        is_null: item.is_null,
        selected,
        control: setting_control_view(&item.control),
    }
}

impl Editor {
    /// Full semantic model of the Settings modal: the category tree, the item
    /// list for the selected category (every control kind), search, the footer,
    /// and the add/edit entry sub-dialog (Map/ObjectArray). Keyboard-driven via
    /// `handle_key`; rendered natively. `None` unless settings is showing.
    pub fn settings_view(&self) -> Option<SettingsView> {
        use crate::view::settings::state::FocusPanel;
        let st = self.settings_state.as_ref()?;
        if !st.visible {
            return None;
        }

        let categories = st
            .pages
            .iter()
            .enumerate()
            .map(|(i, p)| SettingsCategoryView {
                index: i,
                name: p.name.clone(),
                selected: i == st.selected_category,
                expandable: !p.subpages.is_empty() || p.sections.len() > 1,
                expanded: st.expanded_categories.contains(&i),
                sections: p.sections.iter().map(|s| s.name.clone()).collect(),
            })
            .collect();

        let items = st
            .pages
            .get(st.selected_category)
            .map(|p| {
                p.items
                    .iter()
                    .enumerate()
                    .map(|(i, it)| setting_item_view(it, i, i == st.selected_item))
                    .collect()
            })
            .unwrap_or_default();

        let entry_dialog = st.entry_dialog_stack.last().map(|d| EntryDialogView {
            title: d.title.clone(),
            is_new: d.is_new,
            items: d
                .items
                .iter()
                .enumerate()
                .map(|(i, it)| setting_item_view(it, i, i == d.selected_item))
                .collect(),
            selected_item: d.selected_item,
            focus_on_buttons: d.focus_on_buttons,
            focused_button: d.focused_button,
            no_delete: d.no_delete,
        });

        Some(SettingsView {
            title: "Settings".to_string(),
            focus: match st.focus.current() {
                Some(FocusPanel::Settings) => "settings",
                Some(FocusPanel::Footer) => "footer",
                _ => "categories",
            },
            target_layer: format!("{:?}", st.target_layer),
            categories,
            items,
            footer_buttons: vec![
                format!("{:?}", st.target_layer),
                "Reset".into(),
                "Save".into(),
                "Cancel".into(),
            ],
            footer_selected: st.footer_button_index,
            search_active: st.search_active,
            search_query: st.search_query().to_string(),
            search_results: st
                .search_results
                .iter()
                .map(|r| SettingsSearchResultView {
                    name: r.item.name.clone(),
                    category: r.breadcrumb.clone(),
                })
                .collect(),
            search_selected: st.selected_search_result,
            entry_dialog,
            showing_help: st.showing_help,
            showing_confirm: st.showing_confirm_dialog,
            showing_reset: st.showing_reset_dialog,
        })
    }
}
