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

/// The currently open dropdown's cell geometry, read off the shell tree that
/// placed it, so a frontend can position native rows at the exact cells the
/// editor hit-tests against.
#[derive(Debug, Clone, Serialize)]
pub struct DropdownView {
    /// The dropdown's full bordered box — one row/column larger than the
    /// item union on every side, under its bar label (falls back to the item
    /// union when the box has no rectangle).
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
            // Same enabled/checked logic the TUI description uses — one source.
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
    /// Geometry — each label's `x`/`w`, the dropdown boxes and their rows —
    /// is read off the shell tree that placed them (`view::shell::menu`), the
    /// same rectangles the TUI painted from.
    pub fn menu_view(&self) -> MenuView {
        use crate::view::shell::menu::{dropdown_item_key, dropdown_key, menu_label_key};
        let rect_of = |key: &fresh_ui::Key| -> Option<Rect> {
            let ui = self.shell_ui.as_ref()?;
            let f = self.active_chrome().last_frame;
            crate::view::shell::rect_of(ui, key, Rect::new(0, 0, f.width, f.height))
        };

        // Same expanded menu list the TUI renderer uses (config + plugin menus),
        // so the two frontends never diverge on which menus/items exist.
        let menus: Vec<MenuEntry> = self
            .all_menus_expanded()
            .iter()
            .enumerate()
            .map(|(i, m)| {
                let label = rect_of(&menu_label_key(i));
                MenuEntry {
                    label: m.label.clone(),
                    visible: crate::view::ui::menu::is_menu_visible(m, &self.menu_state().context),
                    x: label.map(|r| r.x),
                    w: label.map(|r| r.width),
                    items: m.items.iter().map(|it| item_view(self, it)).collect(),
                }
            })
            .collect();

        // The open chain: level by level, each box and its rows as the tree
        // laid them out. Rows are keyed by depth and index, so their
        // rectangles come straight back without a walk.
        let (_, levels) = self.menu_description();
        let rows_of = |depth: usize, n: usize| -> Vec<(usize, Rect)> {
            (0..n)
                .filter_map(|i| rect_of(&dropdown_item_key(depth, i)).map(|r| (i, r)))
                .collect()
        };
        let dropdown = levels.first().and_then(|top| {
            let items = rows_of(0, top.rows.len());
            if items.is_empty() {
                return None;
            }
            let rects: Vec<Rect> = items.iter().map(|(_, r)| *r).collect();
            let mut submenus = Vec::new();
            let mut submenu_boxes = Vec::new();
            for (depth, level) in levels.iter().enumerate().skip(1) {
                for (index, r) in rows_of(depth, level.rows.len()) {
                    submenus.push(SubmenuArea {
                        depth,
                        index,
                        rect: RectView::from(r),
                    });
                }
                if let Some(r) = rect_of(&dropdown_key(depth)) {
                    submenu_boxes.push(SubmenuBoxArea {
                        depth,
                        rect: RectView::from(r),
                    });
                }
            }
            Some(DropdownView {
                rect: rect_of(&dropdown_key(0))
                    .map(RectView::from)
                    .or_else(|| union_rect(&rects).map(RectView::from)),
                items: items
                    .iter()
                    .map(|(index, r)| ItemArea {
                        index: *index,
                        rect: RectView::from(*r),
                    })
                    .collect(),
                submenus,
                submenu_boxes,
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
/// the exact cells the shell's tree assigned it.
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
        // Both halves off the retained tree: the strip's row by its key, and
        // each tab's rectangles and label by the tab's — the same nodes the
        // TUI's clicks land on, so a native tab and a painted one cannot
        // disagree about where a tab is or what it says.
        let f = self.active_chrome().last_frame;
        let size = ratatui::layout::Rect::new(0, 0, f.width, f.height);
        let bar = self.shell_ui.as_ref().and_then(|ui| {
            crate::view::shell::rect_of(ui, &crate::view::shell::splits::tabs_key(leaf), size)
        });
        let Some(bar) = bar else {
            return TabBarView::default();
        };
        let active = self.active_buffer();
        TabBarView {
            bar: Some(RectView::from(bar)),
            tabs: self
                .tab_rects(leaf)
                .into_iter()
                .map(|t| {
                    let bid = t.target.as_buffer();
                    TabView {
                        buffer_id: bid.map(|b| b.0),
                        label: t.label,
                        active: bid == Some(active),
                        modified: bid.map(|b| self.buffer_is_modified(b)).unwrap_or(false),
                        rect: RectView::from(t.name),
                        close_rect: RectView::from(t.close),
                    }
                })
                .collect(),
        }
    }

    /// Semantic status bar: the whole bar tiled into labeled indicator segments
    /// plus the untracked text runs between them (file name / Ln,Col). The
    /// segment *text* is lifted from the rendered `buf` for now. Single
    /// derivation shared by both frontends.
    pub fn status_view(&self) -> Option<StatusView> {
        // Both halves come off the retained tree: the row's rectangle from the
        // frame's regions, the segments from the keyed elements inside it.
        // `status_bar_area_now` is also what says the bar is *there* — it
        // returns `None` when the user hid it or a suggestions / file-browser
        // popup took the row, so there is no capture to clear by hand and no
        // way for the web to keep drawing a bar the TUI does not have.
        let area = self.status_bar_area_now()?;

        // Each keyed element (indicators + text) is a segment, and `side` is
        // the description's own left/right tiling carried on the segment, not
        // a midpoint guess from `x`. No cell scraping either way.
        let segments: Vec<StatusSegment> = self
            .shell_status_segments()
            .into_iter()
            .filter(|s| !s.text.trim().is_empty())
            .map(|s| StatusSegment {
                name: s.name,
                key: s.key,
                text: s.text.trim().to_string(),
                x: s.x,
                w: s.w,
                side: s.side,
            })
            .collect();

        Some(StatusView {
            rect: RectView {
                x: area.x,
                y: area.y,
                w: area.width,
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
        let p = self.active_window().prompt.as_ref()?;
        // The overlay card's bands, read off the tree that placed them.
        let card_band = |r: crate::view::shell::overlay_prompt::CardRegion| {
            self.shell_ui.as_ref().and_then(|ui| {
                crate::view::shell::overlay_prompt::regions_of(ui)
                    .into_iter()
                    .find(|(k, _)| *k == r)
                    .map(|(_, rect)| rect)
                    .filter(|rect| rect.width > 0 && rect.height > 0)
            })
        };
        let prompt_results = p
            .overlay
            .then(|| card_band(crate::view::shell::overlay_prompt::CardRegion::Results))
            .flatten();
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
        // Search-option toggles: the row's own content — the same values the
        // TUI describes its toggles with — plus the cell spans the shell's
        // layout assigned them, READ BACK off the laid-out tree rather than
        // recomputed. The web frontend routes clicks to those exact cells, so
        // a second derivation here is a second chance to disagree; there used
        // to be one (`SearchOptionsLayout::compute`) and it existed only to
        // be re-checked against the painter.
        let search_options = self.search_options_content().and_then(|content| {
            use crate::view::shell::search_options::Piece;
            let spans = self.search_option_spans_now()?;
            let row = spans.first().map(|(_, r)| r.y)?;
            let options = content
                .pieces
                .iter()
                .filter_map(|piece| {
                    let Piece::Toggle(t) = piece else { return None };
                    let (_, rect) = spans.iter().find(|(o, _)| *o == t.option)?;
                    Some(SearchOptionView {
                        name: t.option.web_name(),
                        label: t.label.clone(),
                        shortcut: t.shortcut.clone(),
                        active: t.checked,
                        x: rect.x,
                        w: rect.width.max(1),
                    })
                })
                .collect();
            Some(SearchOptionsView { row, options })
        });
        Some(PaletteView {
            query: p.input_str().to_string(),
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
            // The preview pane's content: the band names the pane inside its
            // rule, so this is the rectangle as the tree placed it. Only
            // meaningful for overlay prompts.
            preview_rect: p
                .overlay
                .then(|| card_band(crate::view::shell::overlay_prompt::CardRegion::Preview))
                .flatten()
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
            toolbar: p
                .toolbar
                .as_ref()
                .and_then(|k| self.widget_registry.get(k))
                .map(|panel| panel.spec.clone()),
            toolbar_focus: p
                .toolbar
                .as_ref()
                .and_then(|k| self.widget_registry.focus_key(k))
                .filter(|f| !f.is_empty())
                .map(str::to_string),
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
        PopupContent::Text(lines) => PopupContentView::Lines {
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
    /// Flattened-row indices in screen order, including sticky ancestors.
    pub viewport_rows: Vec<usize>,
    pub rows: Vec<FileRow>,
}

impl Editor {
    /// Semantic file-explorer sidebar: the flattened visible tree rows (the same
    /// `get_display_nodes()` the TUI renderer uses) plus selection/scroll and the
    /// sidebar rect. Rendered natively by the web frontend; row clicks route
    /// back through `handle_mouse` at the sidebar's content cells, where the
    /// shell's own row nodes answer them — `viewport_rows[n]` and the tree's
    /// n-th row key are the same number by construction.
    pub fn file_explorer_view(&self) -> Option<FileExplorerView> {
        // **Derived, not recorded.** The sidebar's rectangle is
        // `HostRegion::Explorer`'s, which is a keyed node — so this asks the
        // tree rather than reading a copy the draw filed a frame ago. Presence
        // is app state and stays app state: a hidden sidebar still has a
        // rectangle, and it is `file_explorer_visible` that says it is not
        // there.
        if !self.file_explorer_visible() {
            return None;
        }
        let view = self.file_explorer()?;
        let rect = self.panel_rect(&crate::view::shell::frame::region_key(
            crate::view::shell::frame::HostRegion::Explorer,
        ))?;
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
            viewport_rows: view.viewport_display_indices(),
            rows,
        })
    }
}

// ──────────────────── file browser (Open File / Save As / Switch Project) ────────────────────

/// One visible file row of the browser popup. The window of rows is the
/// tree's (`shell::file_browser::window`), so `row` — the grid row the entry
/// was laid out on — is the cell a click on it is sent to, where the row
/// answers for itself.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FileBrowserRowView {
    /// Index into the editor's entry list (not into this window).
    pub index: usize,
    /// Grid row this entry occupies.
    pub row: u16,
    pub name: String,
    pub is_dir: bool,
    pub is_symlink: bool,
    /// Formatted size, empty for directories (the TUI prints "--").
    pub size: String,
    /// Formatted modification time, empty when unknown.
    pub modified: String,
    pub selected: bool,
    /// False for entries the current filter text does not match — the TUI dims
    /// them rather than hiding them.
    pub matches_filter: bool,
}

/// A checkbox toggle (Show Hidden / Detect Encoding) with the cell span the
/// tree laid it out at, so a native frontend can draw its own checkbox and
/// send the click to the cells where the toggle's own node answers it.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FileBrowserToggleView {
    pub name: &'static str,
    pub label: String,
    pub shortcut: Option<String>,
    pub active: bool,
    pub x: u16,
    pub y: u16,
    pub w: u16,
}

/// A navigation shortcut (`..`, `/`, `~`, …) plus its cell span.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FileBrowserShortcutView {
    pub index: usize,
    pub label: String,
    pub description: String,
    pub selected: bool,
    pub x: u16,
    pub y: u16,
    pub w: u16,
}

/// A sortable column header plus its cell span.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FileBrowserColumnView {
    pub name: &'static str,
    pub label: String,
    /// This column is the active sort key.
    pub active: bool,
    /// Sort direction, meaningful when `active`.
    pub ascending: bool,
    pub x: u16,
    pub y: u16,
    pub w: u16,
}

/// Semantic file-browser popup: the dialog behind Open File, Save File As and
/// Switch Project. Everything the TUI paints into its bordered band — the
/// directory, the toggles, the nav shortcuts, the sortable columns and the
/// visible slice of entries — with the cell span of every interactive element
/// so a native frontend renders it as DOM and sends clicks to the cells the
/// tree placed each control at. `None` unless one of those prompts is active.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FileBrowserView {
    /// The popup band (including the TUI's border cells).
    pub rect: RectView,
    /// The file-list rows only.
    pub list_rect: RectView,
    /// One-cell scrollbar column beside the list.
    pub scrollbar_rect: RectView,
    /// Directory being browsed, in full — eliding it is the frontend's call.
    pub path: String,
    pub toggles: Vec<FileBrowserToggleView>,
    pub shortcuts: Vec<FileBrowserShortcutView>,
    pub columns: Vec<FileBrowserColumnView>,
    pub rows: Vec<FileBrowserRowView>,
    pub scroll_offset: usize,
    pub visible_rows: usize,
    /// Total entries in the directory (the list scrolls within this).
    pub total: usize,
    pub selected: Option<usize>,
    /// Which section has keyboard focus: `"navigation"` or `"files"`.
    pub active_section: &'static str,
    pub loading: bool,
    pub error: Option<String>,
    /// Scrollbar thumb, in rows from the top of `scrollbar_rect`.
    pub thumb_start: usize,
    pub thumb_end: usize,
}

impl Editor {
    /// Semantic file browser, derived from the dialog's description and the
    /// tree that laid it out (rects, the list's window and the cell span of
    /// every interactive element, read back by key). Rendered natively by
    /// the web frontend; clicks go to those cells, where the controls answer
    /// for themselves — so there is no hit-testing implementation anywhere
    /// but the tree's.
    pub fn file_browser_view(&self) -> Option<FileBrowserView> {
        use crate::app::file_open::{FileOpenSection, SortMode, Toggle};
        use crate::view::shell::file_browser as fb;
        use fresh_i18n::t;

        let win = self.active_window();
        let state = win.file_open_state.as_ref()?;
        let ui = self.shell_ui.as_ref()?;
        let frame = self.active_chrome().last_frame;
        let size = ratatui::layout::Rect {
            x: 0,
            y: 0,
            width: frame.width,
            height: frame.height,
        };
        let rects = fb::rects(ui, size, state.shortcuts.len())?;
        let window = fb::window(ui, size)?;
        // The same content the tree laid out, so a label the web shows is
        // the label the TUI painted.
        let b = self.browser_description(rects.dialog.height)?;
        // The band's last column is the bar's, reserved whether or not the
        // list overflows — the painter's `scrollbar_width` beside the rows.
        let list_rect = ratatui::layout::Rect {
            width: rects.list.width.saturating_sub(1),
            ..rects.list
        };
        let scrollbar_rect = ratatui::layout::Rect {
            x: rects.list.x + list_rect.width,
            width: rects.list.width.min(1),
            ..rects.list
        };

        let toggles = rects
            .toggles
            .iter()
            .filter_map(|(id, r)| {
                let t = b.toggles.iter().find(|t| t.id == *id)?;
                Some(FileBrowserToggleView {
                    name: id.name(),
                    label: t.label.clone(),
                    shortcut: t.shortcut.clone(),
                    active: match id {
                        Toggle::ShowHidden => state.show_hidden,
                        Toggle::DetectEncoding => state.detect_encoding,
                    },
                    x: r.x,
                    y: r.y,
                    w: r.width,
                })
            })
            .collect();

        let shortcuts = rects
            .shortcuts
            .iter()
            .enumerate()
            .filter_map(|(idx, r)| {
                let sc = state.shortcuts.get(idx)?;
                Some(FileBrowserShortcutView {
                    index: idx,
                    label: sc.label.clone(),
                    description: sc.description.clone(),
                    selected: b.selected_shortcut == Some(idx),
                    x: r.x,
                    y: r.y,
                    w: r.width,
                })
            })
            .collect();

        let columns = rects
            .columns
            .iter()
            .map(|(mode, r)| {
                let (name, label) = match mode {
                    SortMode::Name => ("name", t!("file_browser.name")),
                    SortMode::Size => ("size", t!("file_browser.size")),
                    SortMode::Modified => ("modified", t!("file_browser.modified")),
                    SortMode::Type => ("type", t!("file_browser.name")),
                };
                FileBrowserColumnView {
                    name,
                    label: label.to_string(),
                    active: state.sort_mode == *mode,
                    ascending: state.sort_ascending,
                    x: r.x,
                    y: r.y,
                    w: r.width,
                }
            })
            .collect();

        // The window the tree is showing: the rows the TUI has on screen, at
        // the grid rows it put them on.
        let rows = match &b.listing {
            fb::Listing::Entries(entries) => entries
                .iter()
                .enumerate()
                .skip(window.first)
                .take(window.visible)
                .map(|(index, e)| FileBrowserRowView {
                    index,
                    row: list_rect.y + (index - window.first) as u16,
                    name: e.name.clone(),
                    is_dir: e.is_dir,
                    is_symlink: e.is_symlink,
                    size: e.size.clone().unwrap_or_default(),
                    modified: e.modified.clone().unwrap_or_default(),
                    selected: b.selected == Some(index),
                    matches_filter: e.matches,
                })
                .collect(),
            _ => Vec::new(),
        };
        let files_active = state.active_section == FileOpenSection::Files;

        Some(FileBrowserView {
            rect: RectView::from(rects.dialog),
            list_rect: RectView::from(list_rect),
            scrollbar_rect: RectView::from(scrollbar_rect),
            path: state.current_dir.display().to_string(),
            toggles,
            shortcuts,
            columns,
            rows,
            scroll_offset: window.first,
            visible_rows: window.visible,
            total: state.entries.len(),
            selected: state.selected_index,
            active_section: if files_active { "files" } else { "navigation" },
            loading: state.loading,
            error: state.error.clone(),
            thumb_start: window.thumb.0,
            thumb_end: window.thumb.1,
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
    /// `None` unless it's showing.
    ///
    /// Geometry comes off the shell's tree, which is what placed the controls.
    /// It used to come from `TrustDialogLayout`, a set of rectangles the
    /// painter recorded for a hit test the TUI no longer performs — the nodes
    /// answer their own presses, and this projection is the last caller that
    /// wanted the rectangles at all.
    pub fn trust_dialog_view(&self) -> Option<TrustDialogView> {
        let frame = self.active_chrome().last_frame;
        let size = ratatui::layout::Rect {
            x: 0,
            y: 0,
            width: frame.width,
            height: frame.height,
        };
        let layout = crate::view::shell::trust::rects(self.shell_ui.as_ref()?, size)?;
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
            fresh_i18n::t!("trust.dialog.btn_cancel").into_owned()
        } else {
            fresh_i18n::t!("trust.dialog.btn_quit").into_owned()
        };
        Some(TrustDialogView {
            dialog: RectView::from(layout.dialog),
            title: fresh_i18n::t!("trust.dialog.security_warning").into_owned(),
            path: self.working_dir().display().to_string(),
            triggers: self.workspace_trust_markers().join(", "),
            cancellable: self.workspace_trust_cancellable(),
            options,
            ok: RectView::from(layout.ok),
            ok_label: fresh_i18n::t!("trust.dialog.btn_ok").into_owned(),
            quit: RectView::from(layout.secondary),
            quit_label,
        })
    }
}

// ─────────────────────── plugin widget surfaces: deleted with the web path ───────────────────────
//
// **What was here.** `WidgetSurfaceView` / `WidgetInstanceView` /
// `WidgetHitView` and `Editor::widgets_view`: the dock's and the floating
// panel's `WidgetSpec`, the registry's instance-state map, the recorded hit
// list's identity half, the focused key and the panel's rectangle, shipped to
// the web frontend so it could lay the spec out itself and echo a click back
// as an index plus an identity.
//
// **Why it is gone.** It was the last consumer of `WidgetPanelState::hits` and
// `boxes` for a *described* panel, and so the last reason the immediate-mode
// collector had to run for one. Deleting it is what lets the retained tree be
// the only thing that lays a plugin panel out. See
// `docs/internal/retained-mode-ui.md` §3.9 for what the replacement is: the web
// consuming the display list the TUI already folds, the way it consumes the
// status bar, the settings dialog and the file browser.
//
// **The web's plugin panels do not render at all until that lands.** Every
// other surface is unaffected — this deletes one region of the scene, not the
// bridge.

// ─────────────────────────── the tree: plugin panels as the display list ───────────────────────────
//
// **The web consumes the display list.** A plugin panel is nodes in the same
// tree the terminal folds into cells, so the web is handed the *items* that
// tree produced for the panel subtrees — rectangle, clip, resolved colours,
// and what to draw — and folds them into DOM the way the terminal folds them
// into cells. There is no plugin-specific projection: no spec shipped for the
// browser to lay out itself, no recorded hit list, no index to echo back. A
// press comes back as a cell through the ordinary mouse path and is routed
// over the tree like a terminal click; a text press reaches the field through
// `text_byte` like any other. See `docs/internal/retained-mode-ui.md` §3.9.
//
// What is shipped is the panel subtrees only — the dock column, the floating
// panel's frame, and each sidebar section a plugin mounted — plus every layer
// those subtrees raised (a dropdown's pop-over, a modal's scrim), found by
// element ancestry rather than by key range, because a layer paints in the
// display list's tail and not inside its parent's range. The rest of the
// chrome the web still draws natively from its own region views; they retire
// onto this projection surface by surface.

/// One of the panel subtrees the web draws from the display list.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TreeSurfaceView {
    /// `"dock"`, `"floating"` or `"sidebar"`.
    pub kind: &'static str,
    pub x: i32,
    pub y: i32,
    pub w: u16,
    pub h: u16,
    /// A floating panel raised as an anchored popup (a context menu) rather
    /// than a centered modal. Meaningful for `"floating"` only.
    pub anchored: bool,
    /// The sidebar section index, for `"sidebar"`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub section: Option<usize>,
}

/// One display-list item, resolved for a backend that does not hold the
/// theme: the colours are the fold's answer, not the key.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TreeItemView {
    /// The index into `surfaces` of the subtree this item belongs to.
    pub surface: usize,
    /// Painted by a layer the subtree raised — a pop-over, a scrim — rather
    /// than by the subtree's own flow. Layers paint after everything in flow.
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub layer: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub key: Option<String>,
    /// `"fill"`, `"border"`, `"lines"`, `"scrollbar"`, `"scrim"`,
    /// `"selectable"` or `"host"`.
    pub kind: &'static str,
    /// The visible rectangle: the item's own, cut by every enclosing clip.
    pub x: i32,
    pub y: i32,
    pub w: u16,
    pub h: u16,
    /// Where the item's own rectangle begins, which for `lines` is where
    /// column zero of each row sits — left of `x` when the clip cut it.
    pub ox: i32,
    pub oy: i32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fg: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bg: Option<String>,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub bold: bool,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub italic: bool,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub underline: bool,
    /// A `scrim` that dims rather than covers.
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub dim: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lines: Option<Vec<String>>,
    /// The border's corner style, for `border`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub border: Option<String>,
    /// `[top, len]` of the thumb in track cells, for `scrollbar` — the same
    /// arithmetic every backend uses (`Draw::scrollbar_thumb`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thumb: Option<[u16; 2]>,
    /// For `scrollbar`: the track runs across rather than down.
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub horizontal: bool,
    /// For `scrollbar`: the marks on the track, each `[cell, colour]` with
    /// the colour resolved, and whether the mark takes the whole cell.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub marks: Option<Vec<MarkView>>,
}

/// One mark on a `scrollbar` item's track, for the web.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MarkView {
    pub at: u16,
    pub color: String,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub full: bool,
}

#[derive(Debug, Clone, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TreeView {
    pub surfaces: Vec<TreeSurfaceView>,
    pub items: Vec<TreeItemView>,
    /// The hardware caret, when the display list places it inside one of the
    /// surfaces: a text field in a panel.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cursor: Option<[i32; 2]>,
}

fn css_color(c: ratatui::style::Color) -> Option<String> {
    use ratatui::style::Color;
    // The xterm 256-colour cube and ramp, so an indexed colour a theme names
    // reads the same in a browser as in a terminal.
    fn indexed(i: u8) -> (u8, u8, u8) {
        const BASE: [(u8, u8, u8); 16] = [
            (0, 0, 0),
            (205, 0, 0),
            (0, 205, 0),
            (205, 205, 0),
            (0, 0, 238),
            (205, 0, 205),
            (0, 205, 205),
            (229, 229, 229),
            (127, 127, 127),
            (255, 0, 0),
            (0, 255, 0),
            (255, 255, 0),
            (92, 92, 255),
            (255, 0, 255),
            (0, 255, 255),
            (255, 255, 255),
        ];
        match i {
            0..=15 => BASE[i as usize],
            16..=231 => {
                let i = i - 16;
                let step = |v: u8| if v == 0 { 0 } else { 55 + v * 40 };
                (step(i / 36), step((i / 6) % 6), step(i % 6))
            }
            232..=255 => {
                let v = 8 + (i - 232) * 10;
                (v, v, v)
            }
        }
    }
    let (r, g, b) = match c {
        Color::Rgb(r, g, b) => (r, g, b),
        Color::Indexed(i) => indexed(i),
        Color::Black => indexed(0),
        Color::Red => indexed(1),
        Color::Green => indexed(2),
        Color::Yellow => indexed(3),
        Color::Blue => indexed(4),
        Color::Magenta => indexed(5),
        Color::Cyan => indexed(6),
        Color::Gray => indexed(7),
        Color::DarkGray => indexed(8),
        Color::LightRed => indexed(9),
        Color::LightGreen => indexed(10),
        Color::LightYellow => indexed(11),
        Color::LightBlue => indexed(12),
        Color::LightMagenta => indexed(13),
        Color::LightCyan => indexed(14),
        Color::White => indexed(15),
        Color::Reset => return None,
    };
    Some(format!("#{r:02x}{g:02x}{b:02x}"))
}

impl Editor {
    /// The plugin panels, as the display list the tree produced for them.
    /// Empty when no panel is mounted in any of the three slots.
    pub fn tree_view(&self) -> TreeView {
        use crate::view::shell::fold::Palette;
        use crate::view::shell::widgets::Slot;
        use fresh_ui::{Draw, Scrim};
        let Some(ui) = self.shell_ui.as_ref() else {
            return TreeView::default();
        };
        let mut surfaces = Vec::new();
        let mut roots = Vec::new();
        // `whole_layer`: the surface's rectangle is the keyed element's, but
        // the items belong to the layer *around* it — a floating panel's
        // scrim is the layer's own item, and the layer is the frame's parent.
        let mut push = |kind: &'static str,
                        key: fresh_ui::Key,
                        anchored: bool,
                        section: Option<usize>,
                        whole_layer: bool| {
            if let Some(el) = ui.find_by_key(&key) {
                let r = ui.rect_of(el);
                if r.w > 0 && r.h > 0 {
                    surfaces.push(TreeSurfaceView {
                        kind,
                        x: r.x,
                        y: r.y,
                        w: r.w,
                        h: r.h,
                        anchored,
                        section,
                    });
                    let root = match whole_layer {
                        true => ui.parent(el).unwrap_or(el),
                        false => el,
                    };
                    roots.push(root);
                }
            }
        };
        if self.dock.is_some() {
            push(
                "dock",
                crate::view::shell::dock::column_key(),
                false,
                None,
                false,
            );
        }
        if let Some(f) = self.floating_widget_panel.as_ref() {
            let anchored = matches!(f.placement, crate::app::PanelPlacement::Anchored { .. });
            push(
                "floating",
                crate::view::shell::panel::key(),
                anchored,
                None,
                true,
            );
        }
        for (i, s) in self.sidebar_sections.iter().enumerate() {
            if s.panel.is_some() {
                push(
                    "sidebar",
                    crate::view::shell::panel::interior_key(Slot::Sidebar(i)),
                    false,
                    Some(i),
                    false,
                );
            }
        }
        if roots.is_empty() {
            return TreeView::default();
        }
        let palette = self.shell_palette();
        let spec = ui.spec();
        let mut items = Vec::new();
        for (index, item) in spec.items.iter().enumerate() {
            let Some(surface) = roots.iter().position(|r| ui.contains(*r, item.id)) else {
                continue;
            };
            // A scrim is a statement about the whole frame, and its rect says
            // so; everything else is cut to what is visible.
            let vis = match item.draw {
                Draw::Scrim(_) => fresh_ui::Rect {
                    x: 0,
                    y: 0,
                    w: spec.frame.w,
                    h: spec.frame.h,
                },
                _ => item.visible_rect(),
            };
            if vis.w == 0 || vis.h == 0 {
                continue;
            }
            let style = palette.style(&item.theme);
            let m = style.add_modifier;
            let (mut horizontal, mut marks) = (false, None);
            let (kind, lines, border, thumb, dim) = match &item.draw {
                Draw::Fill => ("fill", None, None, None, false),
                Draw::Wash => ("wash", None, None, None, false),
                Draw::Border(bs) => (
                    "border",
                    None,
                    Some(format!("{bs:?}").to_lowercase()),
                    None,
                    false,
                ),
                Draw::Scrim(Scrim::Opaque) => ("scrim", None, None, None, false),
                Draw::Scrim(Scrim::Dim) => ("scrim", None, None, None, true),
                Draw::Lines(ls) => (
                    "lines",
                    Some(ls.iter().map(|l| l.to_string()).collect()),
                    None,
                    None,
                    false,
                ),
                Draw::Scrollbar {
                    offset,
                    content,
                    window,
                    axis,
                    marks: ms,
                } => {
                    horizontal = *axis == fresh_ui::Axis::Horizontal;
                    let track = match axis {
                        fresh_ui::Axis::Vertical => item.rect.h.max(1),
                        fresh_ui::Axis::Horizontal => item.rect.w.max(1),
                    };
                    let (top, len) =
                        Draw::scrollbar_thumb(*offset, *content, u32::from(*window), track);
                    if !ms.is_empty() {
                        marks = Some(
                            ms.iter()
                                .filter_map(|m| {
                                    let st = palette.style(&m.theme);
                                    let c = if m.full { st.bg.or(st.fg) } else { st.fg };
                                    Some(MarkView {
                                        at: m.at,
                                        color: c.and_then(css_color)?,
                                        full: m.full,
                                    })
                                })
                                .collect(),
                        );
                    }
                    ("scrollbar", None, None, Some([top, len]), false)
                }
                Draw::Selectable => ("selectable", None, None, None, false),
                Draw::Host(_) => ("host", None, None, None, false),
            };
            items.push(TreeItemView {
                surface,
                layer: index >= spec.layers_from,
                key: item.key.as_ref().map(|k| k.to_string()),
                kind,
                x: vis.x,
                y: vis.y,
                w: vis.w,
                h: vis.h,
                ox: item.rect.x,
                oy: item.rect.y,
                fg: style.fg.and_then(css_color),
                bg: style.bg.and_then(css_color),
                bold: m.contains(ratatui::style::Modifier::BOLD),
                italic: m.contains(ratatui::style::Modifier::ITALIC),
                underline: m.contains(ratatui::style::Modifier::UNDERLINED),
                dim,
                lines,
                border,
                thumb,
                horizontal,
                marks,
            });
        }
        let cursor = spec.cursor.as_ref().filter(|c| c.visible).and_then(|c| {
            let (x, y) = (c.pos.x, c.pos.y);
            let inside = surfaces.iter().any(|s| {
                x >= s.x && x < s.x + i32::from(s.w) && y >= s.y && y < s.y + i32::from(s.h)
            });
            inside.then_some([x, y])
        });
        TreeView {
            surfaces,
            items,
            cursor,
        }
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
        use crate::app::types::ContextMenuKind;
        let w = self.active_window();
        // One shared geometry core drives all four menus; the only per-menu
        // difference the web cares about is the `kind` tag.
        let (kind, core) = w.open_context_menu()?;
        // Where layout actually put it. The TUI and the web draw the menu
        // differently but must agree on the cells it covers, so both read the
        // one rectangle the shell's tree produced rather than each re-deriving
        // the clamp.
        let rect = self.shell_menu_rect()?;
        let (x, y) = (rect.x.max(0) as u16, rect.y.max(0) as u16);
        let kind = match kind {
            ContextMenuKind::FileExplorer => "fileExplorer",
            ContextMenuKind::NewTab => "newTab",
            ContextMenuKind::Tab => "tab",
            ContextMenuKind::CloseSplit => "closeSplit",
        };
        Some(ContextMenuView {
            kind,
            x,
            y,
            highlighted: core.highlighted,
            items: w.context_menu_labels()?,
        })
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
                    text: fresh_i18n::t!("event_debug.no_events").into_owned(),
                    selected: false,
                });
            }
            return Some(AuxModalView {
                kind: "eventDebug",
                title: fresh_i18n::t!("event_debug.title").into_owned(),
                rect: None,
                lines,
                footer: Some(fresh_i18n::t!("event_debug.help_text").into_owned()),
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
        /// The value as the JSON carries it; a `percent` displays ×100.
        value: f64,
        min: Option<f64>,
        max: Option<f64>,
        integer: bool,
        percent: bool,
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
        /// Entry row carrying keyboard focus (only while the control itself
        /// is focused), same condition the TUI's list highlight uses.
        focused: Option<usize>,
        /// The add-new row is the focused row (`focused_entry == None` on a
        /// focused control), mirroring the TUI's add-row highlight.
        add_focused: bool,
    },
    #[serde(rename_all = "camelCase")]
    ObjectArray {
        entries: Vec<String>,
        /// Focused entry row / add-row focus, as in `Map` above.
        focused: Option<usize>,
        add_focused: bool,
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

/// `store` is the surface's — the page's or the dialog's — and says which
/// control is live (a text field being edited, a dropdown's list up).
/// The row a map's or an object array's list cursor is on, while the list
/// has the keyboard.
fn list_cursor(
    c: &crate::view::settings::items::SettingControl,
    path: &str,
    store: &crate::widgets::WidgetPanelState,
) -> Option<usize> {
    if store.focus_key != path {
        return None;
    }
    let spec = crate::view::settings::widget_map::live_widget(path, c, path);
    crate::view::settings::live::list_row(store, &spec, path)
}

fn setting_control_view(
    c: &crate::view::settings::items::SettingControl,
    path: &str,
    store: &crate::widgets::WidgetPanelState,
) -> SettingControlView {
    use crate::view::settings::items::SettingControl as C;
    match c {
        C::Toggle { checked, .. } => SettingControlView::Toggle { checked: *checked },
        C::Number {
            value,
            min,
            max,
            integer,
            percent,
            ..
        } => SettingControlView::Number {
            value: *value,
            min: *min,
            max: *max,
            integer: *integer,
            percent: *percent,
        },
        C::Dropdown {
            selected, options, ..
        } => SettingControlView::Dropdown {
            selected: *selected,
            options: options.clone(),
            open: crate::widgets::kinds::dropdown::is_open(path, store),
        },
        C::Text {
            value, placeholder, ..
        } => SettingControlView::Text {
            value: value.clone(),
            editing: store.focus_key == path,
            placeholder: placeholder.clone(),
        },
        // A text list's focused row is the one whose field is live.
        C::TextList { items, .. } => SettingControlView::TextList {
            items: items.clone(),
            focused: crate::view::settings::live::text_list::live_row(store, path).flatten(),
        },
        // The columns and cursors as the kind resolves them — the same call
        // the TUI's description makes — so the row indices the web sends
        // back (`ControlDualListIncluded/Available(idx, row)`) name the rows
        // the kind's `dual_focus` press expects.
        C::DualList {
            options,
            included,
            excluded,
            ..
        } => {
            use crate::widgets::kinds::dual_list as dl;
            let opts: Vec<fresh_core::api::DualListOption> = options
                .iter()
                .map(|(value, label)| fresh_core::api::DualListOption {
                    value: value.clone(),
                    label: label.clone(),
                })
                .collect();
            let st = dl::resolve(
                &opts,
                &dl::DualListSeed {
                    included,
                    excluded,
                    active_included: false,
                    available_cursor: 0,
                    included_cursor: 0,
                },
                Some(path),
                &store.instance_states,
                store.focus_key == path,
            );
            let name = |v: &String| {
                options
                    .iter()
                    .find(|(value, _)| value == v)
                    .map(|(_, label)| label.clone())
                    .unwrap_or_else(|| v.clone())
            };
            SettingControlView::DualList {
                included: st.included.iter().map(name).collect(),
                available: st.available.iter().map(name).collect(),
                included_cursor: st.included_cursor,
                available_cursor: st.available_cursor,
                active_column: match st.active_included {
                    true => "included",
                    false => "available",
                },
            }
        }
        // Rows must read exactly as the TUI's: the same domain helper
        // (`map_display_value`, e.g. `/grammar` → "Assembly") formats the
        // value preview, and the value-column title mirrors the TUI's
        // `Name │ <Col>` header — never the raw JSON blob. The cursor is the
        // list's, as the kind resolves it, while the list has the keyboard.
        C::Map {
            entries,
            display_field,
            no_add,
            ..
        } => {
            let cursor = list_cursor(c, path, store);
            SettingControlView::Map {
                entries: entries
                    .iter()
                    .map(|(k, v)| MapEntryView {
                        key: k.clone(),
                        display: crate::view::settings::items::map_display_value(
                            display_field.as_deref(),
                            v,
                        ),
                    })
                    .collect(),
                column: display_field
                    .as_deref()
                    .map(crate::view::settings::widget_map::column_title),
                no_add: *no_add,
                focused: cursor.filter(|r| *r < entries.len()),
                add_focused: cursor.is_some() && cursor == c.add_row(),
            }
        }
        // Same combo → action row text the TUI renders (keybinding-shaped
        // entries), collapsing to the bare display value when there is no
        // key combo (LSP server lists and other non-keybinding arrays).
        C::ObjectArray {
            items,
            display_field,
            ..
        } => {
            let cursor = list_cursor(c, path, store);
            SettingControlView::ObjectArray {
                entries: items
                    .iter()
                    .map(|b| {
                        let (combo, action) = crate::view::settings::items::object_array_row(
                            display_field.as_deref(),
                            b,
                        );
                        match combo.trim().is_empty() {
                            true => action,
                            false => format!("{combo} → {action}"),
                        }
                    })
                    .collect(),
                focused: cursor.filter(|r| *r < items.len()),
                add_focused: cursor.is_some() && cursor == c.add_row(),
            }
        }
        C::Json { text, .. } => SettingControlView::Json {
            value: text.clone(),
        },
        C::Complex { type_name } => SettingControlView::Complex {
            type_name: type_name.clone(),
        },
    }
}

fn setting_item_view(
    item: &crate::view::settings::items::SettingItem,
    i: usize,
    selected: bool,
    store: &crate::widgets::WidgetPanelState,
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
        control: setting_control_view(&item.control, &item.path, store),
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
                    .map(|(i, it)| setting_item_view(it, i, i == st.selected_item, &st.controls))
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
                .map(|(i, it)| setting_item_view(it, i, i == d.selected_item, &d.controls))
                .collect(),
            selected_item: d.selected_item,
            focus_on_buttons: d.focus_on_buttons,
            focused_button: d.focused_button,
            no_delete: d.no_delete,
        });

        Some(SettingsView {
            title: "Settings".to_string(),
            focus: match st.focus_panel() {
                FocusPanel::Settings => "settings",
                FocusPanel::Footer => "footer",
                FocusPanel::Categories => "categories",
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
