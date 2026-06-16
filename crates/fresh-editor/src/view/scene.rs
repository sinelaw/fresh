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
use ratatui::buffer::Buffer;
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
            enabled: crate::view::ui::menu::is_menu_item_enabled(item, &editor.menu_state().context),
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

        let menus: Vec<MenuEntry> = self
            .expanded_menu_definitions()
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

/// Plain text of cells `[x0, x1)` on row `y` of a rendered buffer. Used to lift
/// a status-bar segment's text out of the render (the built-in segments are not
/// otherwise exposed semantically — a Phase-3 cleanup will have the status
/// renderer emit the model directly).
fn text_in_row(buf: &Buffer, y: u16, x0: u16, x1: u16) -> String {
    let mut s = String::new();
    for x in x0..x1 {
        if let Some(cell) = buf.cell(ratatui::layout::Position::new(x, y)) {
            s.push_str(cell.symbol());
        }
    }
    s
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
    pub fn status_view(&self, buf: &Buffer) -> Option<StatusView> {
        let chrome = self.active_chrome();
        let (sy, sx, sw) = chrome.status_bar_area?;
        let bar_end = sx.saturating_add(sw);
        let mid = sx.saturating_add(sw / 2);
        let side = |x: u16| if x < mid { "left" } else { "right" };

        let mut ind: Vec<(&'static str, (u16, u16, u16), Option<String>)> = Vec::new();
        let mut push = |name: &'static str, area: Option<(u16, u16, u16)>| {
            if let Some(a) = area {
                ind.push((name, a, None));
            }
        };
        push("lsp", chrome.status_bar_lsp_area);
        push("warning", chrome.status_bar_warning_area);
        push("language", chrome.status_bar_language_area);
        push("encoding", chrome.status_bar_encoding_area);
        push("lineEnding", chrome.status_bar_line_ending_area);
        push("remote", chrome.status_bar_remote_area);
        push("trust", chrome.status_bar_trust_area);
        push("message", chrome.status_bar_message_area);
        for (key, a) in &chrome.status_bar_plugin_token_areas {
            ind.push(("plugin", *a, Some(key.clone())));
        }
        ind.sort_by_key(|(_, (_, start, _), _)| *start);

        let mut segments: Vec<StatusSegment> = Vec::new();
        let mut emit_gap = |segs: &mut Vec<StatusSegment>, from: u16, to: u16| {
            if to > from {
                let t = text_in_row(buf, sy, from, to);
                if !t.trim().is_empty() {
                    segs.push(StatusSegment {
                        name: "text",
                        key: None,
                        text: t.trim().to_string(),
                        x: from,
                        w: to - from,
                        side: side(from),
                    });
                }
            }
        };
        let mut cur = sx;
        for (name, (row, start, end), key) in &ind {
            emit_gap(&mut segments, cur, *start);
            segments.push(StatusSegment {
                name,
                key: key.clone(),
                text: text_in_row(buf, *row, *start, *end).trim().to_string(),
                x: *start,
                w: end.saturating_sub(*start),
                side: side(*start),
            });
            cur = (*end).max(cur);
        }
        emit_gap(&mut segments, cur, bar_end);

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
        let (scroll_start, visible, total) = sugg_area
            .map(|(_, s, v, t)| (s, v, t))
            .unwrap_or((p.scroll_offset, p.suggestions.len(), p.suggestions.len()));
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
            list_rect: sugg_area.map(|(r, _, _, _)| r).or(prompt_results).map(RectView::from),
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
