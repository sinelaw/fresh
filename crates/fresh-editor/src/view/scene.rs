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
            when,
            checkbox,
        } => MenuItemView::Action {
            label: label.clone(),
            action: action.clone(),
            args: args.clone(),
            accel: editor.accelerator_for(action),
            enabled: when
                .as_ref()
                .map(|w| editor.menu_state().context.get(w))
                .unwrap_or(true),
            checked: checkbox
                .as_ref()
                .map(|c| editor.menu_state().context.get(c)),
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
