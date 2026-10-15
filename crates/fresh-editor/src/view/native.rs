//! The display list's surfaces as content, for a frontend that lays them out
//! itself.
//!
//! `Editor::tree_view` ships the web what the shell's layout drew: items at
//! character cells, labels already padded and elided to the cell width. A
//! frontend that copies that faithfully looks like a terminal, because it is
//! one. This module projects the same surfaces one step earlier — from the
//! content models the shell builds its descriptions from — so the web can
//! give them real buttons, a proportional font and pixel spacing.
//!
//! **The tree still routes every input.** Each interactive piece carries the
//! key of the tree node it mirrors (in [`key_json`]'s wire form), and the
//! web's `node` message presses that node where the shell's layout put it
//! (`Editor::node_rect`). So a native control can do exactly what clicking
//! the terminal's control does and nothing else, and the two renderers cannot
//! disagree about what a press means. `scene_parity` checks that every key a
//! projection names resolves to a node with area.

use crate::app::Editor;
use serde::Serialize;
use serde_json::{json, Value};

/// A tree key in the wire form the web sends back in `node` messages:
/// a string for `Key::Str`, `{"ns", "i"}` for `Key::Pair`, a number for
/// `Key::Int`.
pub fn key_json(k: &fresh_ui::Key) -> Value {
    match k {
        fresh_ui::Key::Str(s) => json!(&**s),
        fresh_ui::Key::Int(i) => json!(i),
        fresh_ui::Key::Pair(ns, i) => json!({ "ns": &**ns, "i": i }),
    }
}

/// Everything projected for native layout. Each field is `None`/empty when
/// its surface is not showing.
#[derive(Debug, Clone, Default, Serialize)]
pub struct NativeView {
    pub confirm: Option<ConfirmCardView>,
    /// The plugin panels: the dock, a floating panel, sidebar sections.
    pub panels: Vec<NativePanelView>,
}

/// A plugin panel's interior, as its spec and the host's state for it.
///
/// The panel's frame (its title bar, close box, border) stays the display
/// list's; this is what goes inside it, at `rect`. Presses come back as the
/// widget's identity (`widget` messages with `surface: "panel"`) and are
/// resolved against this same spec by `app::native_panels`.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NativePanelView {
    /// `dock`, `floating`, `sidebar` or `pane` (a panel mounted into a
    /// split pane: Search & Replace, a review diff, the welcome page).
    pub kind: &'static str,
    /// The sidebar section index, for `sidebar`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub section: Option<usize>,
    /// The pane's leaf id, for `pane`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pane: Option<u64>,
    /// The interior's rectangle, in cells.
    pub rect: Value,
    pub spec: Value,
    /// The host-owned instance state of the panel's stateful widgets, by
    /// widget key: what the spec only seeds (a text field's value and caret,
    /// a list's selection, a tree's expanded rows, …).
    pub instances: serde_json::Map<String, Value>,
    /// The widget holding the panel's keyboard focus; empty for none.
    pub focus_key: String,
    /// Whether the panel itself has the keyboard.
    pub keyboard: bool,
    /// A floating panel anchored to a point (a plugin's pop-up menu) rather
    /// than centred: a press outside it dismisses it, where a centred
    /// modal swallows that press.
    pub anchored: bool,
    /// A floating panel's whole box (its frame), in cells — `rect` is the
    /// interior inside it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub frame: Option<Value>,
    /// The title a centred floating panel wears in its top border.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    /// The tree key of the frame's `[×]`, when it offers one.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub close_key: Option<Value>,
}

/// The confirmation card (`view::confirm::Confirm`, drawn in the terminal by
/// `shell::confirm`).
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ConfirmCardView {
    pub title: String,
    pub body: String,
    /// The quieter second paragraph; empty for none.
    pub detail: String,
    pub buttons: Vec<ConfirmButtonView>,
    /// The tree key of the card itself (`shell::confirm::key`).
    pub key: Value,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ConfirmButtonView {
    /// The label split around its accelerator: `[before, letter, after]`, or
    /// the whole label in `before` when it has none in its text.
    pub label: [String; 3],
    pub destructive: bool,
    /// Holds the keyboard: what Enter takes.
    pub selected: bool,
    /// Under the pointer.
    pub hovered: bool,
    /// What Esc resolves to.
    pub escape: bool,
    /// The tree key of the button (`shell::confirm::button_key`).
    pub key: Value,
}

impl Editor {
    /// The surfaces projected for native layout. See the module docs.
    pub fn native_view(&self) -> NativeView {
        NativeView {
            confirm: self.confirm_card_view(),
            panels: self.native_panels(),
        }
    }

    fn native_panels(&self) -> Vec<NativePanelView> {
        use crate::app::PanelSlot;
        use crate::view::shell::widgets::Slot;
        let mut slots = vec![(PanelSlot::Dock, Slot::Dock, "dock", None)];
        slots.push((PanelSlot::Floating, Slot::Floating, "floating", None));
        for (i, s) in self.sidebar_sections.iter().enumerate() {
            if s.panel.is_some() {
                slots.push((PanelSlot::Sidebar(i), Slot::Sidebar(i), "sidebar", Some(i)));
            }
        }
        let mut out: Vec<NativePanelView> = slots
            .into_iter()
            .filter_map(|(panel_slot, slot, kind, section)| {
                let rect = self.node_rect(&crate::view::shell::panel::interior_key(slot))?;
                let interior = self.panel_interior(panel_slot)?;
                // A floating panel's frame: its box and what its border says.
                let (frame, described) = match panel_slot {
                    PanelSlot::Floating => (
                        self.node_rect(&crate::view::shell::panel::key()),
                        self.panel_description(),
                    ),
                    _ => (None, None),
                };
                let instances = interior
                    .states
                    .iter()
                    .filter_map(|(k, st)| Some((k.clone(), instance_json(st)?)))
                    .collect();
                Some(NativePanelView {
                    kind,
                    section,
                    pane: None,
                    rect: json!({ "x": rect.x, "y": rect.y, "w": rect.width, "h": rect.height }),
                    spec: self.styled_spec(&interior.spec),
                    instances,
                    focus_key: interior.focus_key.clone(),
                    keyboard: interior.keyboard,
                    anchored: matches!(panel_slot, PanelSlot::Floating)
                        && self.floating_widget_panel.as_ref().is_some_and(|f| {
                            matches!(f.placement, crate::app::PanelPlacement::Anchored { .. })
                        }),
                    frame: frame
                        .map(|r| json!({ "x": r.x, "y": r.y, "w": r.width, "h": r.height })),
                    title: described.as_ref().and_then(|d| d.title.clone()),
                    close_key: described
                        .as_ref()
                        .filter(|d| d.closable)
                        .map(|_| key_json(&crate::view::shell::panel::close_key())),
                })
            })
            .collect();
        // Panels mounted into panes, in the panes' order of id so the
        // projection is stable frame to frame.
        let mut panes: Vec<_> = self.pane_interiors().into_iter().collect();
        panes.sort_by_key(|(leaf, _)| leaf.0 .0);
        for (leaf, interior) in panes {
            let Some(rect) =
                self.node_rect(&crate::view::shell::panel::interior_key(Slot::Pane(leaf)))
            else {
                continue;
            };
            out.push(NativePanelView {
                kind: "pane",
                section: None,
                pane: Some(leaf.0 .0 as u64),
                rect: json!({ "x": rect.x, "y": rect.y, "w": rect.width, "h": rect.height }),
                spec: self.styled_spec(&interior.spec),
                instances: interior
                    .states
                    .iter()
                    .filter_map(|(k, st)| Some((k.clone(), instance_json(st)?)))
                    .collect(),
                focus_key: interior.focus_key.clone(),
                keyboard: interior.keyboard,
                anchored: false,
                frame: None,
                title: None,
                close_key: None,
            });
        }
        out
    }

    fn confirm_card_view(&self) -> Option<ConfirmCardView> {
        use crate::view::shell::confirm;
        let c = self.active_window().prompt.as_ref()?.confirm.as_ref()?;
        let buttons = c
            .choices
            .iter()
            .enumerate()
            .map(|(i, ch)| {
                let label = match c.mnemonic_span(i) {
                    Some((a, b))
                        if a <= b
                            && ch.label.is_char_boundary(a)
                            && ch.label.is_char_boundary(b)
                            && b <= ch.label.len() =>
                    {
                        [
                            ch.label[..a].to_string(),
                            ch.label[a..b].to_string(),
                            ch.label[b..].to_string(),
                        ]
                    }
                    _ => [ch.label.clone(), String::new(), String::new()],
                };
                ConfirmButtonView {
                    label,
                    destructive: ch.tone == crate::view::confirm::Tone::Destructive,
                    selected: c.selected == i,
                    hovered: c.hovered == Some(i),
                    escape: c.escape == Some(i),
                    key: key_json(&confirm::button_key(i)),
                }
            })
            .collect();
        Some(ConfirmCardView {
            title: c.title.clone(),
            body: c.body.clone(),
            detail: c.detail.clone(),
            buttons,
            key: key_json(&confirm::key()),
        })
    }
}

impl Editor {
    /// A spec as JSON, with every style's colours also resolved to CSS
    /// (`fgCss`/`bgCss` beside `fg`/`bg`). A plugin colours text by theme
    /// key — `ui.help_key_fg` for a key hint, a syntax colour for a logo —
    /// and only the host knows what those keys are in the theme in use.
    fn styled_spec(&self, spec: &fresh_core::api::WidgetSpec) -> Value {
        let mut v = serde_json::to_value(spec).unwrap_or(Value::Null);
        let theme = self.theme();
        resolve_styles(&mut v, &theme);
        v
    }
}

/// Walk `v`, adding `fgCss`/`bgCss` to every style object whose `fg`/`bg` is
/// a theme key or an `[r, g, b]` triple.
fn resolve_styles(v: &mut Value, theme: &crate::view::theme::Theme) {
    match v {
        Value::Object(o) => {
            for name in ["style", "hoverStyle"] {
                if let Some(Value::Object(st)) = o.get_mut(name) {
                    for (from, to) in [("fg", "fgCss"), ("bg", "bgCss")] {
                        let css = match st.get(from) {
                            Some(Value::String(key)) => theme
                                .resolve_theme_key(key)
                                .and_then(crate::view::scene::css_color),
                            Some(Value::Array(rgb)) if rgb.len() == 3 => {
                                let c = |i: usize| rgb[i].as_u64().unwrap_or(0).min(255);
                                Some(format!("#{:02x}{:02x}{:02x}", c(0), c(1), c(2)))
                            }
                            _ => None,
                        };
                        if let Some(css) = css {
                            st.insert(to.to_string(), Value::String(css));
                        }
                    }
                }
            }
            for (_, child) in o.iter_mut() {
                resolve_styles(child, theme);
            }
        }
        Value::Array(a) => a.iter_mut().for_each(|c| resolve_styles(c, theme)),
        _ => {}
    }
}

/// A widget's host-owned state, in the names the page's widget renderer
/// reads (`widgetEl`, js/65-widgets.js).
fn instance_json(st: &crate::widgets::WidgetInstanceState) -> Option<Value> {
    use crate::widgets::WidgetInstanceState as S;
    Some(match st {
        S::None => return None,
        S::List { selected_index } => json!({ "selectedIndex": selected_index }),
        S::Text {
            editor,
            completions,
            completion_selected_index,
            completion_navigated,
            ..
        } => {
            // The caret as a byte offset into the joined value, which is how
            // the page places it (and how a click reports it back).
            let before: usize = editor
                .lines
                .iter()
                .take(editor.cursor_row)
                .map(|l| l.len() + 1)
                .sum();
            json!({
                "textValue": editor.value(),
                "cursorByte": before + editor.cursor_col,
                "completions": completions.iter().map(|c| c.value.clone()).collect::<Vec<_>>(),
                "completionSelected": completion_selected_index,
                "completionNavigated": completion_navigated,
            })
        }
        S::Tree {
            selected_index,
            expanded_keys,
        } => json!({
            "selectedIndex": selected_index,
            "expandedKeys": expanded_keys.iter().collect::<Vec<_>>(),
        }),
        S::Number { value, .. } => json!({ "numberValue": value }),
        S::Dropdown {
            selected_index,
            open,
            highlight,
        } => json!({
            "selectedIndex": selected_index,
            "dropdownOpen": open,
            "highlight": highlight,
        }),
        S::Radio { selected_index } => json!({ "selectedIndex": selected_index }),
        S::DualList {
            included,
            active_included,
            available_cursor,
            included_cursor,
        } => json!({
            "included": included,
            "activeIncluded": active_included,
            "availableCursor": available_cursor,
            "includedCursor": included_cursor,
        }),
    })
}
