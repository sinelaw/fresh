//! `Dropdown` — `label: [value ▼]` trigger with a floating option pop-over.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::PanelPopup;

pub struct Dropdown;

impl WidgetImpl for Dropdown {
    /// **The dropdown's keys, defined once** (`docs/internal/widget-controls-own-interaction.md` R3).
    ///
    /// Closed: ↑/↓ are not the dropdown's — they pass, so they move focus on;
    /// ←/→ step the value in place (the `◂`/`▸` affordance), a committed
    /// change each; Enter, Space and Alt+↓ open the list on the current value.
    ///
    /// Open: ↑/↓, PgUp/PgDn and Home/End move the *highlight* only — nothing
    /// fires; typing jumps to a matching option ([`WidgetImpl::on_text`]).
    /// Enter, Space or Alt+↑ commit the highlight — one `change`, and only if
    /// it differs — and close. Tab commits and passes on, so focus still
    /// moves. Esc closes with no event.
    fn on_key(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        _viewport: super::Viewport,
        key: &crate::keys::KeySeq,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        use super::KeyDisposition::{Consumed, Pass, PassAfter};
        use crossterm::event::{KeyCode, KeyModifiers};
        let Some(key) = key.single() else {
            return Pass;
        };
        let alt = key.mods() == KeyModifiers::ALT;
        if !key.mods().is_empty() && !alt {
            return Pass;
        }
        if !is_open(widget_key, panel) {
            return match (key.code(), alt) {
                (KeyCode::Left, false) => {
                    cycle_selection(spec, widget_key, panel, -1, fx);
                    Consumed
                }
                (KeyCode::Right, false) => {
                    cycle_selection(spec, widget_key, panel, 1, fx);
                    Consumed
                }
                (KeyCode::Enter | KeyCode::Char(' '), false) | (KeyCode::Down, true) => {
                    set_open(spec, widget_key, panel, true, fx);
                    Consumed
                }
                _ => Pass,
            };
        }
        if let Some(nav) = super::popup_list::nav_of(key) {
            move_highlight(spec, widget_key, panel, nav);
            return Consumed;
        }
        match (key.code(), alt) {
            (KeyCode::Enter | KeyCode::Char(' '), false) | (KeyCode::Up, true) => {
                commit(spec, widget_key, panel, fx);
                Consumed
            }
            (KeyCode::Tab | KeyCode::BackTab, false) => {
                commit(spec, widget_key, panel, fx);
                PassAfter
            }
            (KeyCode::Esc, false) => {
                set_open(spec, widget_key, panel, false, fx);
                Consumed
            }
            // The list holds the keyboard while it is up: the sideways arrows
            // mean nothing to it and must not walk focus out from under it.
            (KeyCode::Left | KeyCode::Right, false) => Consumed,
            _ => Pass,
        }
    }

    /// Typing into an open list jumps the highlight to the next option that
    /// starts with what was typed. A closed dropdown types nothing.
    fn on_text(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        text: &str,
        _fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        let WidgetSpec::Dropdown { options, .. } = spec else {
            return super::KeyDisposition::Pass;
        };
        if !is_open(widget_key, panel) || text.trim().is_empty() {
            return super::KeyDisposition::Pass;
        }
        let from = highlight_of(spec, widget_key, panel).max(0) as usize;
        if let Some(i) = super::popup_list::jump_to_prefix(options, from, text) {
            set_highlight(widget_key, panel, i as i32);
        }
        super::KeyDisposition::Consumed
    }

    /// Pointer model: clicking the `[value ▼]` trigger toggles the option
    /// list (closing it this way is a cancel); clicking an option row commits
    /// that option — one `change` if it differs — and closes the list. The
    /// host owns both the open flag and the index, so both hits are fully
    /// handled here — the recorded events never reach the plugin raw.
    fn on_pointer(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        event_type: &str,
        payload: &serde_json::Value,
        fx: &mut super::PointerFx,
    ) -> super::PointerDisposition {
        match event_type {
            "dropdown_toggle" => {
                let now_open = !is_open(widget_key, panel);
                set_open(spec, widget_key, panel, now_open, &mut fx.key);
                super::PointerDisposition::Consumed
            }
            "dropdown_select" => {
                if let Some(idx) = payload.get("index").and_then(|v| v.as_i64()) {
                    set_highlight(widget_key, panel, idx as i32);
                }
                commit(spec, widget_key, panel, &mut fx.key);
                super::PointerDisposition::Consumed
            }
            _ => super::PointerDisposition::Default,
        }
    }

    /// Focus leaving an open list closes it, uncommitted — a menu goes away
    /// when you act elsewhere, and what it had highlighted was never chosen.
    fn on_focus_change(
        &self,
        panel: &mut crate::widgets::WidgetPanelState,
        key: &str,
        gained: bool,
    ) {
        if gained {
            return;
        }
        if let Some(WidgetInstanceState::Dropdown {
            open, highlight, ..
        }) = panel.instance_states.get_mut(key)
        {
            *open = false;
            *highlight = None;
        }
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("dropdown");
        if let WidgetSpec::Dropdown { key: Some(k), .. } = spec {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = true;
            }
        }
        m
    }
}

/// A `Dropdown`'s two pieces of state, once the spec and the instance map
/// have been reconciled.
pub struct Resolved {
    /// The option index the trigger shows, clamped into the current set.
    pub selected: i32,
    /// Whether the option pop-over is up.
    pub open: bool,
    /// The row the open list highlights: the stored highlight while open,
    /// else the committed value. Clamped into the current set.
    pub highlight: i32,
}

/// **Where a `Dropdown`'s selection and open flag actually come from.**
///
/// Instance state is authoritative after first render; the spec's
/// `selected_index` is a seed. A panel that renders statelessly — no prior
/// instance state, e.g. the Settings dialog re-emitting its model each frame —
/// falls back to the spec's `open`: the host model drives the expansion
/// directly, so it is honored as-is with no focus gate, because that surface's
/// own focus model already decided.
///
/// Pulled out of the collector because the *description* needs the same
/// answer, and a second copy of these rules is a second place for them to
/// drift. This is a pure function of what it is handed — no `next_state`
/// write — which is what lets a description call it. See
/// `view::shell::widgets`'s `Dropdown` arm.
pub fn resolve(
    options: &[String],
    spec_selected: i32,
    spec_open: bool,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    is_focused: bool,
) -> Resolved {
    let (cur, state_open, hl) = match key {
        Some(k) if !k.is_empty() => match prev.get(k) {
            Some(WidgetInstanceState::Dropdown {
                selected_index,
                open,
                highlight,
            }) => (*selected_index, Some(*open), *highlight),
            _ => (spec_selected, None, None),
        },
        _ => (spec_selected, None, None),
    };
    let clamp = |i: i32| match options.is_empty() {
        true => 0,
        false => i.clamp(0, options.len() as i32 - 1),
    };
    let selected = clamp(cur);
    // Instance-state open only persists while the widget is focused —
    // a blur (Tab away, click elsewhere) closes it.
    let open = match state_open {
        Some(o) => o && is_focused,
        None => spec_open,
    } && !options.is_empty();
    let highlight = match open {
        true => clamp(hl.unwrap_or(selected)),
        false => selected,
    };
    Resolved {
        selected,
        open,
        highlight,
    }
}

/// The column the pop-over drops under: the display width of the trigger row's
/// text before the button's `[`.
///
/// Measured in *display* width, never bytes — the focus marker `▸ ` is 4 bytes
/// but 2 columns, so byte length pushed the pop-over two cells right of the
/// value it belongs under.
pub fn anchor_col(row_text: &str, button_start: usize) -> u32 {
    use crate::primitives::display_width::str_width;
    row_text
        .get(..button_start)
        .map(|prefix| str_width(prefix) as u32)
        .unwrap_or(0)
}

/// The open option list: windowing, padding and row styling.
///
/// Windowing lives here with the rest of the render — clamp the scroll, slice
/// the visible rows — and each row is handed over as display text plus its
/// absolute index, so the consumer knows nothing about options or selection.
///
/// **Every row is the width of the widest option.** They were each their own
/// text's length, so the selected row's highlight was the width of its *word* —
/// "default" lit while "macos-gui" beside it set the box's width, leaving the
/// band two columns short of the edge on one row and flush on another. A
/// pop-over is a column of equal cells; padding them here also fixes the box,
/// which takes its width from the longest row it is given.
pub fn popup_of(
    options: &[String],
    selected_index: i32,
    scroll_offset: u32,
    hover_popup_row: &str,
    widget_key: &str,
    anchor_col: u32,
) -> PanelPopup {
    use crate::widgets::render::{KEY_COMPLETION_FG, KEY_COMPLETION_SEL_BG, KEY_COMPLETION_SEL_FG};
    use fresh_core::api::{OverlayColorSpec, OverlayOptions};
    use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};

    let visible = options.len().min(crate::widgets::DROPDOWN_VISIBLE_OPTIONS);
    let scroll = follow_scroll(options.len(), selected_index, scroll_offset as usize);
    // A list longer than its window carries a scrollbar in its last column.
    let bar = scrollbar_cells(options.len(), visible, scroll);
    let cell_cols = options
        .iter()
        .map(|o| crate::primitives::display_width::str_width(o))
        .max()
        .unwrap_or(0);
    let mut entries = Vec::new();
    let mut row_indices = Vec::new();
    for (row, (idx, opt)) in options
        .iter()
        .enumerate()
        .skip(scroll)
        .take(visible)
        .enumerate()
    {
        let text = format!(" {} ", crate::widgets::render::cell(opt, cell_cols));
        let mut e = TextPropertyEntry::text(&text);
        if let Some(bar) = &bar {
            let start = e.text.len();
            e.text.push(if bar[row] { '█' } else { '│' });
            let end = e.text.len();
            e.inline_overlays.push(InlineOverlay {
                start,
                end,
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_FG)),
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }
        let selected = idx == selected_index as usize;
        // The row under the pointer, which the tree reports because the
        // runtime's own hover probe cannot see a pop-over's rows. Selected
        // wins: a hover band under the selection would only mute it.
        let hovered = !selected && hover_popup_row == idx.to_string();
        e.inline_overlays.push(InlineOverlay {
            start: 0,
            end: text.len(),
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(if selected {
                    KEY_COMPLETION_SEL_FG
                } else {
                    KEY_COMPLETION_FG
                })),
                bg: match (selected, hovered) {
                    (true, _) => Some(OverlayColorSpec::theme_key(KEY_COMPLETION_SEL_BG)),
                    (false, true) => Some(OverlayColorSpec::theme_key(
                        crate::widgets::render::KEY_HOVER_BG,
                    )),
                    (false, false) => None,
                },
                bold: selected,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
        entries.push(e);
        row_indices.push(idx);
    }
    PanelPopup {
        widget_key: widget_key.to_string(),
        anchor_row: 0,
        anchor_col,
        anchor_absolute: false,
        entries,
        row_indices,
    }
}

/// The first option row the list shows: the spec's offset, moved just
/// enough to keep `highlight` in the window, and clamped so the window never
/// runs past the end — the shared pop-up list's rule
/// ([`super::popup_list::window`]), run on every layout.
pub fn follow_scroll(len: usize, highlight: i32, scroll_offset: usize) -> usize {
    let h = (highlight >= 0).then_some(highlight as usize);
    super::popup_list::window(
        len,
        crate::widgets::DROPDOWN_VISIBLE_OPTIONS,
        h,
        scroll_offset,
    )
    .0
}

fn scrollbar_cells(len: usize, visible: usize, scroll: usize) -> Option<Vec<bool>> {
    super::popup_list::scrollbar(len, visible, scroll)
}

/// **Which keyed `Dropdown` in this spec has its option list up**, as a walk
/// of the spec rather than a field the render walk left behind.
///
/// The one thing `FloatingWidgetState::popup` was still read for was this
/// string — `UiFact::WidgetPopupDismiss` needs to know which widget to toggle
/// shut — and the rest of that struct (rendered rows, an anchor, per-row click
/// payloads) had no described reader at all: the described `Dropdown` arm
/// builds its own pop-over from [`popup_of`] and never looked at it. So the
/// field is gone and this is what replaced it.
///
/// [`resolve`] is what decides, so this cannot disagree with what the
/// description painted: same clamp, same focus gate, same spec-`open`
/// fallback for a surface that renders statelessly. At most one list is up at
/// a time (the focused widget's), and the first in declaration order wins if a
/// spec somehow says otherwise — which is exactly what the collector's
/// `popups.into_iter().next()` did.
pub fn open_key(
    spec: &WidgetSpec,
    states: &HashMap<String, WidgetInstanceState>,
    focus_key: &str,
) -> Option<String> {
    if let WidgetSpec::Dropdown {
        options,
        selected_index,
        focused,
        open,
        key,
        ..
    } = spec
    {
        let k = key.as_deref();
        let keyed = k.is_some_and(|k| !k.is_empty());
        let is_focused = match keyed {
            true => k == Some(focus_key),
            false => *focused,
        };
        if resolve(options, *selected_index, *open, k, states, is_focused).open {
            return Some(key.clone().unwrap_or_default());
        }
    }
    spec.children().find_map(|c| open_key(c, states, focus_key))
}

/// Is this Dropdown's option popup open?
///
/// **The focus gate is applied here, because it is not stored.** A blur closes
/// the list, and that rule used to be enforced by the render walk writing the
/// gated flag back — so this could read the raw field and still get the gated
/// answer. The walk no longer decides ([`collect_dropdown`]), so the gate has
/// to be where every other reader applies it: at the read. Without this, a
/// press on the trigger of a dropdown that was blurred while open would toggle
/// a flag the user cannot see, and the list would stay shut on the click that
/// should have opened it.
pub fn is_open(widget_key: &str, panel: &crate::widgets::WidgetPanelState) -> bool {
    !widget_key.is_empty()
        && panel.focus_key == widget_key
        && matches!(
            panel.instance_states.get(widget_key),
            Some(WidgetInstanceState::Dropdown { open: true, .. })
        )
}

/// Step the selection by `delta` with wraparound, preserving the
/// popup's open state; queues `change` when the selection moved.
pub fn cycle_selection(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    delta: i32,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::Dropdown {
        options,
        selected_index: spec_sel,
        ..
    } = spec
    else {
        return;
    };
    if options.is_empty() {
        return;
    }
    let (cur, open, highlight) = match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Dropdown {
            selected_index,
            open,
            highlight,
        }) => (*selected_index, *open, *highlight),
        _ => (*spec_sel, false, None),
    };
    let cur = cur.clamp(0, options.len() as i32 - 1);
    let new_sel = crate::widgets::wrap_index(cur, delta, options.len());
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Dropdown {
            selected_index: new_sel,
            open,
            highlight,
        },
    );
    if new_sel != cur {
        let value = options.get(new_sel as usize).cloned().unwrap_or_default();
        fx.events.push((
            "change".into(),
            serde_json::json!({ "index": new_sel, "value": value }),
        ));
    }
}

/// Set the selection to an absolute index (a click on an option row
/// of the open list), clamped into the option set, preserving the
/// popup's open state; queues `change` when the selection actually
/// moved. The absolute-index sibling of [`cycle_selection`].
pub fn set_selection(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    index: i32,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::Dropdown {
        options,
        selected_index: spec_sel,
        ..
    } = spec
    else {
        return;
    };
    if options.is_empty() {
        return;
    }
    let (cur, open, highlight) = match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Dropdown {
            selected_index,
            open,
            highlight,
        }) => (*selected_index, *open, *highlight),
        _ => (*spec_sel, false, None),
    };
    let new_sel = index.clamp(0, options.len() as i32 - 1);
    let changed = new_sel != cur.clamp(0, options.len() as i32 - 1);
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Dropdown {
            selected_index: new_sel,
            open,
            highlight,
        },
    );
    if changed {
        let value = options.get(new_sel as usize).cloned().unwrap_or_default();
        fx.events.push((
            "change".into(),
            serde_json::json!({ "index": new_sel, "value": value }),
        ));
    }
}

/// Open or close the option popup, preserving the selected index;
/// queues `dropdown_open` when the state actually flipped (never
/// `change` — opening/closing is not a value edit; the plugin needs
/// the distinction so e.g. Escape can close the list vs cancel the
/// dialog).
pub fn set_open(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    open: bool,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::Dropdown {
        selected_index: spec_sel,
        ..
    } = spec
    else {
        return;
    };
    let (cur, prev_open) = match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Dropdown {
            selected_index,
            open,
            ..
        }) => (*selected_index, *open),
        _ => (*spec_sel, false),
    };
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Dropdown {
            selected_index: cur,
            open,
            // The list opens on the current value, and closing forgets the
            // highlight: only a commit (`commit`) turns it into the value.
            highlight: open.then_some(cur),
        },
    );
    if open != prev_open {
        fx.events
            .push(("dropdown_open".into(), serde_json::json!({ "open": open })));
    }
}

/// The open list's highlighted row (the committed value when there is no
/// stored highlight), clamped into the option set.
fn highlight_of(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &crate::widgets::WidgetPanelState,
) -> i32 {
    let WidgetSpec::Dropdown {
        options,
        selected_index,
        open,
        ..
    } = spec
    else {
        return 0;
    };
    let focused = panel.focus_key == widget_key;
    resolve(
        options,
        *selected_index,
        *open,
        Some(widget_key),
        &panel.instance_states,
        focused,
    )
    .highlight
}

/// Put the open list's highlight on `index` (clamped when read). Fires
/// nothing: a highlight is not a value.
fn set_highlight(widget_key: &str, panel: &mut crate::widgets::WidgetPanelState, index: i32) {
    if let Some(WidgetInstanceState::Dropdown { highlight, .. }) =
        panel.instance_states.get_mut(widget_key)
    {
        *highlight = Some(index);
    }
}

/// Move the open list's highlight — the shared pop-up list's arithmetic
/// ([`super::popup_list::step`]), a window of rows per page.
fn move_highlight(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    nav: super::popup_list::Nav,
) {
    let WidgetSpec::Dropdown { options, .. } = spec else {
        return;
    };
    let cur = highlight_of(spec, widget_key, panel).max(0) as usize;
    let page = options.len().min(crate::widgets::DROPDOWN_VISIBLE_OPTIONS);
    let next = super::popup_list::step(options.len(), cur, nav, page);
    set_highlight(widget_key, panel, next as i32);
}

/// Commit the open list's highlight as the value — one `change` when it
/// differs from the value the list opened on — and close the list.
pub fn commit(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    fx: &mut super::KeyFx,
) {
    let chosen = highlight_of(spec, widget_key, panel);
    set_selection(spec, widget_key, panel, chosen, fx);
    set_open(spec, widget_key, panel, false, fx);
}

/// Kind policy for the plugin `SetDropdown` mutation: clamp the wire
/// index into THIS spec's option set, preserving an open popup. The
/// mutation arm in `plugin_dispatch` is a pure delegation.
pub fn set_index_state(
    spec: &WidgetSpec,
    prev: Option<&crate::widgets::WidgetInstanceState>,
    index: i32,
) -> crate::widgets::WidgetInstanceState {
    let len = match spec {
        WidgetSpec::Dropdown { options, .. } => options.len(),
        _ => 0,
    };
    let clamped = if len == 0 {
        0
    } else {
        index.clamp(0, len as i32 - 1)
    };
    let open = matches!(
        prev,
        Some(crate::widgets::WidgetInstanceState::Dropdown { open: true, .. })
    );
    crate::widgets::WidgetInstanceState::Dropdown {
        selected_index: clamped,
        open,
        // A plugin's set is the value now, and an open list shows it.
        highlight: open.then_some(clamped),
    }
}

#[cfg(test)]
mod scroll_tests {
    use super::*;

    fn opts(n: usize) -> Vec<String> {
        (0..n).map(|i| format!("option {i}")).collect()
    }

    /// ↓ past the last visible row scrolls the list with it.
    #[test]
    fn the_window_follows_the_selection() {
        let v = crate::widgets::DROPDOWN_VISIBLE_OPTIONS;
        assert_eq!(follow_scroll(20, 0, 0), 0);
        assert_eq!(follow_scroll(20, v as i32 - 1, 0), 0);
        assert_eq!(follow_scroll(20, v as i32, 0), 1);
        assert_eq!(follow_scroll(20, 19, 0), 20 - v);
        assert_eq!(follow_scroll(20, 2, 10), 2);
        assert_eq!(follow_scroll(3, 2, 0), 0);
    }

    /// The popup shows the selected row, and a scrollbar when it scrolls.
    #[test]
    fn a_long_list_shows_the_selection_and_a_scrollbar() {
        let o = opts(20);
        let p = popup_of(&o, 15, 0, "", "k", 0);
        assert!(p.row_indices.contains(&15));
        assert!(p
            .entries
            .iter()
            .all(|e| e.text.ends_with('█') || e.text.ends_with('│')));
        assert!(p.entries.iter().any(|e| e.text.ends_with('█')));
        let short = popup_of(&opts(3), 0, 0, "", "k", 0);
        assert!(short
            .entries
            .iter()
            .all(|e| !e.text.ends_with('█') && !e.text.ends_with('│')));
    }
}

/// **The dropdown's contract** (R3): closed, ↑/↓ are not its keys; open, the
/// keys move a highlight and fire nothing; Enter or a click commits with one
/// `change`; Esc cancels with none.
#[cfg(test)]
mod contract_tests {
    use super::super::{behavior, KeyDisposition, KeyFx};
    use super::*;
    use crate::widgets::WidgetPanelState;

    fn spec(n: usize) -> WidgetSpec {
        WidgetSpec::Dropdown {
            options: (0..n).map(|i| format!("opt {i}")).collect(),
            selected_index: 1,
            label: "L".into(),
            focused: false,
            label_width: 0,
            open: false,
            scroll_offset: 0,
            key: Some("d".into()),
        }
    }

    fn panel(spec: &WidgetSpec) -> WidgetPanelState {
        let mut p = WidgetPanelState::surface(spec.clone());
        p.focus_key = "d".into();
        p
    }

    fn key(spec: &WidgetSpec, p: &mut WidgetPanelState, k: &str) -> (KeyDisposition, KeyFx) {
        let mut fx = KeyFx::default();
        let seq: crate::keys::KeySeq = k.parse().expect("test key name parses");
        let d = behavior(spec).on_key(spec, "d", p, Default::default(), &seq, &mut fx);
        (d, fx)
    }

    fn changes(fx: &KeyFx) -> usize {
        fx.events.iter().filter(|(t, _)| t == "change").count()
    }

    fn value(spec: &WidgetSpec, p: &WidgetPanelState) -> i32 {
        let WidgetSpec::Dropdown { options, .. } = spec else {
            unreachable!()
        };
        resolve(options, 1, false, Some("d"), &p.instance_states, true).selected
    }

    #[test]
    fn closed_the_vertical_arrows_pass_and_change_nothing() {
        let s = spec(4);
        let mut p = panel(&s);
        for k in ["Up", "Down"] {
            let (d, fx) = key(&s, &mut p, k);
            assert_eq!(d, KeyDisposition::Pass, "{k}");
            assert_eq!(changes(&fx), 0);
        }
        assert_eq!(value(&s, &p), 1);
    }

    #[test]
    fn enter_space_and_alt_down_open_the_list() {
        for k in ["Enter", "Space", "M-Down"] {
            let s = spec(4);
            let mut p = panel(&s);
            let (d, fx) = key(&s, &mut p, k);
            assert_eq!(d, KeyDisposition::Consumed, "{k}");
            assert!(is_open("d", &p), "{k} opens");
            assert_eq!(changes(&fx), 0);
        }
    }

    #[test]
    fn open_the_keys_move_the_highlight_only_and_enter_commits_once() {
        let s = spec(20);
        let mut p = panel(&s);
        key(&s, &mut p, "Enter");
        for k in ["Down", "Down", "PageDown", "Up", "End", "Home", "Down"] {
            let (d, fx) = key(&s, &mut p, k);
            assert_eq!(d, KeyDisposition::Consumed, "{k}");
            assert_eq!(changes(&fx), 0, "{k} fires nothing");
        }
        assert_eq!(value(&s, &p), 1, "the value has not moved");
        // Home then Down: row 1, the value the list opened on — a commit
        // that did not move fires nothing.
        let (_, fx) = key(&s, &mut p, "Enter");
        assert_eq!(changes(&fx), 0, "no change when the commit did not move");
        assert!(!is_open("d", &p));
        assert_eq!(value(&s, &p), 1);
    }

    #[test]
    fn a_commit_that_moved_reports_the_new_value() {
        let s = spec(5);
        let mut p = panel(&s);
        key(&s, &mut p, "Enter");
        key(&s, &mut p, "Down");
        key(&s, &mut p, "Down");
        let (_, fx) = key(&s, &mut p, "Enter");
        assert_eq!(changes(&fx), 1);
        assert_eq!(value(&s, &p), 3);
    }

    #[test]
    fn escape_cancels_with_no_event() {
        let s = spec(5);
        let mut p = panel(&s);
        key(&s, &mut p, "Enter");
        key(&s, &mut p, "Down");
        let (d, fx) = key(&s, &mut p, "Esc");
        assert_eq!(d, KeyDisposition::Consumed);
        assert_eq!(changes(&fx), 0);
        assert!(!is_open("d", &p));
        assert_eq!(value(&s, &p), 1);
    }

    #[test]
    fn typing_jumps_the_highlight_to_a_match() {
        let s = WidgetSpec::Dropdown {
            options: vec!["alpha".into(), "beta".into(), "gamma".into()],
            selected_index: 0,
            label: String::new(),
            focused: false,
            label_width: 0,
            open: false,
            scroll_offset: 0,
            key: Some("d".into()),
        };
        let mut p = panel(&s);
        key(&s, &mut p, "Enter");
        let mut fx = KeyFx::default();
        assert_eq!(
            behavior(&s).on_text(&s, "d", &mut p, "g", &mut fx),
            KeyDisposition::Consumed
        );
        let (_, fx) = key(&s, &mut p, "Enter");
        assert_eq!(changes(&fx), 1);
        let WidgetSpec::Dropdown { options, .. } = &s else {
            unreachable!()
        };
        assert_eq!(
            resolve(options, 0, false, Some("d"), &p.instance_states, true).selected,
            2
        );
    }

    #[test]
    fn a_click_on_an_option_commits_it() {
        let s = spec(5);
        let mut p = panel(&s);
        key(&s, &mut p, "Enter");
        let mut fx = crate::widgets::kinds::PointerFx::default();
        behavior(&s).on_pointer(
            &s,
            "d",
            &mut p,
            "dropdown_select",
            &serde_json::json!({ "index": 4 }),
            &mut fx,
        );
        assert_eq!(changes(&fx.key), 1);
        assert!(!is_open("d", &p));
        assert_eq!(value(&s, &p), 4);
    }
}
