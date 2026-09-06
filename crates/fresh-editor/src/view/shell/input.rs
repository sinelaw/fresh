//! Input translation: the editor's terminal events into the shell's vocabulary.
//!
//! The other half of the seam. [`fold`](super::fold) carries output across —
//! a display list becomes cells, and host regions paint themselves. This
//! carries input the other way: the events `Editor::handle_key_press` and
//! `Editor::handle_mouse` already receive, expressed as [`fresh_ui::Input`] so
//! they can be dispatched into the tree.
//!
//! Translation is deliberately *lossy in one direction only*: everything
//! `fresh-ui` understands is passed through faithfully, and everything it does
//! not — key release/repeat kinds, unmapped keys — is dropped here rather than
//! approximated. An event
//! that does not translate returns `None` and stays on the existing path, which
//! is what makes the hybrid dispatch of stage S1 possible: the shell takes what
//! it understands, and the legacy floor keeps the rest.
//!
//! # The layout-character reading
//!
//! `fresh_input_parser::KeyPress` carries both the physical chord and, when
//! they differ, the character the key types on the current layout; the editor's
//! keymap decides which reading wins (see `Editor::handle_key_press`). That
//! decision belongs to the keybinding resolver, not to a widget — so this
//! translation reports the **physical** chord, and the layout reading stays
//! with the resolver at the root fallback where it is applied today.

use crossterm::event::{
    KeyCode as CtKey, KeyEventKind, KeyModifiers, MouseButton as CtButton, MouseEventKind,
};

use fresh_ui::{Axis, Input, KeyCode, KeyPress, Mods, MouseButton, Point};

/// Modifier flags, one for one.
pub fn mods(m: KeyModifiers) -> Mods {
    Mods {
        ctrl: m.contains(KeyModifiers::CONTROL),
        alt: m.contains(KeyModifiers::ALT),
        shift: m.contains(KeyModifiers::SHIFT),
    }
}

/// The other direction, for a fact that carries the modifiers a press had.
///
/// The tree reports them on the `Event`, and the handlers behind a fact are
/// crossterm's. One conversion, so a Ctrl+click means the same thing whether
/// it arrived through the tree or through the walk below it.
pub fn crossterm_mods(m: Mods) -> KeyModifiers {
    let mut out = KeyModifiers::empty();
    out.set(KeyModifiers::CONTROL, m.ctrl);
    out.set(KeyModifiers::ALT, m.alt);
    out.set(KeyModifiers::SHIFT, m.shift);
    out
}

/// The other direction, for a key the tree carried that a host table is
/// keyed by: the keymap resolver reads crossterm's vocabulary.
pub fn crossterm_key_code(code: KeyCode) -> Option<CtKey> {
    Some(match code {
        KeyCode::Char(c) => CtKey::Char(c),
        KeyCode::Enter => CtKey::Enter,
        KeyCode::Esc => CtKey::Esc,
        KeyCode::Tab => CtKey::Tab,
        KeyCode::BackTab => CtKey::BackTab,
        KeyCode::Backspace => CtKey::Backspace,
        KeyCode::Delete => CtKey::Delete,
        KeyCode::Insert => CtKey::Insert,
        KeyCode::Up => CtKey::Up,
        KeyCode::Down => CtKey::Down,
        KeyCode::Left => CtKey::Left,
        KeyCode::Right => CtKey::Right,
        KeyCode::Home => CtKey::Home,
        KeyCode::End => CtKey::End,
        KeyCode::PageUp => CtKey::PageUp,
        KeyCode::PageDown => CtKey::PageDown,
        KeyCode::F(n) => CtKey::F(n),
        KeyCode::Menu => CtKey::Menu,
        #[allow(unreachable_patterns)]
        _ => return None,
    })
}

/// A press the tree carried, as the event the keymap resolves.
pub fn crossterm_key_event(k: KeyPress) -> Option<crossterm::event::KeyEvent> {
    Some(crossterm::event::KeyEvent::new(
        crossterm_key_code(k.code)?,
        crossterm_mods(k.mods),
    ))
}

/// A key the tree understands, or `None` for one it has no vocabulary for.
///
/// `None` is not a failure: the key simply stays on the existing path.
pub fn key_code(code: CtKey) -> Option<KeyCode> {
    Some(match code {
        CtKey::Char(c) => KeyCode::Char(c),
        CtKey::Enter => KeyCode::Enter,
        CtKey::Esc => KeyCode::Esc,
        CtKey::Tab => KeyCode::Tab,
        CtKey::BackTab => KeyCode::BackTab,
        CtKey::Backspace => KeyCode::Backspace,
        CtKey::Delete => KeyCode::Delete,
        CtKey::Insert => KeyCode::Insert,
        CtKey::Up => KeyCode::Up,
        CtKey::Down => KeyCode::Down,
        CtKey::Left => KeyCode::Left,
        CtKey::Right => KeyCode::Right,
        CtKey::Home => KeyCode::Home,
        CtKey::End => KeyCode::End,
        CtKey::PageUp => KeyCode::PageUp,
        CtKey::PageDown => KeyCode::PageDown,
        CtKey::F(n) => KeyCode::F(n),
        CtKey::Menu => KeyCode::Menu,
        // Media keys, modifier-only presses, and everything else the library
        // has no variant for.
        //
        // **Declining here is not free.** A surface that owns the keyboard
        // still swallows them — `handle_key` asks `Ui::keyboard_owned` — so a
        // key with no variant is not "left on the old path" whenever a focus
        // layer is up; it is eaten, silently. `Menu` is here for that reason:
        // the dock's context menu answered `F2` (which has a variant, so it
        // reached the host's router) and not `Menu` (which did not).
        _ => return None,
    })
}

/// Translate a key press.
///
/// Release and repeat events are dropped: the library's model is a press, and
/// approximating the others would deliver phantom input.
pub fn key(press: &fresh_input_parser::KeyPress) -> Option<Input> {
    if press.kind == KeyEventKind::Release {
        return None;
    }
    let code = key_code(press.code)?;
    Some(Input::Key(KeyPress::with(code, mods(press.modifiers))))
}

fn button(b: CtButton) -> MouseButton {
    match b {
        CtButton::Left => MouseButton::Left,
        CtButton::Right => MouseButton::Right,
        CtButton::Middle => MouseButton::Middle,
    }
}

/// Translate a mouse event.
///
/// Every `MouseEventKind` has a counterpart, so this never declines today — the
/// match is deliberately exhaustive rather than ending in a catch-all, so a new
/// crossterm variant fails the build instead of being silently dropped. The
/// `Option` is kept for symmetry with [`key`], which does decline.
///
/// Drag is reported as a move: the library routes it by pointer capture, so the
/// node that took the press keeps receiving motion without the backend having
/// to distinguish the two. That is the whole drag mechanism, and it is what
/// replaces the `PointerGrab` flag ladder.
///
/// `clicks` is which press of a run this is — 1 for a single, 2 for a double, 3
/// for a triple. The editor already computes it, from its own configured
/// threshold and its own substitutable time source (`detect_multi_click`), and
/// the library deliberately has no clock: a double is a fact about time, and
/// the party that owns the input device owns it. So it is *reported* here, and
/// a handler reads it off `Event::clicks` rather than the applier consulting a
/// field snapshotted beside the dispatch.
/// `lines` and `columns` are what one notch is worth on each axis — the
/// caller's, because they are configuration (`mouse_wheel_scroll_lines`) and
/// this mapping has no editor to ask.
///
/// **A notch is not one line.** The walk this runs ahead of hands surfaces
/// `direction * mouse_wheel_scroll_lines` (and `direction * WHEEL_COLUMNS`
/// sideways); a tree that claimed the wheel with a delta of 1 scrolled a
/// migrated surface at a third of the speed of every surface below it, which
/// the file explorer's sticky parent row hid completely — offset 0 and offset
/// 1 render the same first filename.
pub fn mouse(
    m: crossterm::event::MouseEvent,
    clicks: u8,
    lines: i32,
    columns: i32,
) -> Option<Input> {
    let pos = Point::new(m.column as i32, m.row as i32);
    let mods = mods(m.modifiers);
    Some(match m.kind {
        MouseEventKind::Down(b) => Input::press_n(pos, button(b), mods, clicks),
        MouseEventKind::Up(b) => Input::release(pos, button(b), mods),
        MouseEventKind::Moved | MouseEventKind::Drag(_) => Input::Move { pos, mods },
        MouseEventKind::ScrollDown => Input::Wheel {
            pos,
            delta: lines,
            axis: Axis::Vertical,
            mods,
        },
        MouseEventKind::ScrollUp => Input::Wheel {
            pos,
            delta: -lines,
            axis: Axis::Vertical,
            mods,
        },
        MouseEventKind::ScrollRight => Input::Wheel {
            pos,
            delta: columns,
            axis: Axis::Horizontal,
            mods,
        },
        MouseEventKind::ScrollLeft => Input::Wheel {
            pos,
            delta: -columns,
            axis: Axis::Horizontal,
            mods,
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A single press, for the tests that do not care about runs.
    fn mouse_1(m: MouseEvent) -> Option<Input> {
        mouse(m, 1, 1, 1)
    }
    use crossterm::event::{KeyEvent, MouseEvent};

    fn press(code: CtKey, m: KeyModifiers) -> fresh_input_parser::KeyPress {
        fresh_input_parser::KeyPress::new(KeyEvent::new(code, m))
    }

    fn mouse_at(kind: MouseEventKind, col: u16, row: u16) -> MouseEvent {
        MouseEvent {
            kind,
            column: col,
            row,
            modifiers: KeyModifiers::NONE,
        }
    }

    #[test]
    fn a_chord_keeps_its_modifiers() {
        let got = key(&press(CtKey::Char('p'), KeyModifiers::CONTROL)).expect("translates");
        assert_eq!(
            got,
            Input::Key(KeyPress::with(
                KeyCode::Char('p'),
                Mods {
                    ctrl: true,
                    alt: false,
                    shift: false
                }
            ))
        );
    }

    /// The library's model is a press. Releases would arrive as phantom input.
    #[test]
    fn key_releases_are_dropped() {
        let mut p = press(CtKey::Char('a'), KeyModifiers::NONE);
        p.event.kind = KeyEventKind::Release;
        assert!(key(&p).is_none());
    }

    /// A key with no counterpart stays on the existing path rather than being
    /// approximated as something else.
    ///
    /// `Insert` used to be the example here and is a real editing key that a
    /// modal owning the keyboard has to be able to see, so it has a
    /// counterpart now. What is left without one is the class nobody binds.
    #[test]
    fn a_key_without_a_counterpart_declines() {
        assert!(key(&press(CtKey::CapsLock, KeyModifiers::NONE)).is_none());
        assert!(key_code(CtKey::Insert).is_some());
        assert!(key_code(CtKey::Char('x')).is_some());
    }

    #[test]
    fn buttons_and_positions_survive() {
        let got =
            mouse_1(mouse_at(MouseEventKind::Down(CtButton::Right), 7, 3)).expect("translates");
        match got {
            Input::Press { pos, button, .. } => {
                assert_eq!((pos.x, pos.y), (7, 3));
                assert_eq!(button, MouseButton::Right);
            }
            other => panic!("expected a press, got {other:?}"),
        }
    }

    /// Drag arrives as a move: the library routes it by pointer capture, so the
    /// node that took the press keeps receiving motion. This is what replaces
    /// the `PointerGrab` flag ladder.
    #[test]
    fn a_drag_is_a_move() {
        let drag =
            mouse_1(mouse_at(MouseEventKind::Drag(CtButton::Left), 4, 9)).expect("translates");
        let moved = mouse_1(mouse_at(MouseEventKind::Moved, 4, 9)).expect("translates");
        assert_eq!(drag, moved);
    }

    #[test]
    fn wheel_direction_is_down_positive() {
        let down = mouse_1(mouse_at(MouseEventKind::ScrollDown, 0, 0)).expect("translates");
        let up = mouse_1(mouse_at(MouseEventKind::ScrollUp, 0, 0)).expect("translates");
        match (down, up) {
            (Input::Wheel { delta: d, .. }, Input::Wheel { delta: u, .. }) => {
                assert_eq!((d, u), (1, -1));
            }
            other => panic!("expected wheels, got {other:?}"),
        }
    }

    /// Horizontal scroll crosses with its axis intact rather than being
    /// declined or, worse, reported as vertical. The axis exists because this
    /// adapter needed it — see the `fresh-ui` wheel-axis change.
    #[test]
    fn horizontal_wheel_keeps_its_axis() {
        let right = mouse_1(mouse_at(MouseEventKind::ScrollRight, 2, 2)).expect("translates");
        let left = mouse_1(mouse_at(MouseEventKind::ScrollLeft, 2, 2)).expect("translates");
        match (right, left) {
            (
                Input::Wheel {
                    delta: r, axis: ra, ..
                },
                Input::Wheel {
                    delta: l, axis: la, ..
                },
            ) => {
                assert_eq!((r, ra), (1, Axis::Horizontal));
                assert_eq!((l, la), (-1, Axis::Horizontal));
            }
            other => panic!("expected horizontal wheels, got {other:?}"),
        }
    }

    /// And vertical scroll still reports the vertical axis.
    #[test]
    fn vertical_wheel_reports_the_vertical_axis() {
        match mouse_1(mouse_at(MouseEventKind::ScrollDown, 0, 0)).expect("translates") {
            Input::Wheel { axis, .. } => assert_eq!(axis, Axis::Vertical),
            other => panic!("expected a wheel, got {other:?}"),
        }
    }
}
