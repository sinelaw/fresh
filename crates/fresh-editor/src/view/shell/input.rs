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
        CtKey::Up => KeyCode::Up,
        CtKey::Down => KeyCode::Down,
        CtKey::Left => KeyCode::Left,
        CtKey::Right => KeyCode::Right,
        CtKey::Home => KeyCode::Home,
        CtKey::End => KeyCode::End,
        CtKey::PageUp => KeyCode::PageUp,
        CtKey::PageDown => KeyCode::PageDown,
        CtKey::F(n) => KeyCode::F(n),
        // Insert, media keys, modifier-only presses, and everything the
        // library has no variant for.
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
pub fn mouse(m: crossterm::event::MouseEvent) -> Option<Input> {
    let pos = Point::new(m.column as i32, m.row as i32);
    let mods = mods(m.modifiers);
    Some(match m.kind {
        MouseEventKind::Down(b) => Input::Press {
            pos,
            button: button(b),
            mods,
        },
        MouseEventKind::Up(b) => Input::Release {
            pos,
            button: button(b),
            mods,
        },
        MouseEventKind::Moved | MouseEventKind::Drag(_) => Input::Move { pos, mods },
        MouseEventKind::ScrollDown => Input::Wheel {
            pos,
            delta: 1,
            axis: Axis::Vertical,
            mods,
        },
        MouseEventKind::ScrollUp => Input::Wheel {
            pos,
            delta: -1,
            axis: Axis::Vertical,
            mods,
        },
        MouseEventKind::ScrollRight => Input::Wheel {
            pos,
            delta: 1,
            axis: Axis::Horizontal,
            mods,
        },
        MouseEventKind::ScrollLeft => Input::Wheel {
            pos,
            delta: -1,
            axis: Axis::Horizontal,
            mods,
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;
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
    #[test]
    fn a_key_without_a_counterpart_declines() {
        assert!(key(&press(CtKey::Insert, KeyModifiers::NONE)).is_none());
        assert!(key_code(CtKey::Char('x')).is_some());
    }

    #[test]
    fn buttons_and_positions_survive() {
        let got = mouse(mouse_at(MouseEventKind::Down(CtButton::Right), 7, 3)).expect("translates");
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
        let drag = mouse(mouse_at(MouseEventKind::Drag(CtButton::Left), 4, 9)).expect("translates");
        let moved = mouse(mouse_at(MouseEventKind::Moved, 4, 9)).expect("translates");
        assert_eq!(drag, moved);
    }

    #[test]
    fn wheel_direction_is_down_positive() {
        let down = mouse(mouse_at(MouseEventKind::ScrollDown, 0, 0)).expect("translates");
        let up = mouse(mouse_at(MouseEventKind::ScrollUp, 0, 0)).expect("translates");
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
        let right = mouse(mouse_at(MouseEventKind::ScrollRight, 2, 2)).expect("translates");
        let left = mouse(mouse_at(MouseEventKind::ScrollLeft, 2, 2)).expect("translates");
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
        match mouse(mouse_at(MouseEventKind::ScrollDown, 0, 0)).expect("translates") {
            Input::Wheel { axis, .. } => assert_eq!(axis, Axis::Vertical),
            other => panic!("expected a wheel, got {other:?}"),
        }
    }
}
