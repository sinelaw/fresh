//! Mouse events back into the reports a terminal sends.

/// Encode a mouse event as the SGR report (`CSI < Cb ; Cx ; Cy M`/`m`) a
/// terminal would have sent for it.
///
/// The inverse of the SGR reports [`InputParser`](crate::InputParser) reads.
/// A daemon client has no editor to hand a parsed event to: everything it
/// forwards is bytes the server parses. The console mouse arrives over GPM's
/// own fd rather than stdin (the Linux-only `gpm` module), so the client re-encodes
/// each report this way and sends it down the same stream as the keyboard
/// (#3517).
pub fn mouse_to_sgr(event: &crossterm::event::MouseEvent) -> Vec<u8> {
    use crossterm::event::{KeyModifiers, MouseButton, MouseEventKind};

    fn button(b: MouseButton) -> u8 {
        match b {
            MouseButton::Left => 0,
            MouseButton::Middle => 1,
            MouseButton::Right => 2,
        }
    }
    let (mut cb, release) = match event.kind {
        MouseEventKind::Down(b) => (button(b), false),
        MouseEventKind::Up(b) => (button(b), true),
        MouseEventKind::Drag(b) => (32 + button(b), false),
        MouseEventKind::Moved => (35, false),
        MouseEventKind::ScrollUp => (64, false),
        MouseEventKind::ScrollDown => (65, false),
        MouseEventKind::ScrollLeft => (66, false),
        MouseEventKind::ScrollRight => (67, false),
    };
    if event.modifiers.contains(KeyModifiers::SHIFT) {
        cb += 4;
    }
    if event.modifiers.contains(KeyModifiers::ALT) {
        cb += 8;
    }
    if event.modifiers.contains(KeyModifiers::CONTROL) {
        cb += 16;
    }
    format!(
        "\x1b[<{};{};{}{}",
        cb,
        u32::from(event.column) + 1,
        u32::from(event.row) + 1,
        if release { 'm' } else { 'M' }
    )
    .into_bytes()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{MouseButton, MouseEventKind};

    /// Every report the console mouse can produce comes back out of the
    /// parser as the same event.
    #[test]
    fn test_mouse_to_sgr_round_trips_through_the_server_parser() {
        use crossterm::event::{KeyModifiers, MouseEvent};

        let kinds = [
            MouseEventKind::Down(MouseButton::Left),
            MouseEventKind::Up(MouseButton::Left),
            MouseEventKind::Down(MouseButton::Right),
            MouseEventKind::Up(MouseButton::Middle),
            MouseEventKind::Drag(MouseButton::Left),
            MouseEventKind::Moved,
            MouseEventKind::ScrollUp,
            MouseEventKind::ScrollDown,
        ];
        let mods = [
            KeyModifiers::NONE,
            KeyModifiers::SHIFT | KeyModifiers::CONTROL,
            KeyModifiers::ALT,
        ];
        for kind in kinds {
            for modifiers in mods {
                let event = MouseEvent {
                    kind,
                    column: 41,
                    row: 7,
                    modifiers,
                };
                let parsed = crate::InputParser::new().parse(&mouse_to_sgr(&event));
                assert_eq!(
                    parsed,
                    vec![crate::Event::Mouse(event)],
                    "{kind:?} {modifiers:?}"
                );
            }
        }
    }
}
