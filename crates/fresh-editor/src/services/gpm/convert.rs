//! Conversion from GPM events to crossterm events

use super::types::GpmEvent;

/// Convert a GPM event to a crossterm MouseEvent for unified handling
///
/// Returns `None` if the event type is not recognized or cannot be converted.
pub fn gpm_to_crossterm(event: &GpmEvent) -> Option<crossterm::event::MouseEvent> {
    use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

    // Convert modifiers
    let mut modifiers = KeyModifiers::empty();
    if event.modifiers.shift() {
        modifiers |= KeyModifiers::SHIFT;
    }
    if event.modifiers.ctrl() {
        modifiers |= KeyModifiers::CONTROL;
    }
    if event.modifiers.alt() {
        modifiers |= KeyModifiers::ALT;
    }

    // Determine which button (if any) is involved
    let button = if event.buttons.left() {
        Some(MouseButton::Left)
    } else if event.buttons.right() {
        Some(MouseButton::Right)
    } else if event.buttons.middle() {
        Some(MouseButton::Middle)
    } else {
        None
    };

    // Convert event type to crossterm kind
    // Note: GPM reports wheel events as Move events with wdy set, so check wdy for scroll
    let kind = if event.is_down() {
        // Button press
        button.map(MouseEventKind::Down)?
    } else if event.is_up() {
        // Button release
        button.map(MouseEventKind::Up)?
    } else if event.is_drag() {
        // Dragging with button held
        button.map(MouseEventKind::Drag)?
    } else if event.buttons.scroll_up() || event.wdy > 0 {
        // Scroll up (button flag 16 or positive wdy)
        MouseEventKind::ScrollUp
    } else if event.buttons.scroll_down() || event.wdy < 0 {
        // Scroll down (button flag 32 or negative wdy)
        MouseEventKind::ScrollDown
    } else if event.is_move() {
        // Just movement, no button (wdy must be 0 to reach here)
        MouseEventKind::Moved
    } else {
        // Unknown event type
        return None;
    };

    Some(MouseEvent {
        kind,
        column: event.x.max(0) as u16,
        row: event.y.max(0) as u16,
        modifiers,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::gpm::types::{GpmButtons, GpmEventType, GpmModifiers};
    use crossterm::event::{MouseButton, MouseEventKind};

    #[test]
    fn test_gpm_to_crossterm_left_click() {
        let gpm_event = GpmEvent {
            buttons: GpmButtons(GpmButtons::LEFT),
            modifiers: GpmModifiers(0),
            x: 5,
            y: 10,
            dx: 0,
            dy: 0,
            event_type: GpmEventType::Down as u32,
            clicks: 1,
            wdx: 0,
            wdy: 0,
        };

        let crossterm_event = gpm_to_crossterm(&gpm_event).unwrap();
        assert!(matches!(
            crossterm_event.kind,
            MouseEventKind::Down(MouseButton::Left)
        ));
        assert_eq!(crossterm_event.column, 5);
        assert_eq!(crossterm_event.row, 10);
    }

    #[test]
    fn test_gpm_to_crossterm_right_click() {
        let gpm_event = GpmEvent {
            buttons: GpmButtons(GpmButtons::RIGHT),
            modifiers: GpmModifiers(0),
            x: 3,
            y: 7,
            dx: 0,
            dy: 0,
            event_type: GpmEventType::Down as u32,
            clicks: 1,
            wdx: 0,
            wdy: 0,
        };

        let crossterm_event = gpm_to_crossterm(&gpm_event).unwrap();
        assert!(matches!(
            crossterm_event.kind,
            MouseEventKind::Down(MouseButton::Right)
        ));
    }

    #[test]
    fn test_gpm_to_crossterm_with_modifiers() {
        use crossterm::event::KeyModifiers;

        let gpm_event = GpmEvent {
            buttons: GpmButtons(GpmButtons::LEFT),
            modifiers: GpmModifiers(1 | 4), // Shift + Ctrl
            x: 0,
            y: 0,
            dx: 0,
            dy: 0,
            event_type: GpmEventType::Down as u32,
            clicks: 1,
            wdx: 0,
            wdy: 0,
        };

        let crossterm_event = gpm_to_crossterm(&gpm_event).unwrap();
        assert!(crossterm_event.modifiers.contains(KeyModifiers::SHIFT));
        assert!(crossterm_event.modifiers.contains(KeyModifiers::CONTROL));
        assert!(!crossterm_event.modifiers.contains(KeyModifiers::ALT));
    }

    #[test]
    fn test_gpm_to_crossterm_move() {
        let gpm_event = GpmEvent {
            buttons: GpmButtons(0),
            modifiers: GpmModifiers(0),
            x: 20,
            y: 15,
            dx: 1,
            dy: 1,
            event_type: GpmEventType::Move as u32,
            clicks: 0,
            wdx: 0,
            wdy: 0,
        };

        let crossterm_event = gpm_to_crossterm(&gpm_event).unwrap();
        assert!(matches!(crossterm_event.kind, MouseEventKind::Moved));
        assert_eq!(crossterm_event.column, 20);
        assert_eq!(crossterm_event.row, 15);
    }

    #[test]
    fn test_gpm_to_crossterm_drag() {
        let gpm_event = GpmEvent {
            buttons: GpmButtons(GpmButtons::LEFT),
            modifiers: GpmModifiers(0),
            x: 10,
            y: 10,
            dx: 2,
            dy: 0,
            event_type: GpmEventType::Drag as u32,
            clicks: 0,
            wdx: 0,
            wdy: 0,
        };

        let crossterm_event = gpm_to_crossterm(&gpm_event).unwrap();
        assert!(matches!(
            crossterm_event.kind,
            MouseEventKind::Drag(MouseButton::Left)
        ));
    }

    #[test]
    fn test_gpm_to_crossterm_scroll_up() {
        let gpm_event = GpmEvent {
            buttons: GpmButtons(GpmButtons::UP),
            modifiers: GpmModifiers(0),
            x: 5,
            y: 5,
            dx: 0,
            dy: 0,
            event_type: 0,
            clicks: 0,
            wdx: 0,
            wdy: 1,
        };

        let crossterm_event = gpm_to_crossterm(&gpm_event).unwrap();
        assert!(matches!(crossterm_event.kind, MouseEventKind::ScrollUp));
    }

    #[test]
    fn test_gpm_to_crossterm_scroll_down() {
        let gpm_event = GpmEvent {
            buttons: GpmButtons(GpmButtons::DOWN),
            modifiers: GpmModifiers(0),
            x: 5,
            y: 5,
            dx: 0,
            dy: 0,
            event_type: 0,
            clicks: 0,
            wdx: 0,
            wdy: -1,
        };

        let crossterm_event = gpm_to_crossterm(&gpm_event).unwrap();
        assert!(matches!(crossterm_event.kind, MouseEventKind::ScrollDown));
    }

    #[test]
    fn test_gpm_to_crossterm_negative_coords() {
        // GPM might return negative coords at screen edges
        let gpm_event = GpmEvent {
            buttons: GpmButtons(GpmButtons::LEFT),
            modifiers: GpmModifiers(0),
            x: -1,
            y: -5,
            dx: 0,
            dy: 0,
            event_type: GpmEventType::Down as u32,
            clicks: 1,
            wdx: 0,
            wdy: 0,
        };

        let crossterm_event = gpm_to_crossterm(&gpm_event).unwrap();
        // Should clamp to 0
        assert_eq!(crossterm_event.column, 0);
        assert_eq!(crossterm_event.row, 0);
    }
}
