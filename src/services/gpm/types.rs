//! Rust types for GPM events, buttons, and modifiers

/// GPM event types (from gpm.h)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum GpmEventType {
    Move = 1,
    Drag = 2,
    Down = 4,
    Up = 8,
    Single = 16,
    Double = 32,
    Triple = 64,
    MFlag = 128,
    Hard = 256,
    Enter = 512,
    Leave = 1024,
}

/// GPM button flags (from gpm.h)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GpmButtons(pub u8);

impl GpmButtons {
    pub const NONE: u8 = 0;
    pub const RIGHT: u8 = 1;
    pub const MIDDLE: u8 = 2;
    pub const LEFT: u8 = 4;
    pub const FOURTH: u8 = 8;
    pub const UP: u8 = 16; // Scroll up
    pub const DOWN: u8 = 32; // Scroll down

    pub fn left(&self) -> bool {
        self.0 & Self::LEFT != 0
    }

    pub fn middle(&self) -> bool {
        self.0 & Self::MIDDLE != 0
    }

    pub fn right(&self) -> bool {
        self.0 & Self::RIGHT != 0
    }

    pub fn scroll_up(&self) -> bool {
        self.0 & Self::UP != 0
    }

    pub fn scroll_down(&self) -> bool {
        self.0 & Self::DOWN != 0
    }
}

/// GPM modifier flags
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GpmModifiers(pub u8);

impl GpmModifiers {
    pub fn shift(&self) -> bool {
        self.0 & 1 != 0
    }

    pub fn ctrl(&self) -> bool {
        self.0 & 4 != 0
    }

    pub fn alt(&self) -> bool {
        self.0 & 8 != 0
    }
}

/// A mouse event from GPM
#[derive(Debug, Clone)]
pub struct GpmEvent {
    pub buttons: GpmButtons,
    pub modifiers: GpmModifiers,
    pub x: i16,
    pub y: i16,
    pub dx: i16,
    pub dy: i16,
    pub event_type: u32,
    pub clicks: i32,
    pub wdx: i16, // Wheel delta x
    pub wdy: i16, // Wheel delta y
}

impl GpmEvent {
    /// Check if this is a move event
    pub fn is_move(&self) -> bool {
        self.event_type & GpmEventType::Move as u32 != 0
    }

    /// Check if this is a drag event
    pub fn is_drag(&self) -> bool {
        self.event_type & GpmEventType::Drag as u32 != 0
    }

    /// Check if this is a button down event
    pub fn is_down(&self) -> bool {
        self.event_type & GpmEventType::Down as u32 != 0
    }

    /// Check if this is a button up event
    pub fn is_up(&self) -> bool {
        self.event_type & GpmEventType::Up as u32 != 0
    }

    /// Check if this is a single click
    pub fn is_single_click(&self) -> bool {
        self.event_type & GpmEventType::Single as u32 != 0
    }

    /// Check if this is a double click
    pub fn is_double_click(&self) -> bool {
        self.event_type & GpmEventType::Double as u32 != 0
    }

    /// Check if this is a triple click
    pub fn is_triple_click(&self) -> bool {
        self.event_type & GpmEventType::Triple as u32 != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gpm_buttons() {
        let buttons = GpmButtons(GpmButtons::LEFT | GpmButtons::MIDDLE);
        assert!(buttons.left());
        assert!(buttons.middle());
        assert!(!buttons.right());
    }

    #[test]
    fn test_gpm_modifiers() {
        let mods = GpmModifiers(1 | 4); // Shift + Ctrl
        assert!(mods.shift());
        assert!(mods.ctrl());
        assert!(!mods.alt());
    }

    #[test]
    fn test_gpm_event_types() {
        let event = GpmEvent {
            buttons: GpmButtons(0),
            modifiers: GpmModifiers(0),
            x: 10,
            y: 20,
            dx: 0,
            dy: 0,
            event_type: GpmEventType::Down as u32 | GpmEventType::Single as u32,
            clicks: 1,
            wdx: 0,
            wdy: 0,
        };

        assert!(event.is_down());
        assert!(event.is_single_click());
        assert!(!event.is_up());
        assert!(!event.is_move());
    }
}
