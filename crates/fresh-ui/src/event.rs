//! Events, and the controls a handler has over them.
//!
//! One event type covers pointer, wheel and key delivery. A handler receives it
//! by shared reference and steers propagation through interior mutability, so
//! listeners stay `Fn` closures that can be stored in a description.

use std::cell::Cell;
use std::rc::Rc;

use crate::element::ElementId;
use crate::render::geom::Point;

/// Whether propagation continues past this listener.
///
/// There is no third disposition: acting without claiming is expressed by not
/// stopping.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Flow {
    #[default]
    Continue,
    Stop,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Button {
    #[default]
    Left,
    Right,
    Middle,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, Hash)]
pub struct Mods {
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
}

impl Mods {
    pub const NONE: Mods = Mods {
        ctrl: false,
        alt: false,
        shift: false,
    };
    pub const CTRL: Mods = Mods {
        ctrl: true,
        alt: false,
        shift: false,
    };
    pub const ALT: Mods = Mods {
        ctrl: false,
        alt: true,
        shift: false,
    };
    pub const SHIFT: Mods = Mods {
        ctrl: false,
        alt: false,
        shift: true,
    };
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum KeyCode {
    Char(char),
    Enter,
    Esc,
    Tab,
    BackTab,
    Backspace,
    Delete,
    Up,
    Down,
    Left,
    Right,
    Home,
    End,
    PageUp,
    PageDown,
    F(u8),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct KeyPress {
    pub code: KeyCode,
    pub mods: Mods,
}

impl KeyPress {
    pub const fn new(code: KeyCode) -> Self {
        KeyPress {
            code,
            mods: Mods::NONE,
        }
    }

    pub const fn with(code: KeyCode, mods: Mods) -> Self {
        KeyPress { code, mods }
    }

    pub const fn char(c: char) -> Self {
        KeyPress::new(KeyCode::Char(c))
    }
}

/// What happened. Gesture listeners are registered against these.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum GestureKind {
    Press,
    Release,
    Click,
    SecondaryClick,
    Move,
    Enter,
    Leave,
    Wheel,
    Key,
    FocusGained,
    FocusLost,
}

/// Where in the walk this delivery is.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Phase {
    Capture,
    Target,
    Bubble,
}

/// Raw input, as the host reports it. The library derives clicks, hover
/// transitions and capture routing from this.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Input {
    Press {
        pos: Point,
        button: Button,
        mods: Mods,
    },
    Release {
        pos: Point,
        button: Button,
        mods: Mods,
    },
    Move {
        pos: Point,
        mods: Mods,
    },
    Wheel {
        pos: Point,
        delta: i32,
        mods: Mods,
    },
    Key(KeyPress),
}

impl Input {
    pub fn position(&self) -> Option<Point> {
        match self {
            Input::Press { pos, .. }
            | Input::Release { pos, .. }
            | Input::Move { pos, .. }
            | Input::Wheel { pos, .. } => Some(*pos),
            Input::Key(_) => None,
        }
    }
}

#[derive(Default)]
pub(crate) struct Ctl {
    pub flow: Cell<Flow>,
    pub default_prevented: Cell<bool>,
    pub capture_request: Cell<Option<ElementId>>,
    pub release_request: Cell<bool>,
    pub focus_request: Cell<Option<(ElementId, SelectionOnFocus)>>,
}

/// What focusing a target should do to its selection. Clicking, tabbing to and
/// restoring a text field are different operations on the same field, and the
/// difference belongs in the focus request rather than in each call site.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum SelectionOnFocus {
    /// Place the caret at a position.
    Caret(usize),
    SelectAll,
    Preserve,
    /// The target is not editable.
    #[default]
    None,
}

/// One delivery of one event to one listener.
pub struct Event {
    pub kind: GestureKind,
    /// Absolute position. Meaningless for key events.
    pub pos: Point,
    /// Position relative to the listening element's rectangle.
    pub local: Point,
    pub button: Button,
    pub mods: Mods,
    /// Wheel notches; negative is up.
    pub delta: i32,
    pub key: Option<KeyPress>,
    /// The deepest element hit, as this listener sees it: rewritten to a
    /// component's root once propagation leaves that component, so composition
    /// structure does not leak through events.
    pub target: ElementId,
    /// The element whose listener is running.
    pub current: ElementId,
    pub phase: Phase,
    pub(crate) ctl: Rc<Ctl>,
}

impl Event {
    /// An event not produced by the dispatcher: for tests, and for a host that
    /// wants to invoke a handler it holds directly.
    pub fn synthetic(kind: GestureKind, at: ElementId) -> Event {
        Event {
            kind,
            pos: Point::ZERO,
            local: Point::ZERO,
            button: Button::Left,
            mods: Mods::NONE,
            delta: 0,
            key: None,
            target: at,
            current: at,
            phase: Phase::Target,
            ctl: Rc::new(Ctl::default()),
        }
    }

    /// Claim the event: propagation stops after this listener.
    pub fn stop(&self) {
        self.ctl.flow.set(Flow::Stop);
    }

    /// Suppress whatever the framework would otherwise do. Orthogonal to
    /// claiming.
    pub fn prevent_default(&self) {
        self.ctl.default_prevented.set(true);
    }

    pub fn default_prevented(&self) -> bool {
        self.ctl.default_prevented.get()
    }

    /// Route every subsequent move and release to this element, wherever the
    /// pointer goes, until it is released or the element unmounts. This is the
    /// whole drag mechanism.
    pub fn capture_pointer(&self) {
        self.ctl.capture_request.set(Some(self.current));
    }

    pub fn release_pointer(&self) {
        self.ctl.release_request.set(true);
    }

    /// Ask for focus to move to this element.
    pub fn request_focus(&self, sel: SelectionOnFocus) {
        self.ctl.focus_request.set(Some((self.current, sel)));
    }

    pub fn is_key(&self, code: KeyCode, mods: Mods) -> bool {
        matches!(self.key, Some(k) if k.code == code && k.mods == mods)
    }
}

impl std::fmt::Debug for Event {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Event")
            .field("kind", &self.kind)
            .field("pos", &self.pos)
            .field("target", &self.target)
            .field("current", &self.current)
            .field("phase", &self.phase)
            .finish()
    }
}
