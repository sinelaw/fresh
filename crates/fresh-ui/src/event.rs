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
    /// The handler claimed the event: propagation ends, the tree resolves
    /// nothing further, and a host behind the tree does not act on it.
    Stop,
    /// **Observed, not claimed.** The handler acted, propagation ends and
    /// the tree resolves no intent from the key — but the key is still the
    /// host's, and is reported unclaimed so a pipeline behind the tree acts
    /// on it. The disposition a surface whose keys are bound *outside* the
    /// tree needs: without it, a subtree could only swallow every key
    /// (`Stop`) or let the tree's own traversal have it (`Continue`).
    Observe,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum MouseButton {
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
    Insert,
    Up,
    Down,
    Left,
    Right,
    Home,
    End,
    PageUp,
    PageDown,
    F(u8),
    /// The dedicated context-menu key (`Menu`, sometimes "Application"), and
    /// the reason it is here rather than in a host's private vocabulary:
    /// **containment applies to keys the tree cannot name.** A surface that
    /// owns the keyboard swallows anything the host declines to translate, so
    /// a key with no variant here is not "left on the old path" while a focus
    /// layer is up — it is eaten. The editor's dock lost its context menu to
    /// exactly that: `F2` had a variant and fell through to the host's router,
    /// `Menu` did not and died silently.
    Menu,
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
        button: MouseButton,
        mods: Mods,
        /// Which press in a run this is: `1` for a single, `2` for the second
        /// of a double, `3` for a triple, and so on.
        ///
        /// **Reported by the host, not derived here.** A double is a fact about
        /// *time* — two presses close enough together, near enough to each
        /// other — and the library has no clock and should not grow one. Hosts
        /// already have this: a terminal front end has its own configurable
        /// threshold, and its tests have a substitutable time source that a
        /// clock in here would defeat. So the host says how many, and the
        /// library carries it to the handler on [`Event::clicks`] and onto the
        /// `Click` it synthesises.
        ///
        /// [`Input::press`] sets it to 1, which is what a host that does not
        /// track runs should send.
        clicks: u8,
    },
    Release {
        pos: Point,
        button: MouseButton,
        mods: Mods,
    },
    Move {
        pos: Point,
        mods: Mods,
    },
    Wheel {
        pos: Point,
        delta: i32,
        /// Which way the notches run. Terminals report one axis per event and
        /// browsers report two deltas; both resolve to this.
        axis: Axis,
        mods: Mods,
    },
    Key(KeyPress),
}

impl Input {
    /// A single press — `clicks: 1`.
    pub fn press(pos: Point, button: MouseButton, mods: Mods) -> Input {
        Input::Press {
            pos,
            button,
            mods,
            clicks: 1,
        }
    }

    /// A press that is the `clicks`-th of a run.
    pub fn press_n(pos: Point, button: MouseButton, mods: Mods, clicks: u8) -> Input {
        Input::Press {
            pos,
            button,
            mods,
            clicks,
        }
    }

    /// A release.
    pub fn release(pos: Point, button: MouseButton, mods: Mods) -> Input {
        Input::Release { pos, button, mods }
    }
}

/// Which way a wheel turns, and which way an offset moves.
///
/// The scroll model has always been two-dimensional — a `Viewport`'s offset
/// and its maximum are both points — so this is the axis that was already
/// implied by the geometry, named so input can address it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum Axis {
    #[default]
    Vertical,
    Horizontal,
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
    pub button: MouseButton,
    pub mods: Mods,
    /// Wheel notches; negative is up (or left).
    pub delta: i32,
    /// Which axis `delta` moves along. `Vertical` for everything else.
    pub axis: Axis,
    /// The byte of the text under the pointer, in the *logical* string of the
    /// text run this event's target is — `None` when the target is not text.
    ///
    /// **The library is the only thing that can answer this.** The caller
    /// supplied the string; the library decided which of it is visible and
    /// where each grapheme landed. Reporting only a column asks the caller to
    /// redo that decision — which it can only do by laying the text out a
    /// second time, and which is wrong outright if it assumes one byte per
    /// cell. Both of those exist in this workspace's editor today, the second
    /// as a live defect on any non-ASCII label or value.
    ///
    /// Wrapped text answers as well. A row is not a slice of the logical
    /// string — the break ate a space, a hanging indent added some — so each
    /// row says which bytes it *is*, and the press resolves against the row it
    /// landed on rather than against a count of the rows above it. Whitespace
    /// a break dropped belongs to no row, so a press past the end of a wrapped
    /// row reports the byte just past the text that row shows, which is the
    /// caret position at the row's end.
    ///
    /// This is the same mapping, read backwards, that places
    /// [`TextProps::cursor`](crate::desc::TextProps::cursor) from a byte —
    /// deliberately, so a press and the caret it moves cannot disagree about
    /// where the wrap broke.
    pub text_byte: Option<usize>,
    pub key: Option<KeyPress>,
    /// Which press in a run produced this, as the host reported it: `1` for a
    /// single, `2` for a double, `3` for a triple. `1` for everything that is
    /// not a press or a click.
    ///
    /// A `Click` carries the count of the press it completes, so a listener can
    /// tell a double-click from a single without tracking presses itself.
    pub clicks: u8,
    /// On a focus event, what the acquisition asked the target to do with its
    /// selection.
    pub selection: SelectionOnFocus,
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
            text_byte: None,
            button: MouseButton::Left,
            mods: Mods::NONE,
            delta: 0,
            axis: Axis::Vertical,
            key: None,
            clicks: 1,
            selection: SelectionOnFocus::None,
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

    /// End propagation and the tree's own resolution of this key without
    /// claiming it — see [`Flow::Observe`]. A stop already recorded stands.
    pub fn observe(&self) {
        if self.ctl.flow.get() != Flow::Stop {
            self.ctl.flow.set(Flow::Observe);
        }
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
