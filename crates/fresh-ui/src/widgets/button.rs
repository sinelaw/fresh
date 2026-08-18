//! `Button` and `Toggle`.

use std::rc::Rc;

use crate::desc::{col, gesture, row, text, Handler, Node, Sizing};
use crate::event::{Event, GestureKind};
use crate::focus::Intent;
use crate::schedule::{BuildCx, Updater};
use crate::Component;

/// Whether this widget currently draws its focus indicator.
///
/// Focus itself is framework state; a widget that draws differently for it
/// mirrors the transitions here rather than owning the fact.
#[derive(Default)]
pub struct FocusMirror {
    pub focused: bool,
}

pub(crate) fn mirror_focus_at<M: 'static>(cx: &BuildCx<'_, M>) -> Handler<M> {
    let up: Updater<FocusMirror> = cx.updater();
    Rc::new(move |e: &Event| {
        let gained = e.kind == GestureKind::FocusGained;
        up.set(move |s: &mut FocusMirror| s.focused = gained);
        None
    })
}

/// A label that can be pressed, by pointer or by keyboard.
pub struct Button<M> {
    label: Rc<str>,
    on_press: Option<Handler<M>>,
    enabled: bool,
    autofocus: bool,
    theme: Rc<str>,
}

impl<M: 'static> Button<M> {
    pub fn new(label: impl AsRef<str>) -> Self {
        Button {
            label: Rc::from(label.as_ref()),
            on_press: None,
            enabled: true,
            autofocus: false,
            theme: Rc::from("button"),
        }
    }

    pub fn on_press(mut self, f: impl Fn(&Event) -> M + 'static) -> Self {
        self.on_press = Some(Rc::new(move |e| Some(f(e))));
        self
    }

    /// As `on_press`, for a press that may produce no message — a widget that
    /// only changes its own state, for instance.
    pub fn on_press_handler(mut self, h: Handler<M>) -> Self {
        self.on_press = Some(h);
        self
    }

    pub fn enabled(mut self, yes: bool) -> Self {
        self.enabled = yes;
        self
    }

    /// Take focus when this button first appears — the default action of a
    /// dialog, for instance.
    pub fn autofocus(mut self) -> Self {
        self.autofocus = true;
        self
    }

    pub fn theme(mut self, name: impl AsRef<str>) -> Self {
        self.theme = Rc::from(name.as_ref());
        self
    }
}

impl<M: 'static> Component<M> for Button<M> {
    type State = FocusMirror;

    fn build(&self, s: &FocusMirror, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let theme = if !self.enabled {
            format!("{}.disabled", self.theme)
        } else if s.focused {
            format!("{}.focused", self.theme)
        } else {
            self.theme.to_string()
        };
        let press = self.on_press.clone().filter(|_| self.enabled);
        let mut g = gesture(text(&*self.label));
        if let Some(h) = press.clone() {
            g = g.on(GestureKind::Click, h);
        }
        let mut f = crate::desc::focusable(col().pad(1, 0).theme(theme).child(g))
            .h(Sizing::Cells(1))
            .on_focus_handler(mirror_focus_at(cx));
        if let Some(h) = press {
            f = f.action_handler(Intent::Confirm, h);
        }
        if !self.enabled {
            f = f.skip_traversal();
        } else if self.autofocus {
            f = f.autofocus();
        }
        f
    }

    fn describe_state(&self, s: &FocusMirror) -> Option<String> {
        Some(format!("focused={}", s.focused))
    }
}

/// A labelled on/off control. Controlled: the owner holds the value.
pub struct Toggle<M> {
    label: Rc<str>,
    value: bool,
    on_change: Option<Rc<dyn Fn(bool) -> M>>,
}

impl<M: 'static> Toggle<M> {
    pub fn new(label: impl AsRef<str>, value: bool) -> Self {
        Toggle {
            label: Rc::from(label.as_ref()),
            value,
            on_change: None,
        }
    }

    pub fn on_change(mut self, f: impl Fn(bool) -> M + 'static) -> Self {
        self.on_change = Some(Rc::new(f));
        self
    }
}

impl<M: 'static> Component<M> for Toggle<M> {
    type State = FocusMirror;

    fn build(&self, s: &FocusMirror, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let mark = if self.value { "[x] " } else { "[ ] " };
        let theme = if s.focused {
            "toggle.focused"
        } else {
            "toggle"
        };
        let flip = self.on_change.clone().map(|f| {
            let next = !self.value;
            Rc::new(move |_: &Event| Some(f(next))) as Handler<M>
        });
        let mut g = gesture(
            row()
                .theme(theme)
                .children([text(mark), text(&*self.label)]),
        );
        if let Some(h) = flip.clone() {
            g = g.on(GestureKind::Click, h);
        }
        let mut f = crate::desc::focusable(g)
            .h(Sizing::Cells(1))
            .on_focus_handler(mirror_focus_at(cx));
        if let Some(h) = flip {
            f = f.action_handler(Intent::Confirm, h);
        }
        f
    }
}
