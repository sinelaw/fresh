//! Text entry and numeric entry.

use std::rc::Rc;

use crate::desc::{focusable, gesture, row, text, Handler, Node, Sizing};
use crate::event::{Event, GestureKind, KeyCode, KeyPress, SelectionOnFocus};
use crate::focus::Intent;
use crate::schedule::{BuildCx, Updater};
use crate::Component;

/// The editing session around a value the owner holds.
///
/// The value itself is controlled — it is what the application stores, what
/// gets persisted, and what a command acts on. The caret is not: it exists only
/// while this field is on screen, so it lives here.
#[derive(Default)]
pub struct FieldState {
    pub caret: usize,
    pub focused: bool,
}

pub struct TextField<M> {
    value: Rc<str>,
    placeholder: Rc<str>,
    on_change: Option<Rc<dyn Fn(String) -> M>>,
    on_submit: Option<Handler<M>>,
}

impl<M: 'static> TextField<M> {
    pub fn new(value: impl AsRef<str>) -> Self {
        TextField {
            value: Rc::from(value.as_ref()),
            placeholder: Rc::from(""),
            on_change: None,
            on_submit: None,
        }
    }

    pub fn placeholder(mut self, p: impl AsRef<str>) -> Self {
        self.placeholder = Rc::from(p.as_ref());
        self
    }

    pub fn on_change(mut self, f: impl Fn(String) -> M + 'static) -> Self {
        self.on_change = Some(Rc::new(f));
        self
    }

    pub fn on_submit(mut self, f: impl Fn(&Event) -> M + 'static) -> Self {
        self.on_submit = Some(Rc::new(move |e| Some(f(e))));
        self
    }
}

impl<M: 'static> Component<M> for TextField<M> {
    type State = FieldState;

    fn build(&self, s: &FieldState, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let chars: Vec<char> = self.value.chars().collect();
        let caret = s.caret.min(chars.len());
        let up: Updater<FieldState> = cx.updater();

        let change = self.on_change.clone();
        let value = self.value.clone();
        let up_key = up.clone();
        let on_key: Handler<M> = Rc::new(move |e: &Event| {
            let KeyPress { code, mods } = e.key?;
            if mods.ctrl || mods.alt {
                return None;
            }
            let chars: Vec<char> = value.chars().collect();
            let caret = caret.min(chars.len());
            let edit = |next: String, pos: usize| {
                up_key.set(move |st: &mut FieldState| st.caret = pos);
                change.as_ref().map(|f| f(next))
            };
            match code {
                KeyCode::Char(c) => {
                    let mut next: String = chars[..caret].iter().collect();
                    next.push(c);
                    next.extend(chars[caret..].iter());
                    e.stop();
                    edit(next, caret + 1)
                }
                KeyCode::Backspace if caret > 0 => {
                    let mut next: String = chars[..caret - 1].iter().collect();
                    next.extend(chars[caret..].iter());
                    e.stop();
                    edit(next, caret - 1)
                }
                KeyCode::Delete if caret < chars.len() => {
                    let mut next: String = chars[..caret].iter().collect();
                    next.extend(chars[caret + 1..].iter());
                    e.stop();
                    edit(next, caret)
                }
                KeyCode::Left => {
                    e.stop();
                    up_key.set(move |st: &mut FieldState| st.caret = caret.saturating_sub(1));
                    None
                }
                KeyCode::Right => {
                    e.stop();
                    let n = (caret + 1).min(chars.len());
                    up_key.set(move |st: &mut FieldState| st.caret = n);
                    None
                }
                KeyCode::Home => {
                    e.stop();
                    up_key.set(|st: &mut FieldState| st.caret = 0);
                    None
                }
                KeyCode::End => {
                    e.stop();
                    let n = chars.len();
                    up_key.set(move |st: &mut FieldState| st.caret = n);
                    None
                }
                _ => None,
            }
        });

        // Focusing by click puts the caret where the click landed; focusing by
        // Tab selects the whole value; a restore leaves it alone.
        let up_focus = up.clone();
        let len = chars.len();
        let on_focus: Handler<M> = Rc::new(move |e: &Event| {
            let gained = e.kind == GestureKind::FocusGained;
            let sel = e.selection;
            up_focus.set(move |st: &mut FieldState| {
                st.focused = gained;
                if gained {
                    match sel {
                        SelectionOnFocus::Caret(p) => st.caret = p.min(len),
                        SelectionOnFocus::SelectAll => st.caret = len,
                        SelectionOnFocus::Preserve | SelectionOnFocus::None => {}
                    }
                }
            });
            None
        });

        let shown: String = if chars.is_empty() {
            self.placeholder.to_string()
        } else {
            self.value.to_string()
        };
        let theme = if s.focused { "field.focused" } else { "field" };

        let up_click = up;
        let mut f = focusable(gesture(row().theme(theme).child(text(shown))).on(
            GestureKind::Click,
            Rc::new(move |e: &Event| {
                let at = e.local.x.max(0) as usize;
                up_click.set(move |st: &mut FieldState| st.caret = at);
                e.request_focus(SelectionOnFocus::Caret(at));
                None
            }),
        ))
        .h(Sizing::Cells(1))
        .on_key_handler(on_key)
        .on_focus_handler(on_focus);

        if let Some(h) = self.on_submit.clone() {
            f = f.action_handler(Intent::Confirm, h);
        }
        f
    }

    fn describe_state(&self, s: &FieldState) -> Option<String> {
        Some(format!("caret={} focused={}", s.caret, s.focused))
    }
}

/// An integer field with clamped increment and decrement.
pub struct Number<M> {
    value: i64,
    min: i64,
    max: i64,
    step: i64,
    on_change: Option<Rc<dyn Fn(i64) -> M>>,
}

impl<M: 'static> Number<M> {
    pub fn new(value: i64) -> Self {
        Number {
            value,
            min: i64::MIN,
            max: i64::MAX,
            step: 1,
            on_change: None,
        }
    }

    pub fn range(mut self, min: i64, max: i64) -> Self {
        self.min = min;
        self.max = max;
        self
    }

    pub fn step(mut self, s: i64) -> Self {
        self.step = s.max(1);
        self
    }

    pub fn on_change(mut self, f: impl Fn(i64) -> M + 'static) -> Self {
        self.on_change = Some(Rc::new(f));
        self
    }
}

impl<M: 'static> Component<M> for Number<M> {
    type State = super::button::FocusMirror;

    fn build(&self, s: &super::button::FocusMirror, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let theme = if s.focused {
            "number.focused"
        } else {
            "number"
        };
        let bump = |delta: i64, this: &Self| -> Option<Handler<M>> {
            let f = this.on_change.clone()?;
            let next = (this.value + delta).clamp(this.min, this.max);
            Some(Rc::new(move |_: &Event| Some(f(next))))
        };
        let up = bump(self.step, self);
        let down = bump(-self.step, self);

        let mut node = focusable(
            row()
                .theme(theme)
                .children([text(format!("{}", self.value)), text(" ▲▼")]),
        )
        .h(Sizing::Cells(1))
        .on_focus_handler(super::button::mirror_focus_at(cx));
        if let Some(h) = up {
            node = node.action_handler(Intent::Up, h);
        }
        if let Some(h) = down {
            node = node.action_handler(Intent::Down, h);
        }
        node
    }
}
