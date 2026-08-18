//! `Dropdown` and `RadioGroup`.

use std::rc::Rc;

use crate::desc::{col, layer, Anchor, Dismiss, Fit, Modality, Node, Place, Sizing};
use crate::event::Event;
use crate::key::Key;
use crate::schedule::{BuildCx, Updater};
use crate::widgets::button::{Button, FocusMirror};
use crate::widgets::list::List;
use crate::{Component, ComponentExt};

#[derive(Default)]
pub struct DropdownState {
    /// Whether the menu is showing. Nothing outside this widget can see it, it
    /// does not survive a restart, and no command acts on it — so it lives on
    /// the element.
    pub open: bool,
}

/// A button that shows a menu in a layer while it is open.
pub struct Dropdown<M> {
    label: Rc<str>,
    items: Vec<(Key, Rc<str>)>,
    on_choose: Option<Rc<dyn Fn(Key) -> M>>,
}

impl<M: 'static> Dropdown<M> {
    pub fn new(label: impl AsRef<str>) -> Self {
        Dropdown {
            label: Rc::from(label.as_ref()),
            items: Vec::new(),
            on_choose: None,
        }
    }

    pub fn item(mut self, key: impl Into<Key>, label: impl AsRef<str>) -> Self {
        self.items.push((key.into(), Rc::from(label.as_ref())));
        self
    }

    pub fn on_choose(mut self, f: impl Fn(Key) -> M + 'static) -> Self {
        self.on_choose = Some(Rc::new(f));
        self
    }
}

impl<M: 'static> Component<M> for Dropdown<M> {
    type State = DropdownState;

    fn build(&self, s: &DropdownState, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let up: Updater<DropdownState> = cx.updater();
        let toggle = up.clone();
        let open = s.open;

        let trigger: Node<M> = Button::new(&*self.label)
            .on_press_handler(Rc::new(move |_: &Event| {
                toggle.set(move |st: &mut DropdownState| st.open = !open);
                None
            }))
            .node();

        if !s.open {
            return trigger;
        }

        let keys: Vec<Key> = self.items.iter().map(|(k, _)| k.clone()).collect();
        let choose = self.on_choose.clone();
        let close = up.clone();
        let menu = List::keyed(
            &self.items,
            |(k, _)| k.clone(),
            |(_, label)| crate::desc::text(&**label),
        )
        .on_activate_handler(Rc::new(move |i| {
            close.set(|st: &mut DropdownState| st.open = false);
            choose.as_ref().map(|f| f(keys[i].clone()))
        }))
        .node();

        let dismiss = up;
        col().child(trigger).child(
            layer()
                .anchor(Anchor::Parent)
                .place(Place::Below)
                .fit(Fit::FLIP.or(Fit::CLAMP))
                .modality(Modality::Inert)
                .dismiss(Dismiss::OUTSIDE_POINTER.or(Dismiss::ESCAPE))
                .on_dismiss_handler(Rc::new(move |_: &Event| {
                    dismiss.set(|st: &mut DropdownState| st.open = false);
                    None
                }))
                .child(col().border().child(menu)),
        )
    }

    fn describe_state(&self, s: &DropdownState) -> Option<String> {
        Some(format!("open={}", s.open))
    }
}

/// A set of mutually exclusive options. Controlled.
pub struct RadioGroup<M> {
    options: Vec<(Key, Rc<str>)>,
    selected: Option<Key>,
    on_change: Option<Rc<dyn Fn(Key) -> M>>,
}

impl<M: 'static> RadioGroup<M> {
    pub fn new() -> Self {
        RadioGroup {
            options: Vec::new(),
            selected: None,
            on_change: None,
        }
    }

    pub fn option(mut self, key: impl Into<Key>, label: impl AsRef<str>) -> Self {
        self.options.push((key.into(), Rc::from(label.as_ref())));
        self
    }

    pub fn selected(mut self, k: impl Into<Key>) -> Self {
        self.selected = Some(k.into());
        self
    }

    pub fn on_change(mut self, f: impl Fn(Key) -> M + 'static) -> Self {
        self.on_change = Some(Rc::new(f));
        self
    }
}

impl<M: 'static> Default for RadioGroup<M> {
    fn default() -> Self {
        RadioGroup::new()
    }
}

impl<M: 'static> Component<M> for RadioGroup<M> {
    type State = FocusMirror;

    fn build(&self, _s: &FocusMirror, _cx: &mut BuildCx<'_, M>) -> Node<M> {
        let sel = self.selected.clone();
        let keys: Vec<Key> = self.options.iter().map(|(k, _)| k.clone()).collect();
        let idx = sel
            .as_ref()
            .and_then(|k| keys.iter().position(|x| x == k))
            .unwrap_or(0);
        let change = self.on_change.clone();

        let mut list = List::keyed(
            &self.options,
            |(k, _)| k.clone(),
            move |(k, label)| {
                let mark = if sel.as_ref() == Some(k) {
                    "(o) "
                } else {
                    "( ) "
                };
                crate::desc::row()
                    .h(Sizing::Cells(1))
                    .children([crate::desc::text(mark), crate::desc::text(&**label)])
            },
        )
        .selected(idx);
        if let Some(f) = change {
            let keys2 = keys.clone();
            list = list.on_select(move |i| f(keys2[i].clone()));
        }
        list.node()
    }
}
