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
    /// When set, the owner holds the open state — the controlled form. A menu
    /// an outside command can open (a mnemonic, a toolbar) needs its open state
    /// somewhere a command can reach, which is the owner, not this element.
    open: Option<bool>,
    on_toggle: Option<Rc<dyn Fn(bool) -> M>>,
}

impl<M: 'static> Dropdown<M> {
    pub fn new(label: impl AsRef<str>) -> Self {
        Dropdown {
            label: Rc::from(label.as_ref()),
            items: Vec::new(),
            on_choose: None,
            open: None,
            on_toggle: None,
        }
    }

    /// Hand the open state to the owner. Pair with `on_toggle`; omit both to let
    /// the dropdown keep its own.
    pub fn open(mut self, open: bool) -> Self {
        self.open = Some(open);
        self
    }

    /// Told when the trigger or a dismissal would change the open state, so the
    /// owner can record it.
    pub fn on_toggle(mut self, f: impl Fn(bool) -> M + 'static) -> Self {
        self.on_toggle = Some(Rc::new(f));
        self
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
        let controlled = self.on_toggle.is_some();
        // The effective open state, resolved once. Everything below reads this,
        // never `s.open` directly: the internal flag is a shadow that only the
        // uncontrolled form may consult, and mixing the two is the whole bug
        // class this widget invites.
        let open = self.open.unwrap_or(s.open);

        // The single writer of open state. When the owner controls the widget
        // it fires the owner's callback and touches nothing local; otherwise it
        // sets the element's own flag. Every open and close — trigger,
        // dismissal — goes through here, so the internal flag is never written
        // in the controlled form.
        let set_open: Rc<dyn Fn(bool) -> Option<M>> = {
            let up = up.clone();
            let on_toggle = self.on_toggle.clone();
            Rc::new(move |want: bool| match &on_toggle {
                Some(f) => Some(f(want)),
                None => {
                    up.set(move |st: &mut DropdownState| st.open = want);
                    None
                }
            })
        };

        let trigger: Node<M> = Button::new(&*self.label)
            .on_press_handler({
                let set_open = set_open.clone();
                Rc::new(move |_: &Event| set_open(!open))
            })
            .node();

        if !open {
            return trigger;
        }

        let keys: Vec<Key> = self.items.iter().map(|(k, _)| k.clone()).collect();
        let choose = self.on_choose.clone();
        let menu = List::keyed(
            &self.items,
            |(k, _)| k.clone(),
            |(_, label)| crate::desc::text(&**label),
        )
        .on_activate_handler({
            let up = up.clone();
            Rc::new(move |i, _: &crate::Event| {
                // Choosing returns the choice; a handler yields one message, so
                // closing is the owner's job in the controlled form (its choice
                // handler clears the open state) and a local flip otherwise.
                if !controlled {
                    up.set(|st: &mut DropdownState| st.open = false);
                }
                choose.as_ref().map(|f| f(keys[i].clone()))
            })
        })
        .node();

        col().child(trigger).child(
            layer()
                .anchor(Anchor::Parent)
                .place(Place::Below)
                .fit(Fit::FLIP.or(Fit::CLAMP))
                .modality(Modality::Inert)
                .dismiss(Dismiss::OUTSIDE_POINTER.or(Dismiss::ESCAPE))
                .on_dismiss_handler({
                    let set_open = set_open.clone();
                    Rc::new(move |_: &Event| set_open(false))
                })
                // Named, so the backend paints a background under it: without
                // one the menu is transparent and the content behind shows
                // through the gaps between its labels.
                .child(col().border().theme("menu").child(menu)),
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
