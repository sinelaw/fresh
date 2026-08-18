//! `List` and `Tree`.
//!
//! Both forms of `List` — the eager one that takes the rows, and the windowed
//! one that takes a count and a builder — go through the same `LayoutReader`,
//! so both do O(visible) work per frame and both behave identically to
//! keyboard, wheel and selection. The windowed form is what makes a million-row
//! list ordinary: off-screen rows have no descriptions, no elements and no
//! state.

use std::collections::HashSet;
use std::rc::Rc;

use crate::desc::{col, focusable, gesture, layout_reader, text, viewport, Node, Sizing};
use crate::event::{Event, GestureKind};
use crate::focus::Intent;
use crate::key::Key;
use crate::schedule::{BuildCx, Updater};
use crate::{Component, ComponentExt};

/// Rows above and below the window, so a one-cell scroll does not expose a gap.
const OVERSCAN: usize = 2;

#[derive(Default)]
pub struct ListState {
    /// Only consulted when the owner did not supply a selection.
    pub selected: usize,
    pub focused: bool,
    /// A handle to the window, so a selection move can ask it to follow. The
    /// window itself belongs to the viewport.
    pub(crate) anchor: Option<Rc<crate::behavior::Anchor>>,
}

enum Source<M> {
    Eager(Rc<Vec<(Key, Node<M>)>>),
    #[allow(clippy::type_complexity)]
    Windowed {
        count: usize,
        key: Rc<dyn Fn(usize) -> Key>,
        row: Rc<dyn Fn(usize) -> Node<M>>,
    },
}

impl<M> Clone for Source<M> {
    fn clone(&self) -> Self {
        match self {
            Source::Eager(v) => Source::Eager(v.clone()),
            Source::Windowed { count, key, row } => Source::Windowed {
                count: *count,
                key: key.clone(),
                row: row.clone(),
            },
        }
    }
}

impl<M> Source<M> {
    fn len(&self) -> usize {
        match self {
            Source::Eager(v) => v.len(),
            Source::Windowed { count, .. } => *count,
        }
    }

    fn at(&self, i: usize) -> Option<(Key, Node<M>)> {
        match self {
            Source::Eager(v) => v.get(i).cloned(),
            Source::Windowed { count, key, row } => (i < *count).then(|| (key(i), row(i))),
        }
    }
}

pub struct List<M> {
    source: Source<M>,
    selected: Option<usize>,
    on_select: Option<Rc<dyn Fn(usize) -> M>>,
    on_activate: Option<Rc<dyn Fn(usize) -> Option<M>>>,
    focusable: bool,
    autofocus: bool,
    scrollbar: bool,
}

impl<M: 'static> List<M> {
    /// Every row built up front. The key function is required: no algorithm can
    /// tell "the same item at a new position" from "a different item at the same
    /// position", because that distinction exists only in the domain model.
    pub fn keyed<T>(items: &[T], key: impl Fn(&T) -> Key, row: impl Fn(&T) -> Node<M>) -> Self {
        List::from_source(Source::Eager(Rc::new(
            items.iter().map(|t| (key(t), row(t))).collect(),
        )))
    }

    /// Rows built only for the visible index range. The builder receives an
    /// index; the application resolves it against its own storage, so the
    /// library never holds the collection.
    ///
    /// Named `windowed` rather than the design document's `virtual`, which is a
    /// reserved word in Rust.
    pub fn windowed(
        count: usize,
        key: impl Fn(usize) -> Key + 'static,
        row: impl Fn(usize) -> Node<M> + 'static,
    ) -> Self {
        List::from_source(Source::Windowed {
            count,
            key: Rc::new(key),
            row: Rc::new(row),
        })
    }

    fn from_source(source: Source<M>) -> Self {
        List {
            source,
            selected: None,
            on_select: None,
            on_activate: None,
            focusable: true,
            autofocus: false,
            scrollbar: false,
        }
    }

    /// Controlled selection: the owner holds it and is told when it should
    /// change. Omit this and the element keeps its own.
    pub fn selected(mut self, i: usize) -> Self {
        self.selected = Some(i);
        self
    }

    pub fn on_select(mut self, f: impl Fn(usize) -> M + 'static) -> Self {
        self.on_select = Some(Rc::new(f));
        self
    }

    pub fn on_activate(mut self, f: impl Fn(usize) -> M + 'static) -> Self {
        self.on_activate = Some(Rc::new(move |i| Some(f(i))));
        self
    }

    /// As `on_activate`, for an activation that may produce no message.
    pub fn on_activate_handler(mut self, f: Rc<dyn Fn(usize) -> Option<M>>) -> Self {
        self.on_activate = Some(f);
        self
    }

    pub fn focusable(mut self, yes: bool) -> Self {
        self.focusable = yes;
        self
    }

    /// Take focus when this list first appears.
    pub fn autofocus(mut self) -> Self {
        self.autofocus = true;
        self
    }

    /// Show how far through the list the window is.
    pub fn scrollbar(mut self) -> Self {
        self.scrollbar = true;
        self
    }
}

impl<M: 'static> Component<M> for List<M> {
    type State = ListState;

    fn init(&self, cx: &mut crate::schedule::InitCx<'_, M>) -> ListState {
        // A handle to the viewport this list is about to build, so a selection
        // move can ask for the window rather than owning it.
        ListState {
            anchor: Some(cx.register(crate::behavior::Anchor::default())),
            ..ListState::default()
        }
    }

    fn build(&self, s: &ListState, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let n = self.source.len();
        let sel = self.selected.unwrap_or(s.selected).min(n.saturating_sub(1));
        let up: Updater<ListState> = cx.updater();
        let anchor = s.anchor.clone();

        let source = self.source.clone();
        // The window comes from the viewport, which owns it. This component
        // decides only which rows fill it.
        let reader = layout_reader(move |info| {
            let win = info.scroll_window.unwrap_or_default();
            let visible = (win.h as usize).max(1);
            let first = (win.y.max(0) as usize).min(n.saturating_sub(visible.min(n)));
            let last = (first + visible + OVERSCAN).min(n);
            col().children((first..last).map(|i| {
                let (k, row) = source.at(i).expect("index inside the source");
                let theme = if i == sel {
                    "list.row.selected"
                } else {
                    "list.row"
                };
                row.key(k).theme(theme).h(Sizing::Cells(1))
            }))
        });

        let mut body = viewport(reader).items(n as u32);
        if let Some(a) = anchor.clone() {
            body = body.anchor_to(a);
        }
        if self.scrollbar {
            body = body.scrollbar();
        }

        if !self.focusable {
            return body;
        }

        // Moving the selection asks the window to follow; scrolling does not
        // move the selection. A wheel is a statement about the window and a key
        // is a statement about the selection, and the two now live in different
        // places.
        let select = |target: usize,
                      up: &Updater<ListState>,
                      anchor: &Option<Rc<crate::behavior::Anchor>>,
                      on_select: &Option<Rc<dyn Fn(usize) -> M>>|
         -> Option<M> {
            let up = up.clone();
            up.set(move |st: &mut ListState| st.selected = target);
            if let Some(a) = anchor {
                a.reveal(target as u32);
            }
            on_select.as_ref().map(|f| f(target))
        };

        let up_focus = up.clone();
        let mut node = focusable(body)
            .on_focus_handler(Rc::new(move |e: &Event| {
                let gained = e.kind == GestureKind::FocusGained;
                up_focus.set(move |st: &mut ListState| st.focused = gained);
                None
            }))
            .action_handler(Intent::Up, {
                let (up, a, f) = (up.clone(), anchor.clone(), self.on_select.clone());
                Rc::new(move |_: &Event| select(sel.saturating_sub(1), &up, &a, &f))
            })
            .action_handler(Intent::Down, {
                let (up, a, f) = (up.clone(), anchor.clone(), self.on_select.clone());
                Rc::new(move |_: &Event| select((sel + 1).min(n.saturating_sub(1)), &up, &a, &f))
            })
            .action_handler(Intent::Home, {
                let (up, a, f) = (up.clone(), anchor.clone(), self.on_select.clone());
                Rc::new(move |_: &Event| select(0, &up, &a, &f))
            })
            .action_handler(Intent::End, {
                let (up, a, f) = (up.clone(), anchor.clone(), self.on_select.clone());
                Rc::new(move |_: &Event| select(n.saturating_sub(1), &up, &a, &f))
            });

        if let Some(f) = self.on_activate.clone() {
            node = node.action_handler(Intent::Confirm, Rc::new(move |_: &Event| f(sel)));
        }
        if self.autofocus {
            node = node.autofocus();
        }
        node
    }

    fn describe_state(&self, s: &ListState) -> Option<String> {
        Some(format!("sel={}", s.selected))
    }
}

// -- Tree --------------------------------------------------------------------

/// One node of a tree. The application owns the shape; the widget owns which
/// parts of it are open.
pub struct TreeNode<M> {
    pub key: Key,
    pub label: Node<M>,
    pub children: Vec<TreeNode<M>>,
}

impl<M> TreeNode<M> {
    pub fn leaf(key: impl Into<Key>, label: Node<M>) -> Self {
        TreeNode {
            key: key.into(),
            label,
            children: Vec::new(),
        }
    }

    pub fn branch(key: impl Into<Key>, label: Node<M>, children: Vec<TreeNode<M>>) -> Self {
        TreeNode {
            key: key.into(),
            label,
            children,
        }
    }
}

#[derive(Default)]
pub struct TreeState {
    pub expanded: HashSet<Key>,
}

pub struct Tree<M> {
    roots: Vec<TreeNode<M>>,
    on_activate: Option<Rc<dyn Fn(Key) -> M>>,
}

impl<M: 'static> Tree<M> {
    pub fn new(roots: Vec<TreeNode<M>>) -> Self {
        Tree {
            roots,
            on_activate: None,
        }
    }

    pub fn on_activate(mut self, f: impl Fn(Key) -> M + 'static) -> Self {
        self.on_activate = Some(Rc::new(f));
        self
    }
}

impl<M: 'static> Component<M> for Tree<M> {
    type State = TreeState;

    fn build(&self, s: &TreeState, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let mut rows: Vec<(Key, Node<M>)> = Vec::new();
        flatten(&self.roots, 0, &s.expanded, &mut rows);

        let up: Updater<TreeState> = cx.updater();
        let keys: Vec<Key> = rows.iter().map(|(k, _)| k.clone()).collect();
        let activate = self.on_activate.clone();

        let rows: Vec<(Key, Node<M>)> = rows
            .into_iter()
            .map(|(k, node)| {
                let up = up.clone();
                let kk = k.clone();
                let node = gesture(node).on(
                    GestureKind::Click,
                    Rc::new(move |_: &Event| {
                        let k = kk.clone();
                        up.set(move |st: &mut TreeState| {
                            if !st.expanded.remove(&k) {
                                st.expanded.insert(k);
                            }
                        });
                        None
                    }),
                );
                (k, node)
            })
            .collect();

        let mut list = List::from_source(Source::Eager(Rc::new(rows)));
        if let Some(f) = activate {
            list = list.on_activate_handler(Rc::new(move |i| Some(f(keys[i].clone()))));
        }
        list.node()
    }
}

fn flatten<M>(
    nodes: &[TreeNode<M>],
    depth: usize,
    expanded: &HashSet<Key>,
    out: &mut Vec<(Key, Node<M>)>,
) {
    for n in nodes {
        let open = expanded.contains(&n.key);
        let mark = if n.children.is_empty() {
            "  "
        } else if open {
            "v "
        } else {
            "> "
        };
        let row =
            crate::desc::row().children([text(" ".repeat(depth * 2)), text(mark), n.label.clone()]);
        out.push((n.key.clone(), row));
        if open {
            flatten(&n.children, depth + 1, expanded, out);
        }
    }
}

// -- DualList ----------------------------------------------------------------

/// Two lists and the moves between them: available on the left, chosen on the
/// right. Controlled — the owner holds both sides and is told what moved.
pub struct DualList<M> {
    available: Vec<(Key, Rc<str>)>,
    chosen: Vec<(Key, Rc<str>)>,
    on_move: Option<Rc<dyn Fn(Key, bool) -> M>>,
}

impl<M: 'static> Default for DualList<M> {
    fn default() -> Self {
        DualList::new()
    }
}

impl<M: 'static> DualList<M> {
    pub fn new() -> Self {
        DualList {
            available: Vec::new(),
            chosen: Vec::new(),
            on_move: None,
        }
    }

    pub fn available(mut self, items: impl IntoIterator<Item = (Key, String)>) -> Self {
        self.available = items
            .into_iter()
            .map(|(k, s)| (k, Rc::from(s.as_str())))
            .collect();
        self
    }

    pub fn chosen(mut self, items: impl IntoIterator<Item = (Key, String)>) -> Self {
        self.chosen = items
            .into_iter()
            .map(|(k, s)| (k, Rc::from(s.as_str())))
            .collect();
        self
    }

    /// Called with the item and `true` when it moves into the chosen side.
    pub fn on_move(mut self, f: impl Fn(Key, bool) -> M + 'static) -> Self {
        self.on_move = Some(Rc::new(f));
        self
    }
}

impl<M: 'static> Component<M> for DualList<M> {
    type State = ();

    fn build(&self, _s: &(), _cx: &mut BuildCx<'_, M>) -> Node<M> {
        let side = |items: &[(Key, Rc<str>)],
                    into_chosen: bool,
                    f: &Option<Rc<dyn Fn(Key, bool) -> M>>| {
            let keys: Vec<Key> = items.iter().map(|(k, _)| k.clone()).collect();
            let mut l = List::keyed(items, |(k, _)| k.clone(), |(_, s)| text(&**s));
            if let Some(f) = f.clone() {
                l = l.on_activate(move |i| f(keys[i].clone(), into_chosen));
            }
            l.node()
        };
        crate::desc::row().gap(1).children([
            side(&self.available, true, &self.on_move),
            side(&self.chosen, false, &self.on_move),
        ])
    }
}
