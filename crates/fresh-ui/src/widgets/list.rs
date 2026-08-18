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

use crate::desc::{col, focusable, gesture, layout_reader, text, Handler, Node, Sizing};
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
    /// First visible index. Framework-adjacent but component-owned: the window
    /// is what this widget is for.
    pub scroll: usize,
    pub focused: bool,
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
}

impl<M: 'static> Component<M> for List<M> {
    type State = ListState;

    fn build(&self, s: &ListState, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let n = self.source.len();
        let sel = self.selected.unwrap_or(s.selected).min(n.saturating_sub(1));
        let up: Updater<ListState> = cx.updater();

        let source = self.source.clone();
        let scroll = s.scroll;
        let reader = layout_reader(move |c| {
            let visible = (c.max_h as usize).max(1);
            // Keep the selection inside the window without writing to state:
            // the first visible index is a function of the scroll and the
            // selection, computed here rather than stored.
            let first = scroll
                .min(sel)
                .max(sel.saturating_sub(visible.saturating_sub(1)))
                .min(n.saturating_sub(visible.min(n)));
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

        let select = |target: usize, this: &Self, up: &Updater<ListState>| -> Option<M> {
            let up = up.clone();
            up.set(move |st: &mut ListState| st.selected = target);
            this.on_select.as_ref().map(|f| f(target))
        };

        // Wheel: claim the notch only when the window actually moved, so a list
        // at its bound lets the wheel chain outward.
        let up_wheel = up.clone();
        let wheel: Handler<M> = Rc::new(move |e: &Event| {
            let next =
                (scroll as i64 + e.delta as i64).clamp(0, n.saturating_sub(1) as i64) as usize;
            if next != scroll {
                up_wheel.set(move |st: &mut ListState| st.scroll = next);
                e.stop();
            }
            None
        });

        let body = gesture(reader).on(GestureKind::Wheel, wheel);
        if !self.focusable {
            return body;
        }

        let up_focus = up.clone();
        let mut node = focusable(body)
            .on_focus_handler(Rc::new(move |e: &Event| {
                let gained = e.kind == GestureKind::FocusGained;
                up_focus.set(move |st: &mut ListState| st.focused = gained);
                None
            }))
            .action_handler(Intent::Up, {
                let (up, sel) = (up.clone(), sel);
                let this_select = self.on_select.clone();
                Rc::new(move |_: &Event| {
                    let target = sel.saturating_sub(1);
                    up.set(move |st: &mut ListState| {
                        st.selected = target;
                        st.scroll = st.scroll.min(target);
                    });
                    this_select.as_ref().map(|f| f(target))
                })
            })
            .action_handler(Intent::Down, {
                let (up, sel) = (up.clone(), sel);
                let this_select = self.on_select.clone();
                Rc::new(move |_: &Event| {
                    let target = (sel + 1).min(n.saturating_sub(1));
                    up.set(move |st: &mut ListState| st.selected = target);
                    this_select.as_ref().map(|f| f(target))
                })
            })
            .action_handler(Intent::Home, {
                let up = up.clone();
                let this_select = self.on_select.clone();
                Rc::new(move |_: &Event| {
                    up.set(|st: &mut ListState| {
                        st.selected = 0;
                        st.scroll = 0;
                    });
                    this_select.as_ref().map(|f| f(0))
                })
            })
            .action_handler(Intent::End, {
                let up = up.clone();
                let this_select = self.on_select.clone();
                Rc::new(move |_: &Event| {
                    let target = n.saturating_sub(1);
                    up.set(move |st: &mut ListState| st.selected = target);
                    this_select.as_ref().map(|f| f(target))
                })
            });

        let _ = select;
        if let Some(f) = self.on_activate.clone() {
            node = node.action_handler(Intent::Confirm, Rc::new(move |_: &Event| f(sel)));
        }
        node
    }

    fn describe_state(&self, s: &ListState) -> Option<String> {
        Some(format!("sel={} scroll={}", s.selected, s.scroll))
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
