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
    /// The row the pointer is over, if any. Mirrored from Enter/Leave the same
    /// way `focused` is mirrored from focus transitions; `build` reads it to
    /// tint that row.
    pub hovered: Option<usize>,
    /// A handle to the window, so a selection move can ask it to follow. The
    /// window itself belongs to the viewport.
    pub(crate) anchor: Option<Rc<crate::behavior::Anchor>>,
    /// The selection the window was last asked to show.
    pub(crate) revealed: crate::behavior::Cache<usize, ()>,
}

/// Which click in a run activates a row.
///
/// Not a detail of the widget: it is the difference between a menu and a file
/// list. A palette row commits on the first click, because selecting it *is*
/// choosing it; a file list selects on the first and opens on the second,
/// because the user may want to look at what they picked before committing to
/// it. Both were `on_activate` before, and the widget chose the first for
/// everyone.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Activate {
    /// The first click both selects and activates.
    #[default]
    Click,
    /// The second click activates; the first only selects.
    DoubleClick,
}

impl Activate {
    fn wants(self, clicks: u8) -> bool {
        match self {
            Activate::Click => true,
            Activate::DoubleClick => clicks >= 2,
        }
    }
}

/// A row's visual state, as the list knows it.
///
/// **The widget owns the state machine; the host owns the palette.** `List`
/// already tracks which row is selected, which the pointer is over, and whether
/// the list has focus — and two of those live in `ListState`, so a host cannot
/// compute them from the outside. What it *can* decide is what each state looks
/// like, which is why [`List::row_theme`] hands the state out rather than a
/// name.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RowState {
    Normal,
    /// The pointer is over this row.
    Hover,
    /// Selected, in a list that has focus.
    Selected,
    /// Selected, in a list that does not — so the eye can tell which list among
    /// several the keyboard is driving.
    SelectedBlur,
}

impl RowState {
    /// The default name for this state: the vocabulary a host gets when it does
    /// not name its own.
    pub fn theme(self) -> &'static str {
        match self {
            RowState::Normal => "list.row",
            RowState::Hover => "list.row.hover",
            RowState::Selected => "list.row.selected",
            RowState::SelectedBlur => "list.row.selected.blur",
        }
    }
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

/// Builds the click handler for a row, given its index. Factored out because
/// the nested closure type is otherwise unwieldy.
type RowClick<M> = Rc<dyn Fn(usize) -> crate::desc::Handler<M>>;

pub struct List<M> {
    source: Source<M>,
    selected: Option<usize>,
    on_select: Option<Rc<dyn Fn(usize) -> M>>,
    on_activate: Option<Rc<dyn Fn(usize) -> Option<M>>>,
    activate_on: Activate,
    focusable: bool,
    autofocus: bool,
    scrollbar: bool,
    stable_gutter: bool,
    bar_theme: Option<String>,
    #[allow(clippy::type_complexity)]
    row_theme: Option<Rc<dyn Fn(usize, RowState) -> String>>,
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
            activate_on: Activate::default(),
            focusable: true,
            autofocus: false,
            scrollbar: false,
            stable_gutter: false,
            bar_theme: None,
            row_theme: None,
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

    /// Which click activates a row. See [`Activate`]; the default is the first,
    /// which is what every caller got before this existed.
    pub fn activate_on(mut self, a: Activate) -> Self {
        self.activate_on = a;
        self
    }

    /// Whether the list joins the focus ring.
    ///
    /// A list that is driven from outside — its selection set by the caller
    /// each frame, its keys handled by whatever *does* hold the keyboard —
    /// should say no. Otherwise it is a stop on the way round, and Tab lands
    /// on a widget that has nothing to do with the key it was pressed for.
    /// The rows keep answering the mouse either way: this is about the
    /// keyboard, not about being inert.
    pub fn focusable(mut self, yes: bool) -> Self {
        self.focusable = yes;
        self
    }

    /// Take focus when this list first appears.
    pub fn autofocus(mut self) -> Self {
        self.autofocus = true;
        self
    }

    /// Name each row's appearance, given its index and the state the list has
    /// it in.
    ///
    /// Without this a row is stamped with [`RowState::theme`] — `list.row`,
    /// `list.row.selected` and the rest — which is a vocabulary the backend has
    /// to bind. A host that already has theme names for the surface it is
    /// migrating says them here instead, and the stamped name never appears.
    /// The name overwrites whatever the row builder set, so this is the only
    /// way for the caller's own name to survive.
    pub fn row_theme(mut self, f: impl Fn(usize, RowState) -> String + 'static) -> Self {
        self.row_theme = Some(Rc::new(f));
        self
    }

    /// Show how far through the list the window is.
    pub fn scrollbar(mut self) -> Self {
        self.scrollbar = true;
        self
    }

    /// The same, with the bar's column reserved whether the bar is there or
    /// not — see [`Node::scrollbar_gutter`](crate::Node::scrollbar_gutter).
    pub fn scrollbar_gutter(mut self) -> Self {
        self.scrollbar = true;
        self.stable_gutter = true;
        self
    }

    /// Name the bar's appearance — see
    /// [`Node::scrollbar_theme`](crate::Node::scrollbar_theme).
    pub fn scrollbar_theme(mut self, name: impl AsRef<str>) -> Self {
        self.bar_theme = Some(name.as_ref().to_string());
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

        // Keep the selection inside the window when it *moves* — by key here,
        // or by the owner passing a new one down. Asking on every build would
        // fight the wheel, which is a statement about the window rather than
        // about the selection; the memo is what distinguishes the two, and its
        // write is an idempotent function of the build inputs.
        if let Some(a) = &anchor {
            let a = a.clone();
            s.revealed.get_or(sel, move || a.reveal(sel as u32));
        }

        let source = self.source.clone();
        let hov = s.hovered;
        let focused = s.focused;
        let row_theme = self.row_theme.clone();
        // Clicking a row selects it, the same selection the keyboard drives.
        // Built here so it rides along with each visible row the reader emits.
        //
        // A list that is not focusable still has a mouse: declining the focus
        // ring says the *keyboard* is somewhere else, not that the rows stop
        // answering. The click asks for focus only where there is focus to
        // ask for.
        let click: Option<RowClick<M>> = {
            let up = up.clone();
            let anchor = anchor.clone();
            let on_select = self.on_select.clone();
            let on_activate = self.on_activate.clone();
            let activate_on = self.activate_on;
            let takes_focus = self.focusable;
            Some(Rc::new(move |i: usize| {
                let up = up.clone();
                let anchor = anchor.clone();
                let on_select = on_select.clone();
                let on_activate = on_activate.clone();
                let handler: crate::desc::Handler<M> = Rc::new(move |e: &Event| {
                    if takes_focus {
                        e.request_focus(crate::event::SelectionOnFocus::Preserve);
                    }
                    up.set(move |st: &mut ListState| st.selected = i);
                    let _ = &anchor;
                    // A click always moves the selection; whether it also
                    // activates is `activate_on`'s answer, read off the click
                    // run the host reported. A handler may return only one
                    // message, so activation wins when it fires and the
                    // selection is delivered through state either way.
                    let selected = on_select.as_ref().map(|f| f(i));
                    if !activate_on.wants(e.clicks) {
                        return selected;
                    }
                    match on_activate.as_ref().and_then(|f| f(i)) {
                        Some(m) => Some(m),
                        None => selected,
                    }
                });
                handler
            }))
        };

        // The row under the pointer tints itself. Enter and Leave are mirrored
        // into `hovered`, which the reader below reads back — the same shape as
        // the focus mirror, one row at a time.
        let hover: RowClick<M> = {
            let up = up.clone();
            Rc::new(move |i: usize| {
                let up = up.clone();
                let h: crate::desc::Handler<M> = Rc::new(move |e: &Event| {
                    let over = e.kind == GestureKind::Enter;
                    up.set(move |st: &mut ListState| {
                        if over {
                            st.hovered = Some(i);
                        } else if st.hovered == Some(i) {
                            st.hovered = None;
                        }
                    });
                    None
                });
                h
            })
        };

        // The window comes from the viewport, which owns it. This component
        // decides only which rows fill it.
        let reader = layout_reader(move |info| {
            let win = info.scroll_window.unwrap_or_default();
            let visible = (win.h as usize).max(1);
            let first = (win.y.max(0) as usize).min(n.saturating_sub(visible.min(n)));
            let last = (first + visible + OVERSCAN).min(n);
            col().children((first..last).map(|i| {
                let (k, row) = source.at(i).expect("index inside the source");
                let state = if i == sel {
                    // A selected row reads as focused only when the list has
                    // focus; otherwise it is muted.
                    if focused {
                        RowState::Selected
                    } else {
                        RowState::SelectedBlur
                    }
                } else if hov == Some(i) {
                    RowState::Hover
                } else {
                    RowState::Normal
                };
                let theme: String = match &row_theme {
                    Some(f) => f(i, state),
                    None => state.theme().to_string(),
                };
                let content = row.key(k).theme(theme).h(Sizing::Cells(1));
                let g = gesture(content).on_enter(hover(i)).on_leave(hover(i));
                match &click {
                    Some(mk) => g.on(GestureKind::Click, mk(i)),
                    None => g,
                }
            }))
        });

        let mut body = viewport(reader).items(n as u32);
        if let Some(a) = anchor.clone() {
            body = body.anchor_to(a);
        }
        if self.stable_gutter {
            body = body.scrollbar_gutter();
        } else if self.scrollbar {
            body = body.scrollbar();
        }
        if let Some(t) = &self.bar_theme {
            body = body.scrollbar_theme(t);
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
            let _ = anchor;
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
