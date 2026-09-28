//! `List` and `Tree`.
//!
//! Both forms of `List` — the eager one that takes the rows, and the windowed
//! one that takes a count and a builder — go through the same `LayoutReader`,
//! so both do O(visible) work per frame and both behave identically to
//! keyboard, wheel and selection. The windowed form is what makes a million-row
//! list ordinary: off-screen rows have no descriptions, no elements and no
//! state.
//!
//! That last sentence is [`RowHeight::Cells`]'s, and it is why that variant is
//! the default. [`RowHeight::UniformMeasured`] gives up the first half of it —
//! every item is described, because "the tallest" is not a question the visible
//! ones can answer — and keeps the second: nothing off screen is mounted
//! between the measurement and the frame, and nothing at all is described to
//! *scroll*.

use std::rc::Rc;

use crate::desc::{
    col, focusable, gesture, layout_reader, stack, text, viewport, Align, Node, Sizing,
};
use crate::event::{Event, GestureKind};
use crate::focus::Intent;
use crate::key::Key;
use crate::render::object::Band;
use crate::schedule::{BuildCx, Updater};
use crate::{Component, ComponentExt};

/// Where each of `n` rows starts, in cells, and (last) where the column ends.
fn prefix(n: usize, height: &dyn Fn(usize) -> u16) -> Vec<u32> {
    let mut out = Vec::with_capacity(n + 1);
    let mut at = 0u32;
    out.push(0);
    for i in 0..n {
        at += height(i).max(1) as u32;
        out.push(at);
    }
    out
}

/// Rows above and below the window, so a one-cell scroll does not expose a gap.
const OVERSCAN: usize = 2;

/// A selection the window was asked to show: its index and its row's key.
///
/// **The same selection if either is.** Rows arriving above the selected
/// one move its index but not its key; an owner re-keying its rows under an
/// unchanged selection changes the key but not the index. Neither is the
/// reader's selection moving, and revealing again would pull back a window
/// the wheel had moved. A selection that moved changes both.
#[derive(Clone)]
pub(crate) struct Revealed(usize, Option<Key>);

impl PartialEq for Revealed {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 || (self.1.is_some() && self.1 == other.1)
    }
}

#[derive(Default)]
pub struct ListState {
    /// Only consulted when the owner did not supply a selection: where the
    /// element's own selection was last seen, as a hint. The row it names is
    /// [`Self::selected_key`].
    pub selected: usize,
    /// The row the element's own selection is on, by key. **The key is the
    /// selection; the index is where it was.** A row inserted above the
    /// selected one moves it down, and a selection held by index would stay
    /// put and land on the newcomer.
    pub selected_key: Option<Key>,
    pub focused: bool,
    /// The row the pointer is over, by key. Mirrored from Enter/Leave the same
    /// way `focused` is mirrored from focus transitions; `build` reads it to
    /// tint that row. By key for the reason `selected_key` is: the hover
    /// belongs to the row under the pointer, wherever an insertion moves it.
    pub hovered: Option<Key>,
    /// A handle to the window, so a selection move can ask it to follow. The
    /// window itself belongs to the viewport.
    pub(crate) anchor: Option<Rc<crate::behavior::Anchor>>,
    /// The selection the window was last asked to show. See [`Revealed`].
    pub(crate) revealed: crate::behavior::Cache<Revealed, ()>,
    /// What a standing follow was last armed for — the selected row, by its
    /// key, and the owner's token (see [`List::follow_selection`]); the
    /// token alone under [`List::follow_on`]. The value is the row it was
    /// armed on.
    pub(crate) followed: crate::behavior::Cache<(Option<Key>, u64), Option<Key>>,
    /// Which pager the list records its window into, and the one it uses
    /// for its own PageUp/PageDown when the owner passed none.
    pub(crate) pager: Option<Rc<crate::behavior::pager::PagerSlot>>,
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

/// How tall one row of a windowed list is.
///
/// **Uniform either way — the question is who counts.** A window is
/// addressable by index because every row is the same height: row `i` starts at
/// cell `i * height`, so a scroll is arithmetic and nothing is measured to
/// serve it. Both variants keep that. What they differ on is whether the number
/// can be stated before the rows exist.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RowHeight {
    /// The caller states it. No item is ever measured and no off-screen item is
    /// ever described: this is the million-row case, and the default is
    /// `Cells(1)` — a list of lines.
    Cells(u16),
    /// Uniform, but *measured*: every item is laid out once at the current
    /// width and the tallest sets the band; shorter items pad to it.
    ///
    /// For a card list, where each item is a small block rather than a line and
    /// the height is a function of the width — so no number exists until layout
    /// runs, and a caller that produces one anyway has written a second copy of
    /// the layout rules.
    ///
    /// **What it costs, and when.** The measurement is O(count): every item's
    /// description is built and laid out, the off-screen ones included, because
    /// the tallest item is not something the visible ones know. It is paid when
    /// the width changes and when this list's own description is rebuilt —
    /// which is conservative, since a rebuild that changed only which row is
    /// hovered pays for an answer that comes back the same. It is **not** paid
    /// to scroll: the band is cached against the width, and a wheel, a key or a
    /// reveal moves the window without describing anything off screen. Nor is
    /// it paid by a frame that only paints.
    ///
    /// Two things follow from measuring one band for the whole list. The band
    /// is measured with every row in [`RowState::Normal`], so a builder whose
    /// row is *taller* when selected or hovered is not uniform and will be cut
    /// to the normal height. And the items are described during layout and
    /// discarded before the frame is painted, so a row carrying its own
    /// component state pays a mount and an unmount for each measurement.
    UniformMeasured,
}

impl Default for RowHeight {
    fn default() -> Self {
        RowHeight::Cells(1)
    }
}

impl RowHeight {
    /// The number to build rows against before anything has been measured: the
    /// caller's, or one cell where the band is still a question.
    fn declared(self) -> u16 {
        match self {
            RowHeight::Cells(c) => c.max(1),
            RowHeight::UniformMeasured => 1,
        }
    }
}

/// A row builder: the node for index `i` in state `st`.
type RowOf<M> = Rc<dyn Fn(usize, RowState) -> Node<M>>;

/// Per-window work: handed the index range the window holds, it returns the
/// row builder for that window. See [`List::windowed_cut`].
type CutOf<M> = Rc<dyn Fn(std::ops::Range<usize>) -> RowOf<M>>;

enum Source<M> {
    Eager(Rc<Vec<(Key, Node<M>)>>),
    Windowed {
        count: usize,
        key: Rc<dyn Fn(usize) -> Key>,
        row: RowOf<M>,
        /// The per-window step, when the rows depend on which rows are in
        /// the window. `None` for rows that are a function of their index.
        cut: Option<CutOf<M>>,
    },
}

impl<M> Clone for Source<M> {
    fn clone(&self) -> Self {
        match self {
            Source::Eager(v) => Source::Eager(v.clone()),
            Source::Windowed {
                count,
                key,
                row,
                cut,
            } => Source::Windowed {
                count: *count,
                key: key.clone(),
                row: row.clone(),
                cut: cut.clone(),
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

    /// Row `i`'s key.
    fn key_of(&self, i: usize) -> Option<Key> {
        match self {
            Source::Eager(v) => v.get(i).map(|(k, _)| k.clone()),
            Source::Windowed { count, key, .. } => (i < *count).then(|| key(i)),
        }
    }

    /// Where the row keyed `k` is: `hint` when it is still there, a walk
    /// otherwise. Only the element's own selection asks, and only once it
    /// has moved.
    fn position_of(&self, k: &Key, hint: usize) -> Option<usize> {
        if self.key_of(hint).as_ref() == Some(k) {
            return Some(hint);
        }
        (0..self.len()).find(|&i| self.key_of(i).as_ref() == Some(k))
    }

    fn at(&self, i: usize, state: RowState) -> Option<(Key, Node<M>)> {
        match self {
            // Eager rows are built before the list knows anything about them,
            // so their state cannot reach them. `row_theme` still names them.
            Source::Eager(v) => v.get(i).cloned(),
            Source::Windowed {
                count, key, row, ..
            } => (i < *count).then(|| (key(i), row(i, state))),
        }
    }

    /// The source for one window: the same rows, built by the builder the
    /// per-window step returns for `window` when there is one.
    fn for_window(&self, window: std::ops::Range<usize>) -> Source<M> {
        match self {
            Source::Windowed {
                count,
                key,
                cut: Some(cut),
                ..
            } => Source::Windowed {
                count: *count,
                key: key.clone(),
                row: cut(window),
                cut: None,
            },
            other => other.clone(),
        }
    }
}

/// Builds the click handler for a row, given its index. Factored out because
/// the nested closure type is otherwise unwieldy.
type RowClick<M> = Rc<dyn Fn(usize) -> crate::desc::Handler<M>>;

/// What a list's selection is, and who owns it.
///
/// **"Controlled" and "on a row" are different facts.** `selected(i)` could
/// only ever say both at once, so a caller with a controlled list and nothing
/// selected had no way to say so — omitting it handed the selection back to
/// the element, whose own starts at row zero, and the first row came out
/// highlighted. That is what a settings field's `[+] Add new` sentinel looked
/// like when it was *not* the focused row.
#[derive(Clone, Copy, Default, PartialEq, Eq, Debug)]
enum Sel {
    /// The owner never said; the element keeps its own.
    #[default]
    Own,
    /// The owner holds it, and it is on this row.
    At(usize),
    /// The owner holds it, and no row is selected.
    Empty,
}

/// What a row's activation sends: its index and the event that activated it.
pub type ActivateHandler<M> = Rc<dyn Fn(usize, &Event) -> Option<M>>;

pub struct List<M> {
    source: Source<M>,
    selection: Sel,
    on_select: Option<Rc<dyn Fn(usize) -> M>>,
    /// The window's offset, when the owner holds it; `None` leaves it to the
    /// viewport. Two states rather than `Sel`'s three: a window is always
    /// somewhere, so "controlled and empty" has no meaning here.
    scroll: Option<usize>,
    /// Where a window the viewport owns starts. See [`List::start_at`].
    start: usize,
    on_scroll: Option<Rc<dyn Fn(usize) -> M>>,
    pinned: Rc<[usize]>,
    /// The pinned rows as a function of the window's first row, evaluated at
    /// layout. See [`List::pinned_at`].
    /// With the same function as the window sees it, made once here: the
    /// window compares it by identity, and one made per build would read
    /// as a change on every build.
    #[allow(clippy::type_complexity)]
    pinned_at: Option<(Rc<dyn Fn(usize) -> Rc<[usize]>>, crate::desc::PinnedAt)>,
    /// Each row's own height, stated by the owner. See [`List::row_heights`].
    heights: Option<Rc<dyn Fn(usize) -> u16>>,
    on_activate: Option<ActivateHandler<M>>,
    activate_on: Activate,
    focusable: bool,
    autofocus: bool,
    scrollbar: bool,
    stable_gutter: bool,
    bar_hidden: bool,
    overlay: bool,
    bar_theme: Option<String>,
    #[allow(clippy::type_complexity)]
    row_theme: Option<Rc<dyn Fn(usize, RowState) -> String>>,
    row_height: RowHeight,
    /// Keep the selection in the window on every layout, re-armed whenever
    /// the selection or this token changes. See [`List::follow_selection`].
    follow: Option<u64>,
    /// Re-arm the follow on the token alone, not on a selection move. See
    /// [`List::follow_on`].
    follow_by_token: bool,
    /// The follow token the window at [`start`](List::start_at) already
    /// answers. See [`List::follow_answered`].
    answered: Option<u64>,
    /// The owner's page handle, when it pages the list from its own keys.
    /// See [`List::pager`].
    pager: Option<Rc<crate::behavior::Pager>>,
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
            row: Rc::new(move |i, _| row(i)),
            cut: None,
        })
    }

    /// The same, for rows that name their own ink.
    ///
    /// **A stamped theme is a ground, and a row that paints its own cells
    /// covers it.** [`row_theme`](Self::row_theme) names the row node, which
    /// emits a fill under whatever the builder produced — so a host whose rows
    /// carry explicit foreground *and* background (which every row built from
    /// an editor `TextPropertyEntry` does, because a description has no
    /// "already" to show through) got a selection band only in the gaps
    /// between its glyphs. The orchestrator dock's compact rows were the
    /// symptom: the active session's highlight appeared on the padding after
    /// the name and nowhere else.
    ///
    /// The state machine stays the widget's — which is the whole point of
    /// [`RowState`] — and this hands it to the builder as well as to
    /// `row_theme`, so a host can build the row *from* its state instead of
    /// having one painted behind it.
    pub fn windowed_stateful(
        count: usize,
        key: impl Fn(usize) -> Key + 'static,
        row: impl Fn(usize, RowState) -> Node<M> + 'static,
    ) -> Self {
        List::from_source(Source::Windowed {
            count,
            key: Rc::new(key),
            row: Rc::new(row),
            cut: None,
        })
    }

    /// Rows that depend on which rows are in the window.
    ///
    /// **Per-window work belongs at the cut.** Some facts about a row are
    /// facts about its neighbours on screen — the prompt's name column is as
    /// wide as the longest name *in view*, so one long name a thousand rows
    /// away does not squeeze every description above it. The window is known
    /// in exactly one place, the layout reader below, so that is where this
    /// runs: `cut` is handed the index range on screen and returns what the
    /// rows need to know about it, and `row` builds each row with that answer
    /// — the overscan rows above and below included, so a row scrolled in by
    /// one cell looks like its neighbours until the next layout re-cuts.
    ///
    /// Computed anywhere earlier, it has to read the previous frame's window
    /// back off the tree — one frame late whenever the window moves.
    ///
    /// A measuring pass ([`RowHeight::UniformMeasured`]) builds its probe rows
    /// with the answer for the whole list, since every item is on its bench.
    pub fn windowed_cut<W: 'static>(
        count: usize,
        key: impl Fn(usize) -> Key + 'static,
        cut: impl Fn(std::ops::Range<usize>) -> W + 'static,
        row: impl Fn(usize, RowState, &W) -> Node<M> + 'static,
    ) -> Self {
        let cut = Rc::new(cut);
        let row = Rc::new(row);
        // Outside the reader (a measuring pass) the answer is the whole
        // list's, worked out once however many rows ask for it.
        let whole: RowOf<M> = {
            let (cut, row) = (cut.clone(), row.clone());
            let once: Rc<std::cell::OnceCell<W>> = Rc::default();
            Rc::new(move |i, st| row(i, st, once.get_or_init(|| cut(0..count))))
        };
        let per_window: CutOf<M> = Rc::new(move |window| {
            let w = Rc::new(cut(window));
            let row = row.clone();
            let built: RowOf<M> = Rc::new(move |i, st| row(i, st, &w));
            built
        });
        List::from_source(Source::Windowed {
            count,
            key: Rc::new(key),
            row: whole,
            cut: Some(per_window),
        })
    }

    fn from_source(source: Source<M>) -> Self {
        List {
            source,
            selection: Sel::Own,
            on_select: None,
            scroll: None,
            start: 0,
            on_scroll: None,
            pinned: Rc::from(Vec::new()),
            pinned_at: None,
            heights: None,
            on_activate: None,
            activate_on: Activate::default(),
            focusable: true,
            autofocus: false,
            scrollbar: false,
            bar_hidden: false,
            overlay: false,
            stable_gutter: false,
            bar_theme: None,
            row_theme: None,
            row_height: RowHeight::default(),
            follow: None,
            follow_by_token: false,
            answered: None,
            pager: None,
        }
    }

    /// Controlled selection: the owner holds it and is told when it should
    /// change. Omit this and the element keeps its own.
    pub fn selected(mut self, i: usize) -> Self {
        self.selection = Sel::At(i);
        self
    }

    /// The same, for an owner whose selection may be empty. `None` is a
    /// *controlled* empty selection — no row is highlighted, and no row is
    /// confirmed — not "the owner has no opinion".
    pub fn selection(mut self, i: Option<usize>) -> Self {
        self.selection = match i {
            Some(i) => Sel::At(i),
            None => Sel::Empty,
        };
        self
    }

    /// **Keep the selection in view on every layout**, not only when it moves.
    ///
    /// By default the window follows a selection *move* — once, on the build
    /// that carries it — so a wheel that scrolls away is not fought. A caret is
    /// different: the row it sits on can stay the same while what is around it
    /// re-wraps, and the window can change height a frame after the move (a
    /// box that grows with its text), and either leaves a one-shot reveal
    /// answered against a layout that no longer holds. With this, the window
    /// keeps the selection in it on every layout
    /// ([`Anchor::follow`](crate::behavior::anchor::Anchor::follow)); a wheel
    /// over it wins until the request is armed again, which happens when the
    /// selected row or `token` changes — pass something that changes
    /// whenever the caret moves (its byte, say), or whenever the owner acts
    /// on the selection. The selected row is its key: rows inserted above it
    /// move the follow with it, and do not re-arm one a wheel cleared. A
    /// `None` selection stops following.
    pub fn follow_selection(mut self, token: u64) -> Self {
        self.follow = Some(token);
        self
    }

    /// [`follow_selection`](Self::follow_selection), re-armed **only** when
    /// `token` changes — for an owner that says, with its token, which
    /// selection moves should be brought into view. A selection that moves
    /// without it (a right-click that picks the row a menu is about) leaves
    /// the window where it is.
    pub fn follow_on(mut self, token: u64) -> Self {
        self.follow = Some(token);
        self.follow_by_token = true;
        self
    }

    /// The follow token the window at [`start_at`](Self::start_at) already
    /// answers — the owner's token as of the last window it was told of
    /// ([`on_scroll`](Self::on_scroll)). **A list mounted again is not a new
    /// request:** without this, a following list mounted at `start` arms its
    /// follow on the first build and pulls the window back to the selection
    /// the reader had wheeled away from. With it, the list follows on mount
    /// only if the owner's token moved since.
    pub fn follow_answered(mut self, token: u64) -> Self {
        self.answered = Some(token);
        self
    }

    /// Record this list's window in `pager`, so its owner can ask where a
    /// page from the selection lands ([`Pager::target`]). For an owner whose
    /// own keymap resolves the page keys: the key stays the owner's, and the
    /// page is the height layout gave the list. A focusable list pages itself
    /// on PageUp/PageDown by the same number either way.
    ///
    /// [`Pager::target`]: crate::behavior::Pager::target
    pub fn pager(mut self, pager: Rc<crate::behavior::Pager>) -> Self {
        self.pager = Some(pager);
        self
    }

    pub fn on_select(mut self, f: impl Fn(usize) -> M + 'static) -> Self {
        self.on_select = Some(Rc::new(f));
        self
    }

    /// Controlled window: the owner holds the offset and is told, through
    /// [`on_scroll`](Self::on_scroll), when it should change — the same
    /// pattern as [`selected`](Self::selected) / [`on_select`](Self::on_select),
    /// for the other fact a list keeps. Omit this and the viewport keeps its
    /// own.
    ///
    /// The offset is the index of the first row of the run under any
    /// [`pinned`](Self::pinned) rows. See [`Scroll::At`](crate::Scroll::At)
    /// for what the framework does with a wheel meanwhile, and for why the
    /// owner's value is never clamped.
    pub fn scroll(mut self, offset: usize) -> Self {
        self.scroll = Some(offset);
        self
    }

    /// Where the window starts when the list mounts. **The initial value
    /// only:** from then on the window is the viewport's, and wherever the
    /// wheel, the bar and a reveal put it, clamped to its ceiling. For an
    /// owner that mounts the list again later — a panel handed out while a
    /// background task works on it — and wants it back where it was, which
    /// [`on_scroll`](Self::on_scroll) told it. Ignored under
    /// [`scroll`](Self::scroll), which states the offset at every layout.
    pub fn start_at(mut self, offset: usize) -> Self {
        self.start = offset;
        self
    }

    /// Be told where the framework put the window — after a wheel, a bar
    /// drag, or a selection move that asked the window to follow. See
    /// [`Node::on_scroll`](crate::Node::on_scroll).
    pub fn on_scroll(mut self, f: impl Fn(usize) -> M + 'static) -> Self {
        self.on_scroll = Some(Rc::new(f));
        self
    }

    /// Rows held at the top of the window whatever the offset — sticky group
    /// headers, a scrolled tree's expanded ancestors — in the order they are
    /// drawn. The owner names them (they are a function of the offset, and of
    /// what a "header" is, which only the owner knows) and the window makes
    /// room: the run starts under them and the ceiling accounts for them. See
    /// [`Node::pinned`](crate::Node::pinned).
    ///
    /// A pinned row is built by the same builder, in the same state, with the
    /// same handlers as it would be in the run: clicking it selects it, and
    /// hovering it tints it.
    pub fn pinned(mut self, indices: &[usize]) -> Self {
        self.pinned = Rc::from(indices);
        self
    }

    /// The pinned rows as a function of the row the window starts on —
    /// asked by the window at layout, where the offset is known, rather than
    /// named by the owner after it is told where the window went. A tree's
    /// sticky ancestors are this: the expanded ancestors of the first row of
    /// the run. Replaces [`List::pinned`]. See
    /// [`Node::pinned_at`](crate::Node::pinned_at).
    pub fn pinned_at(mut self, f: impl Fn(usize) -> Rc<[usize]> + 'static) -> Self {
        let f: Rc<dyn Fn(usize) -> Rc<[usize]>> = Rc::new(f);
        let window = {
            let f = f.clone();
            crate::desc::PinnedAt(Rc::new(move |y: u32| {
                f(y as usize)
                    .iter()
                    .map(|&i| u32::try_from(i).unwrap_or(u32::MAX))
                    .collect()
            }))
        };
        self.pinned_at = Some((f, window));
        self
    }

    pub fn on_activate(mut self, f: impl Fn(usize) -> M + 'static) -> Self {
        self.on_activate = Some(Rc::new(move |i, _: &Event| Some(f(i))));
        self
    }

    /// As `on_activate`, for an activation that may produce no message — and
    /// that wants the press behind it.
    ///
    /// **The row's index is not the whole of what activated it.** Every other
    /// handler in the library is handed the `Event`; this one was handed an
    /// index alone, so a host whose rows mean different things on a single and
    /// a double press had to reach back for the click count through a side
    /// channel. [`Activate`] is the per-*list* form of that question and stays
    /// the right answer where one policy fits every row; the event is what a
    /// list needs when it does not (a settings map field: its `[+] Add new`
    /// row opens on one press and a committed row on two).
    ///
    /// Keyboard activation passes the key press, so a handler that reads
    /// `clicks` sees zero there — which is the honest answer for an activation
    /// no mouse made.
    pub fn on_activate_handler(mut self, f: ActivateHandler<M>) -> Self {
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

    /// An overlay bar, drawn only while the caller says to — see
    /// [`Node::scrollbar_revealed`](crate::Node::scrollbar_revealed).
    ///
    /// Composes with [`scrollbar_gutter`](Self::scrollbar_gutter): the pair is
    /// a reserved column the bar is only drawn in on attention, which is what
    /// a list whose rows reach their last column wants — an overlay bar alone
    /// is painted over that column.
    pub fn scrollbar_revealed(mut self, shown: bool) -> Self {
        self.scrollbar = true;
        self.overlay = true;
        self.bar_hidden = !shown;
        self
    }

    /// The bar, on terms the caller carries — see
    /// [`Node::scrollbar_when`](crate::Node::scrollbar_when).
    pub fn scrollbar_when(self, reveal: Option<bool>) -> Self {
        match reveal {
            None => self.scrollbar(),
            Some(shown) => self.scrollbar_revealed(shown),
        }
    }

    /// Name the bar's appearance — see
    /// [`Node::scrollbar_theme`](crate::Node::scrollbar_theme).
    pub fn scrollbar_theme(mut self, name: impl AsRef<str>) -> Self {
        self.bar_theme = Some(name.as_ref().to_string());
        self
    }

    /// How many cells one row occupies. One by default, which is what a list
    /// of lines is.
    ///
    /// **Uniform, and that is the point.** A list of little blocks rather than
    /// lines is still addressable by index: the window knows which items it
    /// holds without measuring any of them, which is what keeps a window onto a
    /// million of them possible. Rows that each state their *own* height
    /// are [`List::row_heights`], which windows in cells over a prefix sum
    /// of them instead.
    ///
    /// This is the shorthand for [`RowHeight::Cells`]. A band that is uniform
    /// but that the caller cannot state — a card list, whose height is a
    /// function of the width — is not the other widget either, and is
    /// [`RowHeight::UniformMeasured`]; see [`List::row_height`].
    pub fn row_rows(self, cells: u16) -> Self {
        self.row_height(RowHeight::Cells(cells.max(1)))
    }

    /// **Rows of different heights, each stated by the owner** — a card
    /// tree, whose cards are several rows and whose folder headers are one.
    /// The window counts in cells rather than items: it scrolls a row at a
    /// time, a card can be half in view, and the rows built are the ones
    /// that overlap it, between two spacers that stand for the rest. The
    /// heights are summed once per description, never per scroll.
    ///
    /// A page is not a count of items here, so the list records none in its
    /// pager, and pins are not honoured.
    pub fn row_heights(mut self, f: impl Fn(usize) -> u16 + 'static) -> Self {
        self.heights = Some(Rc::new(f));
        self
    }

    /// Where one row's height comes from: the caller, or the layout. See
    /// [`RowHeight`], which carries the cost of each.
    pub fn row_height(mut self, h: RowHeight) -> Self {
        self.row_height = h;
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
            pager: Some(cx.register(crate::behavior::pager::PagerSlot::default())),
            ..ListState::default()
        }
    }

    fn build(&self, s: &ListState, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let n = self.source.len();
        let last = n.saturating_sub(1);
        // An empty list has no row to select, whatever the owner or the
        // element last held.
        let sel: Option<usize> = match self.selection {
            _ if n == 0 => None,
            // The element's own selection follows its row; an index it was
            // last seen at is only a hint, and a row that is gone leaves the
            // hint where it was.
            Sel::Own => Some(
                s.selected_key
                    .as_ref()
                    .and_then(|k| self.source.position_of(k, s.selected))
                    .unwrap_or(s.selected)
                    .min(last),
            ),
            Sel::At(i) => Some(i.min(last)),
            Sel::Empty => None,
        };
        let up: Updater<ListState> = cx.updater();
        let anchor = s.anchor.clone();

        // Keep the selection inside the window when it *moves* — by key here,
        // or by the owner passing a new one down. Asking on every build would
        // fight the wheel, which is a statement about the window rather than
        // about the selection; the memo is what distinguishes the two, and its
        // write is an idempotent function of the build inputs.
        // Where each row starts, in cells, when the owner states each row's
        // height: summed once for this description and shared with the
        // reader, which reads it on every scroll.
        let heights = self.heights.clone();
        let starts: Rc<std::cell::OnceCell<Vec<u32>>> = Rc::default();
        // The selected row's band in the window's unit: `(start, extent)`.
        // An item window counts items, so a row is one unit at its index; a
        // window over rows of their own heights counts cells.
        let band_of = {
            let (heights, starts) = (heights.clone(), starts.clone());
            move |i: usize| -> (u32, u32) {
                match heights.as_ref() {
                    Some(h) => {
                        let st = starts.get_or_init(|| prefix(n, &**h));
                        (st[i], st[i + 1] - st[i])
                    }
                    None => (i as u32, 1),
                }
            }
        };
        match (self.follow, &anchor, sel) {
            // A standing follow, re-armed when the selection or the owner's
            // token moves — so a wheel since then is not fought, and a layout
            // since then is still answered.
            (Some(token), Some(a), Some(sel)) => {
                let (top, _) = band_of(sel);
                let key = self.source.key_of(sel);
                let armed_on = |t: u64| match self.follow_by_token {
                    true => (None, t),
                    false => (key.clone(), t),
                };
                // Mounted again at a window that already answers a token: that
                // token is spent, as if this state had armed it.
                if let Some(t) = self.answered {
                    s.followed.seed(armed_on(t), key.clone());
                }
                let armed = a.clone();
                let row = s.followed.get_or(armed_on(token), || {
                    armed.follow(top);
                    key.clone()
                });
                // A standing follow is of the row it was armed on, wherever
                // rows arriving above it have moved it; one a wheel cleared
                // stays clear.
                if row == key && a.following().is_some_and(|at| at != top) {
                    a.follow(top);
                }
            }
            (Some(_), Some(a), None) => a.unfollow(),
            // The band's last unit, then its first: the shortest move shows
            // its end, and a band taller than the window is shown from its
            // top.
            (None, Some(a), Some(sel)) => {
                let (top, rows) = band_of(sel);
                let a = a.clone();
                s.revealed
                    .get_or(Revealed(sel, self.source.key_of(sel)), move || {
                        a.reveal(top + rows.saturating_sub(1));
                        a.reveal(top);
                    });
            }
            _ => {}
        }

        let source = self.source.clone();
        let hov = s.hovered.clone();
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
            let keys = self.source.clone();
            Some(Rc::new(move |i: usize| {
                let key = keys.key_of(i);
                let up = up.clone();
                let anchor = anchor.clone();
                let on_select = on_select.clone();
                let on_activate = on_activate.clone();
                let handler: crate::desc::Handler<M> = Rc::new(move |e: &Event| {
                    if takes_focus {
                        e.request_focus(crate::event::SelectionOnFocus::Preserve);
                    }
                    let key = key.clone();
                    up.set(move |st: &mut ListState| {
                        st.selected = i;
                        st.selected_key = key;
                    });
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
                    match on_activate.as_ref().and_then(|f| f(i, e)) {
                        Some(m) => Some(m),
                        None => selected,
                    }
                });
                handler
            }))
        };

        // The row under the pointer tints itself. Enter and Leave are mirrored
        // into `hovered`, which the reader below reads back — the same shape as
        // the focus mirror, one row at a time, named by the row's key.
        let hover: Rc<dyn Fn(Key) -> crate::desc::Handler<M>> = {
            let up = up.clone();
            Rc::new(move |k: Key| {
                let up = up.clone();
                let h: crate::desc::Handler<M> = Rc::new(move |e: &Event| {
                    let over = e.kind == GestureKind::Enter;
                    let k = k.clone();
                    up.set(move |st: &mut ListState| {
                        if over {
                            st.hovered = Some(k);
                        } else if st.hovered.as_ref() == Some(&k) {
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
        //
        // The band comes from the viewport too. For a stated height it is the
        // caller's number arriving back where it is needed; for a measured one
        // it is the viewport's answer to its own question, which is why the
        // builder below runs twice in the one layout pass.
        let measured = self.row_height == RowHeight::UniformMeasured;
        let declared = self.row_height.declared();
        let pinned = self.pinned.clone();
        let pinned_at = self.pinned_at.as_ref().map(|(f, _)| f.clone());
        let pager = match &s.pager {
            Some(slot) => slot.bind(self.pager.clone()),
            None => self.pager.clone().unwrap_or_default(),
        };
        let recorder = pager.clone();
        let reader = layout_reader(move |info| {
            let measuring = info.band == Some(Band::Measuring);
            let row_rows = match info.band {
                Some(Band::Cells(h)) => h.max(1),
                _ => declared,
            };
            // The window the viewport published is the run under the pinned
            // rows, already clamped to its ceiling — so the run starts where
            // it says, and the pinned rows go above it: as many as the
            // viewport said it made room for, which is fewer than were named
            // when they would have taken the whole window.
            let win = info.scroll_window.unwrap_or_default();
            let visible = (win.h as usize).max(1);
            // Rows of their own heights: the window is in cells, and the rows
            // built are the ones overlapping it.
            let cells = heights
                .as_ref()
                .map(|h| starts.get_or_init(|| prefix(n, &**h)).as_slice());
            // `(first, end, last)`: the rows on screen are `first..end`, and
            // the rows built run on to `last`, the overscan past them.
            let (first, end, last) = match cells {
                Some(st) => {
                    let (y, h) = (win.y.max(0) as u32, win.h as u32);
                    let first = st.partition_point(|&s| s <= y).saturating_sub(1).min(n);
                    let end = st.partition_point(|&s| s < y + h).min(n);
                    (first, end, (end + OVERSCAN).min(n))
                }
                None => {
                    // The page is the window this layout placed, in items.
                    if info.scroll_window.is_some() && !measuring {
                        recorder.record(visible);
                    }
                    let first = (win.y.max(0) as usize).min(n);
                    let end = (first + visible).min(n);
                    (first, end, (end + OVERSCAN).min(n))
                }
            };
            // The pins of this window: asked of the owner's function at the
            // offset the window is at, or the fixed list. None for rows of
            // their own heights.
            let pinned: Rc<[usize]> = match (&pinned_at, cells) {
                (_, Some(_)) => Rc::from(Vec::new()),
                (Some(f), None) => f(first),
                (None, None) => pinned.clone(),
            };
            let pins = (info.pinned as usize).min(pinned.len());
            // The window is known here and nowhere earlier: a source with
            // per-window work does it now, over the rows about to be built.
            let run = source.for_window(first..end);
            let indices = pinned[..pins]
                .iter()
                .copied()
                .filter(|&i| i < n)
                .chain(first..last);
            let window = col().children(indices.map(|i| {
                let k = run.key_of(i).expect("index inside the source");
                let state = if Some(i) == sel {
                    // A selected row reads as focused only when the list has
                    // focus; otherwise it is muted.
                    if focused {
                        RowState::Selected
                    } else {
                        RowState::SelectedBlur
                    }
                } else if hov.as_ref() == Some(&k) {
                    RowState::Hover
                } else {
                    RowState::Normal
                };
                let (_, row) = run.at(i, state).expect("index inside the source");
                let theme: String = match &row_theme {
                    Some(f) => f(i, state),
                    None => state.theme().to_string(),
                };
                let rows_h = match &heights {
                    Some(h) => h(i).max(1),
                    None => row_rows,
                };
                let content = row.theme(theme).h(Sizing::Cells(rows_h));
                let g = gesture(content)
                    .on_enter(hover(k.clone()))
                    .on_leave(hover(k.clone()));
                let g = match &click {
                    Some(mk) => g.on(GestureKind::Click, mk(i)),
                    None => g,
                };
                // **The key goes on the row's outermost node.** Siblings are
                // matched by key only where the key is; keyed inside an
                // unkeyed wrapper, a row inserted above would shift every
                // wrapper below it by position and remount what they hold.
                g.key(k)
            }));
            // The rows above and below the window, as two spacers: the column
            // is as tall as all of it, so the window scrolls over the whole.
            if let Some(st) = cells {
                let cap = |c: u32| Sizing::Cells(c.min(u16::MAX as u32) as u16);
                return col().children([
                    crate::desc::row().h(cap(st[first])),
                    window,
                    crate::desc::row().h(cap(st[n] - st[last])),
                ]);
            }
            if !measured {
                return window;
            }
            // **Two slots, in both asks, so that only the probe is transient.**
            // Answering the band means describing every item, and the window's
            // rows have to survive that: a reader whose whole output is
            // replaced loses element identity for everything under it, which
            // for a row means its nested state and any press in flight. So the
            // shape is the same either way — a probe slot and a window slot —
            // and only the probe's contents come and go.
            //
            // The height of what this returns is what the viewport reads as the
            // band, which is why the probe is a stack: laid one on top of
            // another, the items measure as tall as the tallest of them. The
            // window slot is flattened to nothing so that the height coming
            // back is the probe's alone; its rows are built against a band
            // nobody will see, and the ask with the real band is the one whose
            // rows are painted.
            //
            // `Align::Start` gives each item the loose width a row of the
            // window gets from its column, rather than the stretched one a
            // stack would otherwise impose, so the two measurements are of the
            // same thing.
            stack().children([
                stack().align(Align::Start).children(match measuring {
                    true => (0..n)
                        .filter_map(|i| source.at(i, RowState::Normal).map(|(_, row)| row))
                        .collect::<Vec<_>>(),
                    false => Vec::new(),
                }),
                match measuring {
                    true => window.h(Sizing::Cells(0)),
                    false => window,
                },
            ])
        });

        let mut body = viewport(reader);
        if self.heights.is_none() {
            body = body.items(n as u32);
            body = match self.row_height {
                RowHeight::Cells(c) => body.item_rows(c),
                RowHeight::UniformMeasured => body.item_rows_measured(),
            };
        }
        match self.scroll {
            Some(y) => body = body.scroll(u32::try_from(y).unwrap_or(u32::MAX)),
            None if self.start > 0 => {
                body = body.scroll_at(0, u32::try_from(self.start).unwrap_or(u32::MAX))
            }
            None => {}
        }
        if let Some(f) = self.on_scroll.clone() {
            body = body.on_scroll(move |y| f(y as usize));
        }
        if self.heights.is_some() {
            // Pins are an item window's; see `row_heights`.
        } else if let Some((_, window)) = &self.pinned_at {
            body = body.pinned_by(window.clone());
        } else if !self.pinned.is_empty() {
            let pins: Vec<u32> = self
                .pinned
                .iter()
                .map(|&i| u32::try_from(i).unwrap_or(u32::MAX))
                .collect();
            body = body.pinned(&pins);
        }
        if let Some(a) = anchor.clone() {
            body = body.anchor_to(a);
        }
        if self.overlay {
            body = body.scrollbar_revealed(!self.bar_hidden);
            // Asked for both: the column is reserved on every frame and the
            // bar appears in it on attention.
            if self.stable_gutter {
                body = body.scrollbar_gutter();
            }
        } else if self.stable_gutter {
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
        let keys = self.source.clone();
        let select = Rc::new(
            move |target: usize,
                  up: &Updater<ListState>,
                  anchor: &Option<Rc<crate::behavior::Anchor>>,
                  on_select: &Option<Rc<dyn Fn(usize) -> M>>|
                  -> Option<M> {
                let up = up.clone();
                let key = keys.key_of(target);
                up.set(move |st: &mut ListState| {
                    st.selected = target;
                    st.selected_key = key;
                });
                let _ = anchor;
                on_select.as_ref().map(|f| f(target))
            },
        );

        let up_focus = up.clone();
        let mut node = focusable(body)
            .on_focus_handler(Rc::new(move |e: &Event| {
                let gained = e.kind == GestureKind::FocusGained;
                up_focus.set(move |st: &mut ListState| st.focused = gained);
                None
            }))
            .action_handler(Intent::Up, {
                let (up, a, f) = (up.clone(), anchor.clone(), self.on_select.clone());
                let select = select.clone();
                // With nothing selected, Up lands on the last row and Down
                // on the first — the two ends a walk can start from.
                Rc::new(move |_: &Event| {
                    select(sel.map_or(last, |s| s.saturating_sub(1)), &up, &a, &f)
                })
            })
            .action_handler(Intent::Down, {
                let (up, a, f) = (up.clone(), anchor.clone(), self.on_select.clone());
                let select = select.clone();
                Rc::new(move |_: &Event| select(sel.map_or(0, |s| (s + 1).min(last)), &up, &a, &f))
            })
            .action_handler(Intent::PageUp, {
                let (up, a, f) = (up.clone(), anchor.clone(), self.on_select.clone());
                let (select, pager) = (select.clone(), pager.clone());
                Rc::new(move |_: &Event| {
                    let to = pager.target(sel.unwrap_or(0), -1, n)?;
                    select(to, &up, &a, &f)
                })
            })
            .action_handler(Intent::PageDown, {
                let (up, a, f) = (up.clone(), anchor.clone(), self.on_select.clone());
                let (select, pager) = (select.clone(), pager.clone());
                Rc::new(move |_: &Event| {
                    let to = pager.target(sel.unwrap_or(0), 1, n)?;
                    select(to, &up, &a, &f)
                })
            })
            .action_handler(Intent::Home, {
                let (up, a, f) = (up.clone(), anchor.clone(), self.on_select.clone());
                let select = select.clone();
                Rc::new(move |_: &Event| select(0, &up, &a, &f))
            })
            .action_handler(Intent::End, {
                let (up, a, f) = (up.clone(), anchor.clone(), self.on_select.clone());
                let select = select.clone();
                Rc::new(move |_: &Event| select(n.saturating_sub(1), &up, &a, &f))
            });

        if let (Some(f), Some(sel)) = (self.on_activate.clone(), sel) {
            node = node.action_handler(Intent::Confirm, Rc::new(move |e: &Event| f(sel, e)));
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

/// Where a tree is described from: [`Tree::windowed`]. There is no eager form
/// that takes the nodes; the tree and its expansion are the owner's.
pub struct Tree<M>(std::marker::PhantomData<fn() -> M>);

/// What the owner's projection says about the node at a visible index. The
/// tree is the owner's — the nodes, their order and which are open — and the
/// projection is its flattened, visible order; this is one row of it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TreeRow {
    /// How deep the node is: 0 for a root.
    pub depth: usize,
    pub has_children: bool,
    /// Whether the owner has it expanded. Expansion is the owner's fact; the
    /// tree only reads it.
    pub open: bool,
}

/// One row's builder in a [`WindowedTree`]: the visible index, what the
/// projection says about it, and the row's state.
pub type TreeRowOf<M> = Rc<dyn Fn(usize, TreeRow, RowState) -> Node<M>>;

/// A **windowed, controlled** tree over the owner's projection.
///
/// The owner holds the tree and its expansion, and flattens the visible
/// nodes into an order it can index; this takes that projection — a count,
/// and per index a key, a [`TreeRow`] and a row — and does what the window
/// needs done: builds only the rows on screen, keyed by what they are, and,
/// with [`WindowedTree::sticky`], pins the expanded ancestors of the run's
/// first row above it, asked at layout where the offset is known.
///
/// Everything a [`List`] does it does as one — selection, paging, scrolling
/// — because it is one: [`WindowedTree::list`] hands it over. Nothing here
/// holds the tree or its expansion; a toggle is the owner's to make and the
/// row's to offer (the row knows where its disclosure is drawn).
pub struct WindowedTree<M> {
    count: usize,
    key: RowKeyOf,
    node: Rc<dyn Fn(usize) -> TreeRow>,
    row: TreeRowOf<M>,
    sticky: Option<(usize, ParentOf)>,
}

/// A visible node's parent, by visible index.
type ParentOf = Rc<dyn Fn(usize) -> Option<usize>>;

/// A row's key by index.
type RowKeyOf = Rc<dyn Fn(usize) -> Key>;

impl<M: 'static> WindowedTree<M> {
    /// Pin the expanded ancestors of the window's first row above the run,
    /// at most `max` of them — the innermost, when a path is deeper than
    /// that, because a file's own folder says more than the workspace root.
    /// `parent` names a visible node's parent by visible index: the owner's
    /// tree knows it directly, where walking the projection back would not.
    pub fn sticky(mut self, max: usize, parent: impl Fn(usize) -> Option<usize> + 'static) -> Self {
        self.sticky = Some((max, Rc::new(parent)));
        self
    }

    /// The tree as the list it windows.
    pub fn list(self) -> List<M> {
        let WindowedTree {
            count,
            key,
            node,
            row,
            sticky,
        } = self;
        let rows = {
            let node = node.clone();
            move |i: usize, st: RowState| row(i, node(i), st)
        };
        let list = List::windowed_stateful(count, move |i| key(i), rows);
        match sticky {
            None => list,
            Some((max, parent)) => {
                list.pinned_at(move |first| sticky_ancestors(&*parent, first, max))
            }
        }
    }
}

/// The expanded ancestors of `first`, outermost first, at most `max` of the
/// innermost. None at the top of the tree: nothing is scrolled away there
/// that a header would stand for.
fn sticky_ancestors(
    parent: &dyn Fn(usize) -> Option<usize>,
    first: usize,
    max: usize,
) -> Rc<[usize]> {
    if first == 0 || max == 0 {
        return Rc::from(Vec::new());
    }
    let mut out = Vec::new();
    let mut at = parent(first);
    while let Some(p) = at.filter(|&p| p < first) {
        out.push(p);
        at = parent(p);
    }
    out.truncate(max);
    out.reverse();
    Rc::from(out)
}

impl<M: 'static> Tree<M> {
    /// A windowed tree over the owner's projection. See [`WindowedTree`].
    pub fn windowed(
        count: usize,
        key: impl Fn(usize) -> Key + 'static,
        node: impl Fn(usize) -> TreeRow + 'static,
        row: impl Fn(usize, TreeRow, RowState) -> Node<M> + 'static,
    ) -> WindowedTree<M> {
        WindowedTree {
            count,
            key: Rc::new(key),
            node: Rc::new(node),
            row: Rc::new(row),
            sticky: None,
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
