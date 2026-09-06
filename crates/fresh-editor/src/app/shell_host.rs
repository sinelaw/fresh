//! The buffer split grid as host regions — the load-bearing `Host` leaves.
//!
//! This is the seam the whole migration stands on: the text pipeline keeps its
//! existing logic and is reached from the fold, given a rectangle and a cell
//! buffer. The split renderer already paints into an arbitrary `Buffer` at an
//! arbitrary `Rect`, so nothing in it changes.
//!
//! What this file exists to prove is the **borrow**. Painting a pane needs
//! `WindowBuffers::with_all_mut`'s disjoint split — `(&mut buffers, &mut
//! SplitManager, &mut view_states)` — plus config and theme off the editor.
//! The open question was whether that can be assembled *inside a fold
//! callback*, while the display list being folded is borrowed from the `Ui`.
//! It can, on one condition: the `Ui` must not live on the `Editor`. See
//! `fold`'s module documentation. [`with_grid`] is that assembly, and the only
//! copy of it.
//!
//! **The body is no longer one leaf.** It was: a single `Host` spanning the
//! whole grid, which the split renderer filled with every pane at once by
//! laying them out a second time from `SplitManager`. Each pane is its own
//! `Host` now, and the fold hands each the rectangle layout gave it — so the
//! rectangle a pane is painted at and the rectangle it is clicked at are the
//! same rectangle rather than two that agree. What is left to the body's own
//! leaf is what belongs to no pane: the pass they share, and the separators
//! between them.

use std::collections::HashSet;

use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::Style;

use crate::app::Editor;
use crate::model::event::{BufferId, LeafId};
use crate::view::shell::geometry::PaneRects;
use crate::view::shell::splits::PaneChrome;
use crate::view::ui::split_rendering::{
    paint_leaf, paint_separators, prepare_content, reconcile_panes, record_scrollbar_theme_runs,
    ContentPass, FrameFacts, Stores,
};
use crate::view::ui::{EditorRenderConfig, RenderStyle};

use crate::view::shell::fold::Caret;
use crate::view::shell::frame::{HostRegion, HostTarget};

/// Per-frame facts the split renderer needs that are not borrows.
///
/// `paint_host` takes a region and a rectangle and nothing else — which is
/// right, because a host painter is reached from a display list and a display
/// list carries geometry, not the editor's hover state. So `render` leaves
/// this on the editor before it folds, and the callback reads it there.
#[derive(Clone, Copy, Debug, Default)]
pub struct BodyState {
    pub lsp_waiting: bool,
    pub hide_cursor: bool,
    pub hovered_tab: Option<(crate::view::split::TabTarget, LeafId, bool)>,
    pub hovered_close_split: Option<LeafId>,
    pub hovered_maximize_split: Option<LeafId>,
    /// The tab bar lays out but paints no cells when false — the web renders
    /// tabs natively. Panes always draw.
    pub draw_tab_bar: bool,
}

/// What the split grid publishes back across the seam.
///
/// Every rectangle the grid produces, which is what chrome reads *after*
/// paint: click-to-byte mapping, the scrollbar and separator drags, the tab
/// hit tests. `render` takes this off the painter once the fold returns and
/// files it in `WindowLayoutCache`.
///
/// It is the split renderer's own sink: the panes write into it one `Host` at
/// a time, and a copy of it here would be a second list of the same
/// rectangles.
pub(crate) use crate::view::ui::split_rendering::PaneAreas as BodyOutput;

/// What one dispatch into the shell's tree did.
///
/// Two answers, because the walk behind this one needs both. **Claimed** is
/// whether the tree took the event, and it is reported by the library rather
/// than inferred: a modal swallows a key without producing a message, and a
/// dismissal closes a menu while leaving the right-click available to open
/// the next one. **Changed** is whether anything moved as a result, which is
/// what asks for the repaint — and the two differ exactly where hover lives.
/// What the *event* knows, for the handlers that need more than the message.
///
/// Two facts that only exist while one pointer event is being dispatched, and
/// that a `UiMsg` deliberately does not carry: a message says what happened,
/// not where the pointer was or what the frame looked like a moment before it.
/// They were fields on the `Editor` — written at the top of `shell_dispatch`
/// and read inside a handler reached from its own message loop — which is a
/// local threaded through a `&mut self` boundary, and reads at any other time
/// as durable editor state that anyone may consult.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct EventFacts {
    /// What the menu bar was showing when this event *arrived*, before any
    /// message — including the layer's own dismissal — was applied. A toggle
    /// needs it: by the time a press is handled the menu is already shut, so
    /// asking then always answers "not open" and reopens what the press was
    /// meant to close.
    pub menu_open_before: Option<usize>,
    /// Where the pointer was. A `UiFact::Hover` carries *what* is under the
    /// pointer and not where, and the hover reactions anchor tooltips to the
    /// cell.
    pub at: (u16, u16),
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct Dispatched {
    /// The tree took the event: nothing behind it should act on it.
    pub claimed: bool,
    /// The tree changed something, so the frame is stale.
    pub changed: bool,
}

/// The split grid's painter, for the length of one fold.
///
/// **Frame-scoped, and that is the point.** `paint_host` carries a target and
/// a rectangle and nothing else — a display list is geometry, not the
/// editor's hover state — so whatever a painter needs beyond those two has to
/// travel some other way. When the `Editor` was the painter that way was
/// fields on the `Editor`, and the pile was already three deep before the
/// grid needed two more: the pass every pane shares, and the sink they all
/// append to.
///
/// A painter that lives exactly as long as the fold has somewhere better to
/// put them. `render` builds one, folds with it, and takes the rectangles off
/// it — and the two facts that are genuinely per-frame stop being state the
/// editor carries between frames.
pub struct BodyPainter<'a> {
    editor: &'a mut Editor,
    state: BodyState,
    /// What every pane in this frame shares, resolved when the fold reaches
    /// the body and read by each pane's `Host` after it.
    ///
    /// The body's `Host` is the panes' ancestor, so the display list puts it
    /// first; a pane reached without it would be a tree that mounted a pane
    /// outside the body.
    pass: Option<ContentPass>,
    out: BodyOutput,
    /// The frame's width, for the theme runs recorded in [`Self::finish`].
    screen_width: u16,
    /// What the shell's description of this same grid says each pane has.
    ///
    /// Resolved when the frame was built — this painter is the other half of
    /// that frame, not a second opinion about it. Held here rather than
    /// cloned inside [`with_grid`], because the fold calls that once *per
    /// pane*: a clone in there is a copy of the whole map for every pane on
    /// screen, every frame.
    pane_chrome: std::collections::HashMap<LeafId, PaneChrome>,
    /// The splits whose active buffer is a terminal shown in read-only
    /// scrollback. Gathered once per frame, for the same reason.
    scrollback: HashSet<LeafId>,
    /// The panes the shell tree describes instead of painting — a buffer
    /// holding a mounted plugin panel the adapter covers. Gathered here for
    /// the same reason as the two above, and read by
    /// [`crate::view::ui::split_rendering::orchestration::paint_leaf`] to skip
    /// the text pass. See `FrameFacts::described_panes`.
    described_panes: HashSet<LeafId>,
    /// Where the frame put every pane, read off the tree `render` just laid
    /// out — the same tree whose display list this painter is folded over.
    ///
    /// The body's pass used to ask the split manager for this, which laid
    /// the grid out a second time in a scratch `Ui<()>`; a pane's box came
    /// from that grid and its `Host` rect from the tree, and only a parity
    /// test said the two agreed. Now there is one answer, and [`Self::pane`]
    /// asserts the fold's rect is it.
    rects: PaneRects,
    /// The grid as [`reconcile_body`] prepared it for this frame, taken by
    /// [`Self::body`] so the panes are prepared once per frame — preparing
    /// them again would resize a buffer group's inner panels back to their
    /// panel rects after the reconcile sized them to their content rects,
    /// and the text pass would wrap at a width nobody placed for.
    prepared: Option<PreparedGrid>,
}

/// What [`reconcile_body`] prepared: the frame's panes, in paint order, and
/// the split manager's leaves the separators are drawn between.
pub struct PreparedGrid {
    base_visible: Vec<(LeafId, BufferId, Rect)>,
    pass: ContentPass,
}

impl<'a> BodyPainter<'a> {
    pub fn new(
        editor: &'a mut Editor,
        state: BodyState,
        pane_chrome: std::collections::HashMap<LeafId, PaneChrome>,
        rects: PaneRects,
        prepared: Option<PreparedGrid>,
    ) -> Self {
        let (scrollback, described_panes) = frame_pane_sets(editor);
        Self {
            editor,
            state,
            pass: None,
            out: BodyOutput::default(),
            screen_width: 0,
            pane_chrome,
            scrollback,
            described_panes,
            rects,
            prepared,
        }
    }

    /// The rectangles the grid produced.
    ///
    /// The scrollbar theme runs are recorded here rather than in a pane
    /// because `apply_theme_runs` patches cells the panes are still
    /// appending: it needs every pane painted, which is what "after the fold"
    /// means now that a pane is its own `Host`.
    pub fn finish(self) -> BodyOutput {
        let BodyPainter {
            editor,
            out,
            screen_width,
            ..
        } = self;
        let active = editor.active_window;
        if let Some(win) = editor.windows.get_mut(&active) {
            record_scrollbar_theme_runs(
                &out.pane_rects,
                &mut win.chrome_layout.cell_theme_map,
                screen_width,
            );
        }
        out
    }

    /// The body: resolve what the panes share, and paint what is between
    /// them.
    ///
    /// A separator belongs to no pane — it is the gap between two — so it is
    /// the body's, and the body's `Host` is the only leaf that still spans the
    /// whole grid.
    fn body(&mut self, area: Rect, buf: &mut Buffer) {
        let state = self.state;
        self.screen_width = buf.area.width;
        // The pass keeps its own copy: a `ContentPass` is what the preview
        // path builds for a grid with no painter, so it owns its rects.
        let rects = self.rects.clone();
        // The reconcile prepared the grid for this frame; prepare it again
        // only when nothing did (a caller that folds without reconciling).
        let prepared = self.prepared.take();
        self.pass = with_grid(
            self.editor,
            state,
            buf.area.width,
            &self.pane_chrome,
            &self.scrollback,
            &self.described_panes,
            |facts, stores, mgr, window_chrome| {
                let PreparedGrid { base_visible, pass } = prepared.unwrap_or_else(|| {
                    // The panes at the boxes the tree placed them in — not a
                    // second layout of the grid into `area`.
                    let base_visible = rects.visible(&mgr.visible_leaves());
                    let pass = prepare_content(
                        rects,
                        &base_visible,
                        mgr,
                        stores.split_view_states.as_deref_mut(),
                        facts.grouped_subtrees,
                        window_chrome,
                    );
                    PreparedGrid { base_visible, pass }
                });
                paint_separators(buf, area, mgr, &base_visible, facts, stores);
                pass
            },
        );
    }

    /// One pane, into the rectangle layout gave it.
    ///
    /// **The rectangle is the node's, not the split manager's.** They agree —
    /// the description and the model share `split_rect_ext`, and the parity
    /// tests in `view::shell::splits` are what says so — and where they agree
    /// there is no reason to keep two answers. The pointer half already
    /// routes by this same rectangle, so a pane painted at any other one
    /// would be a pane you cannot click.
    fn pane(&mut self, leaf: LeafId, rect: Rect, buf: &mut Buffer, caret: &mut Caret) {
        // No pass means the fold reached a pane without reaching the body,
        // which the tree does not describe.
        let Some(pass) = self.pass.as_ref() else {
            return;
        };
        // A pane the tree mounts and the pass does not list: the window's
        // splits changed under the description. It paints nothing rather than
        // painting a stale leaf's buffer.
        let Some(mut pane) = pass.visible.iter().copied().find(|(_, id, ..)| *id == leaf) else {
            return;
        };
        // The fold's rect is the pane's node's, and the pass's rect was read
        // off the same node before the fold: one layout, one answer.
        debug_assert_eq!(
            self.rects.pane(leaf),
            Some(rect),
            "pane {leaf:?}: the fold's rect is not the one the tree placed it at"
        );
        pane.3 = rect;
        let state = self.state;
        let out = &mut self.out;
        with_grid(
            self.editor,
            state,
            buf.area.width,
            &self.pane_chrome,
            &self.scrollback,
            &self.described_panes,
            |facts, stores, _mgr, _window_chrome| {
                paint_leaf(buf, pane, facts, pass, stores, out, caret);
            },
        );
    }
}

/// The two per-frame sets every pane's paint reads: the splits showing a
/// terminal in read-only scrollback, and the panes the tree describes instead
/// of painting. Gathered once per frame — a `paint_host` call is per pane.
fn frame_pane_sets(editor: &Editor) -> (HashSet<LeafId>, HashSet<LeafId>) {
    let scrollback = editor
        .windows
        .get(&editor.active_window)
        .and_then(|win| {
            win.buffers.splits().map(|(_, vs_map)| {
                vs_map
                    .iter()
                    .filter(|(leaf, svs)| win.split_terminal_scrollback(**leaf, svs.active_buffer))
                    .map(|(leaf, _)| *leaf)
                    .collect()
            })
        })
        .unwrap_or_default();
    (scrollback, editor.described_panes())
}

/// Reconcile every text pane of the frame about to be painted — see
/// `orchestration::reconcile`.
///
/// **Before the frame's paint, at the frame's rectangles.** `rects` is where
/// the tree just laid out put every pane, and each pane is settled at the
/// content rect the painter will format it into. `render` calls this once
/// the tree is laid out and before the `lines_changed` hooks, so everything
/// after it in the frame reads a settled viewport.
///
/// Returns the prepared grid for [`BodyPainter::new`], so the fold paints
/// the panes this reconciled rather than preparing them a second time.
pub fn reconcile_body(
    editor: &mut Editor,
    state: BodyState,
    rects: &PaneRects,
    screen_width: u16,
    pane_chrome: &std::collections::HashMap<LeafId, PaneChrome>,
) -> Option<PreparedGrid> {
    let (scrollback, described_panes) = frame_pane_sets(editor);
    let rects = rects.clone();
    with_grid(
        editor,
        state,
        screen_width,
        pane_chrome,
        &scrollback,
        &described_panes,
        |facts, stores, mgr, window_chrome| {
            let base_visible = rects.visible(&mgr.visible_leaves());
            let pass = prepare_content(
                rects,
                &base_visible,
                mgr,
                stores.split_view_states.as_deref_mut(),
                facts.grouped_subtrees,
                window_chrome,
            );
            reconcile_panes(&pass, facts, stores);
            PreparedGrid { base_visible, pass }
        },
    )
}

/// Assemble the grid's borrows off the editor and hand them to `f`.
///
/// **This is the borrow the whole seam rests on.** `f` runs inside
/// `WindowBuffers::with_all_mut`'s disjoint split — `(&mut buffers, &mut
/// SplitManager, &mut view_states)` — with the config and theme taken off the
/// editor around it, while the display list being folded is borrowed from a
/// `Ui` that does not live on the editor. It is assembled once per call
/// rather than once per frame because a `paint_host` call is where the
/// editor is in hand; there is no place between them to keep it.
fn with_grid<R>(
    editor: &mut Editor,
    state: BodyState,
    screen_width: u16,
    pane_chrome: &std::collections::HashMap<LeafId, PaneChrome>,
    scrollback_view_splits: &HashSet<LeafId>,
    described_panes: &HashSet<LeafId>,
    f: impl FnOnce(&FrameFacts<'_>, &mut Stores<'_>, &crate::view::split::SplitManager, PaneChrome) -> R,
) -> Option<R> {
    // Built before the `&mut editor.windows` borrow below; it only borrows
    // `editor.config`, so the two coexist — as in `Editor::render`.
    let cfg = EditorRenderConfig::new(
        &editor.config.editor,
        editor.background_fade,
        editor.software_cursor_only,
    );
    let session_mode = editor.session_mode || !editor.software_cursor_only;
    let active_window_id = editor.active_window;

    let win = editor.windows.get_mut(&active_window_id)?;

    let is_maximized = win
        .buffers
        .splits()
        .map(|(mgr, _)| mgr.is_maximized())
        .unwrap_or(false);
    // The window's half of the pane-chrome rule: what the frame offers every
    // pane, before each narrows it by what it is.
    let window_chrome = PaneChrome {
        tabs: win.tab_bar_visible,
        vscroll: cfg.show_vertical_scrollbar,
        hscroll: cfg.show_horizontal_scrollbar,
    };
    let metadata_ref = &win.buffer_metadata;
    let preview_buffer = win.preview.map(|(_, b)| b);
    let event_logs_mut = &mut win.event_logs;
    let grouped_ref = &win.grouped_subtrees;
    let composite_buffers_mut = &mut win.composite_buffers;
    let composite_view_states_mut = &mut win.composite_view_states;
    let cell_theme_map_mut = &mut win.chrome_layout.cell_theme_map;

    win.buffers.with_all_mut(|buffers_mut, mgr, vs_map| {
        // The theme read-guard lives only for the call.
        let theme_guard = editor.theme.read().unwrap();
        let facts = FrameFacts {
            style: RenderStyle {
                theme: &theme_guard,
                ansi_background: editor.ansi_background.as_ref(),
                cfg,
            },
            buffer_metadata: metadata_ref,
            preview_buffer,
            grouped_subtrees: grouped_ref,
            pane_chrome,
            scrollback_view_splits,
            described_panes,
            lsp_waiting: state.lsp_waiting,
            hide_cursor: state.hide_cursor,
            hovered_tab: state.hovered_tab,
            hovered_close_split: state.hovered_close_split,
            hovered_maximize_split: state.hovered_maximize_split,
            is_maximized,
            session_mode,
            draw_tab_bar: state.draw_tab_bar,
            screen_width,
        };
        let mut stores = Stores {
            buffers: buffers_mut,
            event_logs: event_logs_mut,
            composite_buffers: composite_buffers_mut,
            composite_view_states: composite_view_states_mut,
            split_view_states: Some(vs_map),
            cell_theme_map: cell_theme_map_mut,
        };
        f(&facts, &mut stores, &*mgr, window_chrome)
    })
}

/// The frame's host painter.
///
/// During the migration this is what shrinks: every region still listed here
/// is one the old painters own, and each stage moves one of them out into a
/// native `fresh-ui` description. [`HostRegion::Body`] never migrates — the
/// buffer and terminal grid stays cells — but it is no longer *one* leaf: the
/// body's is the separators' and the panes' shared preamble, and each pane
/// carries its own.
impl crate::view::shell::fold::HostPainter for BodyPainter<'_> {
    fn paint_host(&mut self, target: HostTarget, rect: Rect, buf: &mut Buffer, caret: &mut Caret) {
        let region = match target {
            HostTarget::Pane(leaf) => return self.pane(leaf, rect, buf, caret),
            HostTarget::Embed(window_id) => return self.embed(window_id, rect, buf),
            // A band of the overlay prompt's card. Nothing paints per band:
            // `render_overlay_prompt` draws the card whole, between the two
            // fold bands, and the tree's job there is to say where the bands
            // are so that painter and every read-back share one set of
            // rectangles. Reached only if the card's layer is ever folded in a
            // hosts-painting band; today it is not.
            HostTarget::Card(_) => return,
            HostTarget::Region(r) => r,
        };
        match region {
            HostRegion::Body => self.body(rect, buf),
            // Native already — the tree paints these, and the fold never
            // reaches here for them because a native region emits no
            // `Draw::Host`. Listed so that un-migrating one is a compile
            // error rather than a blank row.
            HostRegion::MenuBar | HostRegion::SearchOptions | HostRegion::Explorer => {}
            // The prompt's input row: cells the fold writes, at the rectangle
            // layout gave the region.
            HostRegion::PromptLine => self.editor.render_prompt_line(buf, rect, caret),
            // **Neither paints, and both reach here only when empty.**
            //
            // The dock emits a `Host` only for a column with no mounted panel
            // — an empty dock, which has nothing to draw. It used to be the
            // seam the panel painter drew the interior through; that painter
            // is deleted, and the dock's content is the tree's.
            //
            // The status bar emits one only when it has no items: with items
            // it is described down to the run, and the description leaves no
            // `Host` behind. The prompt row is not this region — it is
            // `HostRegion::PromptLine`, painted three arms above, inside the
            // fold and at the rectangle layout gave it.
            HostRegion::Dock | HostRegion::StatusBar => {}
        }
    }
}

impl BodyPainter<'_> {
    /// An editor window embedded in a plugin panel, painted into the rectangle
    /// layout gave it.
    ///
    /// **The rectangle is handed over, not reconstructed.** The runtime
    /// reserved this space by emitting blank rows and then overlaid the
    /// window's paint on top of them, deriving the target rect from the
    /// panel's inner area plus the row and column those blanks had landed on —
    /// a rectangle rebuilt from where text ended up. A `Host` leaf is given
    /// one, which is the whole difference.
    ///
    /// `preview_window_id` is still borrowed around the call because that is
    /// how the per-window paint path selects a session; what has gone is the
    /// arithmetic, not the mechanism. Window id `0` names no window and paints
    /// nothing, which is the spec's own "renders empty placeholder rows".
    fn embed(&mut self, window_id: u32, rect: Rect, buf: &mut Buffer) {
        paint_embed(self.editor, window_id, rect, buf);
    }
}

/// The body of [`BodyPainter::embed`], as a function, because the *overlay*
/// band needs it too and has no `BodyPainter`.
///
/// See [`EmbedHosts`] for why the overlay band paints hosts at all.
fn paint_embed(editor: &mut Editor, window_id: u32, rect: Rect, buf: &mut Buffer) {
    if window_id == 0 || rect.width == 0 || rect.height == 0 {
        return;
    }
    let theme = editor.theme.read().unwrap().clone();
    let saved = editor.preview_window_id;
    editor.preview_window_id = Some(fresh_core::WindowId(window_id as u64));
    editor.render_session_preview_into_rect(buf, rect, &theme);
    editor.preview_window_id = saved;
}

/// The overlay band's host painter: embedded windows, and nothing else.
///
/// **A `Layer` can contain a `Host`, and one does.** The overlay band folded
/// with `SkipHosts`, which was right while every host leaf was in flow — the
/// panes, the status bar, the dock — and stopped being right the moment
/// `WindowEmbed` became one: a plugin panel is a `Layer`, so its embed is
/// resolved in the overlay band and was skipped there. The float came out as
/// an empty box (issue #2035's `windowEmbed` rendered nothing at all).
///
/// It is not `BodyPainter`: that one resolves the split grid's shared pass and
/// hands back the rectangles the frame is read from, and running it twice
/// would be a second opinion about both. The overlay band's hosts are embeds —
/// a `Card` band paints nothing by its own arm's rule, and a pane or a region
/// in an overlay would be a tree that mounted the body inside a popup — so
/// this answers for the one and ignores the rest.
pub struct EmbedHosts<'a>(pub &'a mut Editor);

impl crate::view::shell::fold::HostPainter for EmbedHosts<'_> {
    fn paint_host(&mut self, target: HostTarget, rect: Rect, buf: &mut Buffer, _: &mut Caret) {
        if let HostTarget::Embed(window_id) = target {
            paint_embed(self.0, window_id, rect, buf);
        }
    }
}

/// The backend's half of theming: a theme name resolved to concrete colours.
///
/// `fresh-ui` never says what anything looks like — an item carries a
/// [`ThemeKey`], a name for *where its appearance comes from*, and mapping that
/// name is the backend's job. This is the same lookup `*Colors::from_theme`
/// performs for the existing controls, expressed once for the shell.
///
/// It is a snapshot of the colours rather than a borrow of the theme, so the
/// fold can hold it while the rest of the editor is mutably borrowed.
pub struct ShellPalette {
    theme: std::sync::Arc<crate::view::theme::Theme>,
}

impl crate::view::shell::fold::Palette for ShellPalette {
    fn style(&self, theme: &fresh_ui::ThemeKey) -> Style {
        shell_theme::resolve(theme.as_str(), &self.theme)
    }
}

/// The shell's theme names, and the one rule for reading them.
///
/// **A name is real theme keys, not a name of our own.** A cell needs a
/// foreground and a background, and an `Item` carries exactly one `ThemeKey` —
/// so a shell name is a *pair*, written `fg_key/bg_key`, optionally followed by
/// a text attribute (`+bold`, `+underline`). Both halves go through
/// [`Theme::resolve_theme_key`], the editor's existing table-generated
/// resolver, so no name is invented here and every colour on screen traces to
/// a theme entry a user can edit.
///
/// This replaced a hand-written match of twenty-odd arms over names like
/// `menu.bar.item.active.mnemonic` — six spellings for two orthogonal
/// attributes, which is the combinatorial blow-up that arrives in earnest with
/// the file explorer (git status × selection × cut × focus). A grammar does not
/// blow up; a list of names does.
///
/// It also converges with the theme inspector, which has always recorded
/// provenance as exactly this pair (`ThemeRun { fg_key, bg_key }`). The display
/// list and the inspector now say the same thing in the same words.
///
/// **[`Ink`] is the value; the written name is how it travels.** The grammar
/// above is a serialisation, because `fresh-ui` carries one opaque
/// `ThemeKey` per item and never interprets it — so the shell builds an `Ink`,
/// writes it into that slot, and reads it back with [`Ink::parse`]. There is
/// one parser and one writer, and neither is reachable from a description:
/// "the same background, a different foreground" is a field assignment, and an
/// attribute is one of five constants rather than a word that might be
/// misspelled. Three call sites used to do that layering by splitting the
/// sentence apart and reassembling it, and they did not agree with each other.
pub mod shell_theme {
    use std::borrow::Cow;
    use std::fmt;

    use ratatui::style::{Color, Modifier, Style};

    use crate::view::theme::Theme;

    /// One half of an [`Ink`]: where a colour comes from.
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Paint {
        /// A theme entry, resolved through the editor's own table.
        Key(Cow<'static, str>),
        /// A colour with no name behind it.
        ///
        /// **The one thing here that is not traceable to a theme entry, and it
        /// is honest about that.** A plugin can hand the editor an
        /// `OverlayColorSpec::Rgb`, and the markdown renderer chooses its own
        /// span colours — arbitrary runtime values no theme ever declared, so
        /// there is no key to name them with. [`Ink::names`] reports `None` for
        /// such a half, which is the true answer for a colour nobody named.
        ///
        /// What replaces it: plugins **register** their colours as named keys
        /// (`plugin.git.status_added_fg`) and `resolve_theme_key` gains a
        /// dynamic tier for them, at which point a plugin colour becomes an
        /// ordinary, inspectable, user-overridable name and this variant can
        /// go. See §6.2 of the migration doc.
        Lit(Color),
        /// A name a **plugin** asked for, over the half that stands if the
        /// theme has no such entry.
        ///
        /// **A plugin's theme key is data, not one of our names.** Everything
        /// else here is written by the shell, so a name that does not resolve
        /// is a typo and the `debug_assert` in [`resolve`] is right to say so.
        /// A key that arrived in an `OverlayColorSpec` is the plugin author's,
        /// and the editor's table is under no obligation to know it —
        /// `git_history.ts` colours commit hashes `syntax.number`, which no
        /// theme has ever had.
        ///
        /// The painter's answer was per half and implicit: it set `fg` only
        /// when the key resolved, so an unknown one left the row's own
        /// foreground in place. `Ink::style` is all-or-nothing, so the same
        /// name dropped the *whole run* to the editor's plain ground — and
        /// tripped the assert. This variant is that implicit fallback written
        /// down: the plugin's name, and what it falls back to, both stated.
        Asked {
            name: Cow<'static, str>,
            under: Box<Paint>,
        },
    }

    impl Paint {
        /// A theme key. A `&'static str` — which nearly every call site has —
        /// borrows rather than allocating.
        pub fn key(k: impl Into<Cow<'static, str>>) -> Paint {
            Paint::Key(k.into())
        }

        /// A name a plugin asked for, over what stands without it.
        pub fn asked(name: impl Into<Cow<'static, str>>, under: Paint) -> Paint {
            Paint::Asked {
                name: name.into(),
                under: Box::new(under),
            }
        }

        /// The key behind this half, when there is one.
        ///
        /// A plugin's asked-for name counts: it is what this half is *for*,
        /// and the theme inspector's job is to report provenance rather than
        /// to grade it.
        pub fn name(&self) -> Option<&str> {
            match self {
                Paint::Key(k) => Some(k),
                Paint::Asked { name, .. } => Some(name),
                Paint::Lit(_) => None,
            }
        }

        /// The key **the shell itself wrote**, which is the only kind that can
        /// be a typo — see [`resolve`]'s assertion.
        fn declared(&self) -> Option<&str> {
            match self {
                Paint::Key(k) => Some(k),
                Paint::Asked { .. } | Paint::Lit(_) => None,
            }
        }

        fn color(&self, theme: &Theme) -> Option<Color> {
            match self {
                Paint::Key(k) => theme.resolve_theme_key(k),
                Paint::Lit(c) => Some(*c),
                // The same two-step the painter used for an
                // `OverlayColorSpec::ThemeKey` — one of the sixteen names, or
                // a theme entry — and then what stands without either.
                Paint::Asked { name, under } => crate::view::theme::named_color_from_str(name)
                    .or_else(|| theme.resolve_theme_key(name))
                    .or_else(|| under.color(theme)),
            }
        }

        /// Read one half of the written form back.
        ///
        /// `#7ee787` is a 24-bit literal, `#i42` a palette index, `#Yellow`
        /// one of the sixteen names; anything else is a theme key.
        fn parse(half: &str) -> Option<Paint> {
            // `asked|under`: neither a key nor a literal contains a pipe, so
            // the first one separates the plugin's name from its fallback.
            if let Some((name, under)) = half.split_once('|') {
                if name.is_empty() {
                    return None;
                }
                return Some(Paint::asked(name.to_string(), Paint::parse(under)?));
            }
            let Some(rest) = half.strip_prefix('#') else {
                return (!half.is_empty()).then(|| Paint::Key(Cow::Owned(half.to_string())));
            };
            let c = match rest.as_bytes() {
                _ if rest.len() == 6 && rest.bytes().all(|b| b.is_ascii_hexdigit()) => {
                    let byte = |i: usize| u8::from_str_radix(&rest[i..i + 2], 16).ok();
                    Color::Rgb(byte(0)?, byte(2)?, byte(4)?)
                }
                [b'i', ..] => Color::Indexed(rest[1..].parse().ok()?),
                _ => crate::view::theme::named_color_from_str(rest)?,
            };
            Some(Paint::Lit(c))
        }
    }

    impl fmt::Display for Paint {
        /// **Every colour round-trips.** An earlier spelling wrote only
        /// `Color::Rgb` as a triple and answered `editor.fg` for everything
        /// else — and theme colours are frequently one of the sixteen names
        /// (`file_status_modified_fg` is `Yellow` in the built-in dark theme),
        /// so every plugin-decorated row in the file explorer silently painted
        /// in the panel's ordinary ink. Nothing failed; it just looked
        /// undecorated.
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Paint::Key(k) => f.write_str(k),
                Paint::Asked { name, under } => write!(f, "{name}|{under}"),
                Paint::Lit(Color::Rgb(r, g, b)) => write!(f, "#{r:02x}{g:02x}{b:02x}"),
                Paint::Lit(Color::Indexed(i)) => write!(f, "#i{i}"),
                Paint::Lit(other) => write!(
                    f,
                    "#{}",
                    crate::view::theme::token_color_named_from_ratatui(*other)
                ),
            }
        }
    }

    /// The text attributes the grammar can spell.
    ///
    /// Reserved for attributes that are *structural* rather than themed: a
    /// mnemonic is underlined because it is a mnemonic. They compose with any
    /// pair and with each other, which is why they are grammar rather than
    /// more names.
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub struct Attrs(u8);

    impl Attrs {
        pub const NONE: Attrs = Attrs(0);
        pub const BOLD: Attrs = Attrs(1 << 0);
        pub const UNDERLINE: Attrs = Attrs(1 << 1);
        pub const ITALIC: Attrs = Attrs(1 << 2);
        pub const STRIKETHROUGH: Attrs = Attrs(1 << 3);
        /// How the editor spells "present but receding" — a disabled command,
        /// a suggestion's source label. The painters that owned those surfaces
        /// reached for `Modifier::DIM` directly, which no theme can override
        /// and no name could carry until this grammar existed.
        pub const DIM: Attrs = Attrs(1 << 4);
        /// **A block caret is an attribute, not a colour.** A form control on
        /// a modal overlay has no hardware cursor to place, so the runtime
        /// draws its caret as one reverse-video cell — an `OverlayOptions`
        /// with `reversed: true`. The grammar had no word for it, so every
        /// such caret was dropped on the way into a run: the Settings text
        /// fields, a `TextList`'s add slot and the JSON editor all showed a
        /// bracketed box with nothing in it while the user was typing.
        pub const REVERSED: Attrs = Attrs(1 << 5);

        /// The written spelling of each, and the only place the two forms are
        /// paired: [`Attrs::named`] and [`fmt::Display`] both read this.
        const SPELLINGS: [(Attrs, &'static str, Modifier); 6] = [
            (Attrs::BOLD, "bold", Modifier::BOLD),
            (Attrs::UNDERLINE, "underline", Modifier::UNDERLINED),
            (Attrs::ITALIC, "italic", Modifier::ITALIC),
            (Attrs::STRIKETHROUGH, "strikethrough", Modifier::CROSSED_OUT),
            (Attrs::DIM, "dim", Modifier::DIM),
            (Attrs::REVERSED, "reversed", Modifier::REVERSED),
        ];

        /// One attribute by its written name.
        pub fn named(word: &str) -> Option<Attrs> {
            Self::SPELLINGS
                .iter()
                .find(|(_, n, _)| *n == word)
                .map(|(a, _, _)| *a)
        }

        /// Several at once, by name. Words the grammar does not know are
        /// dropped — see [`Ink::parse`] on why reading stays forgiving while
        /// writing cannot go wrong.
        pub fn all_named<'a>(words: impl IntoIterator<Item = &'a str>) -> Attrs {
            words
                .into_iter()
                .filter_map(Attrs::named)
                .fold(Attrs::NONE, |a, b| a | b)
        }

        pub fn contains(self, other: Attrs) -> bool {
            self.0 & other.0 == other.0
        }

        /// What ratatui paints for these.
        pub fn modifier(self) -> Modifier {
            Self::SPELLINGS
                .iter()
                .filter(|(a, _, _)| self.contains(*a))
                .fold(Modifier::empty(), |m, (_, _, r)| m | *r)
        }

        /// The attributes a ratatui `Style` already carries, for content that
        /// arrives styled rather than named — a markdown span, a plugin's run.
        /// Modifiers the grammar cannot spell are dropped, which is the same
        /// answer as writing a name it cannot read back.
        pub fn from_modifier(m: Modifier) -> Attrs {
            Self::SPELLINGS
                .iter()
                .filter(|(_, _, r)| m.contains(*r))
                .fold(Attrs::NONE, |acc, (a, _, _)| acc | *a)
        }
    }

    impl std::ops::BitOr for Attrs {
        type Output = Attrs;
        fn bitor(self, rhs: Attrs) -> Attrs {
            Attrs(self.0 | rhs.0)
        }
    }

    impl fmt::Display for Attrs {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for (a, name, _) in Attrs::SPELLINGS {
                if self.contains(a) {
                    write!(f, "+{name}")?;
                }
            }
            Ok(())
        }
    }

    /// A cell's appearance, as the shell states it: two paints and the
    /// attributes on top.
    ///
    /// **This is the value; the string is how it travels.** `fresh-ui` carries
    /// one opaque [`fresh_ui::ThemeKey`] per item and never interprets it, so
    /// an `Ink` is written into that slot by [`fmt::Display`] and read back by
    /// [`Ink::parse`]. Building one cannot produce a name the grammar cannot
    /// read: a half is a key or a colour, an attribute is one of five, and
    /// "the same background, a different foreground" is a field assignment
    /// rather than surgery on a sentence.
    ///
    /// That surgery is what this replaced. Three call sites split a name on
    /// `/`, split the tail on `+`, and reassembled it — and they did not agree:
    /// swapping a background kept the attributes while setting attributes
    /// dropped them, in the same function.
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Ink {
        pub fg: Paint,
        pub bg: Paint,
        pub attrs: Attrs,
    }

    impl Ink {
        pub fn new(fg: Paint, bg: Paint) -> Ink {
            Ink {
                fg,
                bg,
                attrs: Attrs::NONE,
            }
        }

        /// The common case: both halves are theme keys.
        pub fn keys(fg: impl Into<Cow<'static, str>>, bg: impl Into<Cow<'static, str>>) -> Ink {
            Ink::new(Paint::key(fg), Paint::key(bg))
        }

        /// The same background, a different foreground.
        ///
        /// A ratatui `Style` with only `fg` set leaves the cell's background
        /// alone. That is how the explorer's caret sits *on* the selection
        /// highlight rather than punching a hole in it. An `Item` carries one
        /// theme name and the fold always writes both halves, so "keep the
        /// background" cannot be left unsaid: it is this.
        pub fn with_fg(mut self, fg: Paint) -> Ink {
            self.fg = fg;
            self
        }

        /// The companion: the same foreground, a different background.
        pub fn with_bg(mut self, bg: Paint) -> Ink {
            self.bg = bg;
            self
        }

        /// Add attributes to whatever this already carries.
        pub fn plus(mut self, attrs: Attrs) -> Ink {
            self.attrs = self.attrs | attrs;
            self
        }

        /// Replace the attributes outright.
        pub fn with_attrs(mut self, attrs: Attrs) -> Ink {
            self.attrs = attrs;
            self
        }

        /// The two halves as *names*. A half that is a literal has no name by
        /// construction — that is what a literal is — and reports `None`.
        ///
        /// This is the theme inspector's provenance, read out of the value
        /// rather than carried beside it.
        pub fn names(&self) -> (Option<&str>, Option<&str>) {
            (self.fg.name(), self.bg.name())
        }

        /// What ratatui paints for this.
        ///
        /// The attribute the theme declares for the foreground key composes
        /// with the structural ones the ink asked for.
        pub fn style(&self, theme: &Theme) -> Option<Style> {
            let (fg, bg) = (self.fg.color(theme)?, self.bg.color(theme)?);
            let declared = match self.fg.name() {
                Some(k) => theme.resolve_modifier_key(k),
                None => Modifier::empty(),
            };
            Some(
                Style::default()
                    .fg(fg)
                    .bg(bg)
                    .add_modifier(self.attrs.modifier() | declared),
            )
        }

        /// Read the written form back.
        ///
        /// **Forgiving where writing is not.** A word after `+` that the
        /// grammar does not know is dropped rather than failing the whole name,
        /// because the alternative — falling back to the editor's plain ground
        /// — turns a typo in one attribute into a surface painted in the wrong
        /// colours entirely. Nothing can *write* such a word: [`Attrs`] has
        /// five constants and no other constructor.
        pub fn parse(name: &str) -> Option<Ink> {
            let mut words = name.split('+');
            let pair = words.next()?;
            let (fg, bg) = pair.split_once('/')?;
            Some(Ink {
                fg: Paint::parse(fg)?,
                bg: Paint::parse(bg)?,
                attrs: Attrs::all_named(words),
            })
        }
    }

    impl fmt::Display for Ink {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}/{}{}", self.fg, self.bg, self.attrs)
        }
    }

    /// Build a name from two theme keys.
    ///
    /// The string form of [`Ink::keys`], for a caller that has a borrowed key
    /// rather than a `'static` one.
    pub fn pair(fg: &str, bg: &str) -> String {
        format!("{fg}/{bg}")
    }

    /// The same, with text attributes the theme does not carry.
    pub fn attrs(fg: &str, bg: &str, attrs: &[&str]) -> String {
        Ink {
            fg: Paint::Key(Cow::Owned(fg.to_string())),
            bg: Paint::Key(Cow::Owned(bg.to_string())),
            attrs: Attrs::all_named(attrs.iter().copied()),
        }
        .to_string()
    }

    /// Resolve a shell name to a concrete style.
    ///
    /// An unreadable or unknown name falls back to the editor's own ground
    /// rather than failing, so a surface that has not been themed yet renders
    /// plainly instead of not at all.
    ///
    /// **But not quietly.** The fallback is what a *release* build should do
    /// with a name it cannot read; a name that does not resolve is always a
    /// bug, and a silent one — the surface simply comes out in the editor's
    /// plain colours, which on most themes is close enough to the popup ground
    /// to look like nothing at all. Ten of them had accumulated across the
    /// migrated tree (`ui.selection_bg` and `ui.line_number_fg` among them,
    /// whose fields live under `editor`, and four `ui.diagnostic_*` whose
    /// section is `diagnostic`); the settings tree's unfocused cursor was one,
    /// and it had simply stopped being drawn. Every test run now says so.
    pub fn resolve(name: &str, theme: &Theme) -> Style {
        let Some(ink) = Ink::parse(name) else {
            // Not a name at all — an empty theme, or a literal the grammar
            // cannot read. The forgiving path, as documented.
            return base(theme);
        };
        debug_assert!(
            [ink.fg.declared(), ink.bg.declared()]
                .into_iter()
                .flatten()
                .all(|k| theme.resolve_theme_key(k).is_some()),
            "shell theme name {name:?} names a key that is not one: \
             `Theme::resolve_theme_key` does not know it, so the whole \
             surface falls back to the editor's plain ground"
        );
        ink.style(theme).unwrap_or_else(|| base(theme))
    }

    /// The two halves of a written name, where each is a *name* rather than a
    /// literal. [`Ink::names`] on a parsed name.
    pub fn names(theme: &str) -> (Option<String>, Option<String>) {
        let Some(ink) = Ink::parse(theme) else {
            return (None, None);
        };
        let (fg, bg) = ink.names();
        (fg.map(str::to_string), bg.map(str::to_string))
    }

    /// A concrete colour as a name, for the literal case above.
    pub fn literal(c: Color) -> String {
        Paint::Lit(c).to_string()
    }

    fn base(theme: &Theme) -> Style {
        Style::default().fg(theme.editor_fg).bg(theme.editor_bg)
    }
}

#[cfg(test)]
mod shell_theme_tests {
    use super::shell_theme::{literal, names, pair, resolve, Attrs, Ink, Paint};
    use ratatui::style::Color;

    fn theme() -> crate::view::theme::Theme {
        crate::view::theme::Theme::from_json(r#"{"name":"test"}"#)
            .expect("a theme of nothing but defaults")
    }

    /// **A block caret is an attribute the grammar has to carry.** A form
    /// control on a modal overlay draws its caret as one reverse-video cell —
    /// there is no hardware cursor to place there — and a word the grammar
    /// does not know is dropped on the way in, so the caret simply did not
    /// appear. Reading is forgiving on purpose; that is exactly why the word
    /// has to exist.
    #[test]
    fn a_reversed_cell_survives_the_written_form_and_reaches_the_style() {
        let ink = Ink::keys("editor.fg", "editor.bg").plus(Attrs::REVERSED);
        let written = ink.to_string();
        assert!(written.ends_with("+reversed"), "{written:?}");
        assert_eq!(Ink::parse(&written), Some(ink));
        let style = resolve(&written, &theme());
        assert!(
            style
                .add_modifier
                .contains(ratatui::style::Modifier::REVERSED),
            "the caret's cell reverses: {style:?}"
        );
    }

    /// **What is written is what is read.** The name is a serialisation, so
    /// the only thing that makes it safe to keep passing strings through
    /// `fresh-ui` is that the round trip is lossless — including for the parts
    /// the string form used to lose.
    #[test]
    fn an_ink_survives_the_written_form() {
        for ink in [
            Ink::keys("editor.fg", "editor.bg"),
            Ink::keys("editor.fg", "editor.bg").plus(Attrs::BOLD | Attrs::DIM),
            Ink::new(
                Paint::Lit(Color::Rgb(126, 231, 135)),
                Paint::key("editor.bg"),
            ),
            Ink::new(Paint::key("editor.fg"), Paint::Lit(Color::Indexed(42)))
                .plus(Attrs::UNDERLINE),
            Ink::new(Paint::Lit(Color::Yellow), Paint::Lit(Color::Black))
                .plus(Attrs::ITALIC | Attrs::STRIKETHROUGH),
            Ink::keys("editor.fg", "editor.bg").plus(Attrs::REVERSED),
        ] {
            let written = ink.to_string();
            assert_eq!(
                Ink::parse(&written),
                Some(ink.clone()),
                "{written:?} did not read back"
            );
        }
    }

    /// **Swapping one half leaves the other alone — attributes included.**
    ///
    /// This is the divergence the type exists to remove. The string form had
    /// two spellings of "layer something over this name" and they disagreed:
    /// swapping a background re-spliced the `+attrs` tail back on while setting
    /// attributes dropped it, so a plugin span that named both a background and
    /// an attribute silently un-dimmed a disabled suggestion row.
    #[test]
    fn layering_over_an_ink_keeps_what_it_does_not_mention() {
        let row = Ink::keys("ui.suggestion_fg", "ui.suggestion_bg").plus(Attrs::DIM);
        let both = row
            .clone()
            .with_bg(Paint::key("ui.menu_hover_bg"))
            .plus(Attrs::BOLD);
        assert_eq!(both.fg, row.fg, "the foreground was not mentioned");
        assert!(both.attrs.contains(Attrs::DIM), "the row's dim survived");
        assert!(both.attrs.contains(Attrs::BOLD), "the span's bold applied");
    }

    /// A word the grammar does not know is dropped rather than failing the
    /// whole name: the alternative turns one typo into a surface painted in
    /// the editor's plain ground. Nothing can *write* such a word — [`Attrs`]
    /// has five constants and no other constructor — so this is the reading
    /// half being forgiving, not the writing half being loose.
    #[test]
    fn an_unknown_attribute_is_dropped_not_fatal() {
        let ink = Ink::parse("editor.fg/editor.bg+bold+wobble").expect("the pair is readable");
        assert_eq!(ink.attrs, Attrs::BOLD);
        assert_eq!(
            resolve("editor.fg/editor.bg+wobble", &theme()).fg,
            Some(theme().editor_fg)
        );
    }

    /// **A plugin's key that the theme does not know leaves the rest of the
    /// run alone.** `Ink::style` is all-or-nothing, so before `Paint::Asked`
    /// existed one such name — `git_history.ts` colours commit hashes
    /// `syntax.number`, which no theme has ever had — dropped the whole run to
    /// the editor's plain ground, and tripped `resolve`'s assertion on the way
    /// past. The painter's behaviour was to leave the row's own foreground in
    /// place, and that is what this reproduces.
    #[test]
    fn a_plugin_key_the_theme_does_not_know_falls_back_to_what_was_under_it() {
        let t = theme();
        let asked = |k: &str| {
            Ink::new(
                Paint::asked(k.to_string(), Paint::key("ui.suggestion_fg")),
                Paint::key("ui.suggestion_bg"),
            )
        };
        let unknown = asked("syntax.number");
        let style = resolve(&unknown.to_string(), &t);
        assert_eq!(
            style.fg,
            Some(t.suggestion_fg),
            "an unknown plugin key leaves the row's own foreground"
        );
        assert_eq!(
            style.bg,
            Some(t.suggestion_bg),
            "and does not take the background down with it"
        );

        // One the theme *does* know still wins over what is under it.
        let known = asked("syntax.keyword");
        assert_eq!(resolve(&known.to_string(), &t).fg, Some(t.syntax_keyword));

        // And the whole thing survives the written form, fallback included.
        for ink in [unknown, known] {
            let written = ink.to_string();
            assert_eq!(
                Ink::parse(&written),
                Some(ink),
                "{written:?} did not read back"
            );
        }
    }

    /// A literal has no name by construction, and the inspector should say so
    /// rather than attributing a plugin's colour to a theme entry.
    #[test]
    fn a_literal_half_reports_no_name() {
        let ink = Ink::new(Paint::Lit(Color::Rgb(1, 2, 3)), Paint::key("editor.bg"));
        assert_eq!(ink.names(), (None, Some("editor.bg")));
        let (fg, bg) = names(&ink.to_string());
        assert_eq!((fg, bg), (None, Some("editor.bg".to_string())));
    }

    /// **Every colour round-trips.** The literal form exists because a plugin's
    /// colour arrives already resolved, with no key to name it; it is only
    /// honest if it loses nothing.
    ///
    /// It did lose something. An earlier version answered `editor.fg` for
    /// anything that was not `Color::Rgb`, and theme colours are frequently one
    /// of the sixteen names — `file_status_modified_fg` is `Yellow` in the
    /// built-in dark theme — so every plugin-decorated row in the file explorer
    /// silently painted in the panel's ordinary ink instead of its status
    /// colour. Nothing failed; it just looked undecorated.
    #[test]
    fn a_literal_colour_survives_the_round_trip() {
        let theme = crate::view::theme::Theme::from_json(r#"{"name":"test"}"#)
            .expect("a theme of nothing but defaults");
        for c in [
            Color::Rgb(126, 231, 135),
            Color::Rgb(0, 0, 0),
            Color::Yellow,
            Color::LightMagenta,
            Color::Black,
            Color::White,
            Color::Reset,
            Color::Indexed(0),
            Color::Indexed(42),
            Color::Indexed(255),
        ] {
            let style = resolve(&pair(&literal(c), "editor.bg"), &theme);
            assert_eq!(style.fg, Some(c), "{c:?} did not survive {:?}", literal(c));
        }
    }

    /// A literal composes with the rest of the grammar, so a plugin colour can
    /// still be bold or underlined.
    #[test]
    fn a_literal_composes_with_attributes() {
        use ratatui::style::Modifier;
        let theme = crate::view::theme::Theme::from_json(r#"{"name":"test"}"#)
            .expect("a theme of nothing but defaults");
        let style = resolve("#7ee787/editor.bg+bold", &theme);
        assert_eq!(style.fg, Some(Color::Rgb(126, 231, 135)));
        assert!(style.add_modifier.contains(Modifier::BOLD));
    }

    /// A malformed literal falls back to the editor's ground rather than to a
    /// colour nobody asked for.
    #[test]
    fn a_malformed_literal_falls_back() {
        let theme = crate::view::theme::Theme::from_json(r#"{"name":"test"}"#)
            .expect("a theme of nothing but defaults");
        for bad in [
            "#zzzzzz/editor.bg",
            "#12345/editor.bg",
            "#NotAColour/editor.bg",
        ] {
            let style = resolve(bad, &theme);
            assert_eq!(style.fg, Some(theme.editor_fg), "{bad}");
        }
    }
}

impl Editor {
    /// Snapshot the colours the shell's themes resolve to this frame.
    pub(crate) fn shell_palette(&self) -> ShellPalette {
        ShellPalette {
            theme: self.theme.read().unwrap().clone().into(),
        }
    }
}

/// What a press on a settings *card* means.
///
/// **The translation table already existed; it was written in geometry.**
/// Every `hit_rect(&out, "<kind>", "<event>", …)` call in `render_control`
/// paired a widget hit with the `ControlLayoutInfo` field it filled, and every
/// arm of `SettingsLayout::hit_test` turned that field back into a
/// `SettingsHit` by comparing a cell against it. The rectangle in the middle
/// is what goes: the runtime already names the kind and the event, the row
/// index rides in the payload, and the control's key names the item. So this
/// is the same table with the geometry taken out of it.
///
/// The answer goes to `dispatch_settings_hit`, which is the body the web's
/// `/settings` route calls by name — so a click still does the same thing in
/// both frontends.
impl Editor {
    pub(crate) fn settings_widget_hit(
        &mut self,
        hit: &crate::widgets::WidgetEvent,
        byte: Option<usize>,
        clicks: u8,
    ) {
        use crate::view::settings::items::SettingControl;
        use crate::view::settings::SettingsHit;

        // A list row's own key is the row's; the list it belongs to is the
        // owner, and that is the one keyed by the control's path.
        let key = hit.owner_key.as_deref().unwrap_or(hit.widget_key.as_str());
        // `{path}` for a scalar, `{path}::list` / `{path}::add` for the two
        // lists a composite control owns.
        let (path, part) = match key.split_once("::") {
            Some((p, s)) => (p, s),
            None => (key, ""),
        };
        let Some(state) = self.settings_state.as_ref() else {
            return;
        };
        let Some(page) = state.pages.get(state.selected_category) else {
            return;
        };
        let Some(idx) = page.items.iter().position(|i| i.path == path) else {
            return;
        };
        let row = || {
            hit.payload
                .get("index")
                .and_then(|v| v.as_u64())
                .unwrap_or(0) as usize
        };
        let resolved = match (hit.widget_kind, hit.event_type) {
            ("toggle", _) => SettingsHit::ControlToggle(idx),
            ("number", "number_value") => SettingsHit::ControlNumberValue(idx),
            ("dropdown", "dropdown_select") => SettingsHit::ControlDropdownOption(idx, row()),
            ("dropdown", _) => SettingsHit::ControlDropdown(idx),
            // A text list's fields are keyed by row: an item's, or the add
            // row's — `usize::MAX` there, a row past every item.
            ("text", _) => match crate::view::settings::live::text_list::row_of(key) {
                Some(Some(i)) => SettingsHit::ControlTextListRow(idx, i),
                Some(None) => SettingsHit::ControlTextListRow(idx, usize::MAX),
                None => SettingsHit::ControlText(idx),
            },
            // An item's `[x]`.
            ("button", _) => match part.strip_prefix("remove::").and_then(|i| i.parse().ok()) {
                Some(i) => SettingsHit::ControlTextListRemove(idx, i),
                None => SettingsHit::Item(idx),
            },
            // A row of a map's or an object array's list: an entry, or the
            // add row last, which a map names apart.
            ("list", _) => {
                let r = row();
                match (&page.items[idx].control, page.items[idx].control.add_row()) {
                    (SettingControl::Map { .. }, Some(add)) if add == r => {
                        SettingsHit::ControlMapAddNew(idx)
                    }
                    _ => SettingsHit::ControlMapRow(idx, r),
                }
            }
            ("dual_list", _) => match hit.payload.get("column").and_then(|v| v.as_str()) {
                Some("included") => SettingsHit::ControlDualListIncluded(idx, row()),
                _ => SettingsHit::ControlDualListAvailable(idx, row()),
            },
            // Anything else on a card is a press on the card: select it.
            _ => SettingsHit::Item(idx),
        };
        // A press on a text field also says *where* in the value the caret
        // goes (#2573). The press reports its byte in the field's own row;
        // the event's payload carries the breadcrumbs that undo the field's
        // layout, measured from that same row start.
        //
        // **This used to render the control a second time and measure the row
        // it produced**, because a column cannot be turned into a byte without
        // laying the text out — and the width it had to be laid out at was a
        // rectangle read back off the tree, with a comment explaining that a
        // byte resolved at any other width is not the byte under the pointer.
        // A byte needs none of that: the shaping that drew the row is the one
        // that answered.
        let caret = match resolved {
            SettingsHit::ControlText(_) | SettingsHit::ControlTextListRow(..) => {
                byte.and_then(|b| crate::widgets::value_byte_from_hit(hit, b))
            }
            _ => None,
        };

        // A map row activates on a *double* click and its add row on a
        // single one (#604), so the press's own doubleness travels with it —
        // the same bit `handle_settings_mouse` was handed. It rides on the
        // fact rather than being fetched back off the editor: the node that
        // saw the press is the one that knows, and `List`'s activation
        // handler is given its `Event` for exactly this.
        let dbl = clicks >= 2;
        self.dispatch_settings_hit(resolved, dbl);
        if let Some(byte) = caret {
            if let Some(s) = self.settings_state.as_mut() {
                s.position_text_cursor(byte);
            }
        }
    }
}

/// What a press on the settings dialog's category tree means.
///
/// **Three bodies, two callers.** The TUI reaches them through the tree's own
/// nodes (`UiFact::SettingsCategory` and its two siblings); the web's native
/// settings projection reaches them by name, through `webui::apply_settings`.
/// They were arms of `handle_settings_mouse` keyed on a `SettingsHit` the
/// painter's rectangles produced, so deleting those rectangles would have
/// taken the web's path with them.
impl Editor {
    pub(crate) fn settings_select_category(&mut self, idx: usize) {
        use crate::view::settings::state::FocusTarget;
        if let Some(s) = self.settings_state.as_mut() {
            s.focus_on(FocusTarget::Categories);
            s.selected_category = idx;
            s.selected_item = 0;
            s.body_anchor.scroll_to(fresh_ui::Point::ZERO);
            // A click lands the cursor on the category row itself, even after
            // auto-expand reveals its sections — which is where keyboard
            // Up/Down arrives too.
            s.tree_cursor_section = None;
            s.auto_expand_current_category();
        }
    }

    pub(crate) fn settings_jump_to_section(&mut self, cat: usize, section: usize) {
        use crate::view::settings::state::FocusTarget;
        if let Some(s) = self.settings_state.as_mut() {
            s.jump_to_section(cat, section);
            // `jump_to_section` also serves search and the keyboard, where
            // moving focus to the body is right; a click in the tree keeps
            // the tree focused.
            s.focus_on(FocusTarget::Categories);
        }
    }

    pub(crate) fn settings_toggle_category(&mut self, idx: usize) {
        if let Some(s) = self.settings_state.as_mut() {
            s.toggle_category_expanded(idx);
        }
    }
}

impl Editor {
    /// Offer an input to the shell's tree before the legacy path sees it.
    ///
    /// The first of the three stages S1 describes: the legacy modal-capture
    /// band still runs ahead of everything, the shell is offered the event
    /// next, and the existing walk remains the floor. Returns whether the tree
    /// claimed it.
    ///
    /// Only migrated surfaces carry handlers — every region is still a `Host`
    /// leaf standing in for a painter that has not moved — so anything the
    /// tree declines reaches the legacy path exactly as before. A surface
    /// starts taking its own input the moment it stops being a `Host`.
    pub(crate) fn shell_dispatch(&mut self, input: fresh_ui::Input) -> Dispatched {
        // **The previous frame's settle is applied before this input is
        // routed, never after it.**
        //
        // A settle's facts are produced by `ui.frame` and left in
        // `Ui::pending_messages`; the only drain is here. Taken *after*
        // `dispatch`, they are applied after everything this key decided —
        // so a focus the key just moved is overwritten by where focus was
        // one frame ago. The dock's `/` was exactly that: the applier moved
        // the panel's focus to the filter, and the mount frame's pending
        // `WidgetFocus { sessions }` landed on top of it in the same
        // `apply_shell_messages` loop, so every character typed after it was
        // routed to the session list and the filter never filtered.
        //
        // Applied first, they say what was true when the key arrived, which
        // is what the routers below read (`router::WidgetPanelView`'s
        // `focus_key` is the registry's, and the registry is the tree's
        // mirror). What `dispatch` itself queues is still drained below and
        // applied with the messages it routed.
        self.apply_settled_shell_messages();
        // **And the tree is brought up to the facts before the input is
        // routed over it.** A panel write since the last frame — a focus the
        // host decided, a spec a plugin pushed — is carried to the tree by a
        // frame, and a key that arrives before that frame would otherwise be
        // resolved from where focus *was*. See
        // `Editor::shell_description_stale`.
        self.lay_out_shell_if_stale();
        self.apply_settled_shell_messages();
        let Some(mut ui) = self.shell_ui.take() else {
            return Dispatched::default();
        };
        // Snapshotted before a single message is applied. See [`EventFacts`].
        let facts = EventFacts {
            menu_open_before: self.menu_state.active_menu,
            at: input
                .position()
                .map(|p| (p.x.max(0) as u16, p.y.max(0) as u16))
                .unwrap_or_default(),
        };
        let result = ui.dispatch(input);
        // **What this dispatch itself queued, drained before `needs_frame` is
        // asked.**
        //
        // `dispatch` returns what handlers produced while routing. What
        // `apply_autofocus` decides when it *settles* focus — a scope opening,
        // a focused element going away — goes into `Ui::pending_messages`
        // instead, and until this pair of drains nothing in the editor ever
        // took it. Two costs, both live:
        //
        // 1. A focus change the tree decided never reached the host, so the
        //    plugin's `focus` event did not fire for it and the registry's
        //    focus key silently diverged from the tree's.
        // 2. `needs_frame()` is `true` while that queue is non-empty
        //    (`fresh-ui/src/schedule.rs`), and `tree_stale` below reads it. So
        //    one settle left the editor reporting "changed" for every input
        //    event thereafter — repainting unconditionally, forever, which is
        //    exactly what the comment below sets out to avoid. Anything
        //    waiting for the frame to go quiet waited for good;
        //    `dock_pointer_at_rest_requests_no_frame` is that test.
        //
        // Applied with the routed messages below rather than dropped, because
        // these are facts the host is supposed to act on. What a *frame* left
        // pending was taken above, before routing; what is here is this
        // dispatch's own — a handler that asked for focus imperatively.
        let settled = ui.take_messages();
        // **A change is not always a message.** A widget that keeps its own
        // hover — every `List` and `Tree` — writes `hovered` through an
        // updater and produces nothing, precisely so the host is not bothered
        // with it; `dispatch` does not rebuild, so the write is sitting in the
        // scheduler waiting for the next frame that never got asked for. The
        // completion popup was the symptom: its rows never lit under the
        // pointer while the menu bar's did, because the menu bar's hover is a
        // `UiFact` and a `List`'s is not. `needs_frame` is the library's own
        // answer to "is the frame stale" — a dirty element, a queued mutation,
        // a behavior with something to deliver — and it is the right one to
        // ask here.
        let tree_stale = ui.needs_frame();
        self.shell_ui = Some(ui);
        // Claimed is reported, not inferred. Producing a message and taking
        // the event are different things: a hover moves a highlight without
        // claiming the pointer, and a dismissal closes a menu while leaving a
        // right-click to go on and open the next one.
        let claimed = result.claimed;
        // **Claiming and changing are different things**, and both answers are
        // needed. A hover moves a highlight without claiming — the event goes
        // on to the plugin `mouse_move` hook, the terminal-link tracker and
        // the LSP hover probe — and the frame it changed still has to be
        // drawn. That second half went missing with the pointer walk:
        // `update_hover_target` used to return "the target moved, redraw" and
        // nothing replaced it, so every hover the tree owns — the menu bar's
        // labels, the explorer's rows, the status bar's segments, a
        // separator, a tab — restyled a frame nobody asked for.
        //
        // A message is *a* change — a `UiFact` exists to be reacted to — but
        // not the only one: see `tree_stale` above. A motion that crosses no
        // element boundary produces neither, which is what still keeps an idle
        // pointer from drawing a frame.
        let changed = tree_stale || !result.msgs.is_empty();
        let mut msgs = result.msgs;
        msgs.extend(settled);
        self.apply_shell_messages(msgs, facts);
        // **The claim is the tree's word, and only the tree's.** A seam that
        // hands a key to a host interior — the prompt's, a focused panel's —
        // `stop()`s it, because the key *is* that surface's: what the surface
        // does with a key it does not bind is its own business, and for
        // both that business is handing it on to the editor's own keyboard
        // (`Editor::hand_key_to_editor`, from the applier). There is no
        // second verdict folded in after the fact; the `Option<bool>` that
        // used to carry one is gone (L2).
        Dispatched { claimed, changed }
    }

    /// A key a surface holding the keyboard does not bind is still the
    /// editor's: the file browser's `Alt+H` and quick-open's `Ctrl+P` reach
    /// their bindings from inside the prompt, and a shortcut a plugin panel
    /// does not bind blurs the dock and falls through. The surface's seam
    /// claimed the key in the tree; this is what the surface does with it.
    pub(crate) fn hand_key_to_editor(&mut self, ev: crossterm::event::KeyEvent) {
        if let Err(e) = self.dispatch_base_key(ev.code, ev.modifiers) {
            tracing::warn!("key handed on from a keyboard seam failed: {e}");
        }
    }

    /// Apply what the tree decided on its own since the last input: the
    /// facts `apply_autofocus` leaves in `Ui::pending_messages` when a frame
    /// settles focus.
    ///
    /// **They are the tree's, and they describe the frame that produced them
    /// — so they have to be applied before the next input is routed against
    /// them.** Nothing else drains that queue: `Ui::dispatch` returns only
    /// what handlers produced while routing, and a settle happens during
    /// `Ui::frame`, with no input in hand.
    ///
    /// Default `EventFacts` for the same reason
    /// `Editor::advance_panel_focus_in_tree` uses them: they describe the
    /// pointer event a message was produced *by*, and a settle has none.
    ///
    /// The tree is put back before the facts are applied, because an applier
    /// may reach for it (`advance_panel_focus_in_tree` asks the tree whether
    /// it is holding a panel's focus, and answers "no ring" if it is out).
    pub(super) fn apply_settled_shell_messages(&mut self) {
        let Some(mut ui) = self.shell_ui.take() else {
            return;
        };
        let settled = ui.take_messages();
        self.shell_ui = Some(ui);
        if !settled.is_empty() {
            self.apply_shell_messages(settled, Default::default());
        }
    }

    /// Apply what the tree produced.
    ///
    /// Split out of [`Self::shell_dispatch`] because a dispatch is not the
    /// only thing that produces messages: `Ui::take_messages` carries the ones
    /// framework-initiated activity raises — a focus change asked for
    /// imperatively, which is how a plugin's `FocusAdvance` reaches the tree's
    /// ring (`Editor::advance_panel_focus_in_tree`). One loop, so a fact
    /// cannot mean one thing when a key produced it and another when the host
    /// did.
    pub(super) fn apply_shell_messages(
        &mut self,
        msgs: Vec<crate::view::shell::msg::UiMsg>,
        facts: EventFacts,
    ) {
        // A message is a change to something the description reads — that
        // is what a `UiFact` is for — so the description is stale once one
        // has been applied, and the next reader lays it out again. Except
        // the pointer's transient facts: a hover, a wheel, a grip's drag
        // change nothing the next input's routing reads, and marking them
        // stale cost a layout per motion report (the geometry pass counts
        // them: `a_divider_drag_that_moves_nothing_lays_out_nothing`).
        if msgs.iter().any(|m| !m.is_pointer_transient()) {
            self.shell_description_stale = true;
        }
        for msg in msgs {
            match msg {
                crate::view::shell::msg::UiMsg::Action(action) => {
                    // Straight into the pipeline that has always applied
                    // actions; nothing about it changes.
                    if let Err(e) = self.handle_action(action.clone()) {
                        tracing::warn!("shell action {action:?} failed: {e}");
                    }
                }
                crate::view::shell::msg::UiMsg::Ui(fact) => self.apply_ui_fact(fact, facts),
            }
        }
    }

    /// Whether a wheel notch over a pane's content was taken by a live
    /// terminal there rather than scrolling the pane.
    ///
    /// The same gate the content's press asks (`pane_content_takes_pointer`),
    /// which is where the ruling lives; a notch simply has nowhere else to go
    /// once the PTY has it.
    fn pane_content_took_wheel(&mut self, x: u16, y: u16) -> bool {
        let Some((ev, _)) = self.shell_pointer_event else {
            return false;
        };
        match self.pane_content_takes_pointer(x, y, ev) {
            Some(Err(e)) => {
                tracing::warn!("terminal wheel forward failed: {e}");
                true
            }
            Some(Ok(_)) => true,
            None => false,
        }
    }

    /// Apply a positional fact — the half of a message that never becomes a
    /// keybinding.
    fn apply_ui_fact(&mut self, fact: crate::view::shell::msg::UiFact, ev: EventFacts) {
        use crate::view::shell::msg::UiFact;
        match fact {
            // The tree found the widget; the dispatch behind this is the one
            // all three frontends already share, and it does not change.
            // `None` for the clicked byte: the byte range in the hit is a
            // payload now, not a position the caller resolved.
            UiFact::WidgetHit {
                slot,
                event: hit,
                byte,
                clicks,
            } => {
                // **The byte the press landed on, and nothing is done to it.**
                // A described widget is its own node, so the piece the press
                // sits on begins where the widget's row does and `byte` is
                // already in the coordinate space `deliver_widget_hit` wants.
                //
                // Two arithmetics have stood here. The first added a *column*
                // to the recorded `byte_start`, which agrees with a byte only
                // while every character is one byte and one cell — so a
                // localized label or a non-ASCII value put the caret in the
                // wrong place. The second added the *byte* to `byte_start`
                // and the dispatch subtracted it straight back off: correct,
                // but a round trip through the text projection's rows, which
                // this surface does not have. The event carries no
                // `byte_start` now, so neither is stateable.
                let clicked_byte = byte;
                let slot = match slot {
                    crate::view::shell::widgets::Slot::Dock => crate::app::PanelSlot::Dock,
                    crate::view::shell::widgets::Slot::Floating => crate::app::PanelSlot::Floating,
                    crate::view::shell::widgets::Slot::Sidebar(i) => {
                        crate::app::PanelSlot::Sidebar(i)
                    }
                    // Not a plugin panel: the same `WidgetSpec`s, whose hits
                    // are settings actions rather than a plugin's
                    // `widget_event`.
                    crate::view::shell::widgets::Slot::Settings => {
                        self.settings_widget_hit(&hit, byte, clicks);
                        return;
                    }
                    // The same, one surface in: an entry dialog's fields are
                    // its own, not the page's.
                    crate::view::shell::widgets::Slot::SettingsEntry => {
                        self.settings_entry_widget_hit(&hit, byte, clicks);
                        return;
                    }
                    // The overlay prompt's toolbar: the plugin's panel
                    // `PROMPT_TOOLBAR_PANEL_ID`, and the same dispatch — a
                    // press focuses the control and the kind answers it.
                    crate::view::shell::widgets::Slot::PromptToolbar => {
                        if let Some(panel_key) = self.prompt_toolbar_key() {
                            self.deliver_widget_hit(&panel_key, &hit, clicked_byte);
                        }
                        return;
                    }
                    // A panel mounted into a pane's buffer. It is a plugin
                    // panel like the two above, and the only difference is
                    // where its key comes from: a pane is one buffer, and a
                    // buffer names its panel.
                    crate::view::shell::widgets::Slot::Pane(pane) => {
                        // The focus half of the press, which a widget's own
                        // hit swallows: a `hit_node` press calls `e.stop()`,
                        // so the pane's own surface never sees it and the
                        // keyboard would stay wherever it was. A press no
                        // widget claims reaches that surface and arrives as
                        // `PaneContentPress`, which focuses the pane itself.
                        self.focus_pane(pane);
                        if let Some(panel_key) = self.pane_panel_key(pane) {
                            self.deliver_widget_hit(&panel_key, &hit, clicked_byte);
                        }
                        return;
                    }
                };
                if let Some(panel_key) = self.panel(slot).map(|p| p.panel_key.clone()) {
                    self.deliver_widget_hit(&panel_key, &hit, clicked_byte);
                }
            }
            UiFact::SettingsItem(idx) => {
                self.dispatch_settings_hit(crate::view::settings::SettingsHit::Item(idx), false);
            }
            // The pop-over's rows report their own hover, because nothing else
            // can: `update_widget_hover` probes the runtime's panel entries
            // and a pop-over is not among them. Stored on the panel beside the
            // hovered widget, which is where every other row-level hover
            // lives, and read back by the dropdown's renderer.
            UiFact::WidgetPopupDismiss { slot } => {
                use crate::view::shell::widgets::Slot;
                match slot {
                    Slot::Settings | Slot::SettingsEntry => {
                        if let Some(s) = self.settings_state.as_mut() {
                            s.dropdown_cancel();
                        }
                    }
                    // Closing it *is* toggling it: the runtime owns the open
                    // flag and `dropdown_toggle` is how every other surface
                    // flips it, so the dismissal goes through the same
                    // dispatch rather than reaching into the state map.
                    // A pane-mounted panel has no pop-over yet: its dropdown
                    // rows come with the rest of C.5's second step. The prompt
                    // toolbar's toggles and buttons raise none.
                    Slot::Pane(_) | Slot::PromptToolbar => {}
                    Slot::Dock | Slot::Floating | Slot::Sidebar(_) => {
                        let panel = match slot {
                            Slot::Dock => crate::app::PanelSlot::Dock,
                            Slot::Sidebar(i) => crate::app::PanelSlot::Sidebar(i),
                            _ => crate::app::PanelSlot::Floating,
                        };
                        // **Asked of the spec and the state, not of a field
                        // the last render left.** The dismissal needs one
                        // string — which dropdown is up — and
                        // `dropdown::open_key` answers it through the same
                        // `resolve` the description painted the list from, so
                        // the two cannot name different widgets.
                        let open = self.panel(panel).and_then(|p| {
                            let panel_key = p.panel_key.clone();
                            let st = self.widget_registry.get(&panel_key)?;
                            let key = crate::widgets::kinds::dropdown::open_key(
                                &st.spec,
                                &st.instance_states,
                                &st.focus_key,
                            )?;
                            (!key.is_empty()).then_some((panel_key, key))
                        });
                        if let Some((panel_key, widget_key)) = open {
                            let ev = crate::widgets::WidgetEvent {
                                row_target: false,
                                context_click: false,
                                widget_key,
                                widget_kind: "dropdown",
                                payload: serde_json::json!({}),
                                event_type: "dropdown_toggle",
                                owner_key: None,
                            };
                            self.deliver_widget_hit(&panel_key, &ev, None);
                        }
                    }
                }
            }
            // **The tree's answer replaces the probe's**, for a panel whose
            // interior the tree describes. Setting the memo is all of it: the
            // description reads `hovered_widget_key`/`hovered_item_key` when
            // it is next built, and the row renderers take the highlight from
            // there — where `update_widget_hover` had to ask the plugin to
            // re-render before the painter could show it.
            // **The tree's ring reporting a landing: one of the deciders of
            // the panel's focus fact, through the same door as every other.**
            //
            // The registry's key is the fact — one writer function
            // (`WidgetRegistry::decide_focus`), reached here for a landing the
            // tree's own traversal made (a Tab, a click), by
            // `set_panel_focus_and_notify` for the host's decisions, and by
            // the plugin's `SetFocusKey`. The tree is the fact's projection:
            // the description marks the widget the fact names, and the
            // library re-settles onto a mark that moved. So a landing the
            // fact already names is the echo of the tree following it, and
            // is a no-op here; a landing it does not name is the ring having
            // moved, and the fact follows.
            //
            // The plugin is told, exactly as `deliver_widget_hit`'s
            // click-to-focus told it, because a plugin that mirrors focus
            // cannot tell a click from a Tab and should not have to.
            UiFact::WidgetFocus { slot, widget } => {
                use crate::view::shell::widgets::Slot;
                let key = match slot {
                    Slot::Dock => self.panel(crate::app::PanelSlot::Dock),
                    Slot::Floating => self.panel(crate::app::PanelSlot::Floating),
                    Slot::Sidebar(i) => self.panel(crate::app::PanelSlot::Sidebar(i)),
                    _ => None,
                }
                .map(|p| p.panel_key.clone())
                // The prompt toolbar's landing, the same fact: a control on
                // the prompt's ring took the keyboard from the query input.
                .or_else(|| match slot {
                    Slot::PromptToolbar => self.prompt_toolbar_key(),
                    _ => None,
                });
                let Some(key) = key else {
                    return;
                };
                if self.widget_registry.focus_key(&key) == Some(widget.as_str()) {
                    return;
                }
                // **Through the same door every other focus move uses.** This
                // wrote `set_focus_key` directly while the comment above
                // claimed the plugin was told, and it was not: the plugin's
                // `focus` event and the kinds' own `on_focus_change` hook —
                // which is how a `Tree` keeps its selected row coherent with
                // focus — both hang off `set_panel_focus_and_notify`, and
                // neither ran for a focus the tree decided. Tab ran them and
                // a click did not.
                self.set_panel_focus_and_notify(&key, widget);
                self.rerender_widget_panel(&key);
            }
            UiFact::WidgetWheel {
                slot,
                widget,
                delta,
            } => {
                use crate::view::shell::widgets::Slot;
                // Only the two plugin-panel slots carry a runtime state map to
                // scroll. The settings dialogs describe their own `Text`s with
                // no completions behind them (`Ctx::plain`'s empty state map),
                // and a pane-mounted panel raises no float yet — the same
                // boundary `WidgetHover` draws, for the same reason.
                let panel = match slot {
                    Slot::Dock => crate::app::PanelSlot::Dock,
                    Slot::Floating => crate::app::PanelSlot::Floating,
                    Slot::Sidebar(i) => crate::app::PanelSlot::Sidebar(i),
                    _ => return,
                };
                let Some(key) = self.panel(panel).map(|p| p.panel_key.clone()) else {
                    return;
                };
                self.wheel_widget_by_key(&key, &widget, delta);
            }
            UiFact::WidgetHover {
                slot,
                widget,
                item,
                entered,
            } => {
                use crate::view::shell::widgets::Slot;
                // The prompt toolbar's memo is its registry entry's, the
                // way a pane's is; the leave rule below is the same.
                if slot == Slot::PromptToolbar {
                    let Some(key) = self.prompt_toolbar_key() else {
                        return;
                    };
                    let (w, i) = self.widget_registry.hover_keys(&key);
                    let next = match entered {
                        true => Some((widget, item)),
                        false if w == widget && i == item => Some((String::new(), String::new())),
                        false => None,
                    };
                    if let Some((w, i)) = next {
                        if self.widget_registry.set_hover_keys(&key, w, i) {
                            self.shell_description_stale = true;
                        }
                    }
                    return;
                }
                let panel = match slot {
                    Slot::Dock => crate::app::PanelSlot::Dock,
                    Slot::Floating => crate::app::PanelSlot::Floating,
                    // Gated at the source; nothing else has a panel memo. A
                    // pane-mounted panel has none *yet* — it never had one,
                    // because `update_widget_hover` only ever probed the two
                    // above — so its highlight comes with C.5's second step.
                    _ => return,
                };
                if let Some(p) = self.panel_mut(panel) {
                    match entered {
                        true => {
                            p.hovered_widget_key = widget;
                            p.hovered_item_key = item;
                        }
                        // Only if it is still the one being left. Enter and
                        // leave are per-node and leaves fire first, so a row
                        // handing the hover to the piece beside it would
                        // otherwise clear what the enter had just set.
                        false if p.hovered_widget_key == widget && p.hovered_item_key == item => {
                            p.hovered_widget_key.clear();
                            p.hovered_item_key.clear();
                        }
                        false => {}
                    }
                }
            }
            // The right press's second half, from a hit the node carried.
            //
            // The re-focus is first for the same reason it is in `DockFocus`
            // and `DockContext`: the un-blur fires a `focus` widget_event, and
            // any mirror of dock-focus state has to update before the menu the
            // press raises reads it.
            UiFact::WidgetContext {
                slot,
                event: hit,
                x,
                y,
            } => {
                use crate::view::shell::widgets::Slot;
                let panel = match slot {
                    Slot::Dock => crate::app::PanelSlot::Dock,
                    Slot::Floating => crate::app::PanelSlot::Floating,
                    Slot::Sidebar(i) => crate::app::PanelSlot::Sidebar(i),
                    // The settings dialog's rows raise no plugin menu.
                    _ => return,
                };
                if panel == crate::app::PanelSlot::Dock
                    && self.dock.as_ref().is_some_and(|f| !f.focused)
                {
                    self.refocus_floating_panel(crate::app::PanelSlot::Dock);
                }
                if let crate::app::PanelSlot::Sidebar(i) = panel {
                    self.focus_sidebar_section(i);
                }
                self.fire_widget_context(panel, &hit, x, y);
            }
            UiFact::WidgetPopupHover { slot, index } => {
                use crate::view::shell::widgets::Slot;
                let now = index.map(|i| i.to_string()).unwrap_or_default();
                match slot {
                    // The settings dialog renders its controls itself, with no
                    // panel behind them, so its pop-over's hover lives on the
                    // settings state beside the rest of its hover.
                    Slot::Settings | Slot::SettingsEntry => {
                        if let Some(s) = self.settings_state.as_mut() {
                            s.hovered_popup_row = now;
                        }
                    }
                    // As above: no pop-over on a pane-mounted panel yet, and
                    // none on the prompt toolbar.
                    Slot::Pane(_) | Slot::PromptToolbar => {}
                    Slot::Dock | Slot::Floating | Slot::Sidebar(_) => {
                        let panel = match slot {
                            Slot::Dock => crate::app::PanelSlot::Dock,
                            Slot::Sidebar(i) => crate::app::PanelSlot::Sidebar(i),
                            _ => crate::app::PanelSlot::Floating,
                        };
                        let panel_key = match self.panel(panel) {
                            Some(p) if p.hovered_popup_row != now => p.panel_key.clone(),
                            _ => return,
                        };
                        if let Some(p) = self.panel_mut(panel) {
                            p.hovered_popup_row = now;
                        }
                        self.rerender_widget_panel(&panel_key);
                    }
                }
            }
            UiFact::SettingsItemHover(idx) => {
                if let Some(s) = self.settings_state.as_mut() {
                    s.hover_hit = idx.map(crate::view::settings::SettingsHit::Item);
                }
            }
            UiFact::SettingsInherit(idx) => {
                self.dispatch_settings_hit(
                    crate::view::settings::SettingsHit::ControlInherit(idx),
                    false,
                );
            }
            UiFact::SettingsInheritHover(idx) => {
                if let Some(s) = self.settings_state.as_mut() {
                    s.hover_hit = Some(crate::view::settings::SettingsHit::ControlInherit(idx));
                }
            }
            UiFact::SettingsEntryItem(idx) => self.entry_dialog_select_item(idx),
            UiFact::SettingsEntryItemHover(idx) => {
                if let Some(d) = self
                    .settings_state
                    .as_mut()
                    .and_then(|s| s.entry_dialog_mut())
                {
                    d.hover_item = idx;
                }
            }
            UiFact::SettingsEntryButton(i) => {
                let kind = self
                    .settings_state
                    .as_ref()
                    .and_then(|s| s.entry_dialog())
                    .map(|d| Self::entry_button_kind(d, i));
                if let Some(kind) = kind {
                    self.entry_dialog_activate_button(kind);
                }
            }
            UiFact::SettingsEntryButtonHover(i) => {
                if let Some(d) = self
                    .settings_state
                    .as_mut()
                    .and_then(|s| s.entry_dialog_mut())
                {
                    d.hover_button = i;
                }
            }
            UiFact::SettingsEntryFieldAction(item, action) => {
                self.entry_dialog_field_action(item, action);
            }
            UiFact::SettingsSearchResult(idx) => {
                self.dispatch_settings_hit(
                    crate::view::settings::SettingsHit::SearchResult(idx),
                    false,
                );
            }
            UiFact::PanelClosed => {
                self.dismiss_floating_panel_with_cancel(crate::app::PanelSlot::Floating);
            }
            UiFact::StatusBarClicked(id) => {
                // The id→behaviour table is unchanged and stays where it is
                // (`chrome::status_bar`); what the tree replaced is finding
                // *which* element the pointer was over.
                if let Err(e) = self.dispatch_status_bar_click(id) {
                    tracing::warn!("status bar click failed: {e}");
                }
            }
            UiFact::StatusBarTokenClicked(key) => self.fire_status_bar_token_click(&key),
            // The tab strip. The strip is a node per pane; what is *inside* it
            // is the tab renderer's layout, hit-tested against what it
            // recorded — so these arms are the box handlers, minus the box.
            UiFact::PaneTabsPress { pane, x, y } => {
                // Only the tabs are left here. The two buttons drawn over the
                // right end of this row are nodes of their own, and a node
                // deeper on the hit path answers first — which is what the two
                // `LayoutBox`es at z 70 over z 60 were saying.
                if let Some(Err(e)) = self.handle_click_tab_bar(pane, x, y) {
                    tracing::warn!("tab strip click failed: {e}");
                }
            }
            UiFact::PaneTabsSecondary { pane, x, y } => self.open_tab_context_menu(pane, x, y),
            // The two strip buttons. They carry no coordinates: each is a node
            // that knows its pane, so what used to be a scan of two recorded
            // rect lists is the dispatch itself.
            UiFact::PaneMaximize(pane) => self.maximize_split_button(pane),
            UiFact::PaneClose(pane) => self.close_split_button(pane),
            UiFact::PaneTabsHover(at) => {
                self.shell_hover = at.and_then(|(pane, x, y)| self.tab_strip_hover(pane, x, y));
            }
            UiFact::PaneTabsWheel { pane, x, y, delta } => {
                self.dismiss_transient_popups();
                self.active_window().wheel_plugin_hook(x, y, delta);
                self.active_window_mut().scroll_tab_strip(pane, delta);
            }
            UiFact::PaneTabsPan { pane, delta } => {
                self.active_window_mut().scroll_tab_strip(pane, delta);
            }
            UiFact::PaneContentPress {
                pane,
                x,
                y,
                clicks,
                mods,
            } => {
                let mods = crate::view::shell::input::crossterm_mods(mods);
                if let Err(e) = self.press_pane_content(pane, x, y, clicks, mods) {
                    tracing::warn!("pane content click failed: {e}");
                }
            }
            // A pane's scrollbars, and its wheel. Every one of these took a
            // `(col, row)` and asked each pane's recorded rectangle in turn
            // whether it contained the point; the node says which pane, and
            // what stays looked up is the bar's own geometry — the thumb's
            // extent is a read of the scroll state at paint time.
            UiFact::PaneScrollbarPress { pane, axis, x, y } => {
                let r = match axis {
                    fresh_ui::Axis::Vertical => self.handle_click_scrollbar(pane, x, y),
                    fresh_ui::Axis::Horizontal => {
                        self.handle_click_horizontal_scrollbar(pane, x, y)
                    }
                };
                if let Some(Err(e)) = r {
                    tracing::warn!("scrollbar click failed: {e}");
                }
            }
            UiFact::PaneScrollbarHover(at) => {
                self.shell_hover = at.and_then(|(pane, row)| self.scrollbar_hover(pane, row));
            }
            // **The bar captured the pointer, so this is its move.** Whether
            // it means a drag is state this side holds: a bar's `Move` fires
            // on a bare hover too, and then the vertical one's job is the
            // highlight that follows the pointer between thumb and track.
            //
            // What this replaces is `chrome::pointer_grab` reading
            // `mouse_state.dragging_scrollbar` on every event to decide whose
            // drag it was, ranked against nine other flags.
            UiFact::PaneScrollbarDrag { pane, axis, x, y } => {
                let ms = &self.active_window().mouse_state;
                let dragging = match axis {
                    fresh_ui::Axis::Vertical => ms.dragging_scrollbar.is_some(),
                    fresh_ui::Axis::Horizontal => ms.dragging_horizontal_scrollbar.is_some(),
                };
                if !dragging {
                    if axis == fresh_ui::Axis::Vertical {
                        self.shell_hover = self.scrollbar_hover(pane, y);
                    }
                    return;
                }
                let r = match axis {
                    fresh_ui::Axis::Vertical => self.handle_vscrollbar_drag(x, y),
                    fresh_ui::Axis::Horizontal => self.handle_hscrollbar_drag(x, y),
                };
                if let Err(e) = r {
                    tracing::warn!("scrollbar drag failed: {e}");
                }
            }
            // The finalizer the blanket clear used to run for this grab. The
            // release is the captured bar's, so it never reaches that walk.
            UiFact::PaneScrollbarRelease { pane: _, axis } => {
                let ms = &mut self.active_window_mut().mouse_state;
                match axis {
                    fresh_ui::Axis::Vertical => {
                        ms.dragging_scrollbar = None;
                        ms.drag_start_row = None;
                        ms.drag_start_top_byte = None;
                    }
                    fresh_ui::Axis::Horizontal => {
                        ms.dragging_horizontal_scrollbar = None;
                        ms.drag_start_hcol = None;
                        ms.drag_start_left_column = None;
                    }
                }
            }
            UiFact::PaneWheel { pane, x, y, delta } => {
                // A live terminal that asked for the mouse gets the notch —
                // the same gate the content's press asks, for the same reason.
                if self.pane_content_took_wheel(x, y) {
                    return;
                }
                // A plugin's panel inside the pane's content scrolls itself:
                // its lists are viewports the library chains the notch into
                // before this fact is ever emitted, and a panel that shows
                // everything hands the notch on to the pane, which is what
                // reaches here.
                let Some(buffer_id) = self.active_window().pane_buffer(pane) else {
                    return;
                };
                // Only a wheel over a pane changes that terminal's
                // live/scrollback state; panning the tab strip or the explorer
                // leaves a live terminal streaming.
                if self.active_window().focused_terminal_live() {
                    self.enter_terminal_scrollback();
                } else {
                    self.active_window_mut()
                        .set_split_terminal_drag_scrollback(pane, buffer_id, false);
                }
                self.dismiss_transient_popups();
                self.active_window().wheel_plugin_hook(x, y, delta);
                self.active_window_mut()
                    .scroll_split_surface(pane, buffer_id, delta);
            }
            UiFact::PanePan { pane, delta } => {
                let (x, y) = ev.at;
                if self.pane_content_took_wheel(x, y) {
                    return;
                }
                let Some(buffer_id) = self.active_window().pane_buffer(pane) else {
                    return;
                };
                if let Err(e) = self
                    .active_window_mut()
                    .pan_split_horizontal(pane, buffer_id, delta)
                {
                    tracing::warn!("pane pan failed: {e}");
                }
            }
            UiFact::ClearTabMenus => {
                let w = self.active_window_mut();
                w.new_tab_menu = None;
                w.close_split_menu = None;
                w.tab_context_menu = None;
            }
            UiFact::MenuNav(step) => self.menu_nav(step),
            UiFact::CloseContextMenu => {
                self.active_window_mut().close_context_menus();
            }
            UiFact::HighlightContextMenuItem(idx) => {
                if let Some(core) = self.active_window_mut().context_menu_core_mut() {
                    core.highlighted = idx;
                }
            }
            UiFact::StepContextMenu(step) => {
                use crate::view::shell::msg::MenuStep;
                if let Some(core) = self.active_window_mut().context_menu_core_mut() {
                    match step {
                        MenuStep::Prev => core.prev_item(),
                        MenuStep::Next => core.next_item(),
                    }
                }
            }
            UiFact::ActivateContextMenuItem(idx) => {
                // The same two steps the old click handler took: move the
                // highlight, then activate through the path Enter uses.
                let Some((kind, _)) = self.active_window().open_context_menu() else {
                    return;
                };
                if let Some(core) = self.active_window_mut().context_menu_core_mut() {
                    core.highlighted = idx;
                }
                if let Err(e) = self.activate_highlighted_context_menu(kind) {
                    tracing::warn!("context menu activation failed: {e}");
                }
            }

            UiFact::Hover(target) => {
                // The tree says where the pointer is; the existing reaction
                // says what the menu does about it. Both halves of the old
                // walk, minus the walk.
                let target = match target {
                    // `MenuDropdownItem` names the menu it belongs to, and a
                    // row cannot know that — the tree is built per frame while
                    // the open menu changes under it. Fill it in here, where
                    // the answer lives.
                    Some(crate::app::types::HoverTarget::MenuDropdownItem(_, item)) => self
                        .menu_state
                        .active_menu
                        .map(|m| crate::app::types::HoverTarget::MenuDropdownItem(m, item)),
                    other => other,
                };
                let old = self.shell_hover.clone();
                self.shell_hover = target.clone();
                if old == target {
                    return;
                }
                // **Every registered reaction, not one hand-picked one.**
                // The tree says where the pointer is; what each surface does
                // about it stays with that surface. Calling
                // `menu_hover_reaction` directly instead silently dropped the
                // reactions belonging to two surfaces that had *also*
                // migrated: the explorer's git-status tooltip
                // (`FileExplorerStatusIndicator`) and the status bar's
                // indicator styling. This is the only thing that reaches any
                // of them — a reaction this fact does not run is a reaction
                // that never runs.
                //
                // The pointer cell the reactions want is the one the fact
                // arrived at; a hover fact is always produced by a pointer
                // event, and the event's own facts are where that position is.
                // A reaction that changed state — a submenu opened under the
                // pointer — is a change the next input's routing reads, and
                // the hover fact itself is transient: this is where it says so.
                let (col, row) = ev.at;
                for c in crate::app::chrome::components() {
                    if c.on_hover_change(self, old.as_ref(), target.as_ref(), col, row) {
                        self.shell_description_stale = true;
                    }
                }
            }
            UiFact::MenuBarPress { index } => {
                // `open_before` is what the menu was showing when this pointer
                // event *arrived*, before the layer's dismissal closed it. A
                // toggle needs that: by the time any message is applied the
                // menu is already shut, so asking now would always answer "not
                // open" and reopen what the press was meant to close.
                if ev.menu_open_before == Some(index) {
                    self.close_menu_with_auto_hide();
                } else {
                    self.active_window_mut().on_editor_focus_lost();
                    self.menu_state.open_menu(index);
                }
            }
            // -- file explorer ---------------------------------------------
            UiFact::ExplorerRowPress { index, clicks } => self.explorer_row_pressed(index, clicks),
            UiFact::ExplorerRowContext { index, x, y } => self.explorer_row_context(index, x, y),
            UiFact::ExplorerBodyContext { x, y } => self.explorer_body_context(x, y),
            // Focus, and nothing else: a press that reached the panel's own
            // box hit no row, so there is nothing to select or open. This is
            // the half of `handle_file_explorer_click` that ran before it
            // resolved a row.
            UiFact::ExplorerBodyPress => self.take_focus_for_file_explorer(),
            UiFact::PopupSelect(i) => self.select_popup_item(i),
            UiFact::PopupKey(k) => self.popup_key(k),
            UiFact::PopupDismissTransient => self.dismiss_transient_popups(),
            // The card's query input has the keyboard back — the ring
            // wrapped off the toolbar's last control. The toolbar's controls
            // are the toolbar panel's, so their presses and landings arrive
            // as `WidgetHit` and `WidgetFocus` on `Slot::PromptToolbar`.
            UiFact::CardInputFocus => self.release_prompt_toolbar_focus(),
            UiFact::CardPreviewScroll(delta) => {
                self.active_window_mut()
                    .scroll_overlay_preview_by_lines(delta);
            }
            // What a press inside a popup's text *means*. The tree said where
            // it landed, in the content's own coordinates; this is the rest of
            // `handle_click_buffer_popups` — a link if one is there, and the
            // start of a selection otherwise. Finding B is the reason it is
            // still here: the library says where selecting is meaningful and
            // holds no selection model.
            UiFact::PopupTextPress { line, col } => {
                let link = self
                    .active_state()
                    .popups
                    .top()
                    .and_then(|p| p.link_at_position(col, line));
                if let Some(url) = link {
                    #[cfg(feature = "runtime")]
                    match open::that(&url) {
                        Err(e) => self.set_status_message(format!("Failed to open URL: {e}")),
                        Ok(()) => self.set_status_message(format!("Opening: {url}")),
                    }
                    #[cfg(not(feature = "runtime"))]
                    let _ = url;
                    return;
                }
                if let Some(popup) = self.active_state_mut().popups.top_mut() {
                    popup.start_selection(line, col);
                }
            }
            UiFact::PopupTextDrag { line, col } => {
                if let Some(popup) = self.active_state_mut().popups.top_mut() {
                    popup.extend_selection(line, col);
                }
            }
            // The list row knew its own index; both of these used to be a
            // coordinate hit-test that resolved one.
            UiFact::SuggestionSelect(i) => {
                if let Some(Err(e)) = self.select_suggestion(i) {
                    tracing::warn!("suggestion select failed: {e}");
                }
            }
            UiFact::SuggestionConfirm(i) => {
                if let Some(Err(e)) = self.confirm_suggestion(i) {
                    tracing::warn!("suggestion confirm failed: {e}");
                }
            }
            UiFact::ExplorerClose => self.toggle_file_explorer(),
            // A section header's press, move and release, and the two things
            // the header does besides dividing. See `app::sidebar`.
            UiFact::SectionResizeBegin { index, y } => {
                self.begin_sidebar_section_drag(index, y);
            }
            UiFact::SectionToggle { index } => self.toggle_sidebar_section(index),
            UiFact::SectionClose { index } => self.close_sidebar_section(index),
            UiFact::SectionFocus { index } => self.focus_sidebar_section(index),
            UiFact::SidebarBlur => self.blur_sidebar_panels(),
            UiFact::ExplorerResizeBegin { x, y } => {
                let w = self.active_window().file_explorer_width;
                let st = &mut self.active_window_mut().mouse_state;
                st.dragging_file_explorer = true;
                st.drag_start_position = Some((x, y));
                st.drag_start_explorer_width = Some(w);
            }
            // The dock's column, all four of its gestures. Each body is the
            // arm `chrome::Dock::on_pointer` ran; what is gone is the pair of
            // boxes that decided *which* arm, and the insertion-order rule
            // that put the grip above the column.
            // A press on the column's dead space: focus it, and nothing else.
            // The re-focus fires a `focus` widget_event, and any mirror of
            // dock-focus state has to update before whatever the press goes on
            // to do — which is why it was first here while `DockPress` still
            // carried a cell into the runtime's own hit test.
            UiFact::DockFocus => {
                if self.dock.as_ref().is_some_and(|f| !f.focused) {
                    self.refocus_floating_panel(crate::app::PanelSlot::Dock);
                }
            }
            // **The cell is no longer read, and the menu no longer comes from
            // here.** This used to refocus and then probe the runtime's boxes
            // at `(x, y)` to raise the plugin's context menu. The widget's own
            // node carries the `WidgetEvent` now, so `UiFact::WidgetContext` has
            // already raised it by the time this runs — what is left of the
            // right press is the focus it takes, which is the half that was
            // never about geometry.
            UiFact::DockContext { .. } => {
                if self.dock.as_ref().is_some_and(|f| !f.focused) {
                    self.refocus_floating_panel(crate::app::PanelSlot::Dock);
                }
            }
            // The memo the overlay scrollbar reads. It is a memo rather than
            // a value derived where it is used because the *other* thing that
            // reveals the bar is a deadline (`scrollbar_flash_until`), and the
            // two have to be one answer by the time the description is built.
            UiFact::DockHover(over) => {
                if let Some(d) = self.dock.as_mut() {
                    d.scrollbar_zone_hovered = over;
                }
            }
            UiFact::DockResizeBegin => self.dock_resizing = true,
            // **The grip captured the pointer, so this is its move.** The
            // ladder these three replace read `chrome::pointer_grab` on every
            // event to decide whose drag it was; the node says so.
            //
            // The gate is still here and still belongs here: a grip's `Move`
            // fires on a bare hover too, and whether a drag is in progress is
            // state the editor holds. What is gone is deciding *which* drag
            // from that state.
            UiFact::GripDrag { which, x, y } => {
                use crate::view::shell::msg::Grip;
                match which {
                    Grip::DockWidth if self.dock_resizing => self.handle_dock_resize_drag(x),
                    Grip::Separator => {
                        if let Some((id, dir)) = self.active_window().mouse_state.dragging_separator
                        {
                            if let Err(e) = self.handle_separator_drag(x, y, id, dir) {
                                tracing::warn!("separator drag failed: {e}");
                            }
                        }
                    }
                    Grip::ExplorerWidth => {
                        if let Err(e) = self.handle_file_explorer_border_drag(x) {
                            tracing::warn!("explorer width drag failed: {e}");
                        }
                    }
                    Grip::SectionDivider(_) => self.drag_sidebar_section(y),
                    Grip::DockWidth => {}
                }
            }
            UiFact::GripRelease { which } => {
                use crate::view::shell::msg::Grip;
                match which {
                    // End a dock-resize drag and persist the chosen width so
                    // it survives toggling the dock off and on.
                    Grip::DockWidth => {
                        self.dock_resizing = false;
                        if let Some(crate::app::PanelPlacement::LeftDock { width_cols }) =
                            self.dock.as_ref().map(|f| f.placement)
                        {
                            self.dock_width = Some(width_cols);
                        }
                    }
                    // A finished separator drag changed the ratios, so the
                    // frame reflows through the one layout funnel.
                    Grip::Separator => {
                        self.active_window_mut().mouse_state.dragging_separator = None;
                        self.relayout();
                    }
                    Grip::ExplorerWidth => {
                        let ms = &mut self.active_window_mut().mouse_state;
                        ms.dragging_file_explorer = false;
                        ms.drag_start_explorer_width = None;
                    }
                    // A release where the press landed is a click, and a click
                    // on a header toggles the section; either way the drag is
                    // over and the rows it set stay set.
                    Grip::SectionDivider(_) => self.end_sidebar_section_drag(),
                }
            }
            UiFact::DockBlur => {
                if self.dock.as_ref().is_some_and(|f| f.focused) {
                    self.blur_floating_panel(crate::app::PanelSlot::Dock);
                }
            }
            // A split divider. The node is the container, so there is no hit
            // test: `handle_click_split_separator` walked a recorded list of
            // separator rectangles comparing the click against each in turn,
            // to arrive at the identity the node already had. The drag it arms
            // is still the legacy grab.
            UiFact::SeparatorPress {
                container,
                direction,
                x,
                y,
            } => {
                let ratio = self
                    .split_manager_mut()
                    .get_ratio(container.into())
                    .or_else(|| self.grouped_split_ratio(container));
                let st = &mut self.active_window_mut().mouse_state;
                st.dragging_separator = Some((container, direction));
                st.drag_start_position = Some((x, y));
                if let Some(ratio) = ratio {
                    self.active_window_mut().mouse_state.drag_start_ratio = Some(ratio);
                }
            }
            UiFact::SeparatorHover(at) => {
                // The tree's field, not the walk's. The walk runs after this on
                // the same event and finds nothing under a divider cell — it
                // would store `None` straight over the answer. See
                // `Editor::hovered`.
                self.shell_hover =
                    at.map(|(id, dir)| crate::app::types::HoverTarget::SplitSeparator(id, dir));
            }
            // A full-screen modal has the pointer. Which one is the tree's
            // answer — `Modality::Exclusive`, where a capture band offered
            // itself in rank order and stopped at the first taker — and what
            // the event means is the modal's, because its controls are
            // rectangles its own painter recorded.
            // **The editor's dialogs answer for themselves.** Five of the ten
            // rectangles its painter recorded were these three boxes' fields
            // and buttons, and the mouse arm behind them was a chain of
            // `point_in_rect` against each. What is left here is what the
            // press *meant*, which was always the editor's own business.
            // A row of the editor's table. Selecting is the whole of it, and
            // a section heading toggles as well — which is what the arm did
            // once it had worked out which row was under the cell.
            UiFact::KeybindingRow(i) => {
                if let Some(e) = self.keybinding_editor.as_mut() {
                    if i < e.display_rows.len() {
                        e.selected = i;
                        if e.selected_is_section_header() {
                            e.toggle_section_at_selected();
                        }
                    }
                }
            }
            // **The settings dialogs' buttons.** The arm behind them recomputed
            // the painter's layout to find which one a cell was on, with the
            // comment "must match `render_confirm_dialog`" beside the copy.
            // Now the button is a node and this is only what it means.
            UiFact::SettingsDialog(t) => {
                use crate::view::shell::settings::Target;
                match t {
                    Target::Confirm(0) => self.save_settings_and_close(),
                    Target::Confirm(1) => self.discard_settings_and_close(),
                    Target::Confirm(_) => {
                        if let Some(s) = self.settings_state.as_mut() {
                            s.showing_confirm_dialog = false;
                        }
                    }
                    // The reset prompt's two, which the keyboard's `Enter` arm
                    // spells the same way.
                    Target::Reset(0) => {
                        if let Some(s) = self.settings_state.as_mut() {
                            s.discard_changes();
                            s.showing_reset_dialog = false;
                        }
                    }
                    Target::Reset(_) => {
                        if let Some(s) = self.settings_state.as_mut() {
                            s.showing_reset_dialog = false;
                        }
                    }
                    // An entry dialog's two prompts, spelled the way their
                    // `Enter` arms are. Both had no mouse at all before this.
                    Target::EntryDiscard(i) => {
                        if let Some(s) = self.settings_state.as_mut() {
                            s.showing_entry_discard_confirm = false;
                            if i == 1 {
                                s.close_entry_dialog();
                            }
                        }
                    }
                    Target::EntryDelete(i) => {
                        if let Some(s) = self.settings_state.as_mut() {
                            s.showing_entry_delete_confirm = false;
                            if i == 1 {
                                s.delete_entry_dialog();
                            }
                        }
                    }
                }
            }
            UiFact::SettingsDialogHover(t) => {
                use crate::view::shell::settings::Target;
                if let Some(s) = self.settings_state.as_mut() {
                    s.confirm_dialog_hover = match t {
                        Some(Target::Confirm(i)) => Some(i),
                        _ => None,
                    };
                    s.reset_dialog_hover = match t {
                        Some(Target::Reset(i)) => Some(i),
                        _ => None,
                    };
                }
            }
            // **The footer's five buttons.** Each was a rectangle the painter
            // filed and `SettingsLayout::hit_test` compared a cell against.
            // The category tree's three answers. Each body is the arm
            // `handle_settings_mouse` ran for the matching `SettingsHit`;
            // what is gone is the five families of rectangle that decided
            // *which* arm, and the walk over them.
            UiFact::SettingsCategory(idx) => self.settings_select_category(idx),
            UiFact::SettingsCategorySection(cat, section) => {
                self.settings_jump_to_section(cat, section)
            }
            UiFact::SettingsCategoryDisclosure(idx) => self.settings_toggle_category(idx),
            // **The tree's own keys, arriving as what they mean.** The eight
            // arms behind this are the eight `handle_categories_input` still
            // has: one implementation (`SettingsState::tree_key`), reached
            // from the node that holds focus, or from the dispatcher for a
            // key that arrives without the tree in front of it.
            UiFact::SettingsTree(k) => {
                if let Some(s) = self.settings_state.as_mut() {
                    s.tree_key(k);
                }
            }
            // **The tree's ring reporting a landing on one of the dialog's
            // stops: one decider of the dialog's focus fact, through the same
            // door as every other.** A landing the fact already names is the
            // echo of the tree following the description's mark, and is a
            // no-op; one it does not name is the ring having moved — a Tab,
            // a press — and the fact follows.
            UiFact::SettingsFocus(f) => {
                use crate::view::settings::state::FocusTarget;
                use crate::view::shell::settings::Focus;
                let target = match f {
                    Focus::Categories => FocusTarget::Categories,
                    Focus::Card(i) => FocusTarget::Card(i),
                    Focus::Button(b) => FocusTarget::Footer(b.index()),
                };
                if let Some(s) = self.settings_state.as_mut() {
                    if s.focus_target() != target {
                        s.focus_on(target);
                        self.shell_description_stale = true;
                    }
                }
            }
            UiFact::SettingsSearchStep(forward) => {
                if let Some(s) = self.settings_state.as_mut() {
                    match forward {
                        true => s.search_next(),
                        false => s.search_prev(),
                    }
                }
            }
            UiFact::SettingsClearCategory => {
                if let Some(s) = self.settings_state.as_mut() {
                    s.clear_current_category();
                }
            }
            UiFact::SettingsButton(b) => {
                use crate::view::shell::settings::Button;
                match b {
                    Button::Layer => {
                        if let Some(s) = self.settings_state.as_mut() {
                            s.cycle_target_layer();
                        }
                    }
                    Button::Save => self.close_settings(true),
                    Button::Cancel => {
                        if let Some(s) = self.settings_state.as_mut() {
                            match s.has_changes() {
                                true => {
                                    s.showing_confirm_dialog = true;
                                    s.confirm_dialog_selection = 0;
                                }
                                false => s.visible = false,
                            }
                        }
                    }
                    Button::Reset => {
                        if let Some(s) = self.settings_state.as_mut() {
                            s.reset_current_to_default();
                        }
                    }
                    Button::Edit => {
                        if let Some(layer) = self.settings_state.as_ref().map(|s| s.target_layer) {
                            // Best-effort: the file may not exist yet.
                            #[allow(clippy::let_underscore_must_use)]
                            let _ = self.open_config_file(layer);
                        }
                    }
                }
            }
            UiFact::SettingsButtonHover(b) => {
                use crate::view::settings::hit::SettingsHit;
                use crate::view::shell::settings::Button;
                if let Some(s) = self.settings_state.as_mut() {
                    s.hover_hit = b.map(|b| match b {
                        Button::Layer => SettingsHit::LayerButton,
                        Button::Reset => SettingsHit::ResetButton,
                        Button::Save => SettingsHit::SaveButton,
                        Button::Cancel => SettingsHit::CancelButton,
                        Button::Edit => SettingsHit::EditButton,
                    });
                }
            }
            UiFact::KeybindingSearch => {
                if let Some(e) = self.keybinding_editor.as_mut() {
                    e.start_search();
                }
            }
            UiFact::KeybindingDialog(t) => {
                use crate::view::shell::keybinding::Target;
                let Some(mut e) = self.keybinding_editor.take() else {
                    return;
                };
                match t {
                    Target::KeyField | Target::ActionField | Target::ContextField => {
                        use crate::app::keybinding_editor::EditMode;
                        if let Some(d) = e.edit_dialog.as_mut() {
                            let (area, mode) = match t {
                                Target::KeyField => (0, EditMode::RecordingKey),
                                Target::ActionField => (1, EditMode::EditingAction),
                                _ => (2, EditMode::EditingContext),
                            };
                            d.focus_area = area;
                            d.mode = mode;
                        }
                    }
                    Target::Save => {
                        if let Some(err) = e.apply_edit_dialog() {
                            self.set_status_message(err);
                        }
                    }
                    Target::Cancel => e.edit_dialog = None,
                    Target::ConfirmSave => {
                        self.save_keybinding_editor_changes(&e);
                        return;
                    }
                    Target::ConfirmDiscard => {
                        self.set_status_message("Keybinding editor closed".to_string());
                        return;
                    }
                    Target::ConfirmCancel => e.showing_confirm_dialog = false,
                }
                self.keybinding_editor = Some(e);
            }
            // **Which surface a key belongs to is containment; what it means
            // is the surface's.** Each of these had a `ChromeComponent`
            // offering it every key in `layer_rank` order; the layer owns the
            // keyboard, focus is inside it, and a key nothing inside answered
            // arrives here.
            UiFact::ModalKey(slot) => {
                use crate::view::shell::modal::KeySlot;
                let Some(ev) = self.shell_key_event else {
                    return;
                };
                match slot {
                    // A key the dialog's dispatcher answered may have moved
                    // its focus fact; the tree learns of it on the next
                    // layout, which the stale mark brings forward for a key
                    // that follows in the same batch.
                    KeySlot::Settings => {
                        self.dispatch_settings_key(&ev);
                        self.shell_description_stale = true;
                    }
                    KeySlot::KeybindingEditor => {
                        let _ = self.handle_keybinding_editor_input(&ev);
                    }
                    KeySlot::Calibration => {
                        let _ = self.handle_calibration_input(&ev);
                    }
                    KeySlot::WorkspaceTrust => {
                        let _ = self.handle_workspace_trust_key(&ev);
                    }
                }
            }
            // **The prompt: the same seam, and the claim it completes.**
            //
            // `ModalKey` above can be a plain effect because a modal swallows
            // what its interior ignores — the tree already claimed the key
            // when it routed it there. The prompt's layer is
            // `Modality::Focus` instead: it confines the keyboard so nothing
            // else takes a keystroke ahead of it, and hands back what
            // `dispatch_prompt_key` declines, which is how the file browser's
            // `Alt+H` and quick-open's `Ctrl+P` still reach their bindings.
            // `Some` — including `Some(Ignored)`, which is how the
            // query-replace confirm prompt consumes every key — is the
            // prompt taking it.
            UiFact::PromptKey => {
                let Some(ev) = self.shell_key_event else {
                    return;
                };
                // A key the prompt takes is the query input's. Raised from a
                // focused toolbar control (`overlay_prompt::toolbar_band`'s
                // capture rule — the arrows, paging, typing), it hands the
                // keyboard back to the input along with the key.
                self.release_prompt_toolbar_focus();
                // `None` is the prompt declining, and a declined key is the
                // editor's — resolved in the `Prompt` context the focus chain
                // still names, which is how the file browser's toggles reach
                // their bindings.
                if self.dispatch_prompt_key(&ev).is_none() {
                    self.hand_key_to_editor(ev);
                }
            }
            // **A focused plugin panel: the same declining seam.** Its
            // interior is `dispatch_floating_widget_key`, which hands back a
            // shortcut the panel does not bind — a blurred dock and ordinary
            // keybinding resolution — so the layer confines the keyboard
            // without swallowing, and the claim is completed here.
            UiFact::PanelKey(slot) => {
                use crate::view::shell::widgets::Slot;
                let Some(ev) = self.shell_key_event else {
                    return;
                };
                let slot = match slot {
                    Slot::Dock => crate::app::PanelSlot::Dock,
                    Slot::Floating => crate::app::PanelSlot::Floating,
                    Slot::Sidebar(i) => crate::app::PanelSlot::Sidebar(i),
                    // The settings surfaces reuse the widget vocabulary but
                    // are not panels and never raise this layer.
                    Slot::Settings | Slot::SettingsEntry => return,
                    // The prompt toolbar's controls answer their own keys —
                    // Space and Enter on a toggle — and what they decline is
                    // the prompt's, the route the card's input row takes.
                    Slot::PromptToolbar => {
                        if !self.dispatch_prompt_toolbar_key(ev.code, ev.modifiers)
                            && self.dispatch_prompt_key(&ev).is_none()
                        {
                            self.hand_key_to_editor(ev);
                        }
                        return;
                    }
                    // A pane-mounted panel: the key the mode did not bind (the
                    // keymap on its interior answered those) goes to its
                    // widgets exactly as a dock's does — the focused list
                    // steps, the focused field types — and what they decline
                    // is the buffer's own route: the mode's text input,
                    // chords, keybinding resolution, as it always was.
                    Slot::Pane(leaf) => {
                        let taken = match self.pane_panel_key(leaf) {
                            Some(pk) => self.dispatch_pane_panel_key(&pk, ev.code, ev.modifiers),
                            None => false,
                        };
                        if !taken {
                            self.hand_key_to_editor(ev);
                        }
                        return;
                    }
                };
                // **A panel that does not own the keyboard answers for
                // nothing.**
                //
                // The fallback that raises this fact is on the panel's
                // *interior* (`panel::interior`), which is in the tree
                // whenever the panel is described; the layer that makes the
                // panel the keyboard's owner (`panel::keys_layer`) is declared
                // only while it is focused. The two part company when focus is
                // *restored* into a blurred panel — `apply_autofocus` puts it
                // back where it was when a scope opened, and the host may have
                // blurred the panel in between. Closing the command palette
                // did exactly that: focus went back to the dock button it had
                // come from, and every keystroke after that went to the dock
                // instead of the buffer the palette had been opened over.
                //
                // Asked of the host because that is where the fact lives —
                // `FloatingWidgetState::focused`, the same one `Frame`'s
                // `dock_keys` / `panel_keys` read to declare the layer. A
                // `false` here is a decline, so the key carries on to the
                // editor's own pipeline exactly as it did before the panel was
                // described.
                if !self.panel(slot).is_some_and(|p| p.focused) {
                    self.hand_key_to_editor(ev);
                    return;
                }
                // **The focus toggle is resolved ahead of the panel.** A
                // focused dock swallows keys in the dispatch below, so the
                // global toggle (default Alt+O) could never hand focus back
                // to the editor once you had dived in. Only the blur-out
                // direction needs this — focusing a blurred dock is ordinary
                // keybinding resolution, because the editor owns the keyboard
                // then.
                if slot == crate::app::PanelSlot::Dock {
                    let ctx = self.get_key_context();
                    let resolved = self.keybindings.read().ok().map(|kb| kb.resolve(&ev, ctx));
                    if matches!(
                        resolved,
                        Some(crate::input::keybindings::Action::ToggleDockFocus)
                    ) {
                        if let Err(e) =
                            self.handle_action(crate::input::keybindings::Action::ToggleDockFocus)
                        {
                            tracing::warn!("dock focus toggle failed: {e}");
                        }
                        return;
                    }
                }
                // The same for a sidebar section and the cycle that leaves it:
                // resolved ahead of the panel, or the blur-and-fall-through
                // would cycle from the editor rather than from the section.
                if let crate::app::PanelSlot::Sidebar(_) = slot {
                    let ctx = self.get_key_context();
                    let resolved = self.keybindings.read().ok().map(|kb| kb.resolve(&ev, ctx));
                    if matches!(
                        resolved,
                        Some(crate::input::keybindings::Action::FocusNextSidebarSection)
                    ) {
                        self.focus_next_sidebar_section();
                        return;
                    }
                }
                // A `false` here is the interior declining —
                // `BlurUnconsumed` from the router, which has just blurred
                // the panel — and a declined key is the editor's: it goes on
                // to the editor's own resolution, over a tree that now says
                // the pane behind the panel has the keyboard.
                if !self.dispatch_floating_widget_key(slot, ev.code, ev.modifiers) {
                    self.hand_key_to_editor(ev);
                }
            }
            // The workspace-trust prompt. Each body is an arm of
            // `handle_workspace_trust_mouse`, which the capture band reached
            // with a raw `MouseEvent` and a hand-written hit test over four
            // recorded rectangles.
            //
            // **Selecting is not consenting.** A click moves the selection and
            // leaves the prompt up; `[ OK ]` commits. Accepting on click made
            // "Trust folder & Allow Tooling" a one-click grant of execution
            // rights on a security prompt, with no chance to read the option
            // before committing to it.
            UiFact::TrustSelect(i) => self.set_workspace_trust_selection(i),
            UiFact::TrustConfirm => {
                let idx = self.current_workspace_trust_selection();
                self.confirm_workspace_trust(idx);
            }
            UiFact::TrustSecondary => {
                self.hide_popup();
                if !self.workspace_trust_prompt_cancellable {
                    self.should_quit = true;
                }
            }
            // The inspector. Dismissing it is the same statement three
            // places used to make: an outside-press guard returning
            // `PassAfter`, an `on_key` that cleared the field and returned
            // `None`, and the popup's own opacity in between.
            // The file-open dialog. Each control names itself; the
            // coordinate facts, the painter's recorded spans and the hit
            // tests that resolved one against the other are gone.
            UiFact::BrowserToggle(t) => match t {
                crate::app::file_open::Toggle::ShowHidden => self.file_open_toggle_hidden(),
                crate::app::file_open::Toggle::DetectEncoding => {
                    self.file_open_toggle_detect_encoding()
                }
            },
            UiFact::BrowserShortcut(i) => self.file_open_press_shortcut(i),
            UiFact::BrowserNavigation => {
                if let Some(state) = &mut self.active_window_mut().file_open_state {
                    state.active_section = crate::app::file_open::FileOpenSection::Navigation;
                }
            }
            UiFact::BrowserSort(mode) => self.file_open_toggle_sort(mode),
            UiFact::BrowserSelect(i) => self.file_open_select_entry(i),
            UiFact::BrowserActivate(i) => self.file_open_activate_entry(i),
            UiFact::ThemeInfoDismiss => self.active_window_mut().theme_info_popup = None,
            UiFact::ThemeInspect { x, y } => {
                if let Err(e) = self.show_theme_info_popup(x, y) {
                    tracing::warn!("theme inspect failed: {e}");
                }
            }
            UiFact::ThemeInfoOpenEditor => {
                let key = self
                    .active_window()
                    .theme_info_popup
                    .as_ref()
                    .and_then(|p| p.info.fg_key.clone().or_else(|| p.info.bg_key.clone()));
                self.active_window_mut().theme_info_popup = None;
                if let Some(key) = key {
                    self.fire_theme_inspect_hook(key);
                }
            }
            UiFact::ThemeInfoButtonHover(on) => {
                self.shell_hover = on.then_some(crate::app::types::HoverTarget::ThemeInfoButton);
            }
            UiFact::ExplorerScroll { delta, x, y } => {
                // The surface's wheel, with the surface. Unchanged from the
                // chrome component's `on_wheel`, including the plugin hook —
                // the position it reports is the pointer's, which the tree
                // carries on the event.
                self.dismiss_transient_popups();
                self.active_window().wheel_plugin_hook(x, y, delta);
                self.active_window_mut().scroll_file_explorer_view(delta);
            }

            UiFact::MenuItemClick { depth, index } => {
                let Some(active) = self.menu_state.active_menu else {
                    return;
                };
                let menus: Vec<crate::config::Menu> = self
                    .menus
                    .menus
                    .iter()
                    .chain(self.menu_state.plugin_menus.iter())
                    .cloned()
                    .collect();
                let Some(menu) = menus.get(active) else {
                    return;
                };
                match self.activate_menu_item(depth, index, menu) {
                    Ok(Err(e)) | Err(e) => {
                        tracing::warn!("menu item activation failed: {e}")
                    }
                    Ok(Ok(())) => {}
                }
            }
            UiFact::CloseMenu => {
                if self.menu_state.active_menu.is_some() {
                    self.close_menu_with_auto_hide();
                }
            }
        }
    }
}
