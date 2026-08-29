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
use crate::model::event::LeafId;
use crate::view::shell::splits::PaneChrome;
use crate::view::ui::split_rendering::{
    paint_leaf, paint_separators, prepare_content, record_scrollbar_theme_runs, ContentPass,
    FrameFacts, Stores,
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
}

impl<'a> BodyPainter<'a> {
    pub fn new(editor: &'a mut Editor, state: BodyState) -> Self {
        // Cloned rather than taken: a frame may fold more than once, and the
        // second pass must not paint panes with no chrome at all.
        let pane_chrome = editor.pending_pane_chrome.clone();
        let scrollback = editor
            .windows
            .get(&editor.active_window)
            .and_then(|win| {
                win.buffers.splits().map(|(_, vs_map)| {
                    vs_map
                        .iter()
                        .filter(|(leaf, svs)| {
                            win.split_terminal_scrollback(**leaf, svs.active_buffer)
                        })
                        .map(|(leaf, _)| *leaf)
                        .collect()
                })
            })
            .unwrap_or_default();
        Self {
            editor,
            state,
            pass: None,
            out: BodyOutput::default(),
            screen_width: 0,
            pane_chrome,
            scrollback,
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
                &out.split_areas,
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
        let out = &mut self.out;
        self.pass = with_grid(
            self.editor,
            state,
            buf.area.width,
            &self.pane_chrome,
            &self.scrollback,
            |facts, stores, mgr, window_chrome| {
                let base_visible = mgr.get_visible_buffers(area);
                let pass = prepare_content(
                    &base_visible,
                    mgr,
                    stores.split_view_states.as_deref_mut(),
                    facts.grouped_subtrees,
                    facts.pane_chrome,
                    window_chrome,
                );
                paint_separators(buf, area, mgr, &base_visible, facts, stores, out);
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
        pane.3 = rect;
        let state = self.state;
        let out = &mut self.out;
        with_grid(
            self.editor,
            state,
            buf.area.width,
            &self.pane_chrome,
            &self.scrollback,
            |facts, stores, _mgr, _window_chrome| {
                paint_leaf(buf, pane, facts, pass, stores, out, caret);
            },
        );
    }
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
            // The dock's column is native around a `Host` content leaf that
            // the panel painter still owns, and the status bar's prompt states
            // are the one row `Editor::render` still paints outside the fold.
            HostRegion::Dock | HostRegion::StatusBar => {}
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
    }

    impl Paint {
        /// A theme key. A `&'static str` — which nearly every call site has —
        /// borrows rather than allocating.
        pub fn key(k: impl Into<Cow<'static, str>>) -> Paint {
            Paint::Key(k.into())
        }

        /// The key behind this half, when there is one.
        pub fn name(&self) -> Option<&str> {
            match self {
                Paint::Key(k) => Some(k),
                Paint::Lit(_) => None,
            }
        }

        fn color(&self, theme: &Theme) -> Option<Color> {
            match self {
                Paint::Key(k) => theme.resolve_theme_key(k),
                Paint::Lit(c) => Some(*c),
            }
        }

        /// Read one half of the written form back.
        ///
        /// `#7ee787` is a 24-bit literal, `#i42` a palette index, `#Yellow`
        /// one of the sixteen names; anything else is a theme key.
        fn parse(half: &str) -> Option<Paint> {
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

        /// The written spelling of each, and the only place the two forms are
        /// paired: [`Attrs::named`] and [`fmt::Display`] both read this.
        const SPELLINGS: [(Attrs, &'static str, Modifier); 5] = [
            (Attrs::BOLD, "bold", Modifier::BOLD),
            (Attrs::UNDERLINE, "underline", Modifier::UNDERLINED),
            (Attrs::ITALIC, "italic", Modifier::ITALIC),
            (Attrs::STRIKETHROUGH, "strikethrough", Modifier::CROSSED_OUT),
            (Attrs::DIM, "dim", Modifier::DIM),
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
            let declared = match &self.fg {
                Paint::Key(k) => theme.resolve_modifier_key(k),
                Paint::Lit(_) => Modifier::empty(),
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
    pub fn resolve(name: &str, theme: &Theme) -> Style {
        Ink::parse(name)
            .and_then(|ink| ink.style(theme))
            .unwrap_or_else(|| base(theme))
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
        let Some(mut ui) = self.shell_ui.take() else {
            return Dispatched::default();
        };
        // What the menu was showing when this event arrived. Snapshotted
        // before a single message is applied, because the first of them may be
        // the layer's own dismissal — and a toggle has to know what it is
        // toggling. See `UiFact::MenuBarPress`.
        self.shell_menu_open_before = self.menu_state.active_menu;
        // Where the pointer is, for the hover reactions a resulting
        // `UiFact::Hover` will run — they anchor tooltips to it, and the fact
        // itself carries only *what* is under the pointer.
        if let Some(p) = input.position() {
            self.shell_hover_at = (p.x.max(0) as u16, p.y.max(0) as u16);
        }
        let result = ui.dispatch(input);
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
        // A message *is* the change: a `UiFact` exists to be reacted to, and
        // a pointer that crosses no element boundary produces none, which is
        // what keeps an idle motion from drawing a frame.
        let changed = !result.msgs.is_empty();
        for msg in result.msgs {
            match msg {
                crate::view::shell::msg::UiMsg::Action(action) => {
                    // Straight into the pipeline that has always applied
                    // actions; nothing about it changes.
                    if let Err(e) = self.handle_action(action.clone()) {
                        tracing::warn!("shell action {action:?} failed: {e}");
                    }
                }
                crate::view::shell::msg::UiMsg::Ui(fact) => self.apply_ui_fact(fact),
            }
        }
        Dispatched { claimed, changed }
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
    fn apply_ui_fact(&mut self, fact: crate::view::shell::msg::UiFact) {
        use crate::view::shell::msg::UiFact;
        match fact {
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
            UiFact::PaneContentPress { pane, x, y, clicks } => {
                if let Err(e) = self.press_pane_content(pane, x, y, clicks) {
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
            UiFact::PaneWheel { pane, x, y, delta } => {
                // A live terminal that asked for the mouse gets the notch —
                // the same gate the content's press asks, for the same reason.
                if self.pane_content_took_wheel(x, y) {
                    return;
                }
                // A plugin's panel inside the pane's content scrolls itself
                // first — it was a box at z 120 over the pane's content rect,
                // and a nested surface's wheel is genuinely the nested one's.
                if self.handle_split_widget_panel_wheel(x, y, delta) {
                    return;
                }
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
                let (x, y) = self.shell_hover_at;
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
                // event, and `shell_hover_at` is where that event's position
                // is kept for exactly this.
                let (col, row) = self.shell_hover_at;
                for c in crate::app::chrome::components() {
                    c.on_hover_change(self, old.as_ref(), target.as_ref(), col, row);
                }
            }
            UiFact::MenuBarPress { index } => {
                // `open_before` is what the menu was showing when this pointer
                // event *arrived*, before the layer's dismissal closed it. A
                // toggle needs that: by the time any message is applied the
                // menu is already shut, so asking now would always answer "not
                // open" and reopen what the press was meant to close.
                if self.shell_menu_open_before == Some(index) {
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
            UiFact::PopupDismissTransient => self.dismiss_transient_popups(),
            // The toolbar's controls are a plugin's `WidgetSpec`, hit-tested
            // against the widget runtime's own boxes. The band said where the
            // press landed inside it; this is the walk `chrome:prompt_scrim`
            // did after subtracting a stored origin by hand.
            UiFact::CardToolbarPress { x, y } => {
                let hit = {
                    let boxes = &self.active_chrome().prompt_toolbar_boxes;
                    crate::widgets::layout_box::hit_path(boxes, y as u32, x as u32)
                        .into_iter()
                        .rev()
                        .filter(|&i| boxes[i].focusable)
                        .find_map(|i| boxes[i].key.clone())
                };
                if let Some(widget_key) = hit {
                    // Move keyboard focus to the clicked control so Tab
                    // continues from here, then flip it through the host.
                    if let Some(p) = self.active_window_mut().prompt.as_mut() {
                        p.toolbar_focus = Some(widget_key.clone());
                    }
                    self.toggle_overlay_toolbar_widget(&widget_key);
                }
            }
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
            UiFact::DockPress { x, y } => {
                // Re-focus first when blurred: the un-blur notifies the plugin
                // via a `focus` widget_event, so any mirror of dock-focus
                // state updates before the click's row-select event fires its
                // scheduling logic.
                if self.dock.as_ref().is_some_and(|f| !f.focused) {
                    self.refocus_floating_panel(crate::app::PanelSlot::Dock);
                }
                self.handle_floating_widget_click(crate::app::PanelSlot::Dock, x, y);
            }
            UiFact::DockContext { x, y } => {
                if self.dock.as_ref().is_some_and(|f| !f.focused) {
                    self.refocus_floating_panel(crate::app::PanelSlot::Dock);
                }
                self.handle_floating_widget_context_click(crate::app::PanelSlot::Dock, x, y);
            }
            UiFact::DockScroll { delta, x, y } => {
                self.handle_floating_widget_panel_wheel(crate::app::PanelSlot::Dock, x, y, delta);
            }
            UiFact::DockResizeBegin => self.dock_resizing = true,
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
            UiFact::ModalPointer(slot) => {
                use crate::view::shell::modal::Slot;
                let Some((ev, double)) = self.shell_pointer_event else {
                    return;
                };
                let r = match slot {
                    Slot::Settings => self.handle_settings_mouse(ev, double),
                    Slot::KeybindingEditor => self.handle_keybinding_editor_mouse(ev),
                    // Keyboard-driven: it owns the band and ignores the
                    // pointer, which the layer's claim already arranges.
                    Slot::Calibration => Ok(false),
                    Slot::FloatingPanel => self.handle_floating_modal_mouse(ev),
                };
                if let Err(e) = r {
                    tracing::warn!("modal mouse failed: {e}");
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
            // The file-open dialog. Each body is the arm
            // `chrome::FileBrowser` ran; the box that decided *which* — and
            // its full-frame stand-in for the frame before the first paint —
            // is gone.
            UiFact::BrowserPress { x, y, double } => {
                if double {
                    self.handle_file_open_double_click(x, y);
                } else {
                    self.handle_file_open_click(x, y);
                }
            }
            UiFact::BrowserHover { x, y } => {
                self.shell_hover = self.compute_file_browser_hover(x, y);
            }
            UiFact::BrowserScroll(delta) => {
                self.handle_file_open_scroll(delta);
            }
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
