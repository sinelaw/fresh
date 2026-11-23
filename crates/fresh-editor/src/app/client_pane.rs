//! One client's view of one pane: the web client's window onto a buffer
//! (`docs/internal/web-ui.md`, *Buffers, through views*).
//!
//! The buffer and its undo are the editor's and shared. What a client owns
//! of a pane is its size, its scroll, its cursors and selections, and the
//! visual rows it was last drawn with — the rows keyboard motion (Up/Down by
//! visual line, Home and End of a wrapped row) walks and a click resolves
//! against. Those are kept here and swapped into the pane for the client's
//! render and for its commands, then swapped back out, so the terminal's own
//! pane — and every other client's — is untouched. Positions put aside are
//! carried through the edits made meanwhile when they come back.
//!
//! The text is laid out by the editor's own pipeline at the client's width,
//! from the piece tree and its wrap and row caches: only the window the
//! client asks for is formatted.

use super::Editor;
use crate::model::event::{BufferId, LeafId};
use crate::view::shell::buffer_host::PaneView;
use crate::view::viewport::Viewport;
use ratatui::layout::Rect;
use std::collections::HashMap;

/// A client's view of one pane. See the module docs.
pub struct ClientPane {
    /// The window (workspace) whose pane it views. Leaf ids are numbered
    /// per window, so a leaf alone does not name a pane.
    pub window: fresh_core::WindowId,
    /// The pane (split leaf) it views.
    pub leaf: LeafId,
    /// The client's size for it, in cells.
    pub width: u16,
    pub height: u16,
    /// The client's own scroll and cursors in each buffer the pane has shown
    /// it, while they are put aside.
    viewports: HashMap<BufferId, Aside>,
    /// The rows the client was last drawn with.
    rows: PaneView,
    /// Whether a terminal shown here should be sized to this view at its
    /// next render: set when the view opens, changes size or takes input.
    fit_terminal: bool,
}

/// A pane's scroll and cursors while they are put aside — the client's
/// between its turns, the pane's own (the terminal's) during one — and the
/// buffer version they were last valid at. Whichever comes back is first
/// carried through the edits made meanwhile (`CoordMap`), so another
/// client's typing above a cursor moves it along rather than under it.
struct Aside {
    viewport: Viewport,
    cursors: crate::model::cursor::Cursors,
    version: u64,
}

impl Aside {
    /// Carry every position this holds to `state`'s current version, and
    /// swap it with what `view` holds; `self` then holds the other side, as
    /// of now.
    fn swap_with(
        &mut self,
        state: &crate::state::EditorState,
        view: &mut crate::view::split::BufferViewState,
    ) {
        let now = state.buffer.version();
        if self.version != now {
            let len = state.buffer.len();
            let from = self.version;
            let map = |p: usize| state.coord_map.map(p, from).unwrap_or(p).min(len);
            self.cursors.map(|c| {
                c.position = map(c.position);
                c.anchor = c.anchor.map(map);
            });
            let top = map(self.viewport.top_byte());
            self.viewport.set_top_byte(top);
        }
        std::mem::swap(&mut view.viewport, &mut self.viewport);
        std::mem::swap(&mut view.cursors, &mut self.cursors);
        self.version = now;
    }
}

/// One render of a client's pane.
pub struct PaneRender {
    pub buffer: BufferId,
    /// The pane's cells at the client's size.
    pub cells: ratatui::buffer::Buffer,
    /// Columns of gutter left of the text.
    pub gutter: u16,
    /// The caret's cell, when the buffer shows one.
    pub caret: Option<(u16, u16)>,
    /// Where the window starts and how long the buffer is, in bytes — the
    /// scroll position a client shows.
    pub top_byte: usize,
    pub len: usize,
    /// The window's first line, and how many lines the buffer has, when the
    /// buffer knows (a huge file not yet indexed does not): what an exact
    /// scrollbar is drawn from.
    pub top_line: Option<usize>,
    pub lines: Option<usize>,
    /// Whether any cursor has a selection (a right-click keeps it).
    pub selection: bool,
}

impl ClientPane {
    pub fn new(window: fresh_core::WindowId, leaf: LeafId, width: u16, height: u16) -> Self {
        ClientPane {
            window,
            leaf,
            width: width.max(1),
            height: height.max(1),
            viewports: HashMap::new(),
            rows: PaneView::default(),
            fit_terminal: true,
        }
    }

    /// Change the view's size.
    pub fn resize(&mut self, width: u16, height: u16) {
        let (w, h) = (width.max(1), height.max(1));
        if (w, h) != (self.width, self.height) {
            self.width = w;
            self.height = h;
            self.fit_terminal = true;
        }
    }
}

/// Where a client view's pane stands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClientPaneState {
    /// In the active window: drawn and edited in.
    Shown,
    /// In a window that is not active (the Orchestrator switched away from
    /// it): kept, not drawn, until its window is active again.
    Away,
    /// Its window or its pane is gone.
    Gone,
}

impl Editor {
    /// Where `pane`'s pane stands: shown, in a window switched away from,
    /// or gone.
    pub fn client_pane_state(&self, pane: &ClientPane) -> ClientPaneState {
        match self.windows.get(&pane.window) {
            Some(w) if w.pane_buffer(pane.leaf).is_some() => {
                if pane.window == self.active_window {
                    ClientPaneState::Shown
                } else {
                    ClientPaneState::Away
                }
            }
            _ => ClientPaneState::Gone,
        }
    }

    /// Run `f` with `pane`'s scroll and rows in force for its leaf, then put
    /// the pane's own back. `None` when the leaf is gone, or its window is
    /// not the active one (see [`Self::client_pane_state`]).
    pub fn with_client_pane<T>(
        &mut self,
        pane: &mut ClientPane,
        f: impl FnOnce(&mut Editor) -> T,
    ) -> Option<T> {
        if pane.window != self.active_window {
            return None;
        }
        let leaf = pane.leaf;
        let buffer = self.active_window().pane_buffer(leaf)?;
        let (w, h) = (pane.width, pane.height);
        self.active_window_mut()
            .buffers
            .with_buffer_and_split(buffer, leaf, |state, view| {
                // A client new to this buffer starts where the pane is.
                let held = pane.viewports.entry(buffer).or_insert_with(|| Aside {
                    viewport: view.viewport.clone(),
                    cursors: view.cursors.clone(),
                    version: state.buffer.version(),
                });
                held.swap_with(state, view);
                view.viewport.resize(w, h);
            })?;
        self.active_window_mut()
            .pane_handle_for(leaf)
            .swap_view(&mut pane.rows);
        let out = f(self);
        let win = self.active_window_mut();
        win.pane_handle_for(leaf).swap_view(&mut pane.rows);
        win.buffers
            .with_buffer_and_split(buffer, leaf, |state, view| {
                if let Some(held) = pane.viewports.get_mut(&buffer) {
                    held.swap_with(state, view);
                }
            });
        let live: Vec<BufferId> = win
            .buffers
            .split_view_states()
            .and_then(|m| m.get(&leaf))
            .map(|s| s.buffer_states().map(|(b, _)| *b).collect())
            .unwrap_or_default();
        pane.viewports.retain(|b, _| live.contains(b));
        Some(out)
    }

    /// Lay out and draw `pane` at its size, as the pane the user is editing:
    /// active, its caret drawn. The rows it settles are the client's.
    pub fn render_client_pane(&mut self, pane: &mut ClientPane) -> Option<PaneRender> {
        let leaf = pane.leaf;
        let area = Rect::new(0, 0, pane.width, pane.height);
        // A terminal follows the view last used: its PTY takes this view's
        // size when the view opened, was resized or was typed into.
        if std::mem::take(&mut pane.fit_terminal) {
            if let Some(buffer) = self.active_window().pane_buffer(leaf) {
                if self.active_window().is_terminal_buffer(buffer) {
                    self.active_window_mut()
                        .resize_terminal(buffer, pane.width, pane.height);
                }
            }
        }
        self.with_client_pane(pane, |ed| ed.render_leaf_at(leaf, area))
            .flatten()
    }

    /// The editor half of [`Self::render_client_pane`], with the client's
    /// view already swapped in.
    fn render_leaf_at(&mut self, leaf: LeafId, area: Rect) -> Option<PaneRender> {
        let theme = self.theme.read().unwrap().clone();
        let session_mode = self.session_mode || !self.software_cursor_only;
        let highlight_current_column = self.config.editor.highlight_current_column;
        let style = crate::view::ui::RenderStyle {
            theme: &theme,
            ansi_background: self.ansi_background.as_ref(),
            cfg: crate::view::ui::EditorRenderConfig::new(
                &self.config.editor,
                self.background_fade,
                self.software_cursor_only,
                self.linux_console,
            ),
        };
        let window = self.active_window;
        let win = self.windows.get_mut(&window)?;
        let buffer = win.pane_buffer(leaf)?;
        let mut cells = ratatui::buffer::Buffer::empty(area);
        let mut cell_theme_map = Vec::new();
        let (rows, mut caret) = win
            .buffers
            .with_buffer_and_split(buffer, leaf, |state, view| {
                crate::view::ui::SplitRenderer::render_view(
                    &mut cells,
                    state,
                    view,
                    area,
                    style,
                    session_mode,
                    highlight_current_column,
                    &mut cell_theme_map,
                    area.width,
                )
            })?;
        // A live terminal's grid over the rows the text pass drew, as the
        // terminal's own pane paints it.
        let win = self.windows.get(&window)?;
        // Its caret is the terminal's cursor, not the cursor of the text
        // behind the grid (which sits at the start, and blinked there). The
        // grid does not paint the cursor into the cells: the view draws it
        // as its caret, so there is one cursor.
        if win.is_terminal_buffer(buffer) && !win.split_terminal_scrollback(leaf, buffer) {
            caret = win.paint_terminal_grid(&mut cells, leaf, buffer, area, false);
        }
        crate::app::shell_host::settle_pane_view(self, window, leaf, area, rows, caret, true);
        let win = self.windows.get(&window)?;
        let state = win.buffers.get(&buffer)?;
        let gutter = state.margins.left_total_width() as u16;
        let len = state.buffer.len();
        let lines = state.buffer.line_count();
        let view = win
            .buffers
            .split_view_states()
            .and_then(|m| m.get(&leaf))
            .and_then(|s| s.buffer_state(buffer));
        let top_byte = view.map(|v| v.viewport.top_byte()).unwrap_or(0);
        let selection =
            view.is_some_and(|v| v.cursors.iter().any(|(_, c)| c.selection_range().is_some()));
        let top_line = lines.map(|_| state.buffer.get_line_number(top_byte));
        Some(PaneRender {
            buffer,
            cells,
            gutter,
            caret,
            top_byte,
            len,
            top_line,
            lines,
            selection,
        })
    }

    /// A press at a cell of `pane` as the client was last drawn: the
    /// editor's own click (focus, caret, Shift-extend, plugin hooks, line
    /// targets), then a word for a double press and a line for a triple.
    pub fn press_client_pane(
        &mut self,
        pane: &mut ClientPane,
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
        clicks: u8,
    ) {
        let leaf = pane.leaf;
        let rect = Rect::new(0, 0, pane.width, pane.height);
        self.with_client_pane(pane, |ed| {
            let Some(buffer) = ed.active_window().pane_buffer(leaf) else {
                return;
            };
            let byte = ed
                .active_window()
                .pane_view(leaf)
                .and_then(|v| v.byte_at(col, row));
            if let Err(e) = ed.handle_editor_click(byte, col, row, leaf, buffer, rect, modifiers) {
                tracing::warn!("client pane press: {e}");
            }
            let word_or_line = match clicks {
                2 => Some(crate::input::keybindings::Action::SelectWord),
                n if n >= 3 => Some(crate::input::keybindings::Action::SelectLine),
                _ => None,
            };
            if let Some(action) = word_or_line {
                if let Err(e) = ed.handle_action(action) {
                    tracing::warn!("client pane press: {e}");
                }
            }
        });
    }

    /// The pointer dragged to a cell of `pane` with the button down: the
    /// caret follows it, its selection anchored where the press put it.
    pub fn drag_client_pane(&mut self, pane: &mut ClientPane, col: u16, row: u16) {
        let leaf = pane.leaf;
        self.with_client_pane(pane, |ed| {
            let Some(buffer) = ed.active_window().pane_buffer(leaf) else {
                return;
            };
            let Some(byte) = ed
                .active_window()
                .pane_view(leaf)
                .and_then(|v| v.byte_at(col, row))
            else {
                return;
            };
            ed.seat_buffer_cursor_selecting(buffer, byte, true);
        });
    }

    /// Scroll `pane` by `delta` visual lines (negative is up). The client's
    /// own: the cursor stays where it is.
    pub fn scroll_client_pane(&mut self, pane: &mut ClientPane, delta: i32) {
        let leaf = pane.leaf;
        self.with_client_pane(pane, |ed| {
            if let Some(buffer) = ed.active_window().pane_buffer(leaf) {
                ed.active_window_mut()
                    .scroll_split_surface(leaf, buffer, delta);
            }
        });
    }

    /// Give `leaf` the focus, showing `buffer` (one of its tabs) in it.
    pub fn show_in_pane(&mut self, leaf: LeafId, buffer: BufferId) {
        self.focus_split(leaf, buffer);
    }

    /// Scroll `pane` so its window starts `fraction` (0..1) of the way down
    /// the buffer — by line when the buffer knows its lines, else by byte,
    /// at the start of the line there. The client's own, like a wheel scroll.
    pub fn scroll_client_pane_to(&mut self, pane: &mut ClientPane, fraction: f64) {
        let leaf = pane.leaf;
        let f = fraction.clamp(0.0, 1.0);
        self.with_client_pane(pane, |ed| {
            let Some(buffer) = ed.active_window().pane_buffer(leaf) else {
                return;
            };
            ed.active_window_mut()
                .buffers
                .with_buffer_and_split(buffer, leaf, |state, view| {
                    let top = match state.buffer.line_count() {
                        Some(n) if n > 0 => {
                            let line = ((f * n as f64) as usize).min(n - 1);
                            state.buffer.line_start_offset(line).unwrap_or(0)
                        }
                        _ => {
                            let byte = (f * state.buffer.len() as f64) as usize;
                            state
                                .buffer
                                .line_iterator(byte, 80)
                                .next_line()
                                .map_or(byte, |(start, _)| start)
                        }
                    };
                    view.viewport.set_top_byte(top);
                    view.viewport.set_top_view_line_offset(0);
                    view.viewport.set_skip_ensure_visible();
                });
        });
    }

    /// Run `f` — a key, an action, typed text — as `pane`'s client editing
    /// in it: its pane focused, its scroll and rows in force, so motion by
    /// visual line walks the rows the client sees.
    pub fn in_client_pane<T>(
        &mut self,
        pane: &mut ClientPane,
        f: impl FnOnce(&mut Editor) -> T,
    ) -> Option<T> {
        let leaf = pane.leaf;
        let buffer = self.active_window().pane_buffer(leaf)?;
        pane.fit_terminal = true;
        if self
            .active_window()
            .buffers
            .splits()
            .map(|(m, _)| m.active_split())
            != Some(leaf)
        {
            self.focus_split(leaf, buffer);
        }
        self.with_client_pane(pane, f)
    }
}
