use super::lsp_status::compose_lsp_status;
use super::*;
use crate::config::FileExplorerSide;

/// The bottom-row visibility facts (see [`Editor::bottom_row_flags`]):
/// one computation consumed by the paint-time frame split and every
/// event-time row derivation, instead of four hand-copied spellings.
pub(crate) struct BottomRowFlags {
    pub prompt_is_overlay: bool,
    pub has_suggestions: bool,
    pub has_file_browser: bool,
    pub prompt_row_visible: bool,
}

/// The explorer as the frame builds it: its content and the chrome its
/// section wears, before the column it sits in has been assembled.
struct ExplorerSection {
    kind: crate::view::shell::file_explorer::Explorer,
    title: String,
    title_theme: String,
    border_theme: String,
    close_theme: String,
    rows: u16,
    focused: bool,
}

/// How far the `lines_changed` walk will go to close a run of non-blank lines
/// for a composing buffer, in either direction.
///
/// A reflowing renderer needs whole blocks: `markdown_compose` joins a
/// paragraph's source lines into one flowed block and wraps them together, and
/// it can only do that for lines it is offered in the same batch. So a batch
/// for such a buffer is *run-closed* — it never carries a line without the rest
/// of its blank-line-delimited run — and this bounds the extra walking that
/// costs.
///
/// A run longer than this is left un-closed rather than dragged into every
/// batch that touches it: prose paragraphs are a handful of lines, and a file
/// whose lines never blank out (a wrapped log, a minified blob) would otherwise
/// turn each one-line scroll into a walk over the whole run. The consumer then
/// sees a line whose predecessor is missing, which it already has to treat as a
/// block head.
const FLOW_RUN_MAX_LINES: usize = 200;

/// Whether a line separates two runs: empty, or nothing but whitespace.
fn line_is_blank(content: &str) -> bool {
    content.trim().is_empty()
}

/// How far back the run scan reads to find the blank line above the viewport.
///
/// A byte window rather than a line count so the scan is one read and one
/// backwards pass per frame instead of one piece-tree lookup per line: a file
/// whose lines never blank out would otherwise pay for the whole run on every
/// frame, and this is the draw path.
const FLOW_RUN_LOOKBEHIND_BYTES: usize = 8192;

/// Start byte of the run of non-blank lines containing `top_byte`'s line.
///
/// Returns that line's own start when it opens the run, when it is itself
/// blank, and when the run reaches further back than the scan window — the
/// consumer then sees a line whose predecessor is absent from the batch, which
/// is exactly the "treat it as a block head" case it already handles.
fn flow_run_start(buffer: &crate::model::buffer::Buffer, top_byte: usize) -> usize {
    let top_line = buffer.get_line_number(top_byte);
    let Some(line_start) = buffer.line_start_offset(top_line) else {
        return top_byte;
    };
    if line_start == 0 {
        return line_start;
    }
    let window = line_start.min(FLOW_RUN_LOOKBEHIND_BYTES);
    let from = line_start - window;
    let bytes = buffer.slice_bytes(from..line_start);
    if bytes.len() != window {
        return line_start;
    }
    // Splitting on `\n` is UTF-8 safe: no continuation byte can equal it.
    let mut run_start = line_start;
    let mut end = window;
    while end > 0 {
        // `end` is one past the previous line's terminator.
        let mut content_end = end - 1;
        if content_end > 0 && bytes[content_end - 1] == b'\r' {
            content_end -= 1;
        }
        let Some(nl) = bytes[..content_end].iter().rposition(|b| *b == b'\n') else {
            // The window ran out mid-line, unless it starts at the buffer's
            // own beginning — where there is no line above to look at.
            if from == 0 && !bytes[..content_end].iter().all(u8::is_ascii_whitespace) {
                run_start = from;
            }
            break;
        };
        let start = nl + 1;
        if bytes[start..content_end]
            .iter()
            .all(u8::is_ascii_whitespace)
        {
            break;
        }
        run_start = from + start;
        end = start;
    }
    run_start
}

/// Turns the fold's painted items into theme-key runs for the inspector.
///
/// **The fold reports a rectangle and a name; the grammar is this side's.** A
/// described surface's theme name is written in `shell_theme`'s notation
/// (`fg/bg+attrs`, a literal, or a plugin's key over a fallback), so splitting
/// it into the key pair `ThemeRun` carries has to happen where that notation is
/// understood — not inside `view::shell::fold`, which is meant to know nothing
/// about the editor's theme vocabulary.
///
/// One recorder for the whole display list, replacing the per-surface
/// `provenance_runs` walks that each newly described surface would otherwise
/// have had to grow. Later items overwrite earlier ones, which is the paint
/// order the inspector wants.
///
/// **The region is generic on purpose.** `ThemeRun::region` is a surface label
/// ("Status Bar", "Editor Content") and the fold does not know which surface an
/// item belongs to — an item carries a key, a rect and a theme, not a
/// province. Deriving one from key prefixes would be a stringly guess that goes
/// stale the moment a key is renamed. What the theme inspector's popup and its
/// "Open in Theme Editor" action actually consume is the *key pair*, which is
/// exact; the label is decoration, and an honest generic one beats a confident
/// wrong one.
struct FoldProvenance {
    runs: Vec<crate::app::types::ThemeRun>,
}

impl crate::view::shell::fold::ProvenanceSink for FoldProvenance {
    fn item(
        &mut self,
        rect: ratatui::layout::Rect,
        clip: ratatui::layout::Rect,
        theme: &fresh_ui::ThemeKey,
    ) {
        use std::borrow::Cow;
        let vis = rect.intersection(clip);
        if vis.width == 0 || vis.height == 0 {
            return;
        }
        let (fg, bg) = crate::app::shell_host::shell_theme::names(theme.as_str());
        if fg.is_none() && bg.is_none() {
            return;
        }
        let fg = fg.map(Cow::Owned);
        let bg = bg.map(Cow::Owned);
        for y in vis.y..vis.y.saturating_add(vis.height) {
            self.runs.push(crate::app::types::ThemeRun {
                x: vis.x,
                y,
                w: vis.width,
                fg_key: fg.clone(),
                bg_key: bg.clone(),
                region: Cow::Borrowed("Chrome"),
            });
        }
    }
}

impl Editor {
    /// Render the topmost global popup at its computed area and register its
    /// click region in `global_popup_areas`. Shared by the generic
    /// global-popup slot and the workspace-trust modal band so the area math
    /// lives in exactly one place.
    fn cache_top_global_popup_area(&mut self) {
        if self.global_popups.top().is_none() {
            return;
        }
        let top_idx = self.global_popups.all().len() - 1;
        // The tree's answer. The global top is the last entry the description
        // carries — the buffer's stack, then this over it.
        let (buffer_n, _) = self.popup_counts();
        let popup_area = self
            .popup_rects()
            .get(buffer_n)
            .copied()
            .unwrap_or_default();
        let popup = self.global_popups.top().expect("checked just above");
        let desc_height = popup.description_height();
        let inner_area = if popup.bordered {
            ratatui::layout::Rect {
                x: popup_area.x + 1,
                y: popup_area.y + 1 + desc_height,
                width: popup_area.width.saturating_sub(2),
                height: popup_area.height.saturating_sub(2 + desc_height),
            }
        } else {
            ratatui::layout::Rect {
                x: popup_area.x,
                y: popup_area.y + desc_height,
                width: popup_area.width,
                height: popup_area.height.saturating_sub(desc_height),
            }
        };
        let num_items = match &popup.content {
            crate::view::popup::PopupContent::List { items, .. } => items.len(),
            _ => 0,
        };
        let scroll_offset = popup.scroll_offset;
        self.active_chrome_mut().global_popup_areas.push((
            top_idx,
            popup_area,
            inner_area,
            scroll_offset,
            num_items,
        ));
    }

    /// Ask for another frame because plugin work was deferred out of this one.
    /// The drawing itself never blocks on the plugin lock — every hook site
    /// inside the draw uses `try_read` and skips on contention — so anything
    /// skipped has to be guaranteed a retry. (The pre-layout drain at the top
    /// of `render` does take the lock; it runs before any drawing, which is the
    /// point of doing it there.)
    fn request_plugin_render(&mut self) {
        #[cfg(feature = "plugins")]
        {
            self.plugin_render_requested = true;
        }
    }

    /// Render the editor to the terminal
    pub fn render(&mut self, frame: &mut Frame) {
        let _span = tracing::info_span!("render").entered();
        let size = frame.area();

        {
            let _s = tracing::info_span!("pre_layout_drain").entered();
            self.drain_pre_layout_plugin_commands();
        }

        for window in self.windows.values_mut() {
            window.sync_terminal_titles();
            window.enforce_terminal_grid_wrap();
        }

        // The boolean state flags menu `when` conditions read. Refreshed
        // *before* anything is described, because the menu bar's labels are
        // now part of the frame's description, built at the top of this
        // method — the Explorer menu appears only while the sidebar has focus,
        // and a stale context showed it a frame too long. It used to sit just
        // above the old late `render_menu_bar` call, which is where the
        // description was built from.
        self.update_menu_context();

        // Carve a full-height left column for a docked floating panel
        // (e.g. the orchestrator dock) out of the screen *before* the
        // chrome lays itself out, so the menu bar, splits, and status
        // bar all sit to the dock's right. `chrome_area` is the region
        // the rest of `render` lays into; `dock_area` (if any) is
        // painted last alongside the centered-overlay path.
        let (dock_area, chrome_area) = self.compute_dock_split(size);

        // Let active animations snapshot the previous frame's buffer.
        // We can't read the live `frame.buffer_mut()` — ratatui resets it
        // before each draw — so the editor keeps a post-apply clone of
        // the last frame and hands it in.
        let previous_frame = self.last_rendered_frame.take();
        self.active_window_mut()
            .animations
            .capture_before_all(previous_frame.as_ref());
        self.last_rendered_frame = previous_frame;

        // Save frame dimensions for recompute_layout (used by macro replay)
        self.active_chrome_mut().last_frame.width = size.width;
        self.active_chrome_mut().last_frame.height = size.height;

        // Reset per-cell theme key map for this frame
        self.active_chrome_mut().reset_cell_theme_map();

        // Hand over whatever lines a playing-out wheel gesture owes by
        // now, before anything is laid out, so they land in this frame.
        // A multi-line notch walks across several frames this way, which
        // is what makes it read as a slide — and gives the scroll fade a
        // row at a time to work with.
        self.step_pending_wheel_scroll();

        self.pre_sync_and_scroll_sync();

        // NOTE: Viewport sync with cursor is handled by split_rendering.rs which knows the
        // correct content area dimensions. Don't sync here with incorrect EditorState viewport size.

        {
            let _s = tracing::info_span!("request_semantic_ranges").entered();
            self.request_semantic_ranges_for_visible_splits();
        }

        {
            let _s = tracing::info_span!("prepare_visible_buffers").entered();
            self.prepare_visible_buffers_for_render();
        }

        // Refresh search highlights only during incremental search (when prompt is active)
        // After search is confirmed, overlays exist for ALL matches and shouldn't be overwritten
        let is_search_prompt_active = self.active_window().prompt.as_ref().is_some_and(|p| {
            matches!(
                p.prompt_type,
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch
            )
        });
        if is_search_prompt_active {
            // Highlight what the bar currently holds, not the committed
            // query: F3/Shift+F3 keep the bar open (issue #2111), so the user
            // can edit the query after jumping, and history navigation swaps
            // the input without re-highlighting. An empty bar highlights
            // nothing.
            let query = self
                .active_window()
                .prompt
                .as_ref()
                .map(|p| p.input_str().to_string())
                .unwrap_or_default();
            self.update_search_highlights(&query);
        }

        // Hide status bar when suggestions popup or file browser
        // popup is shown — those popups float just above the prompt
        // line, and a visible status bar wedged between them looks
        // wrong. Floating-overlay prompts (Live Grep, issue #1796)
        // are exempt because their suggestions live inside the
        // centred frame, not above the bottom row.
        // The prompt-row flag is read by `shell_frame`, which owns the frame's
        // shape; the two below are read directly for painting decisions.
        let BottomRowFlags {
            prompt_is_overlay: _,
            has_suggestions,
            has_file_browser,
            prompt_row_visible: _,
        } = self.bottom_row_flags();

        // The frame's geometry comes from the migration shell: one `fresh-ui`
        // description, laid out once, giving every region its rectangle. This
        // replaced a vertical `Layout` over five rows plus a horizontal carve
        // for the sidebar. The two agreed rect for rect over every visibility
        // combination and terminal size — `tests/ui_shell_frame_parity.rs` is
        // the standing proof, and it keeps both derivations honest now that
        // only one of them runs here.
        // See `docs/internal/retained-mode-ui.md` §3.1.
        // The settings search list's window, from the band the tree placed
        // last frame. This is the one mutation the description needs made
        // *before* it is built: the row it describes says "(1-3 of 298)", and
        // three is how many results the band has room for.
        //
        // The band is the page's — the same one the cards fill when no search
        // is running — so its height is known on every frame the dialog is
        // open, including the first frame a query has results on. What was
        // here read the *box* and re-derived the band from it by mirroring
        // the painter's arithmetic: subtract the border, the search row, the
        // gap and a footer that is two rows wide and seven narrow, then in the
        // narrow case subtract the footer a second time because the painter
        // did. None of that is anyone's arithmetic any more.
        //
        // The list's own window is the better answer and is preferred where
        // there is one; the band is what the *first* frame of a search has,
        // since the list does not exist until the description that reports
        // its window has already been built.
        let window = self
            .shell_ui
            .as_ref()
            .and_then(|ui| ui.find_by_key(&crate::view::shell::settings::results_key()))
            .map(|el| self.shell_ui.as_ref().expect("checked").scroll(el).1.h)
            .filter(|h| *h > 0);
        let band = self
            .panel_rect(&crate::view::shell::settings::panel_key())
            .map(|r| r.height / crate::view::shell::settings::RESULT_ROWS);
        if let Some(n) = window.or(band) {
            if let Some(s) = self.settings_state.as_mut() {
                s.search_max_visible = (n as usize).max(1);
            }
        }
        // What the body's window turned out to be. Every answer the settings
        // state used to compute from a second copy of every item's height —
        // how far it can scroll, how much a page-down moves, which card is at
        // the top — is read off the column that laid the cards out.
        self.refresh_settings_body_window();
        let shell = self.shell_frame((dock_area, chrome_area));
        self.lay_out_shell(shell.clone(), fresh_ui::Size::new(size.width, size.height));
        let ui = self
            .shell_ui
            .as_ref()
            .expect("the shell tree is taken and returned within one frame");
        let regions = crate::view::shell::frame::regions_of(ui, size);
        // **The frame's one geometry pass, for the panes.** Every pane's box
        // and content slot, off the tree just laid out. The plugin hooks below,
        // the body painter's pass and the pane `Host`s all read this; nothing
        // on the render path lays the grid out again to ask where a pane is.
        // See `view::shell::geometry`.
        let pane_rects =
            crate::view::shell::geometry::PaneRects::read(ui, self.window_panes_leaves(), size);
        // Retained for the callers that ask between frames where a pane is —
        // the same rects this frame paints with. See `Window::pane_rects`.
        self.active_window_mut().set_pane_rects(pane_rects.clone());
        let region = |r: crate::view::shell::frame::HostRegion| -> ratatui::layout::Rect {
            regions
                .iter()
                .find(|(k, _)| *k == r)
                .map(|(_, rect)| *rect)
                .unwrap_or_default()
        };
        // The shell's BACKGROUND band: everything the tree owns that is not a
        // `Layer`, painted *before* every legacy painter so they land on top
        // of it — the mirror of the overlay band at the end of this method.
        //
        // Two passes because there is one display list and many legacy
        // painters, and the legacy painters are not in the list: a native
        // region under them has to be written first, a native overlay over
        // them has to be written last. One pass could only ever serve one of
        // those, which is what confined migration to top-most surfaces.
        // The band itself is folded further down, immediately before the
        // split grid used to be painted — because the grid is painted *by* it
        // now, through `HostPainter`, and the plugin `lines_changed` hooks
        // between here and there add the overlays that paint has to see.
        // Nothing between the two points writes a cell, so deferring the
        // paint changes no pixel; what it buys is one paint of the body
        // instead of a live one here and an unreached copy behind the seam.

        use crate::view::shell::frame::HostRegion;
        let status_bar_area = region(HostRegion::StatusBar);

        // The chrome each pane has, resolved with the shell's description of
        // the same grid — read by the reconcile below and by the body's
        // painter further down, so both lay the panes out the same way.
        let pane_chrome = shell
            .splits
            .as_ref()
            .map(|s| s.chrome.clone())
            .unwrap_or_default();

        // **Every text pane is reconciled here, before anything reads it.**
        // The viewport's size, the byte and row placement of the cursor, the
        // buffer's margins and its wrap index used to be brought up to date
        // *inside* the paint, pane by pane, which is what made the formatter
        // write the model and build its rows up to three times. Now the tree
        // is laid out and `pane_rects` says where every pane is, so each pane
        // is settled once at the content rect it will be painted at — the
        // `lines_changed` hooks below offer the lines the frame will draw,
        // and the fold's text pass is a read. The `BodyState` is the frame's:
        // whether the active pane shows a caret is decided once, here, and
        // the content pass below reads it.
        let lsp_waiting = !self.active_window().pending_completion_requests.is_empty()
            || self
                .active_window()
                .pending_goto_definition_request
                .is_some();
        let hide_cursor = self.hide_cursor();
        let body_state = crate::app::shell_host::BodyState {
            lsp_waiting,
            hide_cursor,
        };
        let mut prepared_grid = {
            let _s = tracing::info_span!("reconcile_panes").entered();
            crate::app::shell_host::reconcile_body(
                self,
                self.active_window,
                body_state,
                &pane_rects,
                size.width,
                &pane_chrome,
            )
        };
        // Note: Tabs are now rendered within each split by SplitRenderer

        // Trigger lines_changed hooks for newly visible lines in all visible buffers
        // This allows plugins to add overlays before rendering
        // Only lines that haven't been seen before are sent (batched for efficiency)
        // Use non-blocking hooks to avoid deadlock when actions are awaiting.
        //
        // `try_read` rather than `read`: a busy plugin thread must never be
        // able to stall the draw on a lock. Losing the race skips this frame's
        // hooks and requests another, so decorations arrive one frame later
        // instead of the whole frame arriving late.
        let plugins_active = self.plugin_manager.try_read().map(|pm| pm.is_active()).ok();
        if plugins_active.is_none() {
            self.request_plugin_render();
        }
        if plugins_active.unwrap_or(false) {
            let _s = tracing::info_span!("render_plugin_hooks").entered();
            let hooks_start = std::time::Instant::now();
            // Get visible buffers and their areas — the boxes the tree placed
            // them in.
            let visible_buffers = pane_rects.visible(
                &self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .visible_leaves(),
            );

            let mut total_new_lines = 0usize;
            for (split_id, buffer_id, split_area) in visible_buffers {
                // Get viewport from SplitViewState (the authoritative source)
                let viewport_top_byte = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .get(&split_id)
                    .map(|vs| vs.viewport.top_byte())
                    .unwrap_or(0);

                // Whether this buffer is being composed in any split, read from
                // the live view states rather than from the plugin snapshot.
                // Travels with the batch (see `HookArgs::LinesChanged`) because
                // the lines below are marked seen as soon as they are handed
                // over: a consumer that had to re-derive this from
                // `getBufferInfo()` would be reading a mirror that is refreshed
                // on the editor thread's own schedule, and would silently drop
                // its only decoration pass whenever it read too early (#2968).
                let composing_in_any_split = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .values()
                    .any(|vs| {
                        vs.buffer_state(buffer_id)
                            .map(|bs| matches!(bs.view_mode, crate::state::ViewMode::PageView))
                            .unwrap_or(false)
                    });

                let __active_id = self.active_window;
                let __win = self
                    .windows
                    .get_mut(&__active_id)
                    .expect("active window must exist");
                // Take a disjoint mut borrow on `seen_byte_ranges` (a sibling
                // field on Window, not part of WindowBuffers) so the closure
                // below can update it alongside the buffer + view-state
                // mutations.
                let seen_ranges_for_win = &mut __win.seen_byte_ranges;
                let plugin_manager = &self.plugin_manager;
                let estimated_line_length = self.config.editor.estimated_line_length;
                let mut hooks_deferred = false;
                let added = __win
                    .buffers
                    .with_buffer_and_view_states(buffer_id, |state, _vs_map| {
                        // Both hooks go through `try_read`: the draw computes
                        // the payload and hands it off, but never waits on the
                        // plugin lock. On contention nothing is marked seen, so
                        // the same lines are offered again next frame — the
                        // payload is deferred, not lost.
                        let Ok(pm_guard) = plugin_manager.try_read() else {
                            hooks_deferred = true;
                            return 0;
                        };
                        // `render_start` has a tiny payload (just the
                        // buffer id) — fire unconditionally so third-party
                        // plugins listening for it still work.
                        pm_guard.run_hook(
                            "render_start",
                            crate::services::plugins::hooks::HookArgs::RenderStart { buffer_id },
                        );

                        let visible_count = split_area.height as usize;

                        let top_byte = viewport_top_byte;
                        let seen_byte_ranges = seen_ranges_for_win.entry(buffer_id).or_default();

                        // Where the walk starts. Normally the viewport's top
                        // line; for a composing buffer, backed up to the start
                        // of the blank-line-delimited run that line belongs to
                        // — see `flow_run_start` for why a reflowing renderer
                        // needs that and nothing else does.
                        let walk_start = if composing_in_any_split {
                            flow_run_start(&state.buffer, top_byte)
                        } else {
                            top_byte
                        };

                        // Every line the walk saw, in order, paired with
                        // whether it had been offered before. Two passes over
                        // it below: one to decide what to send, one to build
                        // the payload. Only a composing buffer keeps more than
                        // the viewport here.
                        struct WalkedLine {
                            line_number: usize,
                            byte_start: usize,
                            byte_end: usize,
                            content: String,
                            seen: bool,
                        }
                        // Source lines to cover the viewport with. One per row,
                        // except that a conceal spanning a line break renders
                        // two source lines as one row — which is how compose
                        // mode reflows a paragraph. The renderer sizes its own
                        // token build the same way, from the same helper, so
                        // the lines this offers the plugin are exactly the ones
                        // the frame will draw; without it the bottom block of a
                        // reflowed viewport is never offered and so never
                        // flows. Self-correcting rather than predictive: the
                        // joins it measures are the ones already on screen, so
                        // a document opens at one line per row and settles over
                        // the next frames as its blocks join.
                        let rows = crate::view::ui::split_rendering::transforms::join_adjusted_visible_count(
                            &state.buffer,
                            &state.conceals,
                            &state.marker_list,
                            &[],
                            top_byte,
                            visible_count,
                            composing_in_any_split,
                        );
                        // A split with no rows draws nothing, so it has nothing
                        // to offer — and the walk below has no stopping rule.
                        if rows == 0 {
                            return 0;
                        }

                        let mut walked: Vec<WalkedLine> = Vec::new();
                        let mut line_number = state.buffer.get_line_number(walk_start);
                        let mut iter = state.buffer.line_iterator(walk_start, estimated_line_length);
                        // End of the last line walked, so the embedded-region
                        // probe below covers exactly the lines we iterated.
                        let mut walked_end = walk_start;
                        // Lines walked ahead of the viewport's own top line:
                        // the run's lead-in, which must not eat the viewport's
                        // line budget.
                        let mut lead_in = 0usize;
                        loop {
                            let Some((line_start, line_content)) = iter.next_line() else {
                                break;
                            };
                            let byte_end = line_start + line_content.len();
                            walked_end = byte_end;
                            let blank = line_is_blank(&line_content);
                            if line_start < top_byte {
                                lead_in += 1;
                            }
                            walked.push(WalkedLine {
                                line_number,
                                byte_start: line_start,
                                byte_end,
                                seen: seen_byte_ranges.contains(&(line_start, byte_end)),
                                content: line_content,
                            });
                            line_number += 1;

                            if walked.len() < lead_in + rows {
                                continue;
                            }
                            // The viewport is covered. A composing buffer walks
                            // on to the end of the run it stopped inside, so a
                            // batch never carries half a paragraph — bounded,
                            // because a file whose lines never blank out must
                            // not turn one frame into a walk over the buffer.
                            if !composing_in_any_split
                                || blank
                                || walked.len() >= lead_in + rows + FLOW_RUN_MAX_LINES
                            {
                                break;
                            }
                        }

                        // What to send. A line the plugin has not been offered
                        // before, always; and for a composing buffer, every
                        // other line of that line's run as well, so a per-line
                        // pass that flows a paragraph sees all of it at once
                        // (`markdown_compose`'s joins and block wrap). Runs are
                        // delimited by blank lines, which is markdown's own
                        // paragraph rule and the only one that does not need a
                        // grammar.
                        let mut send: Vec<bool> = walked.iter().map(|l| !l.seen).collect();
                        if composing_in_any_split {
                            let mut i = 0;
                            while i < walked.len() {
                                if line_is_blank(&walked[i].content) {
                                    i += 1;
                                    continue;
                                }
                                let start = i;
                                while i < walked.len() && !line_is_blank(&walked[i].content) {
                                    i += 1;
                                }
                                // A run longer than the cap is not closed here,
                                // so it is left to the plain unseen-lines rule:
                                // the consumer sees a line whose predecessor is
                                // missing and treats it as a block head, which
                                // is wrong-but-stable rather than unbounded work
                                // on every scroll step.
                                if i - start <= FLOW_RUN_MAX_LINES
                                    && send[start..i].iter().any(|s| *s)
                                {
                                    send[start..i].iter_mut().for_each(|s| *s = true);
                                }
                            }
                        }

                        let mut new_lines: Vec<crate::services::plugins::hooks::LineInfo> =
                            Vec::new();
                        let mut fresh_ranges: Vec<(usize, usize)> = Vec::new();
                        for (line, send) in walked.into_iter().zip(send) {
                            if !send {
                                continue;
                            }
                            fresh_ranges.push((line.byte_start, line.byte_end));
                            new_lines.push(crate::services::plugins::hooks::LineInfo {
                                line_number: line.line_number,
                                byte_start: line.byte_start,
                                byte_end: line.byte_end,
                                content: line.content,
                                region: None,
                                table: None,
                            });
                        }

                        let count = new_lines.len();
                        if !new_lines.is_empty() {
                            // What the grammar says about each reported line's
                            // place in the document's structure: whether it sits
                            // inside an embedded-language region (a Markdown
                            // fence, a Vue `<script>`), and where it sits in a
                            // table. These are the properties a decoration
                            // plugin cannot work out for itself — region
                            // membership is not derivable from a line's own text
                            // at all, and a table's first/last row needs the
                            // neighbouring lines, which an edit-sized batch does
                            // not contain. Attached here, alongside the live
                            // coordinates, so a consumer never has to store
                            // structure of its own.
                            //
                            // One probe answers both. It costs nothing for
                            // syntaxes with neither (the overwhelming majority),
                            // and each list is skipped when it has nothing to
                            // report.
                            let structure = state
                                .highlighter
                                .structure_lines_in(&state.buffer, walk_start..walked_end);
                            if !structure.regions.is_empty() {
                                let mut next = 0usize;
                                for line in new_lines.iter_mut() {
                                    // Both sides ascend by line start, so one
                                    // forward walk pairs them.
                                    while next < structure.regions.len()
                                        && structure.regions[next].0 < line.byte_start
                                    {
                                        next += 1;
                                    }
                                    if structure
                                        .regions
                                        .get(next)
                                        .is_some_and(|r| r.0 == line.byte_start)
                                    {
                                        line.region = Some(structure.regions[next].1);
                                    }
                                }
                            }
                            if !structure.tables.is_empty() {
                                let mut next = 0usize;
                                for line in new_lines.iter_mut() {
                                    while next < structure.tables.len()
                                        && structure.tables[next].0 < line.byte_start
                                    {
                                        next += 1;
                                    }
                                    if structure
                                        .tables
                                        .get(next)
                                        .is_some_and(|t| t.0 == line.byte_start)
                                    {
                                        line.table = Some(structure.tables[next].1);
                                    }
                                }
                            }
                            pm_guard.run_hook(
                                "lines_changed",
                                crate::services::plugins::hooks::HookArgs::LinesChanged {
                                    buffer_id,
                                    lines: new_lines,
                                    epoch: state.buffer.version(),
                                    is_composing_in_any_split: composing_in_any_split,
                                },
                            );
                            for range in fresh_ranges {
                                seen_byte_ranges.insert(range);
                            }
                        }
                        count
                    })
                    .unwrap_or(0);
                if hooks_deferred {
                    self.request_plugin_render();
                }
                total_new_lines += added;
            }
            let hooks_elapsed = hooks_start.elapsed();
            tracing::trace!(
                new_lines = total_new_lines,
                elapsed_ms = hooks_elapsed.as_millis(),
                elapsed_us = hooks_elapsed.as_micros(),
                "lines_changed hooks total"
            );

            // Hook replies (AddOverlay, conceals) are NOT collected here.
            // The draw dispatches no plugin commands after this point: the
            // plugin thread answers the hooks above on its own time, the
            // arriving commands set `plugin_render_requested` via the tick
            // drain, and the decorations land on the next frame. One frame
            // of decoration latency is the fixed, universal price for a
            // frame that cannot tear and whose cost does not depend on how
            // much a plugin decided to emit mid-paint.
        }

        let _content_span = tracing::info_span!("render_content").entered();
        // **The split grid is painted by the fold, through the seam.**
        //
        // What stood here was a hundred lines assembling `render_content`'s 28
        // arguments — the `with_all_mut` disjoint split, the preview buffer,
        // the scrollback set, the cell-theme map — and `app::shell_host` held
        // a second copy of the same assembly behind `HostPainter`, reached by
        // nothing, because `fold_native` installs a painter that skips host
        // regions. Two assemblies of one call is a thing that drifts, and this
        // one had: the unreached copy dropped five of the seven results and
        // passed `BodyState::default()`, so it would have painted a grid with
        // no hovered tab.
        //
        // There is one assembly now, in `shell_host::with_grid`, and the
        // display list is what reaches it — one pane at a time, each at the
        // rectangle *layout* gave it rather than one computed beside it. What
        // a pane needs beyond that rectangle rides on the painter below,
        // because `paint_host` carries a target and a rectangle and nothing
        // else.
        // **The content pass, then the tree's paint.** Every text pane's rows
        // and caret are laid out here, after the hooks decorated the lines
        // and before the tree paints, and settled on the pane's leaf — so the
        // leaf places this frame's caret in this frame's display list, and
        // the fold below draws the rows from the same layout (design §3.7.3).
        if let Some(prepared) = prepared_grid.as_mut() {
            let _s = tracing::info_span!("content_pass").entered();
            crate::app::shell_host::content_body(
                self,
                self.active_window,
                body_state,
                size.width,
                &pane_chrome,
                prepared,
            );
        }
        // **The overlay card's preview is settled before the paint, because
        // the paint is what reads it.** Loading the selected match's file and
        // seeding its cursor is state work, and the card's preview band is a
        // host the fold below paints through `paint_card_preview`. This ran
        // after the paint while the chrome was painted in a second pass, and
        // the band came out blank for a frame — the class of ordering the one
        // paint removes, so it moves up here with the other settling.
        if self.bottom_row_flags().prompt_is_overlay {
            self.prepare_overlay_preview();
        }
        // **The layers hanging off the caret are placed before the tree
        // paints.** The content pass settled every pane's caret; the popups
        // anchored to it (the completion list, the hover) are placed against
        // that cell now, so the one paint below carries them where they go.
        self.publish_popup_carets(size);
        self.paint_shell();

        let palette = self.shell_palette();
        let paints = match self.suppress_chrome_cells {
            true => crate::view::shell::fold::Paints::HostsOnly,
            false => crate::view::shell::fold::Paints::All,
        };
        let ui = self
            .shell_ui
            .take()
            .expect("the shell tree is taken and returned within one frame");
        // A frontend that draws the tree's surfaces itself still gets its host
        // regions painted — the panes are cells even on the web. That used to
        // be "skip the fold and call the split renderer separately", which is
        // where the second assembly came from.
        //
        // The painter lives exactly as long as this fold. What a pane needs
        // beyond a rectangle — the frame's hover state, the pass its panes
        // share, the sink they append to — is its, not the editor's.
        // The chrome each pane has, handed over rather than left on the
        // editor for the painter to find. One producer, one consumer, one
        // frame — a field for that is a value that could not be threaded, and
        // it is the same map the description above already carries.
        let mut body = crate::app::shell_host::BodyPainter::new(
            self,
            self.active_window,
            body_state,
            pane_chrome,
            pane_rects,
            prepared_grid,
        );
        // **The frame's one paint.** Both bands, in one pass, through one
        // host painter: the tree's own surfaces in display-list order and the
        // panes, the embeds and the card's preview where their leaves are.
        // Nothing paints between the in-flow content and the layers any
        // more — the terminal grid and the edge fade are the pane's own paint,
        // the placeholder page is described, the modal dimming is a scrim —
        // and what runs after this is an effect over the finished cells.
        let mut provenance = FoldProvenance { runs: Vec::new() };
        crate::view::shell::fold::fold_all(
            ui.spec(),
            frame.buffer_mut(),
            &palette,
            &mut body,
            paints,
            Some(&mut provenance),
        );
        drop(body);
        self.shell_ui = Some(ui);
        // **F.6: one recorder for every described surface**, applied once
        // both bands are folded so the overlay's items land on top of the
        // background's — the paint order the inspector wants.
        let runs = provenance.runs;
        self.active_chrome_mut().apply_theme_runs(&runs);

        drop(_content_span);
        let _post_content_span = tracing::info_span!("render_post_content").entered();

        // Cursor-jump animation: compare the cursor's screen position to
        // the prior frame and animate either when the cursor crossed split
        // panes or moved more than two rows within the same pane. The
        // trail crosses pane separators when the jump is across splits —
        // that's the intended "follow the focus" cue.
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        let pane_caret = self.active_window().pane_caret();
        self.maybe_start_cursor_jump_animation(pane_caret, active_split);

        // Detect viewport changes and fire hooks
        // Compare against previous frame's viewport state (stored in self.active_window().previous_viewports)
        // This correctly detects changes from scroll events that happen before render()
        //
        // `try_read` again: never wait on the plugin lock inside the draw.
        // When the lock is busy the `previous_viewports` update below is
        // skipped too, so the same change is re-detected next frame rather
        // than being silently swallowed.
        // **`previous_viewports` holds the panes that were on screen**, not
        // every view state: a hidden buffer-group panel keeps its leaf and
        // its `SplitViewState`, so snapshotting it too meant a panel shown
        // again matched its own stale entry and reported nothing.
        let on_screen: std::collections::HashSet<crate::model::event::LeafId> = self
            .window_panes()
            .into_iter()
            .map(|(leaf, _)| leaf)
            .collect();
        let mut viewport_hooks_deferred = false;
        let viewport_plugins_active = match self.plugin_manager.try_read() {
            Ok(pm) => pm.is_active(),
            Err(_) => {
                viewport_hooks_deferred = true;
                false
            }
        };
        if viewport_plugins_active {
            for (split_id, view_state) in self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
            {
                if !on_screen.contains(split_id) {
                    continue;
                }
                let current = (
                    view_state.viewport.top_byte(),
                    view_state.viewport.width,
                    view_state.viewport.height,
                );
                // Arriving on screen is itself the change. Skipping it (the
                // old behaviour) left a pane that then never resizes — a
                // buffer group's side panel — with no announcement at all,
                // so a plugin laying its own rows out to the panel width had
                // only a guess to go on.
                let (changed, previous) =
                    match self.active_window().previous_viewports.get(split_id) {
                        Some(previous) => (*previous != current, Some(*previous)),
                        None => (true, None),
                    };
                tracing::trace!(
                    "viewport_changed check: split={:?} current={:?} previous={:?} changed={}",
                    split_id,
                    current,
                    previous,
                    changed
                );
                if changed {
                    // Buffer-group panels are leaves of a stashed `Grouped`
                    // subtree, not of the main split tree, so `get_buffer_id`
                    // comes back empty for them and the hook used to be
                    // dropped — a panel plugin never heard that its panel had
                    // scrolled or been resized. Fall back to the grouped
                    // subtrees for those leaves.
                    let buffer_id = self
                        .windows
                        .get(&self.active_window)
                        .and_then(|w| w.buffers.splits())
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .get_buffer_id((*split_id).into())
                        .or_else(|| {
                            self.active_window()
                                .grouped_subtrees
                                .values()
                                .find_map(|node| {
                                    if let crate::view::split::SplitNode::Grouped {
                                        layout, ..
                                    } = node
                                    {
                                        layout
                                            .find((*split_id).into())
                                            .and_then(|leaf| leaf.buffer_id())
                                    } else {
                                        None
                                    }
                                })
                        });
                    if let Some(buffer_id) = buffer_id {
                        // Compute top_line if line info is available
                        let top_line = self
                            .windows
                            .get(&self.active_window)
                            .map(|w| &w.buffers)
                            .expect("active window present")
                            .get(&buffer_id)
                            .and_then(|state| {
                                if state.buffer.line_count().is_some() {
                                    Some(
                                        state
                                            .buffer
                                            .get_line_number(view_state.viewport.top_byte()),
                                    )
                                } else {
                                    None
                                }
                            });
                        tracing::debug!(
                            "Firing viewport_changed hook: split={:?} buffer={:?} top_byte={} top_line={:?}",
                            split_id,
                            buffer_id,
                            view_state.viewport.top_byte(),
                            top_line
                        );
                        let Ok(pm) = self.plugin_manager.try_read() else {
                            viewport_hooks_deferred = true;
                            continue;
                        };
                        pm.run_hook(
                            "viewport_changed",
                            crate::services::plugins::hooks::HookArgs::ViewportChanged {
                                split_id: (*split_id).into(),
                                buffer_id,
                                top_byte: view_state.viewport.top_byte(),
                                top_line,
                                width: view_state.viewport.width,
                                height: view_state.viewport.height,
                            },
                        );
                    }
                }
            }
        }

        // A hook we couldn't deliver this frame must stay pending: leaving
        // `previous_viewports` untouched is what makes the change re-detected
        // next frame instead of vanishing.
        if viewport_hooks_deferred {
            self.request_plugin_render();
        }

        // Update previous_viewports for next frame's comparison.
        // Take both `previous_viewports` and the split view-states from
        // the same `__win` borrow so the iterator and the inserts share
        // a single mutable borrow on `self.windows`.
        let skip_viewport_snapshot = viewport_hooks_deferred;
        let __vp_win = self
            .windows
            .get_mut(&self.active_window)
            .expect("active window present");
        if !skip_viewport_snapshot {
            __vp_win.previous_viewports.clear();
        }
        let (_, __vp_vs_map) = __vp_win
            .buffers
            .splits()
            .expect("active window must have a populated split layout");
        let snapshot: Vec<(LeafId, (usize, u16, u16))> = __vp_vs_map
            .iter()
            .filter(|(split_id, _)| on_screen.contains(split_id))
            .map(|(split_id, view_state)| {
                (
                    *split_id,
                    (
                        view_state.viewport.top_byte(),
                        view_state.viewport.width,
                        view_state.viewport.height,
                    ),
                )
            })
            .collect();
        if !skip_viewport_snapshot {
            for (split_id, vp) in snapshot {
                __vp_win.previous_viewports.insert(split_id, vp);
            }
        }

        // **The buffer answers where its caret is, and the layers hanging off
        // it are placed.** This is the seam the popup wave needed: the chrome's
        // geometry has to exist before the splits can be laid out, the splits
        // before the caret's screen position is known, and the caret before a
        // popup anchored to it can go anywhere. Below the frame's layout,
        // which retained `Window::pane_rects`, for exactly the reason the
        // block above it is.
        //
        // When S5 puts the split grid in the tree the buffer's leaf has a
        // rectangle at layout time, the caret becomes an ordinary keyed
        // element, and this call site goes with it.
        // A split that changed size takes any widget panel mounted in it
        // with it: an auto-sized (`visible_rows: None`) list or tree was
        // windowed to the old row budget, and the rects published just
        // above are the first place the panel's new geometry is known.
        // Re-render those panels against it and ask for the frame that
        // shows the result, rather than leaving a grown panel with blank
        // rows under its short list until something else repaints it.
        // Must stay below the frame's layout (`set_pane_rects`):
        // `widget_panels_with_stale_height` reads this frame's rects, and
        // against the previous frame's it would find nothing stale on the
        // one frame that matters.
        let restaled_panels = self.widget_panels_with_stale_height();
        if !restaled_panels.is_empty() {
            for panel_key in &restaled_panels {
                self.rerender_widget_panel(panel_key);
            }
            self.request_plugin_render();
        }

        // Promote any deferred virtual-buffer animations whose Rect is now
        // known. Done here (after the frame's layout retained the pane
        // rects, before
        // apply_all runs at the end of render) so the first frame of the
        // effect lands on the same paint that made the buffer visible.
        self.drain_pending_vb_animations();

        // Initialize popup/suggestion layout state (rendered after status bar below)
        self.active_chrome_mut().suggestions_area = None;
        self.active_chrome_mut().suggestions_outer_area = None;

        // Clone all immutable values before the mutable borrow
        let display_name = self
            .active_window()
            .buffer_metadata
            .get(&self.active_buffer())
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| "[No Name]".to_string());

        // Reflect the active buffer in the terminal window/tab title. Only
        // writes when the title actually changes so we don't flood stdout
        // with OSC sequences every frame.
        self.update_terminal_title(&display_name);

        // Status bar (hidden when toggled off, or when a suggestions/file-
        // browser popup covers the bottom row).
        self.publish_status_bar(status_bar_area, has_suggestions, has_file_browser);

        // Render file browser popup or suggestions popup AFTER status bar + prompt,
        // so they overlay on top of both (fixes bottom border being overwritten by status bar)
        self.settle_prompt_suggestions();

        // Cursor-anchored buffer popups (completion, hover, signature help):
        // recompute their areas for hit-testing and paint them.
        self.cache_buffer_popup_areas();

        // Render editor-level popups (e.g. plugin action popups) on top of any
        // buffer content so they stay visible across buffer switches and over
        // virtual buffers (Dashboard, diagnostics) that own the whole split.
        // These don't need cursor-relative positioning — they all use absolute
        // positions like BottomRight or Centered.
        //
        // Queue semantics: concurrent action popups stack in `global_popups`,
        // but only the top one renders & receives input. Deeper popups
        // surface as the top is resolved — the alternative (drawing all at
        // the same BottomRight slot) makes them illegible.
        self.active_chrome_mut().global_popup_areas.clear();
        // The workspace-trust prompt is a blocking modal: it renders later in
        // the dedicated modal z-band (alongside settings / wizard) on a dimmed
        // backdrop, so it can't be lost amongst dashboard/explorer chrome.
        // Everything else on the global stack renders here, above buffer content.
        let top_is_trust_modal = self.workspace_trust_on_top();
        if !top_is_trust_modal {
            self.cache_top_global_popup_area();
        }

        // The full-screen modals (settings, calibration wizard, keybinding
        // editor, event-debug dialog) and the blocking workspace-trust prompt
        // each dim the *entire* frame — the dock included — and centre in the
        // full window, so they are rendered at the very end of this method,
        // after the dock and floating panels, rather than here, where the
        // dock's later pass would overpaint their left edge. See the bottom of
        // `render` and `render_panels_and_modals`.

        // The menu does not paint here: the bar row is a native region and
        // its dropdowns are layers, and the fold records their provenance
        // for the inspector like every other item's.

        // Where a migrated surface asked for the terminal caret, if any. See
        // the commit at the end of this method.
        // The shell's OVERLAY band: its `Layer`s, painted after every legacy
        // painter because paint order is what puts a menu on top. Its
        // background band was painted at the top of this method, before them,
        // for the mirror-image reason. Host regions are skipped in both — they
        // are painted by their own code, into the rectangles this same layout
        // produced.
        //
        // This replaced `render_context_menus`: a context menu is an ordinary
        // `Layer` in the tree now, not a separately-ranked surface painted by
        // its own function.
        //
        // Suppression is a *fold* decision, not a tree one. The web bridge
        // draws chrome natively and wants the cells to carry buffer interiors
        // only — but it still needs the geometry, and the tree is where
        // geometry lives now. So the description is built either way and only
        // the cell-writing half is skipped, which is what "backends are folds
        // over the display list" buys: two backends, one layout.
        // The full-screen modals' remaining paint, **before** the overlay band
        // rather than after it. What is left of `render_modal_overlays` for
        // the settings dialog is the box and the one divider column between
        // its two panes (`view::settings::render`, 270 lines); everything
        // inside it — both panels, the entry stack, the search row, the footer
        // and every prompt — is the tree's. Order still matters for what is
        // left: a `Block` fills the rectangle it borders, so a painter that
        // ran after the fold wiped the described rows inside it and the help
        // overlay never appeared at all.
        //
        // This is the rule the overlay band already states for every other
        // legacy painter — "painted after every legacy painter, because paint
        // order is what puts a menu on top" — applied to the last painter that
        // was still exempt from it. It was exempt because it used to be the
        // topmost surface there was; it is not, now that the chrome over it is
        // the tree's.
        //
        // The dock's cells are painted just below, still before the band, so
        // the dimming this pass applies to it is re-applied by
        // `render_panels_and_modals` once those cells exist. The modal itself
        // lays into the chrome column beside the dock, so nothing of it is at
        // risk from that later paint.
        self.settle_modal_viewports();

        // Chrome theme-key provenance is the fold's (`FoldProvenance`, applied
        // above): every described surface files its items' rects and keys as
        // it folds. What is still blank is a *tier*, not a surface: an item
        // whose ink resolved to literal colours files nothing — defect
        // `Paint::Lit` in `docs/internal/retained-mode-ui.md` §3.3.

        // Software mouse cursor (GPM) and keyboard-capture dimming — both
        // read already-painted cells, so they run after the main draw.
        self.render_software_cursor_and_capture(frame, size);

        // **The frame's caret is the display list's cursor.** Every surface
        // that has one placed it there — the pane's leaf from the caret its
        // text pass settled, the prompt row's input run, the explorer's row,
        // a panel's field — and the library resolved them: the last placed
        // wins, and a layer painted over a cursor ends it. Nothing here asks
        // whether something covered the cell; that was answered when the
        // list was painted.
        if let Some(c) = self
            .shell_ui
            .as_ref()
            .and_then(|ui| ui.spec().cursor)
            .filter(|c| c.visible)
        {
            frame.set_cursor_position((c.pos.x.max(0) as u16, c.pos.y.max(0) as u16));
        }

        // Frame-buffer animations run after the main draw so they mutate the
        // final paint.
        self.active_window_mut()
            .animations
            .apply_all(frame.buffer_mut());

        // Keep the post-apply paint so the next frame's effects can push
        // it out of view. Cloned because ratatui resets the current
        // buffer before the next draw.
        self.last_rendered_frame = Some(frame.buffer_mut().clone());

        // Convert all colors for terminal capability (256/16 color fallback).
        // Dead last, so the layers painted above — dock, full-screen modals,
        // animations — go through the fallback too instead of emitting
        // truecolor SGR on a terminal that cannot render it.
        crate::view::color_support::convert_buffer_colors(
            frame.buffer_mut(),
            self.color_capability,
        );
    }

    /// The Confirm-each option's live value when it is shown (replace
    /// modes only), `None` when hidden. Read by
    /// [`Editor::search_options_content`], which is the one place the row's
    /// content is derived.
    pub(crate) fn search_confirm_shown(&self) -> Option<bool> {
        self.active_window().prompt.as_ref().and_then(|p| {
            if matches!(
                p.prompt_type,
                PromptType::ReplaceSearch
                    | PromptType::Replace { .. }
                    | PromptType::QueryReplaceSearch
                    | PromptType::QueryReplace { .. }
            ) {
                Some(self.active_window().search_confirm_each)
            } else {
                None
            }
        })
    }

    /// The search-options row's content THIS instant: which toggles are on
    /// the row, what each says, whether it is checked, and what the pointer
    /// is on. `None` when no search-style prompt is up.
    ///
    /// Content only. There is no `x` and no width here, because the tree
    /// measures this row — which is what replaced `SearchOptionsLayout`, a
    /// span table computed once by `compute` for event handling and a second
    /// time by the painter for cells, reconciled by a `debug_assert_eq!` that
    /// release builds compiled out. The spans are now read back off the laid
    /// out tree by [`crate::view::shell::search_options::option_spans`].
    pub(crate) fn search_options_content(
        &self,
    ) -> Option<crate::view::shell::search_options::SearchOptions> {
        use crate::view::shell::search_options::{Piece, SearchOption, SearchOptions, Toggle};
        if !self.active_prompt_has_search_options() {
            return None;
        }
        let confirm = self.search_confirm_shown();
        let win = self.active_window();
        let keybindings = self.keybindings.read().unwrap();
        // The search-option toggles live in the SearchPrompt context; fall
        // back to Prompt then Global so a user override in either still
        // surfaces in the hint.
        let shortcut = |a: &crate::input::keybindings::Action| -> Option<String> {
            use crate::input::keybindings::KeyContext;
            keybindings
                .get_keybinding_for_action(a, KeyContext::SearchPrompt)
                .or_else(|| keybindings.get_keybinding_for_action(a, KeyContext::Prompt))
                .or_else(|| keybindings.get_keybinding_for_action(a, KeyContext::Global))
        };
        let toggle = |option: SearchOption, label: String, checked: bool| {
            Piece::Toggle(Toggle {
                option,
                label,
                shortcut: shortcut(&option.action()),
                checked,
            })
        };
        let mut pieces = vec![
            toggle(
                SearchOption::CaseSensitive,
                t!("search.case_sensitive").to_string(),
                win.search_case_sensitive,
            ),
            toggle(
                SearchOption::WholeWord,
                t!("search.whole_word").to_string(),
                win.search_whole_word,
            ),
            toggle(
                SearchOption::Regex,
                t!("search.regex").to_string(),
                win.search_use_regex,
            ),
        ];
        if let Some(confirm) = confirm {
            // The capture-group reminder only makes sense where captures can
            // be spent: a replace prompt with regex on.
            if win.search_use_regex {
                pieces.push(Piece::Hint(" \u{2502} $1,$2,\u{2026}".to_string()));
            }
            pieces.push(toggle(
                SearchOption::ConfirmEach,
                t!("search.confirm_each").to_string(),
                confirm,
            ));
        }
        Some(SearchOptions {
            pieces,
            // The shell's own hover, not the legacy walk's: this row's chrome
            // box is deleted, so the walk has nothing to say about it.
            hovered: match self.shell_hover {
                Some(HoverTarget::SearchOptionCaseSensitive) => Some(SearchOption::CaseSensitive),
                Some(HoverTarget::SearchOptionWholeWord) => Some(SearchOption::WholeWord),
                Some(HoverTarget::SearchOptionRegex) => Some(SearchOption::Regex),
                Some(HoverTarget::SearchOptionConfirmEach) => Some(SearchOption::ConfirmEach),
                _ => None,
            },
        })
    }

    /// Where layout put each search-option toggle, read off the retained tree.
    ///
    /// The read-back the web projection uses. `None` when the row is hidden —
    /// the toggles have no element then, so there is nothing to read.
    pub(crate) fn search_option_spans_now(
        &self,
    ) -> Option<
        Vec<(
            crate::view::shell::search_options::SearchOption,
            ratatui::layout::Rect,
        )>,
    > {
        if !self.active_prompt_has_search_options() {
            return None;
        }
        let ui = self.shell_ui.as_ref()?;
        let frame = self.active_chrome().last_frame;
        let size = ratatui::layout::Rect::new(0, 0, frame.width, frame.height);
        Some(crate::view::shell::search_options::option_spans(ui, size))
    }

    /// Where layout put each clickable status-bar element, read off the
    /// retained tree.
    ///
    /// This replaces `status_bar_layout_now`, which re-ran the whole placement
    /// walk on live state every time a pointer event or a popup anchor needed
    /// a column — a second derivation that could disagree with the painted one
    /// whenever the state behind it had moved. The tree that painted is the
    /// one answering.
    pub(crate) fn status_bar_clickable_rects_now(
        &self,
    ) -> Vec<(
        crate::view::ui::status_bar::StatusBarClickable,
        ratatui::layout::Rect,
    )> {
        let Some(ui) = self.shell_ui.as_ref() else {
            return Vec::new();
        };
        let Some(bar) = self.shell_frame_status_bar.as_ref() else {
            return Vec::new();
        };
        let frame = self.active_chrome().last_frame;
        let size = ratatui::layout::Rect::new(0, 0, frame.width, frame.height);
        crate::view::shell::status_bar::clickable_rects(ui, bar, size)
    }

    /// Screen area `(row, start_col, end_col)` of one clickable element, for
    /// the popups that anchor to their indicator.
    pub(crate) fn status_bar_clickable_area_now(
        &mut self,
        id: crate::view::ui::status_bar::StatusBarClickable,
    ) -> Option<(u16, u16, u16)> {
        self.status_bar_clickable_rects_now()
            .into_iter()
            .find(|(cid, _)| *cid == id)
            .map(|(_, r)| (r.y, r.x, r.x.saturating_add(r.width)))
    }

    /// The prompt row's description: the prompt's message, its query and the
    /// query's caret and selection — and, for the file-open prompts, the
    /// directory the query completes in. `None` when no prompt is in the
    /// row: none is up, or the one that is up is an overlay card of its own.
    fn prompt_row_description(&self) -> Option<crate::view::shell::prompt_line::PromptRow> {
        let win = self.active_window();
        let p = win.prompt.as_ref()?;
        if p.overlay {
            return None;
        }
        let dir = matches!(
            p.prompt_type,
            crate::view::prompt::PromptType::OpenFile
                | crate::view::prompt::PromptType::SwitchProject
        )
        .then(|| win.file_open_state.as_ref().map(|s| s.current_dir.clone()))
        .flatten();
        Some(crate::view::shell::prompt_line::PromptRow {
            message: p.message.clone(),
            input: p.input_str().to_string(),
            cursor: p.cursor_byte(),
            selection: p.selection_range(),
            dir,
        })
    }

    /// Recompute the on-screen areas of the active buffer's cursor-anchored
    /// popups (completion / hover / signature help), cache them for mouse
    /// hit-testing, and paint them. `theme_clone` and `hover_target` are
    /// passed in because `render` reuses them for the global-popup pass.
    /// Tell the tree where the buffer's caret is, and place the layers that
    /// hang off it.
    ///
    /// A popup anchored to the caret names a point *inside* the buffer's host
    /// leaf: the tree hands that leaf a rectangle and knows nothing about its
    /// interior, so the buffer is the only thing that can answer. Two points,
    /// because a completion list lines up with the start of the word being
    /// completed while everything else uses the caret — `render_buffer_popups`
    /// chose between them with an `if` on the popup's kind and then passed a
    /// pair of numbers into `calculate_area`.
    ///
    /// Nothing is published when no popup is up, and `Ui::frame` clears the
    /// previous frame's anchors, so a stale caret can never place anything.
    fn publish_popup_carets(&mut self, size: ratatui::layout::Rect) {
        use crate::view::shell::popup::CaretAnchor;
        if self.popup_descriptions(size).is_empty() {
            return;
        }
        let cells = self.popup_caret_cells();
        let Some(mut ui) = self.shell_ui.take() else {
            return;
        };
        // The area a popup may occupy when it must not paint over the split's
        // vertical scrollbar, which lives in the frame's last column and is
        // not in the tree until S5.
        ui.set_host_anchor(
            crate::view::shell::popup::clear_of_scrollbar_key(),
            fresh_ui::Rect::new(0, 0, size.width.saturating_sub(1), size.height),
        );
        if let Some((caret, word_start)) = cells {
            let cell = |(x, y): (u16, u16)| fresh_ui::Rect::new(x as i32, y as i32, 1, 1);
            ui.set_host_anchor(CaretAnchor::Caret.key(), cell(caret));
            ui.set_host_anchor(CaretAnchor::CompletionWord.key(), cell(word_start));
        }
        ui.place_layers(fresh_ui::Size::new(size.width, size.height));
        self.shell_ui = Some(ui);
    }

    /// The caret's screen position, and the completion word start's.
    ///
    /// Both off the active pane's view — the caret its text pass settled,
    /// and the word start projected through the same rows — so the popup is
    /// anchored where the caret is drawn, not where a second projection of
    /// the viewport says it should be.
    fn popup_caret_cells(&mut self) -> Option<((u16, u16), (u16, u16))> {
        let active = self.effective_active_split();
        let primary = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .and_then(|(_, vs)| vs.get(&active))
            .map(|vs| vs.cursors.primary().position)?;
        let word_start = {
            use crate::primitives::word_navigation::find_completion_word_start;
            find_completion_word_start(&self.active_state().buffer, primary)
        };
        let view = self.active_window().pane_view(active)?;
        let (cx, cy) = view.caret?;
        let caret = (view.rect.x + cx, view.rect.y + cy);
        // The word start's column, on the caret's row: as many cells left of
        // the caret as the rows put between the two bytes.
        let back = match (
            view.byte_to_visual_column(primary),
            view.byte_to_visual_column(word_start),
        ) {
            (Some(c), Some(w)) => c.saturating_sub(w) as u16,
            _ => 0,
        };
        Some((caret, (caret.0.saturating_sub(back), caret.1)))
    }

    /// How many popups the description carries, and how many of those are the
    /// buffer's.
    ///
    /// The same rule `popup_descriptions` builds by, stated once so the two
    /// painters index the tree's answer the way the description filled it: the
    /// buffer's stack first, the top of the global one after.
    fn popup_counts(&self) -> (usize, usize) {
        let buffer = match self.active_state().popups.is_visible() {
            true => self.active_state().popups.all().len(),
            false => 0,
        };
        let global =
            (!self.workspace_trust_on_top() && self.global_popups.top().is_some()) as usize;
        (buffer, buffer + global)
    }

    /// Where the tree put the popups.
    fn popup_rects(&self) -> Vec<ratatui::layout::Rect> {
        let (_, total) = self.popup_counts();
        match self.shell_ui.as_ref() {
            Some(ui) => crate::view::shell::popup::rects_of(ui, total),
            None => vec![ratatui::layout::Rect::default(); total],
        }
    }

    fn cache_buffer_popup_areas(&mut self) {
        self.active_chrome_mut().popup_areas.clear();
        if !self.active_state().popups.is_visible() {
            return;
        }
        // Where each one landed, read off the tree. This was the caret's
        // screen position computed here and handed to `calculate_area`, which
        // then said "clamp to the area's edges" six times; the caret is
        // published to the tree now (`publish_popup_carets`) and the layer
        // that names it has already been placed.
        let rects = self.popup_rects();
        let popup_info: Vec<_> = self
            .active_state()
            .popups
            .all()
            .iter()
            .enumerate()
            .map(|(popup_idx, popup)| {
                let popup_area = rects.get(popup_idx).copied().unwrap_or_default();
                // The rows a painter still owns, inside the frame the tree
                // placed: the description occupies the top of them.
                let desc_height = popup.description_height();
                let inner_area = if popup.bordered {
                    ratatui::layout::Rect {
                        x: popup_area.x + 1,
                        y: popup_area.y + 1 + desc_height,
                        width: popup_area.width.saturating_sub(2),
                        height: popup_area.height.saturating_sub(2 + desc_height),
                    }
                } else {
                    ratatui::layout::Rect {
                        x: popup_area.x,
                        y: popup_area.y + desc_height,
                        width: popup_area.width,
                        height: popup_area.height.saturating_sub(desc_height),
                    }
                };
                let num_items = match &popup.content {
                    crate::view::popup::PopupContent::List { items, .. } => items.len(),
                    _ => 0,
                };
                let total_lines = popup.item_count();
                let visible_lines = inner_area.height as usize;
                let scrollbar_rect = if total_lines > visible_lines && inner_area.width > 2 {
                    Some(ratatui::layout::Rect {
                        x: inner_area.x + inner_area.width - 1,
                        y: inner_area.y,
                        width: 1,
                        height: inner_area.height,
                    })
                } else {
                    None
                };
                (
                    popup_idx,
                    popup_area,
                    inner_area,
                    popup.scroll_offset,
                    num_items,
                    scrollbar_rect,
                    total_lines,
                )
            })
            .collect();

        // Store popup areas for mouse hit testing
        self.active_chrome_mut().popup_areas = popup_info.clone();

        // Nothing is painted here any more: a popup is a layer in the shell's
        // tree, and the overlay band draws it. What survives is the area cache
        // above, which the not-yet-migrated hit-testing still reads.
    }

    /// Draw the software mouse cursor (GPM, which can't paint its own caret on
    /// the alt-screen) and the keyboard-capture dimming. Both read cells that
    /// the main draw already painted, so they run near the end of `render`.
    fn render_software_cursor_and_capture(
        &mut self,
        frame: &mut Frame,
        size: ratatui::layout::Rect,
    ) {
        // Render software mouse cursor when GPM is active
        // GPM can't draw its cursor on the alternate screen buffer used by TUI apps,
        // so we draw our own cursor at the tracked mouse position.
        // This must happen LAST in the render flow so we can read the already-rendered
        // cell content and invert it.
        if self.active_window().gpm_active {
            if let Some((col, row)) = self.active_window().mouse_cursor_position {
                use ratatui::style::Modifier;

                // Only render if within screen bounds
                if col < size.width && row < size.height {
                    // Get the cell at this position and add REVERSED modifier to invert colors
                    let buf = frame.buffer_mut();
                    if let Some(cell) = buf.cell_mut((col, row)) {
                        cell.set_style(cell.style().add_modifier(Modifier::REVERSED));
                    }
                }
            }
        }

        // When keyboard capture mode is active, dim all UI elements outside the terminal
        // to visually indicate that focus is exclusively on the terminal
        if self.active_window().keyboard_capture && self.active_window().focused_terminal_live() {
            // Find the active split's content area
            let active_split = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .active_split();
            let active_split_area = self.pane_content_rect(active_split);

            if let Some(terminal_area) = active_split_area {
                self.apply_keyboard_capture_dimming(frame, terminal_area);
            }
        }
    }

    /// The workspace-trust prompt, as the shell describes it.
    ///
    /// **Every string resolved, no rectangle.** The painter built a
    /// `Vec<Seg>`, counted it to size the dialog "so there is no fixed height
    /// to drift out of sync", then walked it a second time against a scroll
    /// offset it also had to clamp — and recorded four rectangles for a hit
    /// test to compare against. A column of rows in a viewport is all of that:
    /// its height is the count, the window is the viewport's, and each control
    /// answers its own press.
    /// The calibration wizard as a description.
    ///
    /// Every string resolved and every colour named here, where the wizard and
    /// the theme both are — a description is a pure function of what it is
    /// handed, and `t!` and `Theme` are neither.
    fn calibration_description(&self) -> Option<crate::view::shell::calibration::Calibration> {
        use crate::app::calibration_wizard::{CalibrationStep, KeyStatus, PendingConfirmation};
        use crate::view::shell::calibration as cal;
        use fresh_i18n::t;

        let w = self.calibration_wizard.as_ref()?;
        // Its cells were suppressed on the web rather than projected, and
        // that is still true: the description exists, the web fold does not
        // paint it. One less place for the two frontends to disagree comes
        // with D.3, not here.
        let ctrl = |k: &str, label: String, theme: &str| cal::Control {
            key: k.to_string(),
            label,
            key_theme: theme.to_string(),
        };
        let status_of = |s: &KeyStatus, current: bool| -> (String, String) {
            match s {
                KeyStatus::Pending if current => (
                    ">".into(),
                    crate::app::shell_host::shell_theme::attrs(
                        "diagnostic.warning_fg",
                        "ui.popup_bg",
                        &["bold"],
                    ),
                ),
                KeyStatus::Pending => (
                    " ".into(),
                    crate::app::shell_host::shell_theme::pair(
                        "editor.line_number_fg",
                        "ui.popup_bg",
                    ),
                ),
                KeyStatus::Captured => (
                    "*".into(),
                    crate::app::shell_host::shell_theme::pair("diagnostic.info_fg", "ui.popup_bg"),
                ),
                KeyStatus::Skipped => (
                    "-".into(),
                    crate::app::shell_host::shell_theme::pair(
                        "editor.line_number_fg",
                        "ui.popup_bg",
                    ),
                ),
                KeyStatus::Verified => (
                    "v".into(),
                    crate::app::shell_host::shell_theme::pair("ui.help_key_fg", "ui.popup_bg"),
                ),
            }
        };

        let confirm = match w.pending_confirmation {
            PendingConfirmation::None => None,
            PendingConfirmation::Abort => Some(cal::Confirm {
                title: t!("calibration.confirm_abort_title").to_string(),
                message: t!("calibration.confirm_abort_message").to_string(),
                confirm_key: "d".into(),
                confirm_label: t!("calibration.action_discard").to_string(),
                cancel_key: "c".into(),
                cancel_label: t!("calibration.action_cancel").to_string(),
            }),
            PendingConfirmation::Restart => Some(cal::Confirm {
                title: t!("calibration.confirm_restart_title").to_string(),
                message: t!("calibration.confirm_restart_message").to_string(),
                confirm_key: "r".into(),
                confirm_label: t!("calibration.action_restart").to_string(),
                cancel_key: "c".into(),
                cancel_label: t!("calibration.action_cancel").to_string(),
            }),
        };

        let (title, phase, controls) = match &w.step {
            CalibrationStep::Capture { group_idx, key_idx } => {
                let groups = w.groups();
                let group = &groups[*group_idx];
                let target = &group.targets[*key_idx];
                let (step, total) = w.current_step_info();
                let flat_base: usize = groups[..*group_idx].iter().map(|g| g.targets.len()).sum();
                let keys = group
                    .targets
                    .iter()
                    .enumerate()
                    .map(|(i, t)| {
                        let (glyph, theme) = status_of(w.key_status(flat_base + i), i == *key_idx);
                        cal::KeyRow {
                            glyph,
                            name: t.name.to_string(),
                            theme,
                        }
                    })
                    .collect();
                (
                    t!("calibration.title_capture").to_string(),
                    cal::Phase::Capture {
                        group_label: t!("calibration.group").to_string(),
                        group_name: group.name.to_string(),
                        press_prompt: t!("calibration.press_key").to_string(),
                        target_name: target.name.to_string(),
                        keys,
                        at: *key_idx,
                        step_info: format!("{} {}/{}", t!("calibration.step"), step, total),
                    },
                    vec![
                        ctrl("s", t!("calibration.skip").to_string(), "ui.help_key_fg"),
                        ctrl("b", t!("calibration.back").to_string(), "ui.help_key_fg"),
                        ctrl(
                            "g",
                            t!("calibration.skip_group").to_string(),
                            "ui.help_key_fg",
                        ),
                        ctrl(
                            "a",
                            t!("calibration.abort").to_string(),
                            "diagnostic.error_fg",
                        ),
                    ],
                )
            }
            CalibrationStep::Verify if w.translation_count() == 0 => (
                t!("calibration.title_verify").to_string(),
                cal::Phase::AllOk {
                    title: t!("calibration.all_keys_ok_title").to_string(),
                    message: t!("calibration.all_keys_ok_message").to_string(),
                },
                vec![
                    ctrl(
                        "y",
                        t!("calibration.save").to_string(),
                        "diagnostic.info_fg",
                    ),
                    ctrl(
                        "a",
                        t!("calibration.abort").to_string(),
                        "diagnostic.error_fg",
                    ),
                ],
            ),
            CalibrationStep::Verify => {
                let (verified, total) = w.verification_progress();
                let keys = w
                    .all_key_info()
                    .into_iter()
                    .filter_map(|(_, _, target, status)| {
                        let glyph = match status {
                            KeyStatus::Verified => "v",
                            KeyStatus::Captured => " ",
                            _ => return None,
                        };
                        let theme = match status {
                            KeyStatus::Verified => crate::app::shell_host::shell_theme::pair(
                                "diagnostic.info_fg",
                                "ui.popup_bg",
                            ),
                            _ => crate::app::shell_host::shell_theme::pair(
                                "diagnostic.warning_fg",
                                "ui.popup_bg",
                            ),
                        };
                        Some(cal::KeyRow {
                            glyph: format!("[{glyph}]"),
                            name: target.name.to_string(),
                            theme,
                        })
                    })
                    .collect();
                (
                    t!("calibration.title_verify").to_string(),
                    cal::Phase::Verify {
                        title: t!("calibration.verify_title").to_string(),
                        instructions: t!("calibration.verify_instructions").to_string(),
                        translations_label: t!("calibration.translations").to_string(),
                        translations: w.translation_count().to_string(),
                        verified_line: format!(
                            "{}: {}/{}",
                            t!("calibration.verified"),
                            verified,
                            total
                        ),
                        keys,
                    },
                    vec![
                        ctrl(
                            "y",
                            t!("calibration.save").to_string(),
                            "diagnostic.info_fg",
                        ),
                        ctrl("b", t!("calibration.back").to_string(), "ui.help_key_fg"),
                        ctrl(
                            "r",
                            t!("calibration.restart").to_string(),
                            "diagnostic.warning_fg",
                        ),
                        ctrl(
                            "a",
                            t!("calibration.abort").to_string(),
                            "diagnostic.error_fg",
                        ),
                    ],
                )
            }
        };

        Some(cal::Calibration {
            title,
            phase,
            controls,
            status: w.status_message.clone().unwrap_or_default(),
            confirm,
            // Resolved against the frame by `calibration::sized`, which is
            // where the extent is known.
            width: cal::DIALOG_WIDTH,
            height: cal::DIALOG_HEIGHT,
        })
    }

    /// The event-debug dialog as a description.
    fn event_debug_description(&self) -> Option<crate::view::shell::event_debug::EventDebug> {
        use crate::view::shell::event_debug as ed;
        use fresh_i18n::t;
        let d = self.active_window().event_debug.as_ref()?;
        Some(ed::EventDebug {
            title: t!("event_debug.title").to_string(),
            instructions: t!("event_debug.instructions").to_string(),
            help_text: t!("event_debug.help_text").to_string(),
            recent_label: (!d.history.is_empty())
                .then(|| format!("{} ({})", t!("event_debug.recent_events"), d.history.len())),
            empty_label: t!("event_debug.no_events").to_string(),
            history: d
                .history
                .iter()
                .map(|e| ed::Event {
                    description: e.description.clone(),
                    normalized: e.normalized.clone(),
                })
                .collect(),
            controls: vec![
                ("q".into(), t!("event_debug.close").to_string()),
                ("Esc".into(), t!("event_debug.close").to_string()),
                ("c".into(), t!("event_debug.clear").to_string()),
            ],
            details: d.last_event_details(),
            // Resolved against the frame by `event_debug::sized`.
            width: ed::DIALOG_WIDTH,
            height: ed::DIALOG_HEIGHT,
        })
    }

    /// Read the settings body's window back off the tree.
    ///
    /// **The direction of travel is the point.** `ScrollablePanel` owned the
    /// window and re-derived the column's height from `SettingItem::layout_box`
    /// to bound it — the same arithmetic the painter drew each card with, in a
    /// second place. The `viewport` owns it now, so this reads rather than
    /// computes, and the state's scroll methods ask for a move by handle
    /// instead of writing an offset.
    fn refresh_settings_body_window(&mut self) {
        use crate::view::shell::settings as st;
        let Some(ui) = self.shell_ui.as_ref() else {
            return;
        };
        // **Three windows, three answers, and no one of them gates the
        // others.** The cards' viewport is only in the tree while the body is
        // showing cards: a search replaces it with the results list, so
        // returning here when it is missing left the results' own offset
        // unread, and the count row went on reporting "(1-10 of 176)" however
        // far the wheel had taken the list.
        let body = ui.find_by_key(&st::items_key());
        let vpr = body.map(|vp| ui.rect_of(vp)).unwrap_or_default();
        let (scroll, content) = match body {
            Some(vp) => ui.scroll(vp),
            None => Default::default(),
        };
        let offset = scroll.y.max(0) as u16;
        let moved = body.is_some()
            && self
                .settings_state
                .as_ref()
                .is_some_and(|s| s.body.offset != offset);
        // Which card the window starts on. Only worth a walk when the window
        // has actually moved — it is the left tree's highlight that reads it,
        // and that only has to change when the body does.
        let top_item = match moved {
            false => self.settings_state.as_ref().and_then(|s| s.body.top_item),
            true => {
                let n = self
                    .settings_state
                    .as_ref()
                    .and_then(|s| s.pages.get(s.selected_category))
                    .map(|p| p.items.len())
                    .unwrap_or(0);
                (0..n).find(|&i| {
                    body.and_then(|vp| ui.find_by_key_in(vp, &st::card_key(i)))
                        .map(|e| ui.rect_of(e))
                        // The first card whose bottom edge is below the
                        // window's top is the one the window starts on.
                        .is_some_and(|r| r.y + r.h as i32 > vpr.y)
                })
            }
        };
        let Some(s) = self.settings_state.as_mut() else {
            return;
        };
        if body.is_some() {
            s.body = crate::view::settings::state::BodyWindow {
                offset,
                height: vpr.h,
                content: content.h,
                top_item,
            };
        }
        // The left tree's highlight follows the body, in both directions —
        // the same contract the wheel and the scrollbar had, stated once
        // against the window rather than at each thing that moves it.
        // ...but not when the cursor is what moved it: see
        // `SettingsState::cursor_drove_body`.
        if moved && !s.take_cursor_drove_body() {
            s.sync_tree_cursor_to_body_scroll();
        }
        // The search results' window, on the same terms. The list moves its
        // own window when the selection leaves it, so what the count row
        // reports is read back rather than kept in step by hand.
        if let Some(el) = ui.find_by_key(&st::results_key()) {
            // In results, not rows: an index-scrolled window counts its offset
            // in the items it holds.
            s.search_scroll_offset = ui.scroll(el).0.y.max(0) as usize;
        }
        // Each entry dialog's window, on the same terms. Its offset is read
        // rather than kept: the keyboard moves it by asking, and what it
        // ended up at is the window's answer.
        for (level, d) in s.entry_dialog_stack.iter_mut().enumerate() {
            if let Some(vp) = ui.find_by_key(&st::entry_items_key(level)) {
                d.scroll_offset = ui.scroll(vp).0.y.max(0) as usize;
            }
        }
    }

    /// What a field offers at the right of its first row.
    ///
    /// A nullable field that is already unset shows the badge *only when the
    /// unset value really does inherit* something — a clear-only field just
    /// reads as not set (#2345). Otherwise it offers the actions that lead
    /// somewhere different from where the value already is.
    fn entry_affordance(
        d: &crate::view::settings::entry_dialog::EntryDialogState,
        index: usize,
        item: &crate::view::settings::items::SettingItem,
        focused: bool,
    ) -> Option<crate::view::shell::entry::Affordance> {
        use crate::view::shell::entry as e;
        use fresh_i18n::t;
        if item.read_only {
            return None;
        }
        if item.nullable && item.is_null {
            let inherits = d
                .inheritable_fields
                .contains(item.path.trim_start_matches('/'));
            return inherits
                .then(|| e::Affordance::Badge(t!("settings.inherited_badge").to_string()));
        }
        let buttons = d.field_action_buttons(index);
        if buttons.is_empty() {
            return None;
        }
        let cursor = focused.then_some(d.field_button_focus).flatten();
        Some(e::Affordance::Actions(
            buttons
                .into_iter()
                .enumerate()
                .map(|(i, (_, label))| e::Action {
                    label,
                    focused: cursor == Some(i),
                })
                .collect(),
        ))
    }

    /// The settings entry-edit dialog stack.
    ///
    /// **One description per level.** The painter drew them in a loop with
    /// `apply_dimming` between, and every one of the three mouse paths behind
    /// them recomputed the box, the button row and the field positions from
    /// the modal area. Each level is a layer with its own scrim; each field,
    /// button and per-field action answers its own press.
    fn settings_entry_description(&self) -> Vec<crate::view::shell::entry::Dialog> {
        use crate::view::shell::entry as e;

        let Some(s) = self.settings_state.as_ref() else {
            return Vec::new();
        };
        s.entry_dialog_stack
            .iter()
            .enumerate()
            .map(|(level, d)| {
                let label_width = d.label_column();
                let first_editable = d.first_editable_index;
                let states = std::rc::Rc::new(d.controls.instance_states.clone());
                let divider_at =
                    (first_editable > 0 && first_editable < d.items.len()).then_some(first_editable);
                let items = d
                    .items
                    .iter()
                    .enumerate()
                    .map(|(index, item)| {
                        let focused =
                            !item.read_only && !d.focus_on_buttons && d.selected_item == index;
                        e::Item {
                            index,
                            divider_above: divider_at == Some(index),
                            section: item
                                .section
                                .clone()
                                .filter(|_| item.is_section_start),
                            spec: crate::view::settings::widget_map::setting_control_to_widget_aligned(
                                &item.path,
                                &item.control,
                                label_width,
                                d.composite_cursor_of(item),
                            ),
                            // As on the page: a control is live when the
                            // dialog's store names it, or one of its rows.
                            focus_key: d.focus_key_of(item).unwrap_or("").to_string(),
                            states: states.clone(),
                            focused,
                            hovered: !item.read_only && d.hover_item == Some(index),
                            modified: item.modified,
                            read_only: item.read_only,
                            // A composite control's cursor walks its own rows,
                            // and the `>` goes where the cursor is.
                            cursor_row: d.cursor_row(index),
                            affordance: Self::entry_affordance(d, index, item, focused),
                        }
                    })
                    .collect();
                let mut buttons = vec![
                    e::Button {
                        label: "[ Save ]".into(),
                        focused: d.focus_on_buttons && d.focused_button == 0,
                        hovered: d.hover_button == Some(0),
                        destructive: false,
                    },
                    e::Button {
                        label: "[ Cancel ]".into(),
                        focused: d.focus_on_buttons && d.focused_button == 1,
                        hovered: d.hover_button == Some(1),
                        destructive: false,
                    },
                ];
                if !d.is_new && !d.no_delete {
                    buttons.push(e::Button {
                        label: crate::view::settings::render::entry_delete_button_label(d),
                        focused: d.focus_on_buttons && d.focused_button == 2,
                        hovered: d.hover_button == Some(2),
                        destructive: true,
                    });
                }
                let (legend, warn) = d.legend_line();
                e::Dialog {
                    level,
                    title: match d.is_dirty() {
                        // The painter's own words, not a message key: there is
                        // no `settings.modified_suffix` in the catalogue, so
                        // `t!` handed the title the key itself and the dialog
                        // read "Add Value • settings.modified_suffix". The
                        // settings box beside it spells its own suffix out the
                        // same way.
                        true => format!(" {} • modified ", d.title),
                        false => format!(" {} ", d.title),
                    },
                    dirty: d.is_dirty(),
                    items,
                    buttons,
                    helper: d.helper_line(),
                    legend: match warn {
                        true => e::Legend::Warn(legend),
                        false => e::Legend::Keys(legend),
                    },
                    anchor: Some(d.body_anchor.clone()),
                }
            })
            .collect()
    }

    /// The settings page's cards.
    ///
    /// **This is what `ScrollablePanel::render` and `render_setting_item_pure`
    /// were.** The painter planned each item as an `ItemBox` — five row counts
    /// and five `_y()` accessors — clipped every band against a
    /// `BandViewport`, and filed a `ControlLayoutInfo` per control so a later
    /// click could be compared against it. None of the three survives here:
    /// the column measures each card once, the window is a `viewport`, and the
    /// control's own hits answer its presses.
    fn settings_cards(
        s: &crate::view::settings::SettingsState,
    ) -> Option<crate::view::shell::settings::Items> {
        use crate::view::settings::items::page_label_width;
        use crate::view::shell::settings as st;
        use fresh_i18n::t;

        let page = s.pages.get(s.selected_category)?;
        let focused = s.focus_panel() == crate::view::settings::state::FocusPanel::Settings;
        let label_width = page_label_width(&page.items);
        // The kinds' state, shared by every card of the page: the same map
        // the kinds wrote, read by the description they are painted from.
        let states = std::rc::Rc::new(s.controls.instance_states.clone());
        let cards =
            page.items
                .iter()
                .enumerate()
                .map(|(index, item)| {
                    let hovered = matches!(
                        s.hover_hit,
                        Some(
                            crate::view::settings::SettingsHit::Item(i)
                                | crate::view::settings::SettingsHit::ControlToggle(i)
                                | crate::view::settings::SettingsHit::ControlDropdown(i)
                                | crate::view::settings::SettingsHit::ControlText(i)
                                | crate::view::settings::SettingsHit::ControlNumberValue(i)
                                | crate::view::settings::SettingsHit::ControlTextListRow(i, _)
                                | crate::view::settings::SettingsHit::ControlMapRow(i, _)
                                | crate::view::settings::SettingsHit::ControlInherit(i)
                        ) if i == index
                    );
                    st::Card {
                        index,
                        section: item.section.clone().filter(|_| {
                            item.is_section_start && item.style.section_header_rows > 0
                        }),
                        spec: crate::view::settings::widget_map::setting_control_to_widget_aligned(
                            &item.path,
                            &item.control,
                            label_width,
                            s.composite_cursor_of(item),
                        ),
                        // A control paints its caret, its draft or its list
                        // only while it is the surface's focused widget, and
                        // only being live means that — outside edit mode ↑↓
                        // walks the settings list and a caret would promise a
                        // movement the arrows do not make. A control is live
                        // when the store's focus names it.
                        focus_key: s.focus_key_of(item).unwrap_or("").to_string(),
                        live: s.focus_key_of(item).is_some(),
                        states: states.clone(),
                        hovered_popup_row: s.hovered_popup_row.clone(),
                        description: item.description.clone().filter(|d| !d.is_empty()),
                        layer: match item.layer_source {
                            crate::config_io::ConfigLayer::System => None,
                            crate::config_io::ConfigLayer::User => Some("user"),
                            crate::config_io::ConfigLayer::Project => Some("project"),
                            crate::config_io::ConfigLayer::Session => Some("session"),
                        },
                        selected: focused && index == s.selected_item,
                        hovered,
                        dirty: s.path_has_pending_change(&item.path),
                        inherit: match (item.nullable, item.is_null) {
                            (false, _) => None,
                            (true, true) => Some(st::Inherit::Badge(
                                t!("settings.inherited_badge").to_string(),
                            )),
                            (true, false) => Some(st::Inherit::Button {
                                label: format!("[{}]", t!("settings.btn_inherit")),
                                hovered: matches!(
                                    s.hover_hit,
                                    Some(crate::view::settings::SettingsHit::ControlInherit(i))
                                        if i == index
                                ),
                            }),
                        },
                        bordered: item.style.card_border_rows > 0,
                    }
                })
                .collect();
        Some(st::Items {
            cards,
            entry: s.selected_item,
            anchor: Some(s.body_anchor.clone()),
        })
    }

    /// The settings dialog's title and search row.
    fn settings_chrome_description(&self) -> Option<crate::view::shell::settings::Chrome> {
        use crate::app::shell_host::shell_theme::{attrs, pair};
        use crate::view::shell::settings as st;

        let s = self.settings_state.as_ref().filter(|s| s.visible)?;
        let dim = pair("editor.line_number_fg", "ui.popup_bg");
        let search = match s.search_active {
            false => st::Search::Hint(vec![
                st::Span::new("Press ", dim.clone()),
                st::Span::new(" / ", pair("ui.popup_text_fg", "ui.split_separator_fg")),
                st::Span::new(" to search settings...", dim.clone()),
            ]),
            true => {
                let query = s.search_query();
                let cursor = s.search_cursor().min(query.len()) as i32;
                let (sel_start, sel_end) = s
                    .search_input
                    .selection_flat_range()
                    .map(|(a, b)| (a as i32, b as i32))
                    .unwrap_or((-1, -1));
                let n = s.search_results.len();
                // The count and the scroll arrows, which are search chrome
                // rather than the field.
                let count = match (query.is_empty(), n) {
                    (true, _) => String::new(),
                    (false, 0) => " (no results)".into(),
                    (false, 1) => " (1 result)".into(),
                    (false, _) if s.search_max_visible >= n => format!(" ({n} results)"),
                    (false, _) => format!(
                        " ({}-{} of {n})",
                        s.search_scroll_offset + 1,
                        (s.search_scroll_offset + s.search_max_visible).min(n)
                    ),
                };
                let arrows = match (
                    s.search_scroll_offset > 0,
                    s.search_scroll_offset + s.search_max_visible < n,
                ) {
                    (true, true) => " ↑↓",
                    (true, false) => " ↑",
                    (false, true) => " ↓",
                    (false, false) => "",
                };
                st::Search::Active {
                    field: std::rc::Rc::new(fresh_core::api::WidgetSpec::Text {
                        value: query.to_string(),
                        cursor_byte: cursor,
                        focused: true,
                        label: String::new(),
                        placeholder: None,
                        rows: 1,
                        field_width: 0,
                        max_visible_chars: 0,
                        full_width: false,
                        completions: Vec::new(),
                        completions_visible_rows: 0,
                        block_caret: true,
                        sel_start,
                        sel_end,
                        label_width: 0,
                        read_only: false,
                        markdown: false,
                        key: None,
                    }),
                    suffix: vec![
                        st::Span::new(count, dim.clone()),
                        st::Span::new(arrows, attrs("ui.menu_active_fg", "ui.popup_bg", &["bold"])),
                    ],
                }
            }
        };
        let footer = Some(()).map(|()| {
            let nullable_set = s
                .current_item()
                .map(|i| i.nullable && !i.is_null)
                .unwrap_or(false);
            let focused = (s.focus_panel() == crate::view::settings::state::FocusPanel::Footer)
                .then(|| st::Button::from_index(s.footer_button_index));
            use crate::view::settings::hit::SettingsHit;
            st::Footer {
                layer: format!("[ {} ]", s.target_layer_name()),
                reset: format!(
                    "[ {} ]",
                    match nullable_set {
                        true => t!("settings.btn_inherit").to_string(),
                        false => t!("settings.btn_reset").to_string(),
                    }
                ),
                save: format!("[ {} ]", t!("settings.btn_save")),
                cancel: format!("[ {} ]", t!("settings.btn_cancel")),
                edit: format!("[ {} ]", t!("settings.btn_edit")),
                help: match (s.search_active, focused.is_some(), s.is_editing_dual_list()) {
                    (true, _, _) => t!("settings.help_search").to_string(),
                    (_, true, _) => t!("settings.help_footer").to_string(),
                    // "Enter:Edit" is actively wrong once the two-column
                    // picker has the keyboard.
                    (_, _, true) => t!("settings.help_duallist").to_string(),
                    _ => t!("settings.help_default").to_string(),
                },
                focused,
                hovered: match s.hover_hit {
                    Some(SettingsHit::LayerButton) => Some(st::Button::Layer),
                    Some(SettingsHit::ResetButton) => Some(st::Button::Reset),
                    Some(SettingsHit::SaveButton) => Some(st::Button::Save),
                    Some(SettingsHit::CancelButton) => Some(st::Button::Cancel),
                    Some(SettingsHit::EditButton) => Some(st::Button::Edit),
                    _ => None,
                },
            }
        });
        // The category tree, in the wide layout and while no search is
        // running: the narrow layout lays its categories as a horizontal
        // strip, and a search replaces the whole body with its results.
        //
        // **And it no longer stands down for the entry-dialog stack.** That
        // stack was the painter's, the tree folds after every painter, and a
        // described tree would have been drawn *over* the dialog covering it
        // — so the band behind an open dialog was blank. The stack is a layer
        // now (`view::shell::entry`), which lands the right way round.
        let categories = (!s.search_active).then(|| {
            use crate::view::settings::state::{FocusPanel, TreeRow};
            let nerd = s.nerd_font_icons_enabled();
            let cursor = s.tree_cursor_section;
            let rows: Vec<st::CatRow> = s
                .visible_tree()
                .iter()
                .map(|r| match *r {
                    TreeRow::Category {
                        idx,
                        expandable,
                        expanded,
                    } => {
                        let page = &s.pages[idx];
                        st::CatRow::Category {
                            idx,
                            chevron: match (expandable, expanded) {
                                (false, _) => " ",
                                (true, true) => "▼",
                                (true, false) => "▶",
                            },
                            expandable,
                            dirty: s.page_has_pending_changes(idx),
                            icon: crate::view::settings::render::category_icon(&page.name, nerd),
                            label: page.name.clone(),
                            elide: page.name.starts_with("Plugin: "),
                        }
                    }
                    TreeRow::Section {
                        cat_idx,
                        section_idx,
                    } => st::CatRow::Section {
                        cat: cat_idx,
                        section: section_idx,
                        label: s.pages[cat_idx].sections[section_idx].name.clone(),
                    },
                })
                .collect();
            // **One index, where the painter asked every row.** It compared
            // `idx == selected_category && tree_cursor.is_none()` on a
            // category and `cat_idx == selected_category && tree_cursor ==
            // Some(section_idx)` on a section; the list wants the position.
            let selected = rows.iter().position(|r| match r {
                st::CatRow::Category { idx, .. } => *idx == s.selected_category && cursor.is_none(),
                st::CatRow::Section { cat, section, .. } => {
                    *cat == s.selected_category && cursor == Some(*section)
                }
            });
            st::Categories {
                rows,
                selected,
                focused: s.focus_panel() == FocusPanel::Categories,
            }
        });
        // The settings panel's own header: the page title, and the `[Clear …]`
        // a nullable category with values offers. In either layout — the
        // narrow one's categories are a painted strip, but its page is the
        // tree's like the wide one's.
        let page = (!s.search_active).then(|| {
            let p = s.current_page();
            st::Page {
                title: p.map(|p| p.name.clone()).unwrap_or_default(),
                clear: p
                    .is_some_and(|p| p.nullable)
                    .then(|| s.current_category_has_values())
                    .unwrap_or(false)
                    .then(|| format!("[{}]", t!("settings.btn_clear_category"))),
                clear_hovered: matches!(
                    s.hover_hit,
                    Some(crate::view::settings::SettingsHit::ClearCategoryButton)
                ),
            }
        });
        // The cards. Described in either layout — the narrow one's categories
        // are a painted strip, but its page is the tree's like the wide one's.
        let items = (!s.search_active)
            .then(|| Self::settings_cards(s))
            .flatten();
        // The narrow layout's categories, which are the other half of the
        // same choice the tree above is.
        let strip = (!s.search_active).then(|| st::Strip {
            focused: s.focus_panel() == crate::view::settings::state::FocusPanel::Categories,
            hint: "←→: Switch category".into(),
            cats: s
                .pages
                .iter()
                .enumerate()
                .map(|(idx, page)| st::StripCat {
                    idx,
                    label: page.name.clone(),
                    dirty: s.page_has_pending_changes(idx),
                    selected: idx == s.selected_category,
                })
                .collect(),
        });
        // The search's results, in place of the page. The painter windowed
        // them by hand and filed a rectangle per visible card; the list is
        // the window, and a row knows its own index.
        let results = (s.search_active && !s.search_results.is_empty()).then(|| st::Results {
            selected: s.selected_search_result,
            rows: s
                .search_results
                .iter()
                .map(crate::view::settings::render::search_result_row)
                .collect(),
        });
        Some(st::Chrome {
            footer,
            categories,
            strip,
            results,
            page,
            items,
            title: match s.has_changes() {
                true => format!(" Settings [{}] • (modified) ", s.target_layer_name()),
                false => format!(" Settings [{}] ", s.target_layer_name()),
            },
            search,
        })
    }

    /// The settings dialog's open prompt or help overlay.
    ///
    /// Not the entry-dialog stack, which is its own surface and its own
    /// migration; when one of those is up this answers `None` and the painter
    /// draws it as before.
    fn settings_dialog_description(&self) -> Option<crate::view::shell::settings::Dialog> {
        use crate::view::shell::settings as st;
        use fresh_i18n::t;

        let s = self.settings_state.as_ref().filter(|s| s.visible)?;
        // The painter's own precedence, topmost first — and it is layer
        // order now, so it holds without the gate that used to sit in the
        // middle of this chain. The entry stack was painted, so anything
        // *under* it had to stay painted too; a described prompt would have
        // landed on top of the dialog it belonged behind.
        if s.showing_entry_delete_confirm {
            let named = !s.entry_delete_target_name.is_empty();
            return Some(st::Dialog::EntryDelete(st::Destructive {
                title: match (named, s.entry_delete_target_is_array_item) {
                    (true, _) => format!("Delete \"{}\"?", s.entry_delete_target_name),
                    (false, true) => "Delete item?".into(),
                    (false, false) => "Delete entry?".into(),
                },
                message: match (named, s.entry_delete_target_is_array_item) {
                    (true, _) => format!(
                        "This will permanently remove \"{}\".",
                        s.entry_delete_target_name
                    ),
                    (false, true) => "This will permanently remove this item.".into(),
                    (false, false) => "This will permanently remove the entry.".into(),
                },
                buttons: vec!["Cancel".into(), "Delete".into()],
                selected: s.entry_delete_confirm_selection,
                destructive: 1,
                help: "Tab/←→: Select   Enter: Confirm   Esc: Cancel".into(),
                grave: true,
                width: 60,
            }));
        }
        if s.showing_entry_discard_confirm {
            return Some(st::Dialog::EntryDiscard(st::Destructive {
                title: "Discard changes?".into(),
                message: "You have uncommitted edits in this dialog.".into(),
                buttons: vec!["Keep editing".into(), "Discard".into()],
                selected: s.entry_discard_confirm_selection,
                destructive: 1,
                help: "Tab/←→: Select   Enter: Confirm   Esc: Keep editing".into(),
                grave: false,
                width: 50,
            }));
        }
        if s.showing_help {
            let head = |k: &str| st::HelpLine {
                key: k.to_string(),
                desc: String::new(),
                heading: true,
            };
            let l = |k: &str, d: &str| st::HelpLine {
                key: k.to_string(),
                desc: d.to_string(),
                heading: false,
            };
            let gap = || st::HelpLine {
                key: String::new(),
                desc: String::new(),
                heading: false,
            };
            return Some(st::Dialog::Help {
                title: "Keyboard Shortcuts".into(),
                lines: vec![
                    head("Navigation"),
                    l("↑ / ↓", "Move up/down"),
                    l("Tab", "Switch between categories and settings"),
                    l("Enter", "Activate/toggle setting"),
                    gap(),
                    head("Search"),
                    l("/", "Start search"),
                    l("Esc", "Cancel search"),
                    l("↑ / ↓", "Navigate results"),
                    l("Enter", "Jump to result"),
                    gap(),
                    head("Actions"),
                    l("Ctrl+S", "Save settings"),
                    l("Esc", "Close settings"),
                    l("?", "Toggle this help"),
                ],
            });
        }
        let help = "←/→/Tab: Select   Enter: Confirm   Esc: Cancel".to_string();
        if s.showing_reset_dialog {
            return Some(st::Dialog::Reset(st::Choice {
                title: "Reset All Changes".into(),
                prompt: "Discard all pending changes?".into(),
                changes: s.get_change_descriptions(),
                buttons: vec!["Reset".into(), "Cancel".into()],
                selected: s.reset_dialog_selection,
                hovered: s.reset_dialog_hover,
                help,
            }));
        }
        if s.showing_confirm_dialog {
            return Some(st::Dialog::Confirm(st::Choice {
                title: t!("confirm.unsaved_changes_title").to_string(),
                prompt: t!("confirm.unsaved_changes_prompt").to_string(),
                changes: s.get_change_descriptions(),
                buttons: vec![
                    t!("confirm.save_and_exit").to_string(),
                    t!("confirm.discard").to_string(),
                    t!("confirm.cancel").to_string(),
                ],
                selected: s.confirm_dialog_selection,
                hovered: s.confirm_dialog_hover,
                help,
            }));
        }
        None
    }

    /// The keybinding editor's title, header rows and footer.
    fn keybinding_chrome_description(&self) -> Option<crate::view::shell::keybinding::Chrome> {
        use crate::app::keybinding_editor::{ContextFilter, SearchMode, SourceFilter};
        use crate::app::shell_host::shell_theme::{attrs, pair};
        use crate::view::shell::keybinding as kb;
        use fresh_i18n::t;

        let e = self.keybinding_editor.as_ref()?;
        let ink = pair("ui.popup_text_fg", "ui.popup_bg");
        let accent = pair("diagnostic.info_fg", "ui.popup_bg");
        let key_ink = attrs("ui.help_key_fg", "ui.popup_bg", &["bold"]);

        let mut path = vec![
            kb::Span::new(
                format!(" {} ", t!("keybinding_editor.label_config")),
                ink.clone(),
            ),
            kb::Span::new(e.config_file_path.clone(), accent.clone()),
        ];
        if !e.keymap_names.is_empty() {
            path.push(kb::Span::new(
                format!("  {} ", t!("keybinding_editor.label_maps")),
                ink.clone(),
            ));
            path.push(kb::Span::new(e.keymap_names.join(", "), ink.clone()));
        }

        let search = match (e.search_active, &e.search_mode) {
            (false, _) => vec![
                kb::Span::new(" ", ink.clone()),
                kb::Span::new(t!("keybinding_editor.search_hint").to_string(), ink.clone()),
            ],
            (true, SearchMode::Text) => {
                let mut v = vec![
                    kb::Span::new(
                        format!(" {} ", t!("keybinding_editor.label_search")),
                        key_ink.clone(),
                    ),
                    kb::Span::new(e.search_query.clone(), ink.clone()),
                ];
                if e.search_focused {
                    v.push(kb::Span::new("_", pair("editor.cursor", "ui.popup_bg")));
                    v.push(kb::Span::new(
                        format!("  {}", t!("keybinding_editor.search_text_hint")),
                        ink.clone(),
                    ));
                }
                v
            }
            (true, SearchMode::RecordKey) => vec![
                kb::Span::new(
                    format!(" {} ", t!("keybinding_editor.label_record_key")),
                    attrs("diagnostic.warning_fg", "ui.popup_bg", &["bold"]),
                ),
                kb::Span::new(
                    match e.search_key_display.is_empty() {
                        true => t!("keybinding_editor.press_a_key").to_string(),
                        false => e.search_key_display.clone(),
                    },
                    ink.clone(),
                ),
                kb::Span::new(
                    format!("  {}", t!("keybinding_editor.search_record_hint")),
                    ink.clone(),
                ),
            ],
        };

        let total = e.bindings.len();
        let filtered = e.filtered_indices.len();
        let counts = match filtered == total {
            true => t!("keybinding_editor.bindings_count", count = total).to_string(),
            false => t!(
                "keybinding_editor.bindings_filtered",
                filtered = filtered,
                total = total
            )
            .to_string(),
        };
        let set = |on: bool| match on {
            true => accent.clone(),
            false => ink.clone(),
        };
        let mut filters = vec![
            kb::Span::new(
                format!(" {} ", t!("keybinding_editor.label_context")),
                ink.clone(),
            ),
            kb::Span::new(
                format!("[{}]", e.context_filter_display()),
                set(e.context_filter != ContextFilter::All),
            ),
            kb::Span::new(
                format!("  {} ", t!("keybinding_editor.label_source")),
                ink.clone(),
            ),
            kb::Span::new(
                format!("[{}]", e.source_filter_display()),
                set(e.source_filter != SourceFilter::All),
            ),
            kb::Span::new(format!("  {counts}"), ink.clone()),
        ];
        if e.has_changes {
            filters.push(kb::Span::new(
                format!("  {}", t!("keybinding_editor.modified")),
                pair("diagnostic.warning_fg", "ui.popup_bg"),
            ));
        }

        // The footer's hints, which differ while the search bar has focus.
        let hint = |k: &str, label: String| {
            vec![
                kb::Span::new(k.to_string(), pair("ui.help_key_fg", "ui.popup_bg")),
                kb::Span::new(format!(":{label}  "), ink.clone()),
            ]
        };
        let footer: Vec<kb::Span> = match e.search_active && e.search_focused {
            true => [
                hint(" Esc", t!("keybinding_editor.footer_cancel").to_string()),
                hint(
                    "Tab",
                    t!("keybinding_editor.footer_toggle_mode").to_string(),
                ),
                hint("Enter", t!("keybinding_editor.footer_confirm").to_string()),
            ]
            .concat(),
            false => [
                hint(" Enter", t!("keybinding_editor.footer_edit").to_string()),
                hint("a", t!("keybinding_editor.footer_add").to_string()),
                hint("d", t!("keybinding_editor.footer_delete").to_string()),
                hint("x", t!("keybinding_editor.footer_disable").to_string()),
                hint("/", t!("keybinding_editor.footer_search").to_string()),
                hint("r", t!("keybinding_editor.footer_record_key").to_string()),
                hint("c", t!("keybinding_editor.footer_context").to_string()),
                hint("s", t!("keybinding_editor.footer_source").to_string()),
                hint("?", t!("keybinding_editor.footer_help").to_string()),
                hint("Ctrl+S", t!("keybinding_editor.footer_save").to_string()),
                hint("Esc", t!("keybinding_editor.footer_close").to_string()),
            ]
            .concat(),
        };

        Some(kb::Chrome {
            title: format!(
                "{} \u{2500} [{}]",
                t!("keybinding_editor.title"),
                e.active_keymap
            ),
            path,
            search,
            filters,
            footer,
        })
    }

    /// The keybinding editor's table, when no dialog covers it.
    ///
    /// **The rows are resolved here** — every column already padded to nothing
    /// and every colour already a name — because a description is a pure
    /// function of what it is handed, and `t!` and `Theme` are neither.
    fn keybinding_table_description(&self) -> Option<crate::view::shell::keybinding::Table> {
        use crate::app::keybinding_editor::{BindingSource, DisplayRow};
        use crate::view::shell::keybinding as kb;
        use fresh_i18n::t;

        let e = self.keybinding_editor.as_ref()?;
        // A dialog is a layer over the box, so the table under it is not seen.
        if e.showing_help || e.edit_dialog.is_some() || e.showing_confirm_dialog {
            return None;
        }
        Some(kb::Table {
            columns: [
                t!("keybinding_editor.header_key").to_string(),
                t!("keybinding_editor.header_action").to_string(),
                t!("keybinding_editor.header_description").to_string(),
                t!("keybinding_editor.header_context").to_string(),
                t!("keybinding_editor.header_source").to_string(),
            ],
            rows: e
                .display_rows
                .iter()
                .map(|r| match r {
                    DisplayRow::SectionHeader {
                        plugin_name,
                        collapsed,
                        binding_count,
                    } => kb::Row::Section {
                        chevron: match collapsed {
                            true => "▶".into(),
                            false => "▼".into(),
                        },
                        label: plugin_name.clone().unwrap_or_else(|| "Builtin".into()),
                        count: *binding_count,
                    },
                    DisplayRow::Binding(i) => {
                        let b = &e.bindings[*i];
                        kb::Row::Binding {
                            key: b.key_display.clone(),
                            action: b.action.clone(),
                            description: b.action_display.clone(),
                            context: b.context.clone(),
                            source: match b.source {
                                BindingSource::Custom => {
                                    t!("keybinding_editor.source_custom").to_string()
                                }
                                BindingSource::Keymap => {
                                    t!("keybinding_editor.source_keymap").to_string()
                                }
                                BindingSource::Plugin => {
                                    t!("keybinding_editor.source_plugin", default = "Plugin")
                                        .to_string()
                                }
                                BindingSource::Unbound => String::new(),
                            },
                            source_accent: matches!(
                                b.source,
                                BindingSource::Custom | BindingSource::Plugin
                            ),
                        }
                    }
                })
                .collect(),
            selected: e.selected,
        })
    }

    /// The keybinding editor's open dialog, as a description.
    fn keybinding_dialog_description(&self) -> Option<crate::view::shell::keybinding::Dialog> {
        use crate::app::keybinding_editor::EditMode;
        use crate::view::shell::keybinding as kb;
        use fresh_i18n::t;

        let e = self.keybinding_editor.as_ref()?;
        // The painter's own precedence: help first, then the edit dialog, then
        // the confirmation. Each returned before reaching the next.
        if e.showing_help {
            let head = |k: &str| kb::HelpLine {
                key: k.to_string(),
                desc: String::new(),
                heading: true,
            };
            let l = |k: &str, d: String| kb::HelpLine {
                key: k.to_string(),
                desc: d,
                heading: false,
            };
            let gap = || kb::HelpLine {
                key: String::new(),
                desc: String::new(),
                heading: false,
            };
            return Some(kb::Dialog::Help(kb::Help {
                title: t!("keybinding_editor.help_title").to_string(),
                lines: vec![
                    head(&t!("keybinding_editor.help_navigation")),
                    l(
                        "  ↑ / ↓",
                        t!("keybinding_editor.help_move_up_down").to_string(),
                    ),
                    l(
                        "  PgUp / PgDn",
                        t!("keybinding_editor.help_page_up_down").to_string(),
                    ),
                    l(
                        "  Home / End",
                        t!("keybinding_editor.help_first_last").to_string(),
                    ),
                    gap(),
                    head(&t!("keybinding_editor.help_search")),
                    l(
                        "  /",
                        t!("keybinding_editor.help_search_by_name").to_string(),
                    ),
                    l(
                        "  r",
                        t!("keybinding_editor.help_search_by_key").to_string(),
                    ),
                    l(
                        "  Tab",
                        t!("keybinding_editor.help_toggle_search").to_string(),
                    ),
                    l(
                        "  Esc",
                        t!("keybinding_editor.help_cancel_search").to_string(),
                    ),
                    gap(),
                    head(&t!("keybinding_editor.help_editing")),
                    l(
                        "  Enter",
                        t!("keybinding_editor.help_edit_binding").to_string(),
                    ),
                    l("  a", t!("keybinding_editor.help_add_binding").to_string()),
                    l(
                        "  d / Delete",
                        t!("keybinding_editor.help_delete_binding").to_string(),
                    ),
                    l(
                        "  x",
                        t!("keybinding_editor.help_disable_binding").to_string(),
                    ),
                    gap(),
                    head(&t!("keybinding_editor.help_filters")),
                    l(
                        "  c",
                        t!("keybinding_editor.help_cycle_context").to_string(),
                    ),
                    l("  s", t!("keybinding_editor.help_cycle_source").to_string()),
                    gap(),
                    l(
                        "  Ctrl+S",
                        t!("keybinding_editor.help_save_changes").to_string(),
                    ),
                    l(
                        "  Esc / ?",
                        t!("keybinding_editor.help_close_help").to_string(),
                    ),
                ],
            }));
        }

        if let Some(d) = &e.edit_dialog {
            let key_value = match d.key_display.is_empty() {
                false => d.key_display.clone(),
                true => match d.mode {
                    EditMode::RecordingKey => t!("keybinding_editor.key_recording").to_string(),
                    _ => t!("keybinding_editor.key_none").to_string(),
                },
            };
            let key_focused = d.focus_area == 0;
            let action_focused = d.focus_area == 1;
            let ctx_focused = d.focus_area == 2;
            let action_value = match d.action_text.is_empty() && d.mode != EditMode::EditingAction {
                true => t!("keybinding_editor.action_placeholder").to_string(),
                false => d.action_text.clone(),
            };
            // Shown only when the resolved form says something the typed name
            // does not — the painter's own comparison.
            let described = (!d.action_text.is_empty())
                .then(|| {
                    crate::input::keybindings::KeybindingResolver::format_action_from_str(
                        &d.action_text,
                    )
                })
                .filter(|desc| {
                    desc.to_lowercase() != d.action_text.replace('_', " ").to_lowercase()
                });
            return Some(kb::Dialog::Edit(kb::Edit {
                title: match d.editing_index.is_some() {
                    true => t!("keybinding_editor.dialog_edit_title").to_string(),
                    false => t!("keybinding_editor.dialog_add_title").to_string(),
                },
                instructions: match d.capturing_special && key_focused {
                    true => t!("keybinding_editor.instr_capturing_special").to_string(),
                    false => match d.mode {
                        EditMode::RecordingKey => {
                            t!("keybinding_editor.instr_recording_key").to_string()
                        }
                        EditMode::EditingAction => {
                            t!("keybinding_editor.instr_editing_action").to_string()
                        }
                        EditMode::EditingContext => {
                            t!("keybinding_editor.instr_editing_context").to_string()
                        }
                    },
                },
                key_field: kb::Field {
                    label: t!("keybinding_editor.label_key").to_string(),
                    value: key_value,
                    hint: key_focused.then(|| match d.capturing_special {
                        true => t!("keybinding_editor.capture_any_key_hint").to_string(),
                        false => t!("keybinding_editor.capture_special_hint").to_string(),
                    }),
                    focused: key_focused,
                    invalid: false,
                    caret: false,
                    target: kb::Target::KeyField,
                },
                action_field: kb::Field {
                    label: t!("keybinding_editor.label_action").to_string(),
                    value: action_value,
                    hint: None,
                    focused: action_focused,
                    invalid: d.action_error.is_some(),
                    caret: action_focused && d.mode == EditMode::EditingAction,
                    target: kb::Target::ActionField,
                },
                action_description: described,
                context_field: kb::Field {
                    label: t!("keybinding_editor.label_context").to_string(),
                    value: format!("[{}]", d.context),
                    hint: ctx_focused
                        .then(|| t!("keybinding_editor.context_change_hint").to_string()),
                    focused: ctx_focused,
                    invalid: false,
                    caret: false,
                    target: kb::Target::ContextField,
                },
                error: d.action_error.clone(),
                conflicts_label: t!("keybinding_editor.conflicts_label").to_string(),
                conflicts: d.conflicts.clone(),
                save_label: t!("keybinding_editor.btn_save").to_string(),
                cancel_label: t!("keybinding_editor.btn_cancel").to_string(),
                focused_button: (d.focus_area == 3).then_some(d.selected_button),
                autocomplete: (d.autocomplete_visible && !d.autocomplete_suggestions.is_empty())
                    .then(|| kb::Autocomplete {
                        suggestions: d.autocomplete_suggestions.clone(),
                        selected: d.autocomplete_selected,
                    }),
            }));
        }

        if e.showing_confirm_dialog {
            return Some(kb::Dialog::Confirm(kb::Confirm {
                title: t!("keybinding_editor.confirm_title").to_string(),
                message: t!("keybinding_editor.confirm_message").to_string(),
                buttons: vec![
                    t!("keybinding_editor.btn_save").to_string(),
                    t!("keybinding_editor.btn_discard").to_string(),
                    t!("keybinding_editor.btn_cancel").to_string(),
                ],
                selected: e.confirm_selection,
            }));
        }
        None
    }

    pub(crate) fn trust_description(
        &self,
        size: ratatui::layout::Rect,
    ) -> Option<crate::view::shell::trust::Trust> {
        use crate::view::shell::trust::{Opt, Trust, DIALOG_WIDTH};
        if !self.workspace_trust_on_top() {
            return None;
        }
        // The dialog's extent is app logic keyed on the frame — the same shape
        // as the dock's bail-out, resolved before the description is built.
        let width = DIALOG_WIDTH.min(size.width.saturating_sub(4));
        let secondary_label = if self.workspace_trust_prompt_cancellable {
            fresh_i18n::t!("trust.dialog.btn_cancel").into_owned()
        } else {
            let quit_hint = self.keybindings.read().ok().and_then(|kb| {
                kb.get_keybinding_for_action(
                    &crate::input::keybindings::Action::Quit,
                    crate::input::keybindings::KeyContext::Normal,
                )
            });
            match quit_hint {
                Some(k) => fresh_i18n::t!("trust.dialog.btn_quit_key", key = k).into_owned(),
                None => fresh_i18n::t!("trust.dialog.btn_quit").into_owned(),
            }
        };
        let triggers = self.workspace_trust_markers.join(", ");
        Some(Trust {
            captures: self.popups_capture_keys(),
            selected: self.current_workspace_trust_selection(),
            title: fresh_i18n::t!("trust.dialog.security_warning").into_owned(),
            can_execute: fresh_i18n::t!("trust.dialog.can_execute").into_owned(),
            path_label: fresh_i18n::t!("trust.dialog.path_label").into_owned(),
            // Elided by the tree, at the width the dialog turned out to be —
            // the painter truncated it in the middle against a width it had to
            // compute first.
            path: self.working_dir().display().to_string(),
            detected: (!triggers.is_empty())
                .then(|| fresh_i18n::t!("trust.dialog.detected", triggers = triggers).into_owned()),
            how_proceed: fresh_i18n::t!("trust.dialog.how_proceed").into_owned(),
            options: crate::view::workspace_trust_dialog::options()
                .into_iter()
                .map(|o| Opt {
                    label: o.label,
                    description: o.description,
                })
                .collect(),
            ok_label: fresh_i18n::t!("trust.dialog.btn_ok").into_owned(),
            secondary_label,
            width,
            max_height: size.height.saturating_sub(2),
        })
    }

    /// Whether a file-explorer sidebar is showing, and if so how many columns
    /// it wants and which side it sits on.
    ///
    /// A *decision*, not a layout: the shell turns this into rectangles (see
    /// the frame layout at the top of `render`). Splitting the two is what let

    /// The sidebar's content THIS instant: its chrome, and one row per visible
    /// tree node.
    ///
    /// This is the old `FileExplorerRenderer::render` and `build_node_line`
    /// (both now deleted) with the
    /// geometry taken out. What is left is what the panel *says* — the title,
    /// each row's runs and the theme name each run paints in — and layout
    /// decides every column from it. `content_width`, `left_side_width`,
    /// `padding`, `trailing_slot_screen_bounds`: all gone, replaced by a flex
    /// spacer with a floor.
    ///
    /// `height` is the column's, which the caller derives from the same rule
    /// `Frame::fixed_rows` states — the viewport's row count is model state
    /// (`set_viewport_height` drives scrolling and the web projection), so it
    /// has to be known before the description exists.
    fn sidebar_content(
        &mut self,
        chrome_area: ratatui::layout::Rect,
        height: u16,
    ) -> Option<crate::view::shell::sidebar::Sidebar> {
        use crate::view::shell::sidebar::{Section, SectionKind, Sidebar};
        let should_show = self.file_explorer_visible()
            && (self.file_explorer().is_some()
                || self.active_window().file_explorer_sync_in_progress);
        if !should_show {
            return None;
        }
        let cols = self
            .active_window()
            .file_explorer_width
            .to_cols(chrome_area.width);
        let on_left = matches!(
            self.active_window().file_explorer_side,
            FileExplorerSide::Left
        );
        // Where the column starts, for the blur observer: beside the dock
        // on the left, or against the frame's edge on the right.
        let x0 = if on_left {
            chrome_area.x
        } else {
            (chrome_area.x + chrome_area.width).saturating_sub(cols)
        };
        let grip_hovered = matches!(self.shell_hover, Some(HoverTarget::FileExplorerBorder));
        // Squeeze what pressure must and size the rest — app logic keyed on
        // the column's height, resolved here for the reason
        // `Frame::resolve_dock` is (see `Editor::resolve_sidebar_sections`).
        // With one section the explorer's body is the column less its two
        // border rows, as the panel has always reserved.
        let rows = self.resolve_sidebar_sections(height);
        let plan: Vec<(crate::app::sidebar::SidebarSectionKind, bool)> = self
            .sidebar_sections
            .iter()
            .map(|s| (s.kind.clone(), s.collapsed))
            .collect();
        let mut sections = Vec::with_capacity(plan.len());
        for (i, (kind, collapsed)) in plan.into_iter().enumerate() {
            use crate::app::sidebar::SidebarSectionKind;
            let rows = rows.get(i).copied().unwrap_or(0);
            let section = match kind {
                SidebarSectionKind::Explorer => {
                    let e = self.explorer_section(rows);
                    Section {
                        kind: SectionKind::Explorer(e.kind),
                        title: e.title,
                        title_theme: e.title_theme,
                        border_theme: e.border_theme,
                        close_theme: e.close_theme,
                        rows,
                        collapsed,
                        focused: e.focused,
                        closable: true,
                    }
                }
                SidebarSectionKind::Panel {
                    title, closable, ..
                } => {
                    // The same call the dock makes, with the section's slot:
                    // `None` is a section whose plugin has not mounted it.
                    let slot = crate::app::PanelSlot::Sidebar(i);
                    let interior = self.panel_interior(slot);
                    let focused = interior.is_some() && self.panel(slot).is_some_and(|p| p.focused);
                    let (title_theme, border_theme) =
                        crate::view::shell::file_explorer::chrome_themes(false, focused);
                    let close_hovered = matches!(
                        self.shell_hover,
                        Some(HoverTarget::SidebarSectionClose(h)) if h == i
                    );
                    Section {
                        kind: match interior {
                            Some(p) => SectionKind::Panel(p),
                            None => SectionKind::Unavailable(
                                fresh_i18n::t!("explorer.section_unavailable").to_string(),
                            ),
                        },
                        title: format!(" {} ", title.trim()),
                        title_theme,
                        border_theme,
                        close_theme: crate::view::shell::file_explorer::close_theme(close_hovered),
                        rows,
                        collapsed,
                        focused,
                        closable,
                    }
                }
            };
            sections.push(section);
        }
        Some(Sidebar {
            cols,
            on_left,
            grip_hovered,
            x0,
            sections,
        })
    }

    /// The explorer as a section: its chrome, and its rows for a body
    /// `rows` tall.
    fn explorer_section(&mut self, rows: u16) -> ExplorerSection {
        use crate::view::shell::file_explorer as fe;
        // The explorer reads as focused only when it actually owns the
        // keyboard — not when a focused orchestrator dock has stolen it out
        // from under the (still-FileExplorer) window context.
        let focused = self.active_window().key_context == KeyContext::FileExplorer
            && !self.dock.as_ref().is_some_and(|d| d.focused);
        let remote = self.connection_display_string();
        let disconnected = remote
            .as_deref()
            .map(|c| c.contains("(Disconnected)"))
            .unwrap_or(false);
        let (title_theme, border_theme) = fe::chrome_themes(disconnected, focused);
        let close_hovered = matches!(self.shell_hover, Some(HoverTarget::FileExplorerCloseButton));
        let title = self.explorer_title(remote.as_deref());
        let (body, scroll) = self.explorer_body(rows, focused);
        let caret_row = focused.then(|| self.explorer_caret_row()).flatten();
        ExplorerSection {
            kind: fe::Explorer {
                body,
                caret_row,
                scroll,
            },
            title,
            title_theme,
            border_theme,
            close_theme: fe::close_theme(close_hovered),
            rows,
            focused,
        }
    }

    /// The panel's title: the search query while an incremental search is
    /// open, otherwise the name plus the focus keybinding, or the remote host.
    fn explorer_title(&self, remote: Option<&str>) -> String {
        if let Some(view) = self.file_explorer() {
            if view.is_search_active() {
                return format!(" /{} ", view.search_query());
            }
        }
        let suffix = self
            .keybindings
            .read()
            .unwrap()
            .get_keybinding_for_action(
                &crate::input::keybindings::Action::FocusFileExplorer,
                self.active_window().key_context.clone(),
            )
            .map(|kb| format!(" ({})", kb))
            .unwrap_or_default();
        match remote {
            Some(host) => {
                // Just the hostname out of "user@host" or "user@host:port".
                let name = host
                    .split('@')
                    .next_back()
                    .unwrap_or(host)
                    .split(':')
                    .next()
                    .unwrap_or(host);
                format!(" [{}]{} ", name, suffix)
            }
            None => format!(" File Explorer{} ", suffix),
        }
    }

    /// Which viewport row the caret sits on, when the panel owns the keyboard.
    fn explorer_caret_row(&self) -> Option<usize> {
        let view = self.file_explorer()?;
        let selected = view.get_selected_index()?;
        view.viewport_display_indices()
            .iter()
            .position(|&i| i == selected)
    }

    /// One row per visible tree node — or the loading placeholder while the
    /// tree is still being built.
    ///
    /// The viewport height is set here because it is model state: scrolling and
    /// the web projection both read it, and it must be current whether or not
    /// anything paints.
    #[allow(clippy::type_complexity)]
    fn explorer_body(
        &mut self,
        rows: u16,
        focused: bool,
    ) -> (
        crate::view::shell::file_explorer::Body,
        Option<crate::view::shell::file_explorer::Scroll>,
    ) {
        use crate::view::shell::file_explorer as fe;
        // The body's rows: the section's, borders already taken off.
        let viewport_rows = rows as usize;
        if let Some(view) = self.file_explorer_mut() {
            view.set_viewport_height(viewport_rows);
        }
        if self.file_explorer().is_none() {
            return (
                fe::Body::Loading(fresh_i18n::t!("explorer.loading").to_string()),
                None,
            );
        }
        let unsaved = self.explorer_unsaved_paths();
        let cut: Vec<std::path::PathBuf> = self
            .active_window()
            .file_explorer_clipboard
            .as_ref()
            .filter(|cb| cb.is_cut)
            .map(|cb| cb.paths.clone())
            .unwrap_or_default();
        let indicators = (
            self.config.file_explorer.tree_indicator_collapsed.clone(),
            self.config.file_explorer.tree_indicator_expanded.clone(),
        );
        let slot_resolver = self.file_explorer_slot_resolver();
        let theme = self.theme.read().unwrap().clone();
        let win = self.active_window();
        let view = win.file_explorer.as_ref().expect("checked above");
        let display = view.get_display_nodes();
        let indices = view.viewport_display_indices();
        let selected = view.get_selected_index();
        let multi = view.multi_selection();
        let search = view.is_search_active();
        let rows: Vec<fe::Row> = indices
            .iter()
            .enumerate()
            .filter_map(|(row, &actual)| {
                let &(node_id, indent) = display.get(actual)?;
                let matched = search.then(|| view.get_match_for_node(node_id)).flatten();
                crate::view::ui::file_explorer::describe_row(
                    crate::view::ui::file_explorer::RowDesc {
                        view,
                        node_id,
                        indent,
                        row,
                        is_cursor: selected == Some(actual),
                        is_multi: multi.contains(&node_id),
                        focused,
                        unsaved: &unsaved,
                        cut: &cut,
                        fuzzy: matched.as_ref(),
                        decorations: &win.file_explorer_decoration_cache,
                        slot_overrides: &win.file_explorer_slot_override_cache,
                        slot_resolver: &slot_resolver,
                        theme: &theme,
                        collapsed: &indicators.0,
                        expanded: &indicators.1,
                    },
                )
            })
            .collect();
        // The bar's numbers, in tree rows: what the tree holds, what the panel
        // shows, where the window sits, and how far it can go. `None` when the
        // whole tree fits, which is the whole of "should there be a bar"
        // (issue #2859). The ceiling is the model's own — pinned ancestors put
        // it past `total - rows`, and a bar that assumed otherwise showed the
        // thumb at the end while the wheel still moved the tree.
        let scroll = (viewport_rows > 0 && display.len() > viewport_rows).then(|| {
            let max_offset = view
                .max_scroll_offset()
                .min(display.len().saturating_sub(1));
            fe::Scroll {
                offset: view.get_scroll_offset().min(max_offset),
                max_offset,
                total: display.len(),
                rows: viewport_rows,
            }
        });
        (fe::Body::Rows(rows), scroll)
    }

    /// Paths with unsaved changes, which a row's status slot reads.
    fn explorer_unsaved_paths(&self) -> std::collections::HashSet<std::path::PathBuf> {
        let win = self.active_window();
        let mut out = std::collections::HashSet::new();
        for (buffer_id, state) in &win.buffers {
            if state.buffer.is_modified() {
                if let Some(p) = win
                    .buffer_metadata
                    .get(buffer_id)
                    .and_then(|m| m.file_path())
                {
                    out.insert(p.clone());
                }
            }
        }
        out
    }

    /// Returns the cell the sidebar wants the hardware caret parked on (its
    /// selected row) when it owns the keyboard, for the caller to commit at
    /// the end of the draw. See `view::shell::file_explorer`.
    /// Render the status bar into `area`, unless it's toggled off or a
    /// suggestions / file-browser popup is occupying the bottom row. The
    /// bar's inputs are gathered by [`Self::with_status_bar_ctx`], shared
    /// with the event-time layout derivation
    /// ([`Self::status_bar_layout_now`]).
    /// Record the status bar's theme-key provenance for the inspector.
    ///
    /// `StatusBarRenderer::render_status_bar` placed every element, drew it,
    /// and recorded provenance in one walk. The tree places, the fold draws,
    /// and this is the only part left.
    ///
    /// It used to publish a `StatusBarChrome` capture beside the runs, so the
    /// web `Scene` could read the segments back. The `Scene` asks the tree
    /// directly now ([`Self::shell_status_segments`]), which is why the
    /// early-return below no longer has a capture to clear.
    fn publish_status_bar(
        &mut self,
        area: ratatui::layout::Rect,
        has_suggestions: bool,
        has_file_browser: bool,
    ) {
        if !(self.active_window().status_bar_visible && !has_suggestions && !has_file_browser) {
            // No bar this frame — the user hid it, or a suggestions / file-
            // browser popup took the row. Nothing to record.
            return;
        }
        // The retained tree and a fresh one must still lay the frame out
        // alike: `render` goes through the `Ui` that persists across frames,
        // while `status_bar_area_now` builds a throwaway one, and stale
        // retained state skewing layout is exactly the failure a retained tree
        // makes possible.
        #[cfg(debug_assertions)]
        debug_assert_eq!(
            self.status_bar_area_now(),
            Some(area),
            "the retained tree and a fresh one must lay the frame out alike"
        );

        let Some(bar) = self.shell_frame_status_bar.clone() else {
            return;
        };
        let frame_rect = {
            let f = self.active_chrome().last_frame;
            ratatui::layout::Rect::new(0, 0, f.width, f.height)
        };
        let runs = {
            let Some(ui) = self.shell_ui.as_ref() else {
                return;
            };
            crate::view::shell::status_bar::provenance_runs(ui, &bar, frame_rect, area)
                .into_iter()
                .map(|(x, y, w, fg, bg)| crate::app::types::ThemeRun {
                    x,
                    y,
                    w,
                    // Validated and given back as `'static` in one step: a
                    // name that is not a real theme key reports `None`, which
                    // is what the inspector should say about it.
                    fg_key: fg
                        .as_deref()
                        .and_then(crate::view::theme::Theme::static_theme_key)
                        .map(std::borrow::Cow::Borrowed),
                    bg_key: bg
                        .as_deref()
                        .and_then(crate::view::theme::Theme::static_theme_key)
                        .map(std::borrow::Cow::Borrowed),
                    region: std::borrow::Cow::Borrowed("Status Bar"),
                })
                .collect::<Vec<_>>()
        };
        self.active_chrome_mut().apply_theme_runs(&runs);
    }

    /// Gather every status-bar input from live editor state and run `f`
    /// with the assembled [`crate::view::ui::status_bar::StatusBarContext`]
    /// and the user's status-bar config. Shared by the paint pass
    /// ([`Self::render_status_bar_row`]) and the event-time layout
    /// derivation ([`Self::status_bar_layout_now`]) so both see the SAME
    /// strings — the bar's geometry is content-dependent (rendered label
    /// widths: encoding, LSP state, cursor position, messages), so any
    /// drift between the two would move the clickable segments. Returns
    /// `None` when the active buffer is missing from the window's buffer
    /// map (teardown).
    pub(crate) fn with_status_bar_ctx<R>(
        &mut self,
        f: impl FnOnce(
            &mut crate::view::ui::status_bar::StatusBarContext<'_>,
            &crate::config::StatusBarConfig,
        ) -> R,
    ) -> Option<R> {
        let display_name_owned = self
            .active_window()
            .buffer_metadata
            .get(&self.active_buffer())
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| "[No Name]".to_string());
        let display_name = display_name_owned.as_str();
        let status_message = self.active_window().status_message.clone();
        let plugin_status_message = self.active_window().plugin_status_message.clone();
        // Compute a simple buffer-aware LSP indicator.
        // Compose the LSP status-bar segment for the active buffer. This
        // runs every render — the editor has no precomputed LSP-status
        // string cached anywhere else, so there is a single source of
        // truth for what the user sees.
        //
        // Priority order (first non-empty wins):
        //
        //   1. Active `$/progress` work for this language — e.g.
        //      "LSP (cpp): indexing (42%)". Conveys the transient
        //      startup/indexing phase.
        //   2. A running server — "LSP". Short because detail belongs
        //      in LSP-specific UI, not the compact status bar pill.
        //   3. Configured `auto_start=true` servers that haven't started
        //      (error / crashed / pending) — "LSP off".
        //   4. Configured `enabled && !auto_start` servers that the user
        //      has to opt into — "LSP: off (N)".
        //   5. Nothing.
        //
        // Rules 3 and 4 address heuristic eval H-1: without them, a
        // configured-but-dormant server is indistinguishable from "no
        // LSP at all."
        let current_language = self
            .buffers()
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
            .unwrap_or_default();
        let buffer_lsp_disabled_reason = self
            .active_window()
            .buffer_metadata
            .get(&self.active_buffer())
            .filter(|m| !m.lsp_enabled)
            .and_then(|m| m.lsp_disabled_reason.as_deref());
        let (lsp_status, lsp_indicator_state) = compose_lsp_status(
            &current_language,
            buffer_lsp_disabled_reason,
            &self.active_window().lsp_progress,
            &self.active_window().lsp_server_statuses,
            &self.config.lsp,
            &self.active_window().user_dismissed_lsp_languages,
            self.config.lsp_enabled,
        );
        let chord_state_cloned = self.active_window().chord_state.clone(); // Clone the chord state

        // Get update availability info
        let update_available = self.latest_version().map(|v| v.to_string());
        let self_update_phase = self.self_update_phase();

        // Get warning level for colored indicator (respects config setting)
        // LSP warning level is scoped to the current buffer's language
        let (warning_level, general_warning_count) = if self.config.warnings.show_status_indicator {
            let lsp_level = {
                use crate::services::async_bridge::LspServerStatus;
                let mut level = WarningLevel::None;
                for ((lang, _), status) in &self.active_window().lsp_server_statuses {
                    if lang == &current_language {
                        match status {
                            LspServerStatus::Error => {
                                level = WarningLevel::Error;
                                break;
                            }
                            LspServerStatus::Starting
                            | LspServerStatus::Initializing
                            // A server that stopped answering requests is
                            // a warning, not a healthy "on" (issue #2197).
                            | LspServerStatus::Unresponsive
                                if level != WarningLevel::Error =>
                            {
                                level = WarningLevel::Warning;
                            }
                            _ => {}
                        }
                    }
                }
                level
            };
            (
                lsp_level,
                self.active_window().warning_domains.general.count,
            )
        } else {
            (WarningLevel::None, 0)
        };

        // Which clickable status-bar segment (if any) the mouse is over —
        // drives hover styling generically (one variant for the whole bar).
        let status_bar_hovered = match &self.hovered() {
            Some(HoverTarget::StatusBarClickable(id)) => Some(*id),
            _ => None,
        };

        let remote_connection = self.connection_display_string();
        // Active window's last failed-reconnect error (drives a core
        // FailedAttach indicator for a dormant remote workspace).
        let remote_reconnect_error = self.active_window().remote_reconnect_error.clone();
        // The active window is a remote session whose window-derived
        // connect (dive / retry; see `start_remote_reconnect`'s request-id
        // scheme) is still in flight — its shell shows `Connecting`.
        let remote_connecting = self
            .remote_attach_inflight
            .contains(&(u64::MAX - self.active_window_id().0))
            && self.active_window().authority_spec.is_remote();

        // Get session label for display (only in session mode). The display
        // name, not `session_name`: an unnamed working-directory daemon has
        // no daemon name but is still labelled with its directory.
        let session_name = self.session_display_name().map(|s| s.to_string());

        let active_split = self.effective_active_split();
        let active_buf = self.active_buffer();
        let default_cursors = crate::model::cursor::Cursors::new();
        let is_read_only = self
            .active_window()
            .buffer_metadata
            .get(&active_buf)
            .map(|m| m.read_only)
            .unwrap_or(false);
        let is_synthetic_placeholder = self
            .active_window()
            .buffer_metadata
            .get(&active_buf)
            .map(|m| m.synthetic_placeholder)
            .unwrap_or(false);
        // Compute plugin-provided status-bar values before taking the
        // mutable window borrow below.
        let dynamic_status_bar_elements = self.get_status_bar_element_values(active_buf);
        // Active session's trust level for the always-present `{trust}`
        // indicator — read here (Copy) before the mutable window borrow.
        let workspace_trust_level = self.authority().workspace_trust.level();
        // Restart affordance for a terminal buffer whose process quit.
        // `exited_terminal` is `Some` only in exactly that state, so the
        // indicator can't offer to restart a live agent.
        let terminal_restart = self.active_window().exited_terminal(active_buf).map(|e| {
            crate::view::ui::status_bar::TerminalRestartState {
                program: e.program_name().map(str::to_string),
                exit_code: e.exit_code,
                resumes_agent: e.resumes_agent() && self.config.terminal.resume_agents,
            }
        });
        // Shared chrome inputs, locked here (rather than passed in) so
        // the event-time caller needs no per-frame clones. Field-level
        // borrows: the guards borrow `self.theme` / `self.keybindings`,
        // disjoint from the `self.windows` borrow below.
        let theme_guard = self.theme.read().unwrap();
        let theme = &*theme_guard;
        let keybindings_guard = self.keybindings.read().unwrap();
        let keybindings = &*keybindings_guard;
        // Single window borrow, split into buffers + cursors so the
        // status-bar context can hold both.
        let __active_id = self.active_window;
        let __win = self
            .windows
            .get_mut(&__active_id)
            .expect("active window must exist");
        __win
            .buffers
            .with_buffer_and_view_states(active_buf, |state, vs_map| {
                let cursors = vs_map
                    .get(&active_split)
                    .map(|v| &v.cursors)
                    .unwrap_or(&default_cursors);
                let mut status_ctx = crate::view::ui::status_bar::StatusBarContext {
                    state,
                    cursors,
                    status_message: &status_message,
                    plugin_status_message: &plugin_status_message,
                    lsp_status: &lsp_status,
                    lsp_indicator_state,
                    theme,
                    display_name,
                    keybindings,
                    chord_state: &chord_state_cloned,
                    update_available: update_available.as_deref(),
                    update_phase: self_update_phase,
                    warning_level,
                    general_warning_count,
                    hovered: status_bar_hovered,
                    remote_connection: remote_connection.as_deref(),
                    session_name: session_name.as_deref(),
                    read_only: is_read_only,
                    remote_state_override: self.remote_indicator_override.as_ref(),
                    remote_reconnect_error: remote_reconnect_error.as_deref(),
                    remote_connecting,
                    is_synthetic_placeholder,
                    // Filled in by `render_status` from the user's
                    // status_bar config; the value here is just a
                    // safe default for the rare path that builds the
                    // ctx but doesn't run `render_status`.
                    remote_indicator_on_bar: false,
                    dynamic_status_bar_elements: dynamic_status_bar_elements.clone(),
                    workspace_trust_level,
                    terminal_restart: terminal_restart.clone(),
                };
                f(&mut status_ctx, &self.config.editor.status_bar)
            })
    }

    /// The bottom-row visibility facts, computed ONCE: whether the
    /// active prompt is a floating overlay, whether a bottom-anchored
    /// suggestions popup is up, whether the file-browser dialog is up,
    /// and whether the prompt line reserves its row. The paint-time
    /// `Layout` split (`render`), the event-time derivations
    /// (`shell_frame`, `status_bar_area_now`,
    /// `search_options_content`) all read THIS — these conditions
    /// used to be hand-copied at four sites, three of them outside the
    /// paint-vs-derived parity oracle's reach.
    fn bottom_row_flags(&self) -> BottomRowFlags {
        let win = self.active_window();
        let prompt_is_overlay = win.prompt.as_ref().is_some_and(|p| p.overlay);
        let has_suggestions = win
            .prompt
            .as_ref()
            .is_some_and(|p| !p.suggestions.is_empty())
            && !prompt_is_overlay;
        let has_file_browser = win.prompt.as_ref().is_some_and(|p| {
            matches!(
                p.prompt_type,
                PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs
            )
        }) && win.file_open_state.is_some();
        let prompt_row_visible =
            (win.prompt_line_visible || win.prompt.is_some()) && !prompt_is_overlay;
        BottomRowFlags {
            prompt_is_overlay,
            has_suggestions,
            has_file_browser,
            prompt_row_visible,
        }
    }

    /// The frame's shape THIS instant: which regions are showing, how wide the
    /// sized ones are.
    ///
    /// The single derivation of that from state. `render` builds the frame from
    /// it, and the per-region `*_area_now` queries below resolve against the
    /// same description, so paint-time and event-time geometry cannot disagree
    /// — they are the same computation.
    ///
    /// This replaced `shell_frame`, which ran its own copy of the vertical
    /// `Layout` split at event time. Two implementations of one layout is the
    /// condition this migration exists to remove.
    /// `split` is the frame's dock/chrome division, computed once by the
    /// caller. It is passed rather than recomputed because `render` has
    /// already run `compute_dock_split` for this frame — and because a frame
    /// whose geometry came from one split while the paint used another is the
    /// class of bug this migration exists to remove, even when the function is
    /// pure and the two agree today.
    ///
    /// The frame size itself is not a parameter: `split` already carries every
    /// rectangle this reads, so taking the size as well would be a second way
    /// to say the same thing — and the two could then disagree, which is the
    /// bug above wearing a different hat.
    /// Lay the shell's retained tree out for one frame.
    ///
    /// The frame is where the tree follows the description: `fresh_ui`
    /// settles focus onto the scope's `autofocus` mark when that mark has
    /// moved, so a panel focus the host decided since the last frame — a
    /// `setFocusKey`, the dock's `/` — lands here, on the frame that carries
    /// it, and nothing outside the tree places focus in it. `render` and the
    /// widget-runtime tests both call this one function, so a step removed
    /// from it fails the tests that depend on the step.
    ///
    /// The tree is retained across frames — element state, focus and the dirty
    /// set live on it — so it is moved out for the duration rather than
    /// borrowed from `self`. `expect`, not `unwrap_or_default`: silently
    /// substituting a fresh `Ui` would discard every element's state, the
    /// focus position and the dirty set, and the frame would still render —
    /// the retained tree's whole point, lost without a symptom. If this is
    /// ever `None` a re-entrant path took it and did not put it back, which is
    /// a bug in that path.
    pub(crate) fn lay_out_shell_tree(
        &mut self,
        shell: crate::view::shell::frame::Frame,
        size: fresh_ui::Size,
    ) {
        self.lay_out_shell(shell, size);
        self.paint_shell();
    }

    /// The frame's first half: the tree reconciled, laid out and settled,
    /// its display list not yet painted. `render` runs the panes' content
    /// pass between this and [`Self::paint_shell`], so each pane's leaf
    /// paints — places its caret — from what the pass settled for this frame.
    pub(crate) fn lay_out_shell(
        &mut self,
        shell: crate::view::shell::frame::Frame,
        size: fresh_ui::Size,
    ) {
        let mut ui = self
            .shell_ui
            .take()
            .expect("the shell tree is taken and returned within one frame");
        crate::view::shell::geometry::stats::note_shell_layout();
        let panes = shell
            .splits
            .as_ref()
            .map(|s| (s.root.count_leaves(), s.active));
        ui.lay_out(crate::view::shell::frame::frame_tree(shell), size);
        tracing::debug!(
            target: "fresh::keys",
            w = size.w,
            h = size.h,
            ?panes,
            focus = ?ui.focused().and_then(|e| ui.key_of(e)),
            "shell laid out"
        );
        self.shell_ui = Some(ui);
        self.shell_description_stale = false;
    }

    /// The frame's second half: the display list of the tree as
    /// [`Self::lay_out_shell`] left it.
    pub(crate) fn paint_shell(&mut self) {
        let mut ui = self
            .shell_ui
            .take()
            .expect("the shell tree is taken and returned within one frame");
        ui.paint();
        self.shell_ui = Some(ui);
    }

    /// Lay the tree out from the current description when a write since the
    /// last frame has made it stale — see `Editor::shell_description_stale`.
    ///
    /// The frame builder is the one `render` uses, at the size the tree was
    /// last laid out for — or, for a tree that has never been laid out, the
    /// size the editor was made at: the tree is the whole keyboard (design
    /// §3.7.5), so a key that arrives before the first frame (a daemon's
    /// client typing before its terminal reported a size) is routed over the
    /// same description a frame would build, not dropped for want of one.
    pub(crate) fn lay_out_shell_if_stale(&mut self) {
        if !self.shell_description_stale {
            return;
        }
        let Some(mut size) = self.shell_ui.as_ref().map(|ui| ui.frame_size()) else {
            return;
        };
        if size.w == 0 || size.h == 0 {
            size = fresh_ui::Size::new(self.terminal_width, self.terminal_height);
        }
        if size.w == 0 || size.h == 0 {
            return;
        }
        let rect = ratatui::layout::Rect::new(0, 0, size.w, size.h);
        let split = self.compute_dock_split(rect);
        let shell = self.shell_frame(split);
        self.lay_out_shell_tree(shell, size);
    }

    /// The active window's pane leaves, collected so a caller can hold the
    /// tree borrowed while it asks.
    fn window_panes_leaves(&self) -> Vec<crate::model::event::LeafId> {
        self.window_panes()
            .into_iter()
            .map(|(leaf, _)| leaf)
            .collect()
    }

    pub(crate) fn shell_frame(
        &mut self,
        split: (Option<ratatui::layout::Rect>, ratatui::layout::Rect),
    ) -> crate::view::shell::frame::Frame {
        let BottomRowFlags {
            prompt_is_overlay: _,
            has_suggestions,
            has_file_browser,
            prompt_row_visible,
        } = self.bottom_row_flags();
        let (dock_area, chrome_area) = split;
        // The dialog's height, which the painter computed from the prompt
        // row's `y`: the space above it, less the menu bar's row, capped at
        // 20. Its placement is the tree's — above the prompt row and as wide
        // as it — and so is its content now (`browser_description`).
        let browser = has_file_browser
            .then(|| self.browser_description(chrome_area.height.saturating_sub(2).min(20)))
            .flatten();
        let menu_bar_visible = self.active_window().menu_bar_visible;
        // One walk for the whole menu: the bar's labels and, when one is open,
        // its dropdown chain. Skipped entirely when the bar is hidden.
        //
        // The bar's rectangle is derived here rather than read back off the
        // last frame's tree. This is `build`, and build must not depend on
        // layout — the rectangle it would read is one frame stale, and asking
        // for it at all is the loop the library refuses. It needs no layout:
        // the bar is the chrome column's top row.
        let win_status_bar = self.active_window().status_bar_visible;
        let search_options = self.search_options_content();
        // The menu, as content: its labels and the open chain. Where each
        // label sits and how wide each box is are the tree's to decide, and
        // the web reads them back off it (`menu_view`).
        let (menu_bar_items, dropdowns) = match menu_bar_visible {
            true => self.menu_description(),
            false => Default::default(),
        };
        // The sidebar's content. Its height is the chrome column minus the
        // fixed rows — the rule `Frame::fixed_rows` states, applied here
        // because the panel's viewport row count is model state that has to be
        // current before the description is built, not a rectangle read back
        // afterwards.
        // Whether the bar's row exists at all. Computed once: it decides the
        // row's height, whether its elements are built, and how many rows the
        // explorer has left — and those three must not be able to disagree.
        let status_row = win_status_bar && !has_suggestions && !has_file_browser;
        let fixed = crate::view::shell::frame::fixed_rows(
            menu_bar_visible,
            status_row,
            search_options.is_some(),
            prompt_row_visible,
        );
        let explorer_h = chrome_area.height.saturating_sub(fixed);
        let sidebar = self.sidebar_content(chrome_area, explorer_h);
        // The bar's elements, measured from the chrome column it will occupy.
        let status_bar_items = status_row
            .then(|| self.status_bar_description(chrome_area.width))
            .flatten();
        self.shell_frame_status_bar = status_bar_items.clone();
        let menu_keys = self.menu_shortcuts();
        let suggestions = self.suggestions_description();
        let card = self.overlay_card_description(chrome_area);
        let popups = self.popup_descriptions(chrome_area);
        let theme_info = self.theme_info_description();
        // The grid's shape, for the tree to lay out. Cloned rather than
        // borrowed: a description is a value, and this one is a handful of
        // nodes.
        let pane_chrome = self.pane_chrome();
        // What every bar shows this frame, settled on the panes' handles for
        // the bars' leaves to paint (design §3.7.6).
        self.settle_pane_bars(&pane_chrome);
        let groups = self.active_window().pane_groups();
        let interiors = self.pane_interiors();
        let strips = self.pane_strips(&pane_chrome);
        let hosts = self.pane_hosts(&interiors, &groups);
        // **The pane that has the keyboard is the *effective* active split.**
        // A buffer group's host leaf is the window's active split while the
        // keyboard is in one of the group's own panes; the pane the keys
        // belong to — and the one the base marks as its focus holder — is
        // that inner pane. Its panel holds the keyboard whenever nothing
        // above it does: its layer is declared first, so every other
        // keyboard layer outranks it by declaration order.
        let active_pane = self.effective_active_split();
        let pane_keys = Some(active_pane).filter(|a| interiors.contains_key(a));
        let contexts = self.pane_contexts(active_pane);
        // The leaf's settled keyboard facts: its context, which
        // `get_key_context` reads when the focus chain names the pane's
        // content, and — the PTY gate's input — whether it takes the keyboard
        // raw, from which the tree derives whether it is reachable
        // (`Ui::raw_input`, design §3.7.8).
        for (pane, h) in &hosts {
            let context = contexts
                .get(pane)
                .cloned()
                .unwrap_or(crate::input::keybindings::KeyContext::Normal);
            h.set_raw_input(context == crate::input::keybindings::KeyContext::Terminal);
            h.set_context(context);
        }
        // A window with nothing to show yet — a dormant remote session, a
        // workspace still being built — describes a placeholder page in the
        // body instead of its (empty, uneditable) grid.
        let placeholder = self.placeholder_page();
        let splits = self.active_window().buffers.splits().map(|(mgr, _)| {
            // Which buttons the strips carry, by the painter's own rule. Both
            // are frame-wide: they read "is there more than one pane" and "is
            // one maximized", neither of which names a pane.
            let is_maximized = mgr.is_maximized();
            let several = mgr.visible_leaves().len() > 1;
            crate::view::shell::splits::Splits {
                root: mgr.root().clone(),
                maximized: mgr.maximized_split().map(crate::model::event::LeafId),
                active: Some(active_pane),
                chrome: pane_chrome.clone(),
                controls: crate::view::shell::splits::PaneControls {
                    maximize: several || is_maximized,
                    close: several && !is_maximized,
                },
                groups,
                interiors,
                strips,
                hover: self.shell_hover.clone(),
                drop_zone: self
                    .active_window()
                    .mouse_state
                    .dragging_tab
                    .as_ref()
                    .filter(|d| d.is_dragging())
                    .and_then(|d| d.drop_zone),
                hosts,
            }
        });
        let trust = self.trust_description(ratatui::layout::Rect {
            x: 0,
            y: 0,
            width: self.active_chrome().last_frame.width,
            height: self.active_chrome().last_frame.height,
        });
        let splits = match placeholder.is_some() {
            true => None,
            false => splits,
        };
        crate::view::shell::frame::Frame {
            placeholder,
            panel: self.panel_description(),
            // The dock's content, described whenever a panel is mounted in
            // the slot. There is no gate left: `panel_interior` answers
            // `None` only for an empty slot, the same way it does for the
            // floating panel. (It used to consult `covered`, which asked
            // whether the adapter had an arm for every variant of the spec;
            // that function ran out of `false` arms and was deleted in 2.4.)
            dock_interior: self.panel_interior(crate::app::PanelSlot::Dock),
            dock_grip_hovered: matches!(
                self.shell_hover,
                Some(crate::app::types::HoverTarget::DockBorder)
            ),
            dock_focused: self.dock.as_ref().is_some_and(|d| d.focused),
            // Which workspace the window-owned half of the frame belongs to.
            // One retained tree, N windows: without this the two match each
            // other and window B's first pane inherits window A's element
            // state. See `Frame::window`.
            window: Some(self.active_window.0),
            theme_info,
            browser,
            trust,
            event_debug: self.event_debug_description(),
            settings: self.settings_chrome_description(),
            settings_dialog: self.settings_dialog_description(),
            settings_entry: self.settings_entry_description(),
            keybinding: self.keybinding_chrome_description(),
            keybinding_table: self.keybinding_table_description(),
            keybinding_dialog: self.keybinding_dialog_description(),
            calibration: self.calibration_description(),
            splits,
            menu_bar: menu_bar_visible,
            status_bar: status_row,
            search_options,
            status_bar_items,
            prompt_line: prompt_row_visible,
            prompt_row: self.prompt_row_description(),
            // The keyboard's owner, which is not the same question as the
            // row's: the overlay form of the prompt draws no prompt row and
            // still owns every key. `chrome::Prompt::layers` asked
            // `is_prompting()` for exactly this and so does the layer.
            prompt_keys: self.is_prompting(),
            search_prompt: self.active_prompt_has_search_options(),
            // A focused panel is the keyboard's owner, which is what
            // `chrome::Dock::layers` and `chrome::FloatingModal::layers` say
            // with `owns_keyboard` — asked here so the frame can declare the
            // layer that used to be a rank.
            dock_keys: self.dock.as_ref().is_some_and(|d| d.focused),
            panel_keys: self
                .floating_widget_panel
                .as_ref()
                .is_some_and(|f| f.focused),
            pane_keys,
            dock: dock_area.map(|d| d.width),
            sidebar,
            menu: self.open_context_menu_for_shell(),
            menu_keys,
            menu_bar_items,
            dropdowns,
            suggestions,
            popups,
            card,
        }
    }

    /// Which chrome each visible pane has, by leaf, for the active window.
    ///
    /// The frame-wide offer, handed to the one gathering (`Window::pane_chrome`).
    fn pane_chrome(
        &self,
    ) -> std::collections::HashMap<
        crate::model::event::LeafId,
        crate::view::shell::splits::PaneChrome,
    > {
        self.active_window()
            .pane_chrome(crate::view::shell::splits::PaneChrome {
                tabs: self.active_window().tab_bar_visible,
                vscroll: self.config.editor.show_vertical_scrollbar,
                hscroll: self.config.editor.show_horizontal_scrollbar,
            })
    }

    /// Every popup on screen, as the shell describes it.
    ///
    /// Size and strategy — never a rectangle. Where each one lands is the
    /// tree's answer, read back with `popup::rects_of`; `calculate_area`'s six
    /// strategies said it six times, each ending in its own clamp.
    ///
    /// The order is the order they paint in, which is the order the two
    /// painters already ran in: the buffer's whole stack, then the top of the
    /// global one over it. Only the top global popup is ever drawn — deeper
    /// ones surface as it resolves — and the workspace-trust prompt is not
    /// here at all, because it renders later in the modal band on its own
    /// dimmed backdrop.
    fn popup_descriptions(
        &self,
        chrome: ratatui::layout::Rect,
    ) -> Vec<crate::view::shell::popup::Placed> {
        use crate::view::shell::popup::{Body, CaretAnchor, Placed};
        let describe = |p: &crate::view::popup::Popup| Placed {
            position: p.position,
            at: CaretAnchor::for_kind(p.kind),
            size: p.asked_size(chrome),
            body: Body {
                title: p.render_title(),
                description: p.description.clone(),
                content: p.content.clone(),
                bordered: p.bordered,
                // The workspace-trust prompt is a forced choice, so it has no
                // close button — the painter's `dismissible`.
                dismissible: !matches!(
                    p.resolver,
                    crate::view::popup::PopupResolver::WorkspaceTrust
                ),
                selected_hint: p.accept_key_hint.clone(),
            },
            transient: p.transient,
            keys: None,
        };
        let mut out: Vec<Placed> = Vec::new();
        let state = self.active_state();
        if state.popups.is_visible() {
            out.extend(state.popups.all().iter().map(describe));
        }
        if !self.workspace_trust_on_top() {
            out.extend(self.global_popups.top().map(describe));
        }
        // **One popup holds the keyboard, and it is the last one described.**
        // `dispatch_popup_keys` said the same thing as an order of rungs —
        // global stack first, then the buffer's — and the order they are
        // pushed in above is that order, so the one on top is the last. Said
        // once here rather than re-derived by a walk that could disagree with
        // the one that painted.
        // Not while the workspace-trust prompt is up: it is a popup that
        // `popups_capture_keys` answers `true` for and that is deliberately
        // *not* described here (it renders in the modal band on its own
        // backdrop), so the last one in this list is somebody else.
        if self.popups_capture_keys() && !self.workspace_trust_on_top() {
            if let Some(top) = out.last_mut() {
                top.keys = Some(self.popup_keys());
            }
        }
        out
    }

    /// What the keymap binds for the popup holding the keyboard.
    ///
    /// The two `resolve_*_popup_action` calls, turned inside out. They asked
    /// the keymap for *this* key as it arrived, from inside a walk the shell
    /// tree is offered the key before — which is how a `menu`-section binding
    /// came to be swallowed before the keymap was ever consulted. Enumerating
    /// the bindings instead puts them on the popup, where nothing is in front
    /// of them.
    fn popup_keys(&self) -> crate::view::shell::popup::Keys {
        use crate::view::popup::PopupKind;
        let kind = self.topmost_popup_kind().unwrap_or(PopupKind::Action);
        let mut bound = Vec::new();
        if let Ok(kb) = self.keybindings.read() {
            // **One section per kind, and only the actions that section is
            // for.** A completion list reads `completion` and takes only
            // accept and dismiss from it; every other kind reads `popup`.
            //
            // Adding `popup` to the completion list's sections looked
            // harmless and was not: that section binds Enter to
            // `popup_confirm`, so Enter over a completion list accepted the
            // selected row — and Enter there means "close this and insert a
            // newline", which is the whole reason the layer declares no
            // `Confirm` intent. `completion_popup_action` never consulted
            // `popup` for exactly this reason ("only `CompletionAccept` and
            // `CompletionDismiss` are recognised here"), and dropping that
            // restriction put the confirm back in through the side door.
            let (ctx, wanted) = crate::view::shell::popup::key_section(kind);
            for ((code, mods), action) in kb.bindings_in_context(ctx) {
                // Only the actions that section is *for*. A binding for
                // anything else in it is the base layer's and reaches it the
                // ordinary way.
                if !wanted.contains(&action) {
                    continue;
                }
                let Some(code) = crate::view::shell::input::key_code(code) else {
                    continue;
                };
                let key = fresh_ui::KeyPress {
                    code,
                    mods: crate::view::shell::input::mods(mods),
                };
                if bound
                    .iter()
                    .any(|(k, _): &(fresh_ui::KeyPress, _)| *k == key)
                {
                    continue;
                }
                bound.push((key, action));
            }
        }
        crate::view::shell::popup::Keys { kind, bound }
    }

    /// The kind of the popup holding the keyboard, on the same rule.
    fn topmost_popup_kind(&self) -> Option<crate::view::popup::PopupKind> {
        match self.global_popups.is_visible() {
            true => self.global_popups.top().map(|p| p.kind),
            false => self.active_state().popups.top().map(|p| p.kind),
        }
    }

    /// The floating-overlay prompt's card, as the shell describes it.
    ///
    /// Only the outer rectangle and two counts. Everything the painter derived
    /// from them — the header band's height, where the body starts, how the
    /// body splits — is what the description states, and
    /// `overlay_prompt::regions_of` is where the painter reads it back.
    ///
    /// The toolbar's row count is the one thing that has to be *measured*
    /// rather than declared: a plugin's toolbar is two rows on a wide terminal
    /// and wraps to more on a narrow one, and only the widget runtime knows.
    /// `render_overlay_prompt` measured it too, at the same width and with the
    /// same arguments; this is now the only call, and the painter reads the
    /// answer back with the rest of the geometry.
    fn overlay_card_description(
        &self,
        chrome: ratatui::layout::Rect,
    ) -> Option<crate::view::shell::overlay_prompt::Card> {
        use crate::view::shell::overlay_prompt::Card;
        let prompt = self.active_window().prompt.as_ref()?;
        if !prompt.overlay {
            return None;
        }
        let at = Self::centered_overlay_rect(chrome, 90, 90);
        let toolbar = self.prompt_toolbar_interior();
        let default_title;
        let title_segs: &[fresh_core::api::StyledText] = if prompt.title.is_empty() {
            default_title = self.default_overlay_title();
            &default_title
        } else {
            &prompt.title
        };
        let title = crate::view::shell::overlay_prompt::styled_runs(
            title_segs,
            "ui.prompt_fg",
            "ui.suggestion_bg",
        );
        let footer_runs = crate::view::shell::overlay_prompt::styled_runs(
            &prompt.footer,
            "ui.suggestion_fg",
            "ui.suggestion_bg",
        );
        let count = (!prompt.suggestions.is_empty()).then(|| {
            (
                prompt.selected_suggestion.map(|i| i + 1).unwrap_or(0),
                prompt.suggestions.len(),
            )
        });
        Some(Card {
            at: fresh_ui::Rect::new(at.x as i32, at.y as i32, at.width, at.height),
            title,
            footer_runs,
            input: crate::view::shell::prompt_line::PromptRow {
                message: prompt.message.clone(),
                input: prompt.input_str().to_string(),
                cursor: prompt.cursor_byte(),
                selection: prompt.selection_range(),
                dir: None,
            },
            status: prompt.status.clone(),
            count,
            // No widget toolbar: the title row takes one, or nothing does.
            title_row: toolbar.is_none() && !prompt.title.is_empty(),
            toolbar,
            footer: !prompt.footer.is_empty(),
            search: self.active_prompt_has_search_options(),
            input_focused: !self.prompt_toolbar_holds_keyboard(),
        })
    }

    /// The prompt's suggestion list as the shell describes it.
    ///
    /// Content only — no window and no rectangle. The painter kept a
    /// `scroll_offset` to say which slice was visible and recorded the box it
    /// drew; `list().windowed(..)` asks for the rows it can show and
    /// `Anchor::Node` + `Place::Above` place the layer, so neither is stored
    /// on this side any more.
    ///
    /// **Only the bottom-anchored form.** The floating-overlay prompt draws
    /// its list inside a card whose rectangle its own painter computes later
    /// in the frame, so `Place::Inside` has nothing to be given yet; and the
    /// file-browser prompts draw a different popup entirely. Both are the
    /// painter's still, and both return `None` here so the two never draw the
    /// same list.
    /// The file-open dialog, as content.
    ///
    /// What `FileBrowserRenderer` read off `FileOpenState`, the keybinding
    /// resolver and the hover target on its way to the cells — the same reads,
    /// made once into a value the tree lays out and the web projects
    /// (`file_browser_view`), so the two cannot disagree about a label.
    pub(crate) fn browser_description(
        &self,
        height: u16,
    ) -> Option<crate::view::shell::file_browser::Browser> {
        use crate::app::file_open::{format_modified, format_size, FileOpenSection, Toggle};
        use crate::input::keybindings::{Action, KeyContext};
        use crate::view::shell::file_browser::{Browser, Entry, Listing, Shortcut, ToggleItem};
        let state = self.active_window().file_open_state.as_ref()?;
        let shortcut_of = |a: Action| -> Option<String> {
            self.keybindings
                .read()
                .ok()
                .and_then(|kb| kb.get_keybinding_for_action(&a, KeyContext::Prompt))
        };
        // The painter underlined the `E` of `Encoding` by splitting a
        // hardcoded string. The label is localized; the mnemonic is wherever
        // that character landed, or nowhere.
        let detect = fresh_i18n::t!("file_browser.detect_encoding").to_string();
        let toggles = vec![
            ToggleItem {
                id: Toggle::ShowHidden,
                label: fresh_i18n::t!("file_browser.show_hidden").to_string(),
                mnemonic: None,
                shortcut: shortcut_of(Action::FileBrowserToggleHidden),
                active: state.show_hidden,
            },
            ToggleItem {
                id: Toggle::DetectEncoding,
                mnemonic: detect.find('E'),
                label: detect,
                shortcut: shortcut_of(Action::FileBrowserToggleDetectEncoding),
                active: state.detect_encoding,
            },
        ];
        let listing = if state.loading {
            Listing::Loading
        } else if let Some(e) = &state.error {
            Listing::Error(e.clone())
        } else {
            Listing::Entries(
                state
                    .entries
                    .iter()
                    .map(|e| {
                        let meta = e.fs_entry.metadata.as_ref();
                        Entry {
                            name: e.fs_entry.name.clone(),
                            is_dir: e.fs_entry.is_dir(),
                            is_symlink: e.fs_entry.is_symlink(),
                            size: (!e.fs_entry.is_dir())
                                .then(|| meta.map(|m| format_size(m.size)))
                                .flatten(),
                            modified: meta.and_then(|m| m.modified).map(format_modified),
                            matches: e.matches_filter,
                        }
                    })
                    .collect(),
            )
        };
        let files = state.active_section == FileOpenSection::Files;
        let hover = match self.shell_hover {
            Some(crate::app::types::HoverTarget::FileBrowser(part)) => Some(part),
            _ => None,
        };
        Some(Browser {
            height,
            dir: state.current_dir.clone(),
            toggles,
            shortcuts: state
                .shortcuts
                .iter()
                .map(|s| Shortcut {
                    label: s.label.clone(),
                    description: s.description.clone(),
                })
                .collect(),
            selected_shortcut: (!files).then_some(state.selected_shortcut),
            sort: state.sort_mode,
            ascending: state.sort_ascending,
            listing,
            selected: files.then_some(state.selected_index).flatten(),
            hover,
        })
    }

    fn suggestions_description(&self) -> Option<crate::view::shell::prompt::Suggestions> {
        use crate::view::shell::prompt::{SuggestionRow, Suggestions};
        let prompt = self.active_window().prompt.as_ref()?;
        if prompt.suggestions.is_empty() {
            return None;
        }
        // The file-browser prompts draw a different popup entirely — a browser
        // card, not a list (`file_browser::layer`).
        if matches!(
            prompt.prompt_type,
            crate::view::prompt::PromptType::OpenFile
                | crate::view::prompt::PromptType::SwitchProject
                | crate::view::prompt::PromptType::SaveFileAs
        ) {
            return None;
        }
        // Which of the two lists this is, and therefore where it goes. The
        // overlay's rectangle is the card's results band, which the shell tree
        // placed a moment ago — that is what `overlay_prompt` moving made
        // possible.
        let place = match prompt.overlay {
            true => crate::view::shell::prompt::Place::InCard,
            false => crate::view::shell::prompt::Place::AbovePrompt,
        };
        Some(Suggestions {
            rows: prompt
                .suggestions
                .iter()
                .map(|s| SuggestionRow {
                    name: s.text.clone(),
                    keybinding: s.keybinding.clone(),
                    description: s.description.clone(),
                    description_spans: s
                        .description_spans
                        .as_ref()
                        .map(|v| v.iter().map(Self::description_span).collect()),
                    // Character for character what `push_source_column`
                    // wrote: the plugin's own name, or the word for a
                    // built-in.
                    source: s.source.as_ref().map(|src| match src {
                        crate::input::commands::CommandSource::Builtin => "builtin".to_string(),
                        crate::input::commands::CommandSource::Plugin(name) => name.clone(),
                    }),
                    disabled: s.disabled,
                })
                .collect(),
            selected: prompt.selected_suggestion,
            // Last frame's window, for the column widths only — see
            // `Suggestions::window`. `suggestions_area` is where
            // `record_suggestions_geometry` put it.
            window: self
                .active_chrome()
                .suggestions_area
                .map(|(_, first, visible, _)| (first, visible)),
            place,
            // The row the painter drew under the popup, now stacked in the
            // layer with it. `render_quick_open_hints` is what this replaces.
            hints: (!prompt.overlay
                && prompt.prompt_type == crate::view::prompt::PromptType::QuickOpen)
                .then(|| fresh_i18n::t!("quick_open.mode_hints").to_string()),
        })
    }

    /// A plugin's styled description span, as a name rather than a colour.
    ///
    /// `styled_span_style` resolved an `OverlayColorSpec` against the theme
    /// here and handed the painter a concrete `Color`, which the theme
    /// inspector could not explain afterwards and a user could not override. A
    /// `ThemeKey` spec passes through as the key it is; an `Rgb` one becomes
    /// the `#rrggbb` literal `shell_theme` reads — untraceable either way,
    /// because a plugin's arbitrary colour has no name, but now it is only the
    /// literals that are.
    fn description_span(
        st: &fresh_core::api::StyledText,
    ) -> crate::view::shell::prompt::DescriptionSpan {
        use crate::view::shell::prompt::DescriptionSpan;
        let name = |c: &fresh_core::api::OverlayColorSpec| match c.as_rgb() {
            Some((r, g, b)) => format!("#{r:02x}{g:02x}{b:02x}"),
            None => c.as_theme_key().unwrap_or_default().to_string(),
        };
        let Some(opts) = st.style.as_ref() else {
            return DescriptionSpan {
                text: st.text.clone(),
                ..DescriptionSpan::default()
            };
        };
        let mut attrs: Vec<&'static str> = Vec::new();
        for (on, a) in [
            (opts.bold, "bold"),
            (opts.italic, "italic"),
            (opts.underline, "underline"),
            (opts.strikethrough, "strikethrough"),
        ] {
            if on {
                attrs.push(a);
            }
        }
        DescriptionSpan {
            text: st.text.clone(),
            fg: opts.fg.as_ref().map(&name),
            bg: opts.bg.as_ref().map(&name),
            attrs,
        }
    }

    /// The open context menu as the shell describes it: the point it was
    /// opened at, what is in it, which row is highlighted.
    ///
    /// The point is raw. Keeping the box on screen is the layer's `Fit::CLAMP`,
    /// so the placement is decided once, by layout, and read back by everyone
    /// who needs it ([`Editor::shell_menu_rect`]).
    ///
    /// Derived whether or not chrome cells are suppressed: the web bridge does
    /// not want the menu's *cells*, but it does want its rectangle, and the
    /// tree is where rectangles come from. The fold is what skips the cells.
    fn open_context_menu_for_shell(&self) -> Option<crate::view::shell::context_menu::Menu> {
        let (_, core) = self.active_window().open_context_menu()?;
        let items = self.active_window().context_menu_labels()?;
        Some(crate::view::shell::context_menu::Menu {
            x: core.position.0,
            y: core.position.1,
            width: core.width,
            highlighted: core.highlighted,
            items,
        })
    }

    /// One region's rectangle THIS instant.
    ///
    /// Read off the retained tree — the layout the last frame produced — not
    /// recomputed. Goal 5 of the library is that layout computes rectangles
    /// and everything else *reads* them; building a throwaway `Ui` here would
    /// be a second layout of the same tree, per query, several times per
    /// pointer event, with all element state discarded.
    pub(crate) fn shell_region_now(
        &self,
        region: crate::view::shell::frame::HostRegion,
    ) -> ratatui::layout::Rect {
        let frame = self.active_chrome().last_frame;
        let size = ratatui::layout::Rect::new(0, 0, frame.width, frame.height);
        let Some(ui) = self.shell_ui.as_ref() else {
            return ratatui::layout::Rect::default();
        };
        crate::view::shell::frame::regions_of(ui, size)
            .into_iter()
            .find(|(r, _)| *r == region)
            .map(|(_, rect)| rect)
            .unwrap_or_default()
    }

    /// Where the open context menu landed, read off the retained tree.
    ///
    /// The partner of [`Self::shell_region_now`] for a surface that is not a
    /// host region: layout placed the menu (its anchor point, pulled inside
    /// the frame by `Fit::CLAMP`) and this reads the answer back. The web
    /// `Scene` draws its own menu and needs the cells it covers; asking here
    /// is what keeps the two frontends on one rectangle.
    pub(crate) fn shell_menu_rect(&self) -> Option<fresh_ui::Rect> {
        crate::view::shell::context_menu::menu_rect(self.shell_ui.as_ref()?.spec())
    }

    /// The status bar's segments, read off the retained tree.
    ///
    /// The third of the same family as [`Self::shell_region_now`] and
    /// [`Self::shell_menu_rect`]: layout placed these, and this reads the
    /// answer back. It replaced a `StatusBarChrome` capture that `render`
    /// filled from this very walk and the web `Scene` read back a moment
    /// later — a second copy of an answer the tree already held, which had to
    /// be cleared by hand on the frames where the bar is hidden (a suggestions
    /// or file-browser popup owns the row) or the web kept drawing a bar the
    /// TUI no longer had.
    pub(crate) fn shell_status_segments(
        &self,
    ) -> Vec<crate::view::ui::status_bar::StatusSegmentInfo> {
        let (Some(ui), Some(bar)) = (self.shell_ui.as_ref(), self.shell_frame_status_bar.as_ref())
        else {
            return Vec::new();
        };
        let f = self.active_chrome().last_frame;
        let size = ratatui::layout::Rect::new(0, 0, f.width, f.height);
        crate::view::shell::status_bar::segments(ui, bar, size)
    }

    /// The status bar's screen area THIS instant, derived from live state:
    /// the same visibility conditions and vertical frame split `render`
    /// uses ([`Self::shell_frame`]; asserted against the paint pass in
    /// debug builds). `None` when the bar is hidden (toggled off, or a
    /// suggestions / file-browser popup takes the bottom rows) — gated on
    /// the CONDITIONS, not the chunk height: on a tiny terminal the paint
    /// pass runs over the same squeezed (possibly zero-height) rect and
    /// records the same (empty) layout, so returning it keeps the parity
    /// oracle exact.
    pub(crate) fn status_bar_area_now(&self) -> Option<ratatui::layout::Rect> {
        let flags = self.bottom_row_flags();
        if !self.active_window().status_bar_visible
            || flags.has_suggestions
            || flags.has_file_browser
        {
            return None;
        }
        Some(self.shell_region_now(crate::view::shell::frame::HostRegion::StatusBar))
    }

    /// The menu's content this frame: the bar's labels and the open dropdown
    /// chain, from the menu state, the keymap's accelerators and mnemonics,
    /// and the shell's own hover. See `view::shell::menu::describe`.
    pub(crate) fn menu_description(
        &self,
    ) -> (
        crate::view::shell::menu::MenuBar,
        Vec<crate::view::shell::menu::DropdownLevel>,
    ) {
        let all_menus = self.all_menus_expanded();
        let keybindings = self.keybindings.read().unwrap();
        crate::view::shell::menu::describe(&crate::view::shell::menu::MenuModel {
            menus: &all_menus,
            state: &self.menu_state,
            keybindings: &keybindings,
            hover: self.shell_hover.as_ref(),
            mnemonics: self.config.editor.menu_bar_mnemonics,
        })
    }

    /// The status bar's layout THIS instant — area plus the content-derived
    /// segment geometry (clickable spans, plugin token areas), from the same
    /// element/width/placement walk the description is built by. Geometry
    /// produced by layout, not recorded by paint: every cache this once had
    /// on the side — `clickable`, `plugin_token_areas`, and finally the whole
    /// `StatusBarChrome` capture — is retired. `None` when the bar is hidden.
    /// What the full-screen modals still need from the painter's pass: the
    /// page heights their keyboards move by, read off the boxes the tree
    /// placed. Every one of them — settings, the calibration wizard, the
    /// keybinding editor and the event-debug dialog — is drawn by the tree,
    /// scrim included.
    /// The placeholder page the body shows instead of the grid, for a
    /// window that has nothing to show yet: a dormant remote session (nothing
    /// can be shown or edited until the backend connects, so a tab bar and a
    /// "[No Name]" scratch buffer would misrepresent the state) or a
    /// workspace whose worktree or agent is still being built. Deliberately
    /// the *same* page for both: from the user's side each is "this
    /// workspace isn't ready yet", and one look should mean one thing.
    fn placeholder_page(&self) -> Option<crate::view::shell::frame::Placeholder> {
        use crate::view::shell::frame::Placeholder;
        let active_id = self.active_window;
        let window = self.windows.get(&active_id)?;
        let pane = window.buffers.splits().map(|(mgr, _)| {
            crate::model::event::LeafId(fresh_core::SplitId(mgr.active_split().0 .0))
        });
        if self.dormant_remote.contains_key(&active_id) {
            let label = window.label.clone();
            let detail = window
                .authority_spec
                .remote_backend_info(false)
                .map(|r| {
                    let glyph = if r.kind == "kubernetes" { "⎈" } else { "⇅" };
                    format!("{glyph} {label} — {}", r.detail)
                })
                .unwrap_or_else(|| format!("⇅ {label}"));
            let connecting = self
                .remote_attach_inflight
                .contains(&(u64::MAX - active_id.0));
            let state = if connecting {
                "Connecting…".to_string()
            } else if let Some(reason) = &window.remote_reconnect_error {
                format!("Disconnected — {reason}")
            } else {
                "Not connected".to_string()
            };
            let (hint, retry) = if connecting || window.remote_reconnect_error.is_none() {
                (
                    "The workspace will open as soon as the connection is established.",
                    "",
                )
            } else {
                (
                    "The workspace could not be loaded without its connection.",
                    "Select it again in the dock (or use the status-bar indicator) to reconnect.",
                )
            };
            return Some(Placeholder {
                detail,
                state,
                hint: hint.to_string(),
                retry: retry.to_string(),
                pane,
            });
        }
        let prep = self.preparing_windows.get(&active_id)?;
        let label = if prep.label.is_empty() {
            window.label.clone()
        } else {
            prep.label.clone()
        };
        let (hint, retry) = if prep.failed {
            (
                "This workspace has not been created yet.",
                "Select it again in the dock to retry, or delete it from the row menu.",
            )
        } else {
            (
                "The workspace will open as soon as it has been created.",
                "",
            )
        };
        Some(Placeholder {
            detail: format!("⛭ {label}"),
            state: prep.message.clone(),
            hint: hint.to_string(),
            retry: retry.to_string(),
            pane,
        })
    }

    fn settle_modal_viewports(&mut self) {
        // The settings dialog is the tree's, box and contents alike
        // (`view::shell::settings`): its ring, its caption, the divider
        // between its columns and the dim over everything behind it are the
        // layer's own. What is left here is the one thing the description
        // cannot say for itself: how many rows a `PgUp` moves the category
        // cursor by.
        // The page a `PgUp` moves the category cursor by: the tree's own
        // height, read from the box the tree placed. It was
        // `categories_scroll.set_viewport(area.height)`, filed by the painter
        // as it drew the rows — so the page and the window it pages through
        // came from two statements of the same rectangle.
        if let (Some(r), Some(s)) = (
            self.panel_rect(&crate::view::shell::settings::categories_key()),
            self.settings_state.as_mut(),
        ) {
            s.categories_scroll.scroll.viewport = r.height;
        }
        // The calibration wizard is the tree's — box, bands, key list and all.
        // It was `apply_dimming` over the frame and four `Paragraph`s into
        // three rectangles it split by hand; it is `Scrim::Dim` and a column
        // now (`view::shell::calibration`). Nothing paints here.

        // Event-debug: the web renders it natively from `aux_modals_view`; paint
        // cells only for the TUI.
        let draw_aux = !self.suppress_chrome_cells;

        // The keybinding editor is the tree's — box, chrome, table and dialogs
        // (`view::shell::keybinding`). What is left here is the one thing the
        // description cannot say for itself: how many rows a `PgUp` moves by,
        // which is the box's height less the bands around the rows.
        if draw_aux {
            // **The box is the tree's.** `view::shell::keybinding` places it —
            // ninety percent of the chrome area, capped, floored, centred
            // beside the dock — and this reads the answer. The four lines of
            // arithmetic that computed it here and then filed it in a
            // `KeybindingEditorLayout` for a mouse handler to compare
            // against were the same rectangle stated twice.
            let modal_area = self.panel_rect(&crate::view::shell::keybinding::key());
            // The page a `PgUp` moves by. It was the table rectangle's height,
            // filed by the painter as it drew; the box is the tree's and the
            // bands between it and the rows are one statement in
            // `keybinding::table_rows`, so the page and the window the rows
            // fill cannot disagree.
            if let (Some(r), Some(e)) = (modal_area, self.keybinding_editor.as_mut()) {
                e.scroll
                    .set_viewport(crate::view::shell::keybinding::table_rows(r.height));
            }
        }

        // The event-debug dialog is the tree's, box and contents alike
        // (`view::shell::event_debug`) — the calibration wizard's twin, and
        // migrated with it for the same reason: no mouse and no recorded
        // rectangles. Nothing paints here.
    }

    /// Apply the theme-key provenance the frame's menu walk recorded.
    ///
    /// This was `render_menu_bar`, then a second walk that existed only to
    /// produce these runs. Nothing about the menu paints from here — the bar
    /// row is a native region in the shell's background band and its dropdowns
    /// are `Layer`s in the overlay band — and nothing is re-derived either:
    /// `shell_frame` did the walk at the top of this method and the runs came
    /// out of it. The expanded-submenu cache is refreshed before that walk, in
    /// `refresh_menu_content`.
    /// Drain plugin commands enqueued before this frame's layout pass.
    ///
    /// Must run before `compute_dock_split` because commands such as
    /// `UnmountFloatingWidget` affect the dock state that layout reads.
    /// Draining any later would compute the dock area from stale state and
    /// leave the freed columns blank until the next input event — which is why
    /// this is the render path's only dispatch point.
    fn drain_pre_layout_plugin_commands(&mut self) {
        #[cfg(feature = "plugins")]
        {
            // The one and only dispatch point on the render path, and it runs
            // before anything is laid out or painted — semantically between
            // frames, so no command can tear this frame or invalidate its
            // layout (the old mid-render drain needed a layout recompute and
            // a hold-back list for window switches; this needs neither).
            // Routed through the same budgeted, backlogged, measured drain as
            // the tick so global FIFO order is preserved.
            let processed = self.process_plugin_commands();
            // ...and re-synced the same way afterwards. A command such as
            // `SetViewMode` changes state that plugins read back through
            // `state_snapshot`, and this frame goes on to fire hooks off the
            // post-command state; leaving the snapshot describing the
            // pre-command editor makes every plugin that consults it decide
            // against a state this frame has already left behind. The tick's
            // drain has always done this (`process_async_messages`); the drain
            // here is the same drain, so it owes the same refresh.
            if processed {
                let _s = tracing::info_span!("update_plugin_state_snapshot_post").entered();
                self.update_plugin_state_snapshot();
            }
        }
    }

    /// Ensure the active split's cursor is in view, then synchronise scroll-sync groups.
    ///
    /// Order matters: `sync_scroll_groups` reads the `viewport.top_byte()` that
    /// `pre_sync_ensure_visible` just updated.  Doing it after the render would
    /// produce a one-frame lag on cursor moves that trigger a scroll-sync anchor
    /// change (e.g. `G` in a side-by-side diff).
    fn pre_sync_and_scroll_sync(&mut self) {
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        {
            let _span = tracing::info_span!("pre_sync_ensure_visible").entered();
            self.active_window_mut()
                .pre_sync_ensure_visible(active_split);
        }
        {
            let _span = tracing::info_span!("sync_scroll_groups").entered();
            self.active_window_mut().sync_scroll_groups();
        }
    }

    /// Compute the visible byte range for each split and issue debounced LSP
    /// requests for semantic tokens and folding ranges.
    fn request_semantic_ranges_for_visible_splits(&mut self) {
        let mut semantic_ranges: std::collections::HashMap<BufferId, (usize, usize)> =
            std::collections::HashMap::new();
        {
            let _span = tracing::info_span!("compute_semantic_ranges").entered();
            for (split_id, view_state) in self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
            {
                if let Some(buffer_id) = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .get_buffer_id((*split_id).into())
                {
                    if let Some(state) = self
                        .windows
                        .get(&self.active_window)
                        .map(|w| &w.buffers)
                        .expect("active window present")
                        .get(&buffer_id)
                    {
                        let start_line =
                            state.buffer.get_line_number(view_state.viewport.top_byte());
                        let visible_lines =
                            view_state.viewport.visible_line_count().saturating_sub(1);
                        let end_line = start_line.saturating_add(visible_lines);
                        semantic_ranges
                            .entry(buffer_id)
                            .and_modify(|(min_start, max_end)| {
                                *min_start = (*min_start).min(start_line);
                                *max_end = (*max_end).max(end_line);
                            })
                            .or_insert((start_line, end_line));
                    }
                }
            }
        }
        for (buffer_id, (start_line, end_line)) in semantic_ranges {
            self.maybe_request_semantic_tokens_range(buffer_id, start_line, end_line);
            self.maybe_request_semantic_tokens_full_debounced(buffer_id);
            self.maybe_request_folding_ranges_debounced(buffer_id);
        }
    }

    /// Pre-load viewport data for each visible buffer.
    ///
    /// Large files use lazy loading: data outside the viewport isn't in memory.
    /// This pass materialises the bytes each split needs before the renderer
    /// touches them, so the render sees a fully-populated buffer.
    fn prepare_visible_buffers_for_render(&mut self) {
        let _span = tracing::info_span!("prepare_for_render").entered();
        // Pre-collect targets so we can take a mut borrow on buffers below
        // without holding the immutable read borrow on self.windows.
        let active_id = self.active_window;
        let prep_targets: Vec<(BufferId, usize, u16)> = {
            let win = self
                .windows
                .get(&active_id)
                .expect("active window must exist");
            let (mgr, vs_map) = win
                .buffers
                .splits()
                .expect("active window must have a populated split layout");
            vs_map
                .iter()
                .filter_map(|(split_id, vs)| {
                    mgr.get_buffer_id((*split_id).into())
                        .map(|bid| (bid, vs.viewport.top_byte(), vs.viewport.height))
                })
                .collect()
        };
        let win_buffers = &mut self
            .windows
            .get_mut(&active_id)
            .expect("active window must exist")
            .buffers;
        for (buffer_id, top_byte, height) in prep_targets {
            if let Some(state) = win_buffers.get_mut(&buffer_id) {
                if let Err(e) = state.prepare_for_render(top_byte, height) {
                    tracing::error!("Failed to prepare buffer for render: {}", e);
                }
            }
        }
    }

    /// Compare the hardware cursor's screen position to the previous frame's
    /// and, if it moved by more than the "jump" threshold, start a
    /// `CursorJump` animation from the old to the new on-screen position.
    /// Successive jumps cancel the prior animation so trail effects don't
    /// pile up.
    ///
    /// Cross-split and cross-buffer transitions (focus change, tab switch)
    /// are also animated — the trail crosses pane separators on its way
    /// from one buffer's cursor cell to another's.
    ///
    /// The threshold is intentionally generous: arrow-key/typing moves
    /// (small `dx`/`dy`) must NOT trigger the animation, but search jumps,
    /// goto-line/definition, and pane switches (which always cross several
    /// rows or many columns) must.
    fn maybe_start_cursor_jump_animation(
        &mut self,
        current_pos: Option<(u16, u16)>,
        active_split: crate::model::event::LeafId,
    ) {
        // Honour the global animations toggle. Tests default to
        // `animations = false` so single-tick `render()` calls observe the
        // settled buffer instead of a mid-flight trail; users can also
        // disable animations entirely from config. The dedicated
        // `cursor_jump_animation` toggle suppresses just the cursor-jump
        // trail while leaving ambient animations (tab slides, dashboard,
        // plugin effects) running.
        if !self.config.editor.animations || !self.config.editor.cursor_jump_animation {
            self.previous_cursor_screen_pos = current_pos.map(|p| (p, active_split));
            return;
        }

        let Some(current) = current_pos else {
            // Cursor is hidden this frame (e.g. prompt has focus). Reset the
            // tracker so the re-emerging cursor doesn't animate from a stale
            // spot when focus returns to a buffer.
            self.previous_cursor_screen_pos = None;
            return;
        };

        let prev_entry = self.previous_cursor_screen_pos;
        // Update tracking unconditionally for the next frame.
        self.previous_cursor_screen_pos = Some((current, active_split));

        let Some((prev, prev_split)) = prev_entry else {
            return;
        };
        if prev == current && prev_split == active_split {
            return;
        }

        let dx = (current.0 as i32 - prev.0 as i32).abs();
        let dy = (current.1 as i32 - prev.1 as i32).abs();
        // Animate when the cursor crossed split panes, or when it made a
        // non-incremental move within the same pane: more than two rows
        // vertically, or — for moves that stay within ±2 rows — at
        // least 80 columns horizontally. The horizontal threshold is
        // generous because typing, arrow keys, word-jump, and Home/End
        // on long source lines can all exceed a smaller bound without
        // being a genuine "jump".
        let crossed_panes = prev_split != active_split;
        let row_jump = dy > 2;
        let col_jump = dx >= 80;
        if !crossed_panes && !row_jump && !col_jump {
            return;
        }

        // Cancel any prior cursor-jump animation so trails don't stack.
        if let Some(prev_anim) = self.cursor_jump_animation.take() {
            self.active_window_mut().animations.cancel(prev_anim);
        }

        let cursor_color = self.theme.read().unwrap().cursor;
        let bg_color = self.theme.read().unwrap().editor_bg;
        let id = self.active_window_mut().animations.start(
            // The bounding box is for runner bookkeeping only — CursorJump
            // paints at absolute screen coords and ignores `area`.
            ratatui::layout::Rect {
                x: prev.0.min(current.0),
                y: prev.1.min(current.1),
                width: dx as u16 + 1,
                height: dy as u16 + 1,
            },
            crate::view::animation::AnimationKind::CursorJump {
                from: prev,
                to: current,
                duration: std::time::Duration::from_millis(140),
                cursor_color,
                bg_color,
            },
        );
        self.cursor_jump_animation = Some(id);
    }

    /// Whether the active pane shows no caret this frame: another surface
    /// owns the keyboard — the file explorer, the dock, a floating panel, a
    /// prompt, a live terminal's own cursor, a dormant remote's placeholder —
    /// or a modal layer is up. The content pass settles a caret on the pane's
    /// leaf only when this is false, so the leaf places none.
    ///
    /// What covers a caret is not this question: a layer painted over the
    /// cell ends the cursor in the display list (the library's rule), so no
    /// list of overlays is kept here.
    fn hide_cursor(&self) -> bool {
        self.shell_ui.as_ref().is_some_and(|ui| ui.modal_up())
            || self.active_window().key_context == KeyContext::FileExplorer
            || self.active_window().focused_terminal_live()
            || self.dock.as_ref().is_some_and(|d| d.focused)
            || self
                .floating_widget_panel
                .as_ref()
                .is_some_and(|f| f.focused)
            || self.is_prompting()
            || self.dormant_remote.contains_key(&self.active_window)
    }

    /// Apply dimming effect to UI elements outside the focused terminal area
    /// This visually indicates that keyboard capture mode is active
    fn apply_keyboard_capture_dimming(
        &self,
        frame: &mut Frame,
        terminal_area: ratatui::layout::Rect,
    ) {
        let size = frame.area();
        crate::view::dimming::apply_dimming_excluding(frame, size, Some(terminal_area));
    }

    /// Settle the open prompt's suggestion list: the selection's window, and
    /// the rectangles the not-yet-migrated readers ask for, read off the tree
    /// that placed the list. Nothing is painted here — the list, the bottom
    /// popup and the overlay card are the tree's.
    fn settle_prompt_suggestions(&mut self) {
        let Some(prompt) = &self.active_window_mut().prompt else {
            return;
        };

        // Overlay prompts (Live Grep, issue #1796) get a dedicated
        // centred floating frame instead of the bottom-anchored popup.
        // Centre it in the chrome area (right of a left dock) so it never
        // overlaps the dock column.
        if prompt.overlay {
            // The card is the tree's; what is left here is the selection's
            // window. How many rows the list can show is the results band's
            // height, read off the card rather than counted — so the
            // selection scrolls only when it genuinely passes the bottom, not
            // when it crosses the bottom-popup default cap.
            let visible = crate::view::shell::overlay_prompt::regions_of(
                self.shell_ui.as_ref().expect("the shell tree is in place"),
            )
            .iter()
            .find(|(k, _)| *k == crate::view::shell::overlay_prompt::CardRegion::Results)
            .map(|(_, r)| r.height as usize)
            .unwrap_or(0);
            if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
                // Skip when the user has wheel-scrolled the list — keeping
                // the selection pinned in view would undo their scroll
                // (issue #2119).
                if !prompt.manual_scroll {
                    prompt.ensure_selected_visible_within(visible);
                }
            }
            self.record_suggestions_geometry();
            return;
        }

        if prompt.suggestions.is_empty() {
            return;
        }

        // Nothing is painted here any more. The layer drew the popup, the
        // hints row and the scrollbar in the overlay band before this method
        // ran, and everything below is the geometry the not-yet-migrated
        // rails still ask `ChromeLayout` for — read off the tree that placed
        // it rather than computed a second time.
        //
        // Gone with the painter: the `Clear` that blanked the cells under the
        // box (a themed box fills its own ground), the `y` arithmetic that had
        // to agree with a second copy in `chrome::Prompt::collect`, and the
        // quick-open hints row, which is now the layer's own last row.
        self.record_suggestions_geometry();
    }

    /// Copy the suggestion list's rectangles out of the shell tree.
    ///
    /// A bridge, and it is meant to read like one. The click and hover walks
    /// and the scrollbar drag are gestures in the tree now, and took the
    /// scrollbar rect with them. What is left reads coordinates for reasons
    /// that are not input routing: the web `Scene`, which draws from rects;
    /// `cursor_obscured_by_overlay`, which asks whether the terminal caret is
    /// under the box; and the column widths the next description is measured
    /// against. Each of those is a separate migration. Until then they read
    /// one answer, produced once, by the layout that actually placed the box —
    /// which is already better than the painter's return value, because there
    /// is no longer a second derivation to disagree with.
    fn record_suggestions_geometry(&mut self) {
        use crate::view::shell::prompt as p;
        let read = self.shell_ui.as_ref().map(|ui| {
            let spec = ui.spec();
            (
                p::suggestions_rect(spec),
                p::suggestions_list_rect(spec),
                p::suggestions_window(spec),
            )
        });
        let Some((outer, list, window)) = read else {
            return;
        };
        let total = self
            .active_window()
            .prompt
            .as_ref()
            .map(|p| p.suggestions.len())
            .unwrap_or(0);
        let to_rect = |r: fresh_ui::Rect| ratatui::layout::Rect {
            x: r.x.max(0) as u16,
            y: r.y.max(0) as u16,
            width: r.w,
            height: r.h,
        };
        let chrome = self.active_chrome_mut();
        chrome.suggestions_outer_area = outer.map(to_rect);
        chrome.suggestions_area = list.map(|r| {
            let (first, visible) = window.unwrap_or((0, r.h as usize));
            (to_rect(r), first, visible.max(r.h as usize), total)
        });
    }

    /// Resolve the overlay's currently-selected match into a real
    /// `Buffer` parked in a phantom `LeafId`, so the preview pane can
    /// reuse the regular per-leaf renderer (with syntax highlighting,
    /// gutter, scrollbars, folding). No-op when the prompt has no
    /// selection or its label is not a `path:line[:col]` triple.
    fn prepare_overlay_preview(&mut self) {
        use crate::input::quick_open::parse_path_line_col;

        let parsed = {
            self.active_window()
                .prompt
                .as_ref()
                .and_then(|prompt| {
                    let idx = prompt.selected_suggestion?;
                    prompt.suggestions.get(idx)
                })
                .map(|s| {
                    // `value` is the authoritative `path:line:col` for the
                    // result. We must not rely on parsing the user-facing
                    // label (`text`), which may carry source badges (e.g.
                    // "[term]") that make it unparseable as a path. Only fall
                    // back to the label when `value` is absent/unparseable.
                    if let Some(v) = s.value.as_deref() {
                        let from_value = parse_path_line_col(v);
                        if !from_value.0.is_empty() && from_value.1.is_some() {
                            return from_value;
                        }
                    }
                    parse_path_line_col(&s.text)
                })
        };
        // No selectable result (empty list, no selection, or an
        // unparseable entry): blank the preview so the previous match's
        // content doesn't linger after the result list clears.
        let (path_str, line, col) = match parsed {
            Some((path, line, col)) if !path.is_empty() => (path, line, col),
            _ => {
                self.blank_overlay_preview();
                return;
            }
        };
        let line = line.unwrap_or(1).saturating_sub(1);
        let col = col.unwrap_or(1).saturating_sub(1);

        // Resolve relative to the working directory.
        let path_buf = std::path::PathBuf::from(&path_str);
        let abs_path = if path_buf.is_absolute() {
            path_buf
        } else {
            self.working_dir().join(&path_buf)
        };
        // Canonicalize for buffer-dedup parity with open_file_no_focus.
        let abs_path = self
            .authority()
            .filesystem
            .canonicalize(&abs_path)
            .unwrap_or(abs_path);

        // If the standalone state already targets this path, just
        // re-seed the cursor and skip the file-load roundtrip.
        let already_target = self
            .active_window()
            .overlay_preview_state
            .as_ref()
            .is_some_and(|st| {
                self.windows
                    .get(&self.active_window)
                    .map(|w| &w.buffers)
                    .expect("active window present")
                    .get(&st.buffer_id)
                    .and_then(|s| s.buffer.file_path())
                    .is_some_and(|p| p == abs_path.as_path())
            });

        let buffer_id = if already_target {
            self.active_window_mut()
                .overlay_preview_state
                .as_ref()
                .unwrap()
                .buffer_id
        } else {
            // Snapshot whether this path was already known so we can
            // tell "I just loaded it for preview" from "the user had
            // it open" — only the former gets cleaned up on close.
            let was_open = self
                .buffers()
                .iter()
                .any(|(_, s)| s.buffer.file_path() == Some(abs_path.as_path()));
            // Capture the active split so we can undo the side
            // effects of `open_file_no_focus` (it adds the buffer to
            // the active split's tabs and may switch its active
            // buffer to the loaded file).
            let source_split = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .active_split();
            // `open_file_for_preview` always allocates a fresh buffer
            // — never repurposes the "no name" empty buffer the user
            // is currently looking at — so the background view stays
            // intact while we cycle through preview results.
            let buffer_id = match self.open_file_for_preview(abs_path.as_path()) {
                Ok(id) => id,
                Err(_e) => return,
            };
            if !was_open {
                if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&buffer_id) {
                    meta.hidden_from_tabs = true;
                }
                // Drop the buffer from every split's `open_buffers`
                // list so it doesn't surface as a tab anywhere. The
                // phantom buffer is rendered exclusively via the
                // overlay's standalone view-state — it doesn't need
                // to be in `open_buffers`.
                let leaf_ids: Vec<_> = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .keys()
                    .copied()
                    .collect();
                for leaf_id in leaf_ids {
                    if let Some(view_state) = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .get_mut(&leaf_id)
                    {
                        view_state.remove_buffer(buffer_id);
                    }
                }
                // open_file_no_focus may have switched the active
                // buffer of the source split. Restore it.
                let preview_loaded: std::collections::HashSet<BufferId> = self
                    .active_window_mut()
                    .overlay_preview_state
                    .as_ref()
                    .map(|st| st.loaded_buffers.clone())
                    .unwrap_or_default();
                let __active_id = self.active_window;
                let __win = self
                    .windows
                    .get_mut(&__active_id)
                    .expect("active window must exist");
                let __buffer_keys: Vec<BufferId> = __win.buffers.ids();
                let (__mgr, __vs_map) = __win
                    .buffers
                    .splits_mut()
                    .expect("active window must have a populated split layout");
                if let Some(source_state) = __vs_map.get_mut(&source_split) {
                    if source_state.active_buffer == buffer_id {
                        let fallback = source_state
                            .open_buffers
                            .iter()
                            .find_map(|t| t.as_buffer())
                            .or_else(|| {
                                __buffer_keys
                                    .iter()
                                    .copied()
                                    .find(|b| *b != buffer_id && !preview_loaded.contains(b))
                            });
                        if let Some(fb) = fallback {
                            source_state.switch_buffer(fb);
                            __mgr.set_split_buffer(source_split, fb);
                        }
                    }
                }
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .set_active_split(source_split);
            }
            buffer_id
        };

        // The buffer (if any) the preview pointed at on the previous
        // frame. When the selection moves to a result in a *different*
        // file we must drop our search-match overlays from the old
        // buffer (see the highlight refresh below).
        let prev_preview_buffer = self
            .active_window()
            .overlay_preview_state
            .as_ref()
            .map(|s| s.buffer_id);

        // Build (or update) the standalone preview state. Held off
        // `split_view_states` so cross-cutting iteration never touches
        // it.
        let need_init = self.active_window_mut().overlay_preview_state.is_none();
        if need_init {
            let mut view_state = crate::view::split::SplitViewState::with_buffer(
                self.terminal_width,
                self.terminal_height,
                buffer_id,
            );
            view_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                line_numbers: self.config.editor.line_numbers,
                highlight_current_line: self.config.editor.highlight_current_line,
                line_wrap: self.active_window().resolve_line_wrap_for_buffer(buffer_id),
                wrap_indent: self.config.editor.wrap_indent,
                wrap_column: self
                    .active_window()
                    .resolve_wrap_column_for_buffer(buffer_id),
                rulers: self.config.editor.rulers.clone(),
                scroll_offset: self.config.editor.scroll_offset,
            });
            let mut loaded_buffers = std::collections::HashSet::new();
            // Whether this *first* preview buffer was newly loaded.
            // The pre-existing case skips the `was_open` branch so
            // we re-derive it from buffer_metadata: a buffer with
            // hidden_from_tabs=true that we just touched is one we
            // owned. Simpler: track via the existing-target check:
            // if `already_target` was false above, the buffer was
            // either pre-open (we left meta alone) or freshly
            // loaded (we set hidden_from_tabs=true). Re-check.
            if let Some(meta) = self.active_window().buffer_metadata.get(&buffer_id) {
                if meta.hidden_from_tabs {
                    loaded_buffers.insert(buffer_id);
                }
            }
            self.active_window_mut().overlay_preview_state =
                Some(crate::app::types::OverlayPreviewState {
                    buffer_id,
                    view_state,
                    loaded_buffers,
                    blanked: false,
                    centered_byte: None,
                });
        } else {
            // Pre-compute hidden flag (immutable borrow on self.windows)
            // before taking the mutable borrow on overlay_preview_state.
            let hidden_from_tabs = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffer_metadata.get(&buffer_id))
                .is_some_and(|meta| meta.hidden_from_tabs);
            if let Some(state) = self.active_window_mut().overlay_preview_state.as_mut() {
                if state.buffer_id != buffer_id {
                    state.view_state.switch_buffer(buffer_id);
                    // Keep the struct's `buffer_id` in lockstep with the
                    // view-state's active buffer: the renderer looks up the
                    // buffer to draw via this field, so a stale value here
                    // renders the *previous* file's text at the new file's
                    // scroll offset (wrong content, or blank past EOF).
                    state.buffer_id = buffer_id;
                    // New file in the preview ⇒ force a recenter below.
                    state.centered_byte = None;
                    if hidden_from_tabs {
                        state.loaded_buffers.insert(buffer_id);
                    }
                }
            }
        }

        // Set the cursor to the match position and centre it vertically.
        let byte_offset = self
            .buffers()
            .get(&buffer_id)
            .map(|s| {
                s.buffer
                    .position_to_offset(crate::model::piece_tree::Position { line, column: col })
            })
            .unwrap_or(0);

        // The overlay preview is used exclusively by the Live Grep
        // floating overlay, so the prompt input IS the search query.
        // Highlight every occurrence in the visible region — previously
        // the match was only reachable via the (hidden) cursor, which is
        // near-invisible against the preview chrome. Capture the query and
        // theme colours before the window borrow below.
        let query = self
            .active_window()
            .prompt
            .as_ref()
            .map(|p| p.input_str().to_string())
            .unwrap_or_default();
        let (search_fg, search_bg) = {
            let theme = self.theme.read().unwrap();
            (theme.search_match_fg, theme.search_match_bg)
        };
        // Live Grep defaults to regex with smart-case (case-insensitive
        // unless the query carries an uppercase letter) — mirror that so
        // the highlight tracks what the search actually matched. A query
        // that isn't valid regex falls back to a literal match.
        let preview_regex = if query.is_empty() {
            None
        } else {
            let case_insensitive = !query.chars().any(|c| c.is_uppercase());
            regex::RegexBuilder::new(&query)
                .case_insensitive(case_insensitive)
                .build()
                .or_else(|_| {
                    regex::RegexBuilder::new(&regex::escape(&query))
                        .case_insensitive(case_insensitive)
                        .build()
                })
                .ok()
        };
        let preview_ns = crate::view::overlay::OverlayNamespace::from_string(
            "overlay-preview-search".to_string(),
        );

        let active_id = self.active_window;
        if let Some(win) = self.windows.get_mut(&active_id) {
            // `buffers` and `overlay_preview_state` are distinct fields, so
            // these mutable borrows are disjoint.
            let preview_buffer = win.buffers.get_mut(&buffer_id);
            let preview_state = win.overlay_preview_state.as_mut();
            if let (Some(state), Some(pstate)) = (preview_buffer, preview_state) {
                pstate.view_state.cursors.primary_mut().position = byte_offset;
                // Force line wrapping on for the preview regardless of the
                // global `editor.line_wrap` setting (and of a switched-in
                // buffer's fresh default): the preview pane has no
                // horizontal scroll affordance, so without wrapping a match
                // deep in a long line scrolls off-screen. Wrapping moots
                // horizontal scroll, so reset it to the left edge.
                // `view_state` derefs to the active buffer's
                // `BufferViewState`, so this targets the rendered buffer.
                pstate.view_state.viewport.line_wrap_enabled = true;
                // Recentre only when the selected match changed (issue
                // #2119) so a mouse-wheel scroll of the preview is
                // preserved; `center_on_position` counts real visual rows so
                // a match deep in a wrapped doc still lands mid-pane.
                if pstate.centered_byte != Some(byte_offset) {
                    pstate.view_state.viewport.left_column = 0;
                    pstate.view_state.viewport.horizontal_scroll_offset = 0;
                    pstate
                        .view_state
                        .viewport
                        .center_on_position(&mut state.buffer, byte_offset);
                    pstate.centered_byte = Some(byte_offset);
                }
                // We have a live target: ensure the pane is shown.
                pstate.blanked = false;

                // Rebuild the search-match overlays for the now-visible
                // region. Cleared + re-added every frame (cheap; bounded
                // to the viewport) so they track scrolling and edits, the
                // same contract `Window::update_search_highlights` uses.
                state
                    .overlays
                    .clear_namespace(&preview_ns, &mut state.marker_list);
                if let Some(re) = &preview_regex {
                    let visible_start = pstate.view_state.viewport.top_byte();
                    let visible_rows = pstate.view_state.viewport.height as usize;
                    let mut visible_end = visible_start;
                    {
                        let mut iter = state.buffer.line_iterator(visible_start, 80);
                        for _ in 0..visible_rows {
                            if let Some((line_start, line_content)) = iter.next_line() {
                                visible_end = line_start + line_content.len();
                            } else {
                                break;
                            }
                        }
                    }
                    visible_end = visible_end.min(state.buffer.len());
                    let visible_text = state.get_text_range(visible_start, visible_end);
                    for mat in re.find_iter(&visible_text) {
                        if mat.start() == mat.end() {
                            continue;
                        }
                        let absolute_pos = visible_start + mat.start();
                        let match_len = mat.end() - mat.start();
                        let style = ratatui::style::Style::default().fg(search_fg).bg(search_bg);
                        let overlay = crate::view::overlay::Overlay::with_namespace(
                            &mut state.marker_list,
                            absolute_pos..(absolute_pos + match_len),
                            crate::view::overlay::OverlayFace::Style { style },
                            preview_ns.clone(),
                        )
                        .with_priority_value(10);
                        state.overlays.add(overlay);
                    }
                }
            }

            // The selection jumped to a result in a different file: scrub
            // our overlays from the previously-previewed buffer. Matters
            // only for buffers the user already had open — preview-loaded
            // buffers are closed wholesale on overlay teardown.
            if let Some(prev) = prev_preview_buffer {
                if prev != buffer_id {
                    if let Some(prev_state) = win.buffers.get_mut(&prev) {
                        prev_state
                            .overlays
                            .clear_namespace(&preview_ns, &mut prev_state.marker_list);
                    }
                }
            }
        }
    }

    /// Blank the Live Grep preview pane: it renders just its frame until
    /// the next selectable result. Keeps `overlay_preview_state` (and its
    /// `loaded_buffers` cleanup tracking) intact.
    fn blank_overlay_preview(&mut self) {
        if let Some(state) = self.active_window_mut().overlay_preview_state.as_mut() {
            state.blanked = true;
        }
    }

    /// Paint the overlay prompt card's preview pane into `inner`: a real
    /// buffer rendered by the same per-leaf pipeline regular splits use, or
    /// another session's whole window when a plugin asked for one. The pane
    /// is the card's one host band (`HostTarget::Card(CardRegion::Preview)`);
    /// the fold hands it the rectangle layout gave the band. Buffer and
    /// cursor are seeded by `prepare_overlay_preview`, earlier in the frame.
    pub(crate) fn paint_card_preview(
        &mut self,
        buf: &mut ratatui::buffer::Buffer,
        inner: ratatui::layout::Rect,
    ) {
        if inner.height == 0 || inner.width == 0 {
            return;
        }
        let theme = self.theme.read().unwrap().clone();
        if let Some(sid) = self
            .preview_window_id
            .filter(|sid| *sid != self.active_window && self.windows.contains_key(sid))
        {
            // Another window's whole grid, as an embed paints it.
            crate::app::shell_host::paint_embed(self, sid, inner, buf);
            return;
        }
        // Snapshot the per-split scalars and group the appearance inputs
        // into one `RenderStyle`, all before the `&mut self.windows` borrow
        // below — they touch only `self.config`/`self.theme`/
        // `self.ansi_background`, which Rust splits from `self.windows` as
        // distinct fields.
        let session_mode = self.session_mode || !self.software_cursor_only;
        let show_tilde = false; // preview hides tilde markers
        let highlight_current_column = self.config.editor.highlight_current_column;
        let screen_width = buf.area.width;
        let style = crate::view::ui::RenderStyle {
            theme: &theme,
            ansi_background: self.ansi_background.as_ref(),
            cfg: crate::view::ui::EditorRenderConfig::new(
                &self.config.editor,
                self.background_fade,
                self.software_cursor_only,
            ),
        };
        let __win = self
            .windows
            .get_mut(&self.active_window)
            .expect("active window present");
        let buffers = &mut __win.buffers;
        let event_logs = &mut __win.event_logs;
        let cell_theme_map = &mut __win.chrome_layout.cell_theme_map;
        let Some(preview_state) = __win.overlay_preview_state.as_mut() else {
            return;
        };
        // Blanked: the current query has no selectable result, so leave the
        // pane empty rather than rendering a stale match.
        if preview_state.blanked {
            return;
        }
        preview_state
            .view_state
            .viewport
            .resize(inner.width, inner.height);
        let buffer_id = preview_state.buffer_id;
        let Some(state) = buffers.get_mut(&buffer_id) else {
            return;
        };
        // Deref the SplitViewState once to a concrete `&mut BufferViewState`
        // so disjoint field splits (`viewport` + `folds`) are visible to the
        // borrow checker.
        let buf_state = preview_state.view_state.active_state_mut();
        let cursors = buf_state.cursors.clone();
        let view_mode = buf_state.view_mode.clone();
        let compose_width = buf_state.compose_width;
        let compose_column_guides = buf_state.compose_column_guides.clone();
        let rulers = buf_state.rulers.clone();
        let show_line_numbers = buf_state.show_line_numbers;
        let highlight_current_line = buf_state.highlight_current_line;
        let viewport_ref = &mut buf_state.viewport;
        let folds_ref = &mut buf_state.folds;
        let event_log = event_logs.get_mut(&buffer_id);
        let _ = crate::view::ui::SplitRenderer::render_phantom_leaf(
            buf,
            state,
            &cursors,
            viewport_ref,
            folds_ref,
            event_log,
            inner,
            style,
            view_mode,
            compose_width,
            compose_column_guides,
            buffer_id,
            session_mode,
            &rulers,
            show_line_numbers,
            highlight_current_line,
            show_tilde,
            highlight_current_column,
            cell_theme_map,
            screen_width,
        );
    }

    /// The live-grep card's caption when the plugin set no title: shortcut
    /// hints only, since the input row's prefix already names the feature.
    /// Mirrors `updateOverlayTitle` in live_grep.ts, so a Resume-replay
    /// overlay and a freshly-opened plugin overlay look identical;
    /// `resume_live_grep` is left out because it matters once the overlay is
    /// closed.
    fn default_overlay_title(&self) -> Vec<fresh_core::api::StyledText> {
        use crate::input::keybindings::KeyContext;
        use fresh_core::api::{OverlayColorSpec, OverlayOptions, StyledText};
        let keybindings = self.keybindings.read().unwrap();
        let mut hints: Vec<(String, &str)> = Vec::new();
        if let Some(k) =
            keybindings.find_keybinding_for_action("cycle_live_grep_provider", KeyContext::Prompt)
        {
            hints.push((k, "switch grep provider"));
        }
        if let Some(k) =
            keybindings.find_keybinding_for_action("live_grep_export_quickfix", KeyContext::Prompt)
        {
            hints.push((k, "save matches"));
        }
        if hints.is_empty() {
            return Vec::new();
        }
        let hint_style = Some(OverlayOptions {
            fg: Some(OverlayColorSpec::ThemeKey("ui.help_key_fg".into())),
            ..OverlayOptions::default()
        });
        let sep_style = Some(OverlayOptions {
            fg: Some(OverlayColorSpec::ThemeKey("ui.popup_border_fg".into())),
            ..OverlayOptions::default()
        });
        let mut segs: Vec<StyledText> = Vec::new();
        for (i, (k, verb)) in hints.into_iter().enumerate() {
            if i > 0 {
                segs.push(StyledText {
                    text: " · ".into(),
                    style: sep_style.clone(),
                });
            }
            segs.push(StyledText {
                text: k,
                style: hint_style.clone(),
            });
            segs.push(StyledText {
                text: format!(" {verb}"),
                style: None,
            });
        }
        segs
    }

    /// Recompute the view_line_mappings layout without drawing.
    /// Used during macro replay so that visual-line movements (MoveLineEnd,
    /// MoveUp, MoveDown on wrapped lines) see correct, up-to-date layout
    /// information between each replayed action.
    pub fn recompute_layout(&mut self, width: u16, height: u16) {
        let size = ratatui::layout::Rect::new(0, 0, width, height);

        // Replicate the pre-render sync steps from render()
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        self.active_window_mut()
            .pre_sync_ensure_visible(active_split);
        self.active_window_mut().sync_scroll_groups();

        // The body's rectangle, from the frame the shell describes — the same
        // description and the same layout `render` uses, laid out at the size
        // the replay is running against.
        //
        // What stood here was a hand-rolled ratatui `Layout` "replicating" it,
        // and a replica of a layout is a layout that disagrees. This one did,
        // three ways: it gave the search-options row a `Length(0)` under a
        // comment saying it "doesn't matter for layout" (it is a row, and it
        // is one cell tall when the row is up), it kept the status bar whether
        // or not a suggestion list or the file browser had taken its row, and
        // it carved the explorer but never the dock — so a replay with a dock
        // open computed visual-line motion against a body twenty-odd columns
        // too wide.
        //
        // **A geometry pass, not a frame.** `layout_only` builds the
        // description and lays it out and stops; a frame goes on to do things
        // — move focus to an autofocused element and tell the application it
        // moved, drain a queued reveal into a viewport's scroll offset, hand a
        // behavior what arrived for it, repaint a display list nobody will
        // draw. Replay asks this question once per replayed action, so a frame
        // here writes the *length of the macro* into editor state that has
        // nothing to do with where the body is. What replay needs is one
        // rectangle.
        //
        // The description still has to be rebuilt and reconciled, and the
        // frame the shell describes is still the only thing that knows the
        // answer: it changes between replayed actions (a suggestion list takes
        // the status row, the search-options row appears, the dock opens), so
        // there is nothing to cache it against short of the whole `Frame` —
        // which is deliberately not comparable, and a hand-picked subset of it
        // is the same "replica of a layout" this call site already got wrong
        // once.
        //
        // The panes come off this same layout — the shape every frame reads
        // them in (`render` does the same after its `frame`). What stood here
        // read the body's region and handed it to a pass that laid the grid
        // out a third time to find the panes inside it.
        let pane_rects = self
            .layout_panes(size)
            .expect("the shell tree is taken and returned within one call");

        // Every text pane reconciled and laid out at the content rect the
        // layout placed it at, and settled on its leaf with its rows and its
        // caret — the frame's own steps, without the frame.
        let pane_chrome = self.pane_chrome();
        let body_state = crate::app::shell_host::BodyState {
            lsp_waiting: false,
            hide_cursor: self.hide_cursor(),
        };
        if let Some(mut prepared) = crate::app::shell_host::reconcile_body(
            self,
            self.active_window,
            body_state,
            &pane_rects,
            size.width,
            &pane_chrome,
        ) {
            crate::app::shell_host::content_body(
                self,
                self.active_window,
                body_state,
                size.width,
                &pane_chrome,
                &mut prepared,
            );
        }
    }

    /// The frame's geometry pass without the frame: build the shell's
    /// description, lay it out at `size` with `layout_only`, and read the
    /// panes off it — the same read `render` does after its `frame`.
    ///
    /// Retains the rects on the active window (see `Window::pane_rects`) and
    /// returns them. `None` when the shell tree is not here to lay out — a
    /// frame, or an event dispatch, is holding it; the caller inside such a
    /// pass has the tree's own answer, and the window keeps the rects of the
    /// last layout, which that pass wrote.
    ///
    /// **A geometry pass, not a frame.** `layout_only` builds the description
    /// and lays it out and stops; a frame goes on to move focus, drain a
    /// reveal, hand a behavior what arrived for it, and repaint a display list
    /// nobody will draw. This runs on actions — once per relayout, once per
    /// replayed macro action — and none of that belongs on an action.
    /// Counted by `geometry::stats` as a shell layout, because it is one.
    pub(crate) fn layout_panes(
        &mut self,
        size: ratatui::layout::Rect,
    ) -> Option<crate::view::shell::geometry::PaneRects> {
        let split = self.compute_dock_split(size);
        let shell = self.shell_frame(split).resolve_dock(size.width);
        let mut ui = self.shell_ui.take()?;
        crate::view::shell::geometry::stats::note_shell_layout();
        ui.layout_only(
            crate::view::shell::frame::frame_tree(shell),
            fresh_ui::Size::new(size.width, size.height),
        );
        let pane_rects = crate::view::shell::geometry::PaneRects::read(
            &ui,
            self.window_panes().into_iter().map(|(leaf, _)| leaf),
            size,
        );
        self.shell_ui = Some(ui);
        self.active_window_mut().set_pane_rects(pane_rects.clone());
        Some(pane_rects)
    }

    /// The shell tree, when no frame or event dispatch is holding it.
    ///
    /// For tests that check a record against the tree that wrote it — the
    /// pane rects a window retains against the frame's own layout. Not for
    /// the editor: what it needs off the tree it reads where it lays the
    /// tree out.
    pub fn shell_ui(&self) -> Option<&fresh_ui::Ui<crate::view::shell::msg::UiMsg>> {
        self.shell_ui.as_ref()
    }

    /// Bring the active window's retained pane rects up to date with the
    /// grid as it is now, at the screen's current size — one `layout_only` of
    /// the frame ([`Self::layout_panes`]).
    ///
    /// For the callers that ask where a pane is right after changing the
    /// grid, before the frame that would lay it out: the layout funnel
    /// (`push_layout_geometry`) runs this first, and so does every
    /// `Editor::resize_visible_terminals`. A no-op while a frame or an event
    /// dispatch holds the tree; see [`Self::layout_panes`].
    pub(crate) fn refresh_pane_rects(&mut self) {
        let size = ratatui::layout::Rect::new(0, 0, self.terminal_width, self.terminal_height);
        let _ = self.layout_panes(size);
    }

    /// Clear the search history
    /// Used primarily for testing to ensure test isolation
    pub fn clear_search_history(&mut self) {
        if let Some(history) = self.active_window_mut().prompt_histories.get_mut("search") {
            history.clear();
        }
    }

    /// Emit an OSC 2 escape sequence to set the host terminal's window/tab
    /// title based on the active buffer's display name and the project name
    /// (the working directory's last path component). Deduplicated against
    /// the last title we wrote so we don't spam stdout every frame.
    ///
    /// Gated by `editor.set_window_title` (default on). Terminals that
    /// don't implement OSC 2 silently drop the sequence.
    fn update_terminal_title(&mut self, display_name: &str) {
        if !self.config.editor.set_window_title {
            return;
        }
        let project_name = self.working_dir().file_name().and_then(|s| s.to_str());
        let new_title =
            crate::services::terminal_title::build_window_title(display_name, project_name);
        if self.last_window_title.as_deref() == Some(new_title.as_str()) {
            return;
        }
        crate::services::terminal_title::write_terminal_title(&new_title);
        self.last_window_title = Some(new_title);
    }

    /// Save all prompt histories to disk
    /// Called on shutdown to persist history across sessions
    pub fn save_histories(&self) {
        // Ensure data directory exists
        if let Err(e) = self
            .authority()
            .filesystem
            .create_dir_all(&self.dir_context.data_dir)
        {
            tracing::warn!("Failed to create data directory: {}", e);
            return;
        }

        // Save all prompt histories
        for (key, history) in &self.active_window().prompt_histories {
            let path = self.dir_context.prompt_history_path(key);
            if let Err(e) = history.save_to_file(&path) {
                tracing::warn!("Failed to save {} history: {}", key, e);
            } else {
                tracing::debug!("Saved {} history to {:?}", key, path);
            }
        }
    }

    /// Resolve a plugin-supplied [`OverlayOptions`] to a ratatui
    /// [`Style`] against the active theme. RGB colours pass through;
    /// theme keys (e.g. `"ui.help_key_fg"`) are looked up via
    /// `theme.resolve_theme_key`. Mirrors the resolution
    /// `OverlayFace::from_options` + char_style.rs do for buffer
    /// overlays — pulled here so the prompt-frame renderer can build
    /// styled spans inline.
    /// Compute a centered overlay rect of `width_pct` × `height_pct`
    /// of the given area. Mirrors `PopupPosition::CenteredOverlay`
    /// math the overlay card is placed by; minimum 20×8 cells so
    /// content stays legible on tiny terminals.
    pub(super) fn centered_overlay_rect(
        area: ratatui::layout::Rect,
        width_pct: u8,
        height_pct: u8,
    ) -> ratatui::layout::Rect {
        let w_pct = width_pct.clamp(1, 100) as u32;
        let h_pct = height_pct.clamp(1, 100) as u32;
        let w = ((area.width as u32 * w_pct) / 100) as u16;
        let h = ((area.height as u32 * h_pct) / 100) as u16;
        let w = w.max(20).min(area.width);
        let h = h.max(8).min(area.height);
        ratatui::layout::Rect {
            x: area.x + (area.width.saturating_sub(w)) / 2,
            y: area.y + (area.height.saturating_sub(h)) / 2,
            width: w,
            height: h,
        }
    }

    /// Render the currently-mounted floating widget panel: dim the
    /// background outside the centered rect, draw the frame, paint
    /// the panel's rendered entries inside, and place the hardware
    /// caret at the focused TextInput. Stores the inner rect on the
    /// `FloatingWidgetState` so the click hit-test can recover the
    /// geometry on the next mouse event.
    /// Split `size` into an optional full-height left dock column and
    /// the remaining chrome area. Returns `(None, size)` unless a
    /// floating panel is currently placed as a `LeftDock`. The dock
    /// width is clamped so it can never crowd out the chrome.
    pub(super) fn compute_dock_split(
        &self,
        size: ratatui::layout::Rect,
    ) -> (Option<ratatui::layout::Rect>, ratatui::layout::Rect) {
        // The editor is the priority: it keeps `EDITOR_MIN` columns, the dock
        // honors its drag-set width below that, and once the terminal is too
        // narrow for a worthwhile dock alongside the editor the dock hides
        // entirely (reappearing when the terminal grows).
        //
        // That rule is `frame::dock_width` — one copy, shared with
        // `Frame::resolve_dock`, which is what the frame-parity test runs.
        // They were separate before, each with its own constants, so the test
        // could pass while this path disagreed with it.
        let requested = match self.dock.as_ref().map(|f| f.placement) {
            Some(super::PanelPlacement::LeftDock { width_cols }) => Some(width_cols),
            _ => None,
        };
        let Some(width) = crate::view::shell::frame::dock_width(requested, size.width) else {
            return (None, size);
        };
        let dock = ratatui::layout::Rect {
            x: size.x,
            y: size.y,
            width,
            height: size.height,
        };
        let chrome = ratatui::layout::Rect {
            x: size.x.saturating_add(width),
            y: size.y,
            width: size.width.saturating_sub(width),
            height: size.height,
        };
        (Some(dock), chrome)
    }
}

/// Building the floating plugin panel's frame from live state.
impl Editor {
    /// The box around the panel: where it goes, its title, its `[×]`.
    ///
    /// The *interior* is not here — it is nineteen `WidgetSpec` variants that
    /// `render_floating_widget_panel` still paints. What this resolves is
    /// everything the painter was deriving twice: the rectangle (once to draw
    /// the `Block`, once to place the close button), and the button's own
    /// rectangle, which it filed for a mouse arm to compare against.
    ///
    /// The dock's slot has no entry here. Its placement is the dock column,
    /// which is already a region, and its frame is one divider rather than a
    /// box — see C.5b.
    /// Where layout put a keyed node of the panel's frame.
    ///
    /// The painter used to compute these — the box's rectangle twice (once to
    /// draw the `Block`, once to place `[×]`) and the content rect by asking
    /// the block for its inner. Layout computes; this reads.
    pub(crate) fn panel_rect(&self, key: &fresh_ui::Key) -> Option<ratatui::layout::Rect> {
        let ui = self.shell_ui.as_ref()?;
        let f = self.active_chrome().last_frame;
        crate::view::shell::rect_of(ui, key, ratatui::layout::Rect::new(0, 0, f.width, f.height))
    }

    /// The mounted panel a pane is showing, if it is showing one.
    ///
    /// **A pane is one buffer, and a buffer names its panel.** That is the
    /// whole of what `Slot::Pane` has to carry: everything else about a
    /// mounted panel — its spec, its state, its hits — is the same as the
    /// dock's, because `widgets::node` never asks where a surface lives.
    pub(crate) fn pane_panel_key(
        &self,
        pane: crate::model::event::LeafId,
    ) -> Option<crate::widgets::PanelKey> {
        let buffer = self.active_window().pane_buffer(pane)?;
        self.widget_registry
            .panels_for_buffer(buffer)
            .into_iter()
            .next()
    }

    /// The described mounted panel each visible pane is showing.
    ///
    /// Every pane of this window, the panes *inside* a buffer group included —
    /// a group's leaves are panes of the same grid, dispatched into their
    /// outer pane's interior, and the review-diff sidebar is exactly one of
    /// them. Empty for a window whose panes all show buffers, which is the
    /// common case and costs one walk of the split tree.
    pub(crate) fn pane_interiors(
        &self,
    ) -> std::collections::HashMap<crate::model::event::LeafId, crate::view::shell::panel::Interior>
    {
        let mut out = std::collections::HashMap::new();
        for (leaf, buffer) in self.window_panes() {
            if let Some(i) = self.pane_panel_interior(buffer) {
                out.insert(leaf, i);
            }
        }
        out
    }

    /// Each pane's tab strip, as content — the active window's
    /// (`Window::pane_strips`), with the frame's hover.
    pub(crate) fn pane_strips(
        &self,
        chrome: &std::collections::HashMap<
            crate::model::event::LeafId,
            crate::view::shell::splits::PaneChrome,
        >,
    ) -> std::collections::HashMap<crate::model::event::LeafId, crate::view::shell::tabs::Strip>
    {
        let hover = match &self.shell_hover {
            Some(crate::app::types::HoverTarget::TabName(t, pane)) => Some((*t, *pane, false)),
            Some(crate::app::types::HoverTarget::TabCloseButton(t, pane)) => {
                Some((*t, *pane, true))
            }
            _ => None,
        };
        self.active_window().pane_strips(chrome, hover)
    }

    /// Each visible pane's leaf handle, for the frame's description — the
    /// active window's (`Window::pane_hosts`), with the panes whose content
    /// is not the buffer's this frame, a described plugin panel's or a
    /// buffer group's grid, named so they keep no rows.
    pub(crate) fn pane_hosts(
        &mut self,
        interiors: &std::collections::HashMap<
            crate::model::event::LeafId,
            crate::view::shell::panel::Interior,
        >,
        groups: &std::collections::HashMap<
            crate::model::event::LeafId,
            crate::view::split::SplitNode,
        >,
    ) -> std::collections::HashMap<
        crate::model::event::LeafId,
        crate::view::shell::buffer_host::PaneHandle,
    > {
        let rowless: std::collections::HashSet<_> =
            interiors.keys().chain(groups.keys()).copied().collect();
        self.active_window_mut().pane_hosts(&rowless)
    }

    /// Each visible pane's keyboard context when it is not the editor's
    /// plain one: the active pane's is `Terminal` while its live terminal
    /// takes the keyboard (the window's mode, which the leaf now states),
    /// and a pane showing a composite buffer resolves in the composite's
    /// section.
    fn pane_contexts(
        &self,
        active: crate::model::event::LeafId,
    ) -> std::collections::HashMap<crate::model::event::LeafId, crate::input::keybindings::KeyContext>
    {
        use crate::input::keybindings::KeyContext;
        let win = self.active_window();
        let terminal = win.key_context == KeyContext::Terminal;
        self.window_panes()
            .into_iter()
            .filter_map(|(pane, buffer)| {
                if pane == active && terminal {
                    Some((pane, KeyContext::Terminal))
                } else if win.is_composite_buffer(buffer) {
                    Some((pane, KeyContext::CompositeBuffer))
                } else {
                    None
                }
            })
            .collect()
    }

    /// The panes the shell tree describes rather than paints.
    ///
    /// The key set [`Self::pane_interiors`] would produce, answered without
    /// building one: this is the painter's question, asked once per frame, and
    /// an `Interior` clones the spec and the whole instance-state map.
    pub(crate) fn described_panes(&self) -> std::collections::HashSet<crate::model::event::LeafId> {
        self.window_panes()
            .into_iter()
            .filter(|(_, buffer)| self.pane_panel_is_described(*buffer))
            .map(|(leaf, _)| leaf)
            .collect()
    }

    /// Every pane of the active window with the buffer it shows — the panes
    /// *inside* a buffer group included.
    ///
    /// A group's leaves are panes of the same grid, dispatched at render time
    /// into their outer pane's interior, and `SplitManager::visible_leaves`
    /// does not walk into them because a group's layout lives in a side map.
    /// Both halves of C.5 need the same list, so it is stated once.
    pub(crate) fn window_panes(&self) -> Vec<(crate::model::event::LeafId, fresh_core::BufferId)> {
        self.active_window().panes_with_buffers()
    }

    /// An embedded window's grid, described as the frame describes the
    /// active window's — for the tree an embed lays out (`shell_host::paint_embed`).
    ///
    /// Without its bars: they are noise in a small box, and the frame's own
    /// chrome is the one that answers. Without the plugin panels the editor
    /// would mount in it, and with no keyboard context: nothing of it is
    /// interactive through the embed. Its panes are leaves on that window's
    /// own handles, kept for as long as the panes exist.
    pub(crate) fn embed_grid(
        &mut self,
        window: fresh_core::WindowId,
    ) -> Option<std::rc::Rc<crate::view::shell::splits::Splits>> {
        use crate::view::shell::splits::{PaneChrome, PaneControls, Splits};
        let win = self.windows.get_mut(&window)?;
        let (root, maximized, active, several, is_maximized) = {
            let (mgr, _) = win.buffers.splits()?;
            (
                mgr.root().clone(),
                mgr.maximized_split().map(crate::model::event::LeafId),
                mgr.active_split(),
                mgr.visible_leaves().len() > 1,
                mgr.is_maximized(),
            )
        };
        let chrome = win.pane_chrome(PaneChrome {
            tabs: win.tab_bar_visible,
            vscroll: false,
            hscroll: false,
        });
        let groups = win.pane_groups();
        let strips = win.pane_strips(&chrome, None);
        let rowless: std::collections::HashSet<_> = groups.keys().copied().collect();
        let hosts = win.pane_hosts(&rowless);
        Some(std::rc::Rc::new(Splits {
            root,
            maximized,
            active: Some(active),
            chrome,
            controls: PaneControls {
                maximize: several || is_maximized,
                close: several && !is_maximized,
            },
            groups,
            interiors: Default::default(),
            strips,
            hover: None,
            drop_zone: None,
            hosts,
        }))
    }

    /// The `Interior` for the panel a pane's buffer holds, when it is one the
    /// adapter covers.
    ///
    /// The pane half of [`Self::panel_interior`], and the differences are the
    /// facts a mounted panel does not have. It reserves no focus-marker
    /// gutter (`rerender_widget_panel` passes `focus_marker: false` for a
    /// panel with no slot). It carries no hover memo, because the runtime's
    /// `update_widget_hover` only ever probed the dock and the floating panel
    /// — a mounted panel's rows have never lit under the pointer, so nothing
    /// is lost by not saying so. Its bars are ordinary bars, not the dock's
    /// overlay pair. And its row budget is layout's, not state's — see
    /// [`crate::view::shell::panel::Interior::avail_height`].
    fn pane_panel_interior(
        &self,
        buffer: fresh_core::BufferId,
    ) -> Option<crate::view::shell::panel::Interior> {
        use std::rc::Rc;
        let key = self
            .widget_registry
            .panels_for_buffer(buffer)
            .into_iter()
            .next()?;
        let spec = self.widget_registry.get(&key)?.spec.clone();
        Some(crate::view::shell::panel::Interior {
            spec: Rc::new(spec),
            states: Rc::new(
                self.widget_registry
                    .instance_states(&key)
                    .cloned()
                    .unwrap_or_default(),
            ),
            focus_key: self
                .widget_registry
                .focus_key(&key)
                .map(|s| s.to_string())
                .unwrap_or_default(),
            // A pane-mounted panel holds the keyboard when its pane is the
            // active one, which the pane knows and this builder does not:
            // `splits::panel_content` sets it. See `Ctx::keyboard`.
            keyboard: false,
            page: self.page_anchors.get(&key).cloned(),
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
            // **The panel's keymap: its buffer's mode.** A pane-mounted
            // panel's plugin declares its bindings on the buffer it mounted
            // into (`setBufferMode`), and that is the mode its interior
            // resolves a key against first — the same way the dock's
            // resolves against the mode it mounted with.
            keymap: self
                .buffer_mode(buffer)
                .map(|mode| crate::view::shell::panel::Keymap {
                    mode: mode.to_string(),
                    resolver: self.keybindings.clone(),
                    text_focused: self.panel_focused_widget_is_text(&key),
                }),
            markdown: Some(self.markdown_ink()),
        })
    }

    /// Whether the tree describes this pane's mounted panel, without building
    /// it — the painter's half of the gate, asked once per pane per frame.
    ///
    /// Same shape and same reason as [`Self::panel_is_described`]: the
    /// interior clones the spec and the whole instance-state map, which is the
    /// right price where the description is built and the wrong one in a
    /// paint loop. And the same answer: a pane's panel is described whenever
    /// one is mounted. The class that rode the buffer's scroll — whose rows
    /// were buffer lines and whose selection was the buffer's cursor — is
    /// gone (design §3.5): its plugins select through the List's own `select`
    /// in a `scrollable: false` pane, or scroll a host-owned page.
    pub(crate) fn pane_panel_is_described(&self, buffer: fresh_core::BufferId) -> bool {
        self.widget_registry
            .panels_for_buffer(buffer)
            .into_iter()
            .next()
            .and_then(|key| self.widget_registry.get(&key))
            .is_some()
    }

    /// The panel's interior as a description.
    ///
    /// **All of it is host state the spec does not carry** — the focused
    /// widget, the widget and row under the pointer, whether the focus-marker
    /// gutter is reserved, the auto-size row budget, and the instance state
    /// the stateful kinds are authoritative for. The runtime read the same
    /// list off a `RenderContext`; here it is resolved once, where the
    /// description is built, and handed down.
    ///
    /// `None` means there is no panel in the slot, or none mounted in the
    /// registry — never "this spec has a variant the tree cannot describe",
    /// which is what the `covered` gate this used to consult meant before it
    /// ran out of `false` arms and was deleted. [`Self::panel_is_described`]
    /// is the same question without the clone, and the surfaces that route a
    /// press by it (`view::shell::dock::column`,
    /// `render_floating_widget_panel`) must keep asking the same one.
    pub(crate) fn panel_interior(
        &self,
        slot: crate::app::PanelSlot,
    ) -> Option<crate::view::shell::panel::Interior> {
        use std::rc::Rc;
        let panel = self.panel(slot)?;
        let key = panel.panel_key.clone();
        let spec = self.widget_registry.get(&key)?.spec.clone();
        Some(crate::view::shell::panel::Interior {
            spec: Rc::new(spec),
            states: Rc::new(
                self.widget_registry
                    .instance_states(&key)
                    .cloned()
                    .unwrap_or_default(),
            ),
            focus_key: self
                .widget_registry
                .focus_key(&key)
                .map(|s| s.to_string())
                .unwrap_or_default(),
            keyboard: panel.focused,
            page: None,
            hovered_key: Some(panel.hovered_widget_key.clone()).filter(|k| !k.is_empty()),
            hovered_item_key: panel.hovered_item_key.clone(),
            hovered_popup_row: panel.hovered_popup_row.clone(),
            marker_gutter: panel.focus_marker,
            avail_height: self.floating_panel_inner_height(slot),
            // **The dock's bars are overlay bars.** Every other panel draws
            // one whenever its content overflows; the dock's appears while
            // the pointer is over the column or a keyboard move just flashed
            // it, and is gone otherwise — see `widgets::Ctx::scrollbar_reveal`
            // for why that is a fact handed down rather than a rule the tree
            // could apply itself.
            scrollbar_reveal: matches!(panel.placement, super::PanelPlacement::LeftDock { .. })
                .then(|| {
                    panel.scrollbar_zone_hovered
                        || panel
                            .scrollbar_flash_until
                            .is_some_and(|until| self.time_source().now() < until)
                }),
            // **The panel's keymap: the mode its plugin defined.** The one
            // it declared at mount, or else the active window's editor mode,
            // which is how a plugin that mounts a centred form declares one;
            // a sidebar section takes its keys through `widget_event` and
            // never through a mode, so it declares none.
            keymap: match slot {
                crate::app::PanelSlot::Dock | crate::app::PanelSlot::Floating => panel
                    .mode
                    .clone()
                    .or_else(|| self.active_window().editor_mode.clone())
                    .map(|mode| crate::view::shell::panel::Keymap {
                        mode,
                        resolver: self.keybindings.clone(),
                        text_focused: self.panel_focused_widget_is_text(&key),
                    }),
                crate::app::PanelSlot::Sidebar(_) => None,
            },
            markdown: Some(self.markdown_ink()),
        })
    }

    /// The theme and grammar handles a described markdown document renders
    /// through.
    ///
    /// One place, because the two panel interiors and every future described
    /// surface want the same pair, and because getting it wrong is silent:
    /// `render_markdown_text_area` answers a missing context by rendering the
    /// markdown *source*, not by failing.
    pub(crate) fn markdown_ink(&self) -> crate::view::shell::panel::MarkdownInk {
        crate::view::shell::panel::MarkdownInk {
            theme: self.theme.read().unwrap().clone().into(),
            grammars: self.grammar_registry.clone(),
        }
    }

    pub(crate) fn panel_description(&self) -> Option<crate::view::shell::panel::Panel> {
        use crate::primitives::display_width::str_width;
        use crate::view::shell::panel::{Panel, Spot};

        let p = self.panel(crate::app::PanelSlot::Floating)?;
        // Every row the spec produced, borders excluded — `WindowEmbed`
        // reservations included, since each contributes its blank entries and
        // an `EmbedRect` painted over them. This is the count the painter's
        // `entries.len() + 2` used, kept as the one measurement the tree needs
        // from the runtime.
        //
        // **Only one of the two is still read.** A described box measures its
        // own height (`Panel::height` answers `Sizing::Auto`), so
        // `content_rows` survives for a panel whose interior is a `Host` —
        // which today means no panel at all. `content_cols` is live: an
        // anchored popup hugs its content horizontally and the interior is
        // built by a `layout_reader` that needs a width as a number, so the
        // mirror answers for it. See `Panel::anchored_width`, which is where
        // that exception is argued and what retires it.
        let content_rows = p.entries.len() as u16;
        let spot = match p.placement {
            super::PanelPlacement::Centered => Spot::Centered {
                width_pct: p.width_pct,
                content_rows,
            },
            super::PanelPlacement::Anchored { x, y } => Spot::Anchored {
                x,
                y,
                content_cols: p
                    .entries
                    .iter()
                    .map(|e| str_width(&e.text) as u16)
                    .max()
                    .unwrap_or(0),
                content_rows,
            },
            // The dock panel's frame is the dock column's, not this box's —
            // and a sidebar section's is its column's.
            super::PanelPlacement::LeftDock { .. }
            | super::PanelPlacement::SidebarSection { .. } => return None,
        };
        Some(Panel {
            // **`None` means there is no panel mounted in the slot**, and
            // nothing else. This used to say "described when every variant of
            // the spec is one the tree describes, and painted whole
            // otherwise" — the `covered` gate, which ran out of `false` arms
            // and was deleted in 2.4. `panel_interior` asks one question, and
            // it is the same one `panel_is_described` asks; `WindowEmbed` is
            // described like everything else, as a `Host` leaf.
            interior: self.panel_interior(crate::app::PanelSlot::Floating),
            spot,
            title: p.title.clone(),
            closable: p.closable,
            focused: p.focused,
            fullscreen: p.fullscreen,
        })
    }
}

/// Building the status bar's description from live state.
impl Editor {
    /// The bar's elements, in the order they sit on the row.
    ///
    /// This is the half of `render_status` that decides *what is on the bar*.
    /// The other half — where each element lands — is the tree's now; see
    /// `view::shell::status_bar`.
    pub(crate) fn status_bar_description(
        &mut self,
        width: u16,
    ) -> Option<crate::view::shell::status_bar::StatusBar> {
        use crate::app::shell_host::shell_theme::{attrs, literal};
        use crate::view::shell::status_bar as sb;
        use crate::view::ui::status_bar::{element_kind_name, StatusBarRenderer};

        let (bar_fg, bar_bg, sep_fg, sep_bg) = {
            let t = self.theme.read().unwrap();
            (
                t.status_bar_fg,
                t.status_bar_bg,
                t.status_separator_fg,
                t.status_separator_bg,
            )
        };
        // What a theme key resolves to, for deciding whether a span's colour
        // *is* the one its element's key names. Snapshotted here so the read
        // guard does not have to be held across the description build.
        let theme_snapshot = self.theme.read().unwrap().clone();
        let theme_of = move |key: &str| theme_snapshot.resolve_theme_key(key);
        self.with_status_bar_ctx(|ctx, config| {
            let lsp_state = ctx.lsp_indicator_state;
            // Whether the dedicated remote indicator is on the bar, so the
            // filename branch can drop its now-redundant prefix. Read before
            // the sides are rendered, exactly as before.
            ctx.remote_indicator_on_bar = config
                .left
                .iter()
                .chain(config.right.iter())
                .any(|e| matches!(e, crate::config::StatusBarElement::RemoteIndicator));

            let left = StatusBarRenderer::render_side(&config.left, ctx);
            let mut right = StatusBarRenderer::render_side(&config.right, ctx);

            // **Which right-hand elements survive** — a content decision, made
            // from measured text, kept verbatim from `render_status`. Reserve
            // a sane minimum for the left side so the buffer name and cursor
            // position are not truncated to a single character on a narrow
            // terminal, then drop low-priority right elements (configured
            // right-most first) until the rest fits alongside it. The *first*
            // right element is never dropped, so a user who configured any
            // right-side status keeps some of it.
            let available = width as usize;
            let sep_w = crate::primitives::display_width::str_width(&config.separator);
            let total_right: usize = right.iter().map(|(_, w, _, _)| *w).sum::<usize>()
                + sep_w * right.len().saturating_sub(1);
            let left_min_target = available.saturating_mul(2).saturating_div(5).min(40);
            let right_budget = available.saturating_sub(left_min_target + 1);
            if total_right > right_budget && right.len() > 1 {
                let mut current = total_right;
                while current > right_budget && right.len() > 1 {
                    let Some(dropped) = right.pop() else { break };
                    current = current.saturating_sub(dropped.1).saturating_sub(sep_w);
                }
            }

            // **And the other half of the budget: cap the left side.**
            //
            // `render_status` reserved the right side first and spent what was
            // left on the left, truncating the element that did not fit and
            // dropping the rest. Only the right-hand drop above was ported;
            // this was not, and layout does not stand in for it — see
            // `sb::left_budget`, which is the rule and where it is tested.
            // Without it a long status message pushed `LSP (off)` and
            // `Palette: Ctrl+P` off the edge, where before the message itself
            // became `...`.
            let right_width: usize = right.iter().map(|(_, w, _, _)| *w).sum::<usize>()
                + sep_w * right.len().saturating_sub(1);
            let widths: Vec<usize> = left.iter().map(|(_, w, _, _)| *w).collect();
            let allowed = sb::left_budget(&widths, right_width, sep_w, available);
            let left: Vec<_> = left
                .into_iter()
                .zip(allowed)
                .map(|((spans, w, kind, token_key), cap)| {
                    if w <= cap {
                        return (spans, w, kind, token_key);
                    }
                    // The element that did not fit is truncated over its
                    // concatenated text, as before. Its runs keep their own
                    // themes rather than collapsing to one style — the only
                    // difference from `render_status`, and invisible for a
                    // single-run element like the message.
                    let text: String = spans.iter().map(|s| s.content.as_ref()).collect();
                    let cut = crate::view::ui::status_bar::truncate_to_width(&text, cap);
                    let cut_w = crate::primitives::display_width::str_width(&cut);
                    let mut budget = cut_w;
                    let mut kept: Vec<ratatui::text::Span<'static>> = Vec::new();
                    for sp in spans {
                        if budget == 0 {
                            break;
                        }
                        let w = crate::primitives::display_width::str_width(&sp.content);
                        if w <= budget {
                            budget -= w;
                            kept.push(sp);
                        } else {
                            let part = crate::view::ui::status_bar::truncate_to_width(
                                sp.content.as_ref(),
                                budget,
                            );
                            budget = 0;
                            kept.push(ratatui::text::Span::styled(part, sp.style));
                        }
                    }
                    (kept, cut_w, kind, token_key)
                })
                .collect();

            let item = |(spans, _w, kind, token_key): (
                Vec<ratatui::text::Span<'static>>,
                usize,
                crate::view::ui::status_bar::ElementKind,
                Option<String>,
            )| {
                // **Names where a name exists.** Every colour on this bar comes
                // from a named theme field — `element_spans` resolves
                // `status_error_indicator_fg` and friends into a `Style`, and
                // re-encoding that as `#rrggbb` threw the name away, which is
                // why provenance had to be carried in a second field beside
                // the paint.
                //
                // A span whose colours are the ones `element_keys` names for
                // this element carries those names; anything else is a colour
                // with no name and carries a literal. That is the honest
                // distinction, and it is the same one the grammar already
                // draws — so provenance is *read back out of* the run's theme
                // rather than duplicated next to it.
                let (kfg, kbg) = StatusBarRenderer::element_keys(kind, lsp_state);
                let named = |c: ratatui::style::Color,
                             key: &'static str,
                             fallback: ratatui::style::Color|
                 -> String {
                    let resolved = theme_of(key).unwrap_or(fallback);
                    if c == resolved {
                        key.to_string()
                    } else {
                        literal(c)
                    }
                };
                let runs = spans
                    .into_iter()
                    .map(|s| {
                        let fg = named(s.style.fg.unwrap_or(bar_fg), kfg, bar_fg);
                        let bg = named(s.style.bg.unwrap_or(bar_bg), kbg, bar_bg);
                        let mut mods: Vec<&str> = Vec::new();
                        if s.style
                            .add_modifier
                            .contains(ratatui::style::Modifier::BOLD)
                        {
                            mods.push("bold");
                        }
                        if s.style
                            .add_modifier
                            .contains(ratatui::style::Modifier::ITALIC)
                        {
                            mods.push("italic");
                        }
                        if s.style
                            .add_modifier
                            .contains(ratatui::style::Modifier::UNDERLINED)
                        {
                            mods.push("underlined");
                        }
                        (s.content.to_string(), attrs(&fg, &bg, &mods))
                    })
                    .collect();
                sb::Item {
                    runs,
                    name: element_kind_name(kind),
                    clickable: StatusBarRenderer::clickable_for_kind(kind),
                    token_key,
                }
            };

            sb::StatusBar {
                left: left.into_iter().map(item).collect(),
                right: right.into_iter().map(item).collect(),
                separator: config.separator.clone(),
                base_theme: crate::app::shell_host::shell_theme::pair(
                    "ui.status_bar_fg",
                    "ui.status_bar_bg",
                ),
                sep_theme: crate::app::shell_host::shell_theme::attrs(
                    &literal(sep_fg),
                    &literal(sep_bg),
                    &[],
                ),
            }
        })
    }
}
