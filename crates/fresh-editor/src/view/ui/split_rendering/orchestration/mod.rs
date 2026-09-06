//! Orchestration layer.
//!
//! This subdirectory holds the code that depends on the shared
//! [`SelectionContext`](contexts::SelectionContext) and
//! [`DecorationContext`](contexts::DecorationContext) carriers. Everything
//! *outside* this directory is self-contained and has no such dependency —
//! the quarantine is intentional.
//!
//! Public entry points ([`prepare_content`], [`reconcile_panes`],
//! [`content_pass`], [`paint_leaf`], [`build_base_tokens_for_hook`]) live
//! here too; the frame's painter (`app::shell_host`) drives them.

pub(super) mod contexts;
pub(super) mod overlay_sweep;
pub(super) mod overlays;
pub(crate) mod reconcile;
pub(super) mod render_buffer;
pub(super) mod render_composite;
pub(super) mod render_line;
pub(super) mod selection_sweep;
pub(super) mod tail_fill;

use super::base_tokens::build_base_tokens;
use super::layout::{resolve_view_preferences, split_layout, SplitLayout};
use super::EditorRenderConfig;
use crate::app::BufferMetadata;
use crate::model::buffer::Buffer;
use crate::model::event::{BufferId, EventLog, LeafId};
use crate::state::EditorState;
use crate::view::shell::geometry::PaneRects;
use crate::view::shell::splits::PaneChrome;
use crate::view::split::{SplitManager, SplitViewState};
use crate::view::ui::RenderStyle;
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::widgets::Paragraph;
use ratatui::widgets::Widget;
// Re-exported one level up (split_rendering::SplitRenderer) so the
// `render_phantom_leaf` façade can forward into the per-leaf
// pipeline. Stays crate-private; callers use the façade.
use reconcile::{reconcile_pane, settle_pane, ReconcileInputs};
pub(super) use render_buffer::{caret_cell, compute_buffer_layout, draw_buffer_in_split};
use render_composite::render_composite_buffer;
use std::collections::HashMap;

/// How a single visible split should be rendered. Computed up-front by
/// [`expand_visible_buffers`], which expands any active buffer-group tab into
/// its inner panels.
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum RenderKind {
    /// Regular split: render the tab bar and the buffer content.
    Normal,
    /// Main split whose buffer group is active: render the tab bar (to show
    /// the group tab) but skip buffer content — the group's inner leaves fill
    /// it instead.
    GroupTabBarOnly,
    /// A leaf inside a Grouped subtree. `split_area` is already the content
    /// rect for this inner leaf; no tab bar is rendered.
    InnerLeaf,
}

/// One visible split to render: `(tab_bar_owner_split, effective_leaf_id,
/// buffer_id, split_area, kind)`.
pub(crate) type VisibleBuffer = (LeafId, LeafId, BufferId, Rect, RenderKind);

/// What every pane in a frame shares, resolved once before any of them paints.
///
/// The preamble half of `render_content`. It has to happen before the first
/// pane and exactly once — `expand_visible_buffers` resizes an inner panel's
/// viewport as it expands a group — so it cannot live inside a per-pane call.
pub(crate) struct ContentPass {
    /// Every pane to paint, in paint order, groups already expanded.
    pub visible: Vec<VisibleBuffer>,
    pub active_split_id: LeafId,
    /// Where the frame put every pane, off the one layout that placed them.
    /// The rects in `visible` came from here, and [`paint_leaf`] checks the
    /// content rect it carves against it.
    pub rects: PaneRects,
}

/// What every pane in a frame reads and none of them writes.
///
/// The appearance and the hover state, which are the frame's, not a pane's.
/// It is `Copy` because `RenderStyle` is: this is a bundle of borrows and
/// flags, so passing it costs what passing its parts cost.
#[derive(Clone, Copy)]
pub(crate) struct FrameFacts<'a> {
    pub style: RenderStyle<'a>,
    pub buffer_metadata: &'a HashMap<BufferId, BufferMetadata>,
    pub grouped_subtrees: &'a HashMap<LeafId, crate::view::split::SplitNode>,
    /// What chrome each of the split manager's panes has, resolved once with
    /// the shell's description of the same grid — see `Window::pane_chrome`.
    /// A buffer group's panel is not one of those panes, and resolves in
    /// [`paint_leaf`].
    pub pane_chrome: &'a HashMap<LeafId, PaneChrome>,
    /// The panes whose mounted plugin panel the shell tree *describes*.
    ///
    /// Their text pass does not run. The virtual buffer is still filled — it
    /// is the mirror search, copy and the `lines_changed` hooks read — but the
    /// panel is drawn by the description mounted over this pane's `Host`, and
    /// painting the mirror underneath it would be the same panel twice, at two
    /// wrap widths, with the tree winning wherever they differ. See
    /// `Editor::described_panes`.
    pub described_panes: &'a std::collections::HashSet<LeafId>,
    pub lsp_waiting: bool,
    pub hide_cursor: bool,
    pub session_mode: bool,
    pub screen_width: u16,
}

/// The stores a pane paints out of and writes its viewport back into.
///
/// Every one of these is shared by the whole frame; what makes them a
/// separate carrier from [`FrameFacts`] is that a pane *changes* them, so
/// they are borrowed mutably and cannot ride along in a `Copy` bundle.
pub(crate) struct Stores<'a> {
    pub buffers: &'a mut HashMap<BufferId, EditorState>,
    pub event_logs: &'a mut HashMap<BufferId, EventLog>,
    pub composite_buffers:
        &'a mut HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    pub composite_view_states:
        &'a mut HashMap<(LeafId, BufferId), crate::view::composite_view::CompositeViewState>,
    pub split_view_states: Option<&'a mut HashMap<LeafId, crate::view::split::SplitViewState>>,
    pub cell_theme_map: &'a mut Vec<crate::app::types::CellThemeInfo>,
}

/// Resolve what every pane in this frame shares. See [`ContentPass`].
///
/// `rects` is where the frame put every pane, and `base_visible` is the
/// split manager's visible leaves at the boxes `rects` gives them — the
/// caller reads both off the same layout, which is the whole point.
pub(crate) fn prepare_content(
    rects: PaneRects,
    base_visible: &[(LeafId, BufferId, Rect)],
    split_manager: &SplitManager,
    split_view_states: Option<&mut HashMap<LeafId, crate::view::split::SplitViewState>>,
    grouped_subtrees: &HashMap<LeafId, crate::view::split::SplitNode>,
) -> ContentPass {
    ContentPass {
        // Expand any active buffer-group tabs into their inner panels.
        visible: expand_visible_buffers(base_visible, split_view_states, grouped_subtrees, &rects),
        active_split_id: split_manager.active_split(),
        rects,
    }
}

/// Paint one pane: its tab strip, its buffer (or composite, or placeholder),
/// and its two scrollbars — and record the rectangles chrome reads back.
///
/// **The per-pane half of a frame's paint.** The loop this came out of mixed
/// three things: a preamble computed once for the frame, this, and seven
/// accumulators. Only this part is a pane's, and separating it is what lets a
/// pane become its own `Host` in the shell's tree rather than a slice of one
/// rectangle that paints them all.
///
/// `pane` carries the rectangle to paint into. For the shell that rectangle
/// is the one *layout* gave the pane's `Host`; for a caller that lays the
/// grid out itself it is [`prepare_content`]'s. They agree — the description
/// and the model share `split_rect_ext` — and the parity tests in
/// `view::shell::splits` are what says so.
pub(crate) fn paint_leaf(
    buf: &mut ratatui::buffer::Buffer,
    pane: VisibleBuffer,
    f: &FrameFacts<'_>,
    pass: &ContentPass,
    s: &mut Stores<'_>,
    contents: &mut HashMap<LeafId, PaneContent>,
) {
    // Unpacked into the names the body below uses. The body is the loop's,
    // moved without an edit inside it; the carriers are what replaced its
    // forty parameters.
    let (main_split_id, split_id, buffer_id, split_area, kind) = pane;
    let FrameFacts {
        style,
        buffer_metadata,
        described_panes,
        ..
    } = *f;
    let theme = style.theme;
    let EditorRenderConfig {
        use_terminal_bg,
        show_tilde,
        ..
    } = style.cfg;
    let active_split_id = pass.active_split_id;
    let buffers = &mut *s.buffers;
    let event_logs = &mut *s.event_logs;
    let composite_buffers = &mut *s.composite_buffers;
    let composite_view_states = &mut *s.composite_view_states;
    let split_view_states = s.split_view_states.as_deref_mut();

    let is_active = split_id == active_split_id;
    let is_inner_group_leaf = kind == RenderKind::InnerLeaf;
    let skip_content = kind == RenderKind::GroupTabBarOnly;
    // For a buffer-group panel (inner leaf), `active_split()` returns the
    // group's *outer* leaf, so `is_active` is never true for the panel
    // itself. The panel is focused when the active split's
    // `focused_group_leaf` points at this inner leaf. Used to gate the
    // composite cursor so it doesn't linger after Tab moves focus away.
    let panel_focused = if is_inner_group_leaf {
        split_view_states
            .as_deref()
            .and_then(|svs| svs.get(&active_split_id))
            .and_then(|vs| vs.focused_group_leaf)
            .is_some_and(|fl| fl == split_id)
    } else {
        is_active
    };
    let _ = main_split_id; // no longer needed below, kept for clarity

    // Hide tildes per-split (e.g., for buffer group panels). Also
    // hide them when the split's active buffer is a terminal in
    // scrollback view — the PTY drew blank rows, so empty rows
    // past end-of-buffer should look blank rather than tilde-padded.
    // Without this, viewing a small-PTY terminal in a larger split
    // (e.g. workspace-restored dock terminal switched via Alt+]
    // into the main pane) shows tildes where the live PTY drew
    // blank.
    let active_buf_is_terminal = buffer_metadata
        .get(&buffer_id)
        .and_then(|m| m.virtual_mode())
        .is_some_and(|m| m == "terminal");
    let split_show_tilde = show_tilde
        && !split_view_states
            .as_deref()
            .and_then(|svs| svs.get(&split_id))
            .is_some_and(|vs| vs.hide_tilde)
        && !active_buf_is_terminal;

    let chrome = resolve_pane_chrome(pane, f);
    // A pane whose content is pinned to its size: it earns no scrollbar,
    // and its viewport does not scroll to follow the cursor either — the
    // second is why this outlives the `PaneChrome` that swallowed the first.
    // An inner leaf has no strip and no bottom bar, so its whole area is
    // content but for the scrollbar column — which is what a
    // `pane_interior` with those two flags off lays out. It used to be
    // four rectangles written by hand right here.
    let layout = split_layout(split_id, split_area, chrome);
    // The content rect this carves is the one the tree's `content_key` node
    // has: `pane_interior` is one statement laid out twice, at the pane's box
    // here and inside the frame there. The clip below is a release safety
    // net, not the contract — where the two ever differ, this says so.
    if let Some(described) = pass.rects.content(split_id) {
        debug_assert_eq!(
            layout.content_rect, described,
            "pane {split_id:?}: the content rect handed to the painter is not the tree's"
        );
    }
    // For GroupTabBarOnly entries we've already rendered the tab bar;
    // skip buffer content rendering so the group's inner leaves can
    // draw into the content rect without being overwritten.
    if skip_content {
        return;
    }

    // **A described mounted panel is drawn by the tree, not from its mirror.**
    // Deliberately after the tab strip and not folded into `skip_content`: the
    // strip above this is still the painter's, and what stops here is the
    // three things the description states for itself — the buffer's text, its
    // tildes past the last row, and its scrollbars. The mirror is still
    // written on every re-render; it is the rendering path it stops being.
    if described_panes.contains(&split_id) {
        return;
    }

    // Synthesized placeholder buffer (kept alive when
    // `auto_create_empty_buffer_on_last_buffer_close` is disabled): paint
    // the pane blank with a subdued, centered hint so the user sees how
    // to leave the empty workspace state.
    let is_synthetic_placeholder = buffer_metadata
        .get(&buffer_id)
        .is_some_and(|m| m.synthetic_placeholder);
    if is_synthetic_placeholder {
        render_placeholder_hint(buf, layout.content_rect, theme);
        return;
    }

    // Composite buffers (side-by-side diff/compare panes) render through a
    // separate pipeline; dispatch them to their own helper.
    if buffers
        .get(&buffer_id)
        .is_some_and(|s| s.is_composite_buffer)
    {
        render_composite_split(
            buf,
            &layout,
            split_id,
            buffer_id,
            buffers,
            composite_buffers,
            composite_view_states,
            split_view_states,
            theme,
            panel_focused,
            use_terminal_bg,
            split_show_tilde,
            chrome,
            is_active,
        );
        return;
    }

    // Get references separately to avoid double borrow
    // **The text is what the content pass laid out.** The rows, the caret
    // and the horizontal scroll were settled for this pane before the tree
    // painted (`content_pass`); this draws them and nothing here formats a
    // line again.
    // A pane whose buffer is gone — closed under the split it still names,
    // until the model catches up — has no content to draw and drew nothing
    // before either.
    let _ = event_logs;
    if !buffers.contains_key(&buffer_id) {
        return;
    }
    let Some(content) = contents.remove(&split_id) else {
        debug_assert!(false, "pane {split_id:?}: painted without a content pass");
        return;
    };
    // The rows were settled on the pane's leaf by the content pass
    // (`settle_views`); this draws the same layout and records nothing.
    let _render_buf_span = tracing::trace_span!("draw_buffer_in_split").entered();
    draw_buffer_in_split(
        buf,
        content.layout,
        layout.content_rect,
        theme,
        style.ansi_background,
        style.cfg.background_fade,
        style.cfg.software_cursor_only,
        &content.rulers,
        content.guides,
        content.highlight_column,
        content.caret.filter(|_| content.caret_shown),
    );
    drop(_render_buf_span);
}

/// What a pane's text pass settled for the frame, before the tree painted.
///
/// **The caret has one source, and it is this.** The rows and the caret's
/// cell are answers of the same layout; the pane's leaf settles them on its
/// view — the rows for the byte under a cell, the caret for the display
/// list's cursor — and [`paint_leaf`] draws the cells from the same layout.
pub(crate) struct PaneContent {
    /// The content rect the rows were laid out for.
    pub rect: Rect,
    pub layout: render_buffer::BufferLayoutOutput,
    /// The caret's cell on screen, when this pane has one: the pane is the
    /// active one and its buffer shows cursors.
    pub caret: Option<(u16, u16)>,
    /// Whether the caret is on screen — nothing above the pane owns the
    /// keyboard and no modal is up. A caret that is not shown still has a
    /// cell, which the popup anchored to it reads.
    pub caret_shown: bool,
    /// The paint's other inputs, resolved with the layout so the two cannot
    /// read a different setting.
    pub rulers: Vec<usize>,
    pub guides: Option<Vec<u16>>,
    pub highlight_column: bool,
}

/// Lay out every pane of the frame that has a text pass, at the content rect
/// [`paint_leaf`] will draw it into.
///
/// **After the reconcile and the hooks, before the tree paints.** The viewport
/// is settled ([`reconcile_panes`]), the plugins have decorated the lines the
/// frame will draw, and the rows are formatted here once; the pane's leaf
/// then paints from what this settled — its caret in the display list — and
/// the fold draws the cells.
pub(crate) fn content_pass(
    pass: &ContentPass,
    f: &FrameFacts<'_>,
    s: &mut Stores<'_>,
) -> HashMap<LeafId, PaneContent> {
    let _span = tracing::trace_span!("content_pass").entered();
    let mut out = HashMap::new();
    for pane in pass.visible.iter().copied() {
        let (_, split_id, _, split_area, _) = pane;
        if !pane_has_text_pass(pane, f, s.buffers) {
            continue;
        }
        let content_rect = pass.rects.content(split_id).unwrap_or_else(|| {
            let chrome = resolve_pane_chrome(pane, f);
            split_layout(split_id, split_area, chrome).content_rect
        });
        if let Some(c) = text_pane_content(pane, f, pass, s, content_rect) {
            out.insert(split_id, c);
        }
    }
    out
}

/// One pane's text pass: its rows at `content_rect`, and its caret.
fn text_pane_content(
    pane: VisibleBuffer,
    f: &FrameFacts<'_>,
    pass: &ContentPass,
    s: &mut Stores<'_>,
    content_rect: Rect,
) -> Option<PaneContent> {
    let (_, split_id, buffer_id, _, _) = pane;
    let FrameFacts {
        style,
        buffer_metadata,
        lsp_waiting,
        hide_cursor,
        session_mode,
        screen_width,
        ..
    } = *f;
    let theme = style.theme;
    let EditorRenderConfig {
        show_horizontal_scrollbar,
        show_tilde,
        highlight_current_column,
        hide_current_line_on_selection,
        ..
    } = style.cfg;
    let is_active = split_id == pass.active_split_id;
    let buffers = &mut *s.buffers;
    let event_logs = &mut *s.event_logs;
    let split_view_states = s.split_view_states.as_deref_mut();
    let cell_theme_map = &mut *s.cell_theme_map;

    let active_buf_is_terminal = buffer_metadata
        .get(&buffer_id)
        .and_then(|m| m.virtual_mode())
        .is_some_and(|m| m == "terminal");
    let split_show_tilde = show_tilde
        && !split_view_states
            .as_deref()
            .and_then(|svs| svs.get(&split_id))
            .is_some_and(|vs| vs.hide_tilde)
        && !active_buf_is_terminal;
    let is_non_scrollable = !buffers.get(&buffer_id).is_none_or(|s| s.scrollable);

    let state = buffers.get_mut(&buffer_id)?;
    let event_log_opt = event_logs.get_mut(&buffer_id);

    let view_prefs = resolve_view_preferences(state, split_view_states.as_deref(), split_id);

    let has_selection = hide_current_line_on_selection
        && split_view_states
            .as_deref()
            .and_then(|vs| vs.get(&split_id))
            .is_some_and(|vs| {
                vs.cursors
                    .iter()
                    .any(|(_, c)| c.selection_range().is_some())
            });
    let effective_highlight_current_line =
        view_prefs.highlight_current_line && state.show_cursors && !has_selection;

    let is_virtual_buffer = buffer_metadata
        .get(&buffer_id)
        .is_some_and(|m| m.is_virtual());
    let effective_rulers: Vec<usize> = if is_virtual_buffer {
        Vec::new()
    } else {
        view_prefs.rulers.clone()
    };

    let mut style = style;
    style.cfg.indentation_guide =
        crate::config::resolve_indentation_guide_mode(crate::config::IndentationGuideInputs {
            global: style.cfg.indentation_guide,
            user_override: split_view_states
                .as_deref()
                .and_then(|vs| vs.get(&split_id))
                .and_then(|vs| vs.indentation_guide_user_override),
            plugin_override: state.indentation_guide_override,
            language_gate: state.buffer_settings.indentation_guide,
            is_virtual_buffer,
        });

    let fold_indicators_visible = split_view_states
        .as_deref()
        .and_then(|vs| vs.get(&split_id))
        .map(|vs| vs.fold_indicators_visible())
        .unwrap_or(true);

    let mut fallback_view: Option<SplitViewState> = None;
    let vs: &mut SplitViewState = match split_view_states.and_then(|m| m.get_mut(&split_id)) {
        Some(vs) => vs,
        None => {
            let fresh = fallback_view.insert(SplitViewState::with_buffer(
                content_rect.width,
                content_rect.height,
                buffer_id,
            ));
            reconcile_pane(
                state,
                fresh.active_state_mut(),
                ReconcileInputs {
                    content_rect,
                    pin_to_top: is_non_scrollable,
                    show_horizontal_scrollbar,
                },
            );
            fresh
        }
    };
    let bvs = vs.active_state_mut();

    let crate::view::ui::EditorRenderConfig {
        estimated_line_length,
        highlight_context_bytes,
        relative_line_numbers,
        use_terminal_bg,
        software_cursor_only,
        diagnostics_inline_text,
        indentation_guide,
        indentation_guide_glyph,
        rainbow_indentation,
        bracket_highlight,
        ..
    } = style.cfg;
    let _span = tracing::trace_span!("text_pane_content").entered();
    let layout = compute_buffer_layout(
        state,
        &bvs.cursors,
        &bvs.viewport,
        &bvs.folds,
        content_rect,
        is_active,
        theme,
        lsp_waiting,
        view_prefs.view_mode.clone(),
        view_prefs.compose_width,
        estimated_line_length,
        highlight_context_bytes,
        relative_line_numbers,
        use_terminal_bg,
        session_mode,
        software_cursor_only,
        view_prefs.show_line_numbers,
        effective_highlight_current_line,
        fold_indicators_visible,
        diagnostics_inline_text,
        split_show_tilde,
        indentation_guide,
        indentation_guide_glyph,
        rainbow_indentation,
        bracket_highlight,
        Some((cell_theme_map, screen_width)),
    );
    let has_caret = is_active && state.show_cursors;
    let shows_caret = has_caret && !hide_cursor;
    let caret = has_caret
        .then(|| caret_cell(&layout, state.buffer.len()))
        .flatten();
    if let (Some(log), Some((x, y))) = (event_log_opt, caret.filter(|_| shows_caret)) {
        log.log_render_state(bvs.cursors.primary().position, x, y, state.buffer.len());
    }
    // The widest line is the bar's fact, settled before the frame
    // (`Editor::settle_pane_bars`); here the viewport is settled to it.
    let _ = settle_pane(
        state,
        &mut bvs.viewport,
        layout.left_column,
        show_horizontal_scrollbar,
    );
    Some(PaneContent {
        rect: content_rect,
        layout,
        caret,
        caret_shown: shows_caret,
        rulers: effective_rulers,
        guides: view_prefs.compose_column_guides,
        highlight_column: highlight_current_column && shows_caret,
    })
}

/// The chrome one pane has this frame: the scrollbars a `Fixed` panel never
/// had, and the column a terminal gives up while it streams its live PTY
/// grid (per split, so one terminal in two panes can differ — fresh#2595).
/// The rule that narrows the window's offer by these is
/// `PaneChrome::resolve`, and the shell's description resolves the same one
/// for the same pane. Shared by the paint and the reconcile before it, so
/// both lay the pane out at one content rect.
fn resolve_pane_chrome(pane: VisibleBuffer, f: &FrameFacts<'_>) -> PaneChrome {
    let (_, split_id, _, _, _) = pane;
    f.pane_chrome.get(&split_id).copied().unwrap_or_default()
}

/// Whether [`paint_leaf`] runs the text pass for this pane — as opposed to
/// painting only its tab strip (a group's outer pane), leaving it to the
/// tree (a described panel), painting the empty-workspace hint, or handing
/// it to the composite pipeline. The same four gates, in the same order.
fn pane_has_text_pass(
    pane: VisibleBuffer,
    f: &FrameFacts<'_>,
    buffers: &HashMap<BufferId, EditorState>,
) -> bool {
    let (_, split_id, buffer_id, _, kind) = pane;
    if kind == RenderKind::GroupTabBarOnly {
        return false;
    }
    if f.described_panes.contains(&split_id) {
        return false;
    }
    if f.buffer_metadata
        .get(&buffer_id)
        .is_some_and(|m| m.synthetic_placeholder)
    {
        return false;
    }
    buffers
        .get(&buffer_id)
        .is_some_and(|s| !s.is_composite_buffer)
}

/// Reconcile every pane of the frame that has a text pass, in paint order,
/// at the content rect [`paint_leaf`] will format it into.
///
/// **Before the frame, not during it.** The shell calls this once the tree
/// is laid out and before the `lines_changed` hooks and the fold; the
/// preview and replay paths call it just ahead of their own pass. Each
/// pane's viewport, margins and wrap index are then settled, and the text
/// pass is a read.
pub(crate) fn reconcile_panes(pass: &ContentPass, f: &FrameFacts<'_>, s: &mut Stores<'_>) {
    let _span = tracing::trace_span!("reconcile_panes").entered();
    let show_horizontal_scrollbar = f.style.cfg.show_horizontal_scrollbar;
    for pane in pass.visible.iter().copied() {
        let (_, split_id, buffer_id, split_area, _) = pane;
        if !pane_has_text_pass(pane, f, s.buffers) {
            continue;
        }
        // The content rect the tree placed the pane's text at; the painter
        // carves the same one from the pane's box and asserts they agree.
        let content_rect = pass.rects.content(split_id).unwrap_or_else(|| {
            let chrome = resolve_pane_chrome(pane, f);
            split_layout(split_id, split_area, chrome).content_rect
        });
        let Some(state) = s.buffers.get_mut(&buffer_id) else {
            continue;
        };
        let Some(vs) = s
            .split_view_states
            .as_deref_mut()
            .and_then(|m| m.get_mut(&split_id))
        else {
            continue;
        };
        reconcile_pane(
            state,
            vs.active_state_mut(),
            ReconcileInputs {
                content_rect,
                // A pane whose content is pinned to its size does not scroll
                // to follow the cursor.
                pin_to_top: !state.scrollable,
                show_horizontal_scrollbar,
            },
        );
    }
}

/// Build the list of splits to render, expanding any active buffer-group tab
/// into a [`RenderKind::GroupTabBarOnly`] entry for the main split followed by
/// one [`RenderKind::InnerLeaf`] entry per panel. Inner-panel viewports are
/// resized to their rendered rects so `editor.getViewport()` reports the panel
/// size (not the terminal size) and resize timing stays correct.
///
/// A panel's rect is the tree's: the group's grid is mounted in the outer
/// pane's content slot and each panel is keyed there like any pane, so
/// `rects` answers for it. This used to carve the outer pane's content rect
/// and lay the group out again in a scratch grid inside it.
fn expand_visible_buffers(
    base_visible: &[(LeafId, BufferId, Rect)],
    mut split_view_states: Option<&mut HashMap<LeafId, crate::view::split::SplitViewState>>,
    grouped_subtrees: &HashMap<LeafId, crate::view::split::SplitNode>,
    rects: &PaneRects,
) -> Vec<VisibleBuffer> {
    let mut visible_buffers: Vec<VisibleBuffer> = Vec::new();
    for (main_split_id, main_buffer_id, split_area) in base_visible {
        let active_group = split_view_states
            .as_deref()
            .and_then(|svs| svs.get(main_split_id))
            .and_then(|vs| vs.active_group_tab);

        let grouped = active_group.and_then(|leaf| grouped_subtrees.get(&leaf));
        let Some(grouped) = grouped else {
            visible_buffers.push((
                *main_split_id,
                *main_split_id,
                *main_buffer_id,
                *split_area,
                RenderKind::Normal,
            ));
            continue;
        };

        // The group's panels, at the boxes the tree placed them in.
        let inner_leaves = rects.visible(&grouped.visible_leaves());
        visible_buffers.push((
            *main_split_id,
            *main_split_id,
            *main_buffer_id,
            *split_area,
            RenderKind::GroupTabBarOnly,
        ));
        for (inner_leaf, inner_buffer, inner_rect) in &inner_leaves {
            // Keep inner panel viewports in sync with their actual rendered
            // dimensions (updated synchronously during rendering).
            if let Some(svs) = split_view_states.as_deref_mut() {
                if let Some(vs) = svs.get_mut(inner_leaf) {
                    vs.viewport.resize(inner_rect.width, inner_rect.height);
                }
            }
            visible_buffers.push((
                *main_split_id,
                *inner_leaf,
                *inner_buffer,
                *inner_rect,
                RenderKind::InnerLeaf,
            ));
        }
    }
    visible_buffers
}

/// Render a composite (side-by-side panes) buffer for one split, plus its
/// scrollbar, and record the content/scrollbar areas for mouse handling.
#[allow(clippy::too_many_arguments)]
fn render_composite_split(
    buf: &mut ratatui::buffer::Buffer,
    layout: &SplitLayout,
    split_id: LeafId,
    buffer_id: BufferId,
    buffers: &mut HashMap<BufferId, EditorState>,
    composite_buffers: &mut HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    composite_view_states: &mut HashMap<
        (LeafId, BufferId),
        crate::view::composite_view::CompositeViewState,
    >,
    split_view_states: Option<&mut HashMap<LeafId, crate::view::split::SplitViewState>>,
    theme: &crate::view::theme::Theme,
    panel_focused: bool,
    use_terminal_bg: bool,
    split_show_tilde: bool,
    chrome: PaneChrome,
    is_active: bool,
) {
    // Take initial_focus_hunk before borrowing composite immutably.
    let initial_focus_hunk = composite_buffers
        .get_mut(&buffer_id)
        .and_then(|c| c.initial_focus_hunk.take());
    let Some(composite) = composite_buffers.get(&buffer_id) else {
        return;
    };

    // Update SplitViewState viewport to match the actual rendered area so
    // cursor movement uses the correct viewport height after a resize.
    if let Some(svs) = split_view_states {
        if let Some(split_vs) = svs.get_mut(&split_id) {
            if split_vs.viewport.width != layout.content_rect.width
                || split_vs.viewport.height != layout.content_rect.height
            {
                split_vs
                    .viewport
                    .resize(layout.content_rect.width, layout.content_rect.height);
            }
        }
    }

    // Get or create the composite view state.
    let pane_count = composite.pane_count();
    let view_state = composite_view_states
        .entry((split_id, buffer_id))
        .or_insert_with(|| {
            crate::view::composite_view::CompositeViewState::new(buffer_id, pane_count)
        });

    // Apply the deferred initial focus hunk (first render only). This runs here
    // because it's the only place where both the CompositeViewState and the
    // correct viewport height exist.
    if let Some(hunk_index) = initial_focus_hunk {
        let mut hunk_count = 0usize;
        let target_row = composite.alignment.rows.iter().position(|row| {
            if row.row_type == crate::model::composite_buffer::RowType::HunkHeader {
                let hit = hunk_count == hunk_index;
                hunk_count += 1;
                hit
            } else {
                false
            }
        });
        if let Some(row) = target_row {
            let viewport_height = layout.content_rect.height.saturating_sub(1) as usize;
            let context_above = viewport_height / 3;
            view_state.cursor_row = row;
            view_state.scroll_row = row.saturating_sub(context_above);
        }
    }

    render_composite_buffer(
        buf,
        layout.content_rect,
        composite,
        buffers,
        theme,
        panel_focused,
        view_state,
        use_terminal_bg,
        split_show_tilde,
    );

    // The bar is a leaf beside the content (`paint_leaf`); `chrome` and
    // `is_active` shaped the content above.
    let _ = (chrome, is_active, split_id, buffer_id);
}

/// Public wrapper for building base tokens.
pub(crate) fn build_base_tokens_for_hook(
    buffer: &mut Buffer,
    top_byte: usize,
    estimated_line_length: usize,
    visible_count: usize,
    is_binary: bool,
    line_ending: crate::model::buffer::LineEnding,
) -> Vec<fresh_core::api::ViewTokenWire> {
    build_base_tokens(
        buffer,
        top_byte,
        estimated_line_length,
        visible_count,
        is_binary,
        line_ending,
        &[],
        // The hook hands this stream to a plugin, which may wrap it at a width
        // of its own choosing; budget by source lines only.
        None,
        false,
    )
}

/// Render a centered, subdued hint in the empty pane left behind when the
/// user closes the last buffer with both `file_explorer.auto_open_on_last_buffer_close`
/// and `editor.auto_create_empty_buffer_on_last_buffer_close` set to false.
/// Tells the user how to escape the blank-workspace state.
fn render_placeholder_hint(
    buf: &mut ratatui::buffer::Buffer,
    area: Rect,
    theme: &crate::view::theme::Theme,
) {
    const HINT: &str =
        "Ctrl+P  command palette   ·   Ctrl+O  open file   ·   Ctrl+E  file explorer";
    let needed_width = HINT.chars().count() as u16;
    if area.width < needed_width || area.height == 0 {
        return;
    }
    let x = area.x + area.width.saturating_sub(needed_width) / 2;
    let y = area.y + area.height / 2;
    let hint_area = Rect::new(x, y, needed_width, 1);
    let style = Style::default().fg(theme.syntax_comment);
    Paragraph::new(HINT).style(style).render(hint_area, buf);
}
