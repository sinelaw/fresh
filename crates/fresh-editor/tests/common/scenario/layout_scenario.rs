//! `LayoutScenario` — layout-dependent observables.
//!
//! Layout state (viewport scroll, hardware cursor screen position,
//! gutter width, visible byte range) is reconciled by the render
//! pipeline, not by action dispatch alone. `LayoutScenario` runs a
//! single render pass at the end of the action sequence so layout
//! state settles before assertion. Scenarios still avoid `for {
//! send_key; render; }` style imperative transcripts.
//!
//! Two assertion shapes are supported:
//! - `expected_top_byte`: legacy single-field shortcut, kept for
//!   the already-landed scenarios.
//! - `expected_snapshot`: a [`RenderSnapshotExpect`] with optional
//!   per-field constraints; unset fields wildcard-match.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::context::{MouseButton as CtxMouseButton, MouseEvent as CtxMouseEvent};
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::Observable;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect};
use fresh::test_api::{Action, EditorTestApi};

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct LayoutScenario {
    pub description: String,
    pub initial_text: String,
    /// Optional path to a fixture file to open via the editor's
    /// real file-open pipeline. When `Some(_)`, `initial_text` is
    /// ignored and the file at this path is loaded into the active
    /// buffer (used for tests whose load-bearing precondition is
    /// loading an on-disk fixture, e.g. CRLF round-trips).
    #[serde(default)]
    pub initial_file: Option<std::path::PathBuf>,
    pub width: u16,
    pub height: u16,
    pub actions: Vec<Action>,
    /// Optional input events (mouse, IME, etc.) dispatched after
    /// `actions` and before the final assertion render. Each event
    /// is translated to the editor's real input path (e.g. a
    /// `MouseEvent::Wheel { dy < 0 }` becomes a real
    /// `crossterm::MouseEventKind::ScrollDown` routed through
    /// `Editor::handle_mouse`). Use this for scenarios whose
    /// load-bearing precondition is a mouse interaction — scrollbar
    /// drags, wheel scrolls, clicks at specific cells — that have no
    /// direct `Action` projection.
    #[serde(default)]
    pub events: Vec<InputEvent>,
    /// Optional editor config. None ⇒ default config. Use for
    /// scenarios where `line_wrap` / `show_horizontal_scrollbar`
    /// etc. are load-bearing.
    #[serde(default, skip_serializing, skip_deserializing)]
    pub config: Option<fresh::config::Config>,
    /// Declarative editor-config overrides. Each `Some(_)` field
    /// is applied on top of `Config::default()` before the
    /// harness is built. Use this from semantic tests that
    /// can't import `fresh::config::Config` directly (the lint
    /// forbids the import outside harness-direct files). If
    /// both `config` and `config_overrides` are set, `config`
    /// wins (the explicit full struct path).
    #[serde(default)]
    pub config_overrides: ScenarioConfigOverrides,
    /// Single-field shortcut: assert just the viewport's top byte.
    /// Kept because most landed scenarios only care about scroll.
    #[serde(default)]
    pub expected_top_byte: Option<usize>,
    /// Multi-field expectation. Combine with or replace
    /// `expected_top_byte`.
    #[serde(default)]
    pub expected_snapshot: RenderSnapshotExpect,
    /// Per-step expectations for multi-step / cross-state claims.
    /// Each entry `{ after_action_index, expect }` is asserted after
    /// dispatching `actions[0..=after_action_index]` and rendering.
    /// Enables declarative encoding of invariants like "before X,
    /// top=A; after X, top=B" or "top_byte changes at most once
    /// across these N moves" (express as N expectations each pinning
    /// to one of two top values via
    /// `viewport_top_byte_in_set`).
    #[serde(default)]
    pub step_assertions: Vec<StepAssertion>,
    /// Cross-step invariant: across the snapshots taken at every
    /// `step_assertions` entry (in their original order), the
    /// number of distinct `viewport_top_byte` values observed must
    /// be `<= max`. Used to encode "viewport scrolled at most N
    /// times over this action sequence" — the load-bearing claim
    /// of issue #1147's viewport-stability tests. Only step
    /// snapshots count; the initial and final snapshots do not, so
    /// the caller controls exactly which points are observed.
    #[serde(default)]
    pub viewport_top_byte_distinct_at_most: Option<usize>,
    /// One-shot "redraw-screen" flag assertion: when `Some(want)`,
    /// the runner checks
    /// `EditorTestApi::take_full_redraw_request_for_tests()` against
    /// `want` after final actions/events have settled. Used for
    /// migrated `Action::RedrawScreen` (issue #1070) — the only
    /// observable for that action is the one-shot flag the event
    /// loop polls each frame.
    #[serde(default)]
    pub expected_full_redraw_requested: Option<bool>,
    /// Declarative mouse drags executed after `actions` and any
    /// `events`, before the final assertion render. Each entry is
    /// one Down/Move…/Up sequence. Symbolic variants (e.g.
    /// `VerticalScrollbarFullRange`) compute coordinates from the
    /// harness's content-area geometry at runtime, so scenario
    /// data doesn't have to hard-code layout-internal numbers.
    #[serde(default)]
    pub mouse_drags: Vec<MouseDragSpec>,
    /// Declarative popup injection. None ⇒ no popup. Becomes an
    /// `Event::ShowPopup` on the active buffer right before the
    /// final render. See [`PopupSpec`].
    #[serde(default)]
    pub show_popup: Option<PopupSpec>,
    /// Optional side-by-side diff composite-buffer setup. When set,
    /// the runner builds the composite (two virtual buffers + line
    /// alignment) and switches to it BEFORE dispatching `actions`
    /// or `events`; `initial_text` is unused in that mode. See
    /// [`CompositeBufferSpec`].
    #[serde(default)]
    pub composite_buffer: Option<CompositeBufferSpec>,
    /// Optional final assertion on the composite buffer's
    /// `initial_focus_hunk` field. `Some(true)` ⇒ the field must
    /// be `None` (the one-shot was consumed by a render);
    /// `Some(false)` ⇒ the field must still be `Some(_)`. Requires
    /// `composite_buffer` to be set. `None` ⇒ skip the check.
    #[serde(default)]
    pub expected_initial_focus_hunk_consumed: Option<bool>,
    /// Optional final assertion: the rightmost column at `col`
    /// contains a vertical scrollbar (track or thumb). Routed
    /// through `EditorTestHarness::has_scrollbar_at_column`.
    /// `None` ⇒ skip the check.
    #[serde(default)]
    pub expected_scrollbar_at_column: Option<u16>,
    /// Optional final assertion: NO column on the bottom-most
    /// content row carries a scrollbar (track or thumb). Used by
    /// migrated_horizontal_scrollbar anti-tests that drop the
    /// `show_horizontal_scrollbar = true` config flag.
    #[serde(default)]
    pub expected_no_horizontal_scrollbar_on_last_content_row: Option<bool>,
    /// Optional final assertion: the horizontal scrollbar IS
    /// present on either the last content row or the row below it
    /// (the natural slots the renderer uses for the horizontal
    /// thumb). Used by positive scrollbar-visibility scenarios.
    #[serde(default)]
    pub expected_horizontal_scrollbar_visible: Option<bool>,
    /// Optional final assertion: the editor's status_message
    /// matches this string. `None` ⇒ skip. Used by scrollbar /
    /// line-numbers toggle scenarios that round-trip through the
    /// "Vertical scrollbar hidden/shown" status display.
    #[serde(default)]
    pub expected_status_message: Option<String>,
    /// Optional final assertion: the primary cursor's hardware
    /// column equals `gutter_width + offset`. Used by the
    /// migrated_margin "cursor X position after typing 'abc' lands
    /// at gutter + 3" scenario.
    #[serde(default)]
    pub expected_cursor_col_equals_margin_plus: Option<u16>,
    /// Optional final assertion: the primary cursor's hardware row
    /// equals this value. Companion to
    /// `expected_cursor_col_equals_margin_plus`.
    #[serde(default)]
    pub expected_cursor_row_equals_content_first: bool,
    /// Optional final assertion: the row text containing a given
    /// substring must NOT start (after trimming leading spaces)
    /// with an ASCII digit. Used by
    /// `migrated_virtual_lines_have_no_gutter_line_number`.
    /// `(substring,)` — every row containing `substring` is checked.
    #[serde(default)]
    pub expected_virtual_rows_no_digit_gutter: Vec<String>,
    /// Optional final assertion: across the snapshot's
    /// `rendered_rows`, the row containing `before` must precede
    /// the row containing `after`. Used by the ABOVE-source-BELOW
    /// ordering scenario for virtual lines.
    #[serde(default)]
    pub expected_row_order: Vec<(String, String)>,
    /// Declarative virtual-text injections. Seeded before any
    /// `clear_virtual_text_namespaces` and before the final render
    /// via `EditorTestApi::seed_virtual_line`.
    #[serde(default)]
    pub initial_virtual_texts: Vec<VirtualTextSpec>,
    /// Declarative virtual-text namespace clears, applied after
    /// `initial_virtual_texts` but before the final render. Use
    /// for "after clearing namespace X, only Y remains".
    #[serde(default)]
    pub clear_virtual_text_namespaces: Vec<String>,
    /// Optional final assertion: the editor's `virtual_text_count`
    /// equals this value (after all injections / clears settle).
    #[serde(default)]
    pub expected_virtual_text_count: Option<usize>,
    /// Declarative margin annotations applied before the final
    /// render via `EditorTestApi::add_margin_annotation`.
    #[serde(default)]
    pub initial_margin_annotations: Vec<MarginAnnotationSpec>,
    /// Declarative margin-annotation removals (by id), applied
    /// after `initial_margin_annotations` but before the final
    /// render.
    #[serde(default)]
    pub remove_margin_annotations: Vec<String>,
}

/// Declarative virtual-line injection. Mirrors the parameter set
/// `VirtualTextManager::add_line` takes. Inline placements are
/// declared in the enum for future expansion but the seed shim
/// only wires the line variants today.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct VirtualTextSpec {
    /// Buffer byte offset the virtual line anchors to.
    pub byte_offset: usize,
    /// Display text of the virtual line.
    pub text: String,
    /// Placement relative to the anchor's source line.
    pub position: VirtualTextPositionSpec,
    /// Optional foreground RGB. `None` ⇒ default DarkGray.
    #[serde(default)]
    pub fg: Option<(u8, u8, u8)>,
    /// Optional background RGB.
    #[serde(default)]
    pub bg: Option<(u8, u8, u8)>,
    /// Namespace label (e.g. `"test"`, `"git-blame"`, `"lsp"`).
    pub namespace: String,
    /// Sort key: higher priority renders later.
    #[serde(default)]
    pub priority: i32,
}

/// Position enum for `VirtualTextSpec`. Mirrors the discriminants
/// of `fresh::view::virtual_text::VirtualTextPosition`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum VirtualTextPositionSpec {
    /// Render as a full line ABOVE the source line.
    Above,
    /// Render as a full line BELOW the source line.
    Below,
    /// Render inline. Reserved — the seed shim only handles
    /// `Above` / `Below` today; `Inline` panics if used.
    Inline,
}

/// Declarative margin annotation. Becomes an
/// `Event::AddMarginAnnotation` on the active buffer via
/// `EditorTestApi::add_margin_annotation`.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MarginAnnotationSpec {
    /// 0-indexed source line.
    pub line: usize,
    /// `"left"` or `"right"`.
    pub position: String,
    /// Glyph rendered in the gutter cell.
    pub symbol: String,
    /// Optional RGB foreground; `None` ⇒ theme default.
    #[serde(default)]
    pub color: Option<(u8, u8, u8)>,
    /// Identifier for later removal via
    /// `LayoutScenario::remove_margin_annotations`.
    #[serde(default)]
    pub annotation_id: Option<String>,
}

/// Declarative side-by-side diff composite-buffer setup. The
/// scenario runner expands this into two virtual buffers + a line
/// alignment computed from `hunks` via
/// [`EditorTestApi::create_side_by_side_diff`] before any event in
/// `events` runs. When `initial_focus_hunk` is `Some(_)`, the
/// runner also sets the composite's `initial_focus_hunk` field
/// before the first render.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct CompositeBufferSpec {
    /// Tab/title for the composite buffer (e.g. `"Diff View"`).
    pub name: String,
    /// Buffer mode for keybinding routing (e.g. `"diff-view"` so
    /// the `n`/`]`/`[`/`p` hunk-nav keybindings fire).
    pub mode: String,
    /// Left-pane source content (the "OLD" side of the diff).
    pub old_content: String,
    /// Right-pane source content (the "NEW" side of the diff).
    pub new_content: String,
    /// Hunks as `(old_start, old_count, new_start, new_count)`,
    /// 0-indexed line numbers — same shape as `DiffHunk::new`.
    pub hunks: Vec<(usize, usize, usize, usize)>,
    /// Optional one-shot scroll-to-hunk-N on the first render.
    /// The first render consumes the field and resets it to
    /// `None`. `None` ⇒ start at the buffer top.
    #[serde(default)]
    pub initial_focus_hunk: Option<usize>,
    /// When `true`, the runner switches to the composite buffer
    /// but does NOT perform the initial settle-render. Used by the
    /// `flush_layout`-before-render tests that probe pre-render
    /// composite state. Default `false` — the runner renders once
    /// to settle the layout, mirroring the e2e `setup_diff` helper.
    #[serde(default)]
    pub skip_initial_render: bool,
}

/// Declarative mouse drag. See `LayoutScenario::mouse_drags`.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum MouseDragSpec {
    /// Drag from `(from_col, from_row)` to `(to_col, to_row)` —
    /// raw cell coordinates.
    Cells {
        from_col: u16,
        from_row: u16,
        to_col: u16,
        to_row: u16,
    },
    /// Drag the vertical scrollbar thumb from the top of the
    /// content area to the bottom of the content area. The thumb
    /// column is `terminal_width - 1`; the first/last rows come
    /// from `harness.content_area_rows()`. Symbolic so scenario
    /// data doesn't need to know terminal geometry.
    VerticalScrollbarFullRange,
}

/// Declarative popup injection. See `LayoutScenario::show_popup`.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PopupSpec {
    #[serde(default)]
    pub title: Option<String>,
    pub lines: Vec<String>,
    pub width: u16,
    #[serde(default = "default_popup_max_height")]
    pub max_height: u16,
    #[serde(default = "default_popup_bordered")]
    pub bordered: bool,
    /// Optional placement. Defaults to `Centered` so existing
    /// scenarios continue to work; tests that need to cover a
    /// specific cell (e.g. the hardware cursor) opt into
    /// `AtHardwareCursorOffset` which resolves to the current
    /// hardware-cursor position at injection time, offset by
    /// `(dx, dy)`. `Fixed { x, y }` is also available for raw
    /// cell coordinates.
    #[serde(default)]
    pub position: PopupPlacement,
}

/// Declarative popup placement. Resolved against runtime state
/// (hardware cursor position) at injection time.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Default)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PopupPlacement {
    /// Default: centered in the viewport.
    #[default]
    Centered,
    /// Fixed `(x, y)` cell coordinates.
    Fixed { x: u16, y: u16 },
    /// Anchor the top-left corner at
    /// `(hardware_cursor.col + dx, hardware_cursor.row + dy)`
    /// (saturating). `dx` / `dy` are signed offsets in cells.
    /// Resolves to `Centered` if the hardware cursor is hidden.
    AtHardwareCursorOffset { dx: i32, dy: i32 },
}

fn default_popup_max_height() -> u16 {
    20
}
fn default_popup_bordered() -> bool {
    true
}

/// One per-step expectation. `after_action_index` is 0-based into
/// `actions`; the runner dispatches `actions[0..=after_action_index]`,
/// renders, then checks `expect` against the resulting snapshot.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct StepAssertion {
    pub after_action_index: usize,
    pub expect: RenderSnapshotExpect,
}

/// Declarative subset of `fresh::config::EditorConfig` flags that
/// scenario-mode tests need to set without importing
/// `fresh::config::Config` directly. Each `Some(_)` overrides the
/// corresponding field on `Config::default()`. New flags can be
/// added here as scenarios require them; production-internal
/// fields stay out of the test surface.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct ScenarioConfigOverrides {
    #[serde(default)]
    pub line_wrap: Option<bool>,
    #[serde(default)]
    pub wrap_indent: Option<bool>,
    #[serde(default)]
    pub show_horizontal_scrollbar: Option<bool>,
    #[serde(default)]
    pub show_vertical_scrollbar: Option<bool>,
}

impl ScenarioConfigOverrides {
    /// True when at least one override is set.
    pub fn is_some(&self) -> bool {
        self.line_wrap.is_some()
            || self.wrap_indent.is_some()
            || self.show_horizontal_scrollbar.is_some()
            || self.show_vertical_scrollbar.is_some()
    }

    /// Apply this struct's overrides on top of a default Config.
    pub fn into_config(self) -> fresh::config::Config {
        let mut config = fresh::config::Config::default();
        if let Some(v) = self.line_wrap {
            config.editor.line_wrap = v;
        }
        if let Some(v) = self.wrap_indent {
            config.editor.wrap_indent = v;
        }
        if let Some(v) = self.show_horizontal_scrollbar {
            config.editor.show_horizontal_scrollbar = v;
        }
        if let Some(v) = self.show_vertical_scrollbar {
            config.editor.show_vertical_scrollbar = v;
        }
        config
    }
}

pub fn check_layout_scenario(s: LayoutScenario) -> Result<(), ScenarioFailure> {
    let width = if s.width == 0 { 80 } else { s.width };
    let height = if s.height == 0 { 24 } else { s.height };

    let effective_config: Option<fresh::config::Config> = match s.config.clone() {
        Some(cfg) => Some(cfg),
        None if s.config_overrides.is_some() => Some(s.config_overrides.clone().into_config()),
        None => None,
    };
    let mut harness = match effective_config {
        Some(cfg) => EditorTestHarness::with_config(width, height, cfg)
            .expect("EditorTestHarness::with_config failed"),
        None => EditorTestHarness::with_temp_project(width, height)
            .expect("EditorTestHarness::with_temp_project failed"),
    };
    // Composite-buffer scenarios build their own buffer set; the
    // `initial_text` / `initial_file` paths are skipped.
    let composite_handle: Option<usize> = if let Some(spec) = &s.composite_buffer {
        let handle = harness.api_mut().create_side_by_side_diff(
            &spec.name,
            &spec.mode,
            &spec.old_content,
            &spec.new_content,
            &spec.hunks,
        );
        if let Some(hunk) = spec.initial_focus_hunk {
            harness
                .api_mut()
                .set_composite_initial_focus_hunk_on(handle, hunk);
        }
        if !spec.skip_initial_render {
            harness
                .render()
                .expect("composite initial render failed");
        }
        Some(handle)
    } else {
        if let Some(path) = &s.initial_file {
            harness.open_file(path).expect("open_file failed");
        } else {
            let _fixture = harness
                .load_buffer_from_text(&s.initial_text)
                .expect("load_buffer_from_text failed");
        }
        harness.render().expect("initial render failed");
        None
    };

    // Declarative virtual-text seeding. Runs before any action /
    // event dispatch so the lines are present in the editor state
    // for the full action sequence.
    for spec in &s.initial_virtual_texts {
        let placement = match spec.position {
            VirtualTextPositionSpec::Above => "above",
            VirtualTextPositionSpec::Below => "below",
            VirtualTextPositionSpec::Inline => {
                return Err(ScenarioFailure::InputProjectionFailed {
                    description: s.description.clone(),
                    reason: "VirtualTextPositionSpec::Inline is reserved; seed shim does not wire it yet".into(),
                });
            }
        };
        harness.api_mut().seed_virtual_line(
            spec.byte_offset,
            &spec.text,
            spec.fg,
            spec.bg,
            placement,
            &spec.namespace,
            spec.priority,
        );
    }

    // Declarative margin-annotation seeding. Mirrors
    // `Event::AddMarginAnnotation` exactly.
    for spec in &s.initial_margin_annotations {
        harness.api_mut().add_margin_annotation(
            spec.line,
            &spec.position,
            &spec.symbol,
            spec.color,
            spec.annotation_id.as_deref(),
        );
    }

    // Determine whether per-row text inspection is needed anywhere
    // in the scenario (final expectation or any step expectation).
    // Any matcher that reads `rendered_rows` / `buffer_text` forces
    // the slower `extract_with_rendered_rows` path.
    let expect_needs_rows = |e: &RenderSnapshotExpect| {
        !e.row_checks.is_empty()
            || e.cursor_cell_matches_buffer_char
            || e.popup_hanging_indent.is_some()
    };
    let needs_rows = expect_needs_rows(&s.expected_snapshot)
        || s.step_assertions
            .iter()
            .any(|sa| expect_needs_rows(&sa.expect));

    let extract_snapshot = |h: &mut EditorTestHarness| -> RenderSnapshot {
        if needs_rows {
            RenderSnapshot::extract_with_rendered_rows(h)
        } else {
            RenderSnapshot::extract(h)
        }
    };

    // Per-step assertions: dispatch up to and including
    // `after_action_index`, render, and check `expect`. Steps are
    // applied in their original order; after the last step we
    // continue dispatching any remaining actions for the final
    // assertion. Action index is checkpointed across steps so we
    // never re-dispatch.
    let mut dispatched_up_to: usize = 0; // exclusive upper bound
    let mut step_assertions = s.step_assertions.clone();
    step_assertions.sort_by_key(|sa| sa.after_action_index);

    let mut top_byte_observations: Vec<usize> = Vec::new();

    for step in &step_assertions {
        let want_inclusive = step.after_action_index + 1;
        assert!(
            want_inclusive <= s.actions.len(),
            "step_assertion.after_action_index {} is out of range (actions.len() = {})",
            step.after_action_index,
            s.actions.len()
        );
        if want_inclusive > dispatched_up_to {
            let slice = &s.actions[dispatched_up_to..want_inclusive];
            {
                let api: &mut dyn EditorTestApi = harness.api_mut();
                api.dispatch_seq(slice);
            }
            harness.render().expect("render between step assertions failed");
            dispatched_up_to = want_inclusive;
        }
        let snapshot = extract_snapshot(&mut harness);
        top_byte_observations.push(snapshot.viewport.top_byte);
        if let Some((field, expected, actual)) = step.expect.check_against(&snapshot) {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: format!(
                    "{} [step after_action_index={}]",
                    s.description, step.after_action_index
                ),
                field: field.to_string(),
                expected,
                actual,
            });
        }
    }

    // Dispatch the remaining actions (if any) for the final assertion.
    if dispatched_up_to < s.actions.len() {
        let remaining = &s.actions[dispatched_up_to..];
        let api: &mut dyn EditorTestApi = harness.api_mut();
        api.dispatch_seq(remaining);
    }

    // Dispatch declarative input events (mouse, hunk-nav, etc.)
    // after the Action sequence. Each event is translated to the
    // editor's real input path. Recorded-rows slots (set by
    // `RecordRenderedRows`, asserted by `AssertRenderedRowsMatch`)
    // live in this map so two events can refer to the same slot.
    let mut recorded_rows: std::collections::HashMap<u32, Vec<String>> =
        std::collections::HashMap::new();
    for ev in &s.events {
        dispatch_layout_event(
            &mut harness,
            ev,
            &s.description,
            composite_handle,
            &mut recorded_rows,
        )?;
    }

    // Dispatch declarative mouse drags. Symbolic variants are
    // resolved against the harness's current content-area
    // geometry, so scenario data doesn't have to know layout
    // internals (status bar height, etc.).
    for drag in &s.mouse_drags {
        let (from_col, from_row, to_col, to_row) = match drag {
            MouseDragSpec::Cells {
                from_col,
                from_row,
                to_col,
                to_row,
            } => (*from_col, *from_row, *to_col, *to_row),
            MouseDragSpec::VerticalScrollbarFullRange => {
                let scrollbar_col = width.saturating_sub(1);
                let (first, last) = harness.content_area_rows();
                (scrollbar_col, first as u16, scrollbar_col, last as u16)
            }
        };
        harness
            .mouse_drag(from_col, from_row, to_col, to_row)
            .map_err(|e| ScenarioFailure::InputProjectionFailed {
                description: s.description.clone(),
                reason: format!("mouse_drag({from_col},{from_row})→({to_col},{to_row}): {e}"),
            })?;
    }

    // Declarative virtual-text namespace clears, applied after
    // actions / events have run.
    for ns in &s.clear_virtual_text_namespaces {
        harness.api_mut().clear_virtual_text_namespace(ns);
    }

    // Declarative margin-annotation removals.
    for id in &s.remove_margin_annotations {
        harness.api_mut().remove_margin_annotation(id);
    }

    // Inject any declarative popup before the final render.
    if let Some(popup) = &s.show_popup {
        use fresh::model::event::{
            Event, PopupContentData, PopupData, PopupKindHint, PopupPositionData,
        };
        // Resolve the declarative `PopupPlacement` to a
        // `PopupPositionData` the editor event accepts.
        //
        // `AtHardwareCursorOffset` needs the cursor's TERMINAL-
        // absolute screen position (the same coordinate system
        // `PopupPosition::Fixed { x, y }` consumes), not the
        // viewport-relative `(col, row)` that
        // `EditorTestApi::hardware_cursor_position` returns.
        // Run a real render first so vt100 sees the post-action
        // frame, then read `vt100_cursor_position()` — that's the
        // exact cell the user's real terminal would put the cursor
        // on. Test data therefore doesn't need to hard-code cell
        // coordinates that depend on gutter width, menu-bar
        // height, or other chrome.
        let position = match &popup.position {
            PopupPlacement::Centered => PopupPositionData::Centered,
            PopupPlacement::Fixed { x, y } => PopupPositionData::Fixed { x: *x, y: *y },
            PopupPlacement::AtHardwareCursorOffset { dx, dy } => {
                // Resolve to the cursor's TERMINAL-absolute screen
                // position (the same coordinate system
                // `PopupPosition::Fixed { x, y }` consumes) via the
                // harness's sentinel-trick render: it runs
                // `Terminal::draw` and reports where ratatui placed
                // the cursor (or `None` if the editor hid it).
                // Falls back to `Centered` if the cursor was
                // hidden.
                match harness.render_observing_cursor() {
                    Ok(Some((cx, cy))) => {
                        let x = (cx as i32 + dx).max(0) as u16;
                        let y = (cy as i32 + dy).max(0) as u16;
                        PopupPositionData::Fixed { x, y }
                    }
                    _ => PopupPositionData::Centered,
                }
            }
        };
        harness
            .apply_event(Event::ShowPopup {
                popup: PopupData {
                    kind: PopupKindHint::Text,
                    title: popup.title.clone(),
                    description: None,
                    transient: false,
                    content: PopupContentData::Text(popup.lines.clone()),
                    position,
                    width: popup.width,
                    max_height: popup.max_height,
                    bordered: popup.bordered,
                },
            })
            .expect("apply_event(ShowPopup) failed");
    }

    harness.render().expect("final render failed");

    if let Some(want) = s.expected_top_byte {
        let actual = harness.api_mut().viewport_top_byte();
        if actual != want {
            return Err(ScenarioFailure::ViewportTopByteMismatch {
                description: s.description,
                expected: want,
                actual,
            });
        }
    }

    let snapshot = extract_snapshot(&mut harness);
    // Note: only step-assertion snapshots feed into the
    // `viewport_top_byte_distinct_at_most` count, not the final.

    if let Some(max_distinct) = s.viewport_top_byte_distinct_at_most {
        let mut sorted = top_byte_observations.clone();
        sorted.sort();
        sorted.dedup();
        if sorted.len() > max_distinct {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description.clone(),
                field: "viewport_top_byte_distinct_at_most".into(),
                expected: format!("<= {max_distinct} distinct value(s)"),
                actual: format!(
                    "{} distinct value(s): {:?}",
                    sorted.len(),
                    sorted
                ),
            });
        }
    }

    if let Some((field, expected, actual)) = s.expected_snapshot.check_against(&snapshot) {
        return Err(ScenarioFailure::SnapshotFieldMismatch {
            description: s.description,
            field: field.to_string(),
            expected,
            actual,
        });
    }

    if let Some(want) = s.expected_full_redraw_requested {
        let actual = harness.api_mut().take_full_redraw_request_for_tests();
        if actual != want {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description,
                field: "full_redraw_requested".into(),
                expected: want.to_string(),
                actual: actual.to_string(),
            });
        }
    }

    if let Some(want_consumed) = s.expected_initial_focus_hunk_consumed {
        let handle = composite_handle.ok_or_else(|| ScenarioFailure::SnapshotFieldMismatch {
            description: s.description.clone(),
            field: "expected_initial_focus_hunk_consumed".into(),
            expected: format!("composite_buffer to be set, consumed={want_consumed}"),
            actual: "composite_buffer was None".into(),
        })?;
        let actual = harness.api_mut().composite_initial_focus_hunk_on(handle);
        // consumed = true ⇒ initial_focus_hunk should now be None.
        let actually_consumed = actual.is_none();
        if actually_consumed != want_consumed {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description.clone(),
                field: "initial_focus_hunk_consumed".into(),
                expected: want_consumed.to_string(),
                actual: format!(
                    "consumed={actually_consumed} (initial_focus_hunk = {actual:?})"
                ),
            });
        }
    }

    if let Some(col) = s.expected_scrollbar_at_column {
        if !harness.has_scrollbar_at_column(col) {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description.clone(),
                field: "scrollbar_at_column".into(),
                expected: format!("scrollbar present at col {col}"),
                actual: "no scrollbar at that column".into(),
            });
        }
    }

    // Horizontal scrollbar visibility: probe the natural slots
    // (last content row, or the row below it). True ⇒ at least
    // one cell in those rows carries a scrollbar style.
    if let Some(want) = s.expected_horizontal_scrollbar_visible {
        let found = horizontal_scrollbar_visible(&harness);
        if want != found {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description.clone(),
                field: "horizontal_scrollbar_visible".into(),
                expected: want.to_string(),
                actual: found.to_string(),
            });
        }
    }
    if let Some(want_absent) = s.expected_no_horizontal_scrollbar_on_last_content_row {
        let (_, last_content_row) = harness.content_area_rows();
        let found = has_scrollbar_at_row(&harness, last_content_row as u16);
        if want_absent && found {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description.clone(),
                field: "no_horizontal_scrollbar_on_last_content_row".into(),
                expected: "no scrollbar on last content row".into(),
                actual: format!("scrollbar present on row {last_content_row}"),
            });
        }
    }

    if let Some(want) = &s.expected_status_message {
        let actual = harness.api_mut().status_message();
        if actual.as_deref() != Some(want.as_str()) {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description.clone(),
                field: "status_message".into(),
                expected: format!("{want:?}"),
                actual: format!("{actual:?}"),
            });
        }
    }

    if let Some(offset) = s.expected_cursor_col_equals_margin_plus {
        let gutter = harness.api_mut().margin_left_total_width() as u16;
        let cursor = harness.api_mut().hardware_cursor_position();
        let expected_col = gutter + offset;
        match cursor {
            Some((col, _)) if col == expected_col => {}
            other => {
                return Err(ScenarioFailure::SnapshotFieldMismatch {
                    description: s.description.clone(),
                    field: "cursor_col_equals_margin_plus".into(),
                    expected: format!("col {expected_col} (gutter {gutter} + {offset})"),
                    actual: format!("{other:?}"),
                });
            }
        }
    }

    if s.expected_cursor_row_equals_content_first {
        let (first, _) = harness.content_area_rows();
        let cursor = harness.api_mut().hardware_cursor_position();
        match cursor {
            Some((_, row)) if row as usize == first => {}
            other => {
                return Err(ScenarioFailure::SnapshotFieldMismatch {
                    description: s.description.clone(),
                    field: "cursor_row_equals_content_first".into(),
                    expected: format!("row {first}"),
                    actual: format!("{other:?}"),
                });
            }
        }
    }

    for substring in &s.expected_virtual_rows_no_digit_gutter {
        let matching_rows: Vec<&String> = snapshot
            .rendered_rows
            .iter()
            .filter(|r| r.contains(substring.as_str()))
            .collect();
        if matching_rows.is_empty() {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description.clone(),
                field: "virtual_rows_no_digit_gutter".into(),
                expected: format!("at least one row containing {substring:?}"),
                actual: "no row contained it".into(),
            });
        }
        for line in matching_rows {
            if line
                .trim_start()
                .starts_with(|c: char| c.is_ascii_digit())
            {
                return Err(ScenarioFailure::SnapshotFieldMismatch {
                    description: s.description.clone(),
                    field: "virtual_rows_no_digit_gutter".into(),
                    expected: format!(
                        "row containing {substring:?} does not start with digit"
                    ),
                    actual: format!("row starts with digit: {line:?}"),
                });
            }
        }
    }

    for (before, after) in &s.expected_row_order {
        let before_idx = snapshot
            .rendered_rows
            .iter()
            .position(|r| r.contains(before.as_str()));
        let after_idx = snapshot
            .rendered_rows
            .iter()
            .position(|r| r.contains(after.as_str()));
        match (before_idx, after_idx) {
            (Some(b), Some(a)) if b < a => {}
            (b, a) => {
                return Err(ScenarioFailure::SnapshotFieldMismatch {
                    description: s.description.clone(),
                    field: "row_order".into(),
                    expected: format!("row({before:?}) < row({after:?})"),
                    actual: format!("before={b:?}, after={a:?}"),
                });
            }
        }
    }

    if let Some(want) = s.expected_virtual_text_count {
        let actual = harness.api_mut().virtual_text_count();
        if actual != want {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description.clone(),
                field: "virtual_text_count".into(),
                expected: want.to_string(),
                actual: actual.to_string(),
            });
        }
    }

    Ok(())
}

/// True if any cell at row `y` carries a scrollbar style (thumb or track).
fn has_scrollbar_at_row(harness: &EditorTestHarness, row: u16) -> bool {
    let buffer = harness.buffer();
    let width = buffer.area.width;
    (0..width).any(|col| {
        harness.is_scrollbar_thumb_at(col, row) || harness.is_scrollbar_track_at(col, row)
    })
}

/// True if the horizontal scrollbar's natural slot (last content
/// row or the row below it) carries any scrollbar cells.
fn horizontal_scrollbar_visible(harness: &EditorTestHarness) -> bool {
    let (_, last_content_row) = harness.content_area_rows();
    has_scrollbar_at_row(harness, last_content_row as u16)
        || has_scrollbar_at_row(harness, (last_content_row + 1) as u16)
}

/// Translate a high-level `InputEvent` into the editor's input
/// path. Only the variants actually exercised by `LayoutScenario`
/// today are wired; other variants return an
/// `InputProjectionFailed` failure so a typo in test data fails
/// loudly rather than silently no-oping.
fn dispatch_layout_event(
    harness: &mut EditorTestHarness,
    ev: &InputEvent,
    description: &str,
    composite_handle: Option<usize>,
    recorded_rows: &mut std::collections::HashMap<u32, Vec<String>>,
) -> Result<(), ScenarioFailure> {
    use crate::common::scenario::buffer_scenario::{key_mods_to_crossterm, key_spec_to_crossterm};
    use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
    match ev {
        InputEvent::Action(a) => {
            harness.api_mut().dispatch(a.clone());
            harness.render().expect("render after Action event failed");
            Ok(())
        }
        InputEvent::SendKey { code, modifiers } => {
            let cc = key_spec_to_crossterm(*code);
            let mm = key_mods_to_crossterm(*modifiers);
            harness
                .send_key(cc, mm)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("send_key({code:?}, {modifiers:?}): {e}"),
                })
        }
        InputEvent::CompositeNextHunk { count } => {
            let handle = composite_handle.ok_or_else(|| ScenarioFailure::InputProjectionFailed {
                description: description.into(),
                reason: "CompositeNextHunk requires composite_buffer to be set".into(),
            })?;
            for _ in 0..*count {
                harness.api_mut().composite_next_hunk_active_on(handle);
            }
            harness
                .render()
                .expect("render after CompositeNextHunk failed");
            Ok(())
        }
        InputEvent::CompositePrevHunk { count } => {
            let handle = composite_handle.ok_or_else(|| ScenarioFailure::InputProjectionFailed {
                description: description.into(),
                reason: "CompositePrevHunk requires composite_buffer to be set".into(),
            })?;
            for _ in 0..*count {
                harness.api_mut().composite_prev_hunk_active_on(handle);
            }
            harness
                .render()
                .expect("render after CompositePrevHunk failed");
            Ok(())
        }
        InputEvent::FlushLayout => {
            harness.api_mut().flush_layout_for_tests();
            Ok(())
        }
        InputEvent::SleepMs(ms) => {
            std::thread::sleep(std::time::Duration::from_millis(*ms));
            Ok(())
        }
        InputEvent::RecordRenderedRows { slot } => {
            let snap = crate::common::scenario::render_snapshot::RenderSnapshot::extract_with_rendered_rows(harness);
            recorded_rows.insert(*slot, snap.rendered_rows);
            Ok(())
        }
        InputEvent::AssertRenderedRowsMatch { slot } => {
            let snap = crate::common::scenario::render_snapshot::RenderSnapshot::extract_with_rendered_rows(harness);
            let want = recorded_rows.get(slot).ok_or_else(|| {
                ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("AssertRenderedRowsMatch: slot {slot} was not recorded"),
                }
            })?;
            if &snap.rendered_rows != want {
                return Err(ScenarioFailure::SnapshotFieldMismatch {
                    description: description.into(),
                    field: format!("rendered_rows_match[slot={slot}]"),
                    expected: format!("{} recorded rows", want.len()),
                    actual: format!(
                        "actual rows differ; first divergent: {:?} vs {:?}",
                        snap.rendered_rows.iter().zip(want.iter()).enumerate()
                            .find(|(_, (a, b))| a != b)
                            .map(|(i, (a, _))| (i, a.clone())),
                        snap.rendered_rows.iter().zip(want.iter()).enumerate()
                            .find(|(_, (a, b))| a != b)
                            .map(|(i, (_, b))| (i, b.clone())),
                    ),
                });
            }
            Ok(())
        }
        InputEvent::Mouse(CtxMouseEvent::Click { row, col, button }) => {
            let xbutton = match button {
                CtxMouseButton::Left => MouseButton::Left,
                CtxMouseButton::Right => MouseButton::Right,
                CtxMouseButton::Middle => MouseButton::Middle,
            };
            let down = MouseEvent {
                kind: MouseEventKind::Down(xbutton),
                column: *col,
                row: *row,
                modifiers: KeyModifiers::empty(),
            };
            harness
                .send_mouse(down)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("mouse Down: {e}"),
                })?;
            let up = MouseEvent {
                kind: MouseEventKind::Up(xbutton),
                column: *col,
                row: *row,
                modifiers: KeyModifiers::empty(),
            };
            harness
                .send_mouse(up)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("mouse Up: {e}"),
                })?;
            harness.render().expect("render after click failed");
            Ok(())
        }
        InputEvent::Mouse(CtxMouseEvent::Drag {
            from_row,
            from_col,
            to_row,
            to_col,
            button,
        }) => {
            let xbutton = match button {
                CtxMouseButton::Left => MouseButton::Left,
                CtxMouseButton::Right => MouseButton::Right,
                CtxMouseButton::Middle => MouseButton::Middle,
            };
            let down = MouseEvent {
                kind: MouseEventKind::Down(xbutton),
                column: *from_col,
                row: *from_row,
                modifiers: KeyModifiers::empty(),
            };
            harness
                .send_mouse(down)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("drag Down: {e}"),
                })?;
            // Interpolate intermediate drag positions, matching
            // EditorTestHarness::mouse_drag's semantics so test
            // behavior stays equivalent.
            let steps = ((*to_row as i32 - *from_row as i32).abs())
                .max((*to_col as i32 - *from_col as i32).abs())
                .max(1);
            for i in 1..=steps {
                let t = i as f32 / steps as f32;
                let col = *from_col as f32 + (*to_col as f32 - *from_col as f32) * t;
                let row = *from_row as f32 + (*to_row as f32 - *from_row as f32) * t;
                let drag = MouseEvent {
                    kind: MouseEventKind::Drag(xbutton),
                    column: col as u16,
                    row: row as u16,
                    modifiers: KeyModifiers::empty(),
                };
                harness.send_mouse(drag).map_err(|e| {
                    ScenarioFailure::InputProjectionFailed {
                        description: description.into(),
                        reason: format!("drag step: {e}"),
                    }
                })?;
            }
            let up = MouseEvent {
                kind: MouseEventKind::Up(xbutton),
                column: *to_col,
                row: *to_row,
                modifiers: KeyModifiers::empty(),
            };
            harness
                .send_mouse(up)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("drag Up: {e}"),
                })?;
            harness.render().expect("render after drag failed");
            Ok(())
        }
        InputEvent::Mouse(CtxMouseEvent::Wheel { row, col, dy }) => {
            // Negative dy = scroll down (content moves up); positive
            // dy = scroll up. Matches the convention in
            // EditorTestHarness::mouse_scroll_down/up where each
            // call advances one wheel notch.
            let kind = if *dy < 0 {
                MouseEventKind::ScrollDown
            } else {
                MouseEventKind::ScrollUp
            };
            let event = MouseEvent {
                kind,
                column: *col,
                row: *row,
                modifiers: KeyModifiers::empty(),
            };
            harness
                .send_mouse(event)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("wheel: {e}"),
                })?;
            harness.render().expect("render after wheel failed");
            Ok(())
        }
        other => Err(ScenarioFailure::InputProjectionFailed {
            description: description.into(),
            reason: format!("LayoutScenario does not handle {other:?} — extend the runner if a scenario needs it"),
        }),
    }
}

pub fn assert_layout_scenario(s: LayoutScenario) {
    if let Err(f) = check_layout_scenario(s) {
        panic!("{f}");
    }
}
