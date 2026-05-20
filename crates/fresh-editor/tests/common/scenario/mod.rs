//! Scenario framework for editor tests.
//!
//! See `docs/internal/e2e-test-migration-design.md` for the design.
//!
//! Tests express claims as data: `(initial state, action sequence,
//! expected final state)`. A runner instantiates a headless editor,
//! applies the actions through `fresh::test_api::EditorTestApi`, and
//! asserts on the resulting state — no `terminal.draw`, no
//! `crossterm::KeyCode`, no screen scraping.
//!
//! Three drivers consume the same scenario value: the regression
//! runner (per-type `assert_*_scenario`), proptest generators
//! ([`property`]), and shadow-model differentials ([`shadow`]).

// ── Phase 1 (landed): pure-state scenarios ──────────────────────────
pub mod buffer_scenario;
pub mod failure;
pub mod property;
pub mod shadow;
pub mod trace_scenario;

// ── Composable architecture (this PR) ───────────────────────────────
pub mod context;
pub mod input_event;
pub mod key_dispatch;
pub mod observable;

// ── Phase 2: Layout (real, naive wrap shadow) ───────────────────────
pub mod layout_scenario;
pub mod layout_shadow;
pub mod render_snapshot;

// ── Phase 3: Modal (real-minimal, popup-based) ──────────────────────
pub mod modal_scenario;

// ── Phase 7: Workspace (real-minimal, multi-buffer) ─────────────────
pub mod workspace_scenario;

// ── Phase 9: Input (real-minimal, mouse projection) ─────────────────
pub mod input_scenario;

// ── Phase 10: Temporal (real, MockClock injection) ──────────────────
pub mod temporal_scenario;

// ── Marker-roundtrip (declarative migration of
//     undo_redo_marker_roundtrip e2e tests) ────────────────────────────
pub mod marker_roundtrip_scenario;

// ── Skeleton phases — types + JSON shape, runners panic with the
// concrete production hook the phase still needs. Each surviving
// skeleton has an inline TODO with the prerequisite.
//
// Phase 11 (PluginScenario) and Phase 12 (GuiScenario) were
// dropped: only ~5 plugin tests and 1 GUI test would target them,
// and their production hooks are heavy. See the design doc §12.
pub mod lsp_scenario;
pub mod persistence_scenario;
pub mod style_scenario;
pub mod terminal_io_scenario;
