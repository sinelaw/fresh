//! `StyleScenario` — `StyledFrame` (cell-role × theme) assertions.
//!
//! **Status:** skeleton. Data shape is real and serialises into
//! the corpus today, but the runner panics with the blocker
//! message below until the production hook lands.
//!
//! ## What's needed to migrate tests to this scenario type
//!
//! 1. **Extract `style()` from `render()`.** The current
//!    renderer collapses layout + style + emit into a single
//!    `terminal.draw` call. Split into three named functions:
//!      - `layout(state, dim) -> RenderSnapshot` (already
//!        partly done — the `RenderSnapshot` type exists),
//!      - `style(snapshot, theme, roles) -> StyledFrame`
//!        ‹this is the new one this scenario needs›,
//!      - `emit(frame, caps) -> AnsiStream` (today's body).
//!
//!    Production stays the composition; tests call `style()`
//!    independently. Estimated cost: 2-3 days in `src/view/`.
//!
//! 2. **`EditorTestApi::styled_frame(theme: &Theme) -> StyledFrame`
//!    accessor.** Test-only, gated by
//!    `#[cfg(any(test, feature = "test-api"))]`.
//!
//! 3. **`Inspect`-driven assertion.** The runner uses the
//!    `Inspect` enum already defined here to scope the
//!    assertion (`Cell { row, col }`, `Row { row }`,
//!    `Column { col }`, `Region { ... }`, `FullFrame`).
//!
//! ## E2e tests this would unblock (~17)
//!
//! - `tests/e2e/theme.rs`, `theme_screenshots.rs`
//! - `cursor_style_rendering.rs`, `crlf_rendering.rs`
//! - `syntax_highlighting_coverage.rs`, `syntax_highlighting_embedded_offset.rs`
//! - `syntax_language_case.rs`, `glob_language_detection.rs`
//! - `config_language_selector.rs`, `csharp_language_coherence.rs`
//! - `warning_indicators.rs`, `blog_showcases.rs`
//! - `issue_1554_scrollbar_theme_color.rs`, `issue_1577_unicode_width.rs`
//! - `issue_1598_shebang_detection.rs`, `issue_779_after_eof_shade.rs`
//! - `visual_regression.rs`
//!
//! Many of those today rely on byte-exact PNG/snapshot
//! comparison; migrating to `StyleScenario` makes diffs
//! structural (cell `(x,y)` changed role from `Selection` to
//! `Normal`, fg `#abc` to `#def`).

use crate::common::scenario::context::ThemeRef;
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::StyledFrame;

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct StyleScenario {
    pub description: String,
    pub initial_text: String,
    #[serde(default)]
    pub theme: ThemeRef,
    pub events: Vec<InputEvent>,
    /// Cells the scenario asserts on. Use [`Inspect`] to scope.
    pub inspect: Inspect,
    pub expected: StyledFrame,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[derive(Default)]
pub enum Inspect {
    Cell {
        row: u16,
        col: u16,
    },
    Row {
        row: u16,
    },
    Column {
        col: u16,
    },
    Region {
        row: u16,
        col: u16,
        rows: u16,
        cols: u16,
    },
    #[default]
    FullFrame,
}

pub fn check_style_scenario(_s: StyleScenario) -> Result<(), ScenarioFailure> {
    Err(ScenarioFailure::InputProjectionFailed {
        description: "StyleScenario".into(),
        reason: "Phase 4 not yet implemented: needs `style()` extracted from `render()` \
            so the cell-role × theme projection is invocable from tests in isolation. \
            See the file-level docs at the top of `style_scenario.rs` for the full \
            prereq list."
            .into(),
    })
}

pub fn assert_style_scenario(s: StyleScenario) {
    if let Err(f) = check_style_scenario(s) {
        panic!("{f}");
    }
}
