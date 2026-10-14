//! `LspScenario` — scripted LSP exchange + buffer assertions.
//!
//! **Status:** skeleton. Data shape is real and serialises into
//! the corpus today, but the runner panics with the blocker
//! message below until the production hook lands.
//!
//! ## What's needed to migrate tests to this scenario type
//!
//! 1. **Transport seam at `LspManager`.** The current LSP
//!    integration spawns subprocesses and talks to them over
//!    stdin/stdout. Add a `LspTransport` trait the manager
//!    accepts; the production impl wraps the subprocess as
//!    today, the test impl is a scripted mock that:
//!      - matches expected outgoing JSON-RPC by method + shape,
//!      - injects pre-canned server replies on cue,
//!      - records all traffic for the `LspTraffic` observable.
//!
//!    Estimated cost: 1-2 days of work in `src/services/lsp/`.
//!
//! 2. **`EditorTestApi::lsp_traffic()` accessor.** Returns the
//!    recorded methods + notifications so the runner can assert
//!    on them. Test-only, gated by `#[cfg(any(test, feature =
//!    "test-api"))]`.
//!
//! 3. **Plug `LspScript` into harness construction.** When a
//!    scenario carries `Some(LspScript)`, the harness installs
//!    the scripted transport via `LspManager::with_transport`.
//!
//! Once those land, this skeleton's `check_lsp_scenario` body
//! becomes ~30 LOC: instantiate harness with scripted transport,
//! dispatch events, snap traffic + buffer, compare expectations.
//!
//! ## E2e tests this would unblock (~30)
//!
//! - `tests/e2e/lsp.rs` and 26 `lsp_*.rs` files
//! - `language_features_e2e.rs`, `universal_lsp.rs`
//! - `inline_diagnostics.rs`, `issue_1572_inlay_hint_drift`,
//!   `issue_1573_format_buffer`, `hot_exit_recovery_lsp_sync`
//!
//! Existing `tests/common/fake_lsp.rs` (2271 LOC, Bash-subprocess
//! based) is partial infrastructure — its capabilities can be
//! ported into the in-process scripted transport above.

use crate::common::scenario::context::LspScript;
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::LspTraffic;
use crate::common::scenario::property::BufferState;

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct LspScenario {
    pub description: String,
    pub initial_text: String,
    pub language: String,
    pub script: LspScript,
    pub events: Vec<InputEvent>,
    pub expected_buffer: BufferState,
    pub expected_traffic: LspTraffic,
}

pub fn check_lsp_scenario(_s: LspScenario) -> Result<(), ScenarioFailure> {
    Err(ScenarioFailure::InputProjectionFailed {
        description: "LspScenario".into(),
        reason: "Phase 5 not yet implemented: needs an `LspTransport` trait at the \
            `LspManager` boundary so a scripted in-process adapter can intercept \
            JSON-RPC. See the file-level docs at the top of `lsp_scenario.rs` \
            for the full prereq list."
            .into(),
    })
}

pub fn assert_lsp_scenario(s: LspScenario) {
    if let Err(f) = check_lsp_scenario(s) {
        panic!("{f}");
    }
}
