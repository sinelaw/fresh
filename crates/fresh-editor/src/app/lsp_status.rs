//! Compose the LSP segment of the status bar.
//!
//! Pure function, no `Editor` access. Takes only the LSP state it reads
//! (progress map, per-server statuses, configured servers, the user's
//! dismissed-language set) and the current buffer's language. Returns the
//! rendered text plus an indicator state that the status-bar theme code
//! consumes.
//!
//! Isolated here (rather than on `Editor`) so it can be unit-tested without
//! a harness — see `docs/internal/editor-modules-refactor-plan.md` phase 1.

use std::collections::{HashMap, HashSet};

use super::types::LspProgressInfo;
use crate::services::async_bridge::LspServerStatus;
use crate::types::LspLanguageConfig;
use crate::view::ui::status_bar::LspIndicatorState;

/// Width of "LSP (error)" — the widest non-empty value we ever render.
///
/// Every other non-empty state is padded out to this width (with the text
/// centered) so the indicator never changes size between states. That in
/// turn keeps every other element on the status bar from shifting sideways
/// when the LSP comes up, goes into progress, or errors out.
pub(crate) const INDICATOR_WIDTH: usize = 11;

/// Pad `s` to exactly `INDICATOR_WIDTH` display cells, splitting the slack
/// evenly on both sides (extra cell goes on the right when the remainder
/// is odd, matching the usual "visual center" of a fixed pill).
fn centered(s: &str) -> String {
    let w = unicode_width::UnicodeWidthStr::width(s);
    if w >= INDICATOR_WIDTH {
        return s.to_string();
    }
    let slack = INDICATOR_WIDTH - w;
    let left = slack / 2;
    let right = slack - left;
    let mut out = String::with_capacity(INDICATOR_WIDTH);
    for _ in 0..left {
        out.push(' ');
    }
    out.push_str(s);
    for _ in 0..right {
        out.push(' ');
    }
    out
}

/// Compose the LSP segment of the status bar for a given buffer language.
///
/// Returns (text, indicator-state).  The state drives the indicator's color
/// in `status_bar::element_style`; the text is what's rendered inside the
/// segment.  Priority:
///
///   1. Progress       — detailed progress string, state = On
///   2. Error          — "LSP (error)",            state = Error
///   3. Running        — "LSP (on)",               state = On
///   4. Configured-but-not-running (either auto_start or opt-in dormant)
///                     — "LSP (off)",              state = Off / OffDismissed
///   5. Nothing        — empty,                    state = None
pub(crate) fn compose_lsp_status(
    current_language: &str,
    lsp_progress: &HashMap<String, LspProgressInfo>,
    lsp_server_statuses: &HashMap<(String, String), LspServerStatus>,
    lsp_config: &HashMap<String, LspLanguageConfig>,
    user_dismissed_languages: &HashSet<String>,
) -> (String, LspIndicatorState) {
    // 1. Progress for this language takes precedence.  We intentionally do
    //    NOT render the progress title/message/percent inline on the status
    //    bar: those strings grow and shrink wildly during indexing (e.g.
    //    rust-analyzer alternates between a 5-char "Roots" message and a
    //    60-char file path) and the indicator width would twitch every few
    //    hundred milliseconds.  Instead, show a stable "LSP " plus a 1-cell
    //    Braille spinner advanced by wall-clock time.  The popup surfaces
    //    the live progress text (see `show_lsp_status_popup`).
    if lsp_progress
        .values()
        .any(|info| info.language == current_language)
    {
        const SPINNER: &[char] = &['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
        // ~100ms per frame.  Using SystemTime (not Instant) keeps this a
        // pure function of "now" — tests that control wall-clock time can
        // drive it deterministically if ever needed, and we don't need a
        // tick counter threaded through the app.
        let idx = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| (d.as_millis() / 100) as usize)
            .unwrap_or(0)
            % SPINNER.len();
        return (
            centered(&format!("LSP {}", SPINNER[idx])),
            LspIndicatorState::On,
        );
    }

    // 2. Any server in Error state for this language wins over "running",
    //    so the indicator surfaces trouble even when another server is fine.
    let has_error = lsp_server_statuses
        .iter()
        .any(|((lang, _), status)| lang == current_language && *status == LspServerStatus::Error);
    if has_error {
        return (centered("LSP (error)"), LspIndicatorState::Error);
    }

    // 3. At least one running (non-Shutdown) server for this language.
    //    Starting/Initializing also counts as "on" — the user has opted in
    //    and it's making progress.
    let has_running = lsp_server_statuses.iter().any(|((lang, _), status)| {
        lang == current_language && !matches!(status, LspServerStatus::Shutdown)
    });
    if has_running {
        return (centered("LSP (on)"), LspIndicatorState::On);
    }

    // 4. No running server — surface any configured server (auto_start or
    //    opt-in, doesn't matter for the indicator) so the user can see an
    //    LSP is available and open the popup to start it.
    let configured_count = lsp_config
        .get(current_language)
        .map(|cfg| {
            cfg.as_slice()
                .iter()
                .filter(|c| c.enabled && !c.command.is_empty())
                .count()
        })
        .unwrap_or(0);
    if configured_count > 0 {
        // User-dismissed languages keep the same `LSP (off)` text — only the
        // style changes (handled by `element_style` via the `OffDismissed`
        // variant).  We deliberately keep the pill visible rather than
        // hiding it, so the user retains a discoverable surface to re-enable
        // or view install help.
        let state = if user_dismissed_languages.contains(current_language) {
            LspIndicatorState::OffDismissed
        } else {
            LspIndicatorState::Off
        };
        return (centered("LSP (off)"), state);
    }

    // 5. Nothing configured and nothing running — no indicator.
    (String::new(), LspIndicatorState::None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::LspServerConfig;

    fn progress_for(lang: &str) -> HashMap<String, LspProgressInfo> {
        let mut m = HashMap::new();
        m.insert(
            "tok-1".to_string(),
            LspProgressInfo {
                language: lang.to_string(),
                title: "indexing".to_string(),
                message: None,
                percentage: Some(42),
            },
        );
        m
    }

    fn status(
        lang: &str,
        server: &str,
        s: LspServerStatus,
    ) -> HashMap<(String, String), LspServerStatus> {
        let mut m = HashMap::new();
        m.insert((lang.to_string(), server.to_string()), s);
        m
    }

    fn configured_for(lang: &str, command: &str) -> HashMap<String, LspLanguageConfig> {
        let mut m = HashMap::new();
        let mut server = LspServerConfig::default();
        server.command = command.to_string();
        server.enabled = true;
        m.insert(
            lang.to_string(),
            LspLanguageConfig::Single(Box::new(server)),
        );
        m
    }

    #[test]
    fn empty_when_nothing_configured_or_running() {
        let (text, state) = compose_lsp_status(
            "rust",
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
            &HashSet::new(),
        );
        assert_eq!(text, "");
        assert_eq!(state, LspIndicatorState::None);
    }

    #[test]
    fn off_when_configured_but_not_running() {
        let (text, state) = compose_lsp_status(
            "rust",
            &HashMap::new(),
            &HashMap::new(),
            &configured_for("rust", "rust-analyzer"),
            &HashSet::new(),
        );
        assert!(text.contains("LSP (off)"));
        assert_eq!(state, LspIndicatorState::Off);
    }

    #[test]
    fn off_dismissed_when_user_dismissed_language() {
        let mut dismissed = HashSet::new();
        dismissed.insert("rust".to_string());
        let (text, state) = compose_lsp_status(
            "rust",
            &HashMap::new(),
            &HashMap::new(),
            &configured_for("rust", "rust-analyzer"),
            &dismissed,
        );
        assert!(text.contains("LSP (off)"));
        assert_eq!(state, LspIndicatorState::OffDismissed);
    }

    #[test]
    fn running_wins_over_off() {
        let statuses = status("rust", "rust-analyzer", LspServerStatus::Running);
        let (text, state) = compose_lsp_status(
            "rust",
            &HashMap::new(),
            &statuses,
            &configured_for("rust", "rust-analyzer"),
            &HashSet::new(),
        );
        assert!(text.contains("LSP (on)"));
        assert_eq!(state, LspIndicatorState::On);
    }

    #[test]
    fn error_wins_over_running() {
        let mut statuses = status("rust", "rust-analyzer", LspServerStatus::Running);
        statuses.insert(
            ("rust".to_string(), "clippy".to_string()),
            LspServerStatus::Error,
        );
        let (text, state) = compose_lsp_status(
            "rust",
            &HashMap::new(),
            &statuses,
            &HashMap::new(),
            &HashSet::new(),
        );
        assert!(text.contains("LSP (error)"));
        assert_eq!(state, LspIndicatorState::Error);
    }

    #[test]
    fn progress_wins_over_error() {
        // Progress takes precedence even over error, because progress
        // implies the server is still alive and doing work.
        let statuses = status("rust", "rust-analyzer", LspServerStatus::Error);
        let (text, state) = compose_lsp_status(
            "rust",
            &progress_for("rust"),
            &statuses,
            &HashMap::new(),
            &HashSet::new(),
        );
        assert!(text.contains("LSP"));
        assert_eq!(state, LspIndicatorState::On);
    }

    #[test]
    fn shutdown_server_does_not_count_as_running() {
        let statuses = status("rust", "rust-analyzer", LspServerStatus::Shutdown);
        let (text, state) = compose_lsp_status(
            "rust",
            &HashMap::new(),
            &statuses,
            &configured_for("rust", "rust-analyzer"),
            &HashSet::new(),
        );
        assert!(text.contains("LSP (off)"));
        assert_eq!(state, LspIndicatorState::Off);
    }

    #[test]
    fn unrelated_language_server_is_ignored() {
        let statuses = status("python", "pyright", LspServerStatus::Running);
        let (text, state) = compose_lsp_status(
            "rust",
            &HashMap::new(),
            &statuses,
            &HashMap::new(),
            &HashSet::new(),
        );
        assert_eq!(text, "");
        assert_eq!(state, LspIndicatorState::None);
    }

    #[test]
    fn indicator_text_is_padded_to_fixed_width() {
        // All non-empty indicator text is padded to INDICATOR_WIDTH so the
        // surrounding status-bar layout does not shift between states.
        let (off, _) = compose_lsp_status(
            "rust",
            &HashMap::new(),
            &HashMap::new(),
            &configured_for("rust", "rust-analyzer"),
            &HashSet::new(),
        );
        let (err, _) = compose_lsp_status(
            "rust",
            &HashMap::new(),
            &status("rust", "rust-analyzer", LspServerStatus::Error),
            &HashMap::new(),
            &HashSet::new(),
        );
        let off_w = unicode_width::UnicodeWidthStr::width(off.as_str());
        let err_w = unicode_width::UnicodeWidthStr::width(err.as_str());
        assert_eq!(off_w, INDICATOR_WIDTH);
        assert_eq!(err_w, INDICATOR_WIDTH);
    }
}
