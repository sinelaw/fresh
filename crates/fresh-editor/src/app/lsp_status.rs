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
///   0. Buffer-level skip (large file, binary, per-buffer toggle)
///                     — "LSP (n/a)",              state = OffDismissed
///   1. Progress       — detailed progress string, state = On
///   2. Error          — "LSP (error)",            state = Error
///   3. Running        — "LSP (on)",               state = On
///   4. Configured-but-not-running (either auto_start or opt-in dormant)
///                     — "LSP (off)",              state = Off / OffDismissed
///   5. Nothing        — empty,                    state = None
///
/// The buffer-level skip only wins over running language state: showing
/// "LSP (on)" while LSP is actively skipping this buffer (e.g. rust-
/// analyzer running elsewhere, but this file exceeded the large-file
/// threshold) would be a lie. When no server is running, the language-
/// level "LSP (off)" pill is the more informative signal and takes over.
pub(crate) fn compose_lsp_status(
    current_language: &str,
    buffer_lsp_disabled_reason: Option<&str>,
    lsp_progress: &HashMap<String, LspProgressInfo>,
    lsp_server_statuses: &HashMap<(String, String), LspServerStatus>,
    lsp_config: &HashMap<String, LspLanguageConfig>,
    user_dismissed_languages: &HashSet<String>,
) -> (String, LspIndicatorState) {
    // 0. Per-buffer LSP skip — only flag it when it's a *mismatch* with
    //    language state: LSP is running for this language, but not for
    //    this buffer (large file, binary, per-buffer toggle). Without
    //    the running check, stopping the language-level server would
    //    also show "LSP (n/a)" since `handle_stop_lsp_server` marks
    //    every buffer disabled — but in that case the "LSP (off)" pill
    //    (configured, not running) is the more informative signal.
    let language_has_running_server = lsp_server_statuses.iter().any(|((lang, _), status)| {
        lang == current_language && !matches!(status, LspServerStatus::Shutdown)
    });
    if buffer_lsp_disabled_reason.is_some()
        && language_has_running_server
        && lsp_config
            .get(current_language)
            .is_some_and(|cfg| cfg.as_slice().iter().any(|c| !c.command.is_empty()))
    {
        return (centered("LSP (n/a)"), LspIndicatorState::OffDismissed);
    }
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

    // 4. No running server — surface any configured server so the user
    //    can see an LSP is available and open the popup to start it.
    //    Includes servers with `enabled = false`: picking "Disable LSP
    //    for <lang>" flips `enabled` off, and hiding the pill at that
    //    point would leave the user with no surface to re-enable
    //    later. The dimmed `OffDismissed` variant makes the disabled
    //    state visually distinct.
    let configured_count = lsp_config
        .get(current_language)
        .map(|cfg| {
            cfg.as_slice()
                .iter()
                .filter(|c| !c.command.is_empty())
                .count()
        })
        .unwrap_or(0);
    if configured_count > 0 {
        // User-dismissed languages keep the same `LSP (off)` text — only
        // the style changes (handled by `element_style` via the
        // `OffDismissed` variant). `enabled = false` on every configured
        // server is the persistent flavour of the same idea, so render
        // it the same way: pill stays visible but dimmed, so the user
        // has a discoverable surface to re-enable.
        let any_enabled = lsp_config
            .get(current_language)
            .is_some_and(|cfg| cfg.as_slice().iter().any(|c| c.enabled));
        let state = if !any_enabled || user_dismissed_languages.contains(current_language) {
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
            None,
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
            None,
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
            None,
            &HashMap::new(),
            &HashMap::new(),
            &configured_for("rust", "rust-analyzer"),
            &dismissed,
        );
        assert!(text.contains("LSP (off)"));
        assert_eq!(state, LspIndicatorState::OffDismissed);
    }

    /// After the user picks "Disable LSP for <lang>" the config flips
    /// to `enabled = false`. The pill must stay visible (dimmed) so
    /// the user still has a surface to re-enable; hiding it would
    /// strand the Enable action.
    #[test]
    fn off_dismissed_when_all_servers_disabled_in_config() {
        let mut config = HashMap::new();
        let mut server = LspServerConfig::default();
        server.command = "rust-analyzer".to_string();
        server.enabled = false;
        config.insert(
            "rust".to_string(),
            LspLanguageConfig::Single(Box::new(server)),
        );
        let (text, state) = compose_lsp_status(
            "rust",
            None,
            &HashMap::new(),
            &HashMap::new(),
            &config,
            &HashSet::new(),
        );
        assert!(
            text.contains("LSP (off)"),
            "pill should still render when the language has configured \
             servers, even if every one is enabled=false"
        );
        assert_eq!(
            state,
            LspIndicatorState::OffDismissed,
            "disabled-in-config renders as the dimmed OffDismissed variant, \
             matching the session-level dismissed flavour so the user can tell \
             their Disable action took effect"
        );
    }

    #[test]
    fn running_wins_over_off() {
        let statuses = status("rust", "rust-analyzer", LspServerStatus::Running);
        let (text, state) = compose_lsp_status(
            "rust",
            None,
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
            None,
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
            None,
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
            None,
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
            None,
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
            None,
            &HashMap::new(),
            &HashMap::new(),
            &configured_for("rust", "rust-analyzer"),
            &HashSet::new(),
        );
        let (err, _) = compose_lsp_status(
            "rust",
            None,
            &HashMap::new(),
            &status("rust", "rust-analyzer", LspServerStatus::Error),
            &HashMap::new(),
            &HashSet::new(),
        );
        let (na, _) = compose_lsp_status(
            "rust",
            Some("File too large"),
            &HashMap::new(),
            &status("rust", "rust-analyzer", LspServerStatus::Running),
            &configured_for("rust", "rust-analyzer"),
            &HashSet::new(),
        );
        let off_w = unicode_width::UnicodeWidthStr::width(off.as_str());
        let err_w = unicode_width::UnicodeWidthStr::width(err.as_str());
        let na_w = unicode_width::UnicodeWidthStr::width(na.as_str());
        assert_eq!(off_w, INDICATOR_WIDTH);
        assert_eq!(err_w, INDICATOR_WIDTH);
        assert_eq!(na_w, INDICATOR_WIDTH);
    }

    // When the focused buffer has LSP disabled per-buffer (large file,
    // binary, per-buffer toggle) the pill must visibly reflect that,
    // overriding the language-level state. Otherwise opening e.g. a
    // huge .rs file while rust-analyzer serves other buffers would
    // show "LSP (on)" — a lie for *this* buffer.
    #[test]
    fn buffer_disabled_wins_over_running_language_lsp() {
        let statuses = status("rust", "rust-analyzer", LspServerStatus::Running);
        let (text, state) = compose_lsp_status(
            "rust",
            Some("File too large (438726656 bytes)"),
            &HashMap::new(),
            &statuses,
            &configured_for("rust", "rust-analyzer"),
            &HashSet::new(),
        );
        assert!(
            text.contains("LSP (n/a)"),
            "expected distinct 'LSP (n/a)' pill for per-buffer skip, got {:?}",
            text
        );
        assert_eq!(state, LspIndicatorState::OffDismissed);
    }

    // The per-buffer pill only makes sense when the language has
    // LSP configured in the first place. For a virtual buffer with
    // no language or no configured servers, fall through to the
    // normal "nothing" rule — no indicator.
    #[test]
    fn buffer_disabled_without_language_config_renders_nothing() {
        let (text, state) = compose_lsp_status(
            "rust",
            Some("Virtual buffer"),
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
            &HashSet::new(),
        );
        assert_eq!(text, "");
        assert_eq!(state, LspIndicatorState::None);
    }

    // When the language-level LSP is stopped, every buffer for that
    // language is marked disabled too (see `handle_stop_lsp_server`).
    // In that case the language-level "LSP (off)" pill is the more
    // informative signal — the buffer-level "(n/a)" pill only fires
    // on a genuine mismatch (LSP running for language but skipped for
    // this buffer). Regression guard for the Stop-LSP-Server flow.
    #[test]
    fn buffer_disabled_does_not_hide_language_off_when_no_server_running() {
        let (text, state) = compose_lsp_status(
            "rust",
            Some("user disabled LSP"),
            &HashMap::new(),
            &HashMap::new(),
            &configured_for("rust", "rust-analyzer"),
            &HashSet::new(),
        );
        assert!(
            text.contains("LSP (off)"),
            "When no server is running, the per-buffer skip should defer \
             to the language-level 'LSP (off)' pill — got {:?}",
            text
        );
        assert_eq!(state, LspIndicatorState::Off);
    }
}
