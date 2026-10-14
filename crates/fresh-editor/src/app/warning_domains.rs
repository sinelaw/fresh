//! Warning domain system for extensible warning indicators
//!
//! This module provides a generic architecture for different subsystems (LSP, plugins, etc.)
//! to report warnings with custom status bar indicators.

use std::path::PathBuf;

/// Warning severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum WarningLevel {
    #[default]
    None,
    Warning,
    Error,
}

/// A domain that can report warnings with custom indicators and popups
pub trait WarningDomain: Send + Sync {
    /// Unique identifier for this domain (e.g., "lsp", "general")
    fn id(&self) -> &str;

    /// Display label for the status bar (e.g., "LSP [python]", "[⚠ 2]")
    fn label(&self) -> String;

    /// Current warning level
    fn level(&self) -> WarningLevel;

    /// Whether this domain has any active warnings
    fn has_warnings(&self) -> bool {
        self.level() != WarningLevel::None
    }
}

/// General warning domain for non-specific warnings (from tracing logs)
#[derive(Debug, Default)]
pub struct GeneralWarningDomain {
    /// Number of warnings
    pub count: usize,
    /// Highest severity level
    pub level: WarningLevel,
    /// Path to the warning log file
    pub log_path: Option<PathBuf>,
    /// Time when last updated
    pub last_update: Option<std::time::Instant>,
}

impl GeneralWarningDomain {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add warnings to the count
    pub fn add_warnings(&mut self, count: usize) {
        self.count = self.count.saturating_add(count);
        if self.level == WarningLevel::None {
            self.level = WarningLevel::Warning;
        }
        self.last_update = Some(std::time::Instant::now());
    }

    /// Clear all warnings
    pub fn clear(&mut self) {
        self.count = 0;
        self.level = WarningLevel::None;
        self.last_update = None;
    }

    /// Set the log file path
    pub fn set_log_path(&mut self, path: PathBuf) {
        self.log_path = Some(path);
    }
}

impl WarningDomain for GeneralWarningDomain {
    fn id(&self) -> &str {
        "general"
    }

    fn label(&self) -> String {
        if self.count > 0 {
            format!("[⚠ {}]", self.count)
        } else {
            String::new()
        }
    }

    fn level(&self) -> WarningLevel {
        self.level
    }

    fn has_warnings(&self) -> bool {
        self.count > 0
    }
}

/// LSP warning domain for language server errors
#[derive(Debug, Default)]
pub struct LspWarningDomain {
    /// Language that has issues (e.g., "python", "rust")
    pub language: Option<String>,
    /// Error message
    pub error_message: Option<String>,
    /// Current level
    pub level: WarningLevel,
}

impl LspWarningDomain {
    pub fn new() -> Self {
        Self::default()
    }

    /// Clear the error state
    pub fn clear(&mut self) {
        self.language = None;
        self.error_message = None;
        self.level = WarningLevel::None;
    }

    /// Update from LSP server statuses
    pub fn update_from_statuses(
        &mut self,
        statuses: &std::collections::HashMap<
            (String, String),
            crate::services::async_bridge::LspServerStatus,
        >,
    ) {
        use crate::services::async_bridge::LspServerStatus;

        // Find the first server with an error
        let error_lang = statuses
            .iter()
            .find(|(_, status)| matches!(status, LspServerStatus::Error))
            .map(|((lang, _), _)| lang.clone());

        if let Some(lang) = error_lang {
            self.language = Some(lang);
            self.level = WarningLevel::Error;
            return;
        }

        // A server that is up but has stopped answering requests is a
        // warning: the feature the user just invoked silently did nothing
        // (issue #2197).
        let stuck = statuses
            .iter()
            .find(|(_, status)| matches!(status, LspServerStatus::Unresponsive))
            .map(|((lang, server), _)| (lang.clone(), server.clone()));

        if let Some((lang, server)) = stuck {
            self.language = Some(lang);
            self.error_message = Some(format!(
                "'{}' is running but is not answering requests (they time out). \
                 Try restarting it, or check the server's log.",
                server,
            ));
            self.level = WarningLevel::Warning;
        } else {
            self.clear();
        }
    }
}

impl WarningDomain for LspWarningDomain {
    fn id(&self) -> &str {
        "lsp"
    }

    fn label(&self) -> String {
        // This is handled separately in status bar since LSP status has its own display
        // The colored background is applied to the existing LSP status text
        String::new()
    }

    fn level(&self) -> WarningLevel {
        self.level
    }
}

/// Registry of all warning domains
#[derive(Default)]
pub struct WarningDomainRegistry {
    pub general: GeneralWarningDomain,
    pub lsp: LspWarningDomain,
}

impl WarningDomainRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if any domain has warnings
    pub fn has_any_warnings(&self) -> bool {
        self.lsp.has_warnings() || self.general.has_warnings()
    }
}
