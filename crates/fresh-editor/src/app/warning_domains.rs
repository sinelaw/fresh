//! Warning domain system for extensible warning indicators
//!
//! This module provides a generic architecture for different subsystems (LSP, plugins, etc.)
//! to report warnings with custom status bar indicators and popup content.

use rust_i18n::t;
use std::path::PathBuf;

/// Warning severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum WarningLevel {
    #[default]
    None,
    Warning,
    Error,
}

/// Content for a warning popup when user clicks the indicator
#[derive(Debug, Clone, Default)]
pub struct WarningPopupContent {
    /// Title of the popup
    pub title: String,
    /// Message body (supports basic formatting)
    pub message: String,
    /// Available actions
    pub actions: Vec<WarningAction>,
}

/// An action that can be taken from a warning popup
#[derive(Debug, Clone)]
pub struct WarningAction {
    /// Display label for the action button
    pub label: String,
    /// Action identifier (handled by the editor)
    pub action_id: WarningActionId,
}

/// Known action types for warning popups
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WarningActionId {
    /// Open the warning log file
    ViewLog,
    /// Dismiss the warning indicator
    Dismiss,
    /// Disable LSP for a specific language
    DisableLsp(String),
    /// Copy text to clipboard
    CopyToClipboard(String),
    /// Custom action (for plugins)
    Custom(String),
}

/// A domain that can report warnings with custom indicators and popups
pub trait WarningDomain: Send + Sync {
    /// Unique identifier for this domain (e.g., "lsp", "general")
    fn id(&self) -> &str;

    /// Display label for the status bar (e.g., "LSP [python]", "[⚠ 2]")
    fn label(&self) -> String;

    /// Current warning level
    fn level(&self) -> WarningLevel;

    /// Content for popup when user clicks the indicator
    fn popup_content(&self) -> WarningPopupContent;

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

    fn popup_content(&self) -> WarningPopupContent {
        let message = if self.count == 1 {
            t!("warning.one_logged").to_string()
        } else {
            t!("warning.many_logged", count = self.count).to_string()
        };

        let mut actions = vec![WarningAction {
            label: t!("warning.dismiss").to_string(),
            action_id: WarningActionId::Dismiss,
        }];

        if self.log_path.is_some() {
            actions.insert(
                0,
                WarningAction {
                    label: t!("warning.view_log").to_string(),
                    action_id: WarningActionId::ViewLog,
                },
            );
        }

        WarningPopupContent {
            title: t!("warning.title").to_string(),
            message,
            actions,
        }
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
    /// Server command that failed
    pub server_command: Option<String>,
    /// Error message
    pub error_message: Option<String>,
    /// Current level
    pub level: WarningLevel,
}

impl LspWarningDomain {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set LSP error state
    pub fn set_error(&mut self, language: String, server_command: String, message: String) {
        self.language = Some(language);
        self.server_command = Some(server_command);
        self.error_message = Some(message);
        self.level = WarningLevel::Error;
    }

    /// Clear the error state
    pub fn clear(&mut self) {
        self.language = None;
        self.server_command = None;
        self.error_message = None;
        self.level = WarningLevel::None;
    }

    /// Update from LSP server statuses
    pub fn update_from_statuses(
        &mut self,
        statuses: &std::collections::HashMap<
            String,
            crate::services::async_bridge::LspServerStatus,
        >,
    ) {
        use crate::services::async_bridge::LspServerStatus;

        // Find the first language with an error
        let error_lang = statuses
            .iter()
            .find(|(_, status)| matches!(status, LspServerStatus::Error))
            .map(|(lang, _)| lang.clone());

        if let Some(lang) = error_lang {
            self.language = Some(lang);
            self.level = WarningLevel::Error;
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

    fn popup_content(&self) -> WarningPopupContent {
        let title = if let Some(lang) = &self.language {
            t!("warning.lsp_title", language = lang).to_string()
        } else {
            t!("warning.lsp_title_default").to_string()
        };

        let message = if let Some(cmd) = &self.server_command {
            t!(
                "warning.lsp_server_not_found",
                command = cmd,
                hint = self.get_install_hint()
            )
            .to_string()
        } else if let Some(err) = &self.error_message {
            err.clone()
        } else {
            t!("warning.lsp_server_error").to_string()
        };

        let mut actions = vec![WarningAction {
            label: t!("warning.dismiss").to_string(),
            action_id: WarningActionId::Dismiss,
        }];

        // Add disable LSP action if we know the language
        if let Some(lang) = &self.language {
            actions.insert(
                0,
                WarningAction {
                    label: t!("warning.disable_lsp", language = lang).to_string(),
                    action_id: WarningActionId::DisableLsp(lang.clone()),
                },
            );
        }

        // Add copy install command if we have one
        if let Some(cmd) = self.get_install_command() {
            actions.insert(
                0,
                WarningAction {
                    label: t!("warning.copy_install_command").to_string(),
                    action_id: WarningActionId::CopyToClipboard(cmd),
                },
            );
        }

        WarningPopupContent {
            title,
            message,
            actions,
        }
    }
}

impl LspWarningDomain {
    /// Get install hint for common LSP servers
    fn get_install_hint(&self) -> String {
        let cmd = self.server_command.as_deref().unwrap_or("");

        match cmd {
            "pylsp" => t!("lsp.install_hint.pylsp").to_string(),
            "rust-analyzer" => t!("lsp.install_hint.rust_analyzer").to_string(),
            "typescript-language-server" => t!("lsp.install_hint.typescript").to_string(),
            "gopls" => t!("lsp.install_hint.gopls").to_string(),
            "clangd" => t!("lsp.install_hint.clangd").to_string(),
            "bash-language-server" => t!("lsp.install_hint.bash").to_string(),
            "vscode-html-language-server"
            | "vscode-css-language-server"
            | "vscode-json-language-server" => t!("lsp.install_hint.vscode").to_string(),
            "csharp-ls" => t!("lsp.install_hint.csharp").to_string(),
            _ => t!("lsp.install_hint.generic", command = cmd).to_string(),
        }
    }

    /// Get install command for clipboard
    fn get_install_command(&self) -> Option<String> {
        let cmd = self.server_command.as_deref()?;

        match cmd {
            "pylsp" => Some("pip install python-lsp-server".to_string()),
            "rust-analyzer" => Some("rustup component add rust-analyzer".to_string()),
            "typescript-language-server" => {
                Some("npm install -g typescript-language-server typescript".to_string())
            }
            "gopls" => Some("go install golang.org/x/tools/gopls@latest".to_string()),
            "bash-language-server" => Some("npm install -g bash-language-server".to_string()),
            "clangd" => Some("sudo apt install clangd".to_string()),
            "vscode-html-language-server"
            | "vscode-css-language-server"
            | "vscode-json-language-server" => {
                Some("npm install -g vscode-langservers-extracted".to_string())
            }
            "csharp-ls" => Some("dotnet tool install --global csharp-ls".to_string()),
            _ => None,
        }
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

    /// Get all domains that have active warnings
    pub fn active_domains(&self) -> Vec<&dyn WarningDomain> {
        let mut domains: Vec<&dyn WarningDomain> = Vec::new();

        if self.lsp.has_warnings() {
            domains.push(&self.lsp);
        }

        if self.general.has_warnings() {
            domains.push(&self.general);
        }

        domains
    }

    /// Get the highest warning level across all domains
    pub fn highest_level(&self) -> WarningLevel {
        if self.lsp.level() == WarningLevel::Error || self.general.level() == WarningLevel::Error {
            WarningLevel::Error
        } else if self.lsp.level() == WarningLevel::Warning
            || self.general.level() == WarningLevel::Warning
        {
            WarningLevel::Warning
        } else {
            WarningLevel::None
        }
    }

    /// Check if any domain has warnings
    pub fn has_any_warnings(&self) -> bool {
        self.lsp.has_warnings() || self.general.has_warnings()
    }
}
