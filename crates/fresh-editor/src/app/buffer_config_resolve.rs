//! Pure resolution of per-buffer editor settings from language overrides.
//!
//! Each function takes the buffer's language name and the active `Config`
//! and returns the effective setting. Language-specific values override
//! the global editor defaults where set; otherwise the global default wins.
//!
//! These helpers have no dependency on `Editor` and no I/O. They exist as
//! free functions so both the orchestrator on `Editor` and any future
//! `BufferRegistry` subsystem can call them without tangled coupling.

use crate::config::Config;

/// Effective `line_wrap` for a buffer with the given language.
pub(crate) fn line_wrap(language: &str, config: &Config) -> bool {
    if let Some(lang_config) = config.languages.get(language) {
        if let Some(line_wrap) = lang_config.line_wrap {
            return line_wrap;
        }
    }
    config.editor.line_wrap
}

/// Effective `wrap_column` for a buffer with the given language.
///
/// Returns the language-specific `wrap_column` if explicitly set, otherwise
/// the global `editor.wrap_column`.
pub(crate) fn wrap_column(language: &str, config: &Config) -> Option<usize> {
    if let Some(lang_config) = config.languages.get(language) {
        if lang_config.wrap_column.is_some() {
            return lang_config.wrap_column;
        }
    }
    config.editor.wrap_column
}

/// Effective `page_view` width for a buffer with the given language.
///
/// The outer `Option` distinguishes "page view disabled" (`None`) from
/// "page view enabled" (`Some(...)`). The inner `Option<usize>` carries the
/// explicit page width: `Some(Some(w))` uses `w`, `Some(None)` falls back
/// to whatever downstream code treats as the default width.
pub(crate) fn page_view(language: &str, config: &Config) -> Option<Option<usize>> {
    let lang_config = config.languages.get(language)?;
    if lang_config.page_view == Some(true) {
        Some(lang_config.page_width.or(config.editor.page_width))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::LanguageConfig;

    fn config_with(language: &str, lang_config: LanguageConfig) -> Config {
        let mut config = Config::default();
        config.languages.insert(language.to_string(), lang_config);
        config
    }

    #[test]
    fn line_wrap_language_override_wins_over_global() {
        let mut lang = LanguageConfig::default();
        lang.line_wrap = Some(true);
        let mut config = config_with("markdown", lang);
        config.editor.line_wrap = false;

        assert!(line_wrap("markdown", &config));
    }

    #[test]
    fn line_wrap_falls_back_to_global_when_language_unset() {
        let mut config = Config::default();
        config.editor.line_wrap = true;
        // No entry for "rust" at all.
        assert!(line_wrap("rust", &config));
    }

    #[test]
    fn line_wrap_falls_back_to_global_when_language_has_none() {
        let lang = LanguageConfig::default(); // line_wrap = None
        let mut config = config_with("rust", lang);
        config.editor.line_wrap = true;
        assert!(line_wrap("rust", &config));
    }

    #[test]
    fn wrap_column_language_override_wins() {
        let mut lang = LanguageConfig::default();
        lang.wrap_column = Some(100);
        let mut config = config_with("rust", lang);
        config.editor.wrap_column = Some(80);
        assert_eq!(wrap_column("rust", &config), Some(100));
    }

    #[test]
    fn wrap_column_falls_back_to_global() {
        let mut config = Config::default();
        config.editor.wrap_column = Some(80);
        assert_eq!(wrap_column("unknown", &config), Some(80));
    }

    #[test]
    fn page_view_none_when_language_missing() {
        let config = Config::default();
        assert_eq!(page_view("unknown", &config), None);
    }

    #[test]
    fn page_view_none_when_flag_not_true() {
        let lang = LanguageConfig::default(); // page_view = None
        let config = config_with("rust", lang);
        assert_eq!(page_view("rust", &config), None);
    }

    #[test]
    fn page_view_uses_language_width_first() {
        let mut lang = LanguageConfig::default();
        lang.page_view = Some(true);
        lang.page_width = Some(72);
        let mut config = config_with("markdown", lang);
        config.editor.page_width = Some(80);
        assert_eq!(page_view("markdown", &config), Some(Some(72)));
    }

    #[test]
    fn page_view_falls_back_to_global_width() {
        let mut lang = LanguageConfig::default();
        lang.page_view = Some(true);
        // page_width left as None
        let mut config = config_with("markdown", lang);
        config.editor.page_width = Some(80);
        assert_eq!(page_view("markdown", &config), Some(Some(80)));
    }
}
