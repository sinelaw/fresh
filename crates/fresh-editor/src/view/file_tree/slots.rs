use std::path::Path;

use crate::primitives::display_width::str_width;
use crate::view::theme::Theme;
use fresh_core::api::OverlayColorSpec;
use ratatui::style::Color;

use super::{cache::insert_with_aliases, decorations::FileExplorerDecorationCache};

pub const COMPATIBILITY_TRAILING_SLOT_HIT_WIDTH: u16 = 2;
pub const DEFAULT_LEADING_SLOT_MIN_WIDTH: usize = 2;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExplorerTooltipSummary {
    pub title: String,
    pub lines: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExplorerLeadingSlotPayload {
    pub text: String,
    pub color: ExplorerLeadingSlotColor,
    pub min_width: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExplorerLeadingSlotColor {
    Fixed(Color),
    Filename,
}

impl ExplorerLeadingSlotColor {
    pub fn resolve(self, filename_fg: Color) -> Color {
        match self {
            Self::Fixed(color) => color,
            Self::Filename => filename_fg,
        }
    }
}

impl ExplorerLeadingSlotPayload {
    pub fn width(&self) -> usize {
        str_width(&self.text).max(self.min_width)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExplorerTrailingSlotPayload {
    pub text: String,
    pub fg: Color,
    pub tooltip: Option<ExplorerTooltipSummary>,
}

impl ExplorerTrailingSlotPayload {
    pub fn width(&self) -> usize {
        str_width(&self.text)
    }
}

#[derive(Debug, Clone)]
pub struct ExplorerTrailingSlotResolution {
    pub payload: Option<ExplorerTrailingSlotPayload>,
    pub name_color_hint: Option<Color>,
}

#[derive(Debug, Clone)]
pub struct ExplorerSlotResolution {
    pub leading: Option<ExplorerLeadingSlotPayload>,
    pub trailing: Option<ExplorerTrailingSlotPayload>,
    pub name_color_hint: Option<Color>,
}

pub struct ExplorerSlotContext<'a> {
    pub path: &'a Path,
    pub is_dir: bool,
    pub has_unsaved: bool,
    pub is_symlink: bool,
    pub is_hidden: bool,
    pub decorations: &'a FileExplorerDecorationCache,
    pub slot_overrides: &'a FileExplorerSlotOverrideCache,
    pub leading_rules: &'a FileExplorerLeadingRuleCache,
    pub theme: &'a Theme,
    pub neutral_fg: Color,
}

pub trait ExplorerLeadingSlotProvider {
    fn resolve(&self, context: &ExplorerSlotContext<'_>) -> Option<ExplorerLeadingSlotPayload>;
}

pub trait ExplorerTrailingSlotProvider {
    fn resolve(&self, context: &ExplorerSlotContext<'_>) -> ExplorerTrailingSlotResolution;

    fn hit_test_width(&self) -> u16 {
        COMPATIBILITY_TRAILING_SLOT_HIT_WIDTH
    }
}

#[derive(Clone, Copy)]
pub struct ExplorerSlotProviders {
    pub leading: &'static dyn ExplorerLeadingSlotProvider,
    pub trailing: &'static dyn ExplorerTrailingSlotProvider,
}

impl ExplorerSlotProviders {
    pub fn resolver(self) -> ExplorerSlotResolver<'static> {
        ExplorerSlotResolver::new(self.leading, self.trailing)
    }
}

pub fn default_slot_providers() -> ExplorerSlotProviders {
    ExplorerSlotProviders {
        leading: &DEFAULT_LEADING_SLOT_PROVIDER,
        trailing: &DEFAULT_TRAILING_SLOT_PROVIDER,
    }
}

#[derive(Clone, Copy)]
pub struct ExplorerSlotResolver<'a> {
    leading: &'a dyn ExplorerLeadingSlotProvider,
    trailing: &'a dyn ExplorerTrailingSlotProvider,
}

impl<'a> ExplorerSlotResolver<'a> {
    pub fn new(
        leading: &'a dyn ExplorerLeadingSlotProvider,
        trailing: &'a dyn ExplorerTrailingSlotProvider,
    ) -> Self {
        Self { leading, trailing }
    }

    pub fn resolve(&self, context: &ExplorerSlotContext<'_>) -> ExplorerSlotResolution {
        let trailing = self.trailing.resolve(context);
        ExplorerSlotResolution {
            leading: self.leading.resolve(context),
            trailing: trailing.payload,
            name_color_hint: trailing.name_color_hint,
        }
    }

    pub fn trailing_hit_test_width(&self) -> u16 {
        self.trailing.hit_test_width()
    }
}

#[derive(Debug, Clone)]
struct CachedLeadingSlot {
    text: String,
    color: OverlayColorSpec,
    min_width: usize,
}

#[derive(Debug, Clone)]
struct CachedTrailingSlot {
    text: String,
    color: OverlayColorSpec,
    tooltip: Option<ExplorerTooltipSummary>,
}

#[derive(Debug, Clone)]
struct CachedLeadingOverride {
    slot: Option<CachedLeadingSlot>,
    priority: i32,
}

#[derive(Debug, Clone)]
struct CachedTrailingOverride {
    slot: Option<CachedTrailingSlot>,
    priority: i32,
}

#[derive(Debug, Clone)]
struct CachedNameColorOverride {
    color: Option<OverlayColorSpec>,
    priority: i32,
}

#[derive(Debug, Default, Clone)]
pub struct FileExplorerSlotOverrideCache {
    direct_leading: std::collections::HashMap<std::path::PathBuf, CachedLeadingOverride>,
    direct_trailing: std::collections::HashMap<std::path::PathBuf, CachedTrailingOverride>,
    direct_name_color: std::collections::HashMap<std::path::PathBuf, CachedNameColorOverride>,
}

#[derive(Debug, Clone)]
struct CachedLeadingRuleSlot {
    text: String,
    color: fresh_core::file_explorer::FileExplorerLeadingRuleColor,
    min_width: usize,
    priority: i32,
}

/// Compiled, editor-global lookup indexes for path-independent leading slots.
/// Registration rebuilds these maps; row rendering performs only bounded hash
/// lookups and never calls JavaScript or touches the filesystem.
#[derive(Debug, Default, Clone)]
pub struct FileExplorerLeadingRuleCache {
    exact_files_sensitive: std::collections::HashMap<String, CachedLeadingRuleSlot>,
    exact_files_insensitive: std::collections::HashMap<String, CachedLeadingRuleSlot>,
    extensions_sensitive: std::collections::HashMap<String, CachedLeadingRuleSlot>,
    extensions_insensitive: std::collections::HashMap<String, CachedLeadingRuleSlot>,
    directory_names_sensitive: std::collections::HashMap<String, CachedLeadingRuleSlot>,
    directory_names_insensitive: std::collections::HashMap<String, CachedLeadingRuleSlot>,
    fallback_file: Option<CachedLeadingRuleSlot>,
    fallback_directory: Option<CachedLeadingRuleSlot>,
}

impl FileExplorerLeadingRuleCache {
    pub fn rebuild(
        by_namespace: &std::collections::HashMap<
            String,
            fresh_core::file_explorer::FileExplorerLeadingSlotRules,
        >,
    ) -> Self {
        let mut cache = Self::default();
        let mut namespaces: Vec<_> = by_namespace.keys().collect();
        namespaces.sort();

        for namespace in namespaces {
            let rules = &by_namespace[namespace];
            let case_sensitive = rules.case_sensitive.unwrap_or(false);
            let priority = rules.priority.unwrap_or_default();
            let (exact_files, extensions, directory_names) = if case_sensitive {
                (
                    &mut cache.exact_files_sensitive,
                    &mut cache.extensions_sensitive,
                    &mut cache.directory_names_sensitive,
                )
            } else {
                (
                    &mut cache.exact_files_insensitive,
                    &mut cache.extensions_insensitive,
                    &mut cache.directory_names_insensitive,
                )
            };

            for (key, slot) in rules.exact_files.iter().flatten() {
                insert_best_rule(
                    exact_files,
                    normalize_rule_key(key, case_sensitive),
                    slot,
                    priority,
                );
            }
            for (key, slot) in rules.extensions.iter().flatten() {
                let key = key.strip_prefix('.').unwrap_or(key);
                insert_best_rule(
                    extensions,
                    normalize_rule_key(key, case_sensitive),
                    slot,
                    priority,
                );
            }
            for (key, slot) in rules.directory_names.iter().flatten() {
                insert_best_rule(
                    directory_names,
                    normalize_rule_key(key, case_sensitive),
                    slot,
                    priority,
                );
            }

            if let Some(slot) = &rules.fallback_file {
                insert_best_fallback(&mut cache.fallback_file, slot, priority);
            }
            if let Some(slot) = &rules.fallback_directory {
                insert_best_fallback(&mut cache.fallback_directory, slot, priority);
            }
        }

        cache
    }

    fn resolve(&self, path: &Path, is_dir: bool) -> Option<&CachedLeadingRuleSlot> {
        let name = path.file_name()?.to_str()?;
        let folded_name = name.to_lowercase();
        let mut explicit = None;

        if is_dir {
            choose_higher_priority(&mut explicit, self.directory_names_sensitive.get(name));
            choose_higher_priority(
                &mut explicit,
                self.directory_names_insensitive.get(&folded_name),
            );
        } else {
            choose_higher_priority(&mut explicit, self.exact_files_sensitive.get(name));
            choose_higher_priority(
                &mut explicit,
                self.exact_files_insensitive.get(&folded_name),
            );

            if let Some(extension) = path.extension().and_then(|extension| extension.to_str()) {
                choose_higher_priority(&mut explicit, self.extensions_sensitive.get(extension));
                choose_higher_priority(
                    &mut explicit,
                    self.extensions_insensitive.get(&extension.to_lowercase()),
                );
            }
        }

        let fallback = if is_dir {
            self.fallback_directory.as_ref()
        } else {
            self.fallback_file.as_ref()
        };
        explicit.or(fallback)
    }
}

fn normalize_rule_key(key: &str, case_sensitive: bool) -> String {
    if case_sensitive {
        key.to_string()
    } else {
        key.to_lowercase()
    }
}

fn cached_rule_slot(
    slot: &fresh_core::file_explorer::FileExplorerLeadingRuleSlot,
    priority: i32,
) -> CachedLeadingRuleSlot {
    CachedLeadingRuleSlot {
        text: slot.text.clone(),
        color: slot.color.clone(),
        min_width: slot.min_width.unwrap_or(DEFAULT_LEADING_SLOT_MIN_WIDTH),
        priority,
    }
}

fn insert_best_rule(
    map: &mut std::collections::HashMap<String, CachedLeadingRuleSlot>,
    key: String,
    slot: &fresh_core::file_explorer::FileExplorerLeadingRuleSlot,
    priority: i32,
) {
    if map
        .get(&key)
        .is_none_or(|existing| priority >= existing.priority)
    {
        map.insert(key, cached_rule_slot(slot, priority));
    }
}

fn insert_best_fallback(
    target: &mut Option<CachedLeadingRuleSlot>,
    slot: &fresh_core::file_explorer::FileExplorerLeadingRuleSlot,
    priority: i32,
) {
    if target
        .as_ref()
        .is_none_or(|existing| priority >= existing.priority)
    {
        *target = Some(cached_rule_slot(slot, priority));
    }
}

fn choose_higher_priority<'a>(
    current: &mut Option<&'a CachedLeadingRuleSlot>,
    candidate: Option<&'a CachedLeadingRuleSlot>,
) {
    if let Some(candidate) = candidate {
        if current.is_none_or(|existing| candidate.priority > existing.priority) {
            *current = Some(candidate);
        }
    }
}

impl FileExplorerSlotOverrideCache {
    pub fn rebuild<I>(
        slots: I,
        root: &Path,
        symlink_mappings: &std::collections::HashMap<std::path::PathBuf, std::path::PathBuf>,
    ) -> Self
    where
        I: IntoIterator<Item = fresh_core::file_explorer::FileExplorerSlotEntry>,
    {
        let mut direct_leading = std::collections::HashMap::new();
        let mut direct_trailing = std::collections::HashMap::new();
        let mut direct_name_color = std::collections::HashMap::new();

        for slot in slots {
            if !slot.path.starts_with(root) {
                continue;
            }

            if slot.leading.is_some() || slot.suppress_leading {
                let cached = CachedLeadingOverride {
                    slot: slot.leading.as_ref().map(|leading| CachedLeadingSlot {
                        text: leading.text.clone(),
                        color: leading.color.clone(),
                        min_width: leading.min_width,
                    }),
                    priority: slot.priority,
                };
                insert_with_aliases(
                    &mut direct_leading,
                    &slot.path,
                    &cached,
                    symlink_mappings,
                    |map, path, value| insert_best_cached(map, path, value, |entry| entry.priority),
                );
            }

            if slot.trailing.is_some() || slot.suppress_trailing {
                let cached = CachedTrailingOverride {
                    slot: slot.trailing.as_ref().map(|trailing| CachedTrailingSlot {
                        text: trailing.text.clone(),
                        color: trailing.color.clone(),
                        tooltip: trailing
                            .tooltip
                            .as_ref()
                            .map(|tooltip| ExplorerTooltipSummary {
                                title: tooltip.title.clone(),
                                lines: tooltip.lines.clone(),
                            }),
                    }),
                    priority: slot.priority,
                };
                insert_with_aliases(
                    &mut direct_trailing,
                    &slot.path,
                    &cached,
                    symlink_mappings,
                    |map, path, value| insert_best_cached(map, path, value, |entry| entry.priority),
                );
            }

            if slot.name_color.is_some() || slot.suppress_name_color {
                let cached = CachedNameColorOverride {
                    color: slot.name_color.clone(),
                    priority: slot.priority,
                };
                insert_with_aliases(
                    &mut direct_name_color,
                    &slot.path,
                    &cached,
                    symlink_mappings,
                    |map, path, value| insert_best_cached(map, path, value, |entry| entry.priority),
                );
            }
        }

        Self {
            direct_leading,
            direct_trailing,
            direct_name_color,
        }
    }

    fn leading_override_for_path(&self, path: &Path) -> Option<&CachedLeadingOverride> {
        self.direct_leading.get(path)
    }

    fn trailing_override_for_path(&self, path: &Path) -> Option<&CachedTrailingOverride> {
        self.direct_trailing.get(path)
    }

    fn name_color_override_for_path(&self, path: &Path) -> Option<&CachedNameColorOverride> {
        self.direct_name_color.get(path)
    }

    pub fn has_trailing_override_for_path(&self, path: &Path) -> bool {
        self.direct_trailing.contains_key(path)
    }
}

pub struct DefaultLeadingSlotProvider;

pub static DEFAULT_LEADING_SLOT_PROVIDER: DefaultLeadingSlotProvider = DefaultLeadingSlotProvider;

impl ExplorerLeadingSlotProvider for DefaultLeadingSlotProvider {
    fn resolve(&self, context: &ExplorerSlotContext<'_>) -> Option<ExplorerLeadingSlotPayload> {
        if let Some(override_entry) = context
            .slot_overrides
            .leading_override_for_path(context.path)
        {
            return override_entry
                .slot
                .as_ref()
                .map(|slot| ExplorerLeadingSlotPayload {
                    text: slot.text.clone(),
                    color: ExplorerLeadingSlotColor::Fixed(resolve_overlay_color(
                        &slot.color,
                        context.theme,
                        context.neutral_fg,
                    )),
                    min_width: slot.min_width,
                });
        }

        context
            .leading_rules
            .resolve(context.path, context.is_dir)
            .map(|slot| ExplorerLeadingSlotPayload {
                text: slot.text.clone(),
                color: match &slot.color {
                    fresh_core::file_explorer::FileExplorerLeadingRuleColor::Color(color) => {
                        ExplorerLeadingSlotColor::Fixed(resolve_overlay_color(
                            color,
                            context.theme,
                            context.neutral_fg,
                        ))
                    }
                    fresh_core::file_explorer::FileExplorerLeadingRuleColor::Filename {
                        ..
                    } => ExplorerLeadingSlotColor::Filename,
                },
                min_width: slot.min_width,
            })
    }
}

pub struct DefaultTrailingSlotProvider;

pub static DEFAULT_TRAILING_SLOT_PROVIDER: DefaultTrailingSlotProvider =
    DefaultTrailingSlotProvider;

impl ExplorerTrailingSlotProvider for DefaultTrailingSlotProvider {
    fn resolve(&self, context: &ExplorerSlotContext<'_>) -> ExplorerTrailingSlotResolution {
        let compatibility =
            super::decorations::COMPATIBILITY_TRAILING_SLOT_PROVIDER.resolve(context);
        let override_trailing = context
            .slot_overrides
            .trailing_override_for_path(context.path);
        let override_name_color = context
            .slot_overrides
            .name_color_override_for_path(context.path);

        ExplorerTrailingSlotResolution {
            payload: match override_trailing {
                Some(override_entry) => {
                    override_entry
                        .slot
                        .as_ref()
                        .map(|slot| ExplorerTrailingSlotPayload {
                            text: slot.text.clone(),
                            fg: resolve_overlay_color(
                                &slot.color,
                                context.theme,
                                context.neutral_fg,
                            ),
                            tooltip: slot.tooltip.clone(),
                        })
                }
                None => compatibility.payload,
            },
            name_color_hint: match override_name_color {
                Some(override_entry) => override_entry
                    .color
                    .as_ref()
                    .map(|color| resolve_overlay_color(color, context.theme, context.neutral_fg)),
                None => compatibility.name_color_hint,
            },
        }
    }

    fn hit_test_width(&self) -> u16 {
        COMPATIBILITY_TRAILING_SLOT_HIT_WIDTH
    }
}

fn resolve_overlay_color(spec: &OverlayColorSpec, theme: &Theme, fallback: Color) -> Color {
    match spec {
        OverlayColorSpec::Rgb(r, g, b) => Color::Rgb(*r, *g, *b),
        OverlayColorSpec::ThemeKey(key) => theme.resolve_theme_key(key).unwrap_or(fallback),
    }
}

fn insert_best_cached<T, FPriority>(
    map: &mut std::collections::HashMap<std::path::PathBuf, T>,
    path: std::path::PathBuf,
    value: T,
    priority: FPriority,
) where
    FPriority: Fn(&T) -> i32,
{
    let replace = match map.get(&path) {
        Some(existing) => priority(&value) >= priority(existing),
        None => true,
    };

    if replace {
        map.insert(path, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn context_for<'a>(
        path: &'a Path,
        decorations: &'a FileExplorerDecorationCache,
        slot_overrides: &'a FileExplorerSlotOverrideCache,
        leading_rules: &'a FileExplorerLeadingRuleCache,
        theme: &'a Theme,
    ) -> ExplorerSlotContext<'a> {
        ExplorerSlotContext {
            path,
            is_dir: false,
            has_unsaved: false,
            is_symlink: false,
            is_hidden: false,
            decorations,
            slot_overrides,
            leading_rules,
            theme,
            neutral_fg: theme.editor_fg,
        }
    }

    fn rule_table(value: serde_json::Value) -> FileExplorerLeadingRuleCache {
        let rules = serde_json::from_value(value).unwrap();
        FileExplorerLeadingRuleCache::rebuild(&std::collections::HashMap::from([(
            "test".to_string(),
            rules,
        )]))
    }

    #[test]
    fn leading_rules_resolve_explicit_before_fallback_and_ignore_case() {
        let theme = Theme::load_builtin("dark").unwrap();
        let decorations = FileExplorerDecorationCache::default();
        let overrides = FileExplorerSlotOverrideCache::default();
        let rules = rule_table(serde_json::json!({
            "priority": 10,
            "extensions": {
                "rs": { "text": "R", "color": { "source": "filename" }, "minWidth": 1 }
            },
            "fallbackFile": { "text": "F", "color": "syntax.string" },
            "fallbackDirectory": { "text": "D", "color": "syntax.keyword" }
        }));

        for path in [Path::new("/repo/main.rs"), Path::new("/repo/main.RS")] {
            let resolved = default_slot_providers().resolver().resolve(&context_for(
                path,
                &decorations,
                &overrides,
                &rules,
                &theme,
            ));
            let leading = resolved.leading.unwrap();
            assert_eq!(leading.text, "R");
            assert_eq!(leading.color, ExplorerLeadingSlotColor::Filename);
        }

        let fallback = default_slot_providers().resolver().resolve(&context_for(
            Path::new("/repo/unknown.bin"),
            &decorations,
            &overrides,
            &rules,
            &theme,
        ));
        assert_eq!(fallback.leading.unwrap().text, "F");
    }

    #[test]
    fn explicit_rule_tier_beats_higher_priority_fallback() {
        let explicit = serde_json::from_value(serde_json::json!({
            "priority": 1,
            "extensions": {
                "rs": { "text": "R", "color": "syntax.keyword" }
            }
        }))
        .unwrap();
        let fallback = serde_json::from_value(serde_json::json!({
            "priority": 100,
            "fallbackFile": { "text": "F", "color": "syntax.string" }
        }))
        .unwrap();
        let cache = FileExplorerLeadingRuleCache::rebuild(&std::collections::HashMap::from([
            ("explicit".to_string(), explicit),
            ("fallback".to_string(), fallback),
        ]));

        assert_eq!(
            cache.resolve(Path::new("main.rs"), false).unwrap().text,
            "R"
        );
        assert_eq!(
            cache.resolve(Path::new("main.bin"), false).unwrap().text,
            "F"
        );
    }

    #[test]
    fn explicit_selectors_arbitrate_by_namespace_priority() {
        let exact = serde_json::from_value(serde_json::json!({
            "priority": 1,
            "exactFiles": {
                "main.rs": { "text": "E", "color": "syntax.keyword" }
            }
        }))
        .unwrap();
        let extension = serde_json::from_value(serde_json::json!({
            "priority": 2,
            "extensions": {
                "rs": { "text": "X", "color": "syntax.string" }
            }
        }))
        .unwrap();
        let cache = FileExplorerLeadingRuleCache::rebuild(&std::collections::HashMap::from([
            ("exact".to_string(), exact),
            ("extension".to_string(), extension),
        ]));

        assert_eq!(
            cache.resolve(Path::new("main.rs"), false).unwrap().text,
            "X"
        );
    }

    #[test]
    fn case_sensitive_and_directory_rules_stay_scoped_to_their_kinds() {
        let rules = serde_json::from_value(serde_json::json!({
            "caseSensitive": true,
            "exactFiles": {
                "Makefile": { "text": "M", "color": "syntax.keyword" }
            },
            "directoryNames": {
                "src": { "text": "D", "color": "syntax.keyword" }
            }
        }))
        .unwrap();
        let cache = FileExplorerLeadingRuleCache::rebuild(&std::collections::HashMap::from([(
            "rules".to_string(),
            rules,
        )]));

        assert_eq!(
            cache.resolve(Path::new("Makefile"), false).unwrap().text,
            "M"
        );
        assert!(cache.resolve(Path::new("makefile"), false).is_none());
        assert_eq!(cache.resolve(Path::new("src"), true).unwrap().text, "D");
        assert!(cache.resolve(Path::new("src"), false).is_none());
    }

    #[test]
    fn exact_path_override_and_suppression_beat_leading_rules() {
        let theme = Theme::load_builtin("dark").unwrap();
        let path = std::path::PathBuf::from("/repo/main.rs");
        let decorations = FileExplorerDecorationCache::default();
        let rules = rule_table(serde_json::json!({
            "priority": 100,
            "extensions": {
                "rs": { "text": "R", "color": "syntax.keyword" }
            }
        }));
        let custom = FileExplorerSlotOverrideCache::rebuild(
            vec![fresh_core::file_explorer::FileExplorerSlotEntry {
                path: path.clone(),
                leading: Some(fresh_core::file_explorer::FileExplorerLeadingSlot {
                    text: "P".into(),
                    color: OverlayColorSpec::ThemeKey("syntax.string".into()),
                    min_width: 1,
                }),
                suppress_leading: false,
                trailing: None,
                suppress_trailing: false,
                name_color: None,
                suppress_name_color: false,
                priority: 1,
            }],
            Path::new("/repo"),
            &std::collections::HashMap::new(),
        );
        let resolved = default_slot_providers().resolver().resolve(&context_for(
            &path,
            &decorations,
            &custom,
            &rules,
            &theme,
        ));
        assert_eq!(resolved.leading.unwrap().text, "P");

        let suppressed = FileExplorerSlotOverrideCache::rebuild(
            vec![fresh_core::file_explorer::FileExplorerSlotEntry {
                path: path.clone(),
                leading: None,
                suppress_leading: true,
                trailing: None,
                suppress_trailing: false,
                name_color: None,
                suppress_name_color: false,
                priority: 1,
            }],
            Path::new("/repo"),
            &std::collections::HashMap::new(),
        );
        assert!(default_slot_providers()
            .resolver()
            .resolve(&context_for(
                &path,
                &decorations,
                &suppressed,
                &rules,
                &theme
            ))
            .leading
            .is_none());
    }

    #[test]
    fn slot_overrides_do_not_bubble_to_ancestors() {
        let cache = FileExplorerSlotOverrideCache::rebuild(
            vec![fresh_core::file_explorer::FileExplorerSlotEntry {
                path: std::path::PathBuf::from("/repo/src/file.ts"),
                leading: None,
                suppress_leading: false,
                trailing: Some(fresh_core::file_explorer::FileExplorerTrailingSlot {
                    text: "P".to_string(),
                    color: OverlayColorSpec::ThemeKey("syntax.string".into()),
                    tooltip: None,
                }),
                suppress_trailing: false,
                name_color: Some(OverlayColorSpec::ThemeKey("syntax.type".into())),
                suppress_name_color: false,
                priority: 10,
            }],
            Path::new("/repo"),
            &std::collections::HashMap::new(),
        );

        assert!(cache.has_trailing_override_for_path(Path::new("/repo/src/file.ts")));
        assert!(!cache.has_trailing_override_for_path(Path::new("/repo/src")));
        assert!(cache
            .name_color_override_for_path(Path::new("/repo/src"))
            .is_none());
    }

    #[test]
    fn suppressed_trailing_and_name_color_block_compatibility_fallback() {
        let theme = Theme::load_builtin("dark").unwrap();
        let path = std::path::PathBuf::from("/repo/file.ts");
        let decorations = FileExplorerDecorationCache::rebuild(
            vec![fresh_core::file_explorer::FileExplorerDecoration {
                path: path.clone(),
                symbol: "M".to_string(),
                color: OverlayColorSpec::ThemeKey("ui.file_status_modified_fg".into()),
                priority: 50,
            }],
            Path::new("/repo"),
            &std::collections::HashMap::new(),
        );
        let slot_overrides = FileExplorerSlotOverrideCache::rebuild(
            vec![fresh_core::file_explorer::FileExplorerSlotEntry {
                path: path.clone(),
                leading: None,
                suppress_leading: false,
                trailing: None,
                suppress_trailing: true,
                name_color: None,
                suppress_name_color: true,
                priority: 10,
            }],
            Path::new("/repo"),
            &std::collections::HashMap::new(),
        );
        let context = ExplorerSlotContext {
            path: &path,
            is_dir: false,
            has_unsaved: false,
            is_symlink: false,
            is_hidden: false,
            decorations: &decorations,
            slot_overrides: &slot_overrides,
            leading_rules: &FileExplorerLeadingRuleCache::default(),
            theme: &theme,
            neutral_fg: theme.editor_fg,
        };

        let resolved = default_slot_providers().resolver().resolve(&context);
        assert!(resolved.trailing.is_none());
        assert!(resolved.name_color_hint.is_none());
    }
}
