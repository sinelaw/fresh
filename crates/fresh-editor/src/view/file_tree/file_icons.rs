use std::path::Path;

use super::slots::{
    ExplorerLeadingSlotPayload, ExplorerLeadingSlotProvider, ExplorerSlotContext,
    DEFAULT_LEADING_SLOT_MIN_WIDTH,
};
use crate::view::theme::Theme;
use ratatui::style::Color;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExplorerIconColorRole {
    Default,
    Folder,
    Markdown,
    TypeScript,
    JavaScript,
    Data,
    Rust,
    Go,
    Python,
    Shell,
    Docker,
    Config,
    Git,
    Env,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ResolvedFileIcon {
    pub glyph: &'static str,
    pub color_role: ExplorerIconColorRole,
}

#[derive(Debug, Clone, Copy)]
struct ExactIconRule {
    basename: &'static str,
    icon: ResolvedFileIcon,
}

#[derive(Debug, Clone, Copy)]
struct ExtensionIconRule {
    extension: &'static str,
    icon: ResolvedFileIcon,
}

pub const GENERIC_FILE_ICON: ResolvedFileIcon = ResolvedFileIcon {
    glyph: "▤",
    color_role: ExplorerIconColorRole::Default,
};

pub const DIRECTORY_ICON: ResolvedFileIcon = ResolvedFileIcon {
    glyph: "▣",
    color_role: ExplorerIconColorRole::Folder,
};

pub struct CompatibilityLeadingSlotProvider;

pub static COMPATIBILITY_LEADING_SLOT_PROVIDER: CompatibilityLeadingSlotProvider =
    CompatibilityLeadingSlotProvider;

const EXACT_ICON_RULES: &[ExactIconRule] = &[
    ExactIconRule {
        basename: "package.json",
        icon: ResolvedFileIcon {
            glyph: "PK",
            color_role: ExplorerIconColorRole::JavaScript,
        },
    },
    ExactIconRule {
        basename: "tsconfig.json",
        icon: ResolvedFileIcon {
            glyph: "TC",
            color_role: ExplorerIconColorRole::TypeScript,
        },
    },
    ExactIconRule {
        basename: "Cargo.toml",
        icon: ResolvedFileIcon {
            glyph: "CG",
            color_role: ExplorerIconColorRole::Rust,
        },
    },
    ExactIconRule {
        basename: "Dockerfile",
        icon: ResolvedFileIcon {
            glyph: "DK",
            color_role: ExplorerIconColorRole::Docker,
        },
    },
    ExactIconRule {
        basename: "Containerfile",
        icon: ResolvedFileIcon {
            glyph: "DK",
            color_role: ExplorerIconColorRole::Docker,
        },
    },
    ExactIconRule {
        basename: ".env",
        icon: ResolvedFileIcon {
            glyph: "EV",
            color_role: ExplorerIconColorRole::Env,
        },
    },
    ExactIconRule {
        basename: ".gitignore",
        icon: ResolvedFileIcon {
            glyph: "GI",
            color_role: ExplorerIconColorRole::Git,
        },
    },
];

const EXTENSION_ICON_RULES: &[ExtensionIconRule] = &[
    ExtensionIconRule {
        extension: "md",
        icon: ResolvedFileIcon {
            glyph: "MD",
            color_role: ExplorerIconColorRole::Markdown,
        },
    },
    ExtensionIconRule {
        extension: "mdx",
        icon: ResolvedFileIcon {
            glyph: "MX",
            color_role: ExplorerIconColorRole::Markdown,
        },
    },
    ExtensionIconRule {
        extension: "ts",
        icon: ResolvedFileIcon {
            glyph: "TS",
            color_role: ExplorerIconColorRole::TypeScript,
        },
    },
    ExtensionIconRule {
        extension: "tsx",
        icon: ResolvedFileIcon {
            glyph: "TX",
            color_role: ExplorerIconColorRole::TypeScript,
        },
    },
    ExtensionIconRule {
        extension: "js",
        icon: ResolvedFileIcon {
            glyph: "JS",
            color_role: ExplorerIconColorRole::JavaScript,
        },
    },
    ExtensionIconRule {
        extension: "jsx",
        icon: ResolvedFileIcon {
            glyph: "JX",
            color_role: ExplorerIconColorRole::JavaScript,
        },
    },
    ExtensionIconRule {
        extension: "json",
        icon: ResolvedFileIcon {
            glyph: "{}",
            color_role: ExplorerIconColorRole::Data,
        },
    },
    ExtensionIconRule {
        extension: "yml",
        icon: ResolvedFileIcon {
            glyph: "YM",
            color_role: ExplorerIconColorRole::Data,
        },
    },
    ExtensionIconRule {
        extension: "yaml",
        icon: ResolvedFileIcon {
            glyph: "YM",
            color_role: ExplorerIconColorRole::Data,
        },
    },
    ExtensionIconRule {
        extension: "toml",
        icon: ResolvedFileIcon {
            glyph: "TL",
            color_role: ExplorerIconColorRole::Config,
        },
    },
    ExtensionIconRule {
        extension: "rs",
        icon: ResolvedFileIcon {
            glyph: "RS",
            color_role: ExplorerIconColorRole::Rust,
        },
    },
    ExtensionIconRule {
        extension: "go",
        icon: ResolvedFileIcon {
            glyph: "GO",
            color_role: ExplorerIconColorRole::Go,
        },
    },
    ExtensionIconRule {
        extension: "py",
        icon: ResolvedFileIcon {
            glyph: "PY",
            color_role: ExplorerIconColorRole::Python,
        },
    },
    ExtensionIconRule {
        extension: "sh",
        icon: ResolvedFileIcon {
            glyph: "SH",
            color_role: ExplorerIconColorRole::Shell,
        },
    },
];

pub fn resolve_file_icon(path: &Path, is_dir: bool) -> ResolvedFileIcon {
    if is_dir {
        return DIRECTORY_ICON;
    }

    let basename = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");
    if let Some(rule) = EXACT_ICON_RULES
        .iter()
        .find(|rule| rule.basename == basename)
    {
        return rule.icon;
    }

    let extension = path.extension().and_then(|ext| ext.to_str()).unwrap_or("");
    if let Some(rule) = EXTENSION_ICON_RULES
        .iter()
        .find(|rule| rule.extension.eq_ignore_ascii_case(extension))
    {
        return rule.icon;
    }

    GENERIC_FILE_ICON
}

impl ExplorerLeadingSlotProvider for CompatibilityLeadingSlotProvider {
    fn resolve(&self, context: &ExplorerSlotContext<'_>) -> Option<ExplorerLeadingSlotPayload> {
        if !context.show_file_icons {
            return None;
        }

        let icon = resolve_file_icon(context.path, context.is_dir);
        Some(ExplorerLeadingSlotPayload {
            text: icon.glyph.to_string(),
            fg: compatibility_icon_color(icon.color_role, context.is_symlink, context.theme),
            min_width: DEFAULT_LEADING_SLOT_MIN_WIDTH,
        })
    }
}

fn compatibility_icon_color(role: ExplorerIconColorRole, is_symlink: bool, theme: &Theme) -> Color {
    if is_symlink {
        return theme.syntax_type;
    }

    match role {
        ExplorerIconColorRole::Default => theme.line_number_fg,
        ExplorerIconColorRole::Folder => theme.syntax_keyword,
        ExplorerIconColorRole::Markdown => theme.syntax_string,
        ExplorerIconColorRole::TypeScript => theme.syntax_keyword,
        ExplorerIconColorRole::JavaScript => theme.syntax_constant,
        ExplorerIconColorRole::Data => theme.syntax_variable,
        ExplorerIconColorRole::Rust => theme.syntax_keyword,
        ExplorerIconColorRole::Go => theme.syntax_function,
        ExplorerIconColorRole::Python => theme.syntax_type,
        ExplorerIconColorRole::Shell => theme.syntax_operator,
        ExplorerIconColorRole::Docker => theme.syntax_variable_builtin,
        ExplorerIconColorRole::Config => theme.syntax_comment,
        ExplorerIconColorRole::Git => theme.syntax_type,
        ExplorerIconColorRole::Env => theme.syntax_variable,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn resolves_icon_by_extension() {
        let icon = resolve_file_icon(Path::new("/tmp/schema.ts"), false);
        assert_eq!(icon.glyph, "TS");
        assert_eq!(icon.color_role, ExplorerIconColorRole::TypeScript);
    }

    #[test]
    fn resolves_icon_by_exact_basename() {
        let icon = resolve_file_icon(Path::new("/tmp/package.json"), false);
        assert_eq!(icon.glyph, "PK");
        assert_eq!(icon.color_role, ExplorerIconColorRole::JavaScript);
    }

    #[test]
    fn resolves_directory_icon() {
        let icon = resolve_file_icon(Path::new("/tmp/src"), true);
        assert_eq!(icon, DIRECTORY_ICON);
    }

    #[test]
    fn falls_back_to_generic_file_icon() {
        let icon = resolve_file_icon(Path::new("/tmp/notes.txt"), false);
        assert_eq!(icon, GENERIC_FILE_ICON);
    }
}
