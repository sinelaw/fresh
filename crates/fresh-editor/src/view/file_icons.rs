//! File-type icons: a Nerd Font glyph and a color per kind of file.
//!
//! Drawn before a file's name in the file explorer and on its tab while the
//! user's `editor.nerd_font_icons` is on — the glyphs are private-use code
//! points that only a Nerd Font draws. The tables here are the defaults, in
//! the familiar colors (nvim-web-devicons'), so a Rust file reads as Rust at
//! a glance.
//!
//! **A theme customizes them** in its `file_icons` section — a glyph, a
//! color or both, by file name, by extension, for the default and for
//! folders ([`resolve`], [`resolve_folder`]) — and can draw every icon in one
//! color with `ui.file_icon_fg`.
//!
//! Every code point is the Nerd Fonts glyph named beside it (v3,
//! `glyphnames.json`).

use crate::view::theme::{FileIcons, IconOverride};
use ratatui::style::Color;

/// A file's icon: the glyph and its color.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FileIcon {
    pub glyph: &'static str,
    pub color: Color,
}

const fn icon(glyph: &'static str, rgb: u32) -> FileIcon {
    FileIcon {
        glyph,
        color: Color::Rgb((rgb >> 16) as u8, (rgb >> 8) as u8, rgb as u8),
    }
}

/// A closed folder: `nf-custom-folder`.
pub const FOLDER: &str = "\u{e5ff}";
/// An open folder: `nf-custom-folder_open`.
pub const FOLDER_OPEN: &str = "\u{e5fe}";

/// Any file the tables below do not know: `nf-fa-file_o`.
pub const DEFAULT: FileIcon = icon("\u{f016}", 0x6d8086);

/// Whole file names, matched case-insensitively before the extension.
const BY_NAME: &[(&str, FileIcon)] = &[
    ("cargo.toml", icon("\u{e7a8}", 0xdea584)),   // dev-rust
    ("cargo.lock", icon("\u{e7a8}", 0xdea584)),   // dev-rust
    ("package.json", icon("\u{e71e}", 0xe8274b)), // dev-npm
    ("package-lock.json", icon("\u{e71e}", 0x7a0d21)),
    ("dockerfile", icon("\u{e7b0}", 0x458ee6)), // dev-docker
    ("makefile", icon("\u{e673}", 0x6d8086)),   // seti-makefile
    ("license", icon("\u{e60a}", 0xd0bf41)),    // seti-license
    ("license.md", icon("\u{e60a}", 0xd0bf41)),
    ("readme.md", icon("\u{f405}", 0xededed)), // oct-book
    ("readme", icon("\u{f405}", 0xededed)),
    (".gitignore", icon("\u{e702}", 0xf54d27)), // dev-git
    (".gitattributes", icon("\u{e702}", 0xf54d27)),
    (".gitmodules", icon("\u{e702}", 0xf54d27)),
    (".editorconfig", icon("\u{e615}", 0xfff2f2)), // seti-config
    ("flake.nix", icon("\u{f313}", 0x7ebae4)),     // linux-nixos
    ("todo.md", icon("\u{e69c}", 0x7fbbd4)),       // seti-todo
];

/// Extensions, lower case, without the dot.
const BY_EXTENSION: &[(&str, FileIcon)] = &[
    ("rs", icon("\u{e7a8}", 0xdea584)), // dev-rust
    ("py", icon("\u{e606}", 0xffbc03)), // seti-python
    ("js", icon("\u{e74e}", 0xcbcb41)), // dev-javascript
    ("mjs", icon("\u{e74e}", 0xf1e05a)),
    ("cjs", icon("\u{e74e}", 0xcbcb41)),
    ("ts", icon("\u{e628}", 0x519aba)), // seti-typescript
    ("mts", icon("\u{e628}", 0x519aba)),
    ("cts", icon("\u{e628}", 0x519aba)),
    ("tsx", icon("\u{e7ba}", 0x1354bf)), // dev-react
    ("jsx", icon("\u{e7ba}", 0x20c2e3)),
    ("json", icon("\u{e60b}", 0xcbcb41)), // seti-json
    ("jsonc", icon("\u{e60b}", 0xcbcb41)),
    ("md", icon("\u{f48a}", 0xdddddd)), // oct-markdown
    ("markdown", icon("\u{f48a}", 0xdddddd)),
    ("toml", icon("\u{e6b2}", 0x9c4221)), // custom-toml
    ("yml", icon("\u{e6a8}", 0x6d8086)),  // seti-yml
    ("yaml", icon("\u{e6a8}", 0x6d8086)),
    ("html", icon("\u{e736}", 0xe44d26)), // dev-html5
    ("htm", icon("\u{e736}", 0xe34c26)),
    ("css", icon("\u{e749}", 0x42a5f5)),  // dev-css3
    ("scss", icon("\u{e74b}", 0xf55385)), // dev-sass
    ("sass", icon("\u{e74b}", 0xf55385)),
    ("go", icon("\u{e627}", 0x00add8)), // seti-go
    ("c", icon("\u{e61e}", 0x599eff)),  // custom-c
    ("h", icon("\u{e61e}", 0xa074c4)),
    ("cpp", icon("\u{e61d}", 0x519aba)), // custom-cpp
    ("cc", icon("\u{e61d}", 0xf34b7d)),
    ("hpp", icon("\u{e61d}", 0xa074c4)),
    ("java", icon("\u{e738}", 0xcc3e44)),  // dev-java
    ("kt", icon("\u{f1219}", 0x7f52ff)),   // md-language_kotlin
    ("swift", icon("\u{e699}", 0xe37933)), // seti-swift
    ("cs", icon("\u{f031b}", 0x596706)),   // md-language_csharp
    ("lua", icon("\u{e620}", 0x51a0cf)),   // seti-lua
    ("rb", icon("\u{e739}", 0x701516)),    // dev-ruby
    ("php", icon("\u{e73d}", 0xa074c4)),   // dev-php
    ("hs", icon("\u{e61f}", 0xa074c4)),    // seti-haskell
    ("ex", icon("\u{e62d}", 0xa074c4)),    // seti-elixir
    ("exs", icon("\u{e62d}", 0xa074c4)),
    ("zig", icon("\u{e6a9}", 0xf69a1b)),   // seti-zig
    ("jl", icon("\u{e624}", 0xa270ba)),    // seti-julia
    ("dart", icon("\u{e798}", 0x03589c)),  // dev-dart
    ("scala", icon("\u{e68e}", 0xcc3e44)), // seti-scala
    ("ml", icon("\u{e67a}", 0xe37933)),    // seti-ocaml
    ("clj", icon("\u{e642}", 0x8dc149)),   // seti-clojure
    ("elm", icon("\u{e62c}", 0x519aba)),   // custom-elm
    ("pl", icon("\u{e67e}", 0x519aba)),    // seti-perl
    ("r", icon("\u{e68a}", 0x2266ba)),     // seti-r
    ("sh", icon("\u{e795}", 0x4d5a5e)),    // dev-terminal
    ("bash", icon("\u{e795}", 0x89e051)),
    ("zsh", icon("\u{e795}", 0x89e051)),
    ("fish", icon("\u{e795}", 0x4d5a5e)),
    ("ps1", icon("\u{e683}", 0x4273ca)),     // seti-powershell
    ("vim", icon("\u{e7c5}", 0x019833)),     // dev-vim
    ("nix", icon("\u{f313}", 0x7ebae4)),     // linux-nixos
    ("svelte", icon("\u{e697}", 0xff3e00)),  // seti-svelte
    ("vue", icon("\u{e6a0}", 0x8dc149)),     // seti-vue
    ("graphql", icon("\u{e662}", 0xe535ab)), // seti-graphql
    ("prisma", icon("\u{e684}", 0x5fa04e)),  // seti-prisma
    ("tf", icon("\u{e69a}", 0x5f43e9)),      // seti-terraform
    ("xml", icon("\u{e619}", 0xe37933)),     // seti-xml
    ("csv", icon("\u{e64a}", 0x89e051)),     // seti-csv
    ("sql", icon("\u{f01bc}", 0xdad8d8)),    // md-database
    ("db", icon("\u{e64d}", 0xdad8d8)),      // seti-db
    ("lock", icon("\u{e672}", 0xbbbbbb)),    // seti-lock
    ("txt", icon("\u{f0f6}", 0x89e051)),     // fa-file_text_o
    ("log", icon("\u{f0f6}", 0xdddddd)),
    ("tex", icon("\u{e69b}", 0x3d6117)), // seti-tex
    ("pdf", icon("\u{e67d}", 0xb30b00)), // seti-pdf
    ("png", icon("\u{f1c5}", 0xa074c4)), // fa-file_image_o
    ("jpg", icon("\u{f1c5}", 0xa074c4)),
    ("jpeg", icon("\u{f1c5}", 0xa074c4)),
    ("gif", icon("\u{f1c5}", 0xa074c4)),
    ("webp", icon("\u{f1c5}", 0xa074c4)),
    ("ico", icon("\u{f1c5}", 0xcbcb41)),
    ("svg", icon("\u{f0721}", 0xffb13b)), // md-svg
    ("mp4", icon("\u{e69f}", 0xfd971f)),  // seti-video
    ("mp3", icon("\u{e638}", 0x00afff)),  // seti-audio
    ("wav", icon("\u{e638}", 0x00afff)),
    ("zip", icon("\u{e6aa}", 0xeca517)), // seti-zip
    ("gz", icon("\u{e6aa}", 0xeca517)),
    ("tar", icon("\u{e6aa}", 0xeca517)),
    ("xz", icon("\u{e6aa}", 0xeca517)),
    ("ini", icon("\u{e615}", 0x6d8086)), // seti-config
    ("conf", icon("\u{e615}", 0x6d8086)),
    ("cfg", icon("\u{e615}", 0x6d8086)),
    ("env", icon("\u{e615}", 0xfaf743)),
];

/// The icon for a file named `name` (a bare name or a path; only the last
/// component counts): its whole name if the table knows it, else its
/// extension, else [`DEFAULT`].
pub fn for_file(name: &str) -> FileIcon {
    let base = name.rsplit(['/', '\\']).next().unwrap_or(name);
    let lower = base.to_ascii_lowercase();
    if let Some((_, i)) = BY_NAME.iter().find(|(n, _)| *n == lower) {
        return *i;
    }
    if lower.starts_with(".env") {
        return icon("\u{e615}", 0xfaf743);
    }
    lower
        .rsplit_once('.')
        .and_then(|(_, ext)| BY_EXTENSION.iter().find(|(e, _)| *e == ext))
        .map(|(_, i)| *i)
        .unwrap_or(DEFAULT)
}

/// The built-in tables in a theme file's `file_icons` shape, for the theme
/// editor: what each override replaces.
pub fn builtin_json() -> serde_json::Value {
    use serde_json::{json, Map, Value};
    let one = |i: &FileIcon| {
        let rgb = match i.color {
            Color::Rgb(r, g, b) => json!([r, g, b]),
            _ => Value::Null,
        };
        json!({ "glyph": i.glyph, "color": rgb })
    };
    let table = |t: &[(&str, FileIcon)]| -> Map<String, Value> {
        t.iter().map(|(k, i)| (k.to_string(), one(i))).collect()
    };
    json!({
        "by_name": table(BY_NAME),
        "by_extension": table(BY_EXTENSION),
        "default": one(&DEFAULT),
        "folder": { "glyph": FOLDER, "color": Value::Null },
        "folder_open": { "glyph": FOLDER_OPEN, "color": Value::Null },
    })
}

/// A file's icon as drawn: the glyph and color, after the theme's
/// overrides.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Resolved {
    pub glyph: String,
    pub color: Color,
}

impl Resolved {
    fn with(glyph: &str, color: Color, over: Option<&IconOverride>) -> Resolved {
        Resolved {
            glyph: over
                .and_then(|o| o.glyph.clone())
                .unwrap_or_else(|| glyph.to_string()),
            color: over.and_then(|o| o.color).unwrap_or(color),
        }
    }
}

/// The icon for a file named `name`, with the theme's `file_icons` over the
/// built-in tables. The most specific match wins — a whole name, then the
/// extension, then the default — and at each of those the theme's entry wins
/// over the built-in one, half by half: an entry with only a color keeps the
/// built-in glyph.
pub fn resolve(theme: &FileIcons, name: &str) -> Resolved {
    let base = name.rsplit(['/', '\\']).next().unwrap_or(name);
    let lower = base.to_ascii_lowercase();
    let builtin = for_file(name);
    if let Some(over) = theme.by_name.get(&lower) {
        return Resolved::with(builtin.glyph, builtin.color, Some(over));
    }
    if BY_NAME.iter().any(|(n, _)| *n == lower) {
        return Resolved::with(builtin.glyph, builtin.color, None);
    }
    if let Some(over) = lower
        .rsplit_once('.')
        .and_then(|(_, ext)| theme.by_extension.get(ext))
    {
        return Resolved::with(builtin.glyph, builtin.color, Some(over));
    }
    let over = (builtin == DEFAULT)
        .then_some(theme.default.as_ref())
        .flatten();
    Resolved::with(builtin.glyph, builtin.color, over)
}

/// A folder's icon, open or closed, with the theme's `file_icons.folder` /
/// `folder_open` over the built-in glyph and `color` (the explorer's
/// directory color).
pub fn resolve_folder(theme: &FileIcons, open: bool, color: Color) -> Resolved {
    let (glyph, over) = if open {
        (FOLDER_OPEN, theme.folder_open.as_ref())
    } else {
        (FOLDER, theme.folder.as_ref())
    };
    Resolved::with(glyph, color, over)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn names_win_over_extensions_and_unknown_files_get_the_default() {
        assert_eq!(
            for_file("Cargo.toml").glyph,
            "\u{e7a8}",
            "the Rust icon, not TOML's"
        );
        assert_eq!(for_file("config.toml").glyph, "\u{e6b2}");
        assert_eq!(
            for_file("src/main.RS").glyph,
            "\u{e7a8}",
            "case and path ignored"
        );
        assert_eq!(for_file("README.md").glyph, "\u{f405}");
        assert_eq!(for_file("notes.md").glyph, "\u{f48a}");
        assert_eq!(for_file("Makefile").glyph, "\u{e673}");
        assert_eq!(for_file("no_extension"), DEFAULT);
        assert_eq!(for_file("archive.unknownext"), DEFAULT);
        assert_eq!(for_file(".env.local").glyph, "\u{e615}");
    }

    /// A theme's entries win at their level and fill in only what they name;
    /// a whole-name match outranks an extension override.
    #[test]
    fn theme_overrides_merge_over_the_builtin_icons() {
        let theme = crate::view::theme::Theme::from_json(
            r#"{"name": "t", "extends": "builtin://dark",
                "file_icons": {
                    "by_extension": {"rs": {"color": [1, 2, 3]}, ".toml": {"glyph": "T"},
                                     "zz": {"glyph": "Z", "color": [9, 9, 9]}},
                    "by_name": {"JUSTFILE": {"glyph": "J"}},
                    "default": {"color": [4, 5, 6]},
                    "folder_open": {"glyph": "O"}}}"#,
        )
        .unwrap();
        let icons = &theme.file_icons;
        let rs = resolve(icons, "main.rs");
        assert_eq!(rs.glyph, "\u{e7a8}", "only the color was named");
        assert_eq!(rs.color, Color::Rgb(1, 2, 3));
        assert_eq!(
            resolve(icons, "x.toml").glyph,
            "T",
            "a leading dot is ignored"
        );
        assert_eq!(
            resolve(icons, "Cargo.toml").glyph,
            "\u{e7a8}",
            "the built-in name wins"
        );
        assert_eq!(
            resolve(icons, "a.zz"),
            Resolved {
                glyph: "Z".into(),
                color: Color::Rgb(9, 9, 9)
            }
        );
        assert_eq!(
            resolve(icons, "justfile").glyph,
            "J",
            "names match case-insensitively"
        );
        let unknown = resolve(icons, "data.unknown");
        assert_eq!(unknown.glyph, DEFAULT.glyph);
        assert_eq!(unknown.color, Color::Rgb(4, 5, 6));
        assert_eq!(resolve_folder(icons, true, Color::Red).glyph, "O");
        assert_eq!(
            resolve_folder(icons, false, Color::Red),
            Resolved {
                glyph: FOLDER.into(),
                color: Color::Red
            }
        );

        // No overrides: the built-in tables.
        let plain = FileIcons::default();
        assert_eq!(resolve(&plain, "main.rs").color, for_file("main.rs").color);
    }

    /// Every glyph is one terminal cell, so the explorer's and the tab's
    /// columns line up whatever the file.
    #[test]
    fn every_glyph_is_one_cell() {
        use crate::primitives::display_width::str_width;
        for (_, i) in BY_NAME.iter().chain(BY_EXTENSION) {
            assert_eq!(str_width(i.glyph), 1, "{:?}", i.glyph);
        }
        assert_eq!(str_width(DEFAULT.glyph), 1);
    }
}
