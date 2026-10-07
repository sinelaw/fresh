//! Plugin API reference generator.
//!
//! Renders the markdown pages under `docs/plugins/api/` from the generated
//! `fresh.d.ts`, so the reference can't drift from the API. Each method's
//! text is its doc comment in the Rust source (or in the hand-written
//! TypeScript in `ts_export.rs`); each method's page and heading come from
//! its `#[plugin_api(section = "...")]`.
//!
//! Doc comment conventions the renderer understands:
//! - Markdown, as in any doc comment.
//! - `@param name - description` lines become a parameter table.
//! - `@returns description` becomes a "Returns" line.
//!
//! Regenerate with
//! `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`.

use std::collections::{BTreeMap, HashMap};

use oxc_allocator::Allocator;
use oxc_ast::ast::{Comment, Statement, TSSignature};
use oxc_parser::Parser;
use oxc_span::{GetSpan, SourceType, Span};

/// The command that regenerates `fresh.d.ts` and these pages.
pub const REGENERATE_COMMAND: &str =
    "cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored";

/// One generated markdown page.
pub struct Page {
    /// File name under `docs/plugins/api/`.
    pub file: &'static str,
    pub content: String,
}

/// A page of methods, grouped into sections in the order listed.
struct PageSpec {
    file: &'static str,
    title: &'static str,
    intro: &'static str,
    sections: &'static [&'static str],
}

/// Every method section must appear on exactly one page.
const METHOD_PAGES: &[PageSpec] = &[
    PageSpec {
        file: "runtime.md",
        title: "Plugin Runtime",
        intro: "The globals every plugin uses, plugin information, sharing an API \
                with other plugins, loading plugins, and small utilities.",
        sections: &[
            "Plugin Info",
            "Plugin APIs",
            "Plugin Management",
            "Utilities",
        ],
    },
    PageSpec {
        file: "status-logging.md",
        title: "Status, Logging & Translation",
        intro: "Show messages in the status bar, write to the log, and translate \
                your plugin's text.",
        sections: &["Status Bar", "Logging", "Translation"],
    },
    PageSpec {
        file: "buffer.md",
        title: "Buffers & Editing",
        intro: "Read and change buffer text, move cursors, open, save and close \
                files, and search.",
        sections: &[
            "Buffer Queries",
            "Cursors & Viewport",
            "Text Editing",
            "Opening, Saving & Closing",
            "Clipboard",
            "Search & Replace",
            "Diff Baselines",
        ],
    },
    PageSpec {
        file: "ui.md",
        title: "Commands, Prompts & Dialogs",
        intro: "Add commands and key bindings, and ask the user for input with \
                prompts, dialogs, popups and single keys. For a tour of the input \
                options with screenshots, see \
                [Asking for Input](../examples/asking-for-input/).",
        sections: &[
            "Commands",
            "Modes & Keybindings",
            "Macros",
            "Prompts",
            "Reading Keys",
            "Dialogs & Widgets",
            "Popups & Menus",
        ],
    },
    PageSpec {
        file: "overlays.md",
        title: "Decorations",
        intro: "Change how buffer text looks without changing the text: colours, \
                virtual text, hidden ranges, folds, gutter marks and more.",
        sections: &[
            "Overlays",
            "Virtual Text",
            "Conceals",
            "Soft Breaks & Layout Hints",
            "Folds",
            "Gutter & Line Display",
            "Scrollbar Markers",
            "Markers",
            "Highlights",
            "Text Measurement",
            "File Explorer",
            "Animations",
        ],
    },
    PageSpec {
        file: "virtual-buffers.md",
        title: "Virtual Buffers & Panels",
        intro: "Buffers whose content a plugin writes, such as result lists and \
                side panels, and groups of them shown as one tab.",
        sections: &[
            "Virtual Buffers",
            "Buffer Groups",
            "Composite Buffers",
            "Scroll Sync",
            "View State",
        ],
    },
    PageSpec {
        file: "windows.md",
        title: "Windows, Splits & Terminals",
        intro: "Split the screen, manage windows (workspaces), and run terminals.",
        sections: &["Splits", "Windows", "Terminals"],
    },
    PageSpec {
        file: "filesystem.md",
        title: "Files, Paths & Environment",
        intro: "Read and write files, work with paths, watch for changes, read the \
                environment, store plugin data, and reach other machines.",
        sections: &[
            "Files",
            "Paths",
            "File Watching",
            "Environment",
            "Plugin Storage",
            "Remote & Authority",
            "Machines",
        ],
    },
    PageSpec {
        file: "processes.md",
        title: "Processes, Timers & Network",
        intro: "Run programs, wait or repeat on a timer, and fetch over HTTP.",
        sections: &["Processes", "Timers", "Network"],
    },
    PageSpec {
        file: "config.md",
        title: "Config, Themes & Languages",
        intro: "Plugin settings, editor config, directories, themes, grammars and \
                language servers.",
        sections: &["Config", "Directories", "Themes", "Languages & LSP"],
    },
    PageSpec {
        file: "events.md",
        title: "Events & Hooks",
        intro: "Run code when something happens in the editor. Subscribe with \
                `editor.on(event, handler)`; the handler receives the event's \
                payload, listed under [Events](#events) below.",
        sections: &["Event Handlers"],
    },
];

/// Text shown under a section heading, before its methods. For what
/// belongs to a whole section rather than to one method.
const SECTION_INTROS: &[(&str, &str)] = &[
    (
        "Dialogs & Widgets",
        "A dialog is a floating panel built from widgets: text fields, checkboxes,
dropdowns, buttons and labels. The editor draws it and handles typing, focus
and Tab. The plugin hears what the user does through the
[`widget_event`](./events#widget-event) hook. The bundled plugins build the
widget spec with the helpers in `plugins/lib/widgets.ts`.",
    ),
    (
        "Windows",
        "A *window* is a project-rooted bundle of editor state: file explorer,
language servers, file watchers, split layout and open buffers. It can be
swapped in and out as a unit. The window at startup is window 1; plugins
create more. The Orchestrator plugin uses windows to run agents in parallel
worktrees and shows them as *sessions*. The API calls them windows
(`Window`, `windowId`) because Fresh already uses \"session\" for workspace
recovery and config layers. See `docs/internal/orchestrator-sessions-design.md`
for the design.",
    ),
    (
        "Plugin Storage",
        "Plugins can't delete, move or overwrite a path they name. There is no
`removePath`, `renamePath` or `copyPath`. The calls below name a *thing*
instead (a staging directory the editor issued, a package, a state entry),
and the editor works out the path itself.

This closes real holes. `removePath` once checked only its top-level
argument, so a symlink inside the target let a recursive delete escape.
`renamePath` had no check and fell back to copy-then-delete, so anything
`removePath` refused could be moved elsewhere and deleted there. Now nothing a
plugin passes decides what gets removed.

Removals a user would notice (replacing or uninstalling a package, deleting a
theme) go to the system trash, so they can be recovered. Staging directories
are the editor's own working space and are deleted outright.",
    ),
    (
        "Scrollbar Markers",
        "Paint coloured marks on a split's vertical scrollbar, at positions
proportional to where they are in the buffer, like an overview ruler. Use them
with a line highlight (`addOverlay` with `extendToLineEnd`) and a gutter mark
(`setLineIndicator`), so marked content can be found even when it is scrolled
off screen.

Markers are anchored by byte offset, so they move with edits and stay correct
between refreshes. They work the same on a ten-line file and a
multi-gigabyte one: when line numbers aren't known yet, marks are placed by
byte ratio instead.",
    ),
];

const TYPES_FILE: &str = "types.md";
const EVENTS_FILE: &str = "events.md";
const RUNTIME_FILE: &str = "runtime.md";

/// Interfaces that are rendered elsewhere, not on the types page.
const NOT_TYPES: &[&str] = &["EditorAPI", "HookEventMap"];

/// A method of `EditorAPI`, merged across every `interface EditorAPI` block.
#[derive(Default)]
struct Method {
    docs: Vec<String>,
    signatures: Vec<String>,
}

/// A named top-level declaration (type, interface or global function).
struct Decl {
    name: String,
    doc: String,
    code: String,
}

/// One entry of `HookEventMap`.
struct Event {
    name: String,
    group: String,
    doc: String,
    code: String,
}

/// Render every page from the formatted `fresh.d.ts` text.
///
/// `sections` maps each `EditorAPI` method name to its section.
pub fn render(dts: &str, sections: &HashMap<&str, &str>) -> Result<Vec<Page>, String> {
    let unknown: Vec<&str> = SECTION_INTROS
        .iter()
        .map(|(s, _)| *s)
        .filter(|s| !METHOD_PAGES.iter().any(|p| p.sections.contains(s)))
        .collect();
    if !unknown.is_empty() {
        return Err(format!(
            "SECTION_INTROS names sections that are not on any page: {}",
            unknown.join(", ")
        ));
    }
    render_pages(dts, sections, METHOD_PAGES)
}

fn render_pages(
    dts: &str,
    sections: &HashMap<&str, &str>,
    page_specs: &[PageSpec],
) -> Result<Vec<Page>, String> {
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, dts, SourceType::d_ts()).parse();
    if !parsed.errors.is_empty() {
        return Err(format!("fresh.d.ts does not parse: {:?}", parsed.errors));
    }
    let program = parsed.program;
    let comments: &[Comment] = &program.comments;

    let mut method_order: Vec<String> = Vec::new();
    let mut methods: HashMap<String, Method> = HashMap::new();
    let mut types: Vec<Decl> = Vec::new();
    let mut globals: Vec<Decl> = Vec::new();
    let mut events: Vec<Event> = Vec::new();

    for stmt in &program.body {
        match stmt {
            Statement::TSInterfaceDeclaration(iface) => {
                let name = iface.id.name.as_str();
                if name == "EditorAPI" {
                    for member in &iface.body.body {
                        let TSSignature::TSMethodSignature(sig) = member else {
                            continue;
                        };
                        let Some(key) = sig.key.static_name() else {
                            continue;
                        };
                        let key = key.to_string();
                        if !methods.contains_key(&key) {
                            method_order.push(key.clone());
                        }
                        let entry = methods.entry(key).or_default();
                        let doc = leading_doc(dts, comments, sig.span);
                        if !doc.is_empty() && !entry.docs.contains(&doc) {
                            entry.docs.push(doc);
                        }
                        entry.signatures.push(code_of(dts, sig.span));
                    }
                } else if name == "HookEventMap" {
                    events = collect_events(dts, comments, &iface.body.body);
                } else {
                    types.push(Decl {
                        name: name.to_string(),
                        doc: leading_doc(dts, comments, iface.span),
                        code: code_of(dts, iface.span),
                    });
                }
            }
            Statement::TSTypeAliasDeclaration(alias) => types.push(Decl {
                name: alias.id.name.to_string(),
                doc: leading_doc(dts, comments, alias.span),
                code: code_of(dts, alias.span),
            }),
            Statement::FunctionDeclaration(func) => {
                if let Some(id) = &func.id {
                    globals.push(Decl {
                        name: id.name.to_string(),
                        doc: leading_doc(dts, comments, func.span),
                        code: code_of(dts, func.span),
                    });
                }
            }
            _ => {}
        }
    }
    types.retain(|t| !NOT_TYPES.contains(&t.name.as_str()));
    types.sort_by_key(|t| t.name.to_lowercase());

    // Group methods: section -> names, in fresh.d.ts order.
    let mut by_section: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    let mut unsectioned = Vec::new();
    for name in &method_order {
        match sections.get(name.as_str()) {
            Some(section) => by_section.entry(section).or_default().push(name),
            None => unsectioned.push(name.as_str()),
        }
    }
    if !unsectioned.is_empty() {
        return Err(format!(
            "these EditorAPI methods have no API reference section: {}. Add \
             `#[plugin_api(section = \"...\")]` in quickjs_backend.rs, or an entry in \
             TRAILER_SECTIONS for methods declared in ts_export.rs",
            unsectioned.join(", ")
        ));
    }
    let placed: Vec<&str> = page_specs
        .iter()
        .flat_map(|p| p.sections.iter().copied())
        .collect();
    let unplaced: Vec<&str> = by_section
        .keys()
        .copied()
        .filter(|s| !placed.contains(s))
        .collect();
    if !unplaced.is_empty() {
        return Err(format!(
            "these API sections are not on any page: {}. Add them to METHOD_PAGES in api_docs.rs",
            unplaced.join(", ")
        ));
    }
    let empty: Vec<&str> = placed
        .iter()
        .copied()
        .filter(|s| !by_section.contains_key(s))
        .collect();
    if !empty.is_empty() {
        return Err(format!(
            "these sections in METHOD_PAGES have no methods: {}",
            empty.join(", ")
        ));
    }

    let mut pages = Vec::new();
    for spec in page_specs {
        let mut out = page_header(spec.title, spec.intro);
        if spec.file == RUNTIME_FILE {
            out.push_str("## Globals\n\n");
            for g in &globals {
                render_entry(
                    &mut out,
                    &g.name,
                    std::slice::from_ref(&g.doc),
                    std::slice::from_ref(&g.code),
                );
            }
        }
        for section in spec.sections {
            out.push_str(&format!("## {}\n\n", section));
            if let Some((_, intro)) = SECTION_INTROS.iter().find(|(s, _)| s == section) {
                out.push_str(&escape_prose(intro));
                out.push_str("\n\n");
            }
            for name in &by_section[section] {
                let m = &methods[*name];
                render_entry(&mut out, name, &m.docs, &m.signatures);
            }
        }
        if spec.file == EVENTS_FILE {
            render_events(&mut out, &events);
        }
        out.push_str(PAGE_FOOTER);
        pages.push(Page {
            file: spec.file,
            content: out,
        });
    }

    let mut out = page_header(
        "Types",
        "The data types the API takes and returns, in alphabetical order.",
    );
    for t in &types {
        render_entry(
            &mut out,
            &t.name,
            std::slice::from_ref(&t.doc),
            std::slice::from_ref(&t.code),
        );
    }
    out.push_str(PAGE_FOOTER);
    pages.push(Page {
        file: TYPES_FILE,
        content: out,
    });

    Ok(pages)
}

/// The VitePress sidebar entries for the generated pages, in page order.
pub fn sidebar_entries() -> Vec<(&'static str, &'static str)> {
    let mut v: Vec<_> = METHOD_PAGES.iter().map(|p| (p.title, p.file)).collect();
    v.push(("Types", TYPES_FILE));
    v
}

const PAGE_FOOTER: &str = ":::\n";

fn page_header(title: &str, intro: &str) -> String {
    // `v-pre` keeps Vue from reading `{{ }}` in doc text as template syntax.
    format!(
        "<!-- Generated from the plugin API source. Do not edit: change the doc \
         comments in the Rust source, then run `{REGENERATE_COMMAND}`. -->\n\n\
         # {title}\n\n{intro}\n\n::: v-pre\n\n"
    )
}

fn render_entry(out: &mut String, name: &str, docs: &[String], code: &[String]) {
    out.push_str(&format!("### `{}`\n\n", name));
    let mut params: Vec<(String, String)> = Vec::new();
    let mut returns: Vec<String> = Vec::new();
    let mut bodies: Vec<String> = Vec::new();
    for doc in docs.iter().filter(|d| !d.is_empty()) {
        let parsed = split_tags(doc);
        if !parsed.body.is_empty() {
            bodies.push(parsed.body);
        }
        params.extend(parsed.params);
        returns.extend(parsed.returns);
    }
    for body in &bodies {
        out.push_str(&escape_prose(body));
        out.push_str("\n\n");
    }
    out.push_str("```typescript\n");
    out.push_str(&code.join("\n"));
    out.push_str("\n```\n\n");
    if !params.is_empty() {
        out.push_str("| Parameter | Description |\n|-----------|-------------|\n");
        for (name, desc) in &params {
            out.push_str(&format!(
                "| `{}` | {} |\n",
                name,
                escape_prose(desc).replace('|', "\\|").replace('\n', " ")
            ));
        }
        out.push('\n');
    }
    for r in &returns {
        out.push_str(&format!("**Returns:** {}\n\n", escape_prose(r)));
    }
}

fn render_events(out: &mut String, events: &[Event]) {
    out.push_str("## Events\n\n");
    let mut group = None;
    for e in events {
        if group != Some(&e.group) {
            out.push_str(&format!("### {}\n\n", e.group));
            group = Some(&e.group);
        }
        out.push_str(&format!("#### `{}`\n\n", e.name));
        if !e.doc.is_empty() {
            out.push_str(&escape_prose(&split_tags(&e.doc).body));
            out.push_str("\n\n");
        }
        out.push_str(&format!("```typescript\n{}\n```\n\n", e.code));
    }
}

fn collect_events(dts: &str, comments: &[Comment], members: &[TSSignature]) -> Vec<Event> {
    let mut events = Vec::new();
    let mut group = String::from("Other");
    let mut prev_end = 0u32;
    for member in members {
        let span = member.span();
        // A `// ── name ──` line comment since the previous member starts a group.
        for c in comments {
            if c.is_line() && c.span.start >= prev_end && c.span.end <= span.start {
                let text = &dts[c.content_span().start as usize..c.content_span().end as usize];
                let text = text.trim();
                if text.starts_with('─') {
                    let name = text.trim_matches('─').trim();
                    if !name.is_empty() {
                        group = capitalize(name);
                    }
                }
            }
        }
        prev_end = span.end;
        let TSSignature::TSPropertySignature(prop) = member else {
            continue;
        };
        let Some(name) = prop.key.static_name() else {
            continue;
        };
        events.push(Event {
            name: name.to_string(),
            group: group.clone(),
            doc: leading_doc(dts, comments, span),
            code: code_of(dts, span),
        });
    }
    events
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

/// Source text of a node, de-indented, with tabs as two spaces.
fn code_of(dts: &str, span: Span) -> String {
    let raw = &dts[span.start as usize..span.end as usize];
    // The first line starts at the node; later lines carry the
    // surrounding indentation, which is one tab per nesting level.
    let indent = dts[..span.start as usize]
        .rsplit('\n')
        .next()
        .map(|l| l.len() - l.trim_start_matches('\t').len())
        .unwrap_or(0);
    raw.lines()
        .enumerate()
        .map(|(i, line)| {
            let line = if i == 0 {
                line
            } else {
                let tabs = line.len() - line.trim_start_matches('\t').len();
                &line[tabs.min(indent)..]
            };
            line.replace('\t', "  ")
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// The `/** ... */` comment directly before `span`, as plain text.
fn leading_doc(dts: &str, comments: &[Comment], span: Span) -> String {
    let Some(c) = comments
        .iter()
        .rev()
        .find(|c| c.span.end <= span.start && c.is_block())
    else {
        return String::new();
    };
    let between = &dts[c.span.end as usize..span.start as usize];
    let text = &dts[c.span.start as usize..c.span.end as usize];
    if !between.trim().is_empty() || !text.starts_with("/**") {
        return String::new();
    }
    jsdoc_text(text)
}

/// Strip `/**`, `*/` and the leading `*` of each line.
fn jsdoc_text(comment: &str) -> String {
    let inner = comment
        .strip_prefix("/**")
        .and_then(|s| s.strip_suffix("*/"))
        .unwrap_or(comment);
    let lines: Vec<String> = inner
        .lines()
        .map(|line| {
            let t = line.trim_start();
            let t = t.strip_prefix('*').unwrap_or(t);
            t.strip_prefix(' ').unwrap_or(t).trim_end().to_string()
        })
        .collect();
    let text = lines.join("\n").replace("*\\/", "*/");
    text.trim_matches('\n').to_string()
}

struct Tags {
    body: String,
    params: Vec<(String, String)>,
    returns: Vec<String>,
}

/// Split `@param` / `@returns` tags out of a doc text. Tags inside a
/// fenced code block are left alone.
fn split_tags(doc: &str) -> Tags {
    let mut body = Vec::new();
    let mut params: Vec<(String, String)> = Vec::new();
    let mut returns: Vec<String> = Vec::new();
    // 0 = body, 1 = last param, 2 = last returns
    let mut cont = 0;
    let mut in_fence = false;
    for line in doc.lines() {
        if line.trim_start().starts_with("```") {
            in_fence = !in_fence;
        }
        if !in_fence {
            if let Some(rest) = line.strip_prefix("@param ") {
                let rest = rest.trim();
                let (name, desc) = rest.split_once(char::is_whitespace).unwrap_or((rest, ""));
                let desc = desc.trim_start();
                let desc = desc.strip_prefix("- ").unwrap_or(desc);
                params.push((name.trim_matches(['[', ']']).to_string(), desc.to_string()));
                cont = 1;
                continue;
            }
            if let Some(rest) = line
                .strip_prefix("@returns ")
                .or_else(|| line.strip_prefix("@return "))
            {
                returns.push(rest.trim().to_string());
                cont = 2;
                continue;
            }
            if cont != 0 && !line.trim().is_empty() && !line.starts_with('@') {
                let target = if cont == 1 {
                    &mut params.last_mut().unwrap().1
                } else {
                    returns.last_mut().unwrap()
                };
                target.push(' ');
                target.push_str(line.trim());
                continue;
            }
        }
        cont = 0;
        body.push(line);
    }
    Tags {
        body: body.join("\n").trim().to_string(),
        params,
        returns,
    }
}

/// Escape `<` and `{` outside code, so VitePress reads neither an HTML tag
/// nor a trailing `{...}` attribute list.
fn escape_prose(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut in_fence = false;
    for (i, line) in text.lines().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        if line.trim_start().starts_with("```") {
            in_fence = !in_fence;
            out.push_str(line);
            continue;
        }
        if in_fence {
            out.push_str(line);
            continue;
        }
        let mut in_code = false;
        for c in line.chars() {
            match c {
                '`' => {
                    in_code = !in_code;
                    out.push(c);
                }
                '<' if !in_code => out.push_str("&lt;"),
                '{' if !in_code => out.push_str("&#123;"),
                _ => out.push(c),
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"/** Get the editor. */
declare function getEditor(): EditorAPI;
/** A thing. */
type Thing = {
	id: number;
};
interface EditorAPI {
	/**
	* Ask for text.
	*
	* @param label - Shown before the input
	* @param initial - Text already typed
	*/
	prompt(label: string, initial: string): Promise<string | null>;
	noDoc(): void;
}
interface EditorAPI {
	/** Typed form. */
	prompt<T>(label: T): Promise<T>;
}
interface HookEventMap {
	// ── prompts ──────
	// A plain note, not a group.
	/** Enter pressed. */
	prompt_confirmed: {
		input: string;
	};
}
"#;

    fn sample_sections() -> HashMap<&'static str, &'static str> {
        HashMap::from([("prompt", "Prompts"), ("noDoc", "Commands")])
    }

    const SAMPLE_PAGES: &[PageSpec] = &[
        PageSpec {
            file: "runtime.md",
            title: "Runtime",
            intro: "",
            sections: &[],
        },
        PageSpec {
            file: "ui.md",
            title: "UI",
            intro: "",
            sections: &["Commands", "Prompts"],
        },
        PageSpec {
            file: "events.md",
            title: "Events",
            intro: "",
            sections: &[],
        },
    ];

    #[test]
    fn jsdoc_text_strips_stars() {
        assert_eq!(jsdoc_text("/**\n\t* a\n\t*\n\t* b *\\/\n\t*/"), "a\n\nb */");
        assert_eq!(jsdoc_text("/** one line */"), "one line");
    }

    #[test]
    fn split_tags_reads_params_and_returns() {
        let t = split_tags("Body.\n\n@param a - first\n  more\n@param b second\n@returns ok");
        assert_eq!(t.body, "Body.");
        assert_eq!(
            t.params,
            vec![
                ("a".to_string(), "first more".to_string()),
                ("b".to_string(), "second".to_string())
            ]
        );
        assert_eq!(t.returns, vec!["ok".to_string()]);
    }

    #[test]
    fn escape_prose_leaves_code_alone() {
        assert_eq!(
            escape_prose("a <b> {x} `c<d>{y}`\n```\n<e>{z}\n```"),
            "a &lt;b> &#123;x} `c<d>{y}`\n```\n<e>{z}\n```"
        );
    }

    #[test]
    fn render_groups_methods_types_and_events() {
        let pages = render_pages(SAMPLE, &sample_sections(), SAMPLE_PAGES).expect("render");
        let ui = &pages.iter().find(|p| p.file == "ui.md").unwrap().content;
        assert!(
            ui.contains("## Commands\n\n### `noDoc`\n\n```typescript\nnoDoc(): void;\n```"),
            "{ui}"
        );
        assert!(ui.contains("## Prompts\n\n### `prompt`\n\nAsk for text.\n\nTyped form."));
        assert!(ui.contains(
            "prompt(label: string, initial: string): Promise<string | null>;\nprompt<T>(label: T): Promise<T>;"
        ));
        assert!(ui.contains("| `label` | Shown before the input |"));
        let types = &pages.iter().find(|p| p.file == "types.md").unwrap().content;
        assert!(
            types.contains(
                "### `Thing`\n\nA thing.\n\n```typescript\ntype Thing = {\n  id: number;\n};\n```"
            ),
            "{types}"
        );
        let events = &pages
            .iter()
            .find(|p| p.file == "events.md")
            .unwrap()
            .content;
        assert!(
            events.contains("### Prompts\n\n#### `prompt_confirmed`\n\nEnter pressed."),
            "{events}"
        );
        let runtime = &pages
            .iter()
            .find(|p| p.file == "runtime.md")
            .unwrap()
            .content;
        assert!(
            runtime.contains("### `getEditor`\n\nGet the editor."),
            "{runtime}"
        );
    }

    /// Every generated page is in the docs sidebar, in page order.
    #[test]
    fn sidebar_lists_every_generated_page() {
        let config = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../docs/.vitepress/config.ts");
        let config = std::fs::read_to_string(&config).expect("read docs/.vitepress/config.ts");
        let mut last = 0;
        for (title, file) in sidebar_entries() {
            let link = format!("link: \"/plugins/api/{}\"", file.trim_end_matches(".md"));
            let pos = config
                .find(&link)
                .unwrap_or_else(|| panic!("docs sidebar has no entry for {title} ({link})"));
            assert!(pos > last, "docs sidebar lists {title} out of order");
            last = pos;
        }
    }

    #[test]
    fn render_rejects_bad_sections() {
        let mut sections = sample_sections();
        sections.remove("noDoc");
        let err = render_pages(SAMPLE, &sections, SAMPLE_PAGES)
            .err()
            .unwrap_or_default();
        assert!(err.contains("noDoc"), "{err}");

        sections.insert("noDoc", "Nowhere");
        let err = render_pages(SAMPLE, &sections, SAMPLE_PAGES)
            .err()
            .unwrap_or_default();
        assert!(err.contains("Nowhere"), "{err}");

        sections.insert("noDoc", "Prompts");
        let err = render_pages(SAMPLE, &sections, SAMPLE_PAGES)
            .err()
            .unwrap_or_default();
        assert!(err.contains("Commands"), "{err}");
    }
}
