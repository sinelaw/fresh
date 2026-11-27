use super::*;
use crate::structure::{RegionRole, TableLine, TableRole};
use crate::MAX_LINE_BYTES;
use proptest::prelude::*;
use std::sync::OnceLock;
use syntect::parsing::SyntaxDefinition;

/// The editor's own grammars for the hosts of embedded regions syntect does
/// not bundle: its unified diff (file regions) and Vue (`<script>` and
/// `<style>` blocks). Markdown's fences are in syntect's own grammar.
const HOST_GRAMMARS: [&str; 2] = [
    include_str!("../../../fresh-editor-core/src/grammars/diff.sublime-syntax"),
    include_str!("../../../fresh-editor-core/src/grammars/vue.sublime-syntax"),
];

/// Syntect's bundled syntaxes and [`HOST_GRAMMARS`].
fn syntaxes() -> SyntaxSet {
    static SET: OnceLock<SyntaxSet> = OnceLock::new();
    SET.get_or_init(|| {
        let mut builder = SyntaxSet::load_defaults_newlines().into_builder();
        for grammar in HOST_GRAMMARS {
            builder.add(SyntaxDefinition::load_from_str(grammar, true, None).unwrap());
        }
        builder.build()
    })
    .clone()
}

fn highlighter() -> Highlighter {
    Highlighter::new(syntaxes())
}

/// Work until nothing remains.
fn finish(h: &mut Highlighter) {
    while h.work(64 * 1024).unwrap() {}
}

/// Every line's highlight and roles, in order.
fn highlights(h: &Highlighter, doc: u32) -> Vec<(Option<Highlight>, Roles)> {
    h.documents[&doc]
        .lines
        .iter()
        .map(|l| (l.highlight.clone(), l.roles))
        .collect()
}

/// The highlight and roles of `text` parsed in one go in `syntax`.
fn fresh(syntax: &str, text: &str) -> Vec<(Option<Highlight>, Roles)> {
    let mut h = highlighter();
    h.open(0, syntax, text).unwrap();
    finish(&mut h);
    highlights(&h, 0)
}

/// Every line's runs, once all is highlighted.
fn runs(h: &Highlighter, doc: u32) -> Vec<Vec<Run>> {
    highlights(h, doc)
        .into_iter()
        .map(|(highlight, _)| highlight.expect("every line is highlighted").runs)
        .collect()
}

/// Every line's roles.
fn roles(h: &Highlighter, doc: u32) -> Vec<Roles> {
    highlights(h, doc)
        .into_iter()
        .map(|(_, roles)| roles)
        .collect()
}

/// The runs of each line of `text`, which ends with `\n`, parsed on its own
/// in `syntax` (without the empty line after the last `\n`).
fn alone(syntax: &str, text: &str) -> Vec<Vec<Run>> {
    let mut h = highlighter();
    h.open(0, syntax, text).unwrap();
    finish(&mut h);
    let mut lines = runs(&h, 0);
    assert_eq!(lines.pop(), Some(Vec::new()));
    lines
}

/// The index of the line of `text` that is `line`.
fn line_of(text: &str, line: &str) -> usize {
    text.lines().position(|l| l == line).unwrap()
}

#[test]
fn an_unknown_syntax_is_refused() {
    let mut h = highlighter();
    assert!(matches!(
        h.open(1, "No Such Language", "x"),
        Err(Error::UnknownSyntax(_))
    ));
}

#[test]
fn an_edit_inside_a_character_is_refused() {
    let mut h = highlighter();
    h.open(1, "Rust", "é").unwrap();
    assert!(matches!(
        h.edit(1, 1, 0, "x"),
        Err(Error::NotCharBoundary { offset: 1, .. })
    ));
    assert!(matches!(h.edit(1, 0, 3, ""), Err(Error::OutOfRange { .. })));
}

#[test]
fn take_reports_each_changed_line_once() {
    let mut h = highlighter();
    h.open(1, "Rust", "fn main() {}\n// done\n").unwrap();
    finish(&mut h);
    let taken = h.take(1).unwrap();
    assert_eq!(taken.iter().map(|l| l.line).collect::<Vec<_>>(), [0, 1, 2]);
    assert!(taken.iter().all(|l| l.exact));
    assert_eq!(
        taken[1].runs,
        [Run {
            start: 0,
            len: 7,
            cat: Category::Comment,
            background: None,
        }]
    );
    assert!(h.take(1).unwrap().is_empty());
}

#[test]
fn a_long_line_is_highlighted_up_to_the_cut_and_the_next_goes_on_from_there() {
    // The string opened on line 0 is still open where the line is cut, so
    // line 1 continues it, as it does natively.
    let text = format!("let s = \"{}\";\nlet y = 2;\n", "a".repeat(30_000));
    let mut h = highlighter();
    h.open(1, "Rust", &text).unwrap();
    finish(&mut h);
    let taken = h.take(1).unwrap();
    let end = |l: &LineRuns| l.runs.last().map_or(0, |r| r.start + r.len);
    assert_eq!(end(&taken[0]) as usize, MAX_LINE_BYTES);
    assert_eq!(
        taken[1].runs,
        [Run {
            start: 0,
            len: 10,
            cat: Category::String,
            background: None,
        }]
    );
}

#[test]
fn a_far_wanted_range_is_provisional_until_the_exact_parse_reaches_it() {
    // Everything after the first line is in a block comment, which a parse
    // starting mid-file cannot know.
    let text = format!("/*\n{}", "let x = 1;\n".repeat(30_000));
    let mut h = highlighter();
    h.open(1, "Rust", &text).unwrap();
    h.want(1, 29_000, 29_010).unwrap();
    assert!(h.work(1).unwrap());
    let provisional = h.take(1).unwrap();
    assert_eq!(
        provisional.iter().map(|l| l.line).collect::<Vec<_>>(),
        (29_000..29_010).collect::<Vec<_>>()
    );
    assert!(provisional
        .iter()
        .all(|l| !l.exact && l.roles == Roles::Unknown));
    assert!(provisional
        .iter()
        .all(|l| l.runs.iter().all(|r| r.cat != Category::Comment)));

    finish(&mut h);
    let exact: Vec<LineRuns> = h
        .take(1)
        .unwrap()
        .into_iter()
        .filter(|l| (29_000..29_010).contains(&l.line))
        .collect();
    assert_eq!(exact.len(), 10);
    let comment = [Run {
        start: 0,
        len: 10,
        cat: Category::Comment,
        background: None,
    }];
    assert!(exact
        .iter()
        .all(|l| l.exact && l.runs == comment && l.roles == NO_ROLES));
}

#[test]
fn an_edit_far_from_the_end_converges_without_reparsing_the_rest() {
    let text = "fn f() { let x = 1; }\n".repeat(2_000);
    let mut h = highlighter();
    h.open(1, "Rust", &text).unwrap();
    finish(&mut h);
    h.take(1).unwrap();
    h.edit(1, 3, 0, "g").unwrap();
    // Far less than the rest of the file: it must converge to finish.
    assert!(!h.work(4 * 1024).unwrap());
    assert_eq!(
        h.take(1)
            .unwrap()
            .iter()
            .map(|l| l.line)
            .collect::<Vec<_>>(),
        [0]
    );
}

const RUST_BODY: &str = "/* a\n   comment */\nfn main() {\n    let s = \"hi\"; // note\n}\n";
const PYTHON_BODY: &str = "def f(x):\n    \"\"\"doc\n    string\"\"\"\n    return x + 1  # inc\n";

#[test]
fn markdown_fences_are_coloured_as_their_body_parsed_alone_in_their_language() {
    let text = format!(
        "# Title\n\nSome *prose*.\n\n```rust\n{RUST_BODY}```\n\nMore prose.\n\n```python\n{PYTHON_BODY}```\n"
    );
    let mut h = highlighter();
    h.open(1, "Markdown", &text).unwrap();
    finish(&mut h);
    let lines = runs(&h, 1);

    let rust = alone("Rust", RUST_BODY);
    let rust_at = line_of(&text, "```rust") + 1;
    assert_eq!(lines[rust_at..rust_at + rust.len()], rust);
    let python = alone("Python", PYTHON_BODY);
    let python_at = line_of(&text, "```python") + 1;
    assert_eq!(lines[python_at..python_at + python.len()], python);

    // Rust's colours, not the fence's raw-code one.
    let categories: Vec<Category> = rust.iter().flatten().map(|r| r.cat).collect();
    for category in [Category::Keyword, Category::String, Category::Comment] {
        assert!(
            categories.contains(&category),
            "no {category:?} in {rust:?}"
        );
    }
}

#[test]
fn a_fence_in_no_known_language_keeps_the_hosts_raw_code_colour() {
    let body = "fn answer() { 42 }";
    let unknown = format!("```definitely-not-a-language\n{body}\n```\n");
    let bare = format!("```\n{body}\n```\n");
    let body_runs = |text: &str| {
        let mut h = highlighter();
        h.open(1, "Markdown", text).unwrap();
        finish(&mut h);
        runs(&h, 1)[1].clone()
    };
    let raw = body_runs(&bare);
    assert_eq!(body_runs(&unknown), raw);
    assert!(matches!(raw[..], [Run { start: 0, len, .. }] if len as usize == body.len()));
}

#[test]
fn changing_a_fences_language_recolours_its_block_and_converges_after_it() {
    let body_line = "let x = 1; // c\n";
    // Long enough for checkpoints inside the block: there the host's state
    // is the same in either language, the child's is not.
    let body = body_line.repeat(40);
    let text = format!("```rust\n{body}```\n{}", "Some prose here.\n".repeat(2_000));
    let mut h = highlighter();
    h.open(1, "Markdown", &text).unwrap();
    finish(&mut h);
    h.take(1).unwrap();
    let rust = alone("Rust", body_line).remove(0);
    let python = alone("Python", body_line).remove(0);
    assert_ne!(rust, python);
    assert!(runs(&h, 1)[1..=40].iter().all(|line| *line == rust));

    h.edit(1, "```".len(), "rust".len(), "python").unwrap();
    // Far less than the rest of the file: it must converge to finish.
    assert!(!h.work(8 * 1024).unwrap());
    let changed: Vec<u32> = h.take(1).unwrap().iter().map(|l| l.line).collect();
    assert_eq!(changed, (0..=40).collect::<Vec<u32>>());
    assert!(runs(&h, 1)[1..=40].iter().all(|line| *line == python));
    let text = text.replacen("```rust", "```python", 1);
    assert_eq!(highlights(&h, 1), fresh("Markdown", &text));
}

/// A unified diff source row's runs as its child parser gave them: without
/// the row's wash (`wash`, with no background: the marker and the gaps
/// between tokens), each checked to sit on the wash, and back at the
/// source's offsets.
fn child_runs(row: &[Run], wash: Option<Category>) -> Vec<Run> {
    row.iter()
        .filter(|r| !(Some(r.cat) == wash && r.background.is_none()))
        .map(|r| {
            assert_eq!(r.background, wash, "{r:?}");
            Run {
                start: r.start - 1,
                background: None,
                ..*r
            }
        })
        .collect()
}

/// Whether `row`'s runs cover its first `len` bytes without a gap.
fn covers(row: &[Run], len: usize) -> bool {
    let mut at = 0;
    for run in row {
        if run.start != at {
            return false;
        }
        at += run.len;
    }
    at as usize == len
}

#[test]
fn a_diff_colours_each_files_rows_in_its_language_on_the_rows_wash() {
    let text = "\
diff --git a/src/lib.rs b/src/lib.rs
--- a/src/lib.rs
+++ b/src/lib.rs
@@ -1,2 +1,2 @@ fn section
 fn same() {}
-pub fn old() -> u32 { 1 }
+pub fn new() -> u32 { 2 }
diff --git a/tools/check.py b/tools/check.py
--- a/tools/check.py
+++ b/tools/check.py
@@ -1 +1 @@
+def new(): pass
";
    let mut h = highlighter();
    h.open(1, "Fresh Diff", text).unwrap();
    finish(&mut h);
    let lines = runs(&h, 1);
    let line = |l: &str| line_of(text, l);

    // The hunk header is the host's, washed to its end past the section
    // name the grammar leaves unscoped.
    let header = "@@ -1,2 +1,2 @@ fn section";
    assert_eq!(
        lines[line(header)],
        [Run {
            start: 0,
            len: header.len() as u32,
            cat: Category::Changed,
            background: None,
        }]
    );

    // The rows go to one Rust parser, in order, without their markers.
    let rust = alone(
        "Rust",
        "fn same() {}\npub fn old() -> u32 { 1 }\npub fn new() -> u32 { 2 }\n",
    );
    assert_eq!(child_runs(&lines[line(" fn same() {}")], None), rust[0]);
    let old = "-pub fn old() -> u32 { 1 }";
    assert_eq!(
        child_runs(&lines[line(old)], Some(Category::Deleted)),
        rust[1]
    );
    assert!(covers(&lines[line(old)], old.len()));
    let new = "+pub fn new() -> u32 { 2 }";
    assert_eq!(
        child_runs(&lines[line(new)], Some(Category::Inserted)),
        rust[2]
    );
    assert!(covers(&lines[line(new)], new.len()));
    assert_eq!(lines[line(new)][0].cat, Category::Inserted, "the marker");

    // The next file's header switches the child to its language.
    let python = alone("Python", "def new(): pass\n");
    assert_eq!(
        child_runs(&lines[line("+def new(): pass")], Some(Category::Inserted)),
        python[0]
    );
}

#[test]
fn vue_blocks_are_coloured_in_their_default_languages() {
    let script = "const a = \"x\"; // c";
    let style = ".a { color: red; }";
    let text =
        format!("<template>\n  <div/>\n</template>\n<script>\n{script}\n</script>\n<style>\n{style}\n</style>\n");
    let mut h = highlighter();
    h.open(1, "Vue", &text).unwrap();
    finish(&mut h);
    let lines = runs(&h, 1);
    assert_eq!(
        lines[line_of(&text, script)],
        alone("JavaScript", &format!("{script}\n"))[0]
    );
    assert_eq!(
        lines[line_of(&text, style)],
        alone("CSS", &format!("{style}\n"))[0]
    );
}

/// The roles of a line in no region and no table.
const NO_ROLES: Roles = Roles::Known {
    region: None,
    table: None,
};

fn region(role: RegionRole) -> Roles {
    Roles::Known {
        region: Some(role),
        table: None,
    }
}

fn table(role: TableRole, first_row: bool, last: bool) -> Roles {
    Roles::Known {
        region: None,
        table: Some(TableLine {
            role,
            first_row,
            last,
        }),
    }
}

/// Every line's roles in `text`, a Markdown document, once all is parsed.
fn markdown_roles(text: &str) -> Vec<Roles> {
    let mut h = highlighter();
    h.open(1, "Markdown", text).unwrap();
    finish(&mut h);
    roles(&h, 1)
}

/// The native engine's `region_lines_in` case: delimiters open and close,
/// content is body, everything else has no role — a bare fence's
/// delimiters included, whose roles their own text does not say.
#[test]
fn fence_lines_are_classified_as_natively() {
    use RegionRole::{Body, Close, Open};
    let text = "# T\n\n```rust\nlet x = 1;\n```\n\nprose\n\n```\nbare\n```\n\nend\n";
    assert_eq!(
        markdown_roles(text),
        [
            NO_ROLES,
            NO_ROLES,
            region(Open),
            region(Body),
            region(Close),
            NO_ROLES,
            NO_ROLES,
            NO_ROLES,
            region(Open),
            region(Body),
            region(Close),
            NO_ROLES,
            NO_ROLES,
            NO_ROLES,
        ]
    );
}

/// An unclosed fence runs to the document's end, through its last, empty
/// line — a line the native engine keeps no record of.
#[test]
fn an_unclosed_fence_runs_to_the_end() {
    use RegionRole::{Body, Open};
    assert_eq!(
        markdown_roles("```\nbare\n"),
        [region(Open), region(Body), region(Body)]
    );
}

/// The native engine's `structure_lines_in` table cases: the header is the
/// line above the delimiter, the first row the one below it, and the last
/// row the one above the line that closes the table — or the document's
/// last line while the table is open there.
#[test]
fn table_lines_are_classified_as_natively() {
    use TableRole::{Delimiter, Header, Row};
    assert_eq!(
        markdown_roles("intro\n\n| K | V |\n|---|---|\n| a | b |\n| c | d |\n\nafter\n"),
        [
            NO_ROLES,
            NO_ROLES,
            table(Header, false, false),
            table(Delimiter, false, false),
            table(Row, true, false),
            table(Row, false, true),
            NO_ROLES,
            NO_ROLES,
            NO_ROLES,
        ]
    );
    // The empty line after the last `\n` closes the table.
    assert_eq!(
        markdown_roles("intro\n\n| K | V |\n|---|---|\n| a | b |\n"),
        [
            NO_ROLES,
            NO_ROLES,
            table(Header, false, false),
            table(Delimiter, false, false),
            table(Row, true, true),
            NO_ROLES,
        ]
    );
    // Without one, the document's end does.
    assert_eq!(
        markdown_roles("| K |\n|---|\n| a |"),
        [
            table(Header, false, false),
            table(Delimiter, false, false),
            table(Row, true, true),
        ]
    );
}

#[test]
fn regions_and_tables_are_classified_together() {
    use RegionRole::{Body, Close, Open};
    use TableRole::{Delimiter, Header, Row};
    assert_eq!(
        markdown_roles("| K |\n|---|\n| a |\n\n```rust\nlet x = 1;\n```\n\nend\n"),
        [
            table(Header, false, false),
            table(Delimiter, false, false),
            table(Row, true, true),
            NO_ROLES,
            region(Open),
            region(Body),
            region(Close),
            NO_ROLES,
            NO_ROLES,
            NO_ROLES,
        ]
    );
}

/// A line's table role reads the line after it (is that the delimiter? does
/// it close the table?), so it is unknown until that line is exact too —
/// and reported again once it is known.
#[test]
fn a_lines_roles_wait_for_the_line_after_it() {
    let mut h = highlighter();
    h.open(1, "Markdown", "| K |\n|---|\n| a |\n").unwrap();
    assert!(h.work(1).unwrap());
    let first: Vec<(u32, bool, Roles)> = h
        .take(1)
        .unwrap()
        .iter()
        .map(|l| (l.line, l.exact, l.roles))
        .collect();
    assert_eq!(first, [(0, true, Roles::Unknown)]);
    finish(&mut h);
    let rest = h.take(1).unwrap();
    assert_eq!(rest[0].line, 0);
    assert_eq!(rest[0].roles, table(TableRole::Header, false, false));
}

/// A syntax without tables needs no next line: an exact line's roles are
/// known at once.
#[test]
fn without_tables_an_exact_lines_roles_are_known_at_once() {
    let mut h = highlighter();
    h.open(1, "Rust", "fn a() {}\nfn b() {}\n").unwrap();
    assert!(h.work(1).unwrap());
    let first: Vec<(u32, Roles)> = h
        .take(1)
        .unwrap()
        .iter()
        .map(|l| (l.line, l.roles))
        .collect();
    assert_eq!(first, [(0, NO_ROLES)]);
}

/// Through edits that open and close a fence over a table, take its
/// delimiter away and back, split and join it and add tables, each worked
/// in small slices so passes stop mid-table, every line's roles (and runs)
/// are what a fresh parse gives.
#[test]
fn roles_follow_edits_as_a_fresh_parse_gives_them() {
    let mut text = format!(
        "{}| K | V |\n|---|---|\n| a | b |\n| c | d |\n\nprose\n```rust\nlet x = 1;\n```\n{}",
        "intro\n".repeat(200),
        "tail\n".repeat(200),
    );
    let mut h = highlighter();
    h.open(1, "Markdown", &text).unwrap();
    finish(&mut h);
    h.take(1).unwrap();
    // (where, by the text there; bytes deleted; text inserted)
    let edits: [(&str, usize, &str); 8] = [
        ("| K | V |", 0, "```\n"),
        ("```\n| K | V |", 4, ""),
        ("|---|---|\n", 10, ""),
        ("| K | V |\n", 10, "| K | V |\n|---|---|\n"),
        ("| c | d |\n", 0, "\n"),
        ("\n| c | d |", 1, ""),
        ("prose\n", 0, "| x |\n|---|\n"),
        ("tail\ntail\n", 0, "| e |\n|---|\n| f |\n"),
    ];
    for (needle, delete, insert) in edits {
        let at = text.find(needle).unwrap();
        h.edit(1, at, delete, insert).unwrap();
        text.replace_range(at..at + delete, insert);
        while h.work(16).unwrap() {}
        assert_eq!(
            highlights(&h, 1),
            fresh("Markdown", &text),
            "after {needle:?}"
        );
    }
    // And a table at the very end, without a final newline.
    let end = text.len();
    h.edit(1, end, 0, "| g |\n|---|\n| h |").unwrap();
    text.push_str("| g |\n|---|\n| h |");
    while h.work(16).unwrap() {}
    assert_eq!(highlights(&h, 1), fresh("Markdown", &text));
    assert_eq!(
        roles(&h, 1).last(),
        Some(&table(TableRole::Row, true, true))
    );
}

/// A row's runs, their starts less the prefix they were shifted by.
fn unprefixed(row: &[Run], prefix: u32) -> Vec<Run> {
    row.iter()
        .map(|r| Run {
            start: r.start - prefix,
            ..*r
        })
        .collect()
}

/// Rows of a composed document, each feeding the streams given or none,
/// and their regions: one per row that feeds any, in `language`, past
/// `prefix` bytes.
fn composed(
    rows: &[(&str, Option<&[u32]>)],
    language: &str,
    prefix: usize,
) -> (String, Vec<DeclaredRegion>) {
    let text: String = rows.iter().map(|(row, _)| format!("{row}\n")).collect();
    let regions = rows
        .iter()
        .scan(0, |at, (row, streams)| {
            let start = *at;
            *at += row.len() + 1;
            Some(streams.map(|streams| DeclaredRegion {
                start,
                end: *at,
                language: language.to_string(),
                prefix,
                streams: streams.to_vec(),
            }))
        })
        .flatten()
        .collect();
    (text, regions)
}

/// The two sides of a hunk interleave row by row, with a note between
/// them and a context row both share, and each side is coloured as its own
/// rows parsed alone: a docstring opened on an old row is still open on
/// the next old row, while the new rows between are code.
#[test]
fn declared_streams_interleave_and_each_parses_as_its_rows_alone() {
    let old = [
        "def f(x):",
        "    \"\"\"doc",
        "    string\"\"\"",
        "    return z",
    ];
    let new = [
        "def g(y):",
        "    s = '''a",
        "    b'''",
        "    return z",
        "    t = 1",
    ];
    let rows: Vec<(String, Option<&[u32]>)> = vec![
        ("@@ hunk @@".to_string(), None),
        (format!("-{}", old[0]), Some(&[0])),
        (format!("+{}", new[0]), Some(&[1])),
        (format!("-{}", old[1]), Some(&[0])),
        (format!("+{}", new[1]), Some(&[1])),
        ("  [ a note ]".to_string(), None),
        (format!("-{}", old[2]), Some(&[0])),
        (format!("+{}", new[2]), Some(&[1])),
        (format!(" {}", old[3]), Some(&[0, 1])),
        (format!("+{}", new[4]), Some(&[1])),
    ];
    let rows: Vec<(&str, Option<&[u32]>)> =
        rows.iter().map(|(row, s)| (row.as_str(), *s)).collect();
    let (text, regions) = composed(&rows, "py", 1);
    let mut h = highlighter();
    h.open_declared(1, &text).unwrap();
    assert_eq!(
        h.set_regions(1, &regions).unwrap(),
        vec![RegionStatus::Kept; regions.len()]
    );
    finish(&mut h);
    let lines = runs(&h, 1);

    let alone_rows = |rows: &[&str]| {
        alone(
            "Python",
            &rows
                .iter()
                .map(|row| format!("{row}\n"))
                .collect::<String>(),
        )
    };
    let old_alone = alone_rows(&old);
    let new_alone = alone_rows(&new);
    let row = |i: usize| unprefixed(&lines[i], 1);
    assert_eq!(row(1), old_alone[0]);
    assert_eq!(row(3), old_alone[1]);
    assert_eq!(row(6), old_alone[2]);
    assert_eq!(row(2), new_alone[0]);
    assert_eq!(row(4), new_alone[1]);
    assert_eq!(row(7), new_alone[2]);
    // The shared row is coloured by its first stream, and fed to both.
    assert_eq!(row(8), old_alone[3]);
    assert_eq!(row(9), new_alone[4]);
    // Rows outside every region have no runs and advance no parser.
    assert!(lines[0].is_empty() && lines[5].is_empty() && lines[10].is_empty());
    // The docstring really is open across the new rows and the note.
    assert!(row(6).iter().any(|r| r.cat == Category::Comment));
    assert_ne!(row(7), row(6));
}

/// Four streams' parsers are kept: a stream fed again after three others
/// goes on where it was, after four others it starts afresh.
#[test]
fn a_stream_fed_again_after_four_others_starts_afresh() {
    let last_row = |others: &[u32]| {
        let rows: Vec<(&str, Option<&[u32]>)> = std::iter::once(("\"\"\"open", Some(&[0][..])))
            .chain(
                others
                    .iter()
                    .map(|s| ("y = 2", Some(std::slice::from_ref(s)))),
            )
            .chain(std::iter::once(("x = 1", Some(&[0][..]))))
            .collect();
        let (text, regions) = composed(&rows, "py", 0);
        let mut h = highlighter();
        h.open_declared(1, &text).unwrap();
        h.set_regions(1, &regions).unwrap();
        finish(&mut h);
        runs(&h, 1)[rows.len() - 1].clone()
    };
    let in_docstring = last_row(&[1, 2, 3]);
    assert!(in_docstring.iter().all(|r| r.cat == Category::Comment));
    assert_eq!(last_row(&[1, 2, 3, 4]), alone("Python", "x = 1\n")[0]);
}

#[test]
fn declared_regions_report_what_became_of_them() {
    let text = "+def f():\n+x\n";
    let mut h = highlighter();
    h.open_declared(1, text).unwrap();
    let region = |start, end, language: &str| DeclaredRegion {
        start,
        end,
        language: language.to_string(),
        prefix: 1,
        streams: Vec::new(),
    };
    assert_eq!(
        h.set_regions(
            1,
            &[
                region(0, 10, "src/calc.py"),
                region(2, 10, "py"),
                region(10, 13, "definitely-not-a-language"),
                region(13, 13, "py"),
            ],
        )
        .unwrap(),
        [
            RegionStatus::Kept,
            RegionStatus::Overlaps,
            RegionStatus::UnknownLanguage,
            RegionStatus::Empty,
        ]
    );
    finish(&mut h);
    let lines = runs(&h, 1);
    assert!(lines[0]
        .iter()
        .any(|r| r.start == 1 && r.cat == Category::Keyword));
    assert!(
        lines[1].is_empty(),
        "a region in no known language stays plain"
    );

    assert!(matches!(
        h.set_regions(1, &[region(0, 14, "py")]),
        Err(Error::RegionOutOfRange {
            index: 0,
            end: 14,
            len: 13,
            ..
        })
    ));
    h.open(2, "Rust", "fn main() {}").unwrap();
    assert!(matches!(h.set_regions(2, &[]), Err(Error::NotDeclared(2))));
    // Replacing the text clears the regions.
    h.replace(1, text).unwrap();
    finish(&mut h);
    assert!(runs(&h, 1).iter().all(Vec::is_empty));
}

/// Regions ride the text: a row inserted before a region moves it, one
/// inserted at its end goes into it, and the parse after each edit ends
/// where a fresh parse with the moved regions does.
#[test]
fn declared_regions_follow_edits() {
    let code = "+def f():\n";
    let mut h = highlighter();
    h.open_declared(1, code).unwrap();
    h.set_regions(
        1,
        &[DeclaredRegion {
            start: 0,
            end: code.len(),
            language: "py".to_string(),
            prefix: 1,
            streams: Vec::new(),
        }],
    )
    .unwrap();
    finish(&mut h);

    h.edit(1, 0, 0, "HEADER\n").unwrap();
    finish(&mut h);
    assert_eq!(highlights(&h, 1), fresh_declared(&h, 1));
    let lines = runs(&h, 1);
    assert!(lines[0].is_empty());
    assert!(lines[1].iter().any(|r| r.cat == Category::Keyword));

    let end = "HEADER\n+def f():\n".len();
    h.edit(1, end, 0, "+    return 1\n").unwrap();
    finish(&mut h);
    assert_eq!(highlights(&h, 1), fresh_declared(&h, 1));
    assert!(runs(&h, 1)[2]
        .iter()
        .any(|r| r.start == 5 && r.cat == Category::Keyword));
}

/// Pieces of Rust that open and close multi-line constructs, and multibyte
/// text.
const TOKENS: &[&str] = &[
    "fn ", "x", "/*", "*/", "\"", "'", "{", "}", "\n", "\n", "\n", "// ", "é", "r#\"", "\"#", " ",
    "1", "\r\n", "let ", "::", "#[a]",
];

/// Pieces of Markdown with fences in Rust, Python, no language and an
/// unknown one, pieces of those languages' multi-line constructs, so edits
/// open, close and retarget fences around code in mid-construct, and table
/// rows, so they open, split and close tables.
const MARKDOWN_TOKENS: &[&str] = &[
    "| a | b |\n",
    "|---|---|\n",
    "|",
    "```rust\n",
    "```python\n",
    "```\n",
    "```zzz\n",
    "```",
    "rust",
    "python",
    "~~~\n",
    "# ",
    "- ",
    "fn ",
    "def ",
    "x",
    "/*",
    "*/",
    "\"",
    "\"\"\"",
    "{",
    "}",
    "\n",
    "\n",
    "\n",
    "// ",
    "é",
    " ",
    "    ",
    "\r\n",
    "`",
    "<div>",
];

fn tokens(set: &'static [&'static str], range: Range<usize>) -> impl Strategy<Value = String> {
    prop::collection::vec(prop::sample::select(set), range).prop_map(|t| t.concat())
}

#[derive(Debug, Clone)]
enum Op {
    Edit {
        at: usize,
        delete: usize,
        insert: String,
    },
    Want {
        from: usize,
        len: usize,
    },
    Work(usize),
}

fn op(set: &'static [&'static str]) -> impl Strategy<Value = Op> {
    prop_oneof![
        (any::<usize>(), 0usize..40, tokens(set, 0..6)).prop_map(|(at, delete, insert)| Op::Edit {
            at,
            delete,
            insert
        }),
        (0usize..200, 0usize..30).prop_map(|(from, len)| Op::Want { from, len }),
        (1usize..2_000).prop_map(Op::Work),
    ]
}

/// The largest character boundary of `text` at or before `at`.
fn floor_boundary(text: &str, at: usize) -> usize {
    (0..=at.min(text.len()))
        .rev()
        .find(|&i| text.is_char_boundary(i))
        .unwrap_or(0)
}

/// Apply `ops` to document `doc`, whose text is `text`, and work until
/// nothing remains. Returns the final text.
fn apply(h: &mut Highlighter, doc: u32, mut text: String, ops: Vec<Op>) -> String {
    for op in ops {
        match op {
            Op::Edit { at, delete, insert } => {
                let start = floor_boundary(&text, at % (text.len() + 1));
                let end = floor_boundary(&text, start + delete);
                h.edit(doc, start, end - start, &insert).unwrap();
                text.replace_range(start..end, &insert);
            }
            Op::Want { from, len } => h.want(doc, from, from + len).unwrap(),
            Op::Work(budget) => {
                h.work(budget).unwrap();
            }
        }
    }
    finish(h);
    text
}

fn assert_lines_eq(
    got: &[(Option<Highlight>, Roles)],
    expected: &[(Option<Highlight>, Roles)],
) -> Result<(), TestCaseError> {
    prop_assert_eq!(got.len(), expected.len());
    for (line, (got, expected)) in got.iter().zip(expected).enumerate() {
        prop_assert_eq!(got, expected, "line {}", line);
    }
    Ok(())
}

/// However edits, wanted ranges and budgets interleave, once the work is
/// done every line of a `syntax` document opened with `initial` holds
/// exactly what a parse of the final text gives, roles included.
fn check_incremental(syntax: &str, initial: String, ops: Vec<Op>) -> Result<(), TestCaseError> {
    let mut h = highlighter();
    h.open(7, syntax, &initial).unwrap();
    let text = apply(&mut h, 7, initial, ops);
    assert_lines_eq(&highlights(&h, 7), &fresh(syntax, &text))
}

/// Pieces of Python with its multi-line constructs, for rows of a composed
/// document.
const PYTHON_TOKENS: &[&str] = &[
    "def ", "x", "\"\"\"", "'''", "'", "# ", "(", ")", ":", " ", "\n", "\n", "\n", "1", "é",
    "return ", "+", "-",
];

/// `text` as a composed document: each row a region of its own when its
/// `kinds` entry (cycled) says so — 0: none, 1: stream 0, 2: stream 1, 3:
/// both — in Python, past a one-byte prefix.
fn composed_regions(text: &str, kinds: &[u8]) -> Vec<DeclaredRegion> {
    text.split_inclusive('\n')
        .scan(0, |at, row| {
            let start = *at;
            *at += row.len();
            Some(start..*at)
        })
        .zip(kinds.iter().cycle())
        .filter_map(|(range, kind)| {
            let streams = match kind {
                1 => vec![0],
                2 => vec![1],
                3 => vec![0, 1],
                _ => return None,
            };
            Some(DeclaredRegion {
                start: range.start,
                end: range.end,
                language: "py".to_string(),
                prefix: 1,
                streams,
            })
        })
        .collect()
}

/// Every line of a parse in one go of declared document `doc`'s current
/// text, with the regions it has now.
fn fresh_declared(h: &Highlighter, doc: u32) -> Vec<(Option<Highlight>, Roles)> {
    let d = &h.documents[&doc];
    let Host::Declared(regions) = &d.host else {
        panic!("document {doc} is not declared");
    };
    let mut fresh = highlighter();
    fresh.documents.insert(
        0,
        Document::new(d.syntax, Host::Declared(regions.clone()), &d.text()),
    );
    finish(&mut fresh);
    highlights(&fresh, 0)
}

/// [`check_incremental`] for a declared document: its regions move with
/// the edits, and its stream parsers are part of the state a parse
/// converges on.
fn check_declared(initial: String, kinds: Vec<u8>, ops: Vec<Op>) -> Result<(), TestCaseError> {
    let mut h = highlighter();
    h.open_declared(7, &initial).unwrap();
    h.set_regions(7, &composed_regions(&initial, &kinds))
        .unwrap();
    apply(&mut h, 7, initial, ops);
    assert_lines_eq(&highlights(&h, 7), &fresh_declared(&h, 7))
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(64))]

    #[test]
    fn incremental_highlighting_matches_a_fresh_parse(
        initial in tokens(TOKENS, 50..400),
        ops in prop::collection::vec(op(TOKENS), 1..40),
    ) {
        check_incremental("Rust", initial, ops)?;
    }

    /// The same through Markdown fences, whose child parsers' state the
    /// checkpoints carry, and tables, whose lines' roles read their
    /// neighbours'.
    #[test]
    fn incremental_highlighting_matches_a_fresh_parse_through_fences(
        initial in tokens(MARKDOWN_TOKENS, 50..400),
        ops in prop::collection::vec(op(MARKDOWN_TOKENS), 1..40),
    ) {
        check_incremental("Markdown", initial, ops)?;
    }

    /// The same for a declared document, whose regions edits move and
    /// whose two streams interleave.
    #[test]
    fn incremental_declared_highlighting_matches_a_fresh_parse(
        initial in tokens(PYTHON_TOKENS, 20..300),
        kinds in prop::collection::vec(0u8..4, 1..20),
        ops in prop::collection::vec(op(PYTHON_TOKENS), 1..40),
    ) {
        check_declared(initial, kinds, ops)?;
    }
}
