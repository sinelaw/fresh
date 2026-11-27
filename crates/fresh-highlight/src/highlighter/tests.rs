use super::*;
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

/// Every line's highlight, in order.
fn highlights(h: &Highlighter, doc: u32) -> Vec<Option<Highlight>> {
    h.documents[&doc]
        .lines
        .iter()
        .map(|l| l.highlight.clone())
        .collect()
}

/// The highlight of `text` parsed in one go in `syntax`.
fn fresh(syntax: &str, text: &str) -> Vec<Option<Highlight>> {
    let mut h = highlighter();
    h.open(0, syntax, text).unwrap();
    finish(&mut h);
    highlights(&h, 0)
}

/// Every line's runs, once all is highlighted.
fn runs(h: &Highlighter, doc: u32) -> Vec<Vec<Run>> {
    highlights(h, doc)
        .into_iter()
        .map(|highlight| highlight.expect("every line is highlighted").runs)
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
    assert!(provisional.iter().all(|l| !l.exact));
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
    assert!(exact.iter().all(|l| l.exact && l.runs == comment));
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

/// Pieces of Rust that open and close multi-line constructs, and multibyte
/// text.
const TOKENS: &[&str] = &[
    "fn ", "x", "/*", "*/", "\"", "'", "{", "}", "\n", "\n", "\n", "// ", "é", "r#\"", "\"#", " ",
    "1", "\r\n", "let ", "::", "#[a]",
];

/// Pieces of Markdown with fences in Rust, Python, no language and an
/// unknown one, and pieces of those languages' multi-line constructs, so
/// edits open, close and retarget fences around code in mid-construct.
const MARKDOWN_TOKENS: &[&str] = &[
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

/// However edits, wanted ranges and budgets interleave, once the work is
/// done every line of a `syntax` document opened with `initial` holds
/// exactly what a parse of the final text gives.
fn check_incremental(syntax: &str, initial: String, ops: Vec<Op>) -> Result<(), TestCaseError> {
    let mut h = highlighter();
    let mut text = initial;
    h.open(7, syntax, &text).unwrap();
    for op in ops {
        match op {
            Op::Edit { at, delete, insert } => {
                let start = floor_boundary(&text, at % (text.len() + 1));
                let end = floor_boundary(&text, start + delete);
                h.edit(7, start, end - start, &insert).unwrap();
                text.replace_range(start..end, &insert);
            }
            Op::Want { from, len } => h.want(7, from, from + len).unwrap(),
            Op::Work(budget) => {
                h.work(budget).unwrap();
            }
        }
    }
    finish(&mut h);
    let expected = fresh(syntax, &text);
    let got = highlights(&h, 7);
    prop_assert_eq!(got.len(), expected.len());
    for (line, (got, expected)) in got.iter().zip(&expected).enumerate() {
        prop_assert_eq!(got, expected, "line {}", line);
    }
    Ok(())
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
    /// checkpoints carry.
    #[test]
    fn incremental_highlighting_matches_a_fresh_parse_through_fences(
        initial in tokens(MARKDOWN_TOKENS, 50..400),
        ops in prop::collection::vec(op(MARKDOWN_TOKENS), 1..40),
    ) {
        check_incremental("Markdown", initial, ops)?;
    }
}
