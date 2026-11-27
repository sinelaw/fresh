use super::*;
use crate::MAX_LINE_BYTES;
use proptest::prelude::*;
use std::sync::OnceLock;

fn syntaxes() -> SyntaxSet {
    static SET: OnceLock<SyntaxSet> = OnceLock::new();
    SET.get_or_init(SyntaxSet::load_defaults_newlines).clone()
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

/// The highlight of `text` parsed in one go.
fn fresh(text: &str) -> Vec<Option<Highlight>> {
    let mut h = highlighter();
    h.open(0, "Rust", text).unwrap();
    finish(&mut h);
    highlights(&h, 0)
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
            cat: Category::Comment
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
            cat: Category::String
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

/// Pieces of Rust that open and close multi-line constructs, and multibyte
/// text.
const TOKENS: &[&str] = &[
    "fn ", "x", "/*", "*/", "\"", "'", "{", "}", "\n", "\n", "\n", "// ", "é", "r#\"", "\"#", " ",
    "1", "\r\n", "let ", "::", "#[a]",
];

fn tokens(range: Range<usize>) -> impl Strategy<Value = String> {
    prop::collection::vec(prop::sample::select(TOKENS), range).prop_map(|t| t.concat())
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

fn op() -> impl Strategy<Value = Op> {
    prop_oneof![
        (any::<usize>(), 0usize..40, tokens(0..6)).prop_map(|(at, delete, insert)| Op::Edit {
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

proptest! {
    #![proptest_config(ProptestConfig::with_cases(64))]

    /// However edits, wanted ranges and budgets interleave, once the work is
    /// done every line holds exactly what a parse of the final text gives.
    #[test]
    fn incremental_highlighting_matches_a_fresh_parse(
        initial in tokens(50..400),
        ops in prop::collection::vec(op(), 1..40),
    ) {
        let mut h = highlighter();
        let mut text = initial;
        h.open(7, "Rust", &text).unwrap();
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
        let expected = fresh(&text);
        let got = highlights(&h, 7);
        prop_assert_eq!(got.len(), expected.len());
        for (line, (got, expected)) in got.iter().zip(&expected).enumerate() {
            prop_assert_eq!(got, expected, "line {}", line);
        }
    }
}
