//! Structural auto-indentation for Pascal.
//!
//! # Why Pascal needs its own tier
//!
//! The regex rules tier ([`crate::primitives::indent_rules`]) derives an
//! indent from **one** reference line: the previous non-blank line's indent,
//! plus or minus a unit if that line matches an `increase` or `decrease`
//! pattern. That is enough for a language whose nesting is visible one line at
//! a time — a C-family `{` opens exactly one level and the `}` that closes it
//! starts its own line — and it is not enough for Pascal, where the level a
//! line belongs to is a property of the *stack of constructs above it* rather
//! than of the line before it:
//!
//! ```pascal
//! if a then
//!   if b then
//!     DoIt;
//! Next;
//! ```
//!
//! `Next` belongs two levels out, and nothing about `DoIt;` — the line before
//! it — says so. Only the knowledge that two one-statement `then` bodies were
//! opened and both have now been satisfied does. The same is true of a
//! `begin` closing back to the `if` that introduced it, of `else` finding its
//! `if`, and of `end` finding the construct it closes rather than simply
//! moving one unit left from wherever the previous line happened to sit.
//!
//! So this module keeps the stack. It replays the code above the cursor as a
//! token stream and maintains one frame per open construct; the indent of a
//! line is `base + unit × (frames still open)`.
//!
//! # The frames
//!
//! | Frame | Opened by | Closed by |
//! |---|---|---|
//! | [`Frame::End`] | `begin`, `case`, `record`, `try`, `asm` | `end` |
//! | [`Frame::Until`] | `repeat` | `until` |
//! | [`Frame::Shot`] | a line ending in `then`, `else` or `do` | the next `;`, or the construct that takes its place |
//!
//! A line that *starts* with a closer is placed by the stack its own keyword
//! leaves behind — that is what puts `end` on its `begin` rather than one unit
//! left of whatever preceded it, and `except` on its `try` while the `try`
//! stays open. [`LeadingKeyword`] is that placement; the folding itself is
//! [`consume_line`]'s, and the two must not both run on the same text.
//!
//! [`Frame::Shot`] is what makes braceless control flow work. `if a then` at
//! the end of a line promises exactly one statement, so the next line indents;
//! the `;` that finishes that statement pops the promise, and the line after
//! it comes back out. A `begin` arriving instead of a statement pops the
//! promise too and opens a block in its place, which is what puts `begin` at
//! the `if`'s own indent rather than one level in — the layout Pascal is
//! conventionally written in, and the one Fresh already produced.
//!
//! The openers are exactly the keywords the `PascalLike` regex family already
//! treated as openers; what changes is how the indent is derived from them,
//! not which words count.
//!
//! # Cost
//!
//! Bounded, like the tree-sitter tier it sits beside: the replay starts at the
//! nearest enclosing top-level construct (a `procedure`, `begin`, `unit`, …
//! at column 0) and never looks back further than [`MAX_SCAN_BYTES`]. Editing
//! the end of a 200k-line unit costs the same as editing the end of a short
//! one. Within that window it is a byte scan and a `Vec` of a handful of
//! frames — no parse, no tree, nothing cached and so nothing to invalidate.
//!
//! Past the window the answer degrades rather than breaking: the first line of
//! the window is taken as the base, so a construct opened further up is
//! invisible and the indent is relative instead of absolute. This is the same
//! bargain `indent.rs` makes with its 2000-byte parse window.

use crate::model::buffer::Buffer;

/// How far back the replay may look for its anchor. Matches the tree-sitter
/// tier's parse window, for the same reason: a bound that keeps a keystroke's
/// cost independent of the file's size.
const MAX_SCAN_BYTES: usize = 2000;

/// One open construct.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Frame {
    /// A block closed by `end`: `begin`, `case … of`, `record`, `try`, `asm`.
    End,
    /// `repeat`, closed by `until`.
    Until,
    /// A one-statement body promised by a trailing `then` / `else` / `do`.
    Shot,
}

/// What a line's leading keyword does to the stack before the line is placed.
///
/// A line is indented by the stack as it stands *after* its own opening
/// keyword has been accounted for — which is why `end` sits at its `begin`'s
/// column and not one line-and-a-unit away from whatever preceded it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LeadingKeyword {
    /// `end` — closes the nearest `End` frame.
    End,
    /// `until` — closes the nearest `Until` frame.
    Until,
    /// `else` — satisfies the one-statement body it follows, if there is one.
    ///
    /// Distinct from [`Handler`](Self::Handler) because `else` has two homes
    /// and both are the depth it is already at: after `if a then X` it belongs
    /// on the `if`, which is where popping that body leaves it; and as a
    /// `case`'s own `else` it belongs with the arms, which is the depth inside
    /// the `case` block. Neither steps out of a block.
    Else,
    /// `except` / `finally` — still inside the `try`, but on the `try`'s own
    /// column, so the line steps out one level that its body steps back into.
    Handler,
    /// `begin` — takes the place of a pending one-statement body.
    Begin,
}

impl LeadingKeyword {
    /// The keyword a line starts with, if it is one this module places.
    pub fn from_word(word: &str) -> Option<Self> {
        match word {
            "end" => Some(Self::End),
            "until" => Some(Self::Until),
            "else" => Some(Self::Else),
            "except" | "finally" => Some(Self::Handler),
            "begin" => Some(Self::Begin),
            _ => None,
        }
    }
}

/// Indent (in visual columns) for a new line inserted at `position`.
///
/// `is_code(byte)` reports `false` for bytes inside a comment or a string, so
/// an `end` in a `{ … }` comment or a `'begin'` literal moves nothing. Pass
/// `|_| true` to disable masking — the module then degrades to plain keyword
/// matching rather than misbehaving, exactly as the regex tier does.
pub fn indent_for_new_line<F: Fn(usize) -> bool>(
    buffer: &Buffer,
    position: usize,
    tab_size: usize,
    is_code: F,
) -> usize {
    let unit = tab_size.max(1);
    let cur = line_bounds(buffer, position);

    // Everything above the cursor, plus the part of the cursor's own line that
    // stays behind when the line is split.
    let (base, mut stack) = replay(buffer, cur.start, tab_size, &is_code);
    consume_line(
        &mut stack,
        &code_view(buffer, cur.start, position, &is_code),
    );

    // The tail — text after the cursor, which moves down onto the new line. A
    // closer there closes on the new line, so the new line is placed as that
    // closer's own line would be.
    let tail = code_view(buffer, position, cur.end, &is_code);
    if let Some(kw) = first_word(&tail).and_then(|w| LeadingKeyword::from_word(&w)) {
        apply_leading(&mut stack, kw);
    }

    base + unit * stack.len()
}

/// Indent for a line whose leading keyword is `kw` — the electric dedent that
/// fires as the user finishes typing `end`, `until`, `else`, `except`,
/// `finally` or `begin` at the start of an otherwise empty line.
///
/// `line_start` is the byte offset of that line's first byte. Returns the
/// column the keyword belongs in, which is the enclosing construct's own
/// column rather than "one unit left of the line above" — the distinction
/// that lets `end` land on its `begin` after any number of one-statement
/// bodies have been opened and satisfied in between.
pub fn indent_for_leading_keyword<F: Fn(usize) -> bool>(
    buffer: &Buffer,
    line_start: usize,
    kw: LeadingKeyword,
    tab_size: usize,
    is_code: F,
) -> usize {
    let unit = tab_size.max(1);
    let (base, mut stack) = replay(buffer, line_start, tab_size, &is_code);
    apply_leading(&mut stack, kw);
    base + unit * stack.len()
}

/// Replay the code above `line_start` into a frame stack.
///
/// Returns the base column the stack is measured from — the anchor line's own
/// indent, which is zero whenever a real top-level anchor was found.
fn replay<F: Fn(usize) -> bool>(
    buffer: &Buffer,
    line_start: usize,
    tab_size: usize,
    is_code: &F,
) -> (usize, Vec<Frame>) {
    let lines = window(buffer, line_start, tab_size, is_code);
    let base = lines
        .first()
        .map(|l| visual_indent(buffer, l.start, l.end, tab_size))
        .unwrap_or(0);

    let mut stack = Vec::new();
    for line in &lines {
        consume_line(
            &mut stack,
            &code_view(buffer, line.start, line.end, is_code),
        );
    }
    (base, stack)
}

/// The lines the replay covers: back to the nearest top-level anchor, or to
/// [`MAX_SCAN_BYTES`], whichever comes first. Oldest first.
fn window<F: Fn(usize) -> bool>(
    buffer: &Buffer,
    line_start: usize,
    tab_size: usize,
    is_code: &F,
) -> Vec<LineSpan> {
    let floor = line_start.saturating_sub(MAX_SCAN_BYTES);
    let mut lines: Vec<LineSpan> = Vec::new();
    let mut at = line_start;
    while at > floor {
        let line = line_bounds(buffer, at - 1);
        if line.start < floor {
            break;
        }
        lines.push(line);
        // An anchor is a construct that can only appear at the outermost
        // level, written there: everything above it is another declaration's
        // business, so the stack starts empty here and the base is its column.
        let view = code_view(buffer, line.start, line.end, is_code);
        if visual_indent(buffer, line.start, line.end, tab_size) == 0
            && first_word(&view).is_some_and(|w| is_anchor_word(&w))
        {
            break;
        }
        if line.start == 0 {
            break;
        }
        at = line.start;
    }
    lines.reverse();
    lines
}

/// Words that only ever start a top-level construct, used to stop the scan.
///
/// `begin` is deliberately absent: a `begin` at column 0 is a main program
/// body, but it is also the opener whose frame the lines under it need, so
/// anchoring *on* it would drop the level it opens. The declaration keywords
/// that precede a body (`procedure`, `function`, …) anchor instead, and they
/// sit above any `begin` that matters.
fn is_anchor_word(word: &str) -> bool {
    matches!(
        word,
        "program"
            | "unit"
            | "library"
            | "interface"
            | "implementation"
            | "procedure"
            | "function"
            | "constructor"
            | "destructor"
            | "initialization"
            | "finalization"
    )
}

/// Fold one line's masked code into the stack.
///
/// **The leading keyword is not applied separately here.** Every word
/// [`LeadingKeyword`] names — `end`, `until`, `else`, `except`, `finally`,
/// `begin` — is also handled by the token loop below, wherever on the line it
/// appears. Doing both popped twice for a line starting with one, so
/// `begin / begin / X; / end;` came out at column 0 instead of the outer
/// body's. [`apply_leading`] is for the two places the keyword is *not* in the
/// text being folded: a line's tail moving down past the cursor, and a keyword
/// still being typed.
fn consume_line(stack: &mut Vec<Frame>, view: &str) {
    let mut last_word: Option<String> = None;
    for token in tokens(view) {
        match token {
            Token::Semicolon => {
                pop_shots(stack);
                last_word = None;
            }
            Token::Word(w) => {
                match w.as_str() {
                    // A block opener. `begin` (and only `begin`) takes the
                    // place of a pending one-statement body: `if a then` /
                    // `begin` is one construct, not two nested ones.
                    "begin" => {
                        pop_shots(stack);
                        stack.push(Frame::End);
                    }
                    "case" | "record" | "try" | "asm" => stack.push(Frame::End),
                    "repeat" => stack.push(Frame::Until),
                    "end" => close(stack, Frame::End),
                    "until" => close(stack, Frame::Until),
                    // Mid-block keywords leave their construct open. `else`
                    // additionally satisfies the `then` body it follows; the
                    // body it introduces is opened below, when it turns out
                    // to end the line.
                    "else" | "except" | "finally" => pop_shots(stack),
                    _ => {}
                }
                last_word = Some(w);
            }
        }
    }

    // A line that ends on `then`, `else` or `do` has promised exactly one
    // statement and not yet been given it.
    if matches!(
        last_word.as_deref(),
        Some("then") | Some("else") | Some("do")
    ) {
        stack.push(Frame::Shot);
    }
}

/// Apply a line's own leading keyword, so the line is placed by the stack it
/// leaves behind rather than the one it arrived with.
fn apply_leading(stack: &mut Vec<Frame>, kw: LeadingKeyword) {
    match kw {
        LeadingKeyword::End => close(stack, Frame::End),
        LeadingKeyword::Until => close(stack, Frame::Until),
        // `else` drops the one-statement body it follows and nothing else —
        // see the variant's own note for why that is right in both of its
        // homes.
        LeadingKeyword::Else => pop_shots(stack),
        // `except` / `finally` belong on their `try`'s column while the `try`
        // is still open. The stack here is only ever used to place this one
        // line — the walk's own stack is folded by `consume_line`, which
        // leaves the block alone — so stepping out of it is how "one level
        // shallower than my contents" is said.
        LeadingKeyword::Handler => {
            pop_shots(stack);
            stack.pop();
        }
        // `begin` replaces a pending body, so it sits where the `if` that
        // promised it sits. The frame it opens is pushed when the line is
        // consumed, not here.
        LeadingKeyword::Begin => pop_shots(stack),
    }
}

/// Drop every one-statement body sitting on top of the stack.
fn pop_shots(stack: &mut Vec<Frame>) {
    while stack.last() == Some(&Frame::Shot) {
        stack.pop();
    }
}

/// Close the nearest `want` frame, discarding any one-statement bodies opened
/// inside it. A closer with nothing to close leaves the stack alone rather
/// than emptying it — half-written code is the normal case here, and an
/// unmatched `end` should not flatten the file.
fn close(stack: &mut Vec<Frame>, want: Frame) {
    let Some(at) = stack.iter().rposition(|f| *f == want) else {
        pop_shots(stack);
        return;
    };
    stack.truncate(at);
}

/// A token the stack cares about. Everything else is skipped.
enum Token {
    Word(String),
    Semicolon,
}

/// Lowercased identifier words and statement terminators, in order.
///
/// Whole words only, so `append` is not an `end` and `Repeated` is not a
/// `repeat`. Pascal is case-insensitive, so the words are folded.
fn tokens(view: &str) -> Vec<Token> {
    let bytes = view.as_bytes();
    let mut out = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        if b == b';' {
            out.push(Token::Semicolon);
            i += 1;
        } else if b.is_ascii_alphabetic() || b == b'_' {
            let start = i;
            while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
                i += 1;
            }
            out.push(Token::Word(view[start..i].to_ascii_lowercase()));
        } else {
            i += 1;
        }
    }
    out
}

/// The first identifier word on a line, lowercased.
fn first_word(view: &str) -> Option<String> {
    let trimmed = view.trim_start();
    let end = trimmed
        .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
        .unwrap_or(trimmed.len());
    let word = &trimmed[..end];
    (!word.is_empty()).then(|| word.to_ascii_lowercase())
}

// ---------------------------------------------------------------------------
// Line geometry. Byte-oriented and tab-aware, kept local for the same reason
// `indent_rules` keeps its own copy: this module must not depend on the tier
// beside it.
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
struct LineSpan {
    start: usize,
    end: usize,
}

fn byte_at(buffer: &Buffer, pos: usize) -> Option<u8> {
    if pos >= buffer.len() {
        return None;
    }
    buffer.slice_bytes(pos..pos + 1).first().copied()
}

fn line_bounds(buffer: &Buffer, position: usize) -> LineSpan {
    let mut start = position;
    while start > 0 && byte_at(buffer, start - 1) != Some(b'\n') {
        start -= 1;
    }
    let mut end = position;
    while end < buffer.len() && byte_at(buffer, end) != Some(b'\n') {
        end += 1;
    }
    LineSpan { start, end }
}

fn visual_indent(buffer: &Buffer, start: usize, end: usize, tab_size: usize) -> usize {
    let mut indent = 0;
    let mut p = start;
    while p < end {
        match byte_at(buffer, p) {
            Some(b' ') => indent += 1,
            Some(b'\t') => indent += tab_size,
            _ => break,
        }
        p += 1;
    }
    indent
}

/// `[start, end)` with comment/string bytes blanked to spaces and `\r`/`\n`
/// dropped — the "code view" the whole indent tier matches against.
fn code_view<F: Fn(usize) -> bool>(
    buffer: &Buffer,
    start: usize,
    end: usize,
    is_code: &F,
) -> String {
    let bytes = buffer.slice_bytes(start..end);
    let mut out = String::with_capacity(bytes.len());
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'\r' || b == b'\n' {
            continue;
        }
        if is_code(start + i) {
            out.push(b as char);
        } else {
            out.push(' ');
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn buf(s: &str) -> Buffer {
        Buffer::from_str_test(s)
    }

    /// Indent for a new line at the end of `content`.
    fn indent(content: &str) -> usize {
        indent_for_new_line(&buf(content), content.len(), 2, |_| true)
    }

    /// Indent the given leading keyword would be placed at, typed on a fresh
    /// line at the end of `content` (which must end in a newline).
    fn keyword_indent(content: &str, kw: LeadingKeyword) -> usize {
        indent_for_leading_keyword(&buf(content), content.len(), kw, 2, |_| true)
    }

    #[test]
    fn a_begin_indents_its_contents() {
        assert_eq!(indent("begin\n"), 2);
        assert_eq!(indent("begin\n  X;\n"), 2);
    }

    #[test]
    fn nested_begins_nest() {
        assert_eq!(indent("begin\n  begin\n"), 4);
        assert_eq!(indent("begin\n  begin\n    X;\n  end;\n"), 2);
    }

    /// `end` lands on its `begin`, not one unit left of the line above it.
    #[test]
    fn end_lines_up_with_the_construct_it_closes() {
        let src = "begin\n  if a then\n    if b then\n      X;\n";
        // The line above `end` is `X;` at 6, but `end` closes the `begin` at 0.
        assert_eq!(keyword_indent(src, LeadingKeyword::End), 0);
    }

    /// The case the regex tier cannot express: two one-statement bodies open
    /// and both satisfied, so the next statement comes back to the outer level
    /// even though the line above it is deeply indented.
    #[test]
    fn a_statement_closes_every_one_statement_body_it_satisfies() {
        assert_eq!(indent("if a then\n"), 2);
        assert_eq!(indent("if a then\n  if b then\n"), 4);
        assert_eq!(indent("if a then\n  if b then\n    X;\n"), 0);
    }

    #[test]
    fn the_loop_forms_indent_their_bodies() {
        assert_eq!(indent("for i := 1 to 10 do\n"), 2);
        assert_eq!(indent("while a do\n"), 2);
        assert_eq!(indent("with r do\n"), 2);
        // …and the statement that fills the body closes it.
        assert_eq!(indent("for i := 1 to 10 do\n  X;\n"), 0);
    }

    #[test]
    fn else_takes_its_own_body() {
        assert_eq!(indent("if a then\n  X\nelse\n"), 2);
        assert_eq!(indent("if a then\n  X\nelse\n  Y;\n"), 0);
    }

    /// `else` sits at the `if` it belongs to, not under the `then` body.
    #[test]
    fn else_lines_up_with_its_if() {
        assert_eq!(
            keyword_indent("begin\n  if a then\n    X\n", LeadingKeyword::Else),
            2
        );
    }

    /// A line starting with a closer must be folded once, not twice. Before
    /// the fold and the placement were separated, `end;` popped its `begin`
    /// *and* the one enclosing it, so the line after came out at column 0.
    #[test]
    fn a_closing_line_pops_one_frame_not_two() {
        assert_eq!(indent("begin\n  begin\n    X;\n  end;\n"), 2);
        assert_eq!(indent("repeat\n  repeat\n    X;\n  until a;\n"), 2);
    }

    /// A `begin` arriving where a one-statement body was promised replaces it,
    /// so it sits at the `if`'s column and its contents one unit in.
    #[test]
    fn begin_replaces_a_promised_body_rather_than_nesting_under_it() {
        assert_eq!(
            keyword_indent("begin\n  if a then\n", LeadingKeyword::Begin),
            2
        );
        assert_eq!(indent("begin\n  if a then\n  begin\n"), 4);
        assert_eq!(indent("begin\n  if a then\n  begin\n    X;\n  end;\n"), 2);
    }

    #[test]
    fn repeat_until() {
        assert_eq!(indent("repeat\n"), 2);
        assert_eq!(indent("repeat\n  X;\n"), 2);
        assert_eq!(keyword_indent("repeat\n  X;\n", LeadingKeyword::Until), 0);
        assert_eq!(indent("repeat\n  X;\nuntil a;\n"), 0);
    }

    #[test]
    fn case_of_end() {
        assert_eq!(indent("case x of\n"), 2);
        assert_eq!(indent("case x of\n  1: X;\n"), 2);
        assert_eq!(
            keyword_indent("case x of\n  1: X;\n", LeadingKeyword::End),
            0
        );
    }

    #[test]
    fn record_and_try() {
        assert_eq!(indent("type\nTPoint = record\n"), 2);
        assert_eq!(indent("try\n"), 2);
        // `except` sits on the `try`'s column…
        assert_eq!(keyword_indent("try\n  X;\n", LeadingKeyword::Handler), 0);
        // …and stays inside it, so its own body is indented again.
        assert_eq!(indent("try\n  X;\nexcept\n"), 2);
        // Nested one level in, the same pair moves with it.
        assert_eq!(
            keyword_indent("begin\n  try\n    X;\n", LeadingKeyword::Handler),
            2
        );
    }

    /// A procedure body is a `begin … end` and indents like one; the
    /// declaration header itself opens nothing.
    #[test]
    fn a_procedure_body_indents_its_contents() {
        assert_eq!(indent("procedure Greet(n: string);\n"), 0);
        assert_eq!(indent("procedure Greet(n: string);\nbegin\n"), 2);
        assert_eq!(
            keyword_indent(
                "procedure Greet(n: string);\nbegin\n  X;\n",
                LeadingKeyword::End
            ),
            0
        );
    }

    /// Whole words only: an identifier that merely contains a keyword opens
    /// and closes nothing.
    #[test]
    fn keywords_inside_identifiers_are_not_keywords() {
        assert_eq!(indent("begin\n  Appended := Rendered;\n"), 2);
        assert_eq!(indent("beginning := 1;\n"), 0);
    }

    /// Case-insensitive, as Pascal is.
    #[test]
    fn keywords_are_matched_without_regard_to_case() {
        assert_eq!(indent("BEGIN\n"), 2);
        assert_eq!(indent("Begin\n  If a Then\n"), 4);
    }

    /// A keyword the caller reports as comment or string text moves nothing.
    #[test]
    fn masked_spans_are_not_code() {
        let src = "begin\n  S := 'end';\n";
        let quote = src.find('\'').unwrap();
        let close_quote = src.rfind('\'').unwrap();
        let b = buf(src);
        let masked = |byte: usize| !(quote..=close_quote).contains(&byte);
        assert_eq!(indent_for_new_line(&b, src.len(), 2, masked), 2);
    }

    /// An unmatched closer leaves the stack alone rather than flattening the
    /// file: half-written code is the normal case while typing.
    #[test]
    fn an_unmatched_closer_does_not_flatten_everything() {
        assert_eq!(indent("begin\n  X;\nend;\nend;\n"), 0);
        assert_eq!(indent("until a;\n"), 0);
    }

    /// The replay is bounded, and past the bound the answer is relative to the
    /// first line it can see rather than wrong in an unbounded way.
    #[test]
    fn the_scan_is_bounded() {
        let filler = "  X := 1;\n".repeat(600);
        let src = format!("begin\n{filler}");
        assert!(src.len() > MAX_SCAN_BYTES * 2);
        // The `begin` is far out of the window, so the window's own first line
        // sets the base: the indent is that line's, not zero.
        assert_eq!(indent(&src), 2);
    }

    /// Tab size drives the unit.
    #[test]
    fn the_unit_is_the_tab_size() {
        let src = "begin\n";
        assert_eq!(indent_for_new_line(&buf(src), src.len(), 4, |_| true), 4);
        assert_eq!(indent_for_new_line(&buf(src), src.len(), 8, |_| true), 8);
    }
}
