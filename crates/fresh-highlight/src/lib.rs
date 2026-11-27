//! Syntax highlighting with syntect (TextMate grammars), shared by the
//! editor's native engine and the web client, which runs [`Highlighter`] as
//! WebAssembly.
//!
//! The pieces both use are here once: the categories text is coloured as
//! ([`Category`]), the scope → category table ([`scope_to_category`]), and
//! how much of a long line is highlighted ([`MAX_LINE_BYTES`],
//! [`highlighted_prefix`]). [`embedding`] holds the embedded-language
//! regions (Markdown fences, Vue blocks, diff hunks) and how their language
//! is found, and [`RowWash`] the filling of rows a diff category washes.

mod category;
pub mod embedding;
mod highlighter;
mod wash;

pub use category::{scope_to_category, Category};
pub use highlighter::{Error, Highlighter, LineRuns, Run};
pub use wash::RowWash;

/// How much of a line is highlighted: the rest of a longer one is plain text.
///
/// Syntect parses a line in one shot, and an edit re-parses the line it lands
/// in, so without a bound an edit in a long line costs the whole line up to
/// the viewport. On a file that is one long line (minified JSON or JS,
/// routinely hundreds of KB to tens of MB) that was every keystroke: ~270 ms
/// near the end of the first megabyte of a 50 MB array (issue #3631).
///
/// Only the line's first this-many bytes reach syntect. The parse after the
/// line carries on from the state those bytes leave, as if the line ended
/// there. Because the cut is at a fixed distance into the line, it falls in
/// the same place however much of the line a pass has in hand — the colours
/// do not depend on where the viewport is, there is nothing about a long line
/// for the cache to track, and no scan for the line's end is needed. VS Code
/// makes the same call at the same size (`editor.maxTokenizationLineLength`),
/// leaving such lines untokenized altogether.
pub const MAX_LINE_BYTES: usize = 20_000;

/// The part of a line's content that is highlighted: all of it, or its first
/// [`MAX_LINE_BYTES`] cut back to a character boundary.
pub fn highlighted_prefix(line: &str) -> &str {
    if line.len() <= MAX_LINE_BYTES {
        return line;
    }
    let mut cut = MAX_LINE_BYTES;
    while !line.is_char_boundary(cut) {
        cut -= 1;
    }
    &line[..cut]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_short_line_is_highlighted_whole() {
        let line = "a".repeat(MAX_LINE_BYTES);
        assert_eq!(highlighted_prefix(&line), line);
    }

    #[test]
    fn a_long_line_is_cut_back_to_a_character_boundary() {
        // `é` is two bytes: the one starting at MAX_LINE_BYTES - 1 straddles
        // the cut, so the cut moves back before it.
        let line = format!("{}é{}", "a".repeat(MAX_LINE_BYTES - 1), "b".repeat(10));
        assert_eq!(highlighted_prefix(&line).len(), MAX_LINE_BYTES - 1);
        let line = "a".repeat(MAX_LINE_BYTES + 5);
        assert_eq!(highlighted_prefix(&line).len(), MAX_LINE_BYTES);
    }
}
