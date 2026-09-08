//! The frame a control wears, in cells.
//!
//! **One definition, two renderers.** A framed button is `[ Label ]` in the
//! terminal, and until this existed that string was composed in one place and
//! its *width* asserted as a constant in another — two spellings of the same
//! fact, free to drift. Here the glyphs are the source and the columns are
//! derived from them, so changing `[` to `(` moves the padding, the reserved
//! columns and the runtime's text together.
//!
//! Two readers, deliberately unlike each other:
//!
//! - the widget runtime's text projection, which needs the finished string,
//!   because its whole output is text;
//! - the shell's stylesheet (`app::shell_style`), which needs the *columns* —
//!   its description carries the naked label and the fold draws the glyphs
//!   into the cells this reserved.
//!
//! It lives here rather than beside the stylesheet because this crate cannot
//! see that one, and because a frame is cells: it needs no theme, no ink, and
//! no display list to be true.

use crate::primitives::display_width::str_width;

/// Glyphs on the sides of a box, and the padding inside them.
///
/// Top and bottom are absent because nothing yet draws a rule above or below a
/// control, and vertical padding for the same reason — a property that is
/// named but unimplemented invites a caller the code cannot serve.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Frame {
    pub l: Option<&'static str>,
    pub r: Option<&'static str>,
    /// Columns of padding inside the glyphs, on each side.
    pub pad_x: u16,
}

impl Frame {
    /// What a framed button is drawn with: `[ ` and ` ]`.
    pub const BUTTON: Frame = Frame {
        l: Some("["),
        r: Some("]"),
        pad_x: 1,
    };

    /// The columns this frame reserves on **each** side: its glyph plus its
    /// padding.
    pub fn reserved(&self) -> usize {
        let side = |g: Option<&str>| g.map(str_width).unwrap_or(0);
        debug_assert_eq!(
            side(self.l),
            side(self.r),
            "a frame whose sides differ in width reserves different columns on \
             each side, which nothing that reads this can yet express"
        );
        side(self.l).max(side(self.r)) + usize::from(self.pad_x)
    }

    /// Both sides at once — the columns a frame costs a row.
    pub fn chrome(&self) -> usize {
        self.reserved() * 2
    }

    /// `content` with the frame around it, for the reader whose output is
    /// text. The other reader reserves [`Frame::reserved`] columns instead and
    /// lets its backend draw the glyphs there.
    pub fn wrap(&self, content: &str) -> String {
        let pad = " ".repeat(usize::from(self.pad_x));
        let mut out = String::with_capacity(content.len() + self.chrome());
        out.push_str(self.l.unwrap_or(""));
        out.push_str(&pad);
        out.push_str(content);
        out.push_str(&pad);
        out.push_str(self.r.unwrap_or(""));
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The glyphs are the source and the columns are derived, which is the
    /// whole reason this type exists rather than a `const` beside a `format!`.
    #[test]
    fn the_columns_are_the_glyphs_it_draws() {
        assert_eq!(Frame::BUTTON.wrap("Go"), "[ Go ]");
        assert_eq!(Frame::BUTTON.chrome(), 4);
        assert_eq!(
            str_width(&Frame::BUTTON.wrap("Go")),
            str_width("Go") + Frame::BUTTON.chrome(),
        );
    }
}
