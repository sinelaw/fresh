//! The primitives' render objects.
//!
//! Each one is an ordinary `RenderObject`. Nothing here can do anything a host
//! leaf cannot: they measure their children through `LayoutCx`, position them,
//! emit display-list items and answer hit queries. The framework supplies the
//! tree, the cache and the dirty bits; the algorithms live here.

use std::rc::Rc;

use crate::desc::{
    Align, BoxProps, Dir, LayerProps, Sizing, TextProps, ViewportProps, Wrap, HANGING_MIN_TEXT,
};
use crate::render::geom::{distribute, Constraints, Point, Rect, Size};
use crate::render::object::{FocusReg, Geom, LayerGeom, LayoutCx, LayoutInfo, RenderObject};
use crate::render::spec::{Draw, DrawList};

// -- axes --------------------------------------------------------------------

pub(crate) fn main_of(d: Dir, s: Size) -> u16 {
    match d {
        Dir::Row => s.w,
        Dir::Col => s.h,
    }
}

pub(crate) fn cross_of(d: Dir, s: Size) -> u16 {
    match d {
        Dir::Row => s.h,
        Dir::Col => s.w,
    }
}

fn size_of(d: Dir, main: u16, cross: u16) -> Size {
    match d {
        Dir::Row => Size::new(main, cross),
        Dir::Col => Size::new(cross, main),
    }
}

fn point_of(d: Dir, main: i32, cross: i32) -> Point {
    match d {
        Dir::Row => Point::new(main, cross),
        Dir::Col => Point::new(cross, main),
    }
}

fn axes(d: Dir, main: (u16, u16), cross: (u16, u16)) -> Constraints {
    match d {
        Dir::Row => Constraints::new(main.0, main.1, cross.0, cross.1),
        Dir::Col => Constraints::new(cross.0, cross.1, main.0, main.1),
    }
}

pub(crate) fn pct(extent: u16, p: u8) -> u16 {
    ((extent as u32 * p as u32) / 100) as u16
}

/// Resolve a size request into a constraint range on one axis.
pub(crate) fn range(s: Sizing, extent: u16, definite: bool, align: Align) -> (u16, u16) {
    match s {
        Sizing::Cells(v) => {
            let v = v.min(extent);
            (v, v)
        }
        Sizing::Pct(p) => {
            let v = pct(extent, p);
            (v, v)
        }
        Sizing::Flex(_) => (extent, extent),
        Sizing::Auto => {
            if definite && align == Align::Stretch {
                (extent, extent)
            } else {
                (0, extent)
            }
        }
    }
}

/// The main-axis half of a `(min_w, min_h)` pair.
fn main_floor(dir: Dir, floor: (u16, u16)) -> u16 {
    match dir {
        Dir::Row => floor.0,
        Dir::Col => floor.1,
    }
}

fn align_offset(align: Align, extent: u16, size: u16) -> i32 {
    let slack = extent.saturating_sub(size);
    match align {
        Align::Stretch | Align::Start => 0,
        Align::Center => (slack / 2) as i32,
        Align::End => slack as i32,
    }
}

/// One wrapped row: what it shows, and which bytes of the source it is.
///
/// Wrapping is not a pure slicing of the input — it drops the space it broke
/// at and, for [`Wrap::Hanging`], puts spaces of its own at the front. So a
/// row cannot be located in the source by counting the rows before it, and a
/// caller that tries ends up guessing (this used to say "step past one space
/// if there is one", which is right only when exactly one was dropped).
///
/// **The row states it instead, and the invariant is exact:**
/// `text[indent..] == source[src]`, byte for byte. Everything wrapping added
/// is in front of `indent`; everything wrapping dropped is in the gap between
/// one row's `src.end` and the next row's `src.start`. That one fact is the
/// whole mapping between a caret's byte and the cell it is drawn in — see
/// [`cell_of`] and [`byte_of`], which are its two directions and read nothing
/// else.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Row {
    pub text: String,
    /// Leading chars of `text` that wrapping *added*, with no source behind
    /// them. They are spaces, so this is equally a byte count and a column.
    pub indent: usize,
    /// The bytes of the source that `text[indent..]` is, verbatim.
    pub src: std::ops::Range<usize>,
}

/// The chunks a line breaks into: each carries the spaces that precede it, so
/// a run of spaces inside the line survives a wrap and the line's own leading
/// indent belongs to its first chunk.
///
/// `split(' ')` loses both. It yields an empty piece per space, and a wrapper
/// that skips empties has silently normalised `"    sep  a string"` to
/// `"sep a string"` — which is how the migrated popups lost the indent on
/// their *first* row as well as their continuations.
fn chunks(para: &str) -> Vec<&str> {
    let mut out: Vec<&str> = Vec::new();
    let mut start = 0usize;
    let mut seen_word = false;
    for (i, c) in para.char_indices() {
        if c == ' ' {
            if seen_word {
                out.push(&para[start..i]);
                start = i;
                seen_word = false;
            }
        } else {
            seen_word = true;
        }
    }
    if start < para.len() || out.is_empty() {
        out.push(&para[start..]);
    }
    out
}

/// Greedy word wrap, breaking a word too long for a line of its own.
///
/// Measure and paint call this same function, so the height layout reserves is
/// exactly the number of rows paint emits.
pub fn wrap_text(text: &str, width: u16, mode: Wrap) -> Vec<String> {
    wrap_rows(text, width, mode)
        .into_iter()
        .map(|r| r.text)
        .collect()
}

/// [`wrap_text`], keeping each row's alignment with the source.
///
/// `src` is maintained beside the text rather than reconstructed from it: the
/// three places a row can end — a break between chunks, a cut through an
/// over-long chunk, the end of a paragraph — are the three places that know
/// how much source went into the row, and each says so where it happens.
pub fn wrap_rows(text: &str, width: u16, mode: Wrap) -> Vec<Row> {
    use unicode_width::UnicodeWidthStr;
    if width == 0 {
        return Vec::new();
    }
    let w = |s: &str| UnicodeWidthStr::width(s);
    let width = width as usize;
    let mut out: Vec<Row> = Vec::new();
    // Where this paragraph starts in `text`. `split` hands back slices without
    // saying where they came from, and the `\n` between two of them belongs to
    // neither row.
    let mut para_start = 0usize;
    for para in text.split('\n') {
        // What every row after the first starts with. Dropped when it would
        // leave the text almost nothing — a deeply indented line in a narrow
        // box is better flush left than one word per row.
        let indent = match mode {
            Wrap::Hanging => {
                let n = para.chars().take_while(|c| *c == ' ').count();
                match n + HANGING_MIN_TEXT <= width {
                    true => n,
                    false => 0,
                }
            }
            _ => 0,
        };
        let pad = " ".repeat(indent);
        let mut row = Row {
            text: String::new(),
            indent: 0,
            src: para_start..para_start,
        };
        let mut first = true;
        // The source byte the next chunk starts at. `chunks` partitions the
        // paragraph exactly, so stepping by their lengths tracks it.
        let mut at = para_start;
        for chunk in chunks(para) {
            // The line's own leading whitespace is source text and stays.
            if first {
                row.text.push_str(chunk);
                first = false;
                at += chunk.len();
                row.src.end = at;
            } else if w(&row.text) + w(chunk) <= width {
                row.text.push_str(chunk);
                at += chunk.len();
                row.src.end = at;
            } else {
                // The break eats the spaces before the chunk, and the next row
                // opens with the hanging indent instead. The eaten spaces are
                // in no row's `src`: they are the gap.
                let body = chunk.trim_start_matches(' ');
                at += chunk.len() - body.len();
                out.push(std::mem::replace(
                    &mut row,
                    Row {
                        text: pad.clone(),
                        indent,
                        src: at..at,
                    },
                ));
                row.text.push_str(body);
                at += body.len();
                row.src.end = at;
            }
            // A chunk too long for a row of its own is cut, and the remainder
            // opens the next row — still behind the indent.
            while w(&row.text) > width {
                let head: String = row.text.chars().take(width).collect();
                let tail: String = row.text.chars().skip(width).collect();
                // The indent is spaces, so `head` is `row.indent` added bytes
                // followed by that many fewer bytes of source: the cut lands
                // on a source boundary as well as a char one.
                let cut = row.src.start + (head.len() - row.indent);
                out.push(Row {
                    text: head,
                    indent: row.indent,
                    src: row.src.start..cut,
                });
                row = Row {
                    text: format!("{pad}{tail}"),
                    indent,
                    src: cut..row.src.end,
                };
            }
        }
        out.push(row);
        para_start = at + 1;
    }
    if out.is_empty() {
        out.push(Row {
            text: String::new(),
            indent: 0,
            src: 0..0,
        });
    }
    out
}

/// The rows an unwrapped string is: its lines, and nothing added or dropped
/// except the `\n` between them.
///
/// Separate from [`wrap_rows`] because no width is involved, and shared with
/// it through [`rows_of`] so that the byte mapping has one input whatever the
/// wrap mode is.
fn source_rows(text: &str) -> Vec<Row> {
    let mut out = Vec::new();
    let mut at = 0usize;
    for line in text.split('\n') {
        out.push(Row {
            text: line.to_string(),
            indent: 0,
            src: at..at + line.len(),
        });
        at += line.len() + 1;
    }
    out
}

/// The rows `text` becomes at `width` under `wrap`.
///
/// **One entry point on purpose.** Both directions of the byte mapping read
/// these rows, as does the shaping that paints them; three walks that each
/// decided for themselves where a break went could disagree, and the failure
/// would be a caret one cell away from the character it is on.
pub fn rows_of(text: &str, width: u16, wrap: Wrap) -> Vec<Row> {
    match wrap {
        Wrap::None => source_rows(text),
        mode => wrap_rows(text, width, mode),
    }
}

/// One painted fragment: a piece of a row, and the theme it paints in.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Frag {
    pub text: Rc<str>,
    pub theme: Option<crate::render::spec::ThemeKey>,
}

/// A run's rows, shaped: what paints, and where each one came from.
///
/// The two halves are produced together and held together because they are
/// two views of one wrap. `frags` concatenate to the row's text; `row.text`
/// minus its indent is `whole[row.src]`. Nothing downstream re-derives either
/// from the other.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Shaping {
    /// The logical string the rows were shaped from.
    ///
    /// **Kept, not recomputed from the props.** `src` indexes *this* string,
    /// and the framework replaces a live object's props without necessarily
    /// re-shaping (see [`TextRender::stale`]) — so re-concatenating the props
    /// would index a string the ranges were never measured against, which is
    /// a wrong answer at best and a panic on a char boundary at worst.
    pub whole: String,
    pub rows: Vec<Row>,
    /// Row `i`'s painted pieces, in order. Parallel to [`Self::rows`].
    pub frags: Vec<Vec<Frag>>,
}

/// Wrap a run's pieces as one logical string, reporting each output row as the
/// fragments that compose it and the source it is.
///
/// The pieces are concatenated, wrapped by the same [`rows_of`] the unstyled
/// path uses — so styled and unstyled text break identically — and the result
/// is then cut back into fragments along the original piece boundaries. A wrap
/// point inside a piece splits it; a row crossing three pieces is three
/// fragments.
///
/// The cutting is done by byte range rather than by stepping a char count
/// alongside the rows: [`Row::src`] already says which bytes the row is, so
/// intersecting it with each piece's range is the whole of it, and there is no
/// second opinion about where the wrap broke.
pub fn shape_runs(runs: &[crate::desc::Run], width: u16, wrap: Wrap) -> Shaping {
    let whole: String = runs.iter().map(|r| &*r.text).collect();
    let rows = rows_of(&whole, width, wrap);

    // Each piece's byte range in `whole`, in order.
    let mut bounds: Vec<(usize, usize)> = Vec::with_capacity(runs.len());
    let mut at = 0usize;
    for r in runs {
        bounds.push((at, at + r.text.len()));
        at += r.text.len();
    }

    let mut frags: Vec<Vec<Frag>> = Vec::with_capacity(rows.len());
    // Rows run forward through the source and so do the pieces, so the search
    // for a row's first piece starts where the last row's did. Without it this
    // would be a scan of every piece per row.
    let mut first = 0usize;
    for row in &rows {
        while first + 1 < bounds.len() && bounds[first].1 <= row.src.start {
            first += 1;
        }
        let mut out: Vec<Frag> = Vec::new();
        // The indent has no source behind it, so it takes the theme of the
        // text it is indenting: the row it belongs to, not the one before.
        if row.indent > 0 {
            out.push(Frag {
                text: Rc::from(" ".repeat(row.indent).as_str()),
                theme: runs.get(first).and_then(|r| r.theme.clone()),
            });
        }
        for (i, &(s, e)) in bounds.iter().enumerate().skip(first) {
            if s >= row.src.end {
                break;
            }
            let (lo, hi) = (s.max(row.src.start), e.min(row.src.end));
            if lo >= hi {
                continue;
            }
            out.push(Frag {
                text: Rc::from(&whole[lo..hi]),
                theme: runs[i].theme.clone(),
            });
        }
        if out.is_empty() {
            out.push(Frag {
                text: Rc::from(""),
                theme: None,
            });
        }
        frags.push(out);
    }

    let mut shaping = Shaping { whole, rows, frags };
    if shaping.rows.is_empty() {
        shaping.rows.push(Row {
            text: String::new(),
            indent: 0,
            src: 0..0,
        });
        shaping.frags.push(vec![Frag {
            text: Rc::from(""),
            theme: None,
        }]);
    }
    shaping
}

/// Which cell of which row the byte at `byte` is drawn in.
///
/// The inverse of [`byte_of`], and the two read the same [`Row::src`] so they
/// cannot disagree about where a break went.
///
/// A byte the wrap *dropped* — the whitespace a break ate, the `\n` between
/// two paragraphs — is in no row. It answers with the end of the row before
/// the gap, which is where a caret on it is drawn. Several dropped bytes in a
/// row therefore share one cell, so the round trip through [`byte_of`] is the
/// identity for the first of them and snaps the rest onto it. Every byte a row
/// *shows* round-trips exactly, which is the property a caret stated in bytes
/// depends on.
pub fn cell_of(rows: &[Row], whole: &str, byte: usize) -> (usize, u16) {
    use unicode_width::UnicodeWidthStr;
    // The last row that starts at or before `byte`. A byte inside a row lands
    // on that row; a byte in the gap after one lands on the row before the
    // gap, clamped below to its end.
    let Some(i) = rows.iter().rposition(|r| r.src.start <= byte) else {
        return (0, 0);
    };
    let row = &rows[i];
    let end = byte.clamp(row.src.start, row.src.end);
    let col = row.indent + UnicodeWidthStr::width(&whole[row.src.start..end]);
    (i, col.min(u16::MAX as usize) as u16)
}

/// The cells each row shows of the byte range `bytes`: `(row, columns)`.
///
/// Built from the same [`Row::src`] [`cell_of`] reads, so a selection and the
/// caret that walks out of it cannot disagree about where the wrap put a byte.
/// A row the range covers none of, or covers only bytes the wrap *dropped* —
/// the whitespace a break ate — contributes nothing, which is why this yields
/// spans rather than one rectangle per row.
///
/// An empty or reversed range selects nothing.
pub(crate) fn selected_spans(
    rows: &[Row],
    whole: &str,
    bytes: std::ops::Range<usize>,
) -> Vec<(usize, std::ops::Range<u16>)> {
    use unicode_width::UnicodeWidthStr;
    let mut out = Vec::new();
    if bytes.start >= bytes.end {
        return out;
    }
    for (i, row) in rows.iter().enumerate() {
        let lo = bytes.start.clamp(row.src.start, row.src.end);
        let hi = bytes.end.clamp(row.src.start, row.src.end);
        if lo >= hi {
            continue;
        }
        let col = |at: usize| -> u16 {
            (row.indent + UnicodeWidthStr::width(&whole[row.src.start..at])).min(u16::MAX as usize)
                as u16
        };
        let (a, b) = (col(lo), col(hi));
        if a < b {
            out.push((i, a..b));
        }
    }
    out
}

/// Which byte of the logical string the cell at `(row, col)` addresses.
///
/// The inverse of [`cell_of`]. `None` when there is no such row.
///
/// Three cells do not name a byte of their own, and each snaps to the nearest
/// one that a caret can sit on:
///
/// * a cell inside the hanging indent — added text, no source — gives the
///   row's first byte;
/// * the second cell of a wide glyph gives that glyph's byte, so a press on
///   either half of `名` puts the caret before it;
/// * a cell past the end of the row's text gives the byte just past that
///   text, which is where a caret at end-of-row sits — what every text field
///   does with a press in its trailing space.
///
/// Zero-width characters are stepped over rather than landed on: a combining
/// mark has no cell, so the answer after its base character is the end of the
/// whole cluster, which is where a caret goes.
pub fn byte_of(rows: &[Row], whole: &str, row: usize, col: i32) -> Option<usize> {
    use unicode_width::UnicodeWidthChar;
    let r = rows.get(row)?;
    if col < r.indent as i32 {
        return Some(r.src.start);
    }
    let mut at = r.indent as i32;
    for (i, c) in whole[r.src.clone()].char_indices() {
        let cw = UnicodeWidthChar::width(c).unwrap_or(0) as i32;
        if cw > 0 && at + cw > col {
            return Some(r.src.start + i);
        }
        at += cw;
    }
    Some(r.src.end)
}

/// Lay children out along the main axis, breaking onto a new line whenever the
/// next child would not fit.
///
/// CSS calls this `flex-wrap: wrap`, and the shape is the same: children are
/// never split — a break happens at a child boundary — so a group that must
/// stay together is a nested non-wrapping box. A child wider than the whole
/// container gets a line to itself and overflows it, which is the honest
/// answer: the alternative is silently shrinking something that said how wide
/// it was.
///
/// **`Flex` on the main axis is treated as `Auto` here**, deliberately. A
/// flexible child absorbs the remainder of its line, so one of them makes
/// every line the full width and there is nothing left to wrap; the two
/// features answer opposite questions ("fill the row" and "let the row become
/// as many rows as it needs") and a container that tried to honour both would
/// silently do neither. Cross-axis sizing is unaffected.
fn wrap_in(
    c: Constraints,
    cx: &mut dyn LayoutCx,
    dir: Dir,
    align: Align,
    gap: u16,
    inset: Point,
) -> Size {
    let kids = cx.children();
    let n = kids.len();
    let avail = main_of(dir, c.max());
    let cross_extent = cross_of(dir, c.max());

    // Every child at its natural main extent. Nothing here depends on which
    // line a child lands on, so this is one pass — the lines are decided from
    // the answers.
    let mut mains = vec![0u16; n];
    let mut crosses = vec![0u16; n];
    for i in 0..n {
        let (sw, sh) = cx.sizing(kids[i]);
        let (s_main, s_cross) = match dir {
            Dir::Row => (sw, sh),
            Dir::Col => (sh, sw),
        };
        let floor = main_floor(dir, cx.floor(kids[i]));
        let main = match s_main {
            Sizing::Cells(v) => (v.min(avail), v.min(avail)),
            Sizing::Pct(p) => {
                let v = pct(avail, p);
                (v, v)
            }
            // See above: flexible means "as big as it needs" in a wrapping box.
            Sizing::Auto | Sizing::Flex(_) => (0, avail),
        };
        let main = (main.0.max(floor), main.1.max(floor));
        // The cross axis is never definite per-child here: a line's extent is
        // not known until the line is full, so a child that wants to fill it
        // is stretched afterwards rather than measured against a guess.
        let cross = range(s_cross, cross_extent, false, Align::Start);
        let s = cx.measure(kids[i], axes(dir, main, cross));
        mains[i] = main_of(dir, s);
        crosses[i] = cross_of(dir, s);
    }

    // Greedy fill. `used` is the line's main extent including the gaps already
    // spent inside it, so the fit test is the same arithmetic as the placement.
    let mut lines: Vec<(usize, usize)> = Vec::new(); // [start, end)
    let mut start = 0usize;
    let mut used = 0u16;
    for i in 0..n {
        let with_gap = match i == start {
            true => mains[i],
            false => mains[i].saturating_add(gap),
        };
        if i > start && used.saturating_add(with_gap) > avail {
            lines.push((start, i));
            start = i;
            used = mains[i];
        } else {
            used = used.saturating_add(with_gap);
        }
    }
    if start < n {
        lines.push((start, n));
    }

    let line_cross: Vec<u16> = lines
        .iter()
        .map(|&(a, b)| crosses[a..b].iter().copied().max().unwrap_or(0))
        .collect();
    let content_main = lines
        .iter()
        .map(|&(a, b)| {
            let sum = mains[a..b].iter().fold(0u16, |x, y| x.saturating_add(*y));
            sum.saturating_add(gap.saturating_mul((b - a).saturating_sub(1) as u16))
        })
        .max()
        .unwrap_or(0);
    let content_cross = line_cross
        .iter()
        .fold(0u16, |x, y| x.saturating_add(*y))
        .saturating_add(gap.saturating_mul(lines.len().saturating_sub(1) as u16));

    // A child that asked to fill its line's cross extent is measured again now
    // that the line is full — the same second pass the non-wrapping path makes
    // when the cross extent was not known in advance, per line instead of per
    // container.
    if align == Align::Stretch {
        for (li, &(a, b)) in lines.iter().enumerate() {
            for i in a..b {
                let (sw, sh) = cx.sizing(kids[i]);
                let s_cross = match dir {
                    Dir::Row => sh,
                    Dir::Col => sw,
                };
                if s_cross == Sizing::Auto && crosses[i] != line_cross[li] {
                    let s = cx.measure(
                        kids[i],
                        axes(dir, (mains[i], mains[i]), (line_cross[li], line_cross[li])),
                    );
                    mains[i] = main_of(dir, s);
                    crosses[i] = cross_of(dir, s);
                }
            }
        }
    }

    let mut cross_at = 0u16;
    for (li, &(a, b)) in lines.iter().enumerate() {
        let mut main_at = 0u16;
        for i in a..b {
            let at = point_of(
                dir,
                main_at as i32,
                cross_at as i32 + align_offset(align, line_cross[li], crosses[i]),
            );
            cx.place(kids[i], Point::new(at.x + inset.x, at.y + inset.y));
            main_at = main_at.saturating_add(mains[i]).saturating_add(gap);
        }
        cross_at = cross_at.saturating_add(line_cross[li]).saturating_add(gap);
    }

    c.constrain(size_of(dir, content_main, content_cross))
}

/// Lay children out one on top of another, each honouring its own size
/// request. What a node with no layout of its own does.
fn stack_in(c: Constraints, cx: &mut dyn LayoutCx, align: Align, inset: Point) -> Size {
    let w_definite = c.min_w == c.max_w;
    let h_definite = c.min_h == c.max_h;
    let mut size = c.min();
    let kids = cx.children();
    let mut sizes = Vec::with_capacity(kids.len());
    for &k in &kids {
        let (sw, sh) = cx.sizing(k);
        // **The floor wins over the sizing, here as in a row.** `min_w` says
        // "never narrower than this, whatever the sizing resolves to", and a
        // stack honouring the sizing but not the floor made that a promise
        // that held in flow layout and quietly did not in a stack — or in a
        // layer, which measures its child through this. A percentage below the
        // floor is exactly where the two differ, so it is exactly where the
        // omission hid.
        let (fw, fh) = cx.floor(k);
        let wc = range(sw, c.max_w, w_definite, align);
        let hc = range(sh, c.max_h, h_definite, align);
        let (wc, hc) = ((wc.0.max(fw), wc.1.max(fw)), (hc.0.max(fh), hc.1.max(fh)));
        let s = cx.measure(k, Constraints::new(wc.0, wc.1, hc.0, hc.1));
        sizes.push(s);
        size = Size::new(size.w.max(s.w), size.h.max(s.h));
    }
    let own = c.constrain(size);
    for (i, &k) in kids.iter().enumerate() {
        cx.place(
            k,
            Point::new(
                inset.x + align_offset(align, own.w, sizes[i].w),
                inset.y + align_offset(align, own.h, sizes[i].h),
            ),
        );
    }
    own
}

// -- Box ---------------------------------------------------------------------

pub struct BoxRender {
    pub props: BoxProps,
}

impl RenderObject for BoxRender {
    fn clips(&self) -> bool {
        self.props.clip
    }

    /// The bound sits inside the border ring and the padding — the same inset
    /// `layout` uses to place children, so the clip and the content rect are
    /// the same rectangle by construction rather than by two copies of the sum.
    fn clip_inset(&self) -> (u16, u16) {
        let border = u16::from(self.props.border.is_some());
        (
            self.props.pad.x.saturating_add(border),
            self.props.pad.y.saturating_add(border),
        )
    }

    fn layout(&mut self, c: Constraints, cx: &mut dyn LayoutCx) -> Size {
        let p = &self.props;
        let border = u16::from(p.border.is_some());
        let ins_x = p.pad.x.saturating_add(border);
        let ins_y = p.pad.y.saturating_add(border);
        let inner = Constraints::new(
            c.min_w.saturating_sub(2 * ins_x),
            c.max_w.saturating_sub(2 * ins_x),
            c.min_h.saturating_sub(2 * ins_y),
            c.max_h.saturating_sub(2 * ins_y),
        );

        if p.stack {
            let content = stack_in(inner, cx, p.align, Point::new(ins_x as i32, ins_y as i32));
            return c.constrain(Size::new(
                content.w.saturating_add(2 * ins_x),
                content.h.saturating_add(2 * ins_y),
            ));
        }

        if p.wrap {
            let content = wrap_in(
                inner,
                cx,
                p.dir,
                p.align,
                p.gap,
                Point::new(ins_x as i32, ins_y as i32),
            );
            return c.constrain(Size::new(
                content.w.saturating_add(2 * ins_x),
                content.h.saturating_add(2 * ins_y),
            ));
        }

        let dir = p.dir;
        let kids = cx.children();
        let n = kids.len();
        let gaps = p.gap.saturating_mul(n.saturating_sub(1) as u16);
        let avail = main_of(dir, inner.max()).saturating_sub(gaps);
        let cross_extent = cross_of(dir, inner.max());
        let cross_definite = match dir {
            Dir::Row => inner.min_h == inner.max_h,
            Dir::Col => inner.min_w == inner.max_w,
        };
        // **Is there a main extent to divide, or is this box being asked how
        // big it wants to be?** A loose main axis is a question, not a room:
        // the answer must not be "as much as you offered", which is what
        // dividing the maximum among the flex children says. See the flex pass
        // below (rule L15).
        let main_definite = match dir {
            Dir::Row => inner.min_w == inner.max_w,
            Dir::Col => inner.min_h == inner.max_h,
        };

        let mut mains = vec![0u16; n];
        let mut crosses = vec![0u16; n];
        let mut weights = vec![0u16; n];
        let mut fixed_used: u16 = 0;

        // Each child's main-axis floor, read once: it is consulted when the
        // child is sized and again when it is placed.
        let floors: Vec<u16> = (0..n).map(|i| main_floor(dir, cx.floor(kids[i]))).collect();

        // **Who yields when the room runs out.** Children resolve against the
        // space that is left, so the order they are *sized* in is the order
        // they get their ask honoured — which is not the order they are
        // *placed* in. Descending `priority` separates the two: a status bar
        // reserves its right-hand side by giving it a higher priority than the
        // left, and the row still paints left to right.
        //
        // Equal priorities keep declaration order (the sort is stable), so a
        // tree that never sets one lays out exactly as it did before.
        let order: Vec<usize> = {
            let mut o: Vec<usize> = (0..n).collect();
            o.sort_by_key(|&i| std::cmp::Reverse(cx.priority(kids[i])));
            o
        };

        // Everything that is not flex resolves first; flex divides what is left.
        for i in order {
            let (sw, sh) = cx.sizing(kids[i]);
            let (s_main, s_cross) = match dir {
                Dir::Row => (sw, sh),
                Dir::Col => (sh, sw),
            };
            if let Sizing::Flex(w) = s_main {
                weights[i] = w.max(1);
                continue;
            }
            let room = avail.saturating_sub(fixed_used);
            let floor = floors[i];
            let main = match s_main {
                Sizing::Cells(v) => (v.min(room), v.min(room)),
                Sizing::Pct(pc) => {
                    let v = pct(avail, pc).min(room);
                    (v, v)
                }
                Sizing::Auto => (0, room),
                Sizing::Flex(_) => unreachable!(),
            };
            // The floor wins over the room left: a child that says it is never
            // narrower than N is never narrower than N, and the overflow is the
            // container's problem — which is the honest answer, because the
            // alternative is a gap that silently closes.
            let main = (main.0.max(floor), main.1.max(floor));
            let cross = range(s_cross, cross_extent, cross_definite, p.align);
            let s = cx.measure(kids[i], axes(dir, main, cross));
            mains[i] = main_of(dir, s);
            crosses[i] = cross_of(dir, s);
            fixed_used = fixed_used.saturating_add(mains[i]);
        }

        let remaining = avail.saturating_sub(fixed_used);
        let shares = distribute(remaining, &weights);
        for i in 0..n {
            if weights[i] == 0 {
                continue;
            }
            let (sw, sh) = cx.sizing(kids[i]);
            let s_cross = match dir {
                Dir::Row => sh,
                Dir::Col => sw,
            };
            let cross = range(s_cross, cross_extent, cross_definite, p.align);
            // **Flex contributes nothing but its floor to an intrinsic
            // measure** (rule L15). Flex means "divide what is left over" —
            // and under a loose main constraint nothing is left over, because
            // nothing has been given out: the box is being measured, and the
            // maximum it was handed is the room its *parent* has, not a size
            // anyone has claimed. Dividing that among the flex children made
            // every `Sizing::Auto` box holding a spacer as wide as the frame,
            // which is why a menu dropdown and an anchored panel measured
            // their rows' text by hand instead of saying `Auto`.
            let share = match main_definite {
                true => shares[i].max(floors[i]),
                false => floors[i],
            };
            let s = cx.measure(kids[i], axes(dir, (share, share), cross));
            mains[i] = main_of(dir, s);
            crosses[i] = cross_of(dir, s);
        }

        let content_main = mains
            .iter()
            .fold(0u16, |a, b| a.saturating_add(*b))
            .saturating_add(gaps);
        let content_cross = crosses.iter().copied().max().unwrap_or(0);
        let content = size_of(dir, content_main, content_cross);
        let mut own = c.constrain(Size::new(
            content.w.saturating_add(2 * ins_x),
            content.h.saturating_add(2 * ins_y),
        ));

        let (ins_main, ins_cross) = match dir {
            Dir::Row => (ins_x, ins_y),
            Dir::Col => (ins_y, ins_x),
        };
        let mut inner_cross = cross_of(dir, own).saturating_sub(2 * ins_cross);

        // Intrinsic sizing: when the cross extent was not known before the
        // children were measured, the ones that asked to fill it are measured
        // again now that it is. This is the case the design document says
        // measures a subtree twice, and the framework counts it so the cost is
        // visible in a dump rather than inferred.
        if p.align == Align::Stretch && !cross_definite && inner_cross > 0 {
            let mut again = false;
            for i in 0..n {
                let (sw, sh) = cx.sizing(kids[i]);
                let s_cross = match dir {
                    Dir::Row => sh,
                    Dir::Col => sw,
                };
                if s_cross == Sizing::Auto && crosses[i] != inner_cross {
                    let s = cx.measure(
                        kids[i],
                        axes(dir, (mains[i], mains[i]), (inner_cross, inner_cross)),
                    );
                    mains[i] = main_of(dir, s);
                    crosses[i] = cross_of(dir, s);
                    again = true;
                }
            }
            // A child measured again at a known cross extent can come back a
            // different size on *both* axes — text that rewraps is shorter. The
            // node's own size is computed from what came back, not from the
            // first pass, or the second measurement would be thrown away.
            if again {
                let content_main = mains
                    .iter()
                    .fold(0u16, |a, b| a.saturating_add(*b))
                    .saturating_add(gaps);
                let content_cross = crosses.iter().copied().max().unwrap_or(0);
                let content = size_of(dir, content_main, content_cross);
                own = c.constrain(Size::new(
                    content.w.saturating_add(2 * ins_x),
                    content.h.saturating_add(2 * ins_y),
                ));
                inner_cross = cross_of(dir, own).saturating_sub(2 * ins_cross);
            }
        }

        // The far edge of the content box. Children are never placed past it:
        // when the gaps alone exceed the space, the run would otherwise walk
        // out of its own parent and paint over whatever is beside it.
        let limit = ins_main as i32 + main_of(dir, own).saturating_sub(2 * ins_main) as i32;
        let mut pos = ins_main as i32;
        // How far back the clamp may pull a child. It moves forward past every
        // child that declared a floor: those cells were asked for explicitly,
        // and sliding the next child over them would honour the floor in the
        // arithmetic while erasing it on screen — a separator that measures
        // three cells and shows one. Content overflows and is clipped instead,
        // which is the outcome the caller asked for by naming a minimum.
        let mut barrier = ins_main as i32;
        for i in 0..n {
            let off = align_offset(p.align, inner_cross, crosses[i]);
            let at = pos.min(limit - mains[i] as i32).max(barrier);
            cx.place(kids[i], point_of(dir, at, ins_cross as i32 + off));
            pos = at + mains[i] as i32 + p.gap as i32;
            if floors[i] > 0 {
                barrier = at + mains[i] as i32;
            }
        }
        own
    }

    fn paint(&self, g: Geom, out: &mut DrawList) {
        if let Some(style) = self.props.border {
            out.push(Draw::Border(style), g);
        }
    }

    fn render_name(&self) -> &'static str {
        "Box"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

// -- TextRun -----------------------------------------------------------------

/// The ellipsis. One cell, so the arithmetic below is `width - 1` and not a
/// second measurement.
const ELLIPSIS: &str = "…";

/// Cut a row of fragments down to `w` cells, marking the cut.
///
/// Done at paint, not at measure, and that is the whole reason this exists:
/// layout is what decides how many cells a run gets — `priority` and `flex`
/// both hand a run less than it measured at — so nothing before paint knows
/// what to cut to.
///
/// Widths, not chars: a CJK glyph is two cells and a combining mark is zero, so
/// counting characters puts the ellipsis in the wrong place and, worse, can cut
/// a fragment mid-glyph. Fragments are walked whole and only the one straddling
/// the boundary is split, so a styled run keeps its pieces' colours.
fn elide_row(row: &[Frag], w: u16, mode: crate::desc::Elide) -> Vec<Frag> {
    use unicode_width::UnicodeWidthStr;
    let total: usize = row.iter().map(|f| UnicodeWidthStr::width(&*f.text)).sum();
    if mode == crate::desc::Elide::None || total <= w as usize {
        return row.to_vec();
    }
    if w == 0 {
        return Vec::new();
    }
    // One cell for the mark. At a width of exactly one that is all there is
    // room for, which is the honest answer: something was here.
    let budget = w as usize - 1;
    let mark = |f: Option<&Frag>| Frag {
        text: Rc::from(ELLIPSIS),
        theme: f.and_then(|f| f.theme.clone()),
    };

    // `take` walks a fragment in one direction and returns the piece that fits
    // in `left` cells, along with what it consumed.
    //
    // Walked by grapheme cluster, so a cut never separates a mark from its
    // base or splits an emoji sequence — the units the painter advances by
    // (`glyph`) are the units the cut is made in.
    let take = |f: &Frag, left: usize, from_end: bool| -> (String, usize) {
        use unicode_segmentation::UnicodeSegmentation;
        let mut parts: Vec<&str> = f.text.graphemes(true).collect();
        if from_end {
            parts.reverse();
        }
        let (mut used, mut kept) = (0usize, Vec::new());
        for part in parts {
            let cw = UnicodeWidthStr::width(part);
            if used + cw > left {
                break;
            }
            used += cw;
            kept.push(part);
        }
        if from_end {
            kept.reverse();
        }
        (kept.concat(), used)
    };

    let mut kept: Vec<Frag> = Vec::new();
    let mut left = budget;
    match mode {
        crate::desc::Elide::Tail => {
            for f in row {
                if left == 0 {
                    break;
                }
                let (seg, used) = take(f, left, false);
                left -= used;
                if !seg.is_empty() {
                    kept.push(Frag {
                        text: Rc::from(seg.as_str()),
                        theme: f.theme.clone(),
                    });
                }
            }
            let last = kept.last().cloned();
            kept.push(mark(last.as_ref()));
        }
        crate::desc::Elide::Head => {
            for f in row.iter().rev() {
                if left == 0 {
                    break;
                }
                let (seg, used) = take(f, left, true);
                left -= used;
                if !seg.is_empty() {
                    kept.push(Frag {
                        text: Rc::from(seg.as_str()),
                        theme: f.theme.clone(),
                    });
                }
            }
            kept.reverse();
            let first = kept.first().cloned();
            kept.insert(0, mark(first.as_ref()));
        }
        crate::desc::Elide::None => unreachable!(),
    }
    kept
}

pub struct TextRender {
    pub props: TextProps,
    /// The wrapped rows, computed at measure time and reused by paint so the
    /// two cannot disagree — and by the byte mapping, so a caret and a press
    /// answer against the rows that are actually on screen rather than a
    /// second opinion about them.
    shaped: Shaping,
    /// Whether [`Self::shaped`] was shaped from the props now held.
    ///
    /// **A props change that does not reach a re-measure must not paint the
    /// old text.** `shaped` is a cache of `props`, and the framework replaces
    /// `props` in place whenever the description changes; it is the *measure*
    /// that refreshes the cache. Normally a changed run marks the element for
    /// layout and the two happen together — but a `layout_reader` rebuilds
    /// during the layout pass itself, and the dirt its reconcile raises is
    /// against render links the relink immediately afterwards replaces, so
    /// the mark is dropped and the measure never comes. The text then painted
    /// one frame behind the description it was built from.
    ///
    /// Rather than depend on every path remembering to re-measure, the cache
    /// says whether it is still true, and paint re-shapes when it is not.
    pub(crate) stale: bool,
    /// The pieces [`Self::shaped`] was shaped from, when it was shaped
    /// unwrapped — `None` after a wrapped shaping, whose rows depend on a
    /// width as well.
    ///
    /// An unwrapped shaping is a function of the pieces alone, so a measure
    /// that finds the same pieces keeps it rather than joining and re-cutting
    /// them. The editor rebuilds its tree every frame with fresh `Rc`s around
    /// unchanged text, so this compares content when the pointers differ:
    /// a walk over the bytes, against a shaping that allocates per row and
    /// per fragment.
    shaped_from: Option<Rc<[crate::desc::Run]>>,
}

impl TextRender {
    pub fn new(props: TextProps) -> Self {
        TextRender {
            props,
            shaped: Shaping::default(),
            stale: false,
            shaped_from: None,
        }
    }

    /// Whether [`Self::shaped`] is the unwrapped shaping of the pieces held
    /// now.
    /// Which row of the shaping holds `byte` of the run's logical string.
    ///
    /// The reveal's half of [`cell_of`]: a caret is stated in bytes, and only
    /// the shaping knows what row the wrap put one on. Reads the rows paint
    /// draws, so a reveal and the caret it is chasing cannot disagree.
    /// The rows this run was last shaped into, and the string they index.
    ///
    /// **The wrap's answer, at the width layout settled on** — which is the
    /// point of reading it here rather than wrapping the text a second time.
    /// A caller that re-wraps is guessing at that width (`§6.6`), and a caller
    /// that reconstructs the string from its own runs indexes something the
    /// ranges were never measured against; both are why [`Shaping::whole`] is
    /// kept rather than recomputed.
    ///
    /// What it is *for* is the question neither a byte caret nor a byte press
    /// answers: **"which byte is one rendered row below this one"**, asked from
    /// a key handler that has no width and no tree. With the rows in hand it is
    /// [`cell_of`] then [`byte_of`], the same two directions everything else
    /// here reads.
    pub fn shaped_rows(&self) -> (&str, &[Row]) {
        (&self.shaped.whole, &self.shaped.rows)
    }

    pub(crate) fn row_of_byte(&self, byte: usize) -> usize {
        cell_of(&self.shaped.rows, &self.shaped.whole, byte).0
    }

    fn shaped_unwrapped_from_props(&self) -> bool {
        match &self.shaped_from {
            Some(from) => Rc::ptr_eq(from, &self.props.runs) || **from == *self.props.runs,
            None => false,
        }
    }
}

/// The extent of `runs` laid out unwrapped: the widest line in cells, and how
/// many lines there are.
///
/// Measured from the pieces, without joining them (plan §2.5). A `\n` inside a
/// piece ends a line, and a line's width is the sum of the widths of the
/// pieces of it — which is also how paint places them, one item per fragment,
/// so a cluster a piece boundary happens to split measures and paints the
/// same (see [`glyph`](crate::render::glyph) for what that does to it).
pub fn unwrapped_extent(runs: &[crate::desc::Run]) -> (u16, usize) {
    let mut widest = 0usize;
    let mut cur = 0usize;
    let mut lines = 1usize;
    for r in runs {
        let mut parts = r.text.split('\n');
        cur += crate::render::glyph::width(parts.next().unwrap_or("")) as usize;
        for part in parts {
            widest = widest.max(cur);
            cur = crate::render::glyph::width(part) as usize;
            lines += 1;
        }
    }
    (widest.max(cur).min(u16::MAX as usize) as u16, lines)
}

impl RenderObject for TextRender {
    fn layout(&mut self, c: Constraints, _cx: &mut dyn LayoutCx) -> Size {
        // Measured from the pieces, never by joining them into a string of
        // their own (plan §2.5): the natural width is a sum of piece widths,
        // and shaping — which does join, because a wrap point may fall inside
        // a piece and the byte mapping indexes the joined string — happens
        // once per distinct content rather than once per measure.
        let (natural, _) = unwrapped_extent(&self.props.runs);
        if self.props.wrap != Wrap::None {
            let w = if c.min_w > 0 {
                c.max_w
            } else {
                c.max_w.min(natural.max(1))
            };
            // The pieces are one logical run, so styling never changes where
            // the text breaks: `shape_runs` wraps them as one string.
            self.shaped = shape_runs(&self.props.runs, w, self.props.wrap);
            self.shaped_from = None;
            self.stale = false;
            c.constrain(Size::new(
                w,
                self.shaped.rows.len().min(u16::MAX as usize) as u16,
            ))
        } else {
            if !self.shaped_unwrapped_from_props() {
                self.shaped = shape_runs(&self.props.runs, 0, Wrap::None);
                self.shaped_from = Some(self.props.runs.clone());
            }
            self.stale = false;
            c.constrain(Size::new(natural, self.shaped.rows.len().max(1) as u16))
        }
    }

    fn paint(&self, g: Geom, out: &mut DrawList) {
        // What was actually given, cut to fit. Wrapped text has no overflow to
        // mark, and `Elide::None` is the whole of today's behaviour, so both
        // pass straight through.
        // The cache is only usable while it still describes these props; see
        // [`TextRender::stale`]. Re-shaping here uses the width layout settled
        // on, which is the width the rows would have been shaped to.
        let reshaped: Shaping;
        let shaping: &Shaping = match self.stale {
            false => &self.shaped,
            true => {
                reshaped = match self.props.wrap {
                    Wrap::None => shape_runs(&self.props.runs, 0, Wrap::None),
                    wrap => shape_runs(&self.props.runs, g.rect.w, wrap),
                };
                &reshaped
            }
        };
        let shaped: &[Vec<Frag>] = &shaping.frags;
        let elided: Vec<Vec<Frag>>;
        let rows: &[Vec<Frag>] =
            if self.props.wrap != Wrap::None || self.props.elide == crate::desc::Elide::None {
                shaped
            } else {
                elided = shaped
                    .iter()
                    .map(|r| elide_row(r, g.rect.w, self.props.elide))
                    .collect();
                &elided
            };

        // An unstyled run is one item, exactly as before. A styled one emits an
        // item per fragment, each carrying its own theme — so the display list
        // keeps its one-theme-per-item contract and no backend has to learn
        // about spans. `LayoutSpec::index` already maps a key to a *range* of
        // items, so several items for one node is the existing model.
        if rows.iter().all(|r| r.iter().all(|f| f.theme.is_none())) {
            let lines: Vec<Rc<str>> = rows
                .iter()
                .map(|r| {
                    // An elided row is more than one fragment even unstyled —
                    // the content and the mark — so this joins rather than
                    // taking the first.
                    match r.len() {
                        0 => Rc::from(""),
                        1 => r[0].text.clone(),
                        _ => Rc::from(r.iter().map(|f| &*f.text).collect::<String>().as_str()),
                    }
                })
                .collect();
            out.push(Draw::Lines(lines), g);
        } else {
            for (i, row) in rows.iter().enumerate() {
                let mut x = g.rect.x;
                for frag in row {
                    // Display cells, as layout measured and as every backend
                    // advances (`glyph`), so the next fragment starts where
                    // this one's last cluster ends.
                    let w = crate::render::glyph::width(&frag.text);
                    if w == 0 {
                        continue;
                    }
                    let rect = Rect::new(x, g.rect.y + i as i32, w, 1);
                    let draw = Draw::Lines(vec![frag.text.clone()]);
                    match &frag.theme {
                        // A piece with its own theme names it; one without
                        // inherits the node's, which is what `push_at` uses.
                        Some(t) => out.push_themed(draw, rect, g.clip, t.clone()),
                        None => out.push_at(draw, rect, g.clip),
                    }
                    x += w as i32;
                }
            }
        }
        // A selection is a range of the run's own string, and these are the
        // cells the wrap put those bytes on. Pushed after the text so the wash
        // lies over it: the glyphs stay and take the selection's ground, which
        // is what keeps a styled run's colours under a selection.
        if let Some((bytes, theme)) = &self.props.selection {
            for (row, span) in selected_spans(&shaping.rows, &shaping.whole, bytes.clone()) {
                let rect = Rect::new(
                    g.rect.x + span.start as i32,
                    g.rect.y + row as i32,
                    span.end - span.start,
                    1,
                );
                out.push_themed(Draw::Wash, rect, g.clip, theme.clone());
            }
        }
        // The caret is a byte of the run's own string, and this is where the
        // wrap put it. Placing it here rather than asking the caller for a row
        // and a column is the point of stating it in bytes: only the shaping
        // above knows which row a byte landed on.
        if let Some(byte) = self.props.cursor {
            let (row, col) = cell_of(&shaping.rows, &shaping.whole, byte);
            let at = Point::new(g.rect.x + col as i32, g.rect.y + row as i32);
            // A caret the window is not showing is not on screen. Wrapped text
            // is routinely taller than the rect it is scrolled inside, and a
            // terminal has one cursor — placing it on a clipped row would put
            // it somewhere the text it belongs to is not drawn.
            //
            // **A caret is a point between cells, so the trailing edge is on
            // the row.** The position after the last visible glyph is where
            // typing appends, and it lies on the clip's right edge rather
            // than inside it; an empty run's only caret is that edge. Rows
            // are cells, and a caret on a row the clip does not show is off.
            let on_row = at.y >= g.clip.y && at.y < g.clip.bottom();
            let on_span = at.x >= g.clip.x && at.x <= g.clip.right();
            if on_row && on_span {
                out.set_cursor(at);
            }
        }
    }

    /// Where in the logical string the cell at `local` is.
    ///
    /// **The rows this walks are the ones layout shaped and paint draws**
    /// (`Self::shaped`), so the answer is the text that is actually on screen
    /// rather than a second opinion about it — which is the whole reason this
    /// lives here and not in the caller. See `Event::text_byte`.
    ///
    /// Wrapped text answers too, and by the same mapping the caret is placed
    /// through: [`Row::src`] says which bytes each row is, so a press resolves
    /// against the row it actually landed on. What the wrap *dropped* — the
    /// space a break ate — belongs to no row, and a press at the end of a row
    /// reports the last byte the row shows, which is where the caret is drawn.
    ///
    /// A press past the end of a row is the end of that row, not `None` — a
    /// caret placed past the last grapheme is what every text field does with
    /// a click in its trailing space. [`byte_of`] lists the other two cells
    /// that snap rather than naming a byte of their own.
    ///
    /// **[`Elide`](crate::desc::Elide) is not accounted for, and for
    /// `Elide::Head` that is wrong.** Elision happens at paint from the width
    /// layout settled on, and this walk has neither — it sees the unelided
    /// rows. `Elide::Tail` only removes a tail, so a cell that has text under
    /// it has the text this walk finds; `Elide::Head` removes a *head*, and
    /// every cell of such a row is then some bytes further along than this
    /// reports. Closing it means the object remembering the width it painted
    /// at, and mapping through the elided row. Wrapping runs never elide
    /// (`paint` skips it for them), so the two do not compound.
    fn text_byte_at(&self, local: Point) -> Option<usize> {
        if local.x < 0 || local.y < 0 {
            return None;
        }
        byte_of(
            &self.shaped.rows,
            &self.shaped.whole,
            local.y as usize,
            local.x,
        )
    }

    fn render_name(&self) -> &'static str {
        "TextRun"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

// -- Viewport ----------------------------------------------------------------

pub struct ViewportRender {
    pub props: ViewportProps,
    /// The window this viewport shows, in its content's coordinates. Read by a
    /// constraint-dependent builder inside it.
    pub window: Rect,
    pub content: Size,
    pub items: u32,
    /// Whether the description's initial offset has been applied. It is the
    /// initial value only: after that the offset is framework-owned.
    placed: bool,
    /// Whether the last layout gave the bar a gutter. Read only by a measured
    /// band, as the assumption its first measurement starts from.
    gutter: bool,
    /// The last measured band, and what it was measured against:
    /// `(width, count, cells)`. Only
    /// [`ItemHeight::Measured`](crate::desc::ItemHeight::Measured) fills it.
    ///
    /// **This is what keeps the invariant.** An index-scrolled window answers
    /// "which items am I holding" by dividing, and the divisor has to be there
    /// before the builder runs — so if it were recomputed whenever it was
    /// needed, it would be recomputed on every scroll, and a measured band
    /// would cost O(count) per wheel notch. Cached against the width, the
    /// measurement is paid where the answer can actually differ: the width, and
    /// (through [`ViewportRender::set_props`]) a new description of the items.
    band: Option<(u16, u32, u16)>,
}

impl ViewportRender {
    pub fn new(props: ViewportProps) -> Self {
        ViewportRender {
            props,
            window: Rect::ZERO,
            content: Size::ZERO,
            items: 0,
            placed: false,
            gutter: false,
            band: None,
        }
    }

    /// Take a new description, and with it drop any measured band — see the
    /// comment at the `Desc::Viewport` arm of `sync_render` for why the arrival
    /// of a description is the signal that the items may have changed.
    pub fn set_props(&mut self, props: ViewportProps) {
        self.props = props;
        self.band = None;
    }

    /// One item's height at this width, asked of the child.
    ///
    /// The ask is published the same way the window is, so the builder below is
    /// invalidated by it and runs again — which is what makes this and the
    /// statement that follows it two different questions at the same
    /// constraints rather than one cached answer. Whatever the child measures
    /// to is the band; nothing built to answer is placed, painted or kept, and
    /// the statement below replaces it before the pass ends.
    ///
    /// The height offered is unbounded rather than the window's: an item taller
    /// than the window still sets the band, because the band is a fact about
    /// the item.
    fn ask_band(&mut self, cx: &mut dyn LayoutCx, w: u16, count: u32) -> u16 {
        if let Some((cw, cn, cells)) = self.band {
            if cw == w && cn == count {
                return cells;
            }
        }
        // The window is left exactly where it was, because the question is
        // about the items and not about which of them are on screen — and
        // because a builder that keeps its window filled while it answers keeps
        // the elements in it. Widening the window to "every item" for the
        // duration of the ask would mount every row, twice over: once as the
        // thing being measured and once as a window onto everything.
        cx.set_scroll(crate::render::object::ScrollInfo {
            window: self.window,
            content: Size::new(self.window.w, self.window.h),
            max: Point::new(0, count.saturating_sub(self.window.h as u32) as i32),
            translate: false,
            band: Some(crate::render::object::Band::Measuring),
        });
        let probe = Constraints::new(w, w, 0, u16::MAX);
        let mut cells = 0u16;
        for k in cx.children() {
            cells = cells.max(cx.measure(k, probe).h);
        }
        // A band of zero cells would make the window infinite and the index
        // meaningless; an item that measures nothing still occupies its row.
        let cells = cells.max(1);
        self.band = Some((w, count, cells));
        cells
    }

    /// The height this viewport is willing to take, which is what the
    /// constraints allow unless the description asked for less.
    fn bound(&self, c: Constraints) -> Constraints {
        match self.props.max_h {
            Some(m) => Constraints::new(c.min_w, c.max_w, c.min_h.min(m), c.max_h.min(m)),
            None => c,
        }
    }
}

impl RenderObject for ViewportRender {
    fn layout(&mut self, c: Constraints, cx: &mut dyn LayoutCx) -> Size {
        use crate::desc::ScrollMode;
        use crate::render::object::ScrollInfo;

        let c = self.bound(c);
        // A viewport takes the space it is given; its content does not affect
        // it, which is what makes it a relayout boundary.
        let own = c.constrain(c.max());
        if !self.placed {
            // The description states where the window starts; from here on the
            // offset belongs to the framework and survives rebuilds.
            self.placed = true;
            let (x, y) = self.props.scroll;
            if x != 0 || y != 0 {
                cx.set_offset(Point::new(x as i32, y as i32));
            }
        }
        let scroll = cx.scroll();

        let mut own = own;
        match self.props.mode {
            ScrollMode::Cells => {
                let w = own.w;
                self.window = Rect::at(scroll, own);
                cx.set_scroll(ScrollInfo {
                    window: self.window,
                    content: self.content,
                    max: Point::new(
                        self.content.w.saturating_sub(w) as i32,
                        self.content.h.saturating_sub(own.h) as i32,
                    ),
                    translate: true,
                    // An offset in cells has no items, so no band.
                    band: None,
                });
                let inner = if c.min_w == c.max_w {
                    Constraints::new(w, w, 0, u16::MAX)
                } else {
                    Constraints::new(0, w, 0, u16::MAX)
                };
                let mut content = Size::ZERO;
                for k in cx.children() {
                    let s = cx.measure(k, inner);
                    cx.place(k, Point::ZERO);
                    content = Size::new(content.w.max(s.w), content.h.max(s.h));
                }
                self.content = content;
                // Given a definite extent, a viewport takes it. Given a loose
                // one, it is as tall as its content: a window is only a window
                // once something bounds it.
                own = c.constrain(Size::new(content.w.max(c.min_w), content.h));
                // A vertical scrollbar takes a one-column gutter when the
                // content is taller than the window; re-measure the content in
                // the narrower area so it does not run under the bar. A stable
                // gutter keeps the column whether the bar is drawn or not, so
                // the content does not reflow when the list crosses the length
                // that makes it overflow. An overlay bar carves nothing and
                // floats over the last column, which is what lets it come and
                // go without moving the content under the pointer.
                let gutter = u16::from(
                    self.props.scrollbar
                        && !self.props.overlay
                        && (self.props.stable_gutter || content.h > own.h),
                );
                if gutter > 0 {
                    let inner_w = own.w.saturating_sub(gutter);
                    let narrow = if c.min_w == c.max_w {
                        Constraints::new(inner_w, inner_w, 0, u16::MAX)
                    } else {
                        Constraints::new(0, inner_w, 0, u16::MAX)
                    };
                    let mut re = Size::ZERO;
                    for k in cx.children() {
                        let s = cx.measure(k, narrow);
                        re = Size::new(re.w.max(s.w), re.h.max(s.h));
                    }
                    self.content = re;
                    content = re;
                }
                let view_w = own.w.saturating_sub(gutter);
                self.window = Rect::at(scroll, Size::new(view_w, own.h));
                cx.set_scroll(ScrollInfo {
                    window: self.window,
                    content,
                    max: Point::new(
                        content.w.saturating_sub(view_w) as i32,
                        content.h.saturating_sub(own.h) as i32,
                    ),
                    translate: true,
                    // An offset in cells has no items, so no band.
                    band: None,
                });
            }
            ScrollMode::Items {
                count: n,
                height: item_h,
            } => {
                use crate::desc::ItemHeight;
                let measured = matches!(item_h, ItemHeight::Measured);
                // The child renders only the window, so nothing is translated
                // and the offset is an index. A cell extent over a million rows
                // would not fit a coordinate; an index does.
                //
                // A loose width is resolved the intrinsic way: measure the
                // window once to learn how wide its rows are, then again at
                // that width. The framework counts the second look.
                //
                // A measured band does not change that: the width is settled
                // from the window the child is already showing, and only then
                // is the band measured against it. Asking about every item here
                // instead would be a second O(count) look, at a width that is
                // about to change.
                if c.min_w != c.max_w {
                    let probe = Constraints::new(0, c.max_w, 0, own.h);
                    let mut natural = 0u16;
                    for k in cx.children() {
                        natural = natural.max(cx.measure(k, probe).w);
                    }
                    own.w = c.constrain(Size::new(natural, own.h)).w;
                }
                // The band, and with it the bar's column.
                //
                // **A conditional bar and a measured band each depend on the
                // other.** The gutter narrows the items, narrower items can be
                // taller, taller items mean fewer fit, and fewer fitting is
                // what asks for the bar. The knot is cut by measuring against
                // an assumed gutter and measuring once more if the assumption
                // was wrong — starting from last frame's answer, which is
                // right on every frame but the one where it changes, so the
                // steady state is one measurement against one width rather
                // than two against two. A second flip would be the start of a
                // loop, so the second answer stands.
                //
                // A stated band has none of this: the number does not depend
                // on the width, so the gutter is decided once, from the rows,
                // exactly as it always was.
                let bar = self.props.scrollbar && !self.props.overlay;
                let mut gutter =
                    u16::from(bar && (self.props.stable_gutter || (measured && self.gutter)));
                let mut height;
                let mut rows;
                let mut pass = 0;
                loop {
                    height = match item_h {
                        ItemHeight::Cells(h) => h.max(1),
                        ItemHeight::Measured => self.ask_band(cx, own.w.saturating_sub(gutter), n),
                    };
                    // Given a loose height, the viewport is as tall as its
                    // items — which is `count * height` cells, not `count`
                    // rows.
                    let want = (n.saturating_mul(height as u32)).min(u16::MAX as u32) as u16;
                    own = c.constrain(Size::new(own.w, want));
                    // The window is stated in *items*, because that is what the
                    // offset counts and what the builder inside it asks for.
                    // Cells enter only here, dividing.
                    rows = (own.h / height) as u32;
                    // When the content overflows and a scrollbar is asked for,
                    // the last column is a gutter the scrollbar owns, so the
                    // rows are not laid out under it. A stable gutter reserves
                    // it either way; an overlay bar asks for none and floats
                    // over the rows instead — see [`RenderObject::paint_over`].
                    let need = u16::from(bar && (self.props.stable_gutter || n > rows));
                    if !measured {
                        gutter = need;
                        break;
                    }
                    pass += 1;
                    if need == gutter || pass == 2 {
                        break;
                    }
                    gutter = need;
                }
                self.gutter = gutter == 1;
                self.items = n;
                let inner_w = own.w.saturating_sub(gutter);
                self.window = Rect::new(0, scroll.y, inner_w, rows.min(u16::MAX as u32) as u16);
                cx.set_scroll(ScrollInfo {
                    window: self.window,
                    content: Size::new(inner_w, rows.min(u16::MAX as u32) as u16),
                    max: Point::new(0, n.saturating_sub(rows) as i32),
                    translate: false,
                    // The band the rows are about to be built against. Telling
                    // the builder is the other half of asking it: a measured
                    // band is known only here, and a row built at the wrong
                    // height puts every index below it on the wrong cell.
                    band: Some(crate::render::object::Band::Cells(height)),
                });
                let inner = Constraints::new(inner_w, inner_w, 0, own.h);
                for k in cx.children() {
                    cx.measure(k, inner);
                    cx.place(k, Point::ZERO);
                }
                self.content = own;
            }
        }
        own
    }

    fn paint(&self, g: Geom, out: &mut DrawList) {
        if self.props.selectable {
            out.push(Draw::Selectable, g);
        }
    }

    /// **The bar is on top of the window, always.** With a gutter it does not
    /// overlap anything, so this changes nothing; without one — a revealed
    /// overlay bar — it is the difference between a bar and no bar, because a
    /// node's own paint is under its children and the rows would cover it.
    fn paint_over(&self, g: Geom, out: &mut DrawList) {
        use crate::desc::ScrollMode;
        // A revealed bar that is not being revealed draws nothing. Its
        // gutter, when it has one, is still reserved.
        if !self.props.scrollbar || self.props.bar_hidden {
            return;
        }
        let (offset, content) = match self.props.mode {
            ScrollMode::Cells => (self.window.y.max(0) as u32, self.content.h as u32),
            ScrollMode::Items { count, .. } => (self.window.y.max(0) as u32, count),
        };
        // The window is in the same unit the offset and the content are: cells
        // for `Cells`, items for `Items`. They differ once an item is more than
        // one cell tall, and taking the rectangle's height for both is what
        // made a card list's thumb read as a line list's.
        let window = self.window.h;
        if content <= window as u32 {
            return;
        }
        let bar = Draw::scrollbar(offset, content, window);
        let rect = Rect::new(g.rect.right() - 1, g.rect.y, 1, g.rect.h);
        match &self.props.bar_theme {
            Some(t) => out.push_themed(bar, rect, g.clip, crate::ThemeKey(Some(t.clone()))),
            None => out.push_at(bar, rect, g.clip),
        }
    }

    fn relayout_boundary(&self) -> bool {
        true
    }

    fn clips(&self) -> bool {
        true
    }

    fn shows_scrollbar(&self) -> bool {
        // What this answers is whether the column is grabbable, so a
        // withheld bar answers no: `hit.rs` starts a drag from a press on the
        // track before propagation, and an invisible track would swallow a
        // press aimed at the row behind it.
        self.props.scrollbar && !self.props.bar_hidden
    }

    fn render_name(&self) -> &'static str {
        "Viewport"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

// -- Gesture -----------------------------------------------------------------

/// A pointer region and its listeners.
///
/// It has no `hit` of its own any more: whether a node absorbs a pointer is
/// `Node::pointer`, which every node carries, and the hit walk reads it there.
/// A gesture that says nothing about it is opaque, as it always was.
pub struct GestureRender;

impl RenderObject for GestureRender {
    fn layout(&mut self, c: Constraints, cx: &mut dyn LayoutCx) -> Size {
        stack_in(c, cx, Align::Stretch, Point::ZERO)
    }

    fn render_name(&self) -> &'static str {
        "Gesture"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

// -- Focusable ---------------------------------------------------------------

/// Holds this node's focus registration. Created and torn down with the
/// element, which is why focus survives reconciliation.
pub struct FocusRender {
    pub reg: FocusReg,
}

impl RenderObject for FocusRender {
    fn layout(&mut self, c: Constraints, cx: &mut dyn LayoutCx) -> Size {
        stack_in(c, cx, Align::Stretch, Point::ZERO)
    }

    fn focus_reg(&self) -> Option<FocusReg> {
        Some(self.reg.clone())
    }

    fn render_name(&self) -> &'static str {
        "Focusable"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

// -- Layer -------------------------------------------------------------------

pub struct LayerRender {
    pub geom: LayerGeom,
}

impl LayerRender {
    pub fn from_props<M>(p: &LayerProps<M>) -> Self {
        LayerRender {
            geom: LayerGeom {
                anchor: p.anchor.clone(),
                place: p.place,
                fit: p.fit,
                align: p.align,
                modality: p.modality,
                scrim: p.scrim,
                dismiss: p.dismiss,
                within: p.within.clone(),
                scope: p.scope.clone(),
                offset: p.offset,
            },
        }
    }
}

impl RenderObject for LayerRender {
    fn layout(&mut self, c: Constraints, cx: &mut dyn LayoutCx) -> Size {
        stack_in(c, cx, Align::Stretch, Point::ZERO)
    }

    fn out_of_flow(&self) -> bool {
        true
    }

    fn layer(&self) -> Option<LayerGeom> {
        Some(self.geom.clone())
    }

    /// A modal layer groups the focusables inside it without being one: that is
    /// all "traversal is confined to the modal" means.
    ///
    /// Asked of the *keyboard* channel, so a `Modality::Pointer` layer opens
    /// no scope: it never claimed the keyboard, and grouping focusables under
    /// it would confine traversal to a layer that has no business holding it.
    fn focus_reg(&self) -> Option<FocusReg> {
        self.geom.modality.owns_keyboard().then(|| FocusReg {
            ordinal: None,
            skip: true,
            scope: true,
            focus_within: false,
            autofocus: false,
            entry: None,
        })
    }

    fn render_name(&self) -> &'static str {
        "Layer"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

// -- LayoutReader ------------------------------------------------------------

/// Structure that depends on the constraints. Its builder runs here, inside the
/// layout pass, with the constraints as an argument — so the dependency is
/// scoped to this node and evaluated in the right pass rather than becoming a
/// build/layout cycle or a one-frame lag.
#[derive(Default)]
pub struct ReaderRender {
    last: Option<LayoutInfo>,
}

impl ReaderRender {
    /// The builder is a new closure over new state every build, so a
    /// description change must re-run it even when the constraints are
    /// unchanged. Only the framework knows the description changed.
    pub fn invalidate(&mut self) {
        self.last = None;
    }
}

impl RenderObject for ReaderRender {
    fn reads_window(&self) -> bool {
        true
    }

    fn layout(&mut self, c: Constraints, cx: &mut dyn LayoutCx) -> Size {
        let info = LayoutInfo {
            constraints: c,
            scroll_window: None,
            band: None,
        };
        let info = cx.enclosing_window(info);
        if self.last != Some(info) {
            self.last = Some(info);
            cx.rebuild(info);
        }
        stack_in(c, cx, Align::Stretch, Point::ZERO)
    }

    fn render_name(&self) -> &'static str {
        "LayoutReader"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

#[cfg(test)]
mod measure_tests {
    //! How a `TextRun` measures (plan §2.5): from its pieces, not from a
    //! string joined out of them, and shaping the unwrapped case once per
    //! distinct content rather than once per measure.

    use std::rc::Rc;

    use super::{unwrapped_extent, TextRender};
    use crate::desc::{Elide, Run, TextProps, Wrap};
    use crate::render::geom::{Constraints, Point, Size};
    use crate::render::object::{LayoutCx, LayoutInfo, RenderId, RenderObject, ScrollInfo};

    /// A leaf has no children and asks its context for nothing, so this is
    /// the least a `LayoutCx` can be.
    struct Leaf;

    impl LayoutCx for Leaf {
        fn children(&self) -> Vec<RenderId> {
            Vec::new()
        }
        fn sizing(&self, _: RenderId) -> (crate::desc::Sizing, crate::desc::Sizing) {
            unreachable!("a text run has no children")
        }
        fn measure(&mut self, _: RenderId, _: Constraints) -> Size {
            unreachable!("a text run has no children")
        }
        fn place(&mut self, _: RenderId, _: Point) {
            unreachable!("a text run has no children")
        }
        fn enclosing_window(&self, info: LayoutInfo) -> LayoutInfo {
            info
        }
        fn scroll(&self) -> Point {
            Point::new(0, 0)
        }
        fn set_offset(&mut self, _: Point) {}
        fn set_scroll(&mut self, _: ScrollInfo) {}
        fn rebuild(&mut self, _: LayoutInfo) {}
        fn element(&self) -> crate::element::ElementId {
            unreachable!("nothing here reads the element")
        }
    }

    fn props(runs: Vec<Run>, wrap: Wrap) -> TextProps {
        TextProps {
            runs: Rc::from(runs),
            wrap,
            elide: Elide::None,
            cursor: None,
            selection: None,
        }
    }

    fn loose() -> Constraints {
        Constraints {
            min_w: 0,
            max_w: 80,
            min_h: 0,
            max_h: 20,
        }
    }

    #[test]
    fn the_unwrapped_extent_is_the_widest_line_by_display_width() {
        assert_eq!(unwrapped_extent(&[Run::plain("fn")]), (2, 1));
        assert_eq!(unwrapped_extent(&[Run::plain("你好")]), (4, 1));
        // Pieces sum; a piece boundary is not a column.
        assert_eq!(
            unwrapped_extent(&[Run::themed("你好", "kw"), Run::plain("fn")]),
            (6, 1)
        );
        // Lines end inside a piece, and across pieces the line continues.
        assert_eq!(unwrapped_extent(&[Run::plain("ab\ncdef\nx")]), (4, 3));
        assert_eq!(
            unwrapped_extent(&[Run::plain("ab"), Run::plain("cd\ne")]),
            (4, 2)
        );
        assert_eq!(unwrapped_extent(&[]), (0, 1));
        assert_eq!(unwrapped_extent(&[Run::plain("")]), (0, 1));
        // A combining mark is not a column; an emoji sequence is two.
        assert_eq!(unwrapped_extent(&[Run::plain("e\u{301}")]), (1, 1));
        assert_eq!(
            unwrapped_extent(&[Run::plain("👨\u{200d}👩\u{200d}👧")]),
            (2, 1)
        );
    }

    /// The width and height an unwrapped run reports are the extent of its
    /// pieces, and a multi-line run is as wide as its widest line — not the
    /// sum of them, which is what measuring the joined string used to say.
    #[test]
    fn an_unwrapped_run_measures_its_widest_line() {
        let mut t = TextRender::new(props(vec![Run::plain("你好\nab")], Wrap::None));
        assert_eq!(t.layout(loose(), &mut Leaf), Size::new(4, 2));
    }

    /// Equal content in a fresh `Rc` — what the editor hands a live object
    /// every frame — keeps the shaping it has instead of joining and cutting
    /// again. Changed content does not.
    #[test]
    fn an_unwrapped_measure_reshapes_only_when_the_pieces_change() {
        let first: Rc<[Run]> = Rc::from(vec![Run::themed("你好", "kw"), Run::plain(" fn")]);
        let mut t = TextRender::new(TextProps {
            runs: first.clone(),
            wrap: Wrap::None,
            elide: Elide::None,
            cursor: None,
            selection: None,
        });
        assert_eq!(t.layout(loose(), &mut Leaf), Size::new(7, 1));
        assert!(t
            .shaped_from
            .as_ref()
            .is_some_and(|f| Rc::ptr_eq(f, &first)));

        // Same pieces, new allocation.
        t.props.runs = Rc::from(vec![Run::themed("你好", "kw"), Run::plain(" fn")]);
        t.stale = true;
        assert_eq!(t.layout(loose(), &mut Leaf), Size::new(7, 1));
        assert!(
            t.shaped_from
                .as_ref()
                .is_some_and(|f| Rc::ptr_eq(f, &first)),
            "unchanged pieces were shaped again"
        );
        assert!(!t.stale);

        // A different theme on the same text is a different shaping: the
        // fragments carry the theme.
        let third: Rc<[Run]> = Rc::from(vec![Run::themed("你好", "id"), Run::plain(" fn")]);
        t.props.runs = third.clone();
        assert_eq!(t.layout(loose(), &mut Leaf), Size::new(7, 1));
        assert!(t
            .shaped_from
            .as_ref()
            .is_some_and(|f| Rc::ptr_eq(f, &third)));
        assert_eq!(t.shaped.frags[0][0].theme, third[0].theme);

        // Different text: reshaped, and the rows say so.
        t.props.runs = Rc::from(vec![Run::plain("a\nb")]);
        assert_eq!(t.layout(loose(), &mut Leaf), Size::new(1, 2));
        assert_eq!(t.shaped.rows.len(), 2);
    }

    /// A wrapped shaping depends on the width too, so it is never kept
    /// across a measure — and switching an object from wrapped to unwrapped
    /// with the same pieces must not reuse the wrapped rows.
    #[test]
    fn a_wrapped_shaping_is_not_kept_for_an_unwrapped_measure() {
        let runs: Rc<[Run]> = Rc::from(vec![Run::plain("one two three")]);
        let mut t = TextRender::new(TextProps {
            runs: runs.clone(),
            wrap: Wrap::Word,
            elide: Elide::None,
            cursor: None,
            selection: None,
        });
        let narrow = Constraints {
            min_w: 0,
            max_w: 5,
            min_h: 0,
            max_h: 20,
        };
        assert_eq!(t.layout(narrow, &mut Leaf), Size::new(5, 3));
        assert!(t.shaped_from.is_none());

        t.props.wrap = Wrap::None;
        assert_eq!(t.layout(loose(), &mut Leaf), Size::new(13, 1));
        assert_eq!(t.shaped.rows.len(), 1);
    }
}

#[cfg(test)]
mod byte_mapping_tests {
    //! The mapping between a byte of a run's logical string and the cell the
    //! wrap drew it in.
    //!
    //! These are white-box: they name the rows a given text wraps to and then
    //! read the mapping in both directions across them, because the cases that
    //! matter are the ones where a row is *not* a slice of the source — the
    //! space a break ate, the indent a hanging wrap added, the `\n` between
    //! paragraphs. The round-trip property over arbitrary text lives in
    //! `tests/properties.rs`; this is the worked example it generalises.

    use super::{byte_of, cell_of, rows_of, Row, Wrap};

    /// `(text, indent, src)` for each row, which is the whole of what the
    /// mapping reads.
    fn shape(rows: &[Row]) -> Vec<(&str, usize, std::ops::Range<usize>)> {
        rows.iter()
            .map(|r| (r.text.as_str(), r.indent, r.src.clone()))
            .collect()
    }

    #[test]
    fn a_rows_text_is_the_source_it_says_it_is() {
        let text = "hello world here";
        let rows = rows_of(text, 11, Wrap::Word);
        assert_eq!(
            shape(&rows),
            vec![("hello world", 0, 0..11), ("here", 0, 12..16)],
            "byte 11 is the space the break ate, and belongs to no row"
        );
        for r in &rows {
            assert_eq!(&r.text[r.indent..], &text[r.src.clone()]);
        }
    }

    #[test]
    fn a_hanging_indent_has_no_source_behind_it() {
        let text = "    sep  a string put between the values";
        let rows = rows_of(text, 20, Wrap::Hanging);
        assert_eq!(
            shape(&rows),
            vec![
                ("    sep  a string", 0, 0..17),
                ("    put between the", 4, 18..33),
                ("    values", 4, 34..40),
            ],
            "the line's own leading spaces are source; the continuations' are not"
        );
        for r in &rows {
            assert_eq!(&r.text[r.indent..], &text[r.src.clone()]);
        }
        // The first byte a continuation row shows is drawn at column 4,
        // because four cells of that row are indent the source never had.
        assert_eq!(cell_of(&rows, text, 18), (1, 4));
        // And every cell of that indent addresses it back.
        for col in 0..=4 {
            assert_eq!(byte_of(&rows, text, 1, col), Some(18), "column {col}");
        }
    }

    #[test]
    fn the_space_a_break_ate_is_drawn_at_the_end_of_the_row_before_it() {
        let text = "hello   world";
        let rows = rows_of(text, 5, Wrap::Word);
        assert_eq!(shape(&rows), vec![("hello", 0, 0..5), ("world", 0, 8..13)]);
        // Three dropped spaces, one cell: they collapse onto the row's end,
        // which is where a caret on any of them is drawn.
        for b in 5..=7 {
            assert_eq!(cell_of(&rows, text, b), (0, 5), "byte {b}");
        }
        // Reading that cell back gives the first of them — the round trip is
        // the identity for byte 5 and snaps 6 and 7 onto it.
        assert_eq!(byte_of(&rows, text, 0, 5), Some(5));
        assert_eq!(byte_of(&rows, text, 0, 40), Some(5), "far past the text");
        // The next row starts at the first byte the wrap kept.
        assert_eq!(cell_of(&rows, text, 8), (1, 0));
    }

    #[test]
    fn a_newline_is_the_end_of_the_row_before_it() {
        let text = "ab\n\ncd";
        let rows = rows_of(text, 8, Wrap::Word);
        assert_eq!(
            shape(&rows),
            vec![("ab", 0, 0..2), ("", 0, 3..3), ("cd", 0, 4..6)]
        );
        assert_eq!(cell_of(&rows, text, 2), (0, 2), "the first \\n");
        assert_eq!(cell_of(&rows, text, 3), (1, 0), "the second \\n");
        assert_eq!(byte_of(&rows, text, 1, 0), Some(3));
        assert_eq!(
            byte_of(&rows, text, 1, 6),
            Some(3),
            "an empty row is one cell"
        );
    }

    #[test]
    fn a_wide_glyph_is_two_cells_and_one_byte() {
        // Four cells per row: 名前 fills one exactly.
        let text = "名前 ab";
        let rows = rows_of(text, 4, Wrap::Word);
        assert_eq!(shape(&rows), vec![("名前", 0, 0..6), ("ab", 0, 7..9)]);
        assert_eq!(cell_of(&rows, text, 0), (0, 0));
        assert_eq!(
            cell_of(&rows, text, 3),
            (0, 2),
            "前 starts at the third cell"
        );
        // Both halves of a glyph name the glyph.
        assert_eq!(byte_of(&rows, text, 0, 0), Some(0));
        assert_eq!(byte_of(&rows, text, 0, 1), Some(0));
        assert_eq!(byte_of(&rows, text, 0, 2), Some(3));
        assert_eq!(byte_of(&rows, text, 0, 3), Some(3));
        assert_eq!(byte_of(&rows, text, 0, 4), Some(6), "past the row's text");
    }

    #[test]
    fn a_word_too_long_for_a_row_is_cut_on_a_source_boundary() {
        let text = "supercalifragilistic";
        let rows = rows_of(text, 6, Wrap::Word);
        assert_eq!(
            shape(&rows),
            vec![
                ("superc", 0, 0..6),
                ("alifra", 0, 6..12),
                ("gilist", 0, 12..18),
                ("ic", 0, 18..20),
            ],
            "nothing is dropped, so the rows tile the source"
        );
        // Byte 6 is both the end of row 0 and the start of row 1. One caret,
        // two cells; the mapping names the leading edge, and reading it back
        // gives the byte again.
        assert_eq!(cell_of(&rows, text, 6), (1, 0));
        assert_eq!(byte_of(&rows, text, 1, 0), Some(6));
        assert_eq!(byte_of(&rows, text, 0, 6), Some(6));
        assert_eq!(cell_of(&rows, text, 20), (3, 2), "the trailing edge");
        assert_eq!(byte_of(&rows, text, 3, 2), Some(20));
    }

    #[test]
    fn there_is_no_row_below_the_last_one() {
        let rows = rows_of("ab", 8, Wrap::Word);
        assert_eq!(byte_of(&rows, "ab", 1, 0), None);
        assert_eq!(cell_of(&[], "", 0), (0, 0), "and none above the first");
    }
}
