//! The primitives' render objects.
//!
//! Each one is an ordinary `RenderObject`. Nothing here can do anything a host
//! leaf cannot: they measure their children through `LayoutCx`, position them,
//! emit display-list items and answer hit queries. The framework supplies the
//! tree, the cache and the dirty bits; the algorithms live here.

use std::rc::Rc;

use crate::desc::{Align, BoxProps, Dir, LayerProps, Sizing, TextProps, ViewportProps};
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

/// Greedy word wrap, breaking a word too long for a line of its own.
///
/// Measure and paint call this same function, so the height layout reserves is
/// exactly the number of rows paint emits.
pub fn wrap_text(text: &str, width: u16) -> Vec<String> {
    use unicode_width::UnicodeWidthStr;
    if width == 0 {
        return Vec::new();
    }
    let width = width as usize;
    let mut out: Vec<String> = Vec::new();
    for para in text.split('\n') {
        let mut line = String::new();
        for word in para.split(' ') {
            if line.is_empty() {
                line.push_str(word);
            } else if UnicodeWidthStr::width(line.as_str()) + 1 + UnicodeWidthStr::width(word)
                <= width
            {
                line.push(' ');
                line.push_str(word);
            } else {
                out.push(std::mem::take(&mut line));
                line.push_str(word);
            }
            while UnicodeWidthStr::width(line.as_str()) > width {
                let head: String = line.chars().take(width).collect();
                let tail: String = line.chars().skip(width).collect();
                out.push(head);
                line = tail;
            }
        }
        out.push(line);
    }
    if out.is_empty() {
        out.push(String::new());
    }
    out
}

/// One painted fragment: a piece of a row, and the theme it paints in.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Frag {
    pub text: Rc<str>,
    pub theme: Option<crate::render::spec::ThemeKey>,
}

/// Wrap a run's pieces as one logical string, reporting each output row as the
/// fragments that compose it.
///
/// The pieces are concatenated, wrapped by the same [`wrap_text`] the unstyled
/// path uses — so styled and unstyled text break identically — and the result
/// is then cut back into fragments along the original piece boundaries. A wrap
/// point inside a piece splits it; a row crossing three pieces is three
/// fragments.
pub fn wrap_runs(runs: &[crate::desc::Run], width: u16, wrap: bool) -> Vec<Vec<Frag>> {
    use unicode_width::UnicodeWidthStr;

    let whole: String = runs.iter().map(|r| &*r.text).collect();
    let rows: Vec<String> = if wrap {
        wrap_text(&whole, width)
    } else {
        whole.split('\n').map(|s| s.to_string()).collect()
    };

    // Walk the pieces alongside the rows. Wrapping can drop a separator at a
    // break, so rows are matched by content length rather than by byte offset
    // into the original.
    let mut piece = 0usize;
    let mut used = 0usize; // chars of the current piece already emitted
    let mut out: Vec<Vec<Frag>> = Vec::with_capacity(rows.len());

    for row in &rows {
        let mut frags: Vec<Frag> = Vec::new();
        let mut want: usize = row.chars().count();
        let mut at = 0usize; // chars of this row already taken

        while want > 0 && piece < runs.len() {
            let piece_chars: Vec<char> = runs[piece].text.chars().collect();
            let left = piece_chars.len().saturating_sub(used);
            if left == 0 {
                piece += 1;
                used = 0;
                continue;
            }
            let take = left.min(want);
            // Take the text from the *row*, not the piece: the row is what
            // wrapping produced, and it is what must appear on screen.
            let seg: String = row.chars().skip(at).take(take).collect();
            if !seg.is_empty() {
                frags.push(Frag {
                    text: Rc::from(seg.as_str()),
                    theme: runs[piece].theme.clone(),
                });
            }
            at += take;
            want -= take;
            used += take;
            if used >= piece_chars.len() {
                piece += 1;
                used = 0;
            }
        }

        // A separator consumed by the wrap: step past it so the next row starts
        // at the right place in the piece list.
        if want == 0 && piece < runs.len() {
            let piece_chars = runs[piece].text.chars().count();
            if used < piece_chars {
                let next: Option<char> = runs[piece].text.chars().nth(used);
                if next == Some(' ') {
                    used += 1;
                    if used >= piece_chars {
                        piece += 1;
                        used = 0;
                    }
                }
            }
        }

        if frags.is_empty() {
            frags.push(Frag {
                text: Rc::from(""),
                theme: None,
            });
        }
        out.push(frags);
    }

    let _ = UnicodeWidthStr::width("");
    if out.is_empty() {
        out.push(vec![Frag {
            text: Rc::from(""),
            theme: None,
        }]);
    }
    out
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
        let wc = range(sw, c.max_w, w_definite, align);
        let hc = range(sh, c.max_h, h_definite, align);
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
        let border = u16::from(self.props.border);
        (
            self.props.pad.x.saturating_add(border),
            self.props.pad.y.saturating_add(border),
        )
    }

    fn layout(&mut self, c: Constraints, cx: &mut dyn LayoutCx) -> Size {
        let p = &self.props;
        let border = u16::from(p.border);
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
            let share = shares[i].max(floors[i]);
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
        if self.props.border {
            out.push(Draw::Border, g);
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
    use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};
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
    let take = |f: &Frag, left: usize, from_end: bool| -> (String, usize) {
        let mut chars: Vec<char> = f.text.chars().collect();
        if from_end {
            chars.reverse();
        }
        let (mut used, mut out) = (0usize, String::new());
        for c in chars {
            let cw = UnicodeWidthChar::width(c).unwrap_or(0);
            if used + cw > left {
                break;
            }
            used += cw;
            out.push(c);
        }
        if from_end {
            out = out.chars().rev().collect();
        }
        (out, used)
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
    /// two cannot disagree. Each row is its fragments, in order.
    rows: Vec<Vec<Frag>>,
}

impl TextRender {
    pub fn new(props: TextProps) -> Self {
        TextRender {
            props,
            rows: Vec::new(),
        }
    }
}

impl RenderObject for TextRender {
    fn layout(&mut self, c: Constraints, _cx: &mut dyn LayoutCx) -> Size {
        use unicode_width::UnicodeWidthStr;
        // Measured as one string, whatever it is made of: the pieces are one
        // logical run, so styling never changes where the text breaks.
        let whole = self.props.plain();
        let natural = UnicodeWidthStr::width(whole.as_str()) as u16;
        if self.props.wrap {
            let w = if c.min_w > 0 {
                c.max_w
            } else {
                c.max_w.min(natural.max(1))
            };
            self.rows = wrap_runs(&self.props.runs, w, true);
            c.constrain(Size::new(w, self.rows.len().min(u16::MAX as usize) as u16))
        } else {
            self.rows = wrap_runs(&self.props.runs, 0, false);
            c.constrain(Size::new(natural, self.rows.len().max(1) as u16))
        }
    }

    fn paint(&self, g: Geom, out: &mut DrawList) {
        // What was actually given, cut to fit. Wrapped text has no overflow to
        // mark, and `Elide::None` is the whole of today's behaviour, so both
        // pass straight through.
        let elided: Vec<Vec<Frag>>;
        let rows: &[Vec<Frag>] = if self.props.wrap || self.props.elide == crate::desc::Elide::None
        {
            &self.rows
        } else {
            elided = self
                .rows
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
            use unicode_width::UnicodeWidthStr;
            for (i, row) in rows.iter().enumerate() {
                let mut x = g.rect.x;
                for frag in row {
                    let w = UnicodeWidthStr::width(&*frag.text) as u16;
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
        if let Some(col) = self.props.cursor {
            out.set_cursor(Point::new(g.rect.x + col as i32, g.rect.y));
        }
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
}

impl ViewportRender {
    pub fn new(props: ViewportProps) -> Self {
        ViewportRender {
            props,
            window: Rect::ZERO,
            content: Size::ZERO,
            items: 0,
            placed: false,
        }
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
                // the narrower area so it is not painted over the bar.
                let gutter = u16::from(self.props.scrollbar && content.h > own.h);
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
                });
            }
            ScrollMode::Items(n) => {
                // The child renders only the window, so nothing is translated
                // and the offset is an index. A cell extent over a million rows
                // would not fit a coordinate; an index does.
                //
                // A loose width is resolved the intrinsic way: measure the
                // window once to learn how wide its rows are, then again at
                // that width. The framework counts the second look.
                if c.min_w != c.max_w {
                    let probe = Constraints::new(0, c.max_w, 0, own.h);
                    let mut natural = 0u16;
                    for k in cx.children() {
                        natural = natural.max(cx.measure(k, probe).w);
                    }
                    own.w = c.constrain(Size::new(natural, own.h)).w;
                }
                own = c.constrain(Size::new(own.w, (n.min(u16::MAX as u32)) as u16));
                let rows = own.h as u32;
                self.items = n;
                // When the content overflows and a scrollbar is asked for, the
                // last column is a gutter the scrollbar owns: content laid out
                // over it would paint the bar away, since a node's own paint is
                // under its children.
                let gutter = u16::from(self.props.scrollbar && n > rows);
                let inner_w = own.w.saturating_sub(gutter);
                self.window = Rect::new(0, scroll.y, inner_w, own.h);
                cx.set_scroll(ScrollInfo {
                    window: self.window,
                    content: Size::new(inner_w, own.h),
                    max: Point::new(0, n.saturating_sub(rows) as i32),
                    translate: false,
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
        use crate::desc::ScrollMode;
        if self.props.selectable {
            out.push(Draw::Selectable, g);
        }
        if !self.props.scrollbar {
            return;
        }
        let (offset, content) = match self.props.mode {
            ScrollMode::Cells => (self.window.y.max(0) as u32, self.content.h as u32),
            ScrollMode::Items(n) => (self.window.y.max(0) as u32, n),
        };
        if content <= g.rect.h as u32 {
            return;
        }
        out.push_at(
            Draw::Scrollbar {
                offset,
                content,
                window: g.rect.h,
            },
            Rect::new(g.rect.right() - 1, g.rect.y, 1, g.rect.h),
            g.clip,
        );
    }

    fn relayout_boundary(&self) -> bool {
        true
    }

    fn clips(&self) -> bool {
        true
    }

    fn shows_scrollbar(&self) -> bool {
        self.props.scrollbar
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
        Some(self.reg)
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
                stretch: p.stretch,
                modality: p.modality,
                scrim: p.scrim,
                dismiss: p.dismiss,
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
    fn focus_reg(&self) -> Option<FocusReg> {
        (self.geom.modality != crate::desc::Modality::None).then_some(FocusReg {
            ordinal: None,
            skip: true,
            scope: true,
            focus_within: false,
            autofocus: false,
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
