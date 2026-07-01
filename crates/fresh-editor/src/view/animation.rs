//! Frame-buffer animation layer.
//!
//! Area-based post-processing effects applied after the main render pass.
//! The `FrameEffect` trait is the seam: concrete implementations mutate a
//! `(Buffer, Rect)` region given elapsed time. `AnimationRunner` drives
//! active effects from the render clock. The layer knows nothing about
//! virtual buffers; callers resolve areas and pass them in.
//!
//! Current effects: `SlideIn`, `CursorJump`, `ColorTransition`, `Wave`.
//! Easing is an implementation detail. `Wave` is the odd one out — a
//! stateful particle simulation that takes over the whole frame, rather
//! than an area transition.

use ratatui::buffer::{Buffer, Cell};
use ratatui::layout::Rect;
use ratatui::style::Color;
use std::time::{Duration, Instant};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EffectStatus {
    Running,
    Done,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Edge {
    Top,
    Bottom,
    Left,
    Right,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AnimationKind {
    SlideIn {
        from: Edge,
        duration: Duration,
        delay: Duration,
    },
    /// Animate the cursor moving from one screen cell to another. Paints a
    /// short trail of cells along the line from `from` to `to`: the head
    /// cell uses `cursor_color` as background; trailing cells fade toward
    /// `bg_color` (older positions are closer to bg). `from`/`to` are
    /// absolute screen coordinates (col, row).
    CursorJump {
        from: (u16, u16),
        to: (u16, u16),
        duration: Duration,
        cursor_color: Color,
        bg_color: Color,
    },
    /// Crossfade every cell's fg/bg color from what was on screen last
    /// frame to the freshly painted colors. Glyphs are untouched — only
    /// colors interpolate — so a theme switch melts into the new palette
    /// instead of flipping. Cells whose colors can't be resolved to RGB
    /// (`Reset` / indexed) switch instantly.
    ColorTransition { duration: Duration },
    /// Playful full-screen effect: a crest of wave glyphs rises from the
    /// bottom edge and, as it sweeps past each row, kicks every painted
    /// cell ("ink" particle) on that row upward and sideways. Each
    /// particle is then pulled back to its home cell by a damped spring,
    /// so the whole UI — text, gutter, menu bar, status bar — bounces up,
    /// down, and sideways before settling exactly back into place once the
    /// wave exits the top. `duration` is a hard safety cap; the effect
    /// normally ends earlier, the moment the crest is gone and every
    /// particle has settled.
    Wave { duration: Duration },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AnimationId(u64);

impl AnimationId {
    pub fn raw(self) -> u64 {
        self.0
    }
    pub fn from_raw(v: u64) -> Self {
        Self(v)
    }
}

pub trait FrameEffect {
    /// Optionally capture the pre-paint ("before") state of `area` from
    /// the buffer at the start of a render pass. Called by the runner
    /// once per render before the main paint walk, so effects like
    /// `SlideIn` can snapshot the outgoing content and push it out
    /// while new content slides in. Default: no-op.
    fn capture_before(&mut self, _buf: &Buffer, _area: Rect) {}

    fn apply(&mut self, buf: &mut Buffer, area: Rect, elapsed: Duration) -> EffectStatus;
}

/// True iff `outer` fully contains `inner` (all corners inside).
fn rect_contains(outer: Rect, inner: Rect) -> bool {
    inner.x >= outer.x
        && inner.y >= outer.y
        && inner.x.saturating_add(inner.width) <= outer.x.saturating_add(outer.width)
        && inner.y.saturating_add(inner.height) <= outer.y.saturating_add(outer.height)
}

/// Ease-out cubic: starts fast, decelerates.
fn ease_out_cubic(t: f32) -> f32 {
    let t = t.clamp(0.0, 1.0);
    let inv = 1.0 - t;
    1.0 - inv * inv * inv
}

/// Slide-in effect. Paints the incoming ("after") content sliding in from
/// `from`. When the runner captures a "before" snapshot at the start of
/// the render pass, the outgoing content is pushed out the opposite
/// direction in lock-step — giving a "push" transition that replaces
/// old content with new. Without a before snapshot (initial bringup,
/// buffer didn't exist yet), the vacated cells are blank.
pub struct SlideIn {
    from: Edge,
    duration: Duration,
    after: Option<SlideSnapshot>,
    before: Option<SlideSnapshot>,
}

struct SlideSnapshot {
    area: Rect,
    cells: Vec<Cell>,
}

impl SlideIn {
    pub fn new(from: Edge, duration: Duration) -> Self {
        Self {
            from,
            duration,
            after: None,
            before: None,
        }
    }

    fn snapshot_area(buf: &Buffer, area: Rect) -> SlideSnapshot {
        let mut cells = Vec::with_capacity(area.width as usize * area.height as usize);
        for dy in 0..area.height {
            for dx in 0..area.width {
                let x = area.x + dx;
                let y = area.y + dy;
                let cell = buf.cell((x, y)).cloned().unwrap_or_default();
                cells.push(cell);
            }
        }
        SlideSnapshot { area, cells }
    }
}

impl FrameEffect for SlideIn {
    fn capture_before(&mut self, buf: &Buffer, area: Rect) {
        if self.before.is_none() {
            self.before = Some(Self::snapshot_area(buf, area));
        }
    }

    fn apply(&mut self, buf: &mut Buffer, area: Rect, elapsed: Duration) -> EffectStatus {
        // First apply captures the post-paint "after" snapshot. The
        // "before" snapshot, if any, was captured at the top of this
        // render pass via the trait hook.
        if self.after.is_none() {
            self.after = Some(Self::snapshot_area(buf, area));
        }
        let after = match &self.after {
            Some(s) if s.area == area => s,
            Some(_) => {
                // Area changed mid-animation (resize) — re-snapshot the
                // after, and drop the before whose dimensions no longer
                // match. Falls back to the slide-in-with-blanks path.
                self.after = Some(Self::snapshot_area(buf, area));
                self.before = None;
                self.after.as_ref().unwrap()
            }
            None => unreachable!(),
        };
        let before = self.before.as_ref().filter(|b| b.area == area);

        let t = if self.duration.is_zero() {
            1.0
        } else {
            (elapsed.as_secs_f32() / self.duration.as_secs_f32()).clamp(0.0, 1.0)
        };
        let eased = ease_out_cubic(t);

        // offset_row/col: how far the AFTER snapshot is shifted toward
        // `from` at t. At t=0 it sits fully off the `from` edge; at
        // t=1 it's at its natural position. BEFORE moves the same
        // distance in the opposite direction (the "push out").
        let (offset_row, offset_col) = match self.from {
            Edge::Bottom => (((1.0 - eased) * area.height as f32).round() as i32, 0i32),
            Edge::Top => (-(((1.0 - eased) * area.height as f32).round() as i32), 0),
            Edge::Right => (0, ((1.0 - eased) * area.width as f32).round() as i32),
            Edge::Left => (0, -(((1.0 - eased) * area.width as f32).round() as i32)),
        };

        // Before is pushed opposite to After: if After enters from
        // below (offset_row > 0), Before exits upward (offset_row -
        // height in the Bottom case). Same for horizontal edges.
        let (before_offset_row, before_offset_col) = match self.from {
            Edge::Bottom => (offset_row - area.height as i32, 0),
            Edge::Top => (offset_row + area.height as i32, 0),
            Edge::Right => (0, offset_col - area.width as i32),
            Edge::Left => (0, offset_col + area.width as i32),
        };

        let blank = Cell::default();
        for dy in 0..area.height {
            for dx in 0..area.width {
                let x = area.x + dx;
                let y = area.y + dy;

                // Try the incoming snapshot first (post-slide it's what
                // everyone sees); fall back to the outgoing one, then
                // to blank if neither slice covers this cell.
                let after_src_dy = dy as i32 - offset_row;
                let after_src_dx = dx as i32 - offset_col;
                let after_cell = if after_src_dy >= 0
                    && after_src_dy < area.height as i32
                    && after_src_dx >= 0
                    && after_src_dx < area.width as i32
                {
                    let idx = after_src_dy as usize * area.width as usize + after_src_dx as usize;
                    Some(after.cells[idx].clone())
                } else {
                    None
                };

                let before_cell = if let Some(before) = before {
                    let before_src_dy = dy as i32 - before_offset_row;
                    let before_src_dx = dx as i32 - before_offset_col;
                    if before_src_dy >= 0
                        && before_src_dy < area.height as i32
                        && before_src_dx >= 0
                        && before_src_dx < area.width as i32
                    {
                        let idx =
                            before_src_dy as usize * area.width as usize + before_src_dx as usize;
                        Some(before.cells[idx].clone())
                    } else {
                        None
                    }
                } else {
                    None
                };

                let new_cell = after_cell.or(before_cell).unwrap_or_else(|| blank.clone());
                if let Some(dst) = buf.cell_mut((x, y)) {
                    *dst = new_cell;
                }
            }
        }

        if t >= 1.0 {
            EffectStatus::Done
        } else {
            EffectStatus::Running
        }
    }
}

/// Cursor-jump effect. Paints a moving "head" cell along the straight line
/// from `from` to `to` with a short fading trail. Both endpoints are in
/// absolute screen coordinates (col, row). The head cell's background is
/// set to `cursor_color`; trailing cells blend toward `bg_color` so that
/// older positions appear progressively dimmer. The effect operates
/// outside the `area` snapshot model used by `SlideIn`: it directly
/// mutates cells along the interpolated path and never reads/snapshots an
/// area, so the `area` passed to the runner is only used for dedupe and
/// replacement bookkeeping.
pub struct CursorJump {
    from: (i32, i32),
    to: (i32, i32),
    duration: Duration,
    cursor_rgb: (u8, u8, u8),
    bg_rgb: (u8, u8, u8),
}

impl CursorJump {
    pub fn new(
        from: (u16, u16),
        to: (u16, u16),
        duration: Duration,
        cursor_color: Color,
        bg_color: Color,
    ) -> Self {
        // Themes occasionally use Color::Reset / named colors for which we
        // have no RGB; fall back to white/black so the effect still
        // visibly fades rather than silently no-oping.
        let cursor_rgb = color_to_rgb(cursor_color).unwrap_or((255, 255, 255));
        let bg_rgb = color_to_rgb(bg_color).unwrap_or((0, 0, 0));
        Self {
            from: (from.0 as i32, from.1 as i32),
            to: (to.0 as i32, to.1 as i32),
            duration,
            cursor_rgb,
            bg_rgb,
        }
    }

    fn paint_cell(buf: &mut Buffer, col: i32, row: i32, bg: Color) {
        if col < 0 || row < 0 {
            return;
        }
        let buf_area = buf.area;
        let c = col as u16;
        let r = row as u16;
        if c < buf_area.x
            || c >= buf_area.x.saturating_add(buf_area.width)
            || r < buf_area.y
            || r >= buf_area.y.saturating_add(buf_area.height)
        {
            return;
        }
        if let Some(cell) = buf.cell_mut((c, r)) {
            cell.set_bg(bg);
        }
    }
}

impl FrameEffect for CursorJump {
    fn apply(&mut self, buf: &mut Buffer, _area: Rect, elapsed: Duration) -> EffectStatus {
        let t = if self.duration.is_zero() {
            1.0
        } else {
            (elapsed.as_secs_f32() / self.duration.as_secs_f32()).clamp(0.0, 1.0)
        };

        // Final frame: paint nothing and report Done. We MUST leave the
        // buffer clean here — the runner removes the effect after this
        // call and the main loop stops scheduling renders (is_active is
        // now false), so any trail cells painted now would persist on
        // screen until the user does something else. The hardware cursor
        // at the target is drawn by the editor's own pass, so the user
        // still sees the cursor at its final spot.
        if t >= 1.0 {
            return EffectStatus::Done;
        }

        let eased = ease_out_cubic(t);

        let (fx, fy) = (self.from.0 as f32, self.from.1 as f32);
        let (tx, ty) = (self.to.0 as f32, self.to.1 as f32);
        let dx = tx - fx;
        let dy = ty - fy;

        // Trail length scales with the path so short jumps don't get an
        // oversized tail. Min 2 keeps a hint of motion even on tiny jumps.
        let path_cells = dx.abs().max(dy.abs()).round() as i32;
        let trail_len = path_cells.clamp(2, 8) as usize;

        for i in 0..trail_len {
            // Trail samples behind the head: i=0 is the head (alpha=1, full
            // cursor color), larger i is further back along the path with
            // alpha decreasing toward 0 (full bg color).
            let back = (i as f32) / (trail_len as f32);
            let sample = (eased - back * 0.12).max(0.0);
            let col = (fx + dx * sample).round() as i32;
            let row = (fy + dy * sample).round() as i32;
            let alpha = 1.0 - back;
            let blended = blend_rgb(self.cursor_rgb, self.bg_rgb, alpha);
            Self::paint_cell(buf, col, row, blended);
        }

        EffectStatus::Running
    }
}

/// Color-transition effect. Snapshots the previous frame (the colors the
/// user was looking at before the switch) and, on every render while
/// running, re-tints the freshly painted buffer: each cell's fg/bg is the
/// blend of its old color and its new color at the eased progress. The
/// paint pass keeps drawing pure new-theme colors underneath, so content
/// changes mid-transition (typing, cursor) stay live; only the tint lags.
pub struct ColorTransition {
    duration: Duration,
    before: Option<SlideSnapshot>,
}

impl ColorTransition {
    pub fn new(duration: Duration) -> Self {
        Self {
            duration,
            before: None,
        }
    }
}

impl FrameEffect for ColorTransition {
    fn capture_before(&mut self, buf: &Buffer, area: Rect) {
        if self.before.is_none() {
            self.before = Some(SlideIn::snapshot_area(buf, area));
        }
    }

    fn apply(&mut self, buf: &mut Buffer, area: Rect, elapsed: Duration) -> EffectStatus {
        let t = if self.duration.is_zero() {
            1.0
        } else {
            (elapsed.as_secs_f32() / self.duration.as_secs_f32()).clamp(0.0, 1.0)
        };
        // Final frame: the buffer already holds pure new-theme colors, so
        // leave it untouched and report Done (same contract as CursorJump —
        // no further redraw is scheduled after the runner drops us).
        if t >= 1.0 {
            return EffectStatus::Done;
        }
        // Nothing to fade from: no frame was rendered before the switch,
        // or a resize invalidated the snapshot. Snap to the new colors.
        let Some(before) = self.before.as_ref().filter(|b| b.area == area) else {
            return EffectStatus::Done;
        };

        let eased = ease_out_cubic(t);
        for dy in 0..area.height {
            for dx in 0..area.width {
                let idx = dy as usize * area.width as usize + dx as usize;
                let old = &before.cells[idx];
                let Some(cell) = buf.cell_mut((area.x + dx, area.y + dy)) else {
                    continue;
                };
                if let (Some(new_rgb), Some(old_rgb)) =
                    (color_to_rgb(cell.fg), color_to_rgb(old.fg))
                {
                    if new_rgb != old_rgb {
                        cell.set_fg(blend_rgb(new_rgb, old_rgb, eased));
                    }
                }
                if let (Some(new_rgb), Some(old_rgb)) =
                    (color_to_rgb(cell.bg), color_to_rgb(old.bg))
                {
                    if new_rgb != old_rgb {
                        cell.set_bg(blend_rgb(new_rgb, old_rgb, eased));
                    }
                }
            }
        }

        EffectStatus::Running
    }
}

/// Lifecycle of a snapshotted cell.
#[derive(Clone, Copy, PartialEq, Eq)]
enum PState {
    /// Sitting at its home cell above the waterline, untouched so far.
    Resting,
    /// Launched: a ballistic projectile under gravity (its whole word flies
    /// off together with a shared launch velocity).
    Flying,
    /// Fell back into the water and is now drifting slowly down through it.
    Sinking,
}

/// One painted cell turned into a physics particle. `home_x` is its original
/// area-local column, kept for the surface/sway calculations; `pos`/`vel`
/// evolve once its word is struck by the wave. `cell` carries the full visual
/// (glyph, fg, bg, modifier) so chrome colors fly along with the text. `word`
/// indexes the run of characters it belongs to — the unit that launches
/// together.
struct WaveParticle {
    home_x: f32,
    x: f32,
    y: f32,
    vx: f32,
    vy: f32,
    cell: Cell,
    word: usize,
    state: PState,
}

/// A contiguous run of "ink" cells on one row — a word (or a chunk of a
/// chrome band). All its characters launch together with one shared
/// velocity, so the word flies off as a rigid cluster.
struct Word {
    members: Vec<usize>,
    home_y: f32,
    center_x: f32,
    launched: bool,
}

/// Wave effect — see `AnimationKind::Wave`. A body of water, anchored to
/// the bottom edge, rises to about half the view's height and then just
/// undulates there like a real pond. Its surface is the superposition of
/// several sine waves of different wavelengths, with more undulating "wave
/// layers" rippling within the body, all heaving up and down slowly (well
/// under 2 Hz); the swell amplitude builds up slowly. Whenever the
/// undulating surface reaches a rendered word, that word is flung off as a
/// ballistic projectile (gravity arc); when a flying character falls back
/// onto the water it switches to drifting slowly down through it.
pub struct WaveEffect {
    duration: Duration,
    area: Rect,
    particles: Vec<WaveParticle>,
    words: Vec<Word>,
    /// Blank fill cell (background) painted into every cell before the
    /// particles are stamped, so vacated space reads as empty editor bg.
    fill: Cell,
    last_elapsed: Option<Duration>,
    initialized: bool,
}

impl WaveEffect {
    // Ballistic launch + gravity (cells / sec, cells / sec²).
    const GRAVITY: f32 = 24.0;
    const LAUNCH_UP_MIN: f32 = 12.0;
    const LAUNCH_UP_VAR: f32 = 10.0;
    const LAUNCH_SIDE: f32 = 7.0;
    // Slow downward drift once a character is back in the water.
    const SINK_SPEED: f32 = 2.2;
    // Longest contiguous run treated as one word; longer chrome bands split
    // into chunks so the whole status bar doesn't fly as one slab.
    const WORD_CAP: usize = 14;
    // Water tops out at this fraction of the view height, then undulates.
    const MAX_LEVEL_FRAC: f32 = 0.5;
    // Seconds for the water to climb to its level; then it just undulates.
    const RISE_SECS: f32 = 1.6;
    // Swell amplitude builds forever: base + growth·seconds (capped). As it
    // grows the crests reach higher and higher, washing over — and flinging
    // off — words further and further up the view.
    const AMP_BASE: f32 = 0.4;
    const AMP_GROWTH: f32 = 0.6; // per second
    const AMP_MAX: f32 = 12.0;

    // Surface = sum of three sine components with distinct wavelengths
    // (spatial wavenumber k = 2π / wavelength_in_cols) and distinct slow
    // temporal frequencies (angular w = 2π·f; every f ≤ 0.5 Hz, far under
    // the 2 Hz ceiling). A_i are the relative vertical amplitudes (rows).
    const K1: f32 = 0.157; // ~40-col swell
    const K2: f32 = 0.370; // ~17-col chop
    const K3: f32 = 0.785; // ~8-col ripple
    const A1: f32 = 1.0;
    const A2: f32 = 0.55;
    const A3: f32 = 0.28;
    const W1: f32 = 1.95; // 0.31 Hz
    const W2: f32 = 2.95; // 0.47 Hz
    const W3: f32 = 1.19; // 0.19 Hz
                          // Whole-surface vertical heave.
    const W_SWING: f32 = 1.70; // 0.27 Hz
    const SWING_A: f32 = 1.6;
    // Extra undulating layers drawn inside the body, each on its own phase.
    const LAYERS: usize = 3;
    const LAYER_SPACING: f32 = 2.3;

    pub fn new(duration: Duration) -> Self {
        Self {
            duration,
            area: Rect::new(0, 0, 0, 0),
            particles: Vec::new(),
            words: Vec::new(),
            fill: Cell::default(),
            last_elapsed: None,
            initialized: false,
        }
    }

    /// Snapshot the painted buffer into particles grouped into words, and
    /// record the fill bg.
    fn init(&mut self, buf: &Buffer, area: Rect) {
        self.area = area;
        // Dominant background = the fill color for vacated cells. Counting
        // exact `Color` values is enough: the editor bg dominates the
        // frame, so it wins the tally.
        let mut bg_counts: std::collections::HashMap<Color, u32> = std::collections::HashMap::new();
        for dy in 0..area.height {
            for dx in 0..area.width {
                if let Some(c) = buf.cell((area.x + dx, area.y + dy)) {
                    *bg_counts.entry(c.bg).or_insert(0) += 1;
                }
            }
        }
        let fill_bg = bg_counts
            .into_iter()
            .max_by_key(|&(_, n)| n)
            .map(|(c, _)| c)
            .unwrap_or(Color::Reset);
        let mut fill = Cell::default();
        fill.set_symbol(" ");
        fill.set_bg(fill_bg);
        fill.set_fg(fill_bg);
        self.fill = fill;

        // A cell is "ink" (a flying particle) if it draws anything: a
        // non-space glyph, or a background that differs from the dominant
        // fill (so colored chrome bands lift off too). Walk each row left to
        // right, accumulating contiguous ink cells into a word; a gap (or
        // the WORD_CAP) closes the current word.
        self.particles.clear();
        self.words.clear();
        for dy in 0..area.height {
            let mut run: Vec<usize> = Vec::new();
            let close_run = |run: &mut Vec<usize>,
                             particles: &mut Vec<WaveParticle>,
                             words: &mut Vec<Word>| {
                if run.is_empty() {
                    return;
                }
                let wid = words.len();
                let cx = run.iter().map(|&i| particles[i].home_x).sum::<f32>() / run.len() as f32;
                for &i in run.iter() {
                    particles[i].word = wid;
                }
                words.push(Word {
                    members: std::mem::take(run),
                    home_y: dy as f32,
                    center_x: cx,
                    launched: false,
                });
            };
            for dx in 0..area.width {
                let Some(cell) = buf.cell((area.x + dx, area.y + dy)) else {
                    continue;
                };
                let is_ink = cell.symbol() != " " || cell.bg != fill_bg;
                if !is_ink {
                    close_run(&mut run, &mut self.particles, &mut self.words);
                    continue;
                }
                self.particles.push(WaveParticle {
                    home_x: dx as f32,
                    x: dx as f32,
                    y: dy as f32,
                    vx: 0.0,
                    vy: 0.0,
                    cell: cell.clone(),
                    word: 0,
                    state: PState::Resting,
                });
                run.push(self.particles.len() - 1);
                if run.len() >= Self::WORD_CAP {
                    close_run(&mut run, &mut self.particles, &mut self.words);
                }
            }
            close_run(&mut run, &mut self.particles, &mut self.words);
        }
        self.initialized = true;
    }

    /// Water height (rows above the bottom edge) and swell-amplitude at
    /// `elapsed`. The level climbs to ~half the view over `RISE_SECS` and
    /// then just holds, undulating. The amplitude keeps growing with time
    /// (capped), so the swells build and reach ever-higher words. Driven by
    /// absolute wall-clock so it's independent of the (long) safety
    /// duration; the show really ends when the user dismisses it.
    fn level_amp(&self, elapsed: Duration) -> (f32, f32) {
        let secs = elapsed.as_secs_f32();
        let frac = smoothstep((secs / Self::RISE_SECS).min(1.0));
        let level = frac * Self::MAX_LEVEL_FRAC * self.area.height as f32;
        let amp = (Self::AMP_BASE + Self::AMP_GROWTH * secs).min(Self::AMP_MAX);
        (level, amp)
    }

    /// Vertical displacement of the surface at column `x`, time `t`, for a
    /// layer whose phase is offset by `lp`. Sum of three wavelengths.
    fn undulation(x: f32, t: f32, lp: f32, amp: f32) -> f32 {
        amp * (Self::A1 * (Self::K1 * x + Self::W1 * t + lp).sin()
            + Self::A2 * (Self::K2 * x - Self::W2 * t + lp * 1.7).sin()
            + Self::A3 * (Self::K3 * x + Self::W3 * t + lp * 0.5).sin())
    }

    /// Whole-surface heave (slow vertical swing of the mean level).
    fn swing(t: f32, amp: f32) -> f32 {
        amp * Self::SWING_A * (Self::W_SWING * t).sin()
    }

    /// Surface row at column `x` (smaller = higher up the screen).
    fn surface_at(x: f32, t: f32, water_top_mean: f32, amp: f32) -> f32 {
        water_top_mean - Self::undulation(x, t, 0.0, amp) - Self::swing(t, amp)
    }

    /// Advance physics by `dt` seconds at time `t`.
    fn step(&mut self, dt: f32, t: f32, water_top_mean: f32, amp: f32) {
        // Launch any word whose row the wave crest has *visibly* reached.
        // A word only flies when the highest point of the surface over its
        // own columns rises to its row (the painted waterline covers the
        // word's cell) — not merely when the mean level is near. The whole
        // word gets one shared velocity so it flies off as a unit.
        let mut to_launch: Vec<(usize, f32, f32)> = Vec::new();
        for wi in 0..self.words.len() {
            if self.words[wi].launched {
                continue;
            }
            let w = &self.words[wi];
            let mut crest = f32::INFINITY;
            for &pi in &w.members {
                let s = Self::surface_at(self.particles[pi].home_x, t, water_top_mean, amp);
                if s < crest {
                    crest = s;
                }
            }
            // `crest <= row + 0.5` matches the paint rule that turns a cell
            // to water (y + 0.5 >= surf), so launch == visibly submerged.
            if crest <= w.home_y + 0.5 {
                let r = hash01(w.center_x, w.home_y);
                let r2 = hash01(w.home_y, w.center_x);
                let vy0 = -(Self::LAUNCH_UP_MIN + r * Self::LAUNCH_UP_VAR);
                let vx0 = (r2 - 0.5) * 2.0 * Self::LAUNCH_SIDE;
                to_launch.push((wi, vx0, vy0));
            }
        }
        for (wi, vx0, vy0) in to_launch {
            self.words[wi].launched = true;
            for k in 0..self.words[wi].members.len() {
                let pi = self.words[wi].members[k];
                let p = &mut self.particles[pi];
                p.state = PState::Flying;
                p.vx = vx0;
                p.vy = vy0;
            }
        }

        let bottom = self.area.height as f32 - 1.0;
        for p in self.particles.iter_mut() {
            match p.state {
                PState::Resting => {}
                PState::Flying => {
                    p.vy += Self::GRAVITY * dt;
                    p.x += p.vx * dt;
                    p.y += p.vy * dt;
                    // Fell back onto the water? Start sinking.
                    let surf = Self::surface_at(p.x, t, water_top_mean, amp);
                    if p.vy > 0.0 && p.y >= surf {
                        p.state = PState::Sinking;
                        p.y = surf;
                        p.vy = Self::SINK_SPEED;
                        p.vx *= 0.3;
                    }
                }
                PState::Sinking => {
                    // Slow descent with a gentle sideways sway, until it
                    // settles on the seabed.
                    p.vx *= (1.0 - 2.0 * dt).max(0.0);
                    p.x += p.vx * dt + 0.6 * (t * 1.3 + p.home_x).sin() * dt;
                    p.y += Self::SINK_SPEED * dt;
                    if p.y >= bottom {
                        p.y = bottom;
                    }
                }
            }
        }
    }

    /// Repaint the area: bg fill, then above-water particles, then the
    /// water body, then submerged (sinking) particles tinted on top.
    fn paint(&self, buf: &mut Buffer, elapsed: Duration, level: f32, amp: f32) {
        let area = self.area;
        let t = elapsed.as_secs_f32();
        let water_top_mean = area.height as f32 - level;

        for dy in 0..area.height {
            for dx in 0..area.width {
                if let Some(dst) = buf.cell_mut((area.x + dx, area.y + dy)) {
                    *dst = self.fill.clone();
                }
            }
        }
        // Resting / flying particles sit above (or splash through) the
        // surface — paint them first so the water can cover any that are
        // momentarily below the waterline.
        for p in self.particles.iter() {
            if p.state == PState::Sinking {
                continue;
            }
            self.stamp(buf, p.x, p.y, |dst| *dst = p.cell.clone());
        }
        self.paint_water(buf, elapsed, level, amp);
        // Sinking particles are drawn over the water, tinted by depth so
        // they read as submerged and drifting down.
        for p in self.particles.iter() {
            if p.state != PState::Sinking {
                continue;
            }
            let depth = (p.y - Self::surface_at(p.x, t, water_top_mean, amp)).max(0.0);
            let sym = p.cell.symbol().to_string();
            let base = color_to_rgb(p.cell.fg).unwrap_or((230, 230, 230));
            let fg = lerp_rgb(base, water_rgb(depth), 0.55);
            let bg = water_rgb(depth + 0.5);
            self.stamp(buf, p.x, p.y, |dst| {
                dst.set_symbol(&sym);
                dst.set_fg(Color::Rgb(fg.0, fg.1, fg.2));
                dst.set_bg(Color::Rgb(bg.0, bg.1, bg.2));
            });
        }
    }

    /// Write to the cell at rounded `(x, y)` if it's on screen.
    fn stamp(&self, buf: &mut Buffer, x: f32, y: f32, f: impl FnOnce(&mut Cell)) {
        let area = self.area;
        let (cx, cy) = (x.round(), y.round());
        if cx < 0.0 || cy < 0.0 {
            return;
        }
        let (cx, cy) = (cx as u16, cy as u16);
        if cx >= area.width || cy >= area.height {
            return;
        }
        if let Some(dst) = buf.cell_mut((area.x + cx, area.y + cy)) {
            f(dst);
        }
    }

    /// Paint the water body: every cell at or below the undulating surface
    /// is tinted (shallow→deep by depth), the crest row gets foam glyphs,
    /// and `LAYERS` extra undulating foam lines ripple within the body.
    fn paint_water(&self, buf: &mut Buffer, elapsed: Duration, level: f32, amp: f32) {
        const CREST: [&str; 3] = ["~", "≈", "∿"];
        let area = self.area;
        let h = area.height as f32;
        let t = elapsed.as_secs_f32();
        let water_top_mean = h - level;
        let foam = Color::Rgb(210, 245, 255);

        for dx in 0..area.width {
            let x = dx as f32;
            let surf = Self::surface_at(x, t, water_top_mean, amp);
            for dy in 0..area.height {
                let y = dy as f32;
                // Cells above the surface stay as air (content / bg).
                if y + 0.5 < surf {
                    continue;
                }
                let depth = y - surf;
                let Some(dst) = buf.cell_mut((area.x + dx, area.y + dy)) else {
                    continue;
                };
                let body = water_rgb(depth);
                if depth < 0.9 {
                    // Foam crest right at the waterline.
                    let gi = ((x * 0.5 + t * 1.6).floor() as i64).rem_euclid(3) as usize;
                    let crest_bg = lerp_rgb((70, 170, 228), body, 0.5);
                    dst.set_symbol(CREST[gi]);
                    dst.set_fg(foam);
                    dst.set_bg(Color::Rgb(crest_bg.0, crest_bg.1, crest_bg.2));
                } else {
                    // Body: colored water with sparse, slowly twinkling
                    // bubbles for texture.
                    let bg = Color::Rgb(body.0, body.1, body.2);
                    if hash01(x + (t * 1.5).floor(), y) > 0.95 {
                        dst.set_symbol("∘");
                        dst.set_fg(Color::Rgb(150, 205, 235));
                    } else {
                        dst.set_symbol(" ");
                        dst.set_fg(bg);
                    }
                    dst.set_bg(bg);
                }
            }
        }

        // Internal undulating layers — each is its own surface curve, on a
        // distinct phase, sitting progressively deeper. They ride on top of
        // the water bg painted above, so they read as ripples within the
        // body rather than replacing its color.
        for layer in 1..=Self::LAYERS {
            let lp = layer as f32 * 2.3;
            let base = layer as f32 * Self::LAYER_SPACING;
            let lf = layer as f32 / (Self::LAYERS as f32 + 1.0);
            let fg_rgb = lerp_rgb((190, 235, 255), (40, 120, 190), lf);
            let fg = Color::Rgb(fg_rgb.0, fg_rgb.1, fg_rgb.2);
            for dx in 0..area.width {
                let x = dx as f32;
                let surf = Self::surface_at(x, t, water_top_mean, amp);
                let ly =
                    water_top_mean - Self::undulation(x, t, lp, amp) - Self::swing(t, amp) + base;
                // Keep the layer strictly inside the body.
                if ly <= surf + 0.6 || ly < 0.0 || ly >= h {
                    continue;
                }
                let row = ly.round();
                if row < 0.0 || row >= h {
                    continue;
                }
                if let Some(dst) = buf.cell_mut((area.x + dx, area.y + row as u16)) {
                    let gi =
                        ((x * 0.4 + t * 1.2 + layer as f32).floor() as i64).rem_euclid(3) as usize;
                    dst.set_symbol(CREST[gi]);
                    dst.set_fg(fg);
                }
            }
        }
    }
}

impl FrameEffect for WaveEffect {
    fn apply(&mut self, buf: &mut Buffer, area: Rect, elapsed: Duration) -> EffectStatus {
        let t = if self.duration.is_zero() {
            1.0
        } else {
            (elapsed.as_secs_f32() / self.duration.as_secs_f32()).clamp(0.0, 1.0)
        };
        // Hard cap reached: the tide has ebbed. Paint nothing and report
        // Done so the live UI (re-painted under us every frame) shows
        // through cleanly.
        if t >= 1.0 {
            return EffectStatus::Done;
        }

        if !self.initialized || self.area != area {
            // First frame (or a resize changed the area): re-snapshot the
            // freshly painted buffer. A mid-flight resize is rare;
            // restarting is simpler and visually fine.
            self.init(buf, area);
            self.last_elapsed = Some(elapsed);
            let (level, amp) = self.level_amp(elapsed);
            self.paint(buf, elapsed, level, amp);
            return EffectStatus::Running;
        }

        // Integrate from the last frame's timestamp. We sub-step in fixed
        // slices so the simulation advances by the true wall-clock delta
        // regardless of frame rate (a slow debug frame can be 50–100ms),
        // while each slice stays small enough to keep the arcs stable. A
        // long stall (debugger pause) is capped so it can't explode.
        let prev = self.last_elapsed.unwrap_or(elapsed);
        let dt = (elapsed.as_secs_f32() - prev.as_secs_f32()).clamp(0.0, 0.25);
        self.last_elapsed = Some(elapsed);
        let (level, amp) = self.level_amp(elapsed);
        let water_top_mean = self.area.height as f32 - level;
        const SUB: f32 = 1.0 / 120.0;
        let mut remaining = dt;
        while remaining > 0.0 {
            let step = remaining.min(SUB);
            self.step(step, elapsed.as_secs_f32(), water_top_mean, amp);
            remaining -= step;
        }

        self.paint(buf, elapsed, level, amp);
        EffectStatus::Running
    }
}

/// Smoothstep on [0, 1]: 0 at 0, 1 at 1, flat slope at both ends.
fn smoothstep(x: f32) -> f32 {
    let x = x.clamp(0.0, 1.0);
    x * x * (3.0 - 2.0 * x)
}

/// Linear interpolation between two RGB triples (`f`=0 → `a`, `f`=1 → `b`).
fn lerp_rgb(a: (u8, u8, u8), b: (u8, u8, u8), f: f32) -> (u8, u8, u8) {
    let f = f.clamp(0.0, 1.0);
    let mix = |a: u8, b: u8| (a as f32 + (b as f32 - a as f32) * f).round() as u8;
    (mix(a.0, b.0), mix(a.1, b.1), mix(a.2, b.2))
}

/// Water color at a given depth below the surface: shallow turquoise at
/// the top fading to deep navy further down.
fn water_rgb(depth: f32) -> (u8, u8, u8) {
    const SHALLOW: (u8, u8, u8) = (38, 132, 205);
    const DEEP: (u8, u8, u8) = (6, 26, 68);
    lerp_rgb(SHALLOW, DEEP, (depth / 14.0).clamp(0.0, 1.0))
}

/// Cheap deterministic hash of a cell's home position to a float in
/// [0, 1). Gives each particle a stable per-cell jitter without pulling in
/// an RNG dependency.
fn hash01(x: f32, y: f32) -> f32 {
    let xi = x as i64;
    let yi = y as i64;
    let mut h = (xi.wrapping_mul(73_856_093) ^ yi.wrapping_mul(19_349_663)) as u64;
    h ^= h >> 13;
    h = h.wrapping_mul(0x9E37_79B9_7F4A_7C15);
    h ^= h >> 16;
    (h & 0xFFFF) as f32 / 65_536.0
}

fn blend_rgb(fg: (u8, u8, u8), bg: (u8, u8, u8), alpha: f32) -> Color {
    let a = alpha.clamp(0.0, 1.0);
    let mix = |f: u8, b: u8| -> u8 {
        ((f as f32) * a + (b as f32) * (1.0 - a))
            .round()
            .clamp(0.0, 255.0) as u8
    };
    Color::Rgb(mix(fg.0, bg.0), mix(fg.1, bg.1), mix(fg.2, bg.2))
}

fn color_to_rgb(color: Color) -> Option<(u8, u8, u8)> {
    match color {
        Color::Rgb(r, g, b) => Some((r, g, b)),
        Color::Black => Some((0, 0, 0)),
        Color::Red => Some((205, 0, 0)),
        Color::Green => Some((0, 205, 0)),
        Color::Yellow => Some((205, 205, 0)),
        Color::Blue => Some((0, 0, 238)),
        Color::Magenta => Some((205, 0, 205)),
        Color::Cyan => Some((0, 205, 205)),
        Color::Gray => Some((229, 229, 229)),
        Color::DarkGray => Some((127, 127, 127)),
        Color::LightRed => Some((255, 0, 0)),
        Color::LightGreen => Some((0, 255, 0)),
        Color::LightYellow => Some((255, 255, 0)),
        Color::LightBlue => Some((92, 92, 255)),
        Color::LightMagenta => Some((255, 0, 255)),
        Color::LightCyan => Some((0, 255, 255)),
        Color::White => Some((255, 255, 255)),
        // 256-color palette: skip — themes virtually always supply RGB
        // for cursor/editor_bg, and this would pull in a lookup table for
        // a vanishingly rare case.
        Color::Indexed(_) => None,
        Color::Reset => None,
    }
}

struct ActiveEffect {
    id: AnimationId,
    area: Rect,
    started: Instant,
    delay: Duration,
    effect: Box<dyn FrameEffect + Send>,
    status: EffectStatus,
    deadline: Instant,
    /// An interactive, runs-until-dismissed effect (the wave). Such effects
    /// are torn down by the next key press or mouse move rather than a
    /// timer; `cancel_dismissable` removes them.
    dismissable: bool,
}

pub struct AnimationRunner {
    next_id: u64,
    active: Vec<ActiveEffect>,
    /// Cumulative count of effects accepted by either `start` or
    /// `start_with_id`. Monotonic; increments before the effect is
    /// pushed so a sample taken any time after the call sees the
    /// post-increment value. Tests sample this around the action under
    /// test to detect that an effect was kicked off without having to
    /// catch the transient `is_active()` window between polling ticks.
    total_started: u64,
    /// Full snapshot of the buffer at the end of the previous render
    /// pass. Ratatui's swap_buffers resets the "current" buffer, so at
    /// the start of the next draw `frame.buffer_mut()` is blank — not
    /// the previous frame. We keep our own copy so `capture_before`
    /// can see what the user actually saw last frame.
    last_frame: Option<Buffer>,
}

impl Default for AnimationRunner {
    fn default() -> Self {
        Self::new()
    }
}

impl AnimationRunner {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            active: Vec::new(),
            total_started: 0,
            last_frame: None,
        }
    }

    pub fn start(&mut self, area: Rect, kind: AnimationKind) -> AnimationId {
        let id = AnimationId(self.next_id);
        self.next_id += 1;
        self.start_with_id(id, area, kind);
        id
    }

    /// Start an effect using a caller-supplied ID. Intended for the plugin
    /// bridge, where the plugin-side counter is the source of truth so the
    /// JS call can return the ID synchronously.
    ///
    /// Replaces any existing active effect covering exactly the same
    /// area. Without this, rapid re-triggers over the same Rect (tab
    /// cycling, dashboard data refresh) stack effects whose snapshots
    /// contaminate each other: the second effect's "after" snapshot is
    /// taken from a buffer the first effect has already shifted, so
    /// when both finish the final image is frozen mid-transition.
    /// Replacement keeps exactly one effect per area — the latest one
    /// wins, its before-snapshot (read from the runner's last_frame)
    /// captures whatever the user is actually seeing right now,
    /// including any in-flight shift, and the new push-over-blanks
    /// starts from there.
    pub fn start_with_id(&mut self, id: AnimationId, area: Rect, kind: AnimationKind) {
        self.active.retain(|e| e.area != area);
        let now = Instant::now();
        let (effect, delay, duration): (Box<dyn FrameEffect + Send>, Duration, Duration) =
            match kind {
                AnimationKind::SlideIn {
                    from,
                    duration,
                    delay,
                } => (Box::new(SlideIn::new(from, duration)), delay, duration),
                AnimationKind::CursorJump {
                    from,
                    to,
                    duration,
                    cursor_color,
                    bg_color,
                } => (
                    Box::new(CursorJump::new(from, to, duration, cursor_color, bg_color)),
                    Duration::ZERO,
                    duration,
                ),
                AnimationKind::ColorTransition { duration } => (
                    Box::new(ColorTransition::new(duration)),
                    Duration::ZERO,
                    duration,
                ),
                AnimationKind::Wave { duration } => (
                    Box::new(WaveEffect::new(duration)),
                    Duration::ZERO,
                    duration,
                ),
            };
        let dismissable = matches!(kind, AnimationKind::Wave { .. });
        self.total_started += 1;
        self.active.push(ActiveEffect {
            id,
            area,
            started: now,
            delay,
            effect,
            status: EffectStatus::Running,
            deadline: now + delay + duration,
            dismissable,
        });
    }

    pub fn cancel(&mut self, id: AnimationId) {
        self.active.retain(|e| e.id != id);
    }

    /// True if an interactive, dismiss-on-input effect (the wave) is
    /// running.
    pub fn has_dismissable(&self) -> bool {
        self.active.iter().any(|e| e.dismissable)
    }

    /// Tear down any interactive, dismiss-on-input effect (the wave).
    pub fn cancel_dismissable(&mut self) {
        self.active.retain(|e| !e.dismissable);
    }

    /// Let each active effect snapshot the "before" state of its Rect
    /// from the cached last-frame buffer. Called once per render, at
    /// the start of the pass. We can't read the live `frame.buffer_mut()`
    /// here because ratatui resets the current buffer before each draw
    /// (see `swap_buffers`); our own cache is what actually holds what
    /// was on screen last frame.
    ///
    /// Effects still in their `delay` window are skipped, and effects
    /// whose Rect falls outside the cached buffer (resize shrank the
    /// terminal) are skipped too — they fall back to the slide-over-
    /// blanks path.
    pub fn capture_before_all(&mut self) {
        let now = Instant::now();
        let Some(prev) = self.last_frame.as_ref() else {
            return;
        };
        let prev_area = prev.area;
        for e in self.active.iter_mut() {
            if now < e.started + e.delay {
                continue;
            }
            if !rect_contains(prev_area, e.area) {
                continue;
            }
            e.effect.capture_before(prev, e.area);
        }
    }

    pub fn apply_all(&mut self, buf: &mut Buffer) {
        let now = Instant::now();
        for e in self.active.iter_mut() {
            let effective_start = e.started + e.delay;
            if now < effective_start {
                continue;
            }
            let elapsed = now - effective_start;
            e.status = e.effect.apply(buf, e.area, elapsed);
        }
        self.active.retain(|e| e.status == EffectStatus::Running);

        // Cache the final painted buffer so the next frame's
        // `capture_before_all` can read it. We clone because ratatui
        // resets the current buffer before the next draw.
        self.last_frame = Some(buf.clone());
    }

    pub fn is_active(&self) -> bool {
        self.active
            .iter()
            .any(|e| e.status == EffectStatus::Running)
    }

    /// Cumulative number of effects accepted by either `start` or
    /// `start_with_id`, since this runner was constructed. Monotonic —
    /// never decreases. Tests use this to detect that an effect was
    /// kicked off without having to catch the transient `is_active()`
    /// window between two polling ticks.
    pub fn total_started(&self) -> u64 {
        self.total_started
    }

    pub fn next_deadline(&self) -> Option<Instant> {
        self.active.iter().map(|e| e.deadline).min()
    }

    /// Area of the cached last-frame buffer, i.e. the full screen as of
    /// the previous render. `None` until the first frame has been drawn.
    /// Full-screen effects (theme color transition) use this as their
    /// Rect so callers don't need to thread the terminal size through.
    pub fn last_frame_area(&self) -> Option<Rect> {
        self.last_frame.as_ref().map(|b| b.area)
    }

    /// True if `(col, row)` falls inside the area of any running effect.
    /// Use this to suppress click routing during an animation.
    pub fn is_animating_at(&self, col: u16, row: u16) -> bool {
        self.active.iter().any(|e| {
            e.status == EffectStatus::Running
                && col >= e.area.x
                && col < e.area.x.saturating_add(e.area.width)
                && row >= e.area.y
                && row < e.area.y.saturating_add(e.area.height)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::style::Color;

    fn make_buf(w: u16, h: u16) -> Buffer {
        Buffer::empty(Rect::new(0, 0, w, h))
    }

    fn paint(buf: &mut Buffer, area: Rect, ch: char, fg: Color) {
        for dy in 0..area.height {
            for dx in 0..area.width {
                if let Some(cell) = buf.cell_mut((area.x + dx, area.y + dy)) {
                    cell.set_symbol(&ch.to_string());
                    cell.set_fg(fg);
                }
            }
        }
    }

    #[test]
    fn slide_in_bottom_at_t0_pushes_content_out() {
        let area = Rect::new(0, 0, 4, 3);
        let mut buf = make_buf(4, 3);
        paint(&mut buf, area, 'X', Color::Red);

        let mut runner = AnimationRunner::new();
        runner.start(
            area,
            AnimationKind::SlideIn {
                from: Edge::Bottom,
                duration: Duration::from_millis(500),
                delay: Duration::ZERO,
            },
        );
        // First apply_all snapshots and paints t≈0. Content is shifted down by
        // area.height rows, so every visible row is blank.
        runner.apply_all(&mut buf);
        for dy in 0..area.height {
            for dx in 0..area.width {
                let cell = buf.cell((area.x + dx, area.y + dy)).unwrap();
                assert_eq!(cell.symbol(), " ", "blank at ({}, {}) at t=0", dx, dy);
            }
        }
    }

    #[test]
    fn slide_in_bottom_at_duration_matches_snapshot() {
        let area = Rect::new(0, 0, 4, 3);
        let mut buf = make_buf(4, 3);
        paint(&mut buf, area, 'X', Color::Red);

        // Construct SlideIn directly so we can drive its clock.
        let mut effect = SlideIn::new(Edge::Bottom, Duration::from_millis(100));
        // First apply at t=0 snapshots the buffer.
        effect.apply(&mut buf, area, Duration::ZERO);
        // Now drive it to t=duration: result should equal the original painted content.
        let status = effect.apply(&mut buf, area, Duration::from_millis(100));
        assert_eq!(status, EffectStatus::Done);
        for dy in 0..area.height {
            for dx in 0..area.width {
                let cell = buf.cell((area.x + dx, area.y + dy)).unwrap();
                assert_eq!(cell.symbol(), "X");
                assert_eq!(cell.fg, Color::Red);
            }
        }
    }

    #[test]
    fn slide_in_with_before_snapshot_pushes_old_out() {
        // Before: 'O' everywhere. After: 'N' everywhere.
        let area = Rect::new(0, 0, 3, 4);
        let mut before_buf = make_buf(3, 4);
        paint(&mut before_buf, area, 'O', Color::Green);
        let mut after_buf = make_buf(3, 4);
        paint(&mut after_buf, area, 'N', Color::Blue);

        let mut effect = SlideIn::new(Edge::Bottom, Duration::from_millis(100));
        effect.capture_before(&before_buf, area);
        // Mid-transition: at t=0.5, half of OLD should still be
        // visible (shifted up) and half of NEW should have entered
        // (shifted down from the bottom). No blank cells — push
        // means the edge vacated by OLD is filled by NEW.
        let mut work = after_buf.clone();
        effect.apply(&mut work, area, Duration::from_millis(50));
        for dy in 0..area.height {
            for dx in 0..area.width {
                let cell = work.cell((area.x + dx, area.y + dy)).unwrap();
                let sym = cell.symbol();
                assert!(
                    sym == "N" || sym == "O",
                    "push should paint only OLD or NEW cells, got {:?} at ({},{})",
                    sym,
                    dx,
                    dy
                );
            }
        }
        // And: at t=duration, the AFTER content is fully in place.
        let status = effect.apply(&mut work, area, Duration::from_millis(100));
        assert_eq!(status, EffectStatus::Done);
        for dy in 0..area.height {
            for dx in 0..area.width {
                let cell = work.cell((area.x + dx, area.y + dy)).unwrap();
                assert_eq!(cell.symbol(), "N");
            }
        }
    }

    #[test]
    fn runner_caches_last_frame_for_push_transition() {
        // Simulate two frames:
        //   frame 1: buf contains OLD content, no effects, runner
        //            caches this as last_frame.
        //   frame 2: an effect is started, capture_before_all reads
        //            OLD from the cache (not the blank live buffer),
        //            then buf is repainted with NEW, apply_all runs
        //            the push using OLD as the before.
        let area = Rect::new(0, 0, 3, 3);
        let mut runner = AnimationRunner::new();

        // Frame 1: paint OLD into buf, run apply_all (no effects) so
        // the runner caches it.
        let mut frame1 = make_buf(3, 3);
        paint(&mut frame1, area, 'O', Color::Green);
        runner.apply_all(&mut frame1);
        assert!(runner.last_frame.is_some());

        // Frame 2: start the effect, capture_before_all (reads cache),
        // paint NEW into a fresh blank buf (simulating ratatui reset),
        // then apply_all.
        let id = runner.start(
            area,
            AnimationKind::SlideIn {
                from: Edge::Bottom,
                duration: Duration::from_millis(100),
                delay: Duration::ZERO,
            },
        );
        runner.capture_before_all();
        let mut frame2 = make_buf(3, 3); // blank, like ratatui's reset
        paint(&mut frame2, area, 'N', Color::Blue);
        runner.apply_all(&mut frame2);

        // Mid-transition the painted cells should include OLD pixels
        // being pushed out — not blanks where OLD used to be.
        let mut seen_old = false;
        for dy in 0..area.height {
            for dx in 0..area.width {
                let cell = frame2.cell((area.x + dx, area.y + dy)).unwrap();
                if cell.symbol() == "O" {
                    seen_old = true;
                }
                assert!(
                    cell.symbol() == "O" || cell.symbol() == "N",
                    "push should paint only OLD or NEW, got {:?}",
                    cell.symbol()
                );
            }
        }
        assert!(
            seen_old,
            "at least one OLD cell should still be visible mid-transition"
        );
        let _ = id;
    }

    #[test]
    fn runner_is_active_flips_after_duration() {
        let area = Rect::new(0, 0, 2, 2);
        let mut buf = make_buf(2, 2);
        let mut runner = AnimationRunner::new();
        runner.start(
            area,
            AnimationKind::SlideIn {
                from: Edge::Bottom,
                duration: Duration::from_millis(10),
                delay: Duration::ZERO,
            },
        );
        assert!(runner.is_active());
        runner.apply_all(&mut buf);
        assert!(runner.is_active(), "still running immediately after start");
        std::thread::sleep(Duration::from_millis(25));
        runner.apply_all(&mut buf);
        assert!(
            !runner.is_active(),
            "runner should have no active effects after duration elapses"
        );
    }

    #[test]
    fn cancel_removes_effect_and_leaves_buffer_unchanged() {
        let area = Rect::new(0, 0, 4, 3);
        let mut buf = make_buf(4, 3);
        paint(&mut buf, area, 'X', Color::Red);

        let mut runner = AnimationRunner::new();
        let id = runner.start(
            area,
            AnimationKind::SlideIn {
                from: Edge::Bottom,
                duration: Duration::from_millis(500),
                delay: Duration::ZERO,
            },
        );
        runner.cancel(id);
        assert!(!runner.is_active());

        // A fresh buffer with the same content — apply_all must leave it alone.
        let mut buf2 = make_buf(4, 3);
        paint(&mut buf2, area, 'X', Color::Red);
        runner.apply_all(&mut buf2);
        for dy in 0..area.height {
            for dx in 0..area.width {
                let cell = buf2.cell((area.x + dx, area.y + dy)).unwrap();
                assert_eq!(cell.symbol(), "X");
                assert_eq!(cell.fg, Color::Red);
            }
        }
    }

    #[test]
    fn delay_defers_application() {
        let area = Rect::new(0, 0, 2, 2);
        let mut buf = make_buf(2, 2);
        paint(&mut buf, area, 'X', Color::Red);

        let mut runner = AnimationRunner::new();
        runner.start(
            area,
            AnimationKind::SlideIn {
                from: Edge::Bottom,
                duration: Duration::from_millis(10),
                delay: Duration::from_secs(3600),
            },
        );
        runner.apply_all(&mut buf);
        // Under the delay, apply is a no-op — buffer retains painted content.
        for dy in 0..area.height {
            for dx in 0..area.width {
                let cell = buf.cell((area.x + dx, area.y + dy)).unwrap();
                assert_eq!(cell.symbol(), "X");
            }
        }
        assert!(runner.is_active());
    }

    #[test]
    fn next_deadline_is_earliest() {
        // Use two DIFFERENT areas so neither effect replaces the other
        // (`start_with_id` drops any existing effect on the same Rect).
        let area_a = Rect::new(0, 0, 2, 2);
        let area_b = Rect::new(0, 2, 2, 2);
        let mut runner = AnimationRunner::new();
        runner.start(
            area_a,
            AnimationKind::SlideIn {
                from: Edge::Bottom,
                duration: Duration::from_millis(100),
                delay: Duration::ZERO,
            },
        );
        let d1 = runner.next_deadline().unwrap();
        runner.start(
            area_b,
            AnimationKind::SlideIn {
                from: Edge::Bottom,
                duration: Duration::from_millis(1000),
                delay: Duration::ZERO,
            },
        );
        let d2 = runner.next_deadline().unwrap();
        assert!(d2 <= d1 + Duration::from_millis(5));
    }

    #[test]
    fn starting_effect_on_same_area_replaces_previous() {
        let area = Rect::new(0, 0, 2, 2);
        let mut runner = AnimationRunner::new();
        let first = runner.start(
            area,
            AnimationKind::SlideIn {
                from: Edge::Bottom,
                duration: Duration::from_millis(500),
                delay: Duration::ZERO,
            },
        );
        assert_eq!(runner.active.len(), 1);
        let second = runner.start(
            area,
            AnimationKind::SlideIn {
                from: Edge::Top,
                duration: Duration::from_millis(500),
                delay: Duration::ZERO,
            },
        );
        // Exactly one effect still active, and it's the newer one.
        assert_eq!(runner.active.len(), 1);
        assert_eq!(runner.active[0].id, second);
        assert_ne!(first, second);
    }

    #[test]
    fn cursor_jump_final_frame_is_clean() {
        // Cursor jumps from (0,0) to (4,2). At t>=1.0 the effect must
        // paint nothing and just report Done so the last frame on screen
        // has no leftover trail (no further redraw is scheduled once the
        // runner drops the effect).
        let area = Rect::new(0, 0, 6, 4);
        let mut buf = make_buf(6, 4);
        paint(&mut buf, area, '.', Color::White);
        let bg_before: Vec<_> = (0..area.height)
            .flat_map(|dy| (0..area.width).map(move |dx| (dx, dy)))
            .map(|(dx, dy)| buf.cell((area.x + dx, area.y + dy)).unwrap().bg)
            .collect();

        let mut effect = CursorJump::new(
            (0, 0),
            (4, 2),
            Duration::from_millis(100),
            Color::Rgb(255, 200, 0),
            Color::Rgb(20, 20, 20),
        );
        let status = effect.apply(&mut buf, area, Duration::from_millis(100));
        assert_eq!(status, EffectStatus::Done);

        let mut idx = 0;
        for dy in 0..area.height {
            for dx in 0..area.width {
                let cell = buf.cell((area.x + dx, area.y + dy)).unwrap();
                assert_eq!(
                    cell.bg, bg_before[idx],
                    "no cell bg should change at t>=1.0, but ({}, {}) did",
                    dx, dy
                );
                idx += 1;
            }
        }
    }

    #[test]
    fn cursor_jump_head_uses_cursor_color() {
        // Mid-flight, the head cell (sample at the leading edge of the
        // trail) should be painted with the full cursor color (alpha=1).
        let area = Rect::new(0, 0, 12, 5);
        let mut buf = make_buf(12, 5);
        paint(&mut buf, area, '.', Color::White);

        let cursor = Color::Rgb(255, 100, 0);
        let bg = Color::Rgb(0, 0, 0);
        let mut effect = CursorJump::new((0, 0), (10, 4), Duration::from_millis(100), cursor, bg);
        let status = effect.apply(&mut buf, area, Duration::from_millis(50));
        assert_eq!(status, EffectStatus::Running);

        let mut found_full_cursor = false;
        for dy in 0..area.height {
            for dx in 0..area.width {
                let cell = buf.cell((area.x + dx, area.y + dy)).unwrap();
                if cell.bg == cursor {
                    found_full_cursor = true;
                }
            }
        }
        assert!(
            found_full_cursor,
            "head cell should be painted with the full cursor color"
        );
    }

    #[test]
    fn cursor_jump_trail_fades_toward_bg() {
        // Tail cells (older positions) must blend toward bg; checking that
        // among cells the effect touches there is at least one whose bg is
        // strictly between the cursor color and the bg color (i.e., a true
        // blend, not just one or the other).
        let area = Rect::new(0, 0, 20, 5);
        let mut buf = make_buf(20, 5);
        paint(&mut buf, area, '.', Color::White);

        let cursor = Color::Rgb(255, 0, 0);
        let bg = Color::Rgb(0, 0, 0);
        let mut effect = CursorJump::new((0, 0), (18, 4), Duration::from_millis(100), cursor, bg);
        let _ = effect.apply(&mut buf, area, Duration::from_millis(70));

        let mut blended_count = 0;
        for dy in 0..area.height {
            for dx in 0..area.width {
                let cell = buf.cell((area.x + dx, area.y + dy)).unwrap();
                if let Color::Rgb(r, g, b) = cell.bg {
                    // Strictly between cursor (255,0,0) and bg (0,0,0):
                    // red channel partially attenuated, others still 0.
                    if r > 0 && r < 255 && g == 0 && b == 0 {
                        blended_count += 1;
                    }
                }
            }
        }
        assert!(
            blended_count > 0,
            "at least one trail cell should be a blend between cursor and bg"
        );
    }

    #[test]
    fn cursor_jump_through_runner() {
        let mut runner = AnimationRunner::new();
        let area = Rect::new(0, 0, 10, 5);
        let id = runner.start(
            area,
            AnimationKind::CursorJump {
                from: (1, 1),
                to: (8, 4),
                duration: Duration::from_millis(50),
                cursor_color: Color::Rgb(255, 255, 0),
                bg_color: Color::Rgb(0, 0, 0),
            },
        );
        assert!(runner.is_active());
        let mut buf = make_buf(10, 5);
        paint(&mut buf, area, ' ', Color::Reset);
        runner.apply_all(&mut buf);
        // Should still be running right after start.
        assert!(runner.is_active());
        std::thread::sleep(Duration::from_millis(80));
        runner.apply_all(&mut buf);
        assert!(
            !runner.is_active(),
            "cursor jump should complete after duration"
        );
        let _ = id;
    }

    fn paint_colors(buf: &mut Buffer, area: Rect, fg: Color, bg: Color) {
        for dy in 0..area.height {
            for dx in 0..area.width {
                if let Some(cell) = buf.cell_mut((area.x + dx, area.y + dy)) {
                    cell.set_symbol("x");
                    cell.set_fg(fg);
                    cell.set_bg(bg);
                }
            }
        }
    }

    #[test]
    fn color_transition_starts_at_old_colors() {
        let area = Rect::new(0, 0, 3, 2);
        let mut old = make_buf(3, 2);
        paint_colors(
            &mut old,
            area,
            Color::Rgb(200, 100, 0),
            Color::Rgb(10, 20, 30),
        );
        let mut new = make_buf(3, 2);
        paint_colors(
            &mut new,
            area,
            Color::Rgb(0, 100, 200),
            Color::Rgb(90, 80, 70),
        );

        let mut effect = ColorTransition::new(Duration::from_millis(100));
        effect.capture_before(&old, area);
        let status = effect.apply(&mut new, area, Duration::ZERO);
        assert_eq!(status, EffectStatus::Running);
        let cell = new.cell((0, 0)).unwrap();
        assert_eq!(cell.fg, Color::Rgb(200, 100, 0), "t=0 shows old fg");
        assert_eq!(cell.bg, Color::Rgb(10, 20, 30), "t=0 shows old bg");
        assert_eq!(cell.symbol(), "x", "glyphs are not touched");
    }

    #[test]
    fn color_transition_blends_mid_flight() {
        let area = Rect::new(0, 0, 2, 2);
        let mut old = make_buf(2, 2);
        paint_colors(&mut old, area, Color::Rgb(255, 0, 0), Color::Rgb(0, 0, 0));
        let mut new = make_buf(2, 2);
        paint_colors(
            &mut new,
            area,
            Color::Rgb(0, 0, 0),
            Color::Rgb(255, 255, 255),
        );

        let mut effect = ColorTransition::new(Duration::from_millis(100));
        effect.capture_before(&old, area);
        let status = effect.apply(&mut new, area, Duration::from_millis(50));
        assert_eq!(status, EffectStatus::Running);
        let cell = new.cell((1, 1)).unwrap();
        match cell.fg {
            Color::Rgb(r, g, b) => {
                assert!(r > 0 && r < 255, "fg red mid-blend, got {}", r);
                assert_eq!((g, b), (0, 0));
            }
            other => panic!("expected RGB fg, got {:?}", other),
        }
        match cell.bg {
            Color::Rgb(r, g, b) => {
                assert!(r > 0 && r < 255, "bg mid-blend, got {}", r);
                assert_eq!(r, g);
                assert_eq!(g, b);
            }
            other => panic!("expected RGB bg, got {:?}", other),
        }
    }

    #[test]
    fn color_transition_final_frame_is_untouched() {
        // At t>=duration the buffer must keep its pure new-theme colors —
        // the runner drops the effect after this call and no further
        // redraw is scheduled, so any tint painted now would stick.
        let area = Rect::new(0, 0, 2, 2);
        let mut old = make_buf(2, 2);
        paint_colors(&mut old, area, Color::Rgb(255, 0, 0), Color::Rgb(0, 0, 0));
        let mut new = make_buf(2, 2);
        paint_colors(&mut new, area, Color::Rgb(1, 2, 3), Color::Rgb(4, 5, 6));

        let mut effect = ColorTransition::new(Duration::from_millis(100));
        effect.capture_before(&old, area);
        let status = effect.apply(&mut new, area, Duration::from_millis(100));
        assert_eq!(status, EffectStatus::Done);
        let cell = new.cell((0, 1)).unwrap();
        assert_eq!(cell.fg, Color::Rgb(1, 2, 3));
        assert_eq!(cell.bg, Color::Rgb(4, 5, 6));
    }

    #[test]
    fn color_transition_without_before_snapshot_is_done() {
        let area = Rect::new(0, 0, 2, 2);
        let mut new = make_buf(2, 2);
        paint_colors(&mut new, area, Color::Rgb(1, 2, 3), Color::Rgb(4, 5, 6));

        let mut effect = ColorTransition::new(Duration::from_millis(100));
        let status = effect.apply(&mut new, area, Duration::ZERO);
        assert_eq!(status, EffectStatus::Done, "no old frame — snap to new");
        let cell = new.cell((0, 0)).unwrap();
        assert_eq!(cell.fg, Color::Rgb(1, 2, 3));
        assert_eq!(cell.bg, Color::Rgb(4, 5, 6));
    }

    #[test]
    fn color_transition_leaves_unresolvable_colors_alone() {
        // Reset has no RGB equivalent — those cells must flip instantly
        // rather than blend through a bogus fallback color.
        let area = Rect::new(0, 0, 1, 1);
        let mut old = make_buf(1, 1);
        paint_colors(&mut old, area, Color::Reset, Color::Rgb(0, 0, 0));
        let mut new = make_buf(1, 1);
        paint_colors(&mut new, area, Color::Rgb(10, 10, 10), Color::Reset);

        let mut effect = ColorTransition::new(Duration::from_millis(100));
        effect.capture_before(&old, area);
        effect.apply(&mut new, area, Duration::from_millis(50));
        let cell = new.cell((0, 0)).unwrap();
        assert_eq!(
            cell.fg,
            Color::Rgb(10, 10, 10),
            "old fg was Reset — no blend"
        );
        assert_eq!(cell.bg, Color::Reset, "new bg is Reset — no blend");
    }

    #[test]
    fn color_transition_through_runner_uses_cached_frame() {
        // Frame 1: old-theme colors, no effects — runner caches the frame.
        let area = Rect::new(0, 0, 3, 2);
        let mut runner = AnimationRunner::new();
        let mut frame1 = make_buf(3, 2);
        paint_colors(
            &mut frame1,
            area,
            Color::Rgb(255, 0, 0),
            Color::Rgb(0, 0, 255),
        );
        runner.apply_all(&mut frame1);
        assert_eq!(runner.last_frame_area(), Some(area));

        // Frame 2: theme switched — start the transition, capture the old
        // frame from the cache, paint new-theme colors, apply. Right after
        // start (t≈0) the visible colors must still be (close to) the old
        // ones, not the new ones.
        runner.start(
            area,
            AnimationKind::ColorTransition {
                duration: Duration::from_secs(3600),
            },
        );
        runner.capture_before_all();
        let mut frame2 = make_buf(3, 2);
        paint_colors(
            &mut frame2,
            area,
            Color::Rgb(0, 255, 0),
            Color::Rgb(255, 255, 0),
        );
        runner.apply_all(&mut frame2);
        assert!(runner.is_active());

        let cell = frame2.cell((1, 1)).unwrap();
        let Color::Rgb(r, g, _) = cell.fg else {
            panic!("expected RGB fg, got {:?}", cell.fg);
        };
        assert!(
            r > 200 && g < 55,
            "right after start the fg should still be mostly the old red, got ({}, {})",
            r,
            g
        );
    }

    #[test]
    fn wave_snapshots_ink_and_disturbs_content() {
        // A buffer of 'A's gets a wave; after a step the crest has begun
        // climbing and the painted content should differ from the static
        // input (cells displaced / crest glyphs laid down).
        let area = Rect::new(0, 0, 8, 6);
        let mut buf = make_buf(8, 6);
        paint(&mut buf, area, 'A', Color::Rgb(200, 200, 200));

        let mut effect = WaveEffect::new(Duration::from_secs(600));
        // First apply initializes (snapshot) and paints t≈0.
        let s0 = effect.apply(&mut buf, area, Duration::ZERO);
        assert_eq!(s0, EffectStatus::Running);
        // Every cell was ink ('A'), so we get one particle per cell.
        assert_eq!(effect.particles.len(), (area.width * area.height) as usize);

        // Drive past the rise, where the water has climbed to ~half the
        // view and the swell has flung off the lower rows. The buffer
        // should no longer be all 'A'.
        for ms in [400u64, 900, 1500, 2200, 3000] {
            effect.apply(&mut buf, area, Duration::from_millis(ms));
        }
        let mut non_a = 0;
        for dy in 0..area.height {
            for dx in 0..area.width {
                if buf.cell((dx, dy)).unwrap().symbol() != "A" {
                    non_a += 1;
                }
            }
        }
        assert!(
            non_a > 0,
            "wave should have displaced content / drawn crest glyphs"
        );
    }

    #[test]
    fn wave_reports_done_at_duration_cap() {
        let area = Rect::new(0, 0, 4, 4);
        let mut buf = make_buf(4, 4);
        paint(&mut buf, area, 'Z', Color::White);
        let mut effect = WaveEffect::new(Duration::from_millis(100));
        effect.apply(&mut buf, area, Duration::ZERO);
        // At/after the hard cap the effect must report Done so the live UI
        // shows through cleanly (same contract as the other effects).
        let s = effect.apply(&mut buf, area, Duration::from_millis(100));
        assert_eq!(s, EffectStatus::Done);
    }

    #[test]
    fn wave_through_runner_is_active_then_finishes() {
        let area = Rect::new(0, 0, 6, 5);
        let mut runner = AnimationRunner::new();
        runner.start(
            area,
            AnimationKind::Wave {
                duration: Duration::from_millis(60),
            },
        );
        assert!(runner.is_active());
        let mut buf = make_buf(6, 5);
        paint(&mut buf, area, '#', Color::Rgb(180, 180, 180));
        runner.apply_all(&mut buf);
        assert!(runner.is_active(), "running right after start");
        std::thread::sleep(Duration::from_millis(90));
        runner.apply_all(&mut buf);
        assert!(!runner.is_active(), "wave finishes past its duration cap");
    }

    #[test]
    fn is_animating_at_covers_area() {
        let area = Rect::new(10, 5, 3, 2);
        let mut runner = AnimationRunner::new();
        runner.start(
            area,
            AnimationKind::SlideIn {
                from: Edge::Bottom,
                duration: Duration::from_millis(500),
                delay: Duration::ZERO,
            },
        );
        assert!(runner.is_animating_at(10, 5));
        assert!(runner.is_animating_at(12, 6));
        assert!(!runner.is_animating_at(9, 5));
        assert!(!runner.is_animating_at(13, 5));
        assert!(!runner.is_animating_at(10, 7));
    }
}
