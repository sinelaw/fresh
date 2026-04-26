//! Frame-buffer animation layer.
//!
//! Area-based post-processing effects applied after the main render pass.
//! The `FrameEffect` trait is the seam: concrete implementations mutate a
//! `(Buffer, Rect)` region given elapsed time. `AnimationRunner` drives
//! active effects from the render clock. The layer knows nothing about
//! virtual buffers; callers resolve areas and pass them in.
//!
//! Current effects: `SlideIn` only. Easing is an implementation detail.

use ratatui::buffer::{Buffer, Cell};
use ratatui::layout::Rect;
use ratatui::style::Modifier;
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
    /// Animate the cursor moving from one screen cell to another. Paints an
    /// inverted-color "head" cell at the interpolated position with a short
    /// fading trail along the line from `from` to `to`. `from`/`to` are
    /// absolute screen coordinates (col, row).
    CursorJump {
        from: (u16, u16),
        to: (u16, u16),
        duration: Duration,
    },
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
/// absolute screen coordinates (col, row). The effect operates outside the
/// `area` snapshot model used by `SlideIn`: it directly mutates cells along
/// the interpolated path and never reads/snapshots an area, so the `area`
/// passed to the runner is only used for dedupe/replacement bookkeeping.
pub struct CursorJump {
    from: (i32, i32),
    to: (i32, i32),
    duration: Duration,
}

impl CursorJump {
    pub fn new(from: (u16, u16), to: (u16, u16), duration: Duration) -> Self {
        Self {
            from: (from.0 as i32, from.1 as i32),
            to: (to.0 as i32, to.1 as i32),
            duration,
        }
    }

    fn highlight_cell(buf: &mut Buffer, col: i32, row: i32) {
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
            cell.set_style(cell.style().add_modifier(Modifier::REVERSED));
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
        let eased = ease_out_cubic(t);

        let (fx, fy) = (self.from.0 as f32, self.from.1 as f32);
        let (tx, ty) = (self.to.0 as f32, self.to.1 as f32);
        let dx = tx - fx;
        let dy = ty - fy;

        // Trail length scales with how much of the jump has been completed,
        // so the early frames have a short tail and later frames a longer
        // one — gives a sense of acceleration.
        let path_cells = dx.abs().max(dy.abs()).round() as i32;
        let trail_len = (path_cells.min(8).max(2)) as usize;

        for i in 0..trail_len {
            // Trail samples behind the head: i=0 is the head, larger i is
            // further back along the path.
            let back = (i as f32) / (trail_len as f32);
            let sample = (eased - back * 0.12).max(0.0);
            let col = (fx + dx * sample).round() as i32;
            let row = (fy + dy * sample).round() as i32;
            Self::highlight_cell(buf, col, row);
        }

        if t >= 1.0 {
            EffectStatus::Done
        } else {
            EffectStatus::Running
        }
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
                } => (
                    Box::new(CursorJump::new(from, to, duration)),
                    Duration::ZERO,
                    duration,
                ),
            };
        self.total_started += 1;
        self.active.push(ActiveEffect {
            id,
            area,
            started: now,
            delay,
            effect,
            status: EffectStatus::Running,
            deadline: now + delay + duration,
        });
    }

    pub fn cancel(&mut self, id: AnimationId) {
        self.active.retain(|e| e.id != id);
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
    fn cursor_jump_at_t1_highlights_target() {
        // Background painted with '.', cursor jumps from (0,0) to (4,2).
        let area = Rect::new(0, 0, 6, 4);
        let mut buf = make_buf(6, 4);
        paint(&mut buf, area, '.', Color::White);

        let mut effect = CursorJump::new((0, 0), (4, 2), Duration::from_millis(100));
        // Drive past the duration so the head lands exactly on the target.
        let status = effect.apply(&mut buf, area, Duration::from_millis(100));
        assert_eq!(status, EffectStatus::Done);

        let head = buf.cell((4, 2)).unwrap();
        assert!(
            head.style().add_modifier.contains(Modifier::REVERSED),
            "head cell at target should be REVERSED"
        );
    }

    #[test]
    fn cursor_jump_at_t0_highlights_source() {
        let area = Rect::new(0, 0, 6, 4);
        let mut buf = make_buf(6, 4);
        paint(&mut buf, area, '.', Color::White);

        let mut effect = CursorJump::new((0, 0), (4, 2), Duration::from_millis(100));
        let status = effect.apply(&mut buf, area, Duration::ZERO);
        assert_eq!(status, EffectStatus::Running);

        let head = buf.cell((0, 0)).unwrap();
        assert!(
            head.style().add_modifier.contains(Modifier::REVERSED),
            "head cell at source should be REVERSED at t=0"
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
