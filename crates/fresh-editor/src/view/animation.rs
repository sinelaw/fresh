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
    pub fn start_with_id(&mut self, id: AnimationId, area: Rect, kind: AnimationKind) {
        let now = Instant::now();
        let (effect, delay, duration): (Box<dyn FrameEffect + Send>, Duration, Duration) =
            match kind {
                AnimationKind::SlideIn {
                    from,
                    duration,
                    delay,
                } => (Box::new(SlideIn::new(from, duration)), delay, duration),
            };
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

    /// Let each active effect peek at the buffer BEFORE the main paint
    /// walk overwrites it. Effects like `SlideIn` use this to capture
    /// the outgoing content so they can push it out as new content
    /// slides in. Called once per render, at the start of the pass.
    /// Effects still in their `delay` window are skipped — they haven't
    /// conceptually started yet.
    pub fn capture_before_all(&mut self, buf: &Buffer) {
        let now = Instant::now();
        for e in self.active.iter_mut() {
            if now < e.started + e.delay {
                continue;
            }
            e.effect.capture_before(buf, e.area);
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
    }

    pub fn is_active(&self) -> bool {
        self.active
            .iter()
            .any(|e| e.status == EffectStatus::Running)
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
        let area = Rect::new(0, 0, 2, 2);
        let mut runner = AnimationRunner::new();
        runner.start(
            area,
            AnimationKind::SlideIn {
                from: Edge::Bottom,
                duration: Duration::from_millis(100),
                delay: Duration::ZERO,
            },
        );
        let d1 = runner.next_deadline().unwrap();
        runner.start(
            area,
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
