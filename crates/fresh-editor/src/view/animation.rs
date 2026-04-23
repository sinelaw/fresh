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
    fn apply(&mut self, buf: &mut Buffer, area: Rect, elapsed: Duration) -> EffectStatus;
}

/// Ease-out cubic: starts fast, decelerates.
fn ease_out_cubic(t: f32) -> f32 {
    let t = t.clamp(0.0, 1.0);
    let inv = 1.0 - t;
    1.0 - inv * inv * inv
}

/// Slide-in effect: snapshots the cells in `area` on first apply, then paints
/// a shifted version converging to the snapshot. Cells vacated by the shift
/// are filled with the default cell (blank) so prior buffer contents don't
/// bleed through.
pub struct SlideIn {
    from: Edge,
    duration: Duration,
    snapshot: Option<SlideSnapshot>,
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
            snapshot: None,
        }
    }

    fn take_snapshot(&mut self, buf: &Buffer, area: Rect) {
        let mut cells = Vec::with_capacity(area.width as usize * area.height as usize);
        for dy in 0..area.height {
            for dx in 0..area.width {
                let x = area.x + dx;
                let y = area.y + dy;
                let cell = buf.cell((x, y)).cloned().unwrap_or_default();
                cells.push(cell);
            }
        }
        self.snapshot = Some(SlideSnapshot { area, cells });
    }
}

impl FrameEffect for SlideIn {
    fn apply(&mut self, buf: &mut Buffer, area: Rect, elapsed: Duration) -> EffectStatus {
        if self.snapshot.is_none() {
            self.take_snapshot(buf, area);
        }
        let snap = match &self.snapshot {
            Some(s) if s.area == area => s,
            Some(_) => {
                // Area changed mid-animation (resize) — re-snapshot at new size.
                self.take_snapshot(buf, area);
                self.snapshot.as_ref().unwrap()
            }
            None => unreachable!(),
        };

        let t = if self.duration.is_zero() {
            1.0
        } else {
            (elapsed.as_secs_f32() / self.duration.as_secs_f32()).clamp(0.0, 1.0)
        };
        let eased = ease_out_cubic(t);

        let (offset_row, offset_col) = match self.from {
            Edge::Bottom => (((1.0 - eased) * area.height as f32).round() as i32, 0i32),
            Edge::Top => (-(((1.0 - eased) * area.height as f32).round() as i32), 0),
            Edge::Right => (0, ((1.0 - eased) * area.width as f32).round() as i32),
            Edge::Left => (0, -(((1.0 - eased) * area.width as f32).round() as i32)),
        };

        let blank = Cell::default();
        for dy in 0..area.height {
            for dx in 0..area.width {
                let x = area.x + dx;
                let y = area.y + dy;
                let src_dy = dy as i32 - offset_row;
                let src_dx = dx as i32 - offset_col;
                let new_cell = if src_dy >= 0
                    && src_dy < area.height as i32
                    && src_dx >= 0
                    && src_dx < area.width as i32
                {
                    let idx = src_dy as usize * area.width as usize + src_dx as usize;
                    snap.cells[idx].clone()
                } else {
                    blank.clone()
                };
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
        id
    }

    pub fn cancel(&mut self, id: AnimationId) {
        self.active.retain(|e| e.id != id);
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
