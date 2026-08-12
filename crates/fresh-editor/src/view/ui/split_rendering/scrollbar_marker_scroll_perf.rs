//! How much the scrollbar's heading marks cost on a first scroll through a
//! large markdown buffer.
//!
//! This isolates a real inefficiency — the marks are re-projected on every
//! frame of a first scroll, so the cost is quadratic in document length — and
//! measures it. It is *not* the explanation for a first scroll feeling slow:
//! the end-to-end attribution in `e2e::markdown_compose_scroll_perf` shows the
//! per-line decoration rebuild dominating by orders of magnitude. Keep this as
//! the bound on what the marks can cost as documents (and heading counts) grow.
//!
//! # What the production path does
//!
//! `markdown_compose` marks every heading on the scrollbar track. It learns
//! about lines through `lines_changed`, which only ever carries lines the
//! editor has not offered before (`Window::seen_byte_ranges`, see
//! `app::render`), so on a *first* downward scroll every frame delivers a
//! fresh batch and the plugin answers with `setScrollbarMarkersInRange` for
//! that batch's byte span — see `HEADING_MARKER_NS` in
//! `plugins/markdown_compose.ts`. Scrolling back over the same lines delivers
//! nothing, the plugin publishes nothing, and the projection stays cached.
//! That asymmetry is exactly the "slow the first time" shape of the report.
//!
//! The two costs the loop pays per frame are:
//!
//! * [`ScrollbarMarkerManager::set_markers_in_range`] — an O(M) retain over
//!   the namespace, where M is every heading found *so far*.
//! * [`project_scrollbar_markers`] — publication bumps the marker version, so
//!   the [`ProjectionKey`](crate::view::scrollbar_marker::ProjectionKey)
//!   changes on every frame of the scroll and the cached column is always a
//!   miss. The rebuild resolves all M anchors and, on the logical-line basis a
//!   markdown file of this size uses, does an O(log n) `get_line_number`
//!   descent per anchor.
//!
//! Both terms are proportional to markers accumulated so far, and the scroll
//! runs one per frame, so the first pass costs O(frames × headings) — the
//! per-frame price grows the further down the document the reader gets, and
//! the total is quadratic in document length.
//!
//! These tests are measurements, so they assert on *shape* (late frames versus
//! early frames, first pass versus second pass) rather than on wall-clock
//! numbers, which vary by machine and by build profile. For scale, a release
//! build on the machine this was written on scrolled a 20 000-line document
//! with 1 000 headings in ~100 ms of marker work spread over 500 frames — 23 µs
//! on the first frames, 400 µs on the last. Small next to the rest of a compose
//! frame today; the point is that it grows with the square of the document.

use std::sync::Arc;
use std::time::{Duration, Instant};

use super::scrollbar::project_scrollbar_markers;
use crate::state::EditorState;
use crate::view::scrollbar_marker::{MarkerBasis, ResolvedMarker};

const NS: &str = "md-headings";
/// Rows of terminal the reader scrolls by, and the marker track's height.
const VIEWPORT_LINES: usize = 40;
const TRACK_HEIGHT: usize = 38;

/// A markdown-shaped buffer: `lines` lines with an ATX heading every
/// `heading_every` lines. Returns the state and the byte offset of every line
/// start, which stands in for what `lines_changed` hands the plugin.
fn markdown_state(lines: usize, heading_every: usize) -> (EditorState, Vec<usize>) {
    let fs: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
        Arc::new(crate::model::filesystem::StdFileSystem);
    let mut state = EditorState::new(
        80,
        VIEWPORT_LINES as u16,
        crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
        fs,
    );

    let mut text = String::new();
    let mut line_starts = Vec::with_capacity(lines);
    for i in 0..lines {
        line_starts.push(text.len());
        if i % heading_every == 0 {
            text.push_str(&format!("## Section {i}\n"));
        } else {
            text.push_str("body text for this section, long enough to look like prose\n");
        }
    }
    state.buffer.insert(0, &text);
    // A file just opened from disk: no unsaved diff, so the editor's own marks
    // stay off the track and the measurement is purely the plugin's.
    state.buffer.mark_saved_snapshot();

    (state, line_starts)
}

fn heading_marker(start: usize) -> ResolvedMarker {
    ResolvedMarker {
        start,
        end: None,
        color: fresh_core::api::OverlayColorSpec::Rgb(200, 120, 0),
        priority: 9,
    }
}

/// One frame of a first-time downward scroll: publish the batch's headings the
/// way the plugin does, then project the track the way the renderer does.
/// Returns `(publish, project)` timings.
fn scroll_frame(
    state: &mut EditorState,
    line_starts: &[usize],
    heading_every: usize,
    frame: usize,
    basis: MarkerBasis,
    publish: bool,
) -> (Duration, Duration) {
    let first = frame * VIEWPORT_LINES;
    let last = (first + VIEWPORT_LINES - 1).min(line_starts.len() - 1);

    let mut publish_time = Duration::ZERO;
    if publish {
        let start_byte = line_starts[first];
        let end_byte = line_starts
            .get(last + 1)
            .copied()
            .unwrap_or_else(|| state.buffer.len());
        let markers: Vec<ResolvedMarker> = (first..=last)
            .filter(|l| l % heading_every == 0)
            .map(|l| heading_marker(line_starts[l]))
            .collect();

        let t = Instant::now();
        state.scrollbar_markers.set_markers_in_range(
            NS,
            start_byte,
            end_byte.max(start_byte + 1),
            markers,
        );
        publish_time = t.elapsed();
    }

    let t = Instant::now();
    let _ = project_scrollbar_markers(state, basis, TRACK_HEIGHT);
    (publish_time, t.elapsed())
}

struct ScrollProfile {
    per_frame: Vec<Duration>,
    publish_total: Duration,
    project_total: Duration,
}

impl ScrollProfile {
    fn total(&self) -> Duration {
        self.publish_total + self.project_total
    }

    /// Mean frame cost over the first / last tenth of the scroll.
    fn first_decile(&self) -> Duration {
        mean(&self.per_frame[..self.per_frame.len() / 10])
    }

    fn last_decile(&self) -> Duration {
        mean(&self.per_frame[self.per_frame.len() - self.per_frame.len() / 10..])
    }
}

fn mean(d: &[Duration]) -> Duration {
    d.iter().sum::<Duration>() / d.len().max(1) as u32
}

fn scroll_document(
    state: &mut EditorState,
    line_starts: &[usize],
    heading_every: usize,
    publish: bool,
) -> ScrollProfile {
    let basis = MarkerBasis::LogicalLines {
        total: line_starts.len() as u64,
    };
    let frames = line_starts.len() / VIEWPORT_LINES;
    let mut profile = ScrollProfile {
        per_frame: Vec::with_capacity(frames),
        publish_total: Duration::ZERO,
        project_total: Duration::ZERO,
    };
    for frame in 0..frames {
        let (p, q) = scroll_frame(state, line_starts, heading_every, frame, basis, publish);
        profile.publish_total += p;
        profile.project_total += q;
        profile.per_frame.push(p + q);
    }
    profile
}

/// The report, reproduced: on a first pass through the document each frame
/// costs more than the last, because both the publish and the projection are
/// O(headings seen so far) and run once per frame.
#[test]
fn first_scroll_frame_cost_grows_with_accumulated_markers() {
    let heading_every = 20;
    let (mut state, line_starts) = markdown_state(20_000, heading_every);

    let first_pass = scroll_document(&mut state, &line_starts, heading_every, true);

    eprintln!(
        "first scroll: total {:?} (publish {:?}, project {:?}), \
         first-decile frame {:?}, last-decile frame {:?}",
        first_pass.total(),
        first_pass.publish_total,
        first_pass.project_total,
        first_pass.first_decile(),
        first_pass.last_decile(),
    );

    assert!(
        first_pass.last_decile() > first_pass.first_decile() * 4,
        "late frames should cost far more than early ones if the per-frame \
         work is proportional to markers accumulated so far; \
         first decile {:?}, last decile {:?}",
        first_pass.first_decile(),
        first_pass.last_decile(),
    );
}

/// The other half of the report: the *second* pass is nearly free. Nothing
/// republishes (the editor never re-offers a line it has already offered), so
/// the marker version stands still and every projection is a cache hit.
#[test]
fn second_scroll_is_free_because_nothing_republishes() {
    let heading_every = 20;
    let (mut state, line_starts) = markdown_state(20_000, heading_every);

    let first_pass = scroll_document(&mut state, &line_starts, heading_every, true);
    let second_pass = scroll_document(&mut state, &line_starts, heading_every, false);

    eprintln!(
        "first scroll {:?}, second scroll {:?}",
        first_pass.total(),
        second_pass.total()
    );

    assert!(
        second_pass.total() * 10 < first_pass.total(),
        "a re-scroll should be at least an order of magnitude cheaper; \
         first {:?}, second {:?}",
        first_pass.total(),
        second_pass.total()
    );
}

/// The driver is the *accumulated marker count*, not the publish call itself.
/// A document of the same length with no headings at all publishes an empty
/// batch on every frame — the plugin emits marks for every batch, so the
/// version still moves and the projection is still a miss every frame — and
/// the scroll stays flat and cheap.
#[test]
fn a_document_without_headings_scrolls_flat() {
    // `heading_every > lines` produces a document with a single heading on
    // line 0 and nothing else, so the marker set never grows.
    let (mut state, line_starts) = markdown_state(20_000, usize::MAX);

    let flat = scroll_document(&mut state, &line_starts, usize::MAX, true);

    eprintln!(
        "headingless scroll: total {:?}, first-decile frame {:?}, \
         last-decile frame {:?}",
        flat.total(),
        flat.first_decile(),
        flat.last_decile(),
    );

    assert!(
        flat.last_decile() < flat.first_decile() * 4,
        "with no markers accumulating, late frames should cost about what \
         early ones do; first decile {:?}, last decile {:?}",
        flat.first_decile(),
        flat.last_decile(),
    );

    let heading_every = 20;
    let (mut state, line_starts) = markdown_state(20_000, heading_every);
    let with_headings = scroll_document(&mut state, &line_starts, heading_every, true);
    assert!(
        with_headings.total() > flat.total() * 5,
        "the same scroll over the same number of lines costs far more once \
         headings accumulate marks; headingless {:?}, with headings {:?}",
        flat.total(),
        with_headings.total(),
    );
}

/// Doubling the document length more than doubles the first-scroll cost: the
/// frame count and the marker count both scale with it, and they multiply.
#[test]
fn first_scroll_cost_is_superlinear_in_document_length() {
    let heading_every = 20;

    let cost = |lines: usize| {
        let (mut state, line_starts) = markdown_state(lines, heading_every);
        scroll_document(&mut state, &line_starts, heading_every, true).total()
    };

    let small = cost(10_000);
    let large = cost(20_000);

    eprintln!("10k lines {small:?}, 20k lines {large:?}");

    assert!(
        large > small * 2,
        "twice the document should cost more than twice as much; \
         10k {small:?}, 20k {large:?}"
    );
}
