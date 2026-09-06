//! How much the scrollbar's heading marks cost on a first scroll through a
//! large markdown buffer.
//!
//! This isolates a real inefficiency — the marks are re-projected on every
//! frame of a first scroll, so the cost is quadratic in document length — and
//! measures it. It was *not* the explanation for a first scroll feeling slow:
//! that was the wrap index rebuilding the whole document per frame, since
//! fixed by diff repair (see `e2e::markdown_compose_first_scroll_relayout`).
//! Keep this as the bound on what the marks can cost as documents (and
//! heading counts) grow.
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
//! * [`resolve_scrollbar_marks`] — publication bumps the marker version, so
//!   the [`ProjectionKey`](crate::view::scrollbar_marker::ProjectionKey)
//!   changes on every frame of the scroll and the cached rows are always a
//!   miss. The rebuild resolves all M anchors and, on the logical-line basis a
//!   markdown file of this size uses, does an O(log n) `get_line_number`
//!   descent per anchor.
//!
//! Both terms are proportional to markers accumulated so far, and the scroll
//! runs one per frame, so the first pass costs O(frames × headings) — the
//! per-frame price grows the further down the document the reader gets, and
//! the total is quadratic in document length.
//!
//! # Why these count instead of timing
//!
//! These tests are measurements, but what they measure is *counted work*, not
//! wall-clock: markers the publish sweeps, markers the projection re-walks,
//! projections rebuilt. Both O(M) terms are visible as counts — the retain
//! sweeps the namespace it starts from, and
//! [`ScrollbarMarkerBuckets::stats`](crate::view::scrollbar_marker::ScrollbarMarkerBuckets::stats)
//! reports every marker a rebuild walked — and a count is an exact function of
//! the document and the scroll, so it says the same thing on any machine, in
//! any build profile, and on a loaded runner. Timings do not: these assertions
//! were written against `Instant::elapsed` first, and the headingless case
//! failed under `cargo nextest` on nothing but scheduler noise (a first decile
//! of 11 µs against a last decile of 96 µs, both far below the resolution at
//! which a few microseconds of preemption mean anything).
//!
//! For scale, a release build on the machine this was written on scrolled a
//! 20 000-line document with 1 000 headings in ~100 ms of marker work spread
//! over 500 frames — 23 µs on the first frames, 400 µs on the last. Small next
//! to the rest of a compose frame today; the point is that it grows with the
//! square of the document.

use std::sync::Arc;

use super::scrollbar::resolve_scrollbar_marks;
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

/// The markers one frame of the scroll had to touch.
#[derive(Debug, Clone, Copy, Default)]
struct FrameWork {
    /// Size of the namespace the publish's retain swept — the O(M) half of
    /// `set_markers_in_range`. Zero on a frame that publishes nothing.
    swept: u64,
    /// Markers the projection re-walked, straight from
    /// [`ProjectionStats`](crate::view::scrollbar_marker::ProjectionStats).
    /// Zero on a frame the cached column served.
    walked: u64,
    /// 1 when the frame missed the projection cache, 0 when it hit.
    rebuilds: u64,
}

impl FrameWork {
    fn markers(&self) -> u64 {
        self.swept + self.walked
    }
}

/// One frame of a first-time downward scroll: publish the batch's headings the
/// way the plugin does, then project the track the way the renderer does.
fn scroll_frame(
    state: &mut EditorState,
    line_starts: &[usize],
    heading_every: usize,
    frame: usize,
    basis: MarkerBasis,
    publish: bool,
) -> FrameWork {
    let first = frame * VIEWPORT_LINES;
    let last = (first + VIEWPORT_LINES - 1).min(line_starts.len() - 1);

    let mut swept = 0;
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

        // The retain walks the namespace as it stands before the batch lands,
        // so that count *is* what this publish costs.
        swept = state.scrollbar_markers.len() as u64;
        state.scrollbar_markers.set_markers_in_range(
            NS,
            start_byte,
            end_byte.max(start_byte + 1),
            markers,
        );
    }

    let before = state.scrollbar_marker_buckets.stats();
    let rows = resolve_scrollbar_marks(state, basis);
    let _ = crate::view::scrollbar_marker::bucket(&rows, basis.total(), TRACK_HEIGHT);
    let after = state.scrollbar_marker_buckets.stats();

    FrameWork {
        swept,
        walked: after.markers_walked - before.markers_walked,
        rebuilds: after.rebuilds - before.rebuilds,
    }
}

struct ScrollProfile {
    per_frame: Vec<FrameWork>,
}

impl ScrollProfile {
    fn frames(&self) -> u64 {
        self.per_frame.len() as u64
    }

    /// Every marker the pass swept or walked, over all frames.
    fn total(&self) -> u64 {
        self.per_frame.iter().map(FrameWork::markers).sum()
    }

    fn rebuilds(&self) -> u64 {
        self.per_frame.iter().map(|f| f.rebuilds).sum()
    }

    /// Mean markers per frame over the first / last tenth of the scroll.
    fn first_decile(&self) -> u64 {
        mean(&self.per_frame[..self.per_frame.len() / 10])
    }

    fn last_decile(&self) -> u64 {
        mean(&self.per_frame[self.per_frame.len() - self.per_frame.len() / 10..])
    }

    /// The single most expensive frame of the pass.
    fn worst_frame(&self) -> u64 {
        self.per_frame
            .iter()
            .map(FrameWork::markers)
            .max()
            .unwrap_or(0)
    }
}

fn mean(frames: &[FrameWork]) -> u64 {
    frames.iter().map(FrameWork::markers).sum::<u64>() / frames.len().max(1) as u64
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
    };
    for frame in 0..frames {
        profile.per_frame.push(scroll_frame(
            state,
            line_starts,
            heading_every,
            frame,
            basis,
            publish,
        ));
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
        "first scroll: {} markers touched over {} frames, {} rebuilds, \
         first-decile frame {}, last-decile frame {}",
        first_pass.total(),
        first_pass.frames(),
        first_pass.rebuilds(),
        first_pass.first_decile(),
        first_pass.last_decile(),
    );

    assert_eq!(
        first_pass.rebuilds(),
        first_pass.frames(),
        "publishing on every frame moves the marker version, so no frame of a \
         first pass can hit the cached column"
    );
    assert!(
        first_pass.last_decile() > first_pass.first_decile() * 4,
        "late frames should touch far more markers than early ones if the \
         per-frame work is proportional to markers accumulated so far; \
         first decile {}, last decile {}",
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
        "first scroll {} markers touched, second scroll {}",
        first_pass.total(),
        second_pass.total()
    );

    assert_eq!(
        second_pass.rebuilds(),
        0,
        "a re-scroll changes nothing in the projection key, so every frame \
         must come off the cache"
    );
    assert_eq!(
        second_pass.total(),
        0,
        "a re-scroll neither publishes nor re-projects, so it touches no \
         markers at all; touched {}",
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
    let headings = state.scrollbar_markers.len() as u64;

    eprintln!(
        "headingless scroll: {} markers touched over {} frames for {headings} \
         heading(s), {} rebuilds, worst frame {}",
        flat.total(),
        flat.frames(),
        flat.rebuilds(),
        flat.worst_frame(),
    );

    assert_eq!(flat.rebuilds(), flat.frames(), "still a miss every frame");
    assert!(
        flat.worst_frame() <= 2 * headings,
        "with no markers accumulating, every frame sweeps and walks the same \
         lone heading however far down the document it is; worst frame {} for \
         {headings} heading(s)",
        flat.worst_frame(),
    );

    let heading_every = 20;
    let (mut state, line_starts) = markdown_state(20_000, heading_every);
    let with_headings = scroll_document(&mut state, &line_starts, heading_every, true);
    assert!(
        with_headings.total() > flat.total() * 5,
        "the same scroll over the same number of lines touches far more \
         markers once headings accumulate marks; headingless {}, with \
         headings {}",
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

    eprintln!("10k lines {small} markers touched, 20k lines {large}");

    assert!(
        large > small * 2,
        "twice the document should touch more than twice as many markers; \
         10k {small}, 20k {large}"
    );
}
