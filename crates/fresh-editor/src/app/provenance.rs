//! Cell provenance: which writer each cell of a frame came from.
//!
//! The retained-mode migration's progress is *how many cells are still written
//! by a legacy painter* rather than by the fold over the display list
//! (`docs/internal/tui-retained-mode-migration-plan.md`, Stage 0.0). Nothing
//! measured that before this module: [`ProvenanceSink`] sees every item the
//! fold paints, but a painter that runs between the two fold bands — a
//! terminal grid, the overlay prompt card, a hover highlight — is invisible
//! to it, and so is the host painter the fold itself calls for a pane.
//!
//! This recorder attributes every cell of a rendered frame to the writer that
//! **last changed it**: one of the fold's two bands, a host painter the fold
//! called, or a named legacy painter in `Editor::render`. The gate is
//! `tests/cells_provenance.rs`, which prints the table per fixture and fails
//! when the legacy share rises above its recorded baseline, or when a painted
//! cell has no writer at all.
//!
//! # Off unless installed
//!
//! The editor carries a [`Slot`] that is empty until a test installs a
//! recorder ([`Editor::record_cell_provenance`]). Every hook in `render` is an
//! `Option` check while it is empty, and nothing here writes to the buffer in
//! either state — the recorder only reads it — so the painted output is the
//! same whether a recorder is installed or not.
//!
//! # How attribution works
//!
//! By diffing. The recorder keeps a snapshot of the buffer as of the last
//! *mark*; a mark compares the live buffer against it, gives every cell that
//! differs to the writer being marked, and takes a fresh snapshot. `render`
//! brackets each legacy painter with [`Slot::begin`] and [`Slot::end`]:
//! `begin` marks whatever changed since the previous mark as
//! [`UNATTRIBUTED`], so a write that happens *between* two painters is not
//! blamed on the next one, and `end` marks the painter's own cells.
//!
//! The two `fold_band` calls are bracketed the same way, with two refinements
//! that keep the fold's number honest:
//!
//! - **The hosts it calls are marked separately.** A `Draw::Host` item hands a
//!   rectangle to the pane painter, the prompt line or an embed — legacy
//!   code, running *inside* the fold. [`RecordingHost`] wraps the host
//!   painter and marks the fold's cells before, and the host's after, each
//!   call.
//! - **The fold is only credited inside the items it reported.** A cell the
//!   fold changed is given to the band only when it lies in a rectangle the
//!   [`ProvenanceSink`] reported for that band. A `Scrim::Dim` restyles every
//!   cell behind it and reports nothing, so the cells it darkens keep the
//!   writer that put them there — which is what "who painted this" means.
//!
//! A cell is *painted* when it differs from [`Cell::EMPTY`]: a space with no
//! colours and no modifiers is blank, a space with a background is not (a
//! scrollbar track and the status bar's ground are painted with spaces).
//!
//! # Cost
//!
//! A mark is one pass over the buffer, so a frame costs one such pass per
//! bracket: around thirty on the busiest path, over a few thousand cells. It
//! is meant for a test that renders a handful of frames, not for the render
//! loop. [`Report::marks`] says how many a frame took.

use std::cell::RefCell;

use ratatui::buffer::{Buffer, Cell};
use ratatui::layout::Rect;

use fresh_ui::ThemeKey;

use crate::view::shell::fold::{Band, Caret, HostPainter, ProvenanceSink};
use crate::view::shell::frame::{HostRegion, HostTarget};

/// The writer of a painted cell that no bracket claimed.
///
/// A frame that reports any of these has a painter `render` does not name, and
/// the gate fails on it: the allowlist is only complete while this is zero.
pub const UNATTRIBUTED: &str = "<unattributed>";

/// The background band of the fold: the in-flow half of the display list.
pub const FOLD_BACKGROUND: &str = "fold:background";

/// The overlay band of the fold: the `Layer`s.
pub const FOLD_OVERLAY: &str = "fold:overlay";

/// Whether a cell is painted, as the gate counts it.
pub fn is_painted(cell: &Cell) -> bool {
    *cell != Cell::EMPTY
}

/// The recorder proper: a snapshot, and one writer per cell.
pub struct Recorder {
    /// The buffer as of the last mark.
    snapshot: Buffer,
    /// The writer that last changed each cell, by index into `names`.
    owners: Vec<Option<u16>>,
    names: Vec<&'static str>,
    /// The band being folded, while one is.
    band: Option<Band>,
    /// The cells inside an item the sink reported for the current band.
    fold_mask: Vec<bool>,
    /// Marks taken this frame — the cost instrument.
    marks: usize,
    /// How many times each writer's bracket closed this frame, so a painter
    /// that ran and changed nothing is still reported as having run.
    entered: Vec<usize>,
}

impl Recorder {
    fn new() -> Self {
        Recorder {
            snapshot: Buffer::empty(Rect::ZERO),
            owners: Vec::new(),
            names: Vec::new(),
            band: None,
            fold_mask: Vec::new(),
            marks: 0,
            entered: Vec::new(),
        }
    }

    /// Forget the previous frame and snapshot `buf` as the starting state.
    fn start_frame(&mut self, buf: &Buffer) {
        self.snapshot = buf.clone();
        let n = buf.content.len();
        self.owners.clear();
        self.owners.resize(n, None);
        self.fold_mask.clear();
        self.fold_mask.resize(n, false);
        self.band = None;
        self.marks = 0;
        self.entered.iter_mut().for_each(|n| *n = 0);
    }

    fn name_id(&mut self, name: &'static str) -> u16 {
        match self.names.iter().position(|n| *n == name) {
            Some(i) => i as u16,
            None => {
                self.names.push(name);
                self.entered.push(0);
                (self.names.len() - 1) as u16
            }
        }
    }

    /// A writer's bracket closed: it ran, whether or not it changed a cell.
    fn entered(&mut self, name: &'static str) {
        let id = self.name_id(name);
        self.entered[id as usize] += 1;
    }

    /// Give every cell that changed since the last mark to `name`, restricted
    /// to the fold mask when `masked`.
    fn mark_with(&mut self, name: &'static str, buf: &Buffer, masked: bool) {
        if buf.area != self.snapshot.area || buf.content.len() != self.snapshot.content.len() {
            // A frame whose size changed under the recorder: start over, so
            // the diff below is cell-for-cell. Nothing is painted before the
            // first mark of a frame in practice, so this is the safe reading.
            self.start_frame(buf);
            return;
        }
        self.marks += 1;
        let id = self.name_id(name);
        for (i, (now, then)) in buf
            .content
            .iter()
            .zip(self.snapshot.content.iter())
            .enumerate()
        {
            if now != then {
                if !masked || self.fold_mask[i] {
                    self.owners[i] = Some(id);
                }
            }
        }
        self.snapshot.content.clone_from(&buf.content);
    }

    fn mark(&mut self, name: &'static str, buf: &Buffer) {
        self.mark_with(name, buf, false);
    }

    /// Credit the current band with what the fold wrote since the last mark.
    fn mark_fold(&mut self, buf: &Buffer) {
        let name = match self.band {
            Some(Band::Background) => FOLD_BACKGROUND,
            Some(Band::Overlay) => FOLD_OVERLAY,
            // A host called outside a band: nothing to credit the fold with,
            // and whatever was written since the last mark has no writer.
            None => UNATTRIBUTED,
        };
        let masked = self.band.is_some();
        self.mark_with(name, buf, masked);
    }

    fn begin_band(&mut self, band: Band, buf: &Buffer) {
        self.mark(UNATTRIBUTED, buf);
        self.band = Some(band);
        self.fold_mask.iter_mut().for_each(|m| *m = false);
    }

    fn end_band(&mut self, buf: &Buffer) {
        self.mark_fold(buf);
        if let Some(band) = self.band.take() {
            self.entered(match band {
                Band::Background => FOLD_BACKGROUND,
                Band::Overlay => FOLD_OVERLAY,
            });
        }
    }

    /// An item the fold reported: its visible cells are the band's to claim.
    fn fold_item(&mut self, visible: Rect) {
        let area = self.snapshot.area;
        let vis = visible.intersection(area);
        for y in vis.y..vis.y.saturating_add(vis.height) {
            for x in vis.x..vis.x.saturating_add(vis.width) {
                let i = self.snapshot.index_of(x, y);
                if let Some(m) = self.fold_mask.get_mut(i) {
                    *m = true;
                }
            }
        }
    }

    fn report(&self) -> Report {
        let mut counts: Vec<usize> = vec![0; self.names.len()];
        let mut unattributed = 0usize;
        let mut painted = 0usize;
        for (cell, owner) in self.snapshot.content.iter().zip(self.owners.iter()) {
            if !is_painted(cell) {
                continue;
            }
            painted += 1;
            match owner {
                Some(id) => counts[*id as usize] += 1,
                None => unattributed += 1,
            }
        }
        let mut writers: Vec<(String, usize)> = self
            .names
            .iter()
            .zip(counts)
            .filter(|(_, n)| *n > 0)
            .map(|(name, n)| (name.to_string(), n))
            .collect();
        // A cell nobody marked and one marked as unattributed are the same
        // finding: a painted cell with no named writer.
        if unattributed > 0 {
            match writers.iter_mut().find(|(n, _)| n == UNATTRIBUTED) {
                Some(w) => w.1 += unattributed,
                None => writers.push((UNATTRIBUTED.to_string(), unattributed)),
            }
        }
        writers.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
        let ran = self
            .names
            .iter()
            .zip(self.entered.iter())
            .filter(|(_, n)| **n > 0)
            .map(|(name, n)| (name.to_string(), *n))
            .collect();
        Report {
            area: self.snapshot.content.len(),
            painted,
            writers,
            ran,
            marks: self.marks,
        }
    }
}

/// What a frame's cells came from.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Report {
    /// Cells in the frame.
    pub area: usize,
    /// Cells that are not blank.
    pub painted: usize,
    /// Painted cells per writer, most first. Every painted cell is in exactly
    /// one entry; [`UNATTRIBUTED`] is an entry like any other.
    pub writers: Vec<(String, usize)>,
    /// How many times each writer's bracket closed this frame — the painters
    /// that *ran*, including those that changed no cell and so are absent
    /// from `writers`.
    pub ran: Vec<(String, usize)>,
    /// Buffer passes the recorder took for this frame.
    pub marks: usize,
}

impl Report {
    /// Painted cells `name` wrote.
    pub fn count(&self, name: &str) -> usize {
        self.writers
            .iter()
            .find(|(n, _)| n == name)
            .map_or(0, |(_, c)| *c)
    }

    /// Painted cells the fold wrote, both bands.
    pub fn fold(&self) -> usize {
        self.count(FOLD_BACKGROUND) + self.count(FOLD_OVERLAY)
    }

    /// Painted cells a legacy painter wrote — everything that is not the
    /// fold's, the unattributed included.
    pub fn legacy(&self) -> usize {
        self.painted - self.fold()
    }

    /// Painted cells with no named writer.
    pub fn unattributed(&self) -> usize {
        self.count(UNATTRIBUTED)
    }

    /// Whether `name`'s bracket closed at least once this frame.
    pub fn ran(&self, name: &str) -> bool {
        self.ran.iter().any(|(n, _)| n == name)
    }
}

/// The editor's hold on a recorder: empty until a test installs one.
#[derive(Default)]
pub struct Slot(Option<Recorder>);

impl Slot {
    /// Start recording. The next `render` is the first frame reported.
    pub fn install(&mut self) {
        self.0 = Some(Recorder::new());
    }

    pub fn is_installed(&self) -> bool {
        self.0.is_some()
    }

    /// Called at the top of `render`, before anything paints.
    pub fn start_frame(&mut self, buf: &Buffer) {
        if let Some(r) = self.0.as_mut() {
            r.start_frame(buf);
        }
    }

    /// Open a painter's bracket. Whatever changed since the last mark is
    /// nobody's.
    pub fn begin(&mut self, buf: &Buffer) {
        if let Some(r) = self.0.as_mut() {
            r.mark(UNATTRIBUTED, buf);
        }
    }

    /// Close a painter's bracket: what changed inside it is `name`'s.
    pub fn end(&mut self, name: &'static str, buf: &Buffer) {
        if let Some(r) = self.0.as_mut() {
            r.mark(name, buf);
            r.entered(name);
        }
    }

    /// Called at the bottom of `render`: the frame is complete, and anything
    /// written since the last bracket is nobody's.
    pub fn finish(&mut self, buf: &Buffer) {
        if let Some(r) = self.0.as_mut() {
            r.mark(UNATTRIBUTED, buf);
        }
    }

    /// Lend the recorder to a fold, whose host painter borrows the editor.
    pub fn detach(&mut self) -> Shared {
        Shared(self.0.take().map(RefCell::new))
    }

    /// Take it back once the fold has returned.
    pub fn attach(&mut self, shared: Shared) {
        if let Some(r) = shared.0 {
            self.0 = Some(r.into_inner());
        }
    }

    /// The last completed frame's attribution.
    pub fn report(&self) -> Option<Report> {
        self.0.as_ref().map(Recorder::report)
    }
}

/// A recorder lent to a fold: shared between the host wrapper and the sink
/// wrapper, which the fold drives in alternation and never at once.
pub struct Shared(Option<RefCell<Recorder>>);

impl Shared {
    /// Open a band's bracket.
    pub fn begin_band(&self, band: Band, buf: &Buffer) {
        if let Some(r) = &self.0 {
            r.borrow_mut().begin_band(band, buf);
        }
    }

    /// Close it: what the fold changed inside its reported items is the
    /// band's.
    pub fn end_band(&self, buf: &Buffer) {
        if let Some(r) = &self.0 {
            r.borrow_mut().end_band(buf);
        }
    }

    /// Wrap the band's host painter so each host's cells are its own.
    pub fn host<'a, H: HostPainter>(&'a self, inner: &'a mut H) -> RecordingHost<'a, H> {
        RecordingHost {
            inner,
            rec: self.0.as_ref(),
        }
    }

    /// Wrap the band's sink so the fold's items are known to the recorder.
    pub fn sink<'a, S: ProvenanceSink>(&'a self, inner: &'a mut S) -> RecordingSink<'a, S> {
        RecordingSink {
            inner,
            rec: self.0.as_ref(),
        }
    }
}

/// A [`HostPainter`] whose every call is bracketed.
pub struct RecordingHost<'a, H: HostPainter> {
    inner: &'a mut H,
    rec: Option<&'a RefCell<Recorder>>,
}

/// The writer name a host target paints under.
pub fn host_name(target: HostTarget) -> &'static str {
    match target {
        HostTarget::Pane(_) => "host:pane",
        HostTarget::Embed(_) => "host:embed",
        HostTarget::Card(_) => "host:card",
        HostTarget::Region(r) => match r {
            HostRegion::Dock => "host:dock",
            HostRegion::MenuBar => "host:menu_bar",
            HostRegion::Explorer => "host:explorer",
            HostRegion::Body => "host:body",
            HostRegion::StatusBar => "host:status_bar",
            HostRegion::SearchOptions => "host:search_options",
            HostRegion::PromptLine => "host:prompt_line",
        },
    }
}

impl<H: HostPainter> HostPainter for RecordingHost<'_, H> {
    fn paint_host(&mut self, target: HostTarget, rect: Rect, buf: &mut Buffer, caret: &mut Caret) {
        if let Some(r) = self.rec {
            r.borrow_mut().mark_fold(buf);
        }
        self.inner.paint_host(target, rect, buf, caret);
        if let Some(r) = self.rec {
            let mut r = r.borrow_mut();
            r.mark(host_name(target), buf);
            r.entered(host_name(target));
        }
    }
}

/// A [`ProvenanceSink`] that also tells the recorder where the fold painted.
pub struct RecordingSink<'a, S: ProvenanceSink> {
    inner: &'a mut S,
    rec: Option<&'a RefCell<Recorder>>,
}

impl<S: ProvenanceSink> ProvenanceSink for RecordingSink<'_, S> {
    fn item(&mut self, rect: Rect, clip: Rect, theme: &ThemeKey) {
        self.inner.item(rect, clip, theme);
        if let Some(r) = self.rec {
            r.borrow_mut().fold_item(rect.intersection(clip));
        }
    }
}

impl super::Editor {
    /// Record, from the next frame on, which writer each painted cell came
    /// from. For the provenance gate; see the module doc.
    pub fn record_cell_provenance(&mut self) {
        self.cell_provenance.install();
    }

    /// The attribution of the last frame rendered since
    /// [`record_cell_provenance`](Self::record_cell_provenance), if any.
    pub fn cell_provenance(&self) -> Option<Report> {
        self.cell_provenance.report()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::style::{Color, Style};

    fn buf(w: u16, h: u16) -> Buffer {
        Buffer::empty(Rect::new(0, 0, w, h))
    }

    fn paint(b: &mut Buffer, r: Rect, ch: &str) {
        for y in r.y..r.y + r.height {
            for x in r.x..r.x + r.width {
                b[(x, y)].set_symbol(ch);
            }
        }
    }

    #[test]
    fn the_last_writer_that_changed_a_cell_owns_it() {
        let mut slot = Slot::default();
        slot.install();
        let mut b = buf(10, 2);
        slot.start_frame(&b);

        slot.begin(&b);
        paint(&mut b, Rect::new(0, 0, 10, 1), "a");
        slot.end("first", &b);

        slot.begin(&b);
        paint(&mut b, Rect::new(5, 0, 5, 2), "b");
        slot.end("second", &b);

        slot.begin(&b);
        slot.end("idle", &b);

        slot.finish(&b);
        let r = slot.report().unwrap();
        assert_eq!(r.painted, 15);
        assert_eq!(r.count("first"), 5);
        assert_eq!(r.count("second"), 10);
        assert_eq!(r.unattributed(), 0);
        assert_eq!(r.marks, 7);
        assert!(r.ran("idle") && r.count("idle") == 0, "{r:?}");
        assert_eq!(r.ran.len(), 3);
    }

    #[test]
    fn a_write_between_brackets_is_nobodys() {
        let mut slot = Slot::default();
        slot.install();
        let mut b = buf(4, 1);
        slot.start_frame(&b);
        paint(&mut b, Rect::new(0, 0, 2, 1), "x");
        slot.begin(&b);
        paint(&mut b, Rect::new(2, 0, 1, 1), "y");
        slot.end("named", &b);
        paint(&mut b, Rect::new(3, 0, 1, 1), "z");
        slot.finish(&b);
        let r = slot.report().unwrap();
        assert_eq!(r.count("named"), 1);
        assert_eq!(r.unattributed(), 3);
        assert_eq!(r.legacy(), 4);
    }

    #[test]
    fn a_blank_cell_is_not_counted_but_a_coloured_space_is() {
        let mut slot = Slot::default();
        slot.install();
        let mut b = buf(3, 1);
        slot.start_frame(&b);
        slot.begin(&b);
        b[(0, 0)].set_style(Style::default().bg(Color::Red));
        b[(1, 0)].set_symbol("q");
        slot.end("p", &b);
        slot.finish(&b);
        let r = slot.report().unwrap();
        assert_eq!(r.painted, 2);
        assert_eq!(r.count("p"), 2);
    }

    /// The fold is credited only inside the items it reported, and the hosts
    /// it calls are marked as their own.
    #[test]
    fn a_band_is_credited_inside_its_items_and_hosts_are_their_own() {
        struct Nobody;
        impl ProvenanceSink for Nobody {
            fn item(&mut self, _: Rect, _: Rect, _: &ThemeKey) {}
        }
        struct Pane;
        impl HostPainter for Pane {
            fn paint_host(&mut self, _: HostTarget, rect: Rect, buf: &mut Buffer, _: &mut Caret) {
                paint(buf, rect, "p");
            }
        }

        let mut slot = Slot::default();
        slot.install();
        let mut b = buf(10, 1);
        slot.start_frame(&b);

        // A painter first, so the scrim below has something to darken.
        slot.begin(&b);
        paint(&mut b, Rect::new(6, 0, 4, 1), "l");
        slot.end("legacy", &b);

        let shared = slot.detach();
        shared.begin_band(Band::Overlay, &b);
        {
            let mut nobody = Nobody;
            let mut sink = shared.sink(&mut nobody);
            // A reported item: two cells.
            sink.item(
                Rect::new(0, 0, 2, 1),
                Rect::new(0, 0, 10, 1),
                &ThemeKey(Some("t".into())),
            );
            paint(&mut b, Rect::new(0, 0, 2, 1), "f");
            // A host inside the band.
            let mut pane = Pane;
            let mut host = shared.host(&mut pane);
            let mut caret = None;
            host.paint_host(
                HostTarget::Region(HostRegion::Body),
                Rect::new(2, 0, 3, 1),
                &mut b,
                &mut caret,
            );
            // A scrim: restyles the legacy cells, reports nothing.
            for x in 6..10 {
                b[(x, 0)].set_style(Style::default().fg(Color::DarkGray));
            }
        }
        shared.end_band(&b);
        slot.attach(shared);
        slot.finish(&b);

        let r = slot.report().unwrap();
        assert_eq!(r.count(FOLD_OVERLAY), 2, "{r:?}");
        assert_eq!(r.count("host:body"), 3, "{r:?}");
        assert_eq!(
            r.count("legacy"),
            4,
            "the dimmed cells keep their writer: {r:?}"
        );
        assert_eq!(r.unattributed(), 0, "{r:?}");
        assert_eq!(r.painted, 9);
    }

    #[test]
    fn an_uninstalled_slot_reports_nothing_and_costs_nothing() {
        let mut slot = Slot::default();
        let b = buf(4, 4);
        slot.start_frame(&b);
        slot.begin(&b);
        slot.end("x", &b);
        slot.finish(&b);
        assert!(slot.report().is_none());
        let shared = slot.detach();
        shared.begin_band(Band::Background, &b);
        shared.end_band(&b);
        slot.attach(shared);
        assert!(!slot.is_installed());
    }
}
