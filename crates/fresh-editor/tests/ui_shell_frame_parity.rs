//! PoC for the **outside-in** migration shell (see
//! `docs/internal/fresh-editor-ui-migration.md`).
//!
//! The proposal: make the frame a `fresh-ui` tree on day one, with every
//! region below it a `Host` leaf that the backend fold paints using today's
//! painters — then replace regions with native descriptions one at a time.
//!
//! Everything in that plan rests on one testable claim: **`fresh-ui` can
//! reproduce the editor's frame rectangles exactly.** If it cannot, the shell
//! moves every surface by one cell and the cell-identical acceptance bar is
//! lost at step one.
//!
//! This test builds the frame skeleton both ways — as a `fresh-ui` description
//! folded to `Draw::Host` items, and as the ratatui `Layout` calls that
//! `render.rs` used to make — and compares the resulting rectangles over a
//! sweep of terminal sizes and visibility combinations.
//!
//! S1b deleted the production copy of that second computation, so `reference()`
//! below is now the only one left: this is a golden of the layout the editor
//! had, not a live cross-check against one it still runs. That is still worth
//! keeping — it is what stops the shell's layout drifting from the behaviour
//! users have — but it is a pin, not an oracle.
//!
//! Note what is deliberately *not* compared: the dock's bail-out rules
//! (`EDITOR_MIN`/`DOCK_MIN`) are app logic keyed on the frame width, not a
//! layout constraint. `build()` cannot read geometry, so a real shell resolves
//! that width from state before building. Both sides here are handed the
//! already-resolved width, isolating the layout question.

use fresh::view::shell::frame::{region_rects, Frame, HostRegion};
use ratatui::layout::{Constraint, Direction, Layout, Rect};

fn id(r: HostRegion) -> u64 {
    r.id()
}

/// The fold: pull every `Draw::Host` item out of the display list. This is the
/// shape the real backend callback takes — `(HostId, rect)` is exactly what a
/// region painter needs.
fn fold(f: Frame, size: Rect) -> Vec<(u64, Rect)> {
    // Through `region_rects`, which is a layout query — not a scan of the
    // display list for `Draw::Host`. A region that has gone native emits no
    // such item, and one that paints nothing at all (a hidden row) emits none
    // either; both still have a rectangle, and that is what this compares.
    let mut out: Vec<(u64, Rect)> = region_rects(f, size)
        .into_iter()
        .map(|(r, rect)| (r.id(), rect))
        .collect();
    out.sort_by_key(|(id, _)| *id);
    out
}

// ---------------------------------------------------------------------------
// The reference: the ratatui Layout calls render.rs actually makes
// ---------------------------------------------------------------------------

fn reference(f: Frame, size: Rect) -> Vec<(u64, Rect)> {
    let mut out: Vec<(u64, Rect)> = Vec::new();

    // compute_dock_split: rect math, post-bail-out.
    let chrome_area = match f.dock {
        Some(w) => {
            out.push((
                id(HostRegion::Dock),
                Rect {
                    x: size.x,
                    y: size.y,
                    width: w,
                    height: size.height,
                },
            ));
            Rect {
                x: size.x.saturating_add(w),
                y: size.y,
                width: size.width.saturating_sub(w),
                height: size.height,
            }
        }
        None => {
            out.push((
                id(HostRegion::Dock),
                Rect {
                    x: size.x,
                    y: size.y,
                    width: 0,
                    height: size.height,
                },
            ));
            size
        }
    };

    // The five-row chrome column.
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints(vec![
            Constraint::Length(if f.menu_bar { 1 } else { 0 }),
            Constraint::Min(0),
            Constraint::Length(if f.status_bar { 1 } else { 0 }),
            Constraint::Length(f.search_options.is_some() as u16),
            Constraint::Length(if f.prompt_line { 1 } else { 0 }),
        ])
        .split(chrome_area);

    // Every row, whether or not it is drawn: a hidden one still has a
    // position, and S1b takes these rects verbatim.
    out.push((id(HostRegion::MenuBar), chunks[0]));
    out.push((id(HostRegion::StatusBar), chunks[2]));
    out.push((id(HostRegion::SearchOptions), chunks[3]));
    out.push((id(HostRegion::PromptLine), chunks[4]));

    // split_file_explorer_area on the content row.
    match f.explorer.as_ref().map(|e| (e.cols, e.on_left)) {
        Some((cols, on_left)) => {
            let (explorer, editor) = if on_left {
                let c = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([Constraint::Length(cols), Constraint::Min(0)])
                    .split(chunks[1]);
                (c[0], c[1])
            } else {
                let c = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([Constraint::Min(0), Constraint::Length(cols)])
                    .split(chunks[1]);
                (c[1], c[0])
            };
            out.push((id(HostRegion::Explorer), explorer));
            out.push((id(HostRegion::Body), editor));
        }
        None => {
            out.push((id(HostRegion::Body), chunks[1]));
            // No sidebar: an empty rect where one would begin.
            out.push((
                id(HostRegion::Explorer),
                Rect {
                    x: chunks[1].x + chunks[1].width,
                    y: chunks[1].y,
                    width: 0,
                    height: chunks[1].height,
                },
            ));
        }
    }

    out.sort_by_key(|(id, _)| *id);
    out
}

// ---------------------------------------------------------------------------

fn combos() -> Vec<Frame> {
    let mut v = Vec::new();
    for &menu in &[true, false] {
        for &status in &[true, false] {
            for &(search, prompt) in &[(false, false), (true, false), (false, true), (true, true)] {
                for &dock in &[None, Some(24u16)] {
                    for explorer in [None, Some((20u16, true)), Some((20u16, false))] {
                        v.push(Frame {
                            menu_bar: menu,
                            status_bar: status,
                            // Content, not geometry, like the bar's labels:
                            // the row occupies its one cell whatever it says.
                            search_options: search.then(Default::default),
                            prompt_line: prompt,
                            dock,
                            // Content, not geometry: the panel occupies its
                            // column whatever it holds.
                            explorer: explorer.map(|(cols, on_left)| {
                                fresh::view::shell::file_explorer::Explorer {
                                    cols,
                                    on_left,
                                    ..Default::default()
                                }
                            }),
                            // The frame layout is the same with or without an
                            // overlay: a menu is a layer, out of flow.
                            menu: None,
                            dropdowns: Vec::new(),
                            // Content, not geometry: an empty bar row occupies
                            // the same cells a full one does.
                            menu_bar_items: Default::default(),
                        });
                    }
                }
            }
        }
    }
    v
}

/// Rows whose height is fixed at 1 when visible.
/// Report divergences rather than asserting on the first, so the *shape* of any
/// disagreement is visible at once. Only sizes where the fixed rows actually
/// fit are compared; the overflow band is pinned separately below.
fn sweep(sizes: &[(u16, u16)]) -> Vec<String> {
    let mut bad = Vec::new();
    for raw in combos() {
        for &(w, h) in sizes {
            let f = raw.clone().resolve_dock(w);
            if h < f.fixed_rows() {
                continue; // the squeeze band — see the test below
            }
            let size = Rect {
                x: 0,
                y: 0,
                width: w,
                height: h,
            };
            let got = fold(f.clone(), size);
            let want = reference(f.clone(), size);
            if got != want {
                bad.push(format!(
                    "{w}x{h} {f:?}\n     fresh-ui: {got:?}\n     ratatui : {want:?}"
                ));
            }
        }
    }
    bad
}

/// **The load-bearing claim.** Whenever the visible fixed rows fit in the
/// frame, the `fresh-ui` shell reproduces every region rectangle the editor's
/// ratatui `Layout` calls produce — exactly, including the dock and sidebar
/// carves. This is what makes an outside-in migration cell-identical: the
/// frame moves onto `fresh-ui` without a single region shifting.
#[test]
fn frame_rects_match_ratatui_wherever_the_fixed_rows_fit() {
    let mut sizes = Vec::new();
    // A curated grid rather than a dense one: every interesting width band
    // (below EDITOR_MIN, in the dock's shrink band, ordinary, wide) crossed
    // with the heights where rows start to squeeze, plus roomy ones.
    for w in [1u16, 10, 20, 24, 30, 50, 80, 120, 200] {
        for h in [1u16, 2, 3, 4, 5, 6, 8, 12, 24, 40, 60] {
            sizes.push((w, h));
        }
    }
    let bad = sweep(&sizes);
    assert!(
        bad.is_empty(),
        "frame layout diverges from ratatui in {} case(s):\n  {}",
        bad.len(),
        bad.join("\n  ")
    );
}

/// **The one known divergence**, pinned so it cannot drift unnoticed.
///
/// When the visible fixed rows *cannot* all fit, both engines give the content
/// row nothing and drop a row — but they drop a different one. `ratatui`'s
/// solver starves an interior row and keeps the last; `fresh-ui` fills in order
/// and starves the last. `render.rs` flags this band deliberately ("running the
/// actual split … keeps small-terminal squeeze behavior identical by
/// construction").
///
/// The migration consequence is recorded in the plan: it is not a layout bug to
/// fix in `fresh-ui`, it is a signal that *which rows are visible* belongs in
/// `build()` as a function of the available height — app state deciding
/// structure — rather than being left to solver-specific starvation order.
#[test]
fn squeeze_band_starves_a_different_row_than_ratatui() {
    // status + search + prompt visible (three fixed rows) in two rows of height.
    let f = Frame {
        menu_bar: false,
        status_bar: true,
        search_options: Some(Default::default()),
        prompt_line: true,
        dock: None,
        explorer: None,
        menu: None,
        dropdowns: Vec::new(),
        menu_bar_items: Default::default(),
    };
    let size = Rect {
        x: 0,
        y: 0,
        width: 50,
        height: 2,
    };
    let got = fold(f.clone(), size);
    let want = reference(f.clone(), size);
    assert_ne!(
        got, want,
        "the squeeze divergence disappeared - if fresh-ui's flex starvation now \
         matches ratatui's solver, delete this test and widen the sweep above"
    );

    let h = |v: &Vec<(u64, Rect)>, id: u64| v.iter().find(|(i, _)| *i == id).unwrap().1.height;
    // fresh-ui fills in order: status and search get their row, prompt starves.
    assert_eq!(
        (
            h(&got, id(HostRegion::StatusBar)),
            h(&got, id(HostRegion::SearchOptions)),
            h(&got, id(HostRegion::PromptLine))
        ),
        (1, 1, 0)
    );
    // ratatui keeps the last row and starves the interior one instead.
    assert_eq!(
        (
            h(&want, id(HostRegion::StatusBar)),
            h(&want, id(HostRegion::SearchOptions)),
            h(&want, id(HostRegion::PromptLine))
        ),
        (1, 0, 1)
    );
}
