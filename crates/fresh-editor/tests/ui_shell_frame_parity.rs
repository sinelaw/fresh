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

use fresh::view::shell::frame::{frame_tree, region_rects, Frame, HostRegion};
use fresh_ui::{Size, Ui};
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
    match f.sidebar.as_ref().map(|s| (s.cols, s.on_left)) {
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
                        // **Only the fields that can move a rectangle are
                        // named.** Everything else is content or a layer: a
                        // menu, a popup, the theme inspector, the file-open
                        // dialog, the suggestion list and the overlay card are
                        // all out of flow, and the bars occupy their one cell
                        // whatever is written on them. Spelling each of those
                        // out as `None` made this literal break on every new
                        // surface and taught nothing when it did — a field
                        // that *does* move a region belongs in the loops
                        // above, and one that does not belongs to the default.
                        v.push(Frame {
                            menu_bar: menu,
                            status_bar: status,
                            search_options: search.then(Default::default),
                            prompt_line: prompt,
                            dock,
                            sidebar: explorer.map(|(cols, on_left)| {
                                fresh::view::shell::sidebar::Sidebar::explorer_only(
                                    cols,
                                    on_left,
                                    Default::default(),
                                )
                            }),
                            ..Frame::default()
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
        ..Frame::default()
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

/// **What a frame layout costs**, measured — §6.2 item 10 of the migration
/// plan has carried "frame scheduling and rebuild cost" as unmeasured since it
/// was written.
///
/// It stopped being academic with S5. The split grid's geometry has callers
/// that run per-frame (`split_tabs_width`) and one that needs the answer for a
/// size no tree has been laid out at yet (`apply_layout`, which sets a new size
/// and asks before the frame that would record one). Both are served by laying
/// a description out on demand — *if* that is cheap, and this is the number
/// that says whether it is.
///
/// Reported rather than bounded tightly: a wall-clock threshold is a flake
/// waiting for a loaded runner, so the assertion is three orders of magnitude
/// clear and only fires if the cost changes character. Run with `--nocapture`
/// to read the figure.
#[test]
fn a_frame_layout_is_cheap_enough_to_ask_for_on_demand() {
    use std::time::Instant;
    let f = || Frame {
        menu_bar: true,
        status_bar: true,
        prompt_line: true,
        dock: Some(28),
        sidebar: Some(fresh::view::shell::sidebar::Sidebar::explorer_only(
            30,
            true,
            Default::default(),
        )),
        ..Frame::default()
    };
    let size = Size::new(200, 60);

    // Retained: the first frame builds the element tree, later ones reconcile
    // — the cost a per-frame caller actually pays.
    let mut ui: Ui<fresh::view::shell::msg::UiMsg> = Ui::new();
    ui.frame(frame_tree(f()), size);
    const N: u32 = 500;
    let t = Instant::now();
    for _ in 0..N {
        ui.frame(frame_tree(f()), size);
    }
    let retained = t.elapsed() / N;

    // Cold: what a caller with no `Ui` of its own pays. `apply_layout` is one.
    let t = Instant::now();
    for _ in 0..N {
        let mut cold: Ui<fresh::view::shell::msg::UiMsg> = Ui::new();
        cold.frame(frame_tree(f()), size);
    }
    let cold = t.elapsed() / N;

    println!("frame layout: {retained:?} retained, {cold:?} cold");
    assert!(
        cold.as_millis() < 50,
        "a frame layout from cold took {cold:?} — the cost has changed character"
    );
}
