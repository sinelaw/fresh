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
//! folded to `Draw::Host` items, and as the ratatui `Layout` calls `render.rs`
//! actually makes — and compares the resulting rectangles over a sweep of
//! terminal sizes and visibility combinations.
//!
//! Note what is deliberately *not* compared: the dock's bail-out rules
//! (`EDITOR_MIN`/`DOCK_MIN`) are app logic keyed on the frame width, not a
//! layout constraint. `build()` cannot read geometry, so a real shell resolves
//! that width from state before building. Both sides here are handed the
//! already-resolved width, isolating the layout question.

use fresh_ui::{col, host, row, Draw, Node, Sizing, Size, Ui};
use ratatui::layout::{Constraint, Direction, Layout, Rect};

const DOCK: u64 = 1;
const MENU: u64 = 2;
const EXPLORER: u64 = 3;
const BODY: u64 = 4;
const STATUS: u64 = 5;
const SEARCH: u64 = 6;
const PROMPT: u64 = 7;

#[derive(Clone, Copy, Debug)]
struct Flags {
    menu: bool,
    status: bool,
    search: bool,
    prompt: bool,
    /// Already-resolved dock width (see module note).
    dock: Option<u16>,
    /// (columns, on_left)
    explorer: Option<(u16, bool)>,
}

impl Flags {
    /// Apply the dock's real bail-out rules (`compute_dock_split`) for a frame
    /// of `width`. This is **app logic, not layout**: it decides whether a dock
    /// exists at all and how wide it may be, keyed on the frame width that
    /// `build()` cannot read. A real shell resolves it from state first and
    /// hands the layout a settled number — which is exactly what this does, so
    /// both sides below compare layout and nothing else.
    fn resolved(mut self, width: u16) -> Flags {
        const EDITOR_MIN: u16 = 20;
        const DOCK_MIN: u16 = 24;
        self.dock = self.dock.and_then(|requested| {
            let max_dock = width.saturating_sub(EDITOR_MIN);
            if max_dock < DOCK_MIN {
                None
            } else {
                Some(requested.min(max_dock).max(1))
            }
        });
        self
    }
}

// ---------------------------------------------------------------------------
// The shell: the frame as a fresh-ui description
// ---------------------------------------------------------------------------

fn shell(f: Flags) -> Node<()> {
    let mut rows: Vec<Node<()>> = Vec::new();
    if f.menu {
        rows.push(host(MENU).h(Sizing::Cells(1)));
    }
    rows.push(match f.explorer {
        Some((cols, true)) => row().flex(1).children([
            host(EXPLORER).w(Sizing::Cells(cols)),
            host(BODY).flex(1),
        ]),
        Some((cols, false)) => row().flex(1).children([
            host(BODY).flex(1),
            host(EXPLORER).w(Sizing::Cells(cols)),
        ]),
        None => host(BODY).flex(1),
    });
    if f.status {
        rows.push(host(STATUS).h(Sizing::Cells(1)));
    }
    if f.search {
        rows.push(host(SEARCH).h(Sizing::Cells(1)));
    }
    if f.prompt {
        rows.push(host(PROMPT).h(Sizing::Cells(1)));
    }
    let chrome = col().flex(1).children(rows);
    match f.dock {
        Some(w) => row().children([host(DOCK).w(Sizing::Cells(w)), chrome]),
        None => chrome,
    }
}

/// The fold: pull every `Draw::Host` item out of the display list. This is the
/// shape the real backend callback takes — `(HostId, rect)` is exactly what a
/// region painter needs.
fn fold(f: Flags, size: Rect) -> Vec<(u64, Rect)> {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(shell(f), Size::new(size.width, size.height));
    let mut out: Vec<(u64, Rect)> = spec
        .items
        .iter()
        .filter_map(|it| match &it.draw {
            Draw::Host(id) => Some((
                id.0,
                Rect {
                    x: it.rect.x as u16,
                    y: it.rect.y as u16,
                    width: it.rect.w,
                    height: it.rect.h,
                },
            )),
            _ => None,
        })
        .collect();
    out.sort_by_key(|(id, _)| *id);
    out
}

// ---------------------------------------------------------------------------
// The reference: the ratatui Layout calls render.rs actually makes
// ---------------------------------------------------------------------------

fn reference(f: Flags, size: Rect) -> Vec<(u64, Rect)> {
    let mut out: Vec<(u64, Rect)> = Vec::new();

    // compute_dock_split: rect math, post-bail-out.
    let chrome_area = match f.dock {
        Some(w) => {
            out.push((
                DOCK,
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
        None => size,
    };

    // The five-row chrome column.
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints(vec![
            Constraint::Length(if f.menu { 1 } else { 0 }),
            Constraint::Min(0),
            Constraint::Length(if f.status { 1 } else { 0 }),
            Constraint::Length(if f.search { 1 } else { 0 }),
            Constraint::Length(if f.prompt { 1 } else { 0 }),
        ])
        .split(chrome_area);

    if f.menu {
        out.push((MENU, chunks[0]));
    }
    if f.status {
        out.push((STATUS, chunks[2]));
    }
    if f.search {
        out.push((SEARCH, chunks[3]));
    }
    if f.prompt {
        out.push((PROMPT, chunks[4]));
    }

    // split_file_explorer_area on the content row.
    match f.explorer {
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
            out.push((EXPLORER, explorer));
            out.push((BODY, editor));
        }
        None => out.push((BODY, chunks[1])),
    }

    out.sort_by_key(|(id, _)| *id);
    out
}

// ---------------------------------------------------------------------------

fn combos() -> Vec<Flags> {
    let mut v = Vec::new();
    for &menu in &[true, false] {
        for &status in &[true, false] {
            for &(search, prompt) in &[(false, false), (true, false), (false, true), (true, true)] {
                for &dock in &[None, Some(24u16)] {
                    for &explorer in &[None, Some((20u16, true)), Some((20u16, false))] {
                        v.push(Flags {
                            menu,
                            status,
                            search,
                            prompt,
                            dock,
                            explorer,
                        });
                    }
                }
            }
        }
    }
    v
}

/// Rows whose height is fixed at 1 when visible.
fn fixed_rows(f: &Flags) -> u16 {
    f.menu as u16 + f.status as u16 + f.search as u16 + f.prompt as u16
}

/// Report divergences rather than asserting on the first, so the *shape* of any
/// disagreement is visible at once. Only sizes where the fixed rows actually
/// fit are compared; the overflow band is pinned separately below.
fn sweep(sizes: &[(u16, u16)]) -> Vec<String> {
    let mut bad = Vec::new();
    for raw in combos() {
        for &(w, h) in sizes {
            let f = raw.resolved(w);
            if h < fixed_rows(&f) {
                continue; // the squeeze band — see the test below
            }
            let size = Rect { x: 0, y: 0, width: w, height: h };
            let got = fold(f, size);
            let want = reference(f, size);
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
    let f = Flags {
        menu: false,
        status: true,
        search: true,
        prompt: true,
        dock: None,
        explorer: None,
    };
    let size = Rect { x: 0, y: 0, width: 50, height: 2 };
    let got = fold(f, size);
    let want = reference(f, size);
    assert_ne!(
        got, want,
        "the squeeze divergence disappeared - if fresh-ui's flex starvation now \
         matches ratatui's solver, delete this test and widen the sweep above"
    );

    let h = |v: &Vec<(u64, Rect)>, id: u64| v.iter().find(|(i, _)| *i == id).unwrap().1.height;
    // fresh-ui fills in order: status and search get their row, prompt starves.
    assert_eq!((h(&got, STATUS), h(&got, SEARCH), h(&got, PROMPT)), (1, 1, 0));
    // ratatui keeps the last row and starves the interior one instead.
    assert_eq!((h(&want, STATUS), h(&want, SEARCH), h(&want, PROMPT)), (1, 0, 1));
}
