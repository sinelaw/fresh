//! The sidebar as a column of sections.
//!
//! The column the file explorer used to be is a *list* now: the explorer is
//! its first section, and anything else that wants to live beside the tree is
//! another. See `docs/internal/sidebar-sections-design.md`.
//!
//! # The shape
//!
//! ```text
//! ┌ File Explorer ─────────×─┐   header row of section 0 — the column's top border
//! │▼ demo                    │   section 0's body: two walls and its rows
//! │  ▼ crates                │
//! ├ ▼ Outline ─────────────×─┤   header row of section 1 — one *shared* row
//! │▼ fn main                 │   section 1's body
//! └──────────────────────────┘   the column's bottom border
//! ```
//!
//! **Adjacent sections share one border row.** A section's header row is its
//! top border, and it is the row the section above it would otherwise have
//! spent on a bottom border. So the column is `[section, section, …, bottom
//! border]`, where every section is `[header row, body]` and a body draws its
//! two walls and nothing above or below them. With one section that is
//! exactly the bordered box the explorer has always been — the same cells,
//! the same rectangles — which is what the parity and e2e suites pin.
//!
//! **The width grip is the column's, not a section's.** It spans every
//! section's outer edge, as it always spanned the panel's, and it stays in an
//! overlay over the whole column for the same reason the explorer's did: a
//! strip whose spacers pass presses through is the one shape that lets one
//! cell take the pointer without covering the rows beside it.

use std::rc::Rc;

use fresh_ui::{
    col, focusable, gesture, layer, layout_reader, row, stack, text, Align, Anchor, Event,
    GestureKind, Key, KeyCode, LayoutInfo, Modality, Node, Place, PointerMode, Sizing,
};

use crate::app::shell_host::shell_theme::pair;
use crate::app::types::HoverTarget;

use super::file_explorer::{self, Explorer};
use super::msg::{UiFact, UiMsg};

/// The column.
#[derive(Clone, Debug)]
pub struct Sidebar {
    /// Width in columns, already resolved against the frame.
    pub cols: u16,
    pub on_left: bool,
    /// Whether the pointer is on the width grip. The column draws its own
    /// highlight from this; see [`grip_ink`].
    pub grip_hovered: bool,
    /// The screen column the sidebar starts at — the dock's width on the
    /// left, or the frame's edge less `cols` on the right. App-derived, like
    /// the width `dock::blur_observer` is handed, and for the same reason:
    /// a capture-phase observer on the whole frame has no layout to read.
    pub x0: u16,
    /// Top to bottom. Section 0 is the explorer.
    pub sections: Vec<Section>,
}

/// One section of the column: its content, and the chrome it wears.
#[derive(Clone, Debug)]
pub struct Section {
    pub kind: SectionKind,
    /// `" File Explorer (Ctrl+E) "`, `" [host] "`, `" /query "` — or a
    /// plugin section's own name. Leading and trailing space included, as
    /// ratatui's `Block` title always carried them.
    pub title: String,
    pub title_theme: String,
    pub border_theme: String,
    pub close_theme: String,
    /// Body rows, already resolved. Read for every open section but the last
    /// open one, which takes the remainder of the column.
    pub rows: u16,
    /// A collapsed section is its header row and nothing else.
    pub collapsed: bool,
    /// Whether this section owns the keyboard; exactly one chrome region
    /// wears the accent.
    pub focused: bool,
    /// Whether the header carries a `×`. The explorer's always does — it
    /// hides the sidebar, as it always has.
    pub closable: bool,
}

/// What a section holds.
#[derive(Clone, Debug)]
pub enum SectionKind {
    Explorer(Explorer),
    /// A plugin panel — the *same* `Interior` the dock column and the
    /// floating panel mount, with `Slot::Sidebar(index)` so its hits route
    /// back to the right panel.
    Panel(super::panel::Interior),
    /// A plugin section whose plugin has not mounted its panel — a workspace
    /// restored before the plugin loaded. The header stays so the section
    /// keeps its place and its rows; the body says why it is empty.
    Unavailable(String),
}

impl Section {
    /// An explorer section in the panel's blurred chrome — what a test that
    /// only cares about geometry wants.
    pub fn explorer(e: Explorer) -> Section {
        let (title_theme, border_theme) = file_explorer::chrome_themes(false, false);
        Section {
            kind: SectionKind::Explorer(e),
            title: " File Explorer ".to_string(),
            title_theme,
            border_theme,
            close_theme: file_explorer::close_theme(false),
            rows: 0,
            collapsed: false,
            focused: false,
            closable: true,
        }
    }
}

impl Sidebar {
    /// A column holding the explorer and nothing else.
    pub fn explorer_only(cols: u16, on_left: bool, e: Explorer) -> Sidebar {
        Sidebar {
            cols,
            on_left,
            grip_hovered: false,
            x0: 0,
            sections: vec![Section::explorer(e)],
        }
    }

    /// The explorer section's content, if the column has one.
    pub fn explorer(&self) -> Option<&Explorer> {
        self.sections.iter().find_map(|s| match &s.kind {
            SectionKind::Explorer(e) => Some(e),
            SectionKind::Panel(_) | SectionKind::Unavailable(_) => None,
        })
    }

    /// The plugin section that owns the keyboard, if one does.
    pub fn focused_panel(&self) -> Option<(usize, &super::panel::Interior)> {
        self.sections
            .iter()
            .enumerate()
            .find_map(|(i, s)| match &s.kind {
                SectionKind::Panel(p) if s.focused => Some((i, p)),
                _ => None,
            })
    }
}

/// The keys the readers below look elements up by.
pub fn close_key(index: usize) -> Key {
    Key::Pair("sidebar_close".into(), index as u64)
}

pub fn grip_key() -> Key {
    Key::Str("explorer_grip".into())
}

/// A section's header row — the node its keyboard layer names as its scope
/// while the section is collapsed and focused.
pub fn header_key(index: usize) -> Key {
    Key::Pair("sidebar_header".into(), index as u64)
}

/// The header's keyboard, while a collapsed section owns it.
///
/// The same shape as [`super::panel::keys_layer`]: a `Modality::Focus` layer
/// that paints nothing, takes no pointer, and confines traversal to the
/// header without swallowing what the header declines — so every key but
/// Enter and Space continues to the editor's own resolution, where the
/// explorer's `KeyContext` still answers.
pub fn keys_layer(index: usize) -> Node<UiMsg> {
    layer()
        .anchor(Anchor::Screen(Align::Start))
        .place(Place::Fill)
        .pointer_mode(PointerMode::Ignore)
        .modality(Modality::Focus)
        .scope_at(header_key(index))
}

fn hover_msg(t: Option<HoverTarget>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
}

/// The column as a description: the sections in a `col`, and the width grip
/// in an overlay over all of them.
pub fn sidebar(s: &Sidebar) -> Node<UiMsg> {
    let last_open = s.sections.iter().rposition(|sec| !sec.collapsed);
    let mut column = col();
    for (i, sec) in s.sections.iter().enumerate() {
        let node = section(s, i, sec);
        let node = if sec.collapsed {
            node.h(Sizing::Cells(1))
        } else if Some(i) == last_open {
            // The last open section is the remainder, so the column is
            // exactly filled and nothing rounds.
            node.flex(1)
        } else {
            node.h(Sizing::Cells(1 + sec.rows))
        };
        column = column.child(node);
    }
    // The bottom border wears the last section's chrome — with one section,
    // the panel's own.
    let bottom_theme = s
        .sections
        .last()
        .map(|sec| sec.border_theme.clone())
        .unwrap_or_else(Explorer::panel);
    if last_open.is_none() {
        // Every section collapsed: a stack of header rows over empty ground.
        column = column.child(
            row()
                .flex(1)
                .theme(bottom_theme.clone())
                .pointer_mode(PointerMode::Transparent),
        );
    }
    column = column.child(border_line(bottom_theme, '└', '┘').h(Sizing::Cells(1)));
    stack().children([column, overlay(s)])
}

/// One section: its header row over its body, sized by the caller.
fn section(s: &Sidebar, i: usize, sec: &Section) -> Node<UiMsg> {
    let mut c = col().child(header_row(s, i, sec));
    if !sec.collapsed {
        c = c.child(body(i, sec).flex(1));
    }
    match &sec.kind {
        // **The union box.** The explorer's right-press menu and left-press
        // focus cover the whole section, header row included, exactly as the
        // component bound them to the whole panel.
        SectionKind::Explorer(_) => file_explorer::union_box(c),
        // A panel's widgets answer their own presses and stop the flow, so
        // what reaches here is a press they declined: the section's dead
        // space, and the focus is the whole of it — `dock::column`'s rule.
        SectionKind::Panel(_) => gesture(c).on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != fresh_ui::MouseButton::Left {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::SectionFocus { index: i }))
            }),
        ),
        SectionKind::Unavailable(_) => c,
    }
}

/// A section's body: two walls, and the content between them.
///
/// **Not `.border()`**, deliberately: a bordered box draws all four sides,
/// and the row above this body is the section's header — the shared row —
/// while the row below it is the next section's header or the column's own
/// bottom border. The content is laid at `cols - 2` and clipped to it, which
/// is the inner rectangle a bordered box would have given it.
fn body(i: usize, sec: &Section) -> Node<UiMsg> {
    let content = match &sec.kind {
        SectionKind::Explorer(e) => file_explorer::rows(e),
        SectionKind::Panel(p) => panel_body(i, p),
        SectionKind::Unavailable(why) => col().child(
            text(why.clone())
                .theme(pair("editor.line_number_fg", "editor.bg"))
                .h(Sizing::Cells(1)),
        ),
    };
    stack().children([
        walls(&sec.border_theme),
        row().pointer_mode(PointerMode::Transparent).children([
            row()
                .w(Sizing::Cells(1))
                .pointer_mode(PointerMode::Transparent),
            col().flex(1).clip(true).child(content),
            row()
                .w(Sizing::Cells(1))
                .pointer_mode(PointerMode::Transparent),
        ]),
    ])
}

/// A plugin section's interior: the dock column's body with a different
/// slot.
///
/// **The interior width rule is the dock's, inverted for two borders.**
/// `dock::column` lays the interior at `max_w - DIVIDER_COLS` and passes that
/// same number as the wrap width, because the two being one number is what
/// put a title bar's `×` against the divider. Here the constraint arriving is
/// already the inner width — [`body`] insets the walls — so the same one
/// number is what layout hands the reader.
///
/// Wrapped in [`super::panel::interior`] when there is something in it to
/// focus, so the keys layer the frame raises for a focused section has a
/// scope to name, exactly as the dock's does.
fn panel_body(i: usize, p: &super::panel::Interior) -> Node<UiMsg> {
    let interior = p.clone();
    let node = layout_reader(move |info: LayoutInfo| {
        let inner_w = info.constraints.max_w.max(1);
        super::widgets::node(
            &interior.spec,
            inner_w,
            &super::widgets::Ctx {
                slot: super::widgets::Slot::Sidebar(i),
                states: &interior.states,
                focus_key: interior.focus_key.clone(),
                hovered_key: interior.hovered_key.clone(),
                marker_gutter: interior.marker_gutter,
                hovered_item_key: interior.hovered_item_key.clone(),
                hovered_popup_row: interior.hovered_popup_row.clone(),
                avail_height: interior.avail_height,
                scrollbar_reveal: interior.scrollbar_reveal,
                surface: super::widgets::panel_surface(),
                markdown: interior.markdown.as_ref().map(|m| m.ctx()),
            },
        )
        .w(Sizing::Cells(inner_w))
    });
    if p.has_focus_targets() {
        super::panel::interior(super::widgets::Slot::Sidebar(i), p.claims_tab, node)
    } else {
        node
    }
}

/// Blur a focused plugin section when a press lands outside the column.
///
/// `dock::blur_observer`'s shape: a capture-phase listener on the whole
/// frame that acts and lets the press go on to whatever it was aimed at.
/// The column's extent is what "outside" needs, and `Sidebar::x0` and
/// `cols` carry it.
pub fn blur_observer(s: &Sidebar, frame: Node<UiMsg>) -> Node<UiMsg> {
    let (x0, x1) = (s.x0 as i32, s.x0 as i32 + s.cols as i32);
    gesture(frame).on_capture(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            (e.button == fresh_ui::MouseButton::Left && (e.pos.x < x0 || e.pos.x >= x1))
                .then_some(UiMsg::Ui(UiFact::SidebarBlur))
        }),
    )
}

/// The two walls of a body, and the ground between them.
///
/// **A node that repaints the column it sits on has to know how tall it
/// is**, and a description is written before layout runs. `layout_reader`'s
/// builder runs *during* layout with the constraints in hand, so each wall is
/// as long as the body turns out to be — the same shape [`grip_ink`] has.
fn walls(theme: &str) -> Node<UiMsg> {
    let wall = |ink: String| {
        layout_reader(move |c: LayoutInfo| {
            col().children(
                (0..c.constraints.max_h).map(|_| text("│").theme(ink.clone()).h(Sizing::Cells(1))),
            )
        })
        .w(Sizing::Cells(1))
        .pointer_mode(PointerMode::Transparent)
    };
    row()
        // The ground: the fill draws spaces, so only this key's background
        // reaches the eye inside the box.
        .theme(theme)
        .pointer_mode(PointerMode::Transparent)
        .children([
            wall(theme.to_string()),
            row().flex(1).pointer_mode(PointerMode::Transparent),
            wall(theme.to_string()),
        ])
}

/// One full-width row of border: `l`, a run of `─`, `r`.
fn border_line(theme: String, l: char, r: char) -> Node<UiMsg> {
    layout_reader(move |c: LayoutInfo| {
        let w = c.constraints.max_w as usize;
        let mut s = String::with_capacity(w * 3);
        s.push(l);
        for _ in 0..w.saturating_sub(2) {
            s.push('─');
        }
        if w >= 2 {
            s.push(r);
        }
        text(s).theme(theme.clone()).h(Sizing::Cells(1))
    })
    .pointer_mode(PointerMode::Transparent)
}

/// A section's header row: the border line, with the title and the close
/// button drawn on it.
///
/// Section 0's is the column's top border (`┌ … ┐`); every other section's
/// is the row it shares with the section above (`├ … ┤`).
///
/// **With more than one section the row is a control** (design §3.6): the
/// whole of it but the `×` is a `Grip::SectionDivider` that takes the press,
/// and the applier reads a release that never moved as a click, which
/// toggles the section. The chevron is the indicator, not the only target —
/// at 24 columns a one-cell target is a bad one. With one section the row
/// is decoration, as it always was, and a press on it reaches the explorer's
/// union box: the chevron and the control appear only once a second section
/// exists (§4.6).
fn header_row(s: &Sidebar, i: usize, sec: &Section) -> Node<UiMsg> {
    let (l, r) = if i == 0 {
        ('┌', '┐')
    } else {
        ('├', '┤')
    };
    let several = s.sections.len() > 1;
    let mut cells: Vec<Node<UiMsg>> = vec![
        // One cell of border before the title, which is where ratatui's
        // `Block` starts a left-aligned title.
        row()
            .w(Sizing::Cells(1))
            .pointer_mode(PointerMode::Transparent),
    ];
    if several {
        cells.push(
            text(if sec.collapsed { " ▶" } else { " ▼" })
                .theme(sec.title_theme.clone())
                .pointer_mode(PointerMode::Transparent),
        );
    }
    cells.push(
        text(sec.title.clone())
            .theme(sec.title_theme.clone())
            // The title is decoration. Pressing it used to select the
            // panel's first row — `row.saturating_sub(area.y + 1)` clamps
            // to 0 on the title line — while the right-click and
            // double-click paths both guarded the row out explicitly.
            // Saying it is not a target makes all three agree.
            .pointer_mode(PointerMode::Transparent),
    );
    cells.push(row().flex(1).pointer_mode(PointerMode::Transparent));
    if sec.closable {
        cells.push(close(i, sec));
    }
    let strip = row()
        .h(Sizing::Cells(1))
        .pointer_mode(PointerMode::Transparent)
        .children(cells);
    let mut layers = vec![border_line(sec.border_theme.clone(), l, r), strip];
    if several {
        // **On top, and exactly as wide as the row less the `×`.** A hit
        // continues *behind* a transparent node but still runs that node's
        // ancestors' handlers first, and section 0's ancestors include the
        // explorer's union box: a handle under the strip's transparent title
        // cells would lose every press to it. Sized rather than wrapped in a
        // spacer row for the same reason — a transparent container over the
        // `×` would be one more path for the union box to claim.
        let close_cols = if sec.closable { 3 } else { 0 };
        layers.push(
            super::grip::draggable(
                super::msg::Grip::SectionDivider(i),
                row(),
                Rc::new(move |e: &Event| {
                    Some(UiMsg::Ui(UiFact::SectionResizeBegin {
                        index: i,
                        y: e.pos.y.max(0) as u16,
                    }))
                }),
            )
            .w(Sizing::Cells(s.cols.saturating_sub(close_cols)))
            .h(Sizing::Cells(1)),
        );
    }
    let header = stack()
        .h(Sizing::Cells(1))
        .pointer_mode(PointerMode::Transparent)
        .children(layers);
    if !(sec.focused && sec.collapsed) {
        return header;
    }
    // The header *is* the section while it is collapsed, so it is what holds
    // the keyboard: Enter and Space re-open it, and everything else is
    // declined so the host's own resolution still sees it.
    focusable(header)
        .h(Sizing::Cells(1))
        .key(header_key(i))
        .autofocus()
        .on_key(move |e: &Event| {
            let toggles = e
                .key
                .is_some_and(|k| matches!(k.code, KeyCode::Enter | KeyCode::Char(' ')));
            if !toggles {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::SectionToggle { index: i }))
        })
}

/// The close button's three cells at the right of a header row.
///
/// Three cells of *hit area*, one cell of paint. The old close button was a
/// one-cell `Paragraph` at `area.width - 3` whose hit test claimed three
/// columns, and the two cells beside it kept showing the border — including
/// the `┐` corner. A themed node three cells wide fills all three, which
/// erases the corner; the theme goes on the glyph and the region around it
/// stays transparent.
///
/// The explorer's `×` hides the sidebar, as it always has; any other
/// section's removes that section.
fn close(i: usize, sec: &Section) -> Node<UiMsg> {
    let hover = if i == 0 {
        HoverTarget::FileExplorerCloseButton
    } else {
        HoverTarget::SidebarSectionClose(i)
    };
    gesture(
        row()
            .w(Sizing::Cells(3))
            .child(text("×").theme(sec.close_theme.clone())),
    )
    .key(close_key(i))
    .on(
        GestureKind::Press,
        Rc::new(move |ev: &Event| {
            if ev.button != fresh_ui::MouseButton::Left {
                return None;
            }
            ev.stop();
            Some(UiMsg::Ui(match i {
                0 => UiFact::ExplorerClose,
                _ => UiFact::SectionClose { index: i },
            }))
        }),
    )
    .on_enter(hover_msg(Some(hover)))
    .on_leave(hover_msg(None))
}

/// Everything drawn *over* the column: the width grip.
///
/// It covers the whole column, so every part of it that is not a control says
/// it is not a pointer target — otherwise the strip swallows every click on
/// the rows beneath. That is one attribute per container rather than a
/// rectangle each control has to be hit-tested against by hand.
fn overlay(s: &Sidebar) -> Node<UiMsg> {
    col().pointer_mode(PointerMode::Transparent).children([
        // The top border row is the header's: its close button lives there,
        // and it has the precedence the old hover walk gave it (it tested
        // the close button first) — so the grip starts below.
        row()
            .h(Sizing::Cells(1))
            .pointer_mode(PointerMode::Transparent),
        grip_strip(s),
    ])
}

/// What the grip paints: nothing at rest, and its own run of `│` when hovered.
///
/// At rest it paints nothing at all rather than painting the wall's `│` a
/// second time: the section walls already draw that column, and two nodes
/// painting one cell is how they drift apart.
fn grip_ink(hovered: bool) -> Node<UiMsg> {
    if !hovered {
        return row();
    }
    let ink = pair("ui.split_separator_hover_fg", "editor.bg");
    layout_reader(move |c: LayoutInfo| {
        col().children(
            (0..c.constraints.max_h).map(|_| text("│").theme(ink.clone()).h(Sizing::Cells(1))),
        )
    })
}

/// The one-column drag handle on the column's outer edge, below the top
/// border row and above the bottom one.
///
/// The corners belong to the frame that drew them: the old post-pass walked
/// `0..explorer_area.height` and recoloured both of them, so hovering the grip
/// turned `┐` and `┘` into `│`.
fn grip_strip(s: &Sidebar) -> Node<UiMsg> {
    let grip = super::grip::draggable(
        super::msg::Grip::ExplorerWidth,
        grip_ink(s.grip_hovered),
        Rc::new(|e: &Event| {
            Some(UiMsg::Ui(UiFact::ExplorerResizeBegin {
                x: e.pos.x.max(0) as u16,
                y: e.pos.y.max(0) as u16,
            }))
        }),
    )
    // On the outside, on the gesture node: an unconstrained one would cover
    // the whole column and take every press in it.
    .w(Sizing::Cells(1))
    .key(grip_key())
    .on_enter(hover_msg(Some(HoverTarget::FileExplorerBorder)))
    .on_leave(hover_msg(None));
    col().pointer_mode(PointerMode::Transparent).children([
        row()
            .flex(1)
            .pointer_mode(PointerMode::Transparent)
            .children([row().flex(1).pointer_mode(PointerMode::Transparent), grip]),
        row()
            .h(Sizing::Cells(1))
            .pointer_mode(PointerMode::Transparent),
    ])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::file_explorer::{Body, Row};
    use crate::view::shell::fold::{fold_native, Band};
    use crate::view::shell::frame::{frame_tree, Frame};
    use crate::view::shell::msg::Grip;
    use fresh_ui::{Input, KeyPress, Mods, MouseButton, Point, Size, Ui};
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;

    fn row_of(index: usize, name: &str) -> Row {
        Row {
            index,
            theme: Explorer::panel(),
            left: vec![
                ("  ".to_string(), Explorer::panel()),
                (name.to_string(), Explorer::panel()),
            ],
            trailing: None,
            error: None,
        }
    }

    fn explorer_section(rows: u16) -> Section {
        let mut s = Section::explorer(Explorer {
            body: Body::Rows(vec![row_of(0, "src"), row_of(1, "lib")]),
            caret_row: None,
            scroll: None,
        });
        s.title = " Files ".to_string();
        s.rows = rows;
        s
    }

    fn panel_section(rows: u16) -> Section {
        let (title_theme, border_theme) = file_explorer::chrome_themes(false, false);
        Section {
            kind: SectionKind::Unavailable("gone".to_string()),
            title: " Outline ".to_string(),
            title_theme,
            border_theme,
            close_theme: file_explorer::close_theme(false),
            rows,
            collapsed: false,
            focused: false,
            closable: true,
        }
    }

    fn two(cols: u16) -> Sidebar {
        Sidebar {
            cols,
            on_left: true,
            grip_hovered: false,
            x0: 0,
            sections: vec![explorer_section(2), panel_section(0)],
        }
    }

    fn laid_out(s: Sidebar, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                sidebar: Some(s),
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    fn lines(s: Sidebar, w: u16, h: u16) -> Vec<String> {
        let ui = laid_out(s, w, h);
        let spec = ui.spec().clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        let palette = |k: &fresh_ui::ThemeKey| super::super::fold::test_palette::of(k.as_str());
        fold_native(&spec, &mut buf, &palette, Band::Background);
        (0..h)
            .map(|y| {
                (0..w)
                    .map(|x| buf[(x, y)].symbol().to_string())
                    .collect::<String>()
            })
            .collect()
    }

    fn facts(d: fresh_ui::Dispatch<UiMsg>) -> Vec<UiFact> {
        d.msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect()
    }

    /// §3.4: adjacent sections share one border row, and that row carries
    /// the lower section's title in the explorer's own title shape.
    #[test]
    fn two_sections_share_one_border_row() {
        let got = lines(two(20), 20, 8);
        assert_eq!(
            got[0], "┌ ▼ Files ───────×─┐",
            "the top border is section 0's header"
        );
        assert_eq!(got[1], "│  src             │");
        assert_eq!(got[2], "│  lib             │");
        assert_eq!(
            got[3], "├ ▼ Outline ─────×─┤",
            "one shared row, no └┘ above it"
        );
        assert_eq!(got[4], "│gone              │", "the placeholder body");
        assert_eq!(got[5], "│                  │");
        assert_eq!(
            got[7], "└──────────────────┘",
            "the column's own bottom border"
        );
    }

    /// §3.6: a collapsed section is exactly its header row, and the rows it
    /// gave up go to the section still open.
    #[test]
    fn a_collapsed_section_is_one_header_row() {
        let mut s = two(20);
        s.sections[1].collapsed = true;
        let got = lines(s, 20, 8);
        assert_eq!(got[0], "┌ ▼ Files ───────×─┐");
        assert_eq!(
            got[5], "│                  │",
            "the explorer took the column"
        );
        assert_eq!(
            got[6], "├ ▶ Outline ─────×─┤",
            "collapsed: chevron turns, body gone"
        );
        assert_eq!(got[7], "└──────────────────┘");

        // Every section collapsed: a stack of header rows over empty ground.
        let mut s = two(20);
        s.sections[0].collapsed = true;
        s.sections[1].collapsed = true;
        let got = lines(s, 20, 5);
        assert_eq!(got[0], "┌ ▶ Files ───────×─┐");
        assert_eq!(got[1], "├ ▶ Outline ─────×─┤");
        assert_eq!(got[2], "                    ", "ground, no walls");
        assert_eq!(got[4], "└──────────────────┘");
    }

    /// With one section there is no chevron and no shared row — §4.6, the
    /// default configuration looks exactly as it always did.
    #[test]
    fn one_section_wears_no_chevron() {
        let got = lines(
            Sidebar {
                cols: 20,
                on_left: true,
                grip_hovered: false,
                x0: 0,
                sections: vec![explorer_section(0)],
            },
            20,
            5,
        );
        assert_eq!(got[0], "┌ Files ─────────×─┐");
        assert_eq!(got[4], "└──────────────────┘");
    }

    /// A press on a header row is a divider drag's press: it captures the
    /// pointer, so the move and the release come back to it wherever the
    /// pointer went. The `×` is its own, and says so.
    #[test]
    fn a_header_row_is_a_grip_and_its_close_button_is_not() {
        let mut ui = laid_out(two(20), 20, 8);
        let got = ui.dispatch(Input::press(
            Point::new(6, 3),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(
            facts(got),
            vec![UiFact::SectionResizeBegin { index: 1, y: 3 }]
        );
        let got = ui.dispatch(Input::Move {
            pos: Point::new(6, 5),
            mods: Mods::NONE,
        });
        assert_eq!(
            facts(got),
            vec![UiFact::GripDrag {
                which: Grip::SectionDivider(1),
                x: 6,
                y: 5
            }]
        );
        let got = ui.dispatch(Input::release(
            Point::new(6, 5),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(
            facts(got),
            vec![UiFact::GripRelease {
                which: Grip::SectionDivider(1)
            }]
        );

        let close = ui.rect_of(ui.find_by_key(&close_key(1)).expect("close"));
        let got = ui.dispatch(Input::press(
            Point::new(close.x, close.y),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(facts(got), vec![UiFact::SectionClose { index: 1 }]);
        ui.dispatch(Input::release(
            Point::new(close.x, close.y),
            MouseButton::Left,
            Mods::NONE,
        ));

        // Section 0's header is a grip too — the applier finds nothing above
        // it to resize and reads the release as the toggle it is — and its
        // `×` still hides the sidebar.
        let got = ui.dispatch(Input::press(
            Point::new(6, 0),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(
            facts(got),
            vec![UiFact::SectionResizeBegin { index: 0, y: 0 }]
        );
        ui.dispatch(Input::release(
            Point::new(6, 0),
            MouseButton::Left,
            Mods::NONE,
        ));
        let close = ui.rect_of(ui.find_by_key(&close_key(0)).expect("close"));
        let got = ui.dispatch(Input::press(
            Point::new(close.x, close.y),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(facts(got), vec![UiFact::ExplorerClose]);
    }

    /// A plugin section holding one button, focused, in the shape the frame
    /// builds for a mounted panel.
    fn with_panel(focused: bool) -> Sidebar {
        use crate::view::shell::panel::Interior;
        use fresh_core::api::WidgetSpec;
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::Raw {
                    entries: vec![fresh_core::text_property::TextPropertyEntry::text(
                        "0123456789abcdefghijklmnop",
                    )],
                    key: None,
                },
                WidgetSpec::Button {
                    label: "Go".into(),
                    focused: false,
                    intent: Default::default(),
                    key: Some("go".into()),
                    disabled: false,
                    focusable: true,
                    bare: false,
                    full_width: false,
                    hover_style: None,
                    style: None,
                },
            ],
            key: None,
        };
        let interior = Interior {
            spec: Rc::new(spec),
            states: Rc::new(Default::default()),
            focus_key: String::new(),
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
            claims_tab: false,
            markdown: None,
        };
        let mut sec = panel_section(0);
        sec.kind = SectionKind::Panel(interior);
        sec.focused = focused;
        Sidebar {
            cols: 20,
            on_left: true,
            grip_hovered: false,
            x0: 0,
            sections: vec![explorer_section(2), sec],
        }
    }

    /// §4.2: a panel section's interior is laid at `cols - 2` and wrapped at
    /// the same number — the dock's rule, inverted for two walls — so what
    /// does not fit is clipped at the wall rather than painted over it.
    #[test]
    fn a_panel_section_lays_its_interior_between_the_walls() {
        let got = lines(with_panel(false), 20, 9);
        assert_eq!(got[3], "├ ▼ Outline ─────×─┤");
        assert_eq!(
            got[4], "│0123456789abcdefgh│",
            "eighteen columns, then the wall"
        );
        assert!(
            got[5].contains("Go"),
            "the button is on the next row: {:?}",
            got[5]
        );
        assert_eq!(got[8], "└──────────────────┘");
    }

    /// The dock's keys layer, per section: a focused plugin section's
    /// widgets are offered the key and hand back what they decline as
    /// `PanelKey(Slot::Sidebar(i))`, exactly as the dock's do.
    #[test]
    fn a_focused_panel_section_takes_the_keys_the_way_the_dock_does() {
        use crate::view::shell::widgets::Slot;
        // Wider than the column, so there is an outside to press on.
        let mut ui = laid_out(with_panel(true), 60, 9);
        let got = ui.dispatch(Input::Key(KeyPress {
            code: KeyCode::Esc,
            mods: Mods::NONE,
        }));
        assert_eq!(facts(got), vec![UiFact::PanelKey(Slot::Sidebar(1))]);

        // A press on the section's dead space focuses it; a press outside
        // the column blurs it and goes on.
        let got = ui.dispatch(Input::press(
            Point::new(3, 7),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(facts(got), vec![UiFact::SectionFocus { index: 1 }]);
        ui.dispatch(Input::release(
            Point::new(3, 7),
            MouseButton::Left,
            Mods::NONE,
        ));
        let got = ui.dispatch(Input::press(
            Point::new(40, 4),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(
            facts(got).contains(&UiFact::SidebarBlur),
            "a press outside the column blurs the section"
        );

        // Blurred, the section has no layer and Esc is nobody's.
        let mut ui = laid_out(with_panel(false), 20, 9);
        let got = ui.dispatch(Input::Key(KeyPress {
            code: KeyCode::Esc,
            mods: Mods::NONE,
        }));
        assert!(facts(got).is_empty());
    }

    /// A press on a row of the explorer's body still reaches the row — the
    /// header's gestures are the header's alone.
    #[test]
    fn the_body_rows_stay_reachable_under_a_header() {
        let mut ui = laid_out(two(20), 20, 8);
        let got = ui.dispatch(Input::press(
            Point::new(3, 2),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(
            matches!(
                facts(got).as_slice(),
                [UiFact::ExplorerRowPress { index: 1, .. }]
            ),
            "the second row"
        );
    }

    /// Enter or Space on a collapsed section that has the keyboard re-opens
    /// it, and any other key is declined so the host's own resolution still
    /// sees it.
    #[test]
    fn enter_on_a_focused_collapsed_header_toggles_and_other_keys_pass() {
        let mut s = two(20);
        s.sections[0].collapsed = true;
        s.sections[0].focused = true;
        let mut ui = laid_out(s, 20, 8);
        let key = |code| {
            Input::Key(KeyPress {
                code,
                mods: Mods::NONE,
            })
        };
        let got = ui.dispatch(key(KeyCode::Enter));
        assert!(got.claimed);
        assert_eq!(facts(got), vec![UiFact::SectionToggle { index: 0 }]);
        let got = ui.dispatch(key(KeyCode::Char(' ')));
        assert_eq!(facts(got), vec![UiFact::SectionToggle { index: 0 }]);
        let got = ui.dispatch(key(KeyCode::Esc));
        assert!(!got.claimed, "declined, so the explorer's context answers");
        assert!(facts(got).is_empty());

        // Not collapsed: no layer, no header keys — the rows have them.
        let mut s = two(20);
        s.sections[0].focused = true;
        let mut ui = laid_out(s, 20, 8);
        let got = ui.dispatch(key(KeyCode::Enter));
        assert!(facts(got).is_empty());
    }
}
