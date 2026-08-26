//! The search-options row as a description — the first surface the tree
//! *measures*.
//!
//! Every migrated surface so far handed the tree a rectangle someone else had
//! already computed: the context menu its opening point, a dropdown level its
//! `x`/`y`/`width`. This row hands over none. It says what the toggles are —
//! label, shortcut, checked — and layout decides where each one lands, from
//! the text. The spans the rest of the editor needs (the web projection's
//! toggle rects) are then *read back* from the laid-out tree by
//! [`option_spans`], which is the direction the migration is for: geometry
//! produced by layout and read by everyone, rather than spelled twice and
//! reconciled by a `debug_assert_eq!`.
//!
//! That assertion is what this replaces. `SearchOptionsLayout::compute` and
//! `StatusBarRenderer::render_search_options` were two spellings of one width
//! walk — `str_width("[ ] {label}")`, plus the shortcut, plus three cells of
//! separator — kept honest only in debug builds. There is one walk now, and it
//! is the layout.

use std::rc::Rc;

use fresh_ui::{gesture, row, text, text_runs, Event, GestureKind, Key, Node, Run, Sizing};

use crate::app::shell_host::shell_theme::pair;
use crate::app::types::HoverTarget;
use crate::input::keybindings::Action;

use super::msg::{UiFact, UiMsg};

/// Which toggle. The identity a click, a hover and a read-back span share.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum SearchOption {
    CaseSensitive,
    WholeWord,
    Regex,
    ConfirmEach,
}

impl SearchOption {
    /// The action a click performs — a real, bindable action, which is why a
    /// click on this row produces [`UiMsg::Action`] and no `UiFact` at all.
    /// The row's only positional fact is where the pointer is.
    pub fn action(self) -> Action {
        match self {
            SearchOption::CaseSensitive => Action::ToggleSearchCaseSensitive,
            SearchOption::WholeWord => Action::ToggleSearchWholeWord,
            SearchOption::Regex => Action::ToggleSearchRegex,
            SearchOption::ConfirmEach => Action::ToggleSearchConfirmEach,
        }
    }

    /// The hover target the restyle reads. Kept as `HoverTarget` rather than a
    /// row-private enum because the web bridge and the theme inspector already
    /// speak it.
    pub fn hover_target(self) -> HoverTarget {
        match self {
            SearchOption::CaseSensitive => HoverTarget::SearchOptionCaseSensitive,
            SearchOption::WholeWord => HoverTarget::SearchOptionWholeWord,
            SearchOption::Regex => HoverTarget::SearchOptionRegex,
            SearchOption::ConfirmEach => HoverTarget::SearchOptionConfirmEach,
        }
    }

    /// The name the web projection gives this toggle.
    pub fn web_name(self) -> &'static str {
        match self {
            SearchOption::CaseSensitive => "case",
            SearchOption::WholeWord => "word",
            SearchOption::Regex => "regex",
            SearchOption::ConfirmEach => "confirm",
        }
    }

    fn slot(self) -> u64 {
        match self {
            SearchOption::CaseSensitive => 0,
            SearchOption::WholeWord => 1,
            SearchOption::Regex => 2,
            SearchOption::ConfirmEach => 3,
        }
    }

    fn key(self) -> Key {
        Key::Pair("search_option".into(), self.slot())
    }
}

/// One toggle as the row shows it: what it is, what it says, whether it is on.
///
/// No width and no column. `[x]` and `[ ]` are the same width, so `checked`
/// moves nothing — but that is now a fact about the glyphs rather than a rule
/// the two walks had to agree on.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Toggle {
    pub option: SearchOption,
    /// The localized label, without the checkbox.
    pub label: String,
    /// The key hint, without its parentheses. `None` when the action is
    /// unbound.
    pub shortcut: Option<String>,
    pub checked: bool,
}

/// What sits on the row, in order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Piece {
    Toggle(Toggle),
    /// The capture-group reminder shown between Regex and Confirm while regex
    /// is on in a replace prompt. Not a toggle: nothing clicks it, and it
    /// takes no separator of its own.
    Hint(String),
}

/// The row's content, plus what the pointer is on.
///
/// `hovered` is state, not geometry: the tree reports where the pointer is
/// (`UiFact::Hover`), the editor stores it, and the next build reads it back
/// here. A description that asked the layout which item is under the pointer
/// would be a build that depends on layout, which is the loop the library
/// refuses.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SearchOptions {
    pub pieces: Vec<Piece>,
    pub hovered: Option<SearchOption>,
}

/// The row's ground, and the resting colour of an unchecked label.
fn base() -> String {
    pair("ui.menu_dropdown_fg", "ui.menu_dropdown_bg")
}

/// Under the pointer.
fn hovered() -> String {
    pair("ui.menu_hover_fg", "ui.menu_hover_bg")
}

/// Checked. The same `menu_active_*` pair the bar's open label uses — a
/// designed fg/bg pair from the family this row already draws in, rather than
/// two keys meant for different surfaces. (`menu_highlight_fg` on
/// `menu_dropdown_bg` collided on Dracula and rendered the checked box
/// invisible; that is the bug this pair fixed.)
fn checked() -> String {
    pair("ui.menu_active_fg", "ui.menu_active_bg")
}

/// A key hint, and the capture-group reminder beside it.
fn muted() -> String {
    pair("ui.help_separator_fg", "ui.menu_dropdown_bg")
}

fn hover_msg(t: Option<HoverTarget>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
}

/// Empty space of a fixed width: the row's left pad and the gaps between
/// toggles. It carries no theme, so the row's own ground shows through — the
/// `Span::styled("   ", base_style)` the painter pushed, minus the string.
fn gap(cells: u16) -> Node<UiMsg> {
    row().w(Sizing::Cells(cells))
}

/// The row.
///
/// One cell of left pad, then the pieces: three cells of separator before every
/// toggle but the first, none before a hint. That spacing *is* the old
/// `compute` walk's `col += w + 3`, expressed as nodes instead of arithmetic.
pub fn search_options(o: &SearchOptions) -> Node<UiMsg> {
    let mut children: Vec<Node<UiMsg>> = vec![gap(1)];
    for (i, piece) in o.pieces.iter().enumerate() {
        match piece {
            Piece::Toggle(t) => {
                if i > 0 {
                    children.push(gap(3));
                }
                children.push(toggle(t, o.hovered == Some(t.option)));
            }
            Piece::Hint(h) => children.push(text(h.clone()).theme(muted())),
        }
    }
    row().theme(base()).children(children)
}

fn toggle(t: &Toggle, is_hovered: bool) -> Node<UiMsg> {
    let body = if is_hovered {
        hovered()
    } else if t.checked {
        checked()
    } else {
        base()
    };
    let mut runs = vec![Run::themed(
        format!("{} {}", if t.checked { "[x]" } else { "[ ]" }, t.label),
        body,
    )];
    if let Some(s) = &t.shortcut {
        // The hint takes the hover colour with the label: hovering the toggle
        // lights the whole toggle, shortcut included.
        runs.push(Run::themed(
            format!(" ({s})"),
            if is_hovered { hovered() } else { muted() },
        ));
    }
    let option = t.option;
    let action = option.action();
    gesture(text_runs(runs))
        .key(option.key())
        // **Press**, not click, because that is when this row has always
        // toggled: the old routing ran off `MouseEventKind::Down(Left)`
        // through the chrome walk. `stop()` claims it, which is what the
        // component's `Disposition::Consumed` did.
        //
        // **Left only**, and the guard is load-bearing: without it a
        // Ctrl+Right-click on a toggle both flips the option and *claims* the
        // press, so it never reaches the theme inspector's pre-band — which
        // is exactly what the component's `press != Left => Pass` prevented.
        // Issue #2362's inspector test catches this.
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != fresh_ui::MouseButton::Left {
                    return None;
                }
                e.stop();
                Some(UiMsg::Action(action.clone()))
            }),
        )
        .on_enter(hover_msg(Some(option.hover_target())))
        // The tree owns this hover outright — nothing else will clear it when
        // the pointer moves into the gap between two toggles.
        .on_leave(hover_msg(None))
}

/// Where layout put each toggle, in screen cells.
///
/// The read-back half. `SearchOptionsLayout`'s `(start, end)` spans were
/// computed a second time by `compute`; these are the rectangles the one walk
/// assigned, offset into the frame the same way [`super::frame::regions_of`]
/// offsets a region's.
pub fn option_spans(
    ui: &fresh_ui::Ui<UiMsg>,
    size: ratatui::layout::Rect,
) -> Vec<(SearchOption, ratatui::layout::Rect)> {
    [
        SearchOption::CaseSensitive,
        SearchOption::WholeWord,
        SearchOption::Regex,
        SearchOption::ConfirmEach,
    ]
    .into_iter()
    .filter_map(|o| {
        let e = ui.find_by_key(&o.key())?;
        let r = ui.rect_of(e);
        // A toggle that is not on the row this frame still has an element
        // while the tree is reconciling; a zero-width rectangle is how that
        // shows, and it is not a span.
        if r.w == 0 {
            return None;
        }
        Some((
            o,
            ratatui::layout::Rect {
                x: size.x.saturating_add(r.x.max(0) as u16),
                y: size.y.saturating_add(r.y.max(0) as u16),
                width: r.w,
                height: r.h,
            },
        ))
    })
    .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::fold::{fold_native, Band};
    use crate::view::shell::frame::{frame_tree, region_key, Frame, HostRegion};
    use fresh_ui::{Size, Ui};
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;

    fn toggle_of(
        option: SearchOption,
        label: &str,
        shortcut: Option<&str>,
        checked: bool,
    ) -> Piece {
        Piece::Toggle(Toggle {
            option,
            label: label.to_string(),
            shortcut: shortcut.map(str::to_string),
            checked,
        })
    }

    /// The three toggles a plain search prompt shows.
    fn plain() -> SearchOptions {
        SearchOptions {
            pieces: vec![
                toggle_of(SearchOption::CaseSensitive, "Case", Some("Alt+C"), false),
                toggle_of(SearchOption::WholeWord, "Word", Some("Alt+W"), false),
                toggle_of(SearchOption::Regex, "Regex", None, false),
            ],
            hovered: None,
        }
    }

    fn laid_out(o: SearchOptions, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                search_options: Some(o),
                prompt_line: true,
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    fn row_text(o: SearchOptions, w: u16, h: u16) -> String {
        let ui = laid_out(o, w, h);
        let spec = ui.spec().clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        let palette = |k: &fresh_ui::ThemeKey| super::super::fold::test_palette::of(k.as_str());
        fold_native(&spec, &mut buf, &palette, Band::Background);
        let y = {
            let e = ui
                .find_by_key(&region_key(HostRegion::SearchOptions))
                .expect("the row");
            ui.rect_of(e).y as u16
        };
        (0..w).map(|x| buf[(x, y)].symbol().to_string()).collect()
    }

    /// The cells the painter wrote, character for character: one cell of left
    /// pad, three between toggles, the shortcut in parentheses after its
    /// label, and the row's own ground filling the rest.
    #[test]
    fn the_row_reads_the_way_the_painter_wrote_it() {
        assert_eq!(
            row_text(plain(), 52, 6),
            " [ ] Case (Alt+C)   [ ] Word (Alt+W)   [ ] Regex    ",
            "one pad, two three-cell gaps, three toggles, then the row's own ground"
        );
    }

    /// Checking a box changes the glyph and nothing else — `[x]` and `[ ]` are
    /// the same width, so nothing after it moves.
    #[test]
    fn checking_a_box_moves_nothing() {
        let mut o = plain();
        let Piece::Toggle(t) = &mut o.pieces[1] else {
            unreachable!()
        };
        t.checked = true;
        assert_eq!(
            row_text(o, 52, 6),
            " [ ] Case (Alt+C)   [x] Word (Alt+W)   [ ] Regex    "
        );
    }

    /// The capture-group hint sits between Regex and Confirm, with no
    /// separator of its own — Confirm still gets its three cells.
    #[test]
    fn the_hint_takes_no_separator_but_confirm_still_does() {
        let o = SearchOptions {
            pieces: vec![
                toggle_of(SearchOption::Regex, "Regex", None, true),
                Piece::Hint(" | $1".to_string()),
                toggle_of(SearchOption::ConfirmEach, "Confirm", None, false),
            ],
            hovered: None,
        };
        assert_eq!(row_text(o, 32, 6), " [x] Regex | $1   [ ] Confirm   ");
    }

    /// The spans the web projection routes clicks to are the rectangles layout
    /// assigned — read back, not recomputed. A toggle's span covers its
    /// checkbox, its label and its shortcut, which is the extent the old
    /// `checkbox_at` hit-tested.
    #[test]
    fn spans_are_read_back_from_the_layout() {
        let ui = laid_out(plain(), 52, 6);
        let spans = option_spans(&ui, Rect::new(0, 0, 52, 6));
        let cols: Vec<(SearchOption, u16, u16)> =
            spans.iter().map(|(o, r)| (*o, r.x, r.width)).collect();
        assert_eq!(
            cols,
            vec![
                (SearchOption::CaseSensitive, 1, 16),
                (SearchOption::WholeWord, 20, 16),
                (SearchOption::Regex, 39, 9),
            ],
            "one cell of pad, then each toggle's own width and three-cell gaps"
        );
    }

    /// A toggle that is not on the row has no span at all, rather than a
    /// zero-width one that would hit-test as a sliver.
    #[test]
    fn a_hidden_toggle_reports_no_span() {
        let ui = laid_out(plain(), 52, 6);
        let spans = option_spans(&ui, Rect::new(0, 0, 52, 6));
        assert!(!spans.iter().any(|(o, _)| *o == SearchOption::ConfirmEach));
    }

    /// A row too narrow for its toggles clips the last one and reports the
    /// clipped span, rather than reporting cells nobody drew. The old pair
    /// disagreed here by construction: the painter's `Paragraph` truncated
    /// while `compute` went on counting columns past the edge.
    #[test]
    fn a_narrow_row_clips_and_says_so() {
        assert_eq!(
            row_text(plain(), 44, 6),
            " [ ] Case (Alt+C)   [ ] Word (Alt+W)   [ ] R"
        );
        let ui = laid_out(plain(), 44, 6);
        let spans = option_spans(&ui, Rect::new(0, 0, 44, 6));
        assert_eq!(
            spans.last().map(|(o, r)| (*o, r.x, r.width)),
            Some((SearchOption::Regex, 39, 5)),
            "the clipped toggle's span stops where the row does"
        );
    }

    /// Every name this row paints in resolves against the real theme table.
    /// A misspelled key would fall back to the editor's ground and the row
    /// would render plainly rather than not at all — silent, without this.
    #[test]
    fn every_name_resolves() {
        let theme = crate::view::theme::Theme::from_json(r#"{"name":"test"}"#)
            .expect("a theme of nothing but defaults");
        for name in [base(), hovered(), checked(), muted()] {
            let (fg, bg) = name.split_once('/').expect("a pair");
            assert!(
                theme.resolve_theme_key(fg).is_some(),
                "unknown foreground key {fg:?}"
            );
            assert!(
                theme.resolve_theme_key(bg).is_some(),
                "unknown background key {bg:?}"
            );
        }
    }

    /// A press on a toggle produces its action and claims the event — the
    /// press, because that is when this row has always toggled, and claimed
    /// because the chrome component it replaces reported `Consumed`.
    #[test]
    fn a_press_toggles_and_is_claimed() {
        use fresh_ui::{Input, Mods, MouseButton, Point};
        let mut ui = laid_out(plain(), 52, 6);
        let e = ui
            .find_by_key(&SearchOption::WholeWord.key())
            .expect("the toggle");
        let r = ui.rect_of(e);
        let got = ui.dispatch(Input::Press {
            pos: Point::new(r.x + 1, r.y),
            button: MouseButton::Left,
            mods: Mods::default(),
        });
        assert!(got.claimed, "the press is spent on the toggle");
        assert!(
            matches!(
                got.msgs.as_slice(),
                [UiMsg::Action(
                    crate::input::keybindings::Action::ToggleSearchWholeWord
                )]
            ),
            "expected one toggle action, got {:?}",
            got.msgs
        );
    }

    /// A **right** press passes straight through: no message, no claim.
    ///
    /// The claim is the part that matters. Ctrl+Right-click is the theme
    /// inspector's gesture, and it reaches the inspector through the legacy
    /// pre-band — which only runs on events the tree declined. A toggle that
    /// claimed every button would flip the option *and* swallow the
    /// inspector, which is what the old component's `press != Left => Pass`
    /// was for.
    #[test]
    fn a_right_press_is_not_a_toggle_and_is_not_claimed() {
        use fresh_ui::{Input, Mods, MouseButton, Point};
        let mut ui = laid_out(plain(), 52, 6);
        let e = ui
            .find_by_key(&SearchOption::WholeWord.key())
            .expect("the toggle");
        let r = ui.rect_of(e);
        let got = ui.dispatch(Input::Press {
            pos: Point::new(r.x + 1, r.y),
            button: MouseButton::Right,
            mods: Mods::default(),
        });
        assert!(!got.claimed, "a right press must reach the legacy pre-band");
        assert!(got.msgs.is_empty(), "got {:?}", got.msgs);
    }

    /// Moving onto a toggle reports where the pointer is; the restyle is the
    /// next build's business, from state.
    #[test]
    fn moving_onto_a_toggle_reports_it() {
        use fresh_ui::{Input, Mods, Point};
        let mut ui = laid_out(plain(), 52, 6);
        let e = ui
            .find_by_key(&SearchOption::Regex.key())
            .expect("the toggle");
        let r = ui.rect_of(e);
        let got = ui.dispatch(Input::Move {
            pos: Point::new(r.x + 1, r.y),
            mods: Mods::default(),
        });
        assert!(
            !got.claimed,
            "a hover moves a highlight without taking the pointer"
        );
        assert_eq!(
            got.msgs
                .iter()
                .filter_map(|m| match m {
                    UiMsg::Ui(UiFact::Hover(t)) => Some(t.clone()),
                    _ => None,
                })
                .last(),
            Some(Some(HoverTarget::SearchOptionRegex))
        );
    }

    /// And the hover the row shows is the one it was built with — the tree
    /// reports the position, state remembers it, the next build restyles.
    #[test]
    fn a_hovered_toggle_paints_in_the_hover_pair() {
        let mut o = plain();
        o.hovered = Some(SearchOption::WholeWord);
        let ui = laid_out(o, 52, 6);
        let spec = ui.spec().clone();
        let names: Vec<String> = spec
            .in_flow()
            .iter()
            .map(|i| i.theme.as_str().to_string())
            .collect();
        assert!(
            names.iter().any(|n| n == &hovered()),
            "the hovered toggle names the hover pair; got {names:?}"
        );
    }
}
