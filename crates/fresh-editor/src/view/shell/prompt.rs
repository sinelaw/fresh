//! The prompt's suggestion list, as a description.
//!
//! The first surface where the migration is mostly *deletion by concept*. The
//! ledger (`docs/internal/fresh-ui-parity-ledger-prompt.md`) enumerated eleven
//! rules the painter and the chrome component enforce; ten of them are things
//! `fresh-ui` already says, so the work is naming the concept rather than
//! porting the code:
//!
//! ```text
//!   the visible window around the selection   -> list().windowed(..)
//!   hover reports a row, click selects it     -> list().on_select(..)
//!   double-click confirms                     -> list().on_activate(..)
//!   the selected row is highlighted           -> list().selected(i)
//!   a scrollbar that jumps and drags          -> list().scrollbar()   (hit.rs owns the drag)
//! ```
//!
//! And the column budget — the reason `Node::priority` exists. The row is four
//! columns and the rule is a *yield order*: names are never truncated while
//! room remains, the description absorbs the squeeze first, the source column
//! last. `flex` cannot say that; it resolves children against what is left, in
//! declaration order, which is placement rather than precedence. The status bar
//! had already written that rule out by hand as `left_budget`. Two surfaces
//! needing it is what made it a library concept instead of a second budget
//! function here.

use std::rc::Rc;

use fresh_ui::widgets::RowState;
use fresh_ui::{col, row, text, text_runs, Elide, Key, Node, Run, Sizing};

use crate::app::shell_host::shell_theme::{attrs, pair, with_bg, with_fg};

use super::msg::{UiFact, UiMsg};

/// How many rows the list shows at once. The painter's own constant, kept
/// where the description can see it.
pub use crate::view::prompt::MAX_VISIBLE_SUGGESTIONS;

/// `ColumnLayout::left_margin`: the gutter before the first column.
const LEFT_MARGIN: u16 = 2;

/// `ColumnLayout::column_spacing`: the least air between two columns.
const COLUMN_SPACING: u16 = 2;

/// Which column yields first when the row runs out of room.
///
/// The numbers are only an order — `Node::priority` compares them and nothing
/// else. Named rather than inlined because the *order* is the rule the painter
/// enforced in prose ("names are never truncated while room remains") and a
/// bare `.priority(3)` at each call site would lose it again.
mod yields_last {
    /// Sized first: a command palette that hides the command name has failed.
    pub const NAME: u8 = 3;
    /// The shortcut is short and fixed; it is not worth squeezing.
    pub const KEYBINDING: u8 = 2;
    /// Where a command came from — useful, but the first thing to lose after
    /// the description.
    pub const SOURCE: u8 = 1;
    /// Absorbs the squeeze. Default, stated for symmetry with the others.
    pub const DESCRIPTION: u8 = 0;
}

/// A piece of a description a plugin styled itself.
///
/// Already in the grammar rather than in colours: `fg` and `bg` are theme-key
/// names or `#rrggbb` literals, which is exactly what `shell_theme` reads. The
/// painter resolved a plugin's `OverlayColorSpec` to a concrete `Color` here
/// and lost its provenance on the way; a name keeps it, and a span that names
/// only one half inherits the other from the row it sits on.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DescriptionSpan {
    pub text: String,
    pub fg: Option<String>,
    pub bg: Option<String>,
    /// `shell_theme`'s attribute names: `bold`, `italic`, `underline`,
    /// `strikethrough`.
    pub attrs: Vec<&'static str>,
}

/// One row of the list, as content. No geometry: the columns are placed by
/// layout, and which of them survives a narrow row is `priority`'s answer.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SuggestionRow {
    pub name: String,
    pub keybinding: Option<String>,
    pub description: Option<String>,
    /// A description a plugin styled piece by piece. Wins over `description`,
    /// the same way `push_description_column` checks it first.
    pub description_spans: Option<Vec<DescriptionSpan>>,
    pub source: Option<String>,
    pub disabled: bool,
}

/// Where the list goes, and therefore what it looks like.
///
/// The editor grows two suggestion lists from one model. The bottom-anchored
/// prompt puts a bordered popup above its row; the floating-overlay prompt
/// puts a borderless one inside the card it has already drawn a frame around,
/// because a frame inside a frame is a double frame. They were two calls into
/// the same painter with a `with_border` flag between them and two copies of
/// the placement arithmetic — one in `render`, one in `chrome::Prompt::collect`
/// — that had to agree for a click to land.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Place {
    /// Above the prompt row, in its own bordered popup, flipping below when
    /// there is no room above.
    AbovePrompt,
    /// Filling the overlay card's results band, with no frame of its own.
    ///
    /// No rectangle: the band is a node in the same tree, so the layer names
    /// it and layout answers. Layers resolve after the main walk — that is
    /// what `Anchor::Node` is for — so the band has its rectangle by the time
    /// this one is placed, and nothing has to be measured twice or passed
    /// between two passes.
    InCard,
}

impl Default for Place {
    fn default() -> Self {
        Place::AbovePrompt
    }
}

impl Place {
    /// A frame of its own, or somebody else's.
    fn bordered(&self) -> bool {
        matches!(self, Place::AbovePrompt)
    }
}

/// The list itself.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Suggestions {
    pub rows: Vec<SuggestionRow>,
    /// Which row is selected, if any. Controlled: the editor holds it.
    pub selected: Option<usize>,
    pub place: Place,
    /// The quick-open mode hints, when the prompt is quick-open.
    ///
    /// Part of the list rather than beside it, because that is what it is:
    /// the painter drew it into a rectangle it computed as "the prompt row
    /// minus one", and the popup above it computed its own `y` as "the prompt
    /// row minus the popup minus that same one". Two subtractions that had to
    /// agree. Stacked in the layer, the agreement is the stacking.
    pub hints: Option<String>,
}

impl Suggestions {
    /// Which end of a name survives a narrow row.
    ///
    /// `ColumnLayout::names_are_paths` decided this from the shape of the list
    /// rather than from a flag: a list with neither keybindings nor sources is
    /// a file finder, and a path keeps its filename. A command palette keeps
    /// its head — "Toggle Compose/Preview (All Files)" contains a slash and is
    /// still a command name, which is the bug that rule was written for.
    fn name_elide(&self) -> Elide {
        let has = |f: fn(&SuggestionRow) -> bool| self.rows.iter().any(f);
        if has(|r| r.keybinding.is_some()) || has(|r| r.source.is_some()) {
            Elide::Tail
        } else {
            Elide::Head
        }
    }
}

pub fn row_key(index: usize) -> Key {
    Key::Pair("suggestion".into(), index as u64)
}

/// The name column of a row. Keyed so the width rule can be read back off the
/// tree — the same way the status bar's segments and the explorer's slots are.
pub fn name_key(index: usize) -> Key {
    Key::Pair("suggestion_name".into(), index as u64)
}

/// The painter's own ladder, in the painter's own keys.
///
/// **Read off `suggestion_style`, not invented.** `shell_theme`'s contract is
/// that a name is real theme keys — both halves go through
/// `Theme::resolve_theme_key`, and a name it does not know falls back to the
/// base style *silently*. An earlier draft of this used
/// `ui.suggestion_selected_fg`, `ui.suggestion_description_fg` and three more
/// that exist nowhere, which would have painted every row in the default
/// colour with nothing to show for it. `every_theme_name_is_a_real_key` is the
/// guard.
///
/// The state comes from `List` and the names come from here — that is what
/// `row_theme` is for. Without it the widget stamps its own vocabulary
/// (`list.row.selected`), which this editor's theme has no entry for, so every
/// row would have painted in the base style and the highlight would have
/// vanished. Hover included: `hovered` lives in `ListState`, so the ladder's
/// `menu_hover_*` arm is only reachable by being *told* the state.
///
/// One deliberate difference. The painter greys a disabled row with a
/// hardcoded `Color::DarkGray` + `DIM`, which no theme can reach;
/// `editor.line_number_fg` is the theme's own muted foreground and is what the
/// other migrated surfaces use for the same job. That makes a colour
/// themeable that was not.
fn row_bg(disabled: bool, st: RowState) -> &'static str {
    match st {
        RowState::Selected | RowState::SelectedBlur => "ui.suggestion_selected_bg",
        // A disabled row ignores hover — the painter's `row_base_style` picks
        // its background from `is_selected` alone.
        RowState::Hover if !disabled => "ui.menu_hover_bg",
        _ => "ui.suggestion_bg",
    }
}

/// The row's own style, and the default for every column that does not name
/// one — the painter's `base_style`.
fn theme(disabled: bool, st: RowState) -> String {
    let bg = row_bg(disabled, st);
    if disabled {
        // `dim` is new to the grammar and is what makes this a faithful port
        // rather than a near one: the painter reached for `Modifier::DIM`
        // directly, which no name could carry and no theme could override.
        return attrs("editor.line_number_fg", bg, &["dim"]);
    }
    match st {
        RowState::Selected | RowState::SelectedBlur => pair("ui.popup_selection_fg", bg),
        RowState::Hover => pair("ui.menu_hover_fg", bg),
        RowState::Normal => pair("ui.popup_text_fg", bg),
    }
}

/// A column with a foreground of its own. Disabled wins over all of them: the
/// painter returns `base_style` unchanged from every column's ladder, so a
/// greyed row is grey the whole way across.
fn column(disabled: bool, st: RowState, fg: &str) -> String {
    if disabled {
        theme(disabled, st)
    } else {
        pair(fg, row_bg(disabled, st))
    }
}

/// The source label is always dimmed — `source_style`'s three arms differ only
/// in background.
fn source_theme(disabled: bool, st: RowState) -> String {
    if disabled {
        return theme(disabled, st);
    }
    attrs("editor.line_number_fg", row_bg(disabled, st), &["dim"])
}

/// The keybinding reads as a shortcut on a row the eye is on, and recedes to
/// the muted foreground otherwise — `keybinding_style`'s three arms.
fn keybinding_theme(disabled: bool, st: RowState) -> String {
    let fg = match st {
        RowState::Normal => "editor.line_number_fg",
        _ => "ui.help_key_fg",
    };
    column(disabled, st, fg)
}

/// Every name this module can hand to `shell_theme`. The guard test walks it;
/// nothing else should need it.
#[cfg(test)]
fn every_theme_name() -> Vec<String> {
    let states = [
        RowState::Normal,
        RowState::Hover,
        RowState::Selected,
        RowState::SelectedBlur,
    ];
    let mut out = Vec::new();
    for st in states {
        for disabled in [false, true] {
            out.push(theme(disabled, st));
            out.push(keybinding_theme(disabled, st));
            out.push(source_theme(disabled, st));
        }
    }
    out
}

/// A plugin's span as a run, layered over the row's own name.
///
/// Half-named on purpose: `styled_span_style` started from the row's style and
/// set only what the span mentioned, so a span that names a foreground keeps
/// the selection's background under it. `with_fg` and `with_bg` are that, in
/// the grammar.
fn span_run(sp: &DescriptionSpan, row: &str) -> Run {
    let mut name = row.to_string();
    if let Some(fg) = &sp.fg {
        name = with_fg(&name, fg);
    }
    if let Some(bg) = &sp.bg {
        name = with_bg(&name, bg);
    }
    if !sp.attrs.is_empty() {
        let (fg, bg) = name.split_once('/').unwrap_or((name.as_str(), ""));
        let bg = bg.split('+').next().unwrap_or(bg);
        name = attrs(fg, bg, &sp.attrs);
    }
    Run::themed(sp.text.clone(), name)
}

/// One row's four columns, in paint order, each carrying the priority that says
/// when it yields.
fn node_row(index: usize, r: &SuggestionRow, st: RowState, name_elide: Elide) -> Node<UiMsg> {
    let t = theme(r.disabled, st);
    // `ColumnLayout::left_margin`, as a cell rather than as two leading spaces
    // in a span. It carries the row's own fill because the row container
    // paints under it, so it needs no theme of its own — and it is fixed, so
    // it never enters the yield order.
    let mut cells: Vec<Node<UiMsg>> = vec![row().w(Sizing::Cells(LEFT_MARGIN))];
    cells.push(
        text(r.name.clone())
            .theme(t.clone())
            .key(name_key(index))
            .elide(name_elide)
            .priority(yields_last::NAME),
    );

    // A flexible gap rather than padding: it is what puts the trailing columns
    // at the right edge, and it is also `ColumnLayout::column_spacing` — the
    // painter's fixed two cells between columns, here as whatever is left over,
    // with `min_w` keeping the painter's floor when the row is tight.
    cells.push(row().flex(1).min_w(COLUMN_SPACING));

    // The description carries no colour of its own: the painter draws it in
    // `base_style`, so it is the row. A plugin's styled description is the
    // same run with its pieces named — one node either way, because the pieces
    // are one logical string and must wrap and elide as one.
    match (&r.description_spans, &r.description) {
        (Some(spans), _) => cells.push(
            text_runs(spans.iter().map(|sp| span_run(sp, &t)))
                .theme(t.clone())
                .elide(Elide::Tail)
                .priority(yields_last::DESCRIPTION),
        ),
        (None, Some(d)) => cells.push(
            text(d.clone())
                .theme(t.clone())
                .elide(Elide::Tail)
                .priority(yields_last::DESCRIPTION),
        ),
        (None, None) => {}
    }
    if let Some(k) = &r.keybinding {
        cells.push(
            text(k.clone())
                .theme(keybinding_theme(r.disabled, st))
                .priority(yields_last::KEYBINDING),
        );
    }
    if let Some(sc) = &r.source {
        cells.push(
            text(sc.clone())
                .theme(source_theme(r.disabled, st))
                .elide(Elide::Tail)
                .priority(yields_last::SOURCE),
        );
    }

    // No theme on the row itself: `row_theme` names it, and a name here would
    // be overwritten anyway.
    row().h(Sizing::Cells(1)).children(cells)
}

/// A press that reaches the popup's own chrome stops there.
///
/// `chrome:suggestions` was a `pointer_opaque` box over the popup's outer rect
/// whose whole job was this: a click on the border, or on the padding below the
/// last row, must not fall through and move the buffer cursor underneath.
///
/// The library stops the *hit search* at any box that draws — "a plain
/// container is a surface until it says it is not" — but claiming is a separate
/// question from hitting: `Dispatch::claimed` is true only when a handler said
/// `stop()`, because producing a message and taking the event are different
/// things. So the absorb is one handler, and it is the same one the explorer's
/// panel body uses for the same reason.
///
/// Any button: the old box absorbed the right-click too, which is what kept the
/// buffer's context menu from opening through the palette.
fn absorb(n: Node<UiMsg>) -> Node<UiMsg> {
    use fresh_ui::{gesture, Event, GestureKind};
    gesture(n).on(
        GestureKind::Press,
        Rc::new(|ev: &Event| {
            // Rows stop their own press, so this fires only where none did.
            ev.stop();
            None
        }),
    )
}

/// The suggestion list as a description.
///
/// `windowed` is what replaces the painter's `scroll_offset` bookkeeping: the
/// library asks for the rows it can show and the editor resolves each index
/// against its own storage, so no window is stored on either side.
pub fn suggestions(s: &Suggestions) -> Node<UiMsg> {
    let rows: Vec<SuggestionRow> = s.rows.clone();
    let selected = s.selected;
    let rows_for_row = Rc::new(rows);
    let rows_for_key = rows_for_row.clone();
    let rows_for_theme = rows_for_row.clone();
    let name_elide = s.name_elide();

    // `List` reports the state it holds; the names are this module's. Both the
    // row builder and `row_theme` need the state, and only the latter is given
    // it — so the builder paints the columns and the row's own fill comes from
    // here. `selected` is consulted rather than the widget's state because an
    // empty selection is a real prompt state and `List` has no way to say "no
    // row": with none set it falls back to row 0.
    let hover_state = move |i: usize, st: RowState| -> RowState {
        match st {
            RowState::Selected | RowState::SelectedBlur if selected != Some(i) => RowState::Normal,
            other => other,
        }
    };

    let mut list = fresh_ui::widgets::List::windowed(
        rows_for_key.len(),
        move |i| row_key(i),
        move |i| match rows_for_row.get(i) {
            Some(r) => node_row(
                i,
                r,
                if selected == Some(i) {
                    RowState::Selected
                } else {
                    RowState::Normal
                },
                name_elide,
            ),
            None => row().h(Sizing::Cells(1)),
        },
    )
    .row_theme(move |i, st| {
        let st = hover_state(i, st);
        theme(rows_for_theme.get(i).is_some_and(|r| r.disabled), st)
    })
    .scrollbar()
    // A click reports the row; what that *means* is the prompt type's
    // business — `select_suggestion` confirms when `click_confirms()` says a
    // click commits, and otherwise syncs the input. That decision was already
    // editor-side; what the list removes is the coordinate hit-test in front
    // of it (`handle_click_suggestions` recovering an index the row knew).
    //
    // A double click always commits, `click_confirms` or not: it is the
    // mouse-only commit path for the prompts that preview on a single click.
    // Both handlers can be set now that `activate_on` says *which* click
    // activates — before it, the widget fired activation on the first and let
    // it win, so setting both confirmed every click.
    .activate_on(fresh_ui::widgets::Activate::DoubleClick)
    .on_select(|i| UiMsg::Ui(UiFact::SuggestionSelect(i)))
    .on_activate(|i| UiMsg::Ui(UiFact::SuggestionConfirm(i)));
    if let Some(i) = selected {
        list = list.selected(i);
    }

    // Keyed so the window can be read back off the tree: `spec.index` maps
    // this key to the range of items the list produced, and the scrollbar
    // among them is where the viewport reports where its window sits.
    col()
        .key(LIST_KEY.with(|k| k.clone()))
        .children([fresh_ui::ComponentExt::node(list)])
}

/// The popup around the list: its frame and its ground.
///
/// The painter's `Block::default().borders(ALL).border_style(popup_border_fg)
/// .style(bg(suggestion_bg))`, and the `Paragraph` that padded the unused rows
/// with the same background. `border()` is the ring, and a themed box already
/// fills before its content, so the padding rows are what the fill does anyway.
fn popup(s: &Suggestions) -> Node<UiMsg> {
    let body = col()
        .key(POPUP_KEY.with(|k| k.clone()))
        .theme(pair("ui.popup_border_fg", "ui.suggestion_bg"))
        .child(suggestions(s));
    if !s.place.bordered() {
        // Inside a card, the card decides how tall this is.
        return absorb(body);
    }
    // How tall the box is, in the terms the painter used: as many rows as it
    // has, up to the cap, plus the ring. A content rule rather than a
    // measurement — `suggestion_count.min(MAX_VISIBLE) as u16 + 2` was the same
    // sentence in `render`, and it belongs with the list it describes because
    // nothing outside knows the cap.
    let visible = s.rows.len().min(MAX_VISIBLE_SUGGESTIONS) as u16;
    absorb(body.border().h(Sizing::Cells(visible + 2)))
}

/// The list as an overlay above the prompt row.
///
/// **Placement is declared, not computed.** The painter measured the row
/// count, subtracted it from the prompt line's `y`, and flipped the box below
/// when it would not fit above — arithmetic that had to agree with a second
/// copy in `chrome::Prompt::collect` for the click rail to hit the right
/// cells. `Anchor::Node` names the row it sits on and `Place::Above` says
/// which side, with `Fit::FLIP` for the case that used to be an `if`.
pub fn suggestions_layer(s: &Suggestions) -> Node<UiMsg> {
    use fresh_ui::{layer, Anchor, Fit};
    let l = layer().key(LAYER_KEY.with(|k| k.clone()));
    let l = match &s.place {
        Place::AbovePrompt => l
            .anchor(Anchor::Node(super::frame::region_key(
                super::frame::HostRegion::PromptLine,
            )))
            .place(fresh_ui::Place::Above)
            // As wide as the row it sits above, which is what `width =
            // chrome.width` said in `render` — with the difference that the
            // row's width is now something the tree knows rather than
            // something the caller measures and passes in.
            .stretch_to_anchor()
            .fit(Fit::FLIP.or(Fit::CLAMP)),
        // The card's results band, by name. `Fill` takes its whole rectangle,
        // and no `Fit` — a band the card carved out of the frame is already
        // inside it.
        Place::InCard => l
            .anchor(Anchor::Node(super::overlay_prompt::region_key(
                super::overlay_prompt::CardRegion::Results,
            )))
            .place(fresh_ui::Place::Fill),
    };
    // Not modal. The old encoding covered the frame below z15 to stop a
    // click reaching the *body*, which is a rule about a host leaf rather
    // than about this layer — see the ledger's withdrawn finding D.
    let card = popup(s);
    match &s.hints {
        Some(h) => l.child(col().children([card, hints_row(h)])),
        None => l.child(card),
    }
}

/// The quick-open mode hints, as the row under the popup.
///
/// `render_quick_open_hints`, which was a `Paragraph` of three spans — a
/// two-cell margin, the string, and enough trailing spaces to reach the right
/// edge — all in one style. The padding spans are what a themed box fills with,
/// and the margin is the same fixed cell the rows use, so what is left is the
/// text and its name.
fn hints_row(text_of: &str) -> Node<UiMsg> {
    let t = attrs(
        "editor.line_number_fg",
        "ui.suggestion_selected_bg",
        &["dim"],
    );
    row().h(Sizing::Cells(1)).theme(t.clone()).children([
        row().w(Sizing::Cells(LEFT_MARGIN)),
        text(text_of.to_string()).theme(t).elide(Elide::Tail),
    ])
}

thread_local! {
    static LAYER_KEY: Key = Key::Str("prompt_suggestions".into());
    static LIST_KEY: Key = Key::Str("prompt_suggestion_list".into());
    static POPUP_KEY: Key = Key::Str("prompt_suggestion_popup".into());
}

/// Where the popup landed, read back off the laid-out tree.
///
/// The box, not the layer: with quick-open hints the layer is one row taller,
/// and every consumer of this — the click rail's absorb rect, the web `Scene`
/// — means the box.
///
/// The partner of `frame::regions_of` for a surface that is not a host region,
/// the same shape as `context_menu::menu_rect`. It replaces
/// `ChromeLayout::suggestions_outer_area`, which `render` recorded and the
/// click rail and the web `Scene` read back.
pub fn suggestions_rect(spec: &fresh_ui::LayoutSpec) -> Option<fresh_ui::Rect> {
    let key = POPUP_KEY.with(|k| k.clone());
    spec.index
        .iter()
        .find(|(k, _)| *k == key)
        .and_then(|(_, r)| spec.items.get(r.start).map(|i| i.rect))
}

/// The rows' own rectangle — inside the ring and above the hints.
///
/// What `SuggestionsRenderer` returned as `inner_rect`, and what the hover and
/// click rails hit-tested against while they still worked in coordinates.
pub fn suggestions_list_rect(spec: &fresh_ui::LayoutSpec) -> Option<fresh_ui::Rect> {
    let key = LIST_KEY.with(|k| k.clone());
    spec.index
        .iter()
        .find(|(k, _)| *k == key)
        .and_then(|(_, r)| spec.items.get(r.start).map(|i| i.rect))
}

/// The scrollbar's track, when the list has one.
///
/// `render` computed this by hand — one column at the popup's right edge, only
/// when `total > visible` — and cached it in `ChromeLayout` for the drag
/// handlers. The viewport emits the bar as an item when it needs one, so its
/// presence and its rectangle are the same answer.
pub fn suggestions_scrollbar_rect(spec: &fresh_ui::LayoutSpec) -> Option<fresh_ui::Rect> {
    let key = LIST_KEY.with(|k| k.clone());
    let range = spec.index.iter().find(|(k, _)| *k == key)?.1.clone();
    spec.items[range]
        .iter()
        .find(|i| matches!(i.draw, fresh_ui::Draw::Scrollbar { .. }))
        .map(|i| i.rect)
}

/// Which slice of the list is on screen, read back off the laid-out tree.
///
/// **The framework owns the window; this reads it, it does not set it.** The
/// painter took its slice from `Prompt::scroll_offset`, which the wheel
/// handler, the scrollbar handlers and the click rail all wrote to and read
/// from — five places agreeing by hand about one number. A `viewport` owns its
/// window, reveals the selection when it moves, and reports where it is on the
/// scrollbar it already emits; everything else on this side becomes a reader.
///
/// `(first, visible)`, in rows. A list that fits its box emits no scrollbar and
/// answers `(0, rows)`, which is the same thing said cheaply.
pub fn suggestions_window(spec: &fresh_ui::LayoutSpec) -> Option<(usize, usize)> {
    let key = LIST_KEY.with(|k| k.clone());
    let range = spec.index.iter().find(|(k, _)| *k == key)?.1.clone();
    spec.items[range]
        .iter()
        .find_map(|i| match &i.draw {
            fresh_ui::Draw::Scrollbar {
                offset,
                content,
                window,
            } => Some((*offset as usize, (*window as usize).min(*content as usize))),
            _ => None,
        })
        .or(Some((0, 0)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_ui::{Input, Mods, MouseButton, Point, Size, Ui};

    fn rows(n: usize) -> Vec<SuggestionRow> {
        (0..n)
            .map(|i| SuggestionRow {
                name: format!("command-{i}"),
                ..SuggestionRow::default()
            })
            .collect()
    }

    fn laid_out(s: Suggestions, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(suggestions(&s), Size::new(w, h));
        ui
    }

    /// **Every name is a real theme key.**
    ///
    /// `shell_theme::resolve` splits a name on `/`, sends each half through
    /// `Theme::resolve_theme_key`, and falls back to the editor's plain ground
    /// when either half is unknown — silently. An earlier draft of this module
    /// used five keys that exist nowhere (`ui.suggestion_selected_fg` among
    /// them); nothing failed, every row would simply have painted in the
    /// default colour. This is the only thing that catches that.
    #[test]
    fn every_theme_name_is_a_real_key() {
        use crate::view::theme::Theme;
        let theme = Theme::from_json(r#"{"name":"test"}"#).expect("defaults");
        for name in every_theme_name() {
            // `names` is the grammar's own reader: it drops the `+attrs` tail
            // and reports each half only when it is a name rather than a
            // `#rrggbb` literal. Nothing here should be a literal.
            let (fg, bg) = crate::app::shell_host::shell_theme::names(&name);
            for half in [fg, bg] {
                let half = half.unwrap_or_else(|| panic!("{name:?} has an unnamed half"));
                assert!(
                    theme.resolve_theme_key(half).is_some(),
                    "{half:?} (in {name:?}) is not a theme key"
                );
            }
        }
    }

    /// **Ledger rule 2: clicking a row selects it — by index, not by
    /// coordinate.** `handle_click_suggestions` hit-tested a recorded
    /// rectangle to recover an index the list already had.
    ///
    /// A press *and* a release, because that is what `fresh_ui::widgets::List`
    /// derives a click from — and the web frontend now sends both for a chrome
    /// control (`sendClick`). Asserting on the press alone would have passed
    /// only by changing the library to match a host bug.
    #[test]
    fn a_click_on_a_row_reports_that_row() {
        let mut ui = laid_out(
            Suggestions {
                rows: rows(5),
                selected: Some(0),
                place: Place::AbovePrompt,
                hints: None,
            },
            40,
            8,
        );
        let r = ui.rect_of(ui.find_by_key(&row_key(2)).expect("row 2"));
        let at = Point::new(r.x + 1, r.y);
        let mut msgs = ui
            .dispatch(Input::press(at, MouseButton::Left, Mods::NONE))
            .msgs;
        msgs.extend(
            ui.dispatch(Input::release(at, MouseButton::Left, Mods::NONE))
                .msgs,
        );
        assert!(
            msgs.iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::SuggestionSelect(2)))),
            "got {msgs:?}"
        );
    }

    /// **Ledger rule 3, and finding B closed: a double click confirms.**
    ///
    /// The first click selects — `select_suggestion` decides for itself
    /// whether that also commits — and the second always commits, which is the
    /// mouse-only commit path for a prompt that previews on a single click.
    #[test]
    fn a_double_click_confirms_and_a_single_one_only_selects() {
        let mut ui = laid_out(
            Suggestions {
                rows: rows(5),
                selected: Some(0),
                place: Place::AbovePrompt,
                hints: None,
            },
            40,
            8,
        );
        let r = ui.rect_of(ui.find_by_key(&row_key(2)).expect("row 2"));
        let at = Point::new(r.x + 1, r.y);
        let mut click = |n: u8| {
            let mut out = ui
                .dispatch(Input::press_n(at, MouseButton::Left, Mods::NONE, n))
                .msgs;
            out.extend(
                ui.dispatch(Input::release(at, MouseButton::Left, Mods::NONE))
                    .msgs,
            );
            out
        };
        assert!(
            click(1)
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::SuggestionSelect(2)))),
            "the first click selects"
        );
        assert!(
            click(2)
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::SuggestionConfirm(2)))),
            "the second confirms"
        );
    }

    /// **The window is read back, not stored.**
    ///
    /// `Prompt::scroll_offset` was written and read by five places — the
    /// painter's slice, the wheel, the scrollbar's click and its drag, and the
    /// click rail's `start_idx` — which agreed by hand. The viewport owns it
    /// and reports it, so a wheel over the list moves the window and everyone
    /// reading it sees the same number by construction.
    #[test]
    fn the_window_follows_the_wheel_and_is_read_back() {
        use fresh_ui::Axis;
        let mut ui: Ui<UiMsg> = Ui::new();
        let tree = || {
            suggestions(&Suggestions {
                rows: rows(100),
                selected: Some(0),
                place: Place::AbovePrompt,
                hints: None,
            })
        };
        ui.frame(tree(), Size::new(40, 8));
        assert_eq!(
            suggestions_window(ui.spec()).map(|(f, _)| f),
            Some(0),
            "a fresh list starts at the top"
        );
        ui.dispatch(Input::Wheel {
            pos: Point::new(4, 4),
            axis: Axis::Vertical,
            delta: 3,
            mods: Mods::NONE,
        });
        ui.frame(tree(), Size::new(40, 8));
        let (first, visible) = suggestions_window(ui.spec()).expect("a window");
        assert!(first > 0, "the wheel moved the window, got {first}");
        assert!(
            visible > 0 && visible <= 8,
            "the window is what fits, got {visible}"
        );
    }

    /// **Ledger rule 1: at most `MAX_VISIBLE_SUGGESTIONS` rows exist.** The
    /// painter kept a `scroll_offset` window by hand; `windowed` is the
    /// concept, and a list far longer than the viewport must not build a node
    /// per item.
    #[test]
    fn a_long_list_builds_only_the_rows_it_can_show() {
        let ui = laid_out(
            Suggestions {
                rows: rows(1000),
                selected: Some(0),
                place: Place::AbovePrompt,
                hints: None,
            },
            40,
            MAX_VISIBLE_SUGGESTIONS as u16,
        );
        let built = (0..1000)
            .filter(|i| ui.find_by_key(&row_key(*i)).is_some())
            .count();
        assert!(
            built <= MAX_VISIBLE_SUGGESTIONS + 2,
            "windowed list built {built} rows for a {}-row viewport",
            MAX_VISIBLE_SUGGESTIONS
        );
    }

    /// **The popup has a frame and a ground of its own.**
    ///
    /// `Block::default().borders(ALL).border_style(popup_border_fg)
    /// .style(bg(suggestion_bg))`, and a `Paragraph` that padded the rows the
    /// list did not fill with the same background. Here the ring is
    /// `border()`, and the ground is what a themed box already fills with
    /// before its content — so the padding rows stop being something anyone
    /// writes.
    #[test]
    fn the_popup_frames_the_list_and_insets_it() {
        let mut ui: Ui<UiMsg> = Ui::new();
        let s = Suggestions {
            rows: rows(3),
            selected: Some(0),
            place: Place::AbovePrompt,
            hints: None,
        };
        let spec = ui.frame(popup(&s), Size::new(40, 6)).clone();
        assert!(
            spec.items
                .iter()
                .any(|i| matches!(i.draw, fresh_ui::Draw::Border)
                    && i.theme.as_str() == "ui.popup_border_fg/ui.suggestion_bg"),
            "the popup draws its own ring"
        );
        // Inside the ring and past the gutter: one cell of border plus the
        // painter's two-cell `left_margin`.
        let name = ui.rect_of(ui.find_by_key(&name_key(0)).expect("a name"));
        assert_eq!(
            name.x,
            1 + LEFT_MARGIN as i32,
            "the first column clears the border and the gutter"
        );
    }

    /// **Ledger rule 5: the list sits above the prompt row.**
    ///
    /// The painter measured the row count, subtracted it from the prompt
    /// line's `y`, and a second copy of that arithmetic in
    /// `chrome::Prompt::collect` had to agree for clicks to land. Here it is
    /// `Anchor::Node` + `Place::Above`, and this reads the answer back off the
    /// laid-out tree rather than off a recorded rectangle.
    #[test]
    fn the_list_is_placed_above_the_prompt_row() {
        use crate::view::shell::frame::{frame_tree, region_key, Frame, HostRegion};
        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui
            .frame(
                frame_tree(Frame {
                    prompt_line: true,
                    suggestions: Some(Suggestions {
                        rows: rows(3),
                        selected: Some(0),
                        place: Place::AbovePrompt,
                        hints: None,
                    }),
                    ..Frame::default()
                }),
                Size::new(60, 20),
            )
            .clone();
        let list = suggestions_rect(&spec).expect("the list was placed");
        let prompt = ui.rect_of(ui.find_by_key(&region_key(HostRegion::PromptLine)).unwrap());
        assert!(
            list.bottom() <= prompt.y,
            "the list must sit above the prompt row: list {list:?}, prompt {prompt:?}"
        );
    }

    /// **A path keeps its filename; a command keeps its head.**
    ///
    /// `ColumnLayout::names_are_paths` read this off the shape of the list —
    /// neither keybindings nor sources means a file finder — and the two ends
    /// are not interchangeable: `truncate_head_ellipsis` exists so a long path
    /// still shows what file it is, and the tail form exists because "Toggle
    /// Compose/Preview (All Files)" contains a slash and is still a command
    /// name. That was the bug the rule was written for.
    #[test]
    fn a_path_gives_up_its_head_and_a_command_its_tail() {
        let painted = |r: SuggestionRow| {
            let s = Suggestions {
                rows: vec![r],
                selected: Some(0),
                place: Place::AbovePrompt,
                hints: None,
            };
            let ui = laid_out(s, 16, 4);
            let spec = ui.spec();
            let id = ui.find_by_key(&name_key(0)).expect("the name column");
            let rect = ui.rect_of(id);
            spec.items
                .iter()
                .find(|i| i.rect == rect && matches!(&i.draw, fresh_ui::Draw::Lines(_)))
                .and_then(|i| match &i.draw {
                    fresh_ui::Draw::Lines(l) => l.first().map(|s| s.to_string()),
                    _ => None,
                })
                .expect("the name painted")
        };
        // Neither keybinding nor source: a file finder. The filename survives.
        let path = painted(SuggestionRow {
            name: "src/view/shell/prompt.rs".into(),
            ..SuggestionRow::default()
        });
        assert!(
            path.ends_with("prompt.rs") && path.starts_with('…'),
            "a path must keep its filename, got {path:?}"
        );
        // A keybinding makes it a command palette. The head survives.
        let cmd = painted(SuggestionRow {
            name: "Toggle Compose/Preview (All Files)".into(),
            keybinding: Some("^P".into()),
            ..SuggestionRow::default()
        });
        assert!(
            cmd.starts_with("Toggle") && cmd.ends_with('…'),
            "a command name must keep its head, got {cmd:?}"
        );
    }

    /// **A plugin's styled description keeps its pieces, and inherits the
    /// rest of the row.**
    ///
    /// `styled_span_style` started from the row's style and set only what the
    /// span mentioned, so a span naming a foreground still sat on the
    /// selection's background. It also resolved the plugin's colour to a
    /// concrete `Color` on the way, which the theme inspector could not
    /// explain afterwards; a `ThemeKey` spec now stays a key.
    #[test]
    fn a_styled_span_names_only_what_it_overrides() {
        let s = Suggestions {
            rows: vec![SuggestionRow {
                name: "cmd".into(),
                description_spans: Some(vec![
                    DescriptionSpan {
                        text: "hit".into(),
                        fg: Some("editor.warning_fg".into()),
                        attrs: vec!["bold"],
                        ..DescriptionSpan::default()
                    },
                    DescriptionSpan {
                        text: " rest".into(),
                        ..DescriptionSpan::default()
                    },
                ]),
                ..SuggestionRow::default()
            }],
            selected: Some(0),
            place: Place::AbovePrompt,
            hints: None,
        };
        let ui = laid_out(s, 40, 4);
        let themes: Vec<(String, String)> = ui
            .spec()
            .items
            .iter()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => l
                    .first()
                    .map(|t| (t.to_string(), i.theme.as_str().to_string())),
                _ => None,
            })
            .collect();
        let of = |needle: &str| {
            themes
                .iter()
                .find(|(t, _)| t.contains(needle))
                .map(|(_, k)| k.clone())
                .unwrap_or_else(|| panic!("no run for {needle:?} in {themes:?}"))
        };
        // The span's own foreground, the row's background, the row's selected
        // state — all three, from one name.
        assert_eq!(
            of("hit"),
            "editor.warning_fg/ui.suggestion_selected_bg+bold",
            "a span keeps the row under it"
        );
        // A span that overrides nothing is the row.
        assert_eq!(of("rest"), theme(false, RowState::Selected));
    }

    /// **One list, two placements.** The overlay prompt draws its own frame
    /// around the results band, so its list is borderless and fills that band;
    /// the bottom-anchored prompt brings its own popup and sits above the row.
    /// They were two calls into one painter with a `with_border` flag between
    /// them, and two copies of the placement arithmetic that had to agree for a
    /// click to land.
    ///
    /// The band is named, not measured. `Anchor::Node` resolves after the main
    /// walk, so the card has placed it by the time this layer is placed — no
    /// rectangle passes between the two, and there is no second pass.
    #[test]
    fn a_list_placed_in_the_card_fills_its_band_and_brings_no_frame() {
        use crate::view::shell::frame::{frame_tree, Frame};
        use crate::view::shell::overlay_prompt::{
            region_key as band_key, Card, CardRegion, PREVIEW_MIN_COLS,
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui
            .frame(
                frame_tree(Frame {
                    prompt_line: true,
                    card: Some(Card {
                        at: fresh_ui::Rect::new(4, 2, PREVIEW_MIN_COLS + 20, 30),
                        toolbar_rows: 2,
                        footer: true,
                    }),
                    suggestions: Some(Suggestions {
                        rows: rows(3),
                        selected: Some(0),
                        place: Place::InCard,
                        hints: None,
                    }),
                    ..Frame::default()
                }),
                Size::new(200, 50),
            )
            .clone();
        let band = ui.rect_of(
            ui.find_by_key(&band_key(CardRegion::Results))
                .expect("the band"),
        );
        assert!(band.w > 0 && band.h > 0, "the card placed a results band");
        assert_eq!(
            suggestions_rect(&spec),
            Some(band),
            "the list fills the band the card measured"
        );
        assert!(
            !spec
                .items
                .iter()
                .any(|i| matches!(i.draw, fresh_ui::Draw::Border)
                    && i.theme.as_str() == "ui.popup_border_fg/ui.suggestion_bg"
                    && i.rect == band),
            "a frame inside the card's frame is a double frame"
        );
    }

    /// **The popup is as wide as the prompt row.** `render` said
    /// `width = chrome.width` and passed it in; the layer takes it from the
    /// row it is anchored to, so the two cannot drift when a dock opens and
    /// the chrome column narrows.
    #[test]
    fn the_popup_spans_the_prompt_row() {
        use crate::view::shell::frame::{frame_tree, region_key, Frame, HostRegion};
        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui
            .frame(
                frame_tree(Frame {
                    prompt_line: true,
                    // A dock takes cells off the left, so the prompt row is
                    // narrower than the frame — which is the case the passed-in
                    // width had to be kept in step with.
                    dock: Some(12),
                    suggestions: Some(Suggestions {
                        rows: rows(3),
                        selected: Some(0),
                        place: Place::AbovePrompt,
                        hints: None,
                    }),
                    ..Frame::default()
                }),
                Size::new(60, 20),
            )
            .clone();
        let prompt = ui.rect_of(ui.find_by_key(&region_key(HostRegion::PromptLine)).unwrap());
        let popup = suggestions_rect(&spec).expect("placed");
        assert_eq!((popup.x, popup.w), (prompt.x, prompt.w));
    }

    /// **A long list does not make a tall box.** `render` capped the popup's
    /// height at `MAX_VISIBLE_SUGGESTIONS + 2`; a layer measures its content,
    /// so without the cap said here a thousand-row palette would ask for a
    /// thousand-row box.
    #[test]
    fn the_popup_stops_at_the_visible_row_cap() {
        let tall = |n: usize| {
            let mut ui: Ui<UiMsg> = Ui::new();
            ui.frame(
                col().child(suggestions_layer(&Suggestions {
                    rows: rows(n),
                    selected: Some(0),
                    place: Place::AbovePrompt,
                    hints: None,
                })),
                Size::new(60, 40),
            );
            ui.rect_of(
                ui.find_by_key(&POPUP_KEY.with(|k| k.clone()))
                    .expect("the box"),
            )
            .h
        };
        assert_eq!(
            tall(3),
            3 + 2,
            "a short list is its own height, plus the ring"
        );
        assert_eq!(
            tall(1000),
            MAX_VISIBLE_SUGGESTIONS as u16 + 2,
            "a long one stops at the cap"
        );
    }

    /// **A press on the popup's own chrome stops there.**
    ///
    /// `chrome:suggestions` was a `pointer_opaque` box over the outer rect
    /// whose only job was this: a click on the border, or on the ground below
    /// the last row, must not fall through and move the buffer cursor
    /// underneath. The library stops the hit *search* at any box that draws,
    /// but claiming is a separate question — `Dispatch::claimed` is true only
    /// when a handler said `stop()` — so the absorb is one handler.
    ///
    /// Any button, because the old box absorbed the right-click too, which is
    /// what kept the buffer's context menu from opening through the palette.
    #[test]
    fn a_press_on_the_popup_chrome_is_claimed_and_says_nothing() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            col().child(suggestions_layer(&Suggestions {
                rows: rows(3),
                selected: Some(0),
                place: Place::AbovePrompt,
                hints: None,
            })),
            Size::new(60, 20),
        );
        let box_rect = ui.rect_of(
            ui.find_by_key(&POPUP_KEY.with(|k| k.clone()))
                .expect("the box"),
        );
        // The border's own cell: inside the popup, outside every row.
        let corner = Point::new(box_rect.x, box_rect.y);
        for button in [MouseButton::Left, MouseButton::Right] {
            let got = ui.dispatch(Input::press(corner, button, Mods::NONE));
            assert!(
                got.claimed,
                "{button:?} on the border must not fall through"
            );
            assert!(got.msgs.is_empty(), "and must say nothing: {:?}", got.msgs);
        }
    }

    /// **A list that overflows gets a scrollbar; one that fits does not.**
    ///
    /// `overflowing_list_draws_scrollbar_on_right_border` and
    /// `fitting_list_keeps_plain_right_border` in the painter, which drew the
    /// bar over the popup's right border when `suggestions.len() > visible`
    /// and left the border plain otherwise — and, in the overlay's borderless
    /// form, carved a column off the list to put it in. A viewport emits the
    /// bar exactly when its content overflows and reserves the lane itself, so
    /// the presence, the rectangle and the lane are one answer.
    #[test]
    fn the_scrollbar_appears_only_when_the_list_overflows() {
        let bar = |n: usize| {
            let mut ui: Ui<UiMsg> = Ui::new();
            ui.frame(
                suggestions(&Suggestions {
                    rows: rows(n),
                    selected: Some(0),
                    place: Place::AbovePrompt,
                    hints: None,
                }),
                Size::new(40, 6),
            );
            suggestions_scrollbar_rect(ui.spec())
        };
        assert!(bar(3).is_none(), "three rows in six: no bar");
        let over = bar(60).expect("sixty rows in six: a bar");
        assert_eq!(over.w, 1, "one column of track");
        assert_eq!(over.x, 39, "at the list's right edge");
    }

    /// **Ledger finding A: the column yield order.** The name is sized before
    /// the description, so a row too narrow for both keeps the whole command
    /// name and truncates the description — never the other way round. This is
    /// what `left_budget` says for the status bar and what `Node::priority`
    /// replaced for both.
    #[test]
    fn the_description_yields_before_the_name() {
        let one = |w: u16| {
            let s = Suggestions {
                rows: vec![SuggestionRow {
                    name: "a-long-command-name".into(),
                    description: Some("a-long-description".into()),
                    ..SuggestionRow::default()
                }],
                selected: Some(0),
                place: Place::AbovePrompt,
                hints: None,
            };
            let ui = laid_out(s, w, 4);
            ui.rect_of(ui.find_by_key(&name_key(0)).expect("the name column"))
                .w
        };
        // Wide enough for both: the name is whole.
        assert_eq!(one(60), 19, "the name fits at its natural width");
        // Too narrow for both: the name is still whole — the description gave
        // up its cells first. 28 rather than 24 because the row now carries
        // the painter's own gutters: two cells of `left_margin` and two of
        // `column_spacing` are not the name's to give.
        assert_eq!(one(28), 19, "the name kept its width; the description paid");
    }
}
