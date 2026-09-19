//! The terminal's stylesheet: a table from class name to appearance.
//!
//! **The companion table to [`shell_theme`](super::shell_host::shell_theme),
//! and deliberately a different question.** A theme key answers *how is this
//! painted* and its vocabulary is a grammar, because colour combines. A class
//! answers *what is this*, and its vocabulary is a list, because structure does
//! not: a button is a button whether or not it is focused.
//!
//! This is the terminal backend's half of that answer. The web's half is CSS,
//! reading the same class list off the same item, and neither backend is told
//! what the other does. See `docs/internal/retained-mode-ui.md` "The shell's stylesheet".
//!
//! **A [`Rule`] carries only what is implemented** — a border, horizontal
//! padding, and ink. No margin, no fill, no shadow, and no vertical padding
//! either: a property that is named but unimplemented is worse than one that
//! is absent, because it invites a caller the code cannot serve.
//!
//! **Geometry is derived, never declared.** [`Rule::reserved_x`] is the sides
//! the border occupies plus the padding, and it is the one arithmetic both
//! readers use — the description builder to reserve the columns, the fold to
//! find the cell it draws the glyph in. There is no second number to keep in
//! agreement with the first.

use fresh_core::api::ButtonKind;

use super::shell_host::shell_theme::{Attrs, Ink, Paint};
use crate::widgets::render::{
    KEY_DANGER_FG, KEY_FOCUSED_BG, KEY_FOCUSED_FG, KEY_HELP_KEY_FG, KEY_HOVER_BG,
};

/// The theme key for a control that is present but inert.
const KEY_DISABLED_FG: &str = "ui.menu_disabled_fg";

/// A declaration block: the parts of an appearance a rule states.
///
/// **An absent half is unstated, not default.** That is the whole of the
/// cascade: `button.hover` names a background and says nothing about the
/// foreground, so a Danger button stays red while answering the pointer —
/// which is exactly what the ladder it replaces did by spreading `..base`.
///
/// Attributes are the one field that *replaces* rather than accumulating, and
/// it has to be: `button.disabled` must be able to say "not bold" over a
/// Primary base, and a union could only ever add.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Decl {
    pub fg: Option<Paint>,
    pub bg: Option<Paint>,
    pub attrs: Option<Attrs>,
}

impl Decl {
    fn fg(key: &'static str) -> Decl {
        Decl {
            fg: Some(Paint::key(key)),
            ..Decl::default()
        }
    }

    fn bg(key: &'static str) -> Decl {
        Decl {
            bg: Some(Paint::key(key)),
            ..Decl::default()
        }
    }

    fn attrs(mut self, attrs: Attrs) -> Decl {
        self.attrs = Some(attrs);
        self
    }

    /// Merge another declaration over this one, property by property. Later
    /// wins where it states something; where it is silent, this stands.
    fn under(mut self, over: &Decl) -> Decl {
        self.fg = over.fg.clone().or(self.fg);
        self.bg = over.bg.clone().or(self.bg);
        self.attrs = over.attrs.or(self.attrs);
        self
    }

    pub fn is_empty(&self) -> bool {
        *self == Decl::default()
    }

    /// This declaration over a concrete ink — the surface the control sits on.
    ///
    /// Unstated halves leave the surface's own, which is how a button with no
    /// background of its own is drawn *on* the panel rather than punching a
    /// hole in it.
    ///
    /// **Attributes add here, though they replace in the cascade**, and the
    /// difference is not a slip. Within a class list, stating them has to be
    /// able to *clear* — that is the whole of `button.disabled` over a bold
    /// Primary. Against the surface there is nothing to clear: a run has
    /// always been drawn with its own attributes on top of the row's, so a
    /// button on a bold surface stays bold, exactly as `ink_of` composes every
    /// other styled run in the shell. Getting this backwards would have been
    /// invisible in every theme whose surfaces carry no attributes, which is
    /// most of them.
    pub fn over(&self, under: Ink) -> Ink {
        let mut ink = under;
        if let Some(fg) = self.fg.clone() {
            ink = ink.with_fg(fg);
        }
        if let Some(bg) = self.bg.clone() {
            ink = ink.with_bg(bg);
        }
        match self.attrs {
            Some(a) => ink.plus(a),
            None => ink,
        }
    }
}

/// What a class draws on the edges of its own box.
///
/// **An affix is a border.** `[ New Task… ]` is a left side of `[`, a cell of
/// padding, the label, a cell of padding, a right side of `]` — the same shape
/// a card or a divider has, with different glyphs. That is what makes this one
/// mechanism rather than a frame rule and a padding rule that must agree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Border {
    /// Glyphs on the sides that have them. Top and bottom arrive with their
    /// first user; nothing yet needs a rule that draws a row.
    Sides {
        l: Option<&'static str>,
        r: Option<&'static str>,
    },
}

impl Border {
    /// The columns one side occupies, by display width.
    fn width(side: Option<&'static str>) -> u16 {
        side.map(fresh_ui::glyph::width).unwrap_or(0)
    }

    fn sides(&self) -> (u16, u16) {
        match self {
            Border::Sides { l, r } => (Border::width(*l), Border::width(*r)),
        }
    }
}

/// What one class declares.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Rule {
    pub border: Option<Border>,
    /// Columns of padding inside the border, on each side. Vertical padding
    /// waits for its first user, and per-side padding waits for a `Pad` that
    /// can express it — see the note on [`Rule::reserved_x`].
    pub pad_x: u16,
    pub ink: Decl,
}

impl Rule {
    /// The columns this rule reserves on **each** side: the border glyph plus
    /// the padding.
    ///
    /// **Derived, never declared.** The builder reserves these columns and the
    /// fold draws the glyph in the outermost one, so a frame and the room made
    /// for it cannot drift apart — there is one datum with two readings.
    ///
    /// One number rather than two because `fresh_ui`'s `Pad` is symmetric. No
    /// rule needs otherwise yet: `[` and `]` are one cell each. A rule whose
    /// sides differ in width needs per-side padding in the library first, and
    /// the assertion below is what will say so.
    /// Merge another rule over this one, property by property.
    ///
    /// A `pad_x` of zero is not a statement: no padding and zero padding are
    /// the same cells, so there is nothing an explicit "zero" could mean that
    /// silence does not already say.
    fn under(self, over: &Rule) -> Rule {
        Rule {
            border: over.border.clone().or(self.border),
            pad_x: match over.pad_x {
                0 => self.pad_x,
                n => n,
            },
            ink: self.ink.under(&over.ink),
        }
    }

    pub fn reserved_x(&self) -> u16 {
        let (l, r) = self.border.as_ref().map(Border::sides).unwrap_or((0, 0));
        debug_assert_eq!(
            l, r,
            "an asymmetric border needs per-side padding in `fresh_ui` before \
             it can be honest about the columns it reserves"
        );
        l.max(r) + self.pad_x
    }

    /// Where this rule's side glyphs go, given the box's own rectangle: the
    /// left one in its first column, the right one ending at its last.
    ///
    /// **The one placement, for every backend that draws cells.** The terminal
    /// fold calls this, and so does the mirror the widget tests fold with —
    /// the alternative is two spellings of `[` and `]` that agree until one of
    /// them is edited. Vertically centred, because a side is a side of the
    /// whole box; every framed control is one row tall today, so this is the
    /// same cell either way, but a two-row box would otherwise put its
    /// brackets on the top row without anyone deciding that.
    pub fn side_glyphs(&self, rect: fresh_ui::Rect) -> Vec<(i32, i32, &'static str)> {
        let Some(Border::Sides { l, r }) = self.border.as_ref() else {
            return Vec::new();
        };
        if rect.w == 0 || rect.h == 0 {
            return Vec::new();
        }
        let y = rect.y + (rect.h as i32 - 1) / 2;
        let mut out = Vec::new();
        if let Some(g) = l {
            out.push((rect.x, y, *g));
        }
        if let Some(g) = r {
            let w = i32::from(fresh_ui::glyph::width(g)).min(rect.w as i32);
            out.push((rect.x + rect.w as i32 - w, y, *g));
        }
        out
    }
}

/// The table.
///
/// A class the table has never heard of contributes nothing and is not an
/// error — that is how a stylesheet degrades, and it is what lets a class the
/// terminal has no opinion about reach the web's stylesheet intact.
pub fn rule(class: &str) -> Option<Rule> {
    // The framed control, and the only rule with geometry so far. Its ink is
    // empty on purpose: a plain button is drawn in the surrounding surface's
    // own colours, and what `button` contributes is its shape.
    //
    // **The shape is `Frame::BUTTON`'s, not this table's.** The widget
    // runtime's text projection composes `[ Label ]` from the same glyphs, so
    // there is one definition of what a button looks like and two renderers of
    // it — which is the duplication this design set out to end.
    if class == "button" {
        let f = crate::widgets::frame::Frame::BUTTON;
        return Some(Rule {
            border: Some(Border::Sides { l: f.l, r: f.r }),
            pad_x: f.pad_x,
            ink: Decl::default(),
        });
    }
    let ink = match class {
        // A bare affordance — a `×` close glyph — is a button with no frame,
        // because the frame exists to give a *word* the shape of a control and
        // a glyph already has one. It is spelled as its own class rather than
        // as `button` plus an override, because nothing in the box model can
        // yet say "no border", and inventing that word for one caller is the
        // kind of unimplemented property this design refuses.
        "button.bare" => Decl::default(),
        // Primary marks the affirmative action with a bold, strong foreground
        // drawn directly on the surrounding surface — no opinionated
        // background. Focus is the only state that paints one.
        "button.primary" => Decl::fg(KEY_HELP_KEY_FG).attrs(Attrs::BOLD),
        // Danger gets the error foreground, bold, on the surrounding surface —
        // the same foreground-only treatment as Primary.
        "button.danger" => Decl::fg(KEY_DANGER_FG).attrs(Attrs::BOLD),
        // Hover states a background and nothing else, so it paints its band
        // *under* whatever intent the button already carries.
        "button.hover" => Decl::bg(KEY_HOVER_BG),
        "button.focused" => Decl {
            fg: Some(Paint::key(KEY_FOCUSED_FG)),
            bg: Some(Paint::key(KEY_FOCUSED_BG)),
            attrs: Some(Attrs::BOLD),
        },
        // Disabled overrides intent: a "Delete" that is not available should
        // not still scream red. It states its attributes to *clear* the bold a
        // Primary or Danger base declared.
        "button.disabled" => Decl::fg(KEY_DISABLED_FG).attrs(Attrs::NONE),
        _ => return None,
    };
    Some(Rule {
        ink,
        ..Rule::default()
    })
}

/// Resolve a class list, applying each class in order — later ones win,
/// property by property. This is the whole of the cascade: `button` +
/// `button.danger` + `button.disabled` is N + M entries rather than N × M,
/// which is the answer to the combinatorial blow-up that made the shell give
/// up on class names the first time.
pub fn cascade(classes: &str) -> Rule {
    classes
        .split_whitespace()
        .filter_map(rule)
        .fold(Rule::default(), |acc, r| acc.under(&r))
}

/// The classes a button of this state wears.
///
/// **At most one state class, because the ladder it mirrors is exclusive.**
/// Hover outranks focus rather than merging with it — the pointer is the more
/// immediate signal — so a button that is both hovered and focused is spelled
/// `button.hover` alone, and a hovered Danger button keeps its red because
/// hover states no foreground. Kind is not a state and rides alongside; a
/// disabled Primary button therefore carries both, and `button.disabled`
/// winning is what makes "disabled overrides intent" a property of the cascade
/// rather than of the caller.
///
/// `bare` is not a state either — it is the call site saying *this control is a
/// glyph, not a word*, which is structure and belongs in the description. It
/// replaces `button` rather than joining it, because `button` is the class that
/// declares the frame.
pub fn button_classes(
    kind: ButtonKind,
    bare: bool,
    focused: bool,
    hovered: bool,
    disabled: bool,
) -> String {
    let mut out = String::from(match bare {
        true => "button.bare",
        false => "button",
    });
    match kind {
        ButtonKind::Normal => {}
        ButtonKind::Primary => out.push_str(" button.primary"),
        ButtonKind::Danger => out.push_str(" button.danger"),
    }
    if disabled {
        out.push_str(" button.disabled");
    } else if hovered {
        out.push_str(" button.hover");
    } else if focused {
        out.push_str(" button.focused");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_core::api::{OverlayColorSpec, OverlayOptions};

    /// The three fields the ladder and the table both have opinions about,
    /// reduced to something comparable across the two spellings.
    #[derive(Debug, PartialEq, Eq)]
    struct Look {
        fg: Option<String>,
        bg: Option<String>,
        bold: bool,
    }

    fn key_of(paint: &Paint) -> String {
        paint
            .name()
            .expect("every colour the button table names is a theme key")
            .to_string()
    }

    fn from_decl(decl: &Decl) -> Look {
        Look {
            fg: decl.fg.as_ref().map(key_of),
            bg: decl.bg.as_ref().map(key_of),
            bold: decl.attrs.is_some_and(|a| a.contains(Attrs::BOLD)),
        }
    }

    fn from_overlay(options: Option<&OverlayOptions>) -> Look {
        let key = |c: &OverlayColorSpec| match c {
            OverlayColorSpec::ThemeKey(k) => k.clone(),
            OverlayColorSpec::Rgb(r, g, b) => {
                panic!("the ladder named a literal #{r:02x}{g:02x}{b:02x}")
            }
        };
        Look {
            fg: options.and_then(|o| o.fg.as_ref()).map(key),
            bg: options.and_then(|o| o.bg.as_ref()).map(key),
            bold: options.is_some_and(|o| o.bold),
        }
    }

    /// A state class does not disturb the frame: `button` states the geometry
    /// once and the cascade carries it, so a focused button is the same width
    /// as a resting one and the row never reflows as focus moves.
    #[test]
    fn state_does_not_change_the_boxs_width() {
        let plain = cascade("button").reserved_x();
        for state in ["button.focused", "button.hover", "button.disabled"] {
            assert_eq!(
                cascade(&format!("button button.danger {state}")).reserved_x(),
                plain,
                "{state} moved the frame"
            );
        }
    }

    /// The cascade is what makes "disabled overrides intent" a property of the
    /// table rather than of whoever builds the class list: the kind class is
    /// still there, and the state class beats it — foreground *and* the bold
    /// it has to clear.
    #[test]
    fn disabled_beats_the_kind_it_is_written_after() {
        let ink = cascade("button button.danger button.disabled").ink;
        assert_eq!(
            ink.fg.as_ref().map(key_of).as_deref(),
            Some(KEY_DISABLED_FG)
        );
        assert_eq!(
            ink.attrs,
            Some(Attrs::NONE),
            "the bold Danger declared is cleared"
        );
        assert!(
            button_classes(ButtonKind::Danger, false, false, false, true).contains("button.danger"),
            "the kind is not dropped on the way in — the cascade is what settles it"
        );
    }

    /// A class the table has never heard of contributes nothing and is not an
    /// error. That is what lets a theme author or a plugin name something the
    /// terminal has no opinion about and style it in CSS alone.
    #[test]
    fn an_unknown_class_decorates_nothing() {
        assert!(rule("wobble").is_none());
        assert_eq!(
            cascade("button wobble button.primary"),
            cascade("button button.primary"),
        );
    }

    /// An unstated half leaves the surface's own, which is how a button with
    /// no background of its own is drawn *on* the panel rather than punching a
    /// hole in it.
    #[test]
    fn an_unstated_half_leaves_the_surface_alone() {
        let surface = Ink::keys("ui.panel_fg", "ui.panel_bg");
        let hovered = cascade("button button.hover").ink.over(surface.clone());
        assert_eq!(hovered.fg, surface.fg, "hover states no foreground");
        assert_eq!(key_of(&hovered.bg), KEY_HOVER_BG);

        let plain = cascade("button").ink.over(surface.clone());
        assert_eq!(plain, surface, "a plain button declares no ink at all");
    }
}

/// The ink a button paints: its class list resolved over the surface it sits
/// on, with any style the *spec* declared for this call site folded in.
///
/// **A spec-declared style is the escape hatch every stylesheet needs**, and
/// the two it has are not the same kind of thing:
///
/// - `resting` stands where the intent classes would. It replaced the kind's
///   look outright in the ladder this came from, and the state classes still
///   layer over it — a call site that declares a resting look still gets the
///   focus band.
/// - `declared_hover` replaces the look outright: the call site asked for this
///   exact answer while the pointer is on it, and nothing layers over that.
///
/// Both are ignored while the button is disabled, because an inert control
/// advertising a live one's colours would lie. The class list is read for that
/// rather than passed a second flag, so there is one place the fact lives.
pub fn button_ink(
    classes: &str,
    surface: &Ink,
    resting: Option<&Ink>,
    declared_hover: Option<&Ink>,
) -> Ink {
    let disabled = classes.split_whitespace().any(|c| c == "button.disabled");
    if let Some(hover) = declared_hover.filter(|_| !disabled) {
        return hover.clone();
    }
    let resting = resting.filter(|_| !disabled);
    let ground = resting.cloned().unwrap_or_else(|| surface.clone());
    classes
        .split_whitespace()
        .filter(|c| resting.is_none() || !is_intent(c))
        .filter_map(rule)
        .fold(ground, |ink, r| r.ink.over(ink))
}

/// Whether a class names what a button is *for*, as opposed to what it is or
/// what state it is in. These are the ones a spec-declared resting style
/// stands in for.
fn is_intent(class: &str) -> bool {
    matches!(class, "button.primary" | "button.danger")
}

#[cfg(test)]
mod ink_tests {
    use super::*;
    use crate::app::shell_host::shell_theme::Paint;

    fn surface() -> Ink {
        Ink::keys("ui.panel_fg", "ui.panel_bg")
    }

    fn key(p: &Paint) -> String {
        p.name().expect("a theme key").to_string()
    }

    /// **Attributes add against the surface even though they replace within
    /// the cascade.** A run has always been drawn with its own attributes on
    /// top of the row's, so a button on a bold surface stays bold — including
    /// a disabled one, whose `Attrs::NONE` clears the *class list's* bold and
    /// not the surface's. Replacing here instead would have been invisible in
    /// every theme whose surfaces carry no attributes, which is most of them.
    #[test]
    fn a_button_on_an_attributed_surface_keeps_the_surfaces_attributes() {
        let bold_surface = surface().plus(Attrs::BOLD);
        let classes = button_classes(ButtonKind::Danger, false, false, false, true);
        let ink = button_ink(&classes, &bold_surface, None, None);
        assert!(
            ink.attrs.contains(Attrs::BOLD),
            "the surface's own bold survives a disabled button"
        );
        assert_eq!(key(&ink.fg), KEY_DISABLED_FG);
    }

    /// A resting style declared by the spec stands where the intent classes
    /// would — and the *state* classes still layer over it, which is what
    /// keeps a call site that declares its own colours answering focus.
    #[test]
    fn a_declared_resting_style_replaces_the_intent_but_not_the_state() {
        let resting = Ink::keys("plugin.fg", "plugin.bg");
        let classes = button_classes(ButtonKind::Primary, false, true, false, false);

        let ink = button_ink(&classes, &surface(), Some(&resting), None);
        assert_eq!(key(&ink.fg), KEY_FOCUSED_FG, "focus still wins");
        assert_eq!(key(&ink.bg), KEY_FOCUSED_BG);

        let resting_only = button_ink(
            &button_classes(ButtonKind::Primary, false, false, false, false),
            &surface(),
            Some(&resting),
            None,
        );
        assert_eq!(
            resting_only, resting,
            "with no state, the declared style is the whole answer — the \
             Primary foreground it replaced does not come back"
        );
    }

    /// Neither escape hatch reaches a disabled control: an inert button
    /// wearing a live one's colours would lie about what a click does.
    #[test]
    fn a_disabled_button_ignores_both_declared_styles() {
        let declared = Ink::keys("plugin.fg", "plugin.bg");
        let classes = button_classes(ButtonKind::Primary, false, false, false, true);
        for (resting, hover) in [(Some(&declared), None), (None, Some(&declared))] {
            let ink = button_ink(&classes, &surface(), resting, hover);
            assert_eq!(key(&ink.fg), KEY_DISABLED_FG);
            assert_eq!(key(&ink.bg), "ui.panel_bg", "the surface's own ground");
        }
    }

    /// A declared hover replaces the look outright: the call site asked for
    /// this exact answer while the pointer is on it.
    #[test]
    fn a_declared_hover_replaces_the_look_outright() {
        let declared = Ink::keys("plugin.fg", "plugin.bg");
        let classes = button_classes(ButtonKind::Danger, false, true, true, false);
        assert_eq!(
            button_ink(&classes, &surface(), None, Some(&declared)),
            declared,
        );
    }
}
