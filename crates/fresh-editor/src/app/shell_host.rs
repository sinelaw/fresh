//! The buffer split grid as a host region — the load-bearing `Host` leaf.
//!
//! This is the seam the whole migration stands on: the text pipeline keeps its
//! existing logic and is reached from the fold, given a rectangle and a cell
//! buffer. `SplitRenderer::render_content` already paints into an arbitrary
//! `Buffer` at an arbitrary `Rect`, so nothing in it changes.
//!
//! What this file exists to prove is the **borrow**. `render_content` takes
//! ~28 parameters because it needs `WindowBuffers::with_all_mut`'s disjoint
//! split — `(&mut buffers, &SplitManager, &mut view_states)` — plus config and
//! theme off the editor. The open question was whether that can be assembled
//! *inside a fold callback*, while the display list being folded is borrowed
//! from the `Ui`. It can, on one condition: the `Ui` must not live on the
//! `Editor`. See `fold`'s module documentation.
//!
//! The assembly below mirrors `Editor::render`'s, one for one. The per-frame
//! *state* arguments (hover targets, LSP waiting, cursor hiding) are taken as
//! [`BodyState`] rather than recomputed here: they are plain values, they play
//! no part in the borrow question, and threading the real ones is a mechanical
//! step for the wave that puts this on the render path.

use std::collections::{HashMap, HashSet};

use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::Style;

use crate::app::types::ViewLineMapping;
use crate::app::Editor;
use crate::model::event::LeafId;
use crate::view::ui::split_rendering::SplitRenderer;
use crate::view::ui::{EditorRenderConfig, RenderStyle};

use crate::view::shell::fold::Caret;
use crate::view::shell::frame::HostRegion;

/// Per-frame facts the split renderer needs that are not borrows.
#[derive(Clone, Copy, Debug, Default)]
pub struct BodyState {
    pub lsp_waiting: bool,
    pub hide_cursor: bool,
    pub hovered_close_split: Option<LeafId>,
    pub hovered_maximize_split: Option<LeafId>,
}

/// What the split grid publishes back across the seam.
///
/// These are the caches chrome reads *after* paint today. Under the shell the
/// fold owns them, keyed by host region, and hands them to whatever needs them
/// on the next event — click-to-byte mapping, most of all.
#[derive(Default)]
pub struct BodyOutput {
    pub view_line_mappings: HashMap<LeafId, Vec<ViewLineMapping>>,
    pub tab_layouts: HashMap<LeafId, crate::view::ui::tabs::TabLayout>,
}

/// Paint the split grid into `area`, writing the caret it wants into `caret`.
///
/// The signature is [`super::fold::HostPainter::paint_host`]'s, specialised to
/// the body: this is what the `Editor`'s implementation of that trait calls.
pub fn paint_body(
    editor: &mut Editor,
    area: Rect,
    buf: &mut Buffer,
    caret: &mut Caret,
    state: BodyState,
) -> BodyOutput {
    // Built before the `&mut editor.windows` borrow below; it only borrows
    // `editor.config`, so the two coexist — as in `Editor::render`.
    let cfg = EditorRenderConfig::new(
        &editor.config.editor,
        editor.background_fade,
        editor.software_cursor_only,
    );
    let draw_tab_bar = !editor.suppress_chrome_cells;
    let session_mode = editor.session_mode || !editor.software_cursor_only;
    let screen_width = buf.area.width;
    let active_window_id = editor.active_window;

    let win = match editor.windows.get_mut(&active_window_id) {
        Some(w) => w,
        None => return BodyOutput::default(),
    };

    let is_maximized = win
        .buffers
        .splits()
        .map(|(mgr, _)| mgr.is_maximized())
        .unwrap_or(false);
    let tab_bar_visible = win.tab_bar_visible;
    let metadata_ref = &win.buffer_metadata;
    let preview_buffer = win.preview.map(|(_, b)| b);
    let scrollback_view_splits: HashSet<LeafId> = win
        .buffers
        .splits()
        .map(|(_, vs_map)| {
            vs_map
                .iter()
                .filter(|(leaf, svs)| win.split_terminal_scrollback(**leaf, svs.active_buffer))
                .map(|(leaf, _)| *leaf)
                .collect()
        })
        .unwrap_or_default();
    let event_logs_mut = &mut win.event_logs;
    let grouped_ref = &win.grouped_subtrees;
    let composite_buffers_mut = &mut win.composite_buffers;
    let composite_view_states_mut = &mut win.composite_view_states;
    let cell_theme_map_mut = &mut win.chrome_layout.cell_theme_map;

    let rendered = win.buffers.with_all_mut(|buffers_mut, mgr, vs_map| {
        // The theme read-guard lives only for the render call.
        let theme_guard = editor.theme.read().unwrap();
        let style = RenderStyle {
            theme: &theme_guard,
            ansi_background: editor.ansi_background.as_ref(),
            cfg,
        };
        SplitRenderer::render_content(
            buf,
            area,
            &*mgr,
            buffers_mut,
            metadata_ref,
            preview_buffer,
            event_logs_mut,
            composite_buffers_mut,
            composite_view_states_mut,
            style,
            state.lsp_waiting,
            Some(vs_map),
            grouped_ref,
            state.hide_cursor,
            None, // hovered_tab
            state.hovered_close_split,
            state.hovered_maximize_split,
            is_maximized,
            tab_bar_visible,
            session_mode,
            &scrollback_view_splits,
            cell_theme_map_mut,
            screen_width,
            caret,
            draw_tab_bar,
        )
    });

    match rendered {
        Some((_, tab_layouts, _, _, view_line_mappings, _, _)) => BodyOutput {
            view_line_mappings,
            tab_layouts,
        },
        None => BodyOutput::default(),
    }
}

/// The editor as the fold's host.
///
/// During the migration this is what shrinks: every region still listed here
/// is one the old painters own, and each stage moves one of them out into a
/// native `fresh-ui` description until only [`HostRegion::Body`] — the buffer
/// and terminal grid — is left. That last one never migrates.
impl crate::view::shell::fold::HostPainter for Editor {
    fn paint_host(&mut self, region: HostRegion, rect: Rect, buf: &mut Buffer, caret: &mut Caret) {
        match region {
            HostRegion::Body => {
                // TODO(migration S1): thread the real per-frame state instead
                // of the default, and publish `BodyOutput` to the geometry
                // bridge. Neither affects the borrow, which is what this path
                // exists to establish.
                let _ = paint_body(self, rect, buf, caret, BodyState::default());
            }
            // Native already — the tree paints these, and the fold never
            // reaches here for them because a native region emits no
            // `Draw::Host`. Listed so that un-migrating one is a compile
            // error rather than a blank row.
            HostRegion::MenuBar | HostRegion::SearchOptions | HostRegion::Explorer => {}
            // Still painted by `Editor::render`; moved out one stage at a
            // time.
            HostRegion::Dock | HostRegion::StatusBar | HostRegion::PromptLine => {}
        }
    }
}

/// The backend's half of theming: a theme name resolved to concrete colours.
///
/// `fresh-ui` never says what anything looks like — an item carries a
/// [`ThemeKey`], a name for *where its appearance comes from*, and mapping that
/// name is the backend's job. This is the same lookup `*Colors::from_theme`
/// performs for the existing controls, expressed once for the shell.
///
/// It is a snapshot of the colours rather than a borrow of the theme, so the
/// fold can hold it while the rest of the editor is mutably borrowed.
pub struct ShellPalette {
    theme: std::sync::Arc<crate::view::theme::Theme>,
}

impl crate::view::shell::fold::Palette for ShellPalette {
    fn style(&self, theme: &fresh_ui::ThemeKey) -> Style {
        shell_theme::resolve(theme.as_str(), &self.theme)
    }
}

/// The shell's theme names, and the one rule for reading them.
///
/// **A name is real theme keys, not a name of our own.** A cell needs a
/// foreground and a background, and an `Item` carries exactly one `ThemeKey` —
/// so a shell name is a *pair*, written `fg_key/bg_key`, optionally followed by
/// a text attribute (`+bold`, `+underline`). Both halves go through
/// [`Theme::resolve_theme_key`], the editor's existing table-generated
/// resolver, so no name is invented here and every colour on screen traces to
/// a theme entry a user can edit.
///
/// This replaced a hand-written match of twenty-odd arms over names like
/// `menu.bar.item.active.mnemonic` — six spellings for two orthogonal
/// attributes, which is the combinatorial blow-up that arrives in earnest with
/// the file explorer (git status × selection × cut × focus). A grammar does not
/// blow up; a list of names does.
///
/// It also converges with the theme inspector, which has always recorded
/// provenance as exactly this pair (`ThemeRun { fg_key, bg_key }`). The display
/// list and the inspector now say the same thing in the same words.
pub mod shell_theme {
    use ratatui::style::{Color, Modifier, Style};

    use crate::view::theme::Theme;

    /// Build a name from two theme keys.
    pub fn pair(fg: &str, bg: &str) -> String {
        format!("{fg}/{bg}")
    }

    /// The same, with text attributes the theme does not carry.
    ///
    /// Reserved for attributes that are *structural* rather than themed: a
    /// mnemonic is underlined because it is a mnemonic. They compose with any
    /// pair and with each other, so this is grammar rather than more names.
    pub fn attrs(fg: &str, bg: &str, attrs: &[&str]) -> String {
        let mut out = pair(fg, bg);
        for a in attrs {
            out.push('+');
            out.push_str(a);
        }
        out
    }

    /// Resolve a shell name to a concrete style.
    ///
    /// An unreadable or unknown name falls back to the editor's own ground
    /// rather than failing, so a surface that has not been themed yet renders
    /// plainly instead of not at all.
    pub fn resolve(name: &str, theme: &Theme) -> Style {
        let mut parts = name.split('+');
        let pair = parts.next().unwrap_or(name);
        let mut modifier = Modifier::empty();
        for a in parts {
            modifier |= match a {
                "bold" => Modifier::BOLD,
                "underline" => Modifier::UNDERLINED,
                _ => Modifier::empty(),
            };
        }
        let Some((fg_key, bg_key)) = pair.split_once('/') else {
            return base(theme);
        };
        let (Some(fg), Some(bg)) = (resolve_half(fg_key, theme), resolve_half(bg_key, theme))
        else {
            return base(theme);
        };
        // The attribute the theme declares for the foreground key, plus the
        // structural one the name asked for.
        let modifier = modifier | theme.resolve_modifier_key(fg_key);
        Style::default().fg(fg).bg(bg).add_modifier(modifier)
    }

    /// One half of a pair: a theme key, or the `#rrggbb` literal escape.
    ///
    /// **The literal is an interim, and it is the only thing here that is not
    /// traceable to a theme entry.** It exists because a plugin can hand the
    /// editor an `OverlayColorSpec::Rgb` — an arbitrary runtime value that no
    /// theme ever declared, so there is no key to name it with. Today those
    /// colours are already untraceable: `resolve_overlay_color` turns them
    /// straight into `Color::Rgb`, which the theme inspector cannot explain and
    /// a user cannot override. Writing them as `#rrggbb` here loses nothing
    /// that exists and unblocks the surfaces that carry them.
    ///
    /// What replaces it: plugins **register** their colours as named keys
    /// (`plugin.git.status_added_fg`) and `resolve_theme_key` gains a dynamic
    /// tier for them, at which point a plugin colour becomes an ordinary,
    /// inspectable, user-overridable name and this arm can go. See §6.2 of the
    /// migration doc. Nothing in this repository emits a literal: every
    /// in-tree slot provider already sends a `ThemeKey`.
    fn resolve_half(key: &str, theme: &Theme) -> Option<Color> {
        if let Some(rest) = key.strip_prefix('#') {
            return match rest.as_bytes() {
                // `#7ee787` — a 24-bit literal.
                _ if rest.len() == 6 && rest.bytes().all(|b| b.is_ascii_hexdigit()) => {
                    let byte = |i: usize| u8::from_str_radix(&rest[i..i + 2], 16).ok();
                    Some(Color::Rgb(byte(0)?, byte(2)?, byte(4)?))
                }
                // `#i42` — a palette index.
                [b'i', ..] => rest[1..].parse().ok().map(Color::Indexed),
                // `#Yellow` — one of the sixteen names.
                _ => crate::view::theme::named_color_from_str(rest),
            };
        }
        theme.resolve_theme_key(key)
    }

    /// The two halves of a pair, where each is a *name* rather than a literal.
    ///
    /// A cell's theme-key provenance, read back out of the grammar instead of
    /// carried beside it. A half that is a literal (`#7ee787`) has no name by
    /// construction — that is what a literal *is* — and reports `None`, which
    /// is the honest answer for a colour a plugin supplied.
    pub fn names<'a>(theme: &'a str) -> (Option<&'a str>, Option<&'a str>) {
        let body = theme.split('+').next().unwrap_or(theme);
        let (fg, bg) = match body.split_once('/') {
            Some(p) => p,
            None => (body, ""),
        };
        let named =
            |h: &'a str| -> Option<&'a str> { (!h.is_empty() && !h.starts_with('#')).then_some(h) };
        (named(fg), named(bg))
    }

    /// A concrete colour as a name, for the interim case above.
    ///
    /// **Total, on purpose.** An earlier version answered `editor.fg` for
    /// anything that was not `Color::Rgb`, on the assumption that a resolved
    /// colour would be a triple. Theme colours are frequently one of the
    /// sixteen names instead — `file_status_modified_fg` is `Yellow` in the
    /// built-in dark theme — so that fallback silently repainted every
    /// plugin-decorated row in the panel's ordinary ink. Every `Color` variant
    /// round-trips now, and [`resolve`] reads all three forms back.
    pub fn literal(c: Color) -> String {
        match c {
            Color::Rgb(r, g, b) => format!("#{r:02x}{g:02x}{b:02x}"),
            Color::Indexed(i) => format!("#i{i}"),
            other => format!(
                "#{}",
                crate::view::theme::token_color_named_from_ratatui(other)
            ),
        }
    }

    fn base(theme: &Theme) -> Style {
        Style::default().fg(theme.editor_fg).bg(theme.editor_bg)
    }
}

#[cfg(test)]
mod shell_theme_tests {
    use super::shell_theme::{literal, pair, resolve};
    use ratatui::style::Color;

    /// **Every colour round-trips.** The literal form exists because a plugin's
    /// colour arrives already resolved, with no key to name it; it is only
    /// honest if it loses nothing.
    ///
    /// It did lose something. An earlier version answered `editor.fg` for
    /// anything that was not `Color::Rgb`, and theme colours are frequently one
    /// of the sixteen names — `file_status_modified_fg` is `Yellow` in the
    /// built-in dark theme — so every plugin-decorated row in the file explorer
    /// silently painted in the panel's ordinary ink instead of its status
    /// colour. Nothing failed; it just looked undecorated.
    #[test]
    fn a_literal_colour_survives_the_round_trip() {
        let theme = crate::view::theme::Theme::from_json(r#"{"name":"test"}"#)
            .expect("a theme of nothing but defaults");
        for c in [
            Color::Rgb(126, 231, 135),
            Color::Rgb(0, 0, 0),
            Color::Yellow,
            Color::LightMagenta,
            Color::Black,
            Color::White,
            Color::Reset,
            Color::Indexed(0),
            Color::Indexed(42),
            Color::Indexed(255),
        ] {
            let style = resolve(&pair(&literal(c), "editor.bg"), &theme);
            assert_eq!(style.fg, Some(c), "{c:?} did not survive {:?}", literal(c));
        }
    }

    /// A literal composes with the rest of the grammar, so a plugin colour can
    /// still be bold or underlined.
    #[test]
    fn a_literal_composes_with_attributes() {
        use ratatui::style::Modifier;
        let theme = crate::view::theme::Theme::from_json(r#"{"name":"test"}"#)
            .expect("a theme of nothing but defaults");
        let style = resolve("#7ee787/editor.bg+bold", &theme);
        assert_eq!(style.fg, Some(Color::Rgb(126, 231, 135)));
        assert!(style.add_modifier.contains(Modifier::BOLD));
    }

    /// A malformed literal falls back to the editor's ground rather than to a
    /// colour nobody asked for.
    #[test]
    fn a_malformed_literal_falls_back() {
        let theme = crate::view::theme::Theme::from_json(r#"{"name":"test"}"#)
            .expect("a theme of nothing but defaults");
        for bad in [
            "#zzzzzz/editor.bg",
            "#12345/editor.bg",
            "#NotAColour/editor.bg",
        ] {
            let style = resolve(bad, &theme);
            assert_eq!(style.fg, Some(theme.editor_fg), "{bad}");
        }
    }
}

impl Editor {
    /// Snapshot the colours the shell's themes resolve to this frame.
    pub(crate) fn shell_palette(&self) -> ShellPalette {
        ShellPalette {
            theme: self.theme.read().unwrap().clone().into(),
        }
    }
}

impl Editor {
    /// Offer an input to the shell's tree before the legacy path sees it.
    ///
    /// The first of the three stages S1 describes: the legacy modal-capture
    /// band still runs ahead of everything, the shell is offered the event
    /// next, and the existing walk remains the floor. Returns whether the tree
    /// claimed it.
    ///
    /// Only migrated surfaces carry handlers — every region is still a `Host`
    /// leaf standing in for a painter that has not moved — so anything the
    /// tree declines reaches the legacy path exactly as before. A surface
    /// starts taking its own input the moment it stops being a `Host`.
    pub(crate) fn shell_dispatch(&mut self, input: fresh_ui::Input) -> bool {
        let Some(mut ui) = self.shell_ui.take() else {
            return false;
        };
        // What the menu was showing when this event arrived. Snapshotted
        // before a single message is applied, because the first of them may be
        // the layer's own dismissal — and a toggle has to know what it is
        // toggling. See `UiFact::MenuBarPress`.
        self.shell_menu_open_before = self.menu_state.active_menu;
        // Where the pointer is, for the hover reactions a resulting
        // `UiFact::Hover` will run — they anchor tooltips to it, and the fact
        // itself carries only *what* is under the pointer.
        if let Some(p) = input.position() {
            self.shell_hover_at = (p.x.max(0) as u16, p.y.max(0) as u16);
        }
        let result = ui.dispatch(input);
        self.shell_ui = Some(ui);
        // Claimed is reported, not inferred. Producing a message and taking
        // the event are different things: a hover moves a highlight without
        // claiming the pointer, and a dismissal closes a menu while leaving a
        // right-click to go on and open the next one.
        let claimed = result.claimed;
        for msg in result.msgs {
            match msg {
                crate::view::shell::msg::UiMsg::Action(action) => {
                    // Straight into the pipeline that has always applied
                    // actions; nothing about it changes.
                    if let Err(e) = self.handle_action(action.clone()) {
                        tracing::warn!("shell action {action:?} failed: {e}");
                    }
                }
                crate::view::shell::msg::UiMsg::Ui(fact) => self.apply_ui_fact(fact),
            }
        }
        claimed
    }

    /// Apply a positional fact — the half of a message that never becomes a
    /// keybinding.
    fn apply_ui_fact(&mut self, fact: crate::view::shell::msg::UiFact) {
        use crate::view::shell::msg::UiFact;
        match fact {
            UiFact::StatusBarClicked(id) => {
                // The id→behaviour table is unchanged and stays where it is
                // (`chrome::status_bar`); what the tree replaced is finding
                // *which* element the pointer was over.
                if let Err(e) = self.dispatch_status_bar_click(id) {
                    tracing::warn!("status bar click failed: {e}");
                }
            }
            UiFact::StatusBarTokenClicked(key) => self.fire_status_bar_token_click(&key),
            UiFact::MenuNav(step) => self.menu_nav(step),
            UiFact::CloseContextMenu => {
                self.active_window_mut().close_context_menus();
            }
            UiFact::HighlightContextMenuItem(idx) => {
                if let Some(core) = self.active_window_mut().context_menu_core_mut() {
                    core.highlighted = idx;
                }
            }
            UiFact::StepContextMenu(step) => {
                use crate::view::shell::msg::MenuStep;
                if let Some(core) = self.active_window_mut().context_menu_core_mut() {
                    match step {
                        MenuStep::Prev => core.prev_item(),
                        MenuStep::Next => core.next_item(),
                    }
                }
            }
            UiFact::ActivateContextMenuItem(idx) => {
                // The same two steps the old click handler took: move the
                // highlight, then activate through the path Enter uses.
                let Some((kind, _)) = self.active_window().open_context_menu() else {
                    return;
                };
                if let Some(core) = self.active_window_mut().context_menu_core_mut() {
                    core.highlighted = idx;
                }
                if let Err(e) = self.activate_highlighted_context_menu(kind) {
                    tracing::warn!("context menu activation failed: {e}");
                }
            }

            UiFact::Hover(target) => {
                // The tree says where the pointer is; the existing reaction
                // says what the menu does about it. Both halves of the old
                // walk, minus the walk.
                let target = match target {
                    // `MenuDropdownItem` names the menu it belongs to, and a
                    // row cannot know that — the tree is built per frame while
                    // the open menu changes under it. Fill it in here, where
                    // the answer lives.
                    Some(crate::app::types::HoverTarget::MenuDropdownItem(_, item)) => self
                        .menu_state
                        .active_menu
                        .map(|m| crate::app::types::HoverTarget::MenuDropdownItem(m, item)),
                    other => other,
                };
                let old = self.shell_hover.clone();
                self.shell_hover = target.clone();
                if old == target {
                    return;
                }
                // **Every registered reaction, not one hand-picked one.**
                // The tree says where the pointer is; what each surface does
                // about it stays with that surface, exactly as it does for the
                // legacy walk (`update_hover_target`). Calling
                // `menu_hover_reaction` directly instead silently dropped the
                // reactions belonging to two surfaces that had *also*
                // migrated: the explorer's git-status tooltip
                // (`FileExplorerStatusIndicator`) and the status bar's
                // indicator styling. Neither is reachable from the legacy walk
                // any more — those components no longer publish boxes — so a
                // reaction this fact does not reach is a reaction that never
                // runs.
                //
                // The pointer cell the reactions want is the one the fact
                // arrived at; a hover fact is always produced by a pointer
                // event, and `shell_hover_at` is where that event's position
                // is kept for exactly this.
                let (col, row) = self.shell_hover_at;
                for c in crate::app::chrome::components() {
                    c.on_hover_change(self, old.as_ref(), target.as_ref(), col, row);
                }
            }
            UiFact::MenuBarPress { index } => {
                // `open_before` is what the menu was showing when this pointer
                // event *arrived*, before the layer's dismissal closed it. A
                // toggle needs that: by the time any message is applied the
                // menu is already shut, so asking now would always answer "not
                // open" and reopen what the press was meant to close.
                if self.shell_menu_open_before == Some(index) {
                    self.close_menu_with_auto_hide();
                } else {
                    self.active_window_mut().on_editor_focus_lost();
                    self.menu_state.open_menu(index);
                }
            }
            // -- file explorer ---------------------------------------------
            UiFact::ExplorerRowPress { index, clicks } => self.explorer_row_pressed(index, clicks),
            UiFact::ExplorerRowContext { index, x, y } => self.explorer_row_context(index, x, y),
            UiFact::ExplorerClose => self.toggle_file_explorer(),
            UiFact::ExplorerResizeBegin { x, y } => {
                let w = self.active_window().file_explorer_width;
                let st = &mut self.active_window_mut().mouse_state;
                st.dragging_file_explorer = true;
                st.drag_start_position = Some((x, y));
                st.drag_start_explorer_width = Some(w);
            }
            UiFact::ExplorerScroll { delta, x, y } => {
                // The surface's wheel, with the surface. Unchanged from the
                // chrome component's `on_wheel`, including the plugin hook —
                // the position it reports is the pointer's, which the tree
                // carries on the event.
                self.dismiss_transient_popups();
                self.active_window().wheel_plugin_hook(x, y, delta);
                self.active_window_mut().scroll_file_explorer_view(delta);
            }

            UiFact::MenuItemClick { depth, index } => {
                let Some(active) = self.menu_state.active_menu else {
                    return;
                };
                let menus: Vec<crate::config::Menu> = self
                    .menus
                    .menus
                    .iter()
                    .chain(self.menu_state.plugin_menus.iter())
                    .cloned()
                    .collect();
                let Some(menu) = menus.get(active) else {
                    return;
                };
                match self.activate_menu_item(depth, index, menu) {
                    Ok(Err(e)) | Err(e) => {
                        tracing::warn!("menu item activation failed: {e}")
                    }
                    Ok(Ok(())) => {}
                }
            }
            UiFact::CloseMenu => {
                if self.menu_state.active_menu.is_some() {
                    self.close_menu_with_auto_hide();
                }
            }
        }
    }
}
