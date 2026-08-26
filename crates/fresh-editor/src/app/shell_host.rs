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
            // Still painted by `Editor::render`; moved out one stage at a time.
            HostRegion::Dock
            | HostRegion::MenuBar
            | HostRegion::Explorer
            | HostRegion::StatusBar
            | HostRegion::SearchOptions
            | HostRegion::PromptLine => {}
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
    use ratatui::style::{Modifier, Style};

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
        let (Some(fg), Some(bg)) = (
            theme.resolve_theme_key(fg_key),
            theme.resolve_theme_key(bg_key),
        ) else {
            return base(theme);
        };
        // The attribute the theme declares for the foreground key, plus the
        // structural one the name asked for.
        let modifier = modifier | theme.resolve_modifier_key(fg_key);
        Style::default().fg(fg).bg(bg).add_modifier(modifier)
    }

    fn base(theme: &Theme) -> Style {
        Style::default().fg(theme.editor_fg).bg(theme.editor_bg)
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

            UiFact::MenuHover(target) => {
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
                self.shell_hover = target.clone();
                self.menu_hover_reaction(target.as_ref());
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
