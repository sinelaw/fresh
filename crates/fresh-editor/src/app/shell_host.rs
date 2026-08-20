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
    status: Style,
    base: Style,
    menu: Style,
    menu_item: Style,
    menu_highlighted: Style,
    menu_border: Style,
    menu_dropdown: Style,
    menu_hover: Style,
    menu_disabled: Style,
    menu_info: Style,
    menu_separator: Style,
    bar: Style,
    bar_item: Style,
    bar_item_mnemonic: Style,
    bar_active: Style,
    bar_active_mnemonic: Style,
    bar_hover: Style,
    bar_hover_mnemonic: Style,
}

impl crate::view::shell::fold::Palette for ShellPalette {
    fn style(&self, theme: &fresh_ui::ThemeKey) -> Style {
        match theme.as_str() {
            // Grown as regions migrate; an unknown name falls back rather than
            // failing, so a new surface renders plainly before it is themed.
            "status" => self.status,
            // A context menu's box, its rows, and the highlighted row. The
            // border takes its own colour, as the old painter's
            // `border_style` did.
            "menu" => self.menu,
            "menu.item" => self.menu_item,
            "menu.item.highlighted" => self.menu_highlighted,
            "menu.border" => self.menu_border,
            // A menu-bar dropdown's box and the five ways one of its rows can
            // look. The names are `MenuRowStyle`'s, one per style, so the
            // ratatui painter's colours and the shell's cannot drift.
            "menu.dropdown" => self.menu_dropdown,
            "menu.item.hover" => self.menu_hover,
            "menu.item.disabled" => self.menu_disabled,
            "menu.item.info" => self.menu_info,
            "menu.separator" => self.menu_separator,
            // The menu bar row: its ground, its labels, and the one character
            // of a label that wears the mnemonic underline. An underline is
            // part of how a run looks, so it is part of the run's name — the
            // library carries one `ThemeKey` per item and never interprets it.
            "menu.bar" => self.bar,
            "menu.bar.item" => self.bar_item,
            "menu.bar.item.mnemonic" => self.bar_item_mnemonic,
            "menu.bar.item.active" => self.bar_active,
            "menu.bar.item.active.mnemonic" => self.bar_active_mnemonic,
            "menu.bar.item.hover" => self.bar_hover,
            "menu.bar.item.hover.mnemonic" => self.bar_hover_mnemonic,
            _ => self.base,
        }
    }
}

use crate::view::ui::BarLabelStyle;

/// One menu-bar label style, with the mnemonic underline applied on top.
fn bar_style(theme: &crate::view::theme::Theme, style: BarLabelStyle, mnemonic: bool) -> Style {
    let base = style.style(theme);
    if mnemonic {
        base.add_modifier(ratatui::style::Modifier::UNDERLINED)
    } else {
        base
    }
}

impl Editor {
    /// Snapshot the colours the shell's themes resolve to this frame.
    pub(crate) fn shell_palette(&self) -> ShellPalette {
        let theme = self.theme.read().unwrap();
        ShellPalette {
            status: Style::default()
                .fg(theme.status_bar_fg)
                .bg(theme.status_bar_bg),
            base: Style::default().fg(theme.editor_fg).bg(theme.editor_bg),
            menu: Style::default()
                .fg(theme.menu_dropdown_fg)
                .bg(theme.menu_dropdown_bg),
            menu_item: Style::default()
                .fg(theme.menu_dropdown_fg)
                .bg(theme.menu_dropdown_bg),
            menu_highlighted: Style::default()
                .fg(theme.menu_highlight_fg)
                .bg(theme.menu_highlight_bg),
            menu_border: Style::default()
                .fg(theme.menu_border_fg)
                .bg(theme.menu_dropdown_bg),
            // The box: border ink on the dropdown ground. Its fill draws
            // spaces, so only the background reaches the eye there.
            menu_dropdown: Style::default()
                .fg(theme.menu_border_fg)
                .bg(theme.menu_dropdown_bg),
            menu_hover: crate::view::ui::MenuRowStyle::Hovered.style(&theme),
            menu_disabled: crate::view::ui::MenuRowStyle::Disabled.style(&theme),
            menu_info: crate::view::ui::MenuRowStyle::Info.style(&theme),
            menu_separator: crate::view::ui::MenuRowStyle::Separator.style(&theme),
            bar: Style::default().bg(theme.menu_bg),
            bar_item: bar_style(&theme, BarLabelStyle::Normal, false),
            bar_item_mnemonic: bar_style(&theme, BarLabelStyle::Normal, true),
            bar_active: bar_style(&theme, BarLabelStyle::Active, false),
            bar_active_mnemonic: bar_style(&theme, BarLabelStyle::Active, true),
            bar_hover: bar_style(&theme, BarLabelStyle::Hovered, false),
            bar_hover_mnemonic: bar_style(&theme, BarLabelStyle::Hovered, true),
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
