//! The sidebar's sections as editor state, and the arithmetic that sizes them.
//!
//! The *description* of the column is `view::shell::sidebar`; this is what it
//! is built from. Section layout is session state — it is what the workspace
//! file persists — so by the migration's rule it lives on the editor rather
//! than in the tree, which disposes elements on unmount.
//!
//! **Editor-global, like the dock.** A plugin mounts a section once, not once
//! per window, and the panel state it holds is the same `FloatingWidgetState`
//! the dock and the centred modal hold. The list is captured into whichever
//! window's workspace is being saved and restored from the active window's.
//!
//! The three functions at the bottom are pure and are the whole of the
//! accordion's arithmetic (design §3.6, §3.7, §4.3): [`squeeze`] decides which
//! sections pressure collapses, [`distribute`] turns stored extents into body
//! rows, and [`drag`] moves one divider between two neighbours.

use crate::widgets::PanelKey;

/// What a section holds.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum SidebarSectionKind {
    /// The file tree. Always section 0.
    Explorer,
    /// A plugin panel, by its composite identity. `panel` on the section is
    /// `None` while the plugin has not (re)mounted it — a workspace restored
    /// before its plugin loaded — and the section shows a placeholder body.
    Panel {
        key: PanelKey,
        title: String,
        closable: bool,
    },
}

/// One section of the sidebar column.
#[derive(Debug, Clone)]
pub(crate) struct SidebarSection {
    pub kind: SidebarSectionKind,
    /// The mounted panel, for a `Panel` section the plugin has mounted.
    pub panel: Option<super::FloatingWidgetState>,
    /// Requested body rows. `0` is "no preference": the section shares
    /// whatever is left once the sections that asked for rows have them.
    /// Set by a drag, or by a plugin's `sidebar_rows`; survives a collapse
    /// and re-open, so the divider comes back where the user left it.
    pub rows: u16,
    pub collapsed: bool,
    /// Collapsed by pressure rather than by the user, so only these are
    /// re-opened when the column grows back (§3.7).
    pub squeezed: bool,
    /// Whether the user has dragged this section's extent. A plugin's
    /// `sidebar_rows` request no longer overrides it then — the same rule
    /// `dock_width` follows for the dock.
    pub dragged: bool,
    /// The body rows the last frame resolved this section to. Read for a
    /// panel's row budget, and snapshotted when a divider drag begins.
    pub resolved: u16,
}

impl SidebarSection {
    pub(crate) fn explorer() -> SidebarSection {
        SidebarSection {
            kind: SidebarSectionKind::Explorer,
            panel: None,
            rows: 0,
            collapsed: false,
            squeezed: false,
            dragged: false,
            resolved: 0,
        }
    }

    pub(crate) fn panel_key(&self) -> Option<&PanelKey> {
        match &self.kind {
            SidebarSectionKind::Panel { key, .. } => Some(key),
            SidebarSectionKind::Explorer => None,
        }
    }

    fn extent(&self) -> Extent {
        Extent {
            rows: self.rows,
            collapsed: self.collapsed,
            squeezed: self.squeezed,
        }
    }
}

/// A divider drag in progress: which sections it moves and where they were.
///
/// The applier recomputes both rows from the *absolute* pointer row on each
/// move rather than accumulating deltas — what the explorer-width drag does
/// from `drag_start_position`, and why a long drag cannot drift away from the
/// cursor.
#[derive(Debug, Clone, Copy)]
pub(crate) struct SidebarDrag {
    /// The header row that was pressed.
    pub index: usize,
    pub press_y: u16,
    /// The open sections either side of the divider, and their body rows at
    /// the press. `None` when the press cannot resize anything — the top
    /// header, an all-collapsed neighbourhood, or exclusive mode — in which
    /// case the press is a toggle waiting for its release.
    pub neighbours: Option<(usize, usize, u16, u16)>,
    /// Whether the pointer has left the row it was pressed on. A release
    /// without a move is a click, and a click on a header toggles.
    pub moved: bool,
}

/// The sizing inputs of one section, for the pure functions below.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Extent {
    pub rows: u16,
    pub collapsed: bool,
    pub squeezed: bool,
}

/// How many body rows a column `height` tall has to share out among
/// `sections` sections: one header row each, and the bottom border.
pub(crate) fn body_rows(height: u16, sections: usize) -> u16 {
    height.saturating_sub(sections as u16).saturating_sub(1)
}

/// Collapse from the bottom up until the open sections fit, and restore what
/// pressure collapsed once they fit again.
///
/// A section's minimum is one body row, so the column fits when the number
/// of open sections is at most `body_rows`. Only sections *this* collapsed
/// are restored — a section the user collapsed stays collapsed however tall
/// the column grows.
pub(crate) fn squeeze(body_rows: u16, extents: &mut [Extent]) {
    let open = |e: &[Extent]| e.iter().filter(|x| !x.collapsed).count();
    // Restore top-down: the section nearest the tree comes back first.
    for i in 0..extents.len() {
        if extents[i].squeezed && (open(extents) as u16) < body_rows {
            extents[i].collapsed = false;
            extents[i].squeezed = false;
        }
    }
    // Collapse bottom-up: the tree is the last to go.
    while open(extents) as u16 > body_rows {
        let Some(last) = extents.iter().rposition(|x| !x.collapsed) else {
            break;
        };
        extents[last].collapsed = true;
        extents[last].squeezed = true;
    }
}

/// Body rows per section, for a column with `body_rows` to give.
///
/// A collapsed section gets none. Among the open ones, a section with a
/// requested extent is fixed at it and the rest share the remainder; when
/// every open section asked for rows the last one is the remainder instead,
/// so the column is exactly filled and nothing rounds. Fixed extents shrink
/// from the bottom up when they would leave a sharing section without its
/// one row — they are resolved, not rewritten: the stored request survives.
pub(crate) fn distribute(body_rows: u16, extents: &[Extent]) -> Vec<u16> {
    let mut out = vec![0u16; extents.len()];
    let open: Vec<usize> = (0..extents.len())
        .filter(|&i| !extents[i].collapsed)
        .collect();
    let Some(&last_open) = open.last() else {
        return out;
    };
    let mut flex: Vec<usize> = open
        .iter()
        .copied()
        .filter(|&i| extents[i].rows == 0)
        .collect();
    if flex.is_empty() {
        flex.push(last_open);
    }
    let mut fixed: Vec<(usize, u16)> = open
        .iter()
        .copied()
        .filter(|i| !flex.contains(i))
        .map(|i| (i, extents[i].rows.max(1)))
        .collect();
    // Everyone sharing keeps one row; the fixed ones give way bottom-up.
    let mut budget = body_rows.saturating_sub(flex.len() as u16);
    let mut fixed_sum: u16 = fixed.iter().map(|(_, r)| *r).sum();
    for (_, r) in fixed.iter_mut().rev() {
        if fixed_sum <= budget {
            break;
        }
        let give = (fixed_sum - budget).min(r.saturating_sub(1));
        *r -= give;
        fixed_sum -= give;
    }
    budget = body_rows.saturating_sub(fixed_sum);
    for (i, r) in &fixed {
        out[*i] = *r;
    }
    let share = budget / flex.len() as u16;
    let extra = budget % flex.len() as u16;
    for (n, &i) in flex.iter().enumerate() {
        out[i] = share + if n + 1 == flex.len() { extra } else { 0 };
    }
    out
}

/// Move the divider between two open neighbours by `delta` rows.
///
/// Both keep at least one body row, so a section cannot be dragged out of
/// existence — collapsing is the reversible way to reclaim its space.
pub(crate) fn drag(above: u16, below: u16, delta: i32) -> (u16, u16) {
    let total = above + below;
    if total < 2 {
        return (above, below);
    }
    let a = (above as i32 + delta).clamp(1, total as i32 - 1) as u16;
    (a, total - a)
}

/// The accordion's two modes. See `config::SidebarConfig`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Accordion {
    Free,
    Exclusive,
}

impl super::Editor {
    /// The accordion mode the config asks for.
    pub(crate) fn sidebar_accordion(&self) -> Accordion {
        match self.config.sidebar.accordion {
            crate::config::SidebarAccordion::Free => Accordion::Free,
            crate::config::SidebarAccordion::Exclusive => Accordion::Exclusive,
        }
    }

    /// Resolve the sections against a column `height` tall: squeeze what
    /// pressure must, then size the rest. Records each section's body rows
    /// and returns them.
    ///
    /// **App logic keyed on the frame height**, resolved in the `Frame`
    /// builder for the same reason `Frame::resolve_dock` is: `build()` cannot
    /// read geometry, and the migration's frame work recorded that `fresh-ui`
    /// and ratatui starve different rows when a band is over-subscribed, so
    /// a caller who cares picks its own order. The sidebar cares: the top
    /// section is the tree.
    pub(crate) fn resolve_sidebar_sections(&mut self, height: u16) -> Vec<u16> {
        let body = body_rows(height, self.sidebar_sections.len());
        let mut extents: Vec<Extent> = self.sidebar_sections.iter().map(|s| s.extent()).collect();
        squeeze(body, &mut extents);
        let rows = distribute(body, &extents);
        for ((s, e), r) in self
            .sidebar_sections
            .iter_mut()
            .zip(extents)
            .zip(rows.iter())
        {
            s.collapsed = e.collapsed;
            s.squeezed = e.squeezed;
            s.resolved = *r;
        }
        rows
    }

    /// Open a collapsed section or collapse an open one — the header's
    /// press, and Enter or Space while the header has the keyboard.
    pub(crate) fn toggle_sidebar_section(&mut self, index: usize) {
        let Some(sec) = self.sidebar_sections.get(index) else {
            return;
        };
        let opening = sec.collapsed;
        if opening && self.sidebar_accordion() == Accordion::Exclusive {
            for (i, s) in self.sidebar_sections.iter_mut().enumerate() {
                if i != index {
                    s.collapsed = true;
                    s.squeezed = false;
                }
            }
        }
        let sec = &mut self.sidebar_sections[index];
        sec.collapsed = !opening;
        // The user decided; pressure no longer owns this one.
        sec.squeezed = false;
    }

    /// The press that may become a divider drag: snapshot the neighbours.
    pub(crate) fn begin_sidebar_section_drag(&mut self, index: usize, y: u16) {
        let inert = self.sidebar_accordion() == Accordion::Exclusive;
        let sections = &self.sidebar_sections;
        let above = (0..index).rev().find(|&i| !sections[i].collapsed);
        let below = (index..sections.len()).find(|&i| !sections[i].collapsed);
        let neighbours = match (above, below) {
            (Some(a), Some(b)) if !inert => {
                Some((a, b, sections[a].resolved, sections[b].resolved))
            }
            _ => None,
        };
        self.sidebar_drag = Some(SidebarDrag {
            index,
            press_y: y,
            neighbours,
            moved: false,
        });
    }

    /// A move while a header holds the pointer.
    pub(crate) fn drag_sidebar_section(&mut self, y: u16) {
        let Some(d) = self.sidebar_drag.as_mut() else {
            return;
        };
        if y != d.press_y {
            d.moved = true;
        }
        let Some((a, b, above, below)) = d.neighbours else {
            return;
        };
        let delta = y as i32 - d.press_y as i32;
        let (ra, rb) = drag(above, below, delta);
        // Both become explicit: the one above takes its new height, and the
        // one below keeps the rows the drag left it — unless it is the last
        // open section, which is the remainder and absorbs the difference.
        let last_open = self.sidebar_sections.iter().rposition(|s| !s.collapsed);
        let sa = &mut self.sidebar_sections[a];
        sa.rows = ra;
        sa.dragged = true;
        if last_open != Some(b) {
            let sb = &mut self.sidebar_sections[b];
            sb.rows = rb;
            sb.dragged = true;
        }
    }

    /// The release: a press that never moved was a click on the header.
    pub(crate) fn end_sidebar_section_drag(&mut self) {
        let Some(d) = self.sidebar_drag.take() else {
            return;
        };
        if !d.moved {
            self.toggle_sidebar_section(d.index);
        }
    }

    /// Drop section `index`. Section 0 is the explorer and is not a section
    /// the user can close this way — its `×` hides the whole sidebar.
    ///
    /// A mounted panel is unmounted and told, with the `cancel` a closed
    /// modal fires, so the plugin can drop its own state.
    pub(crate) fn close_sidebar_section(&mut self, index: usize) {
        if index == 0 || index >= self.sidebar_sections.len() {
            return;
        }
        if let Some(panel) = self.sidebar_sections[index].panel.take() {
            let widget_key = self
                .widget_registry
                .get(&panel.panel_key)
                .map(|p| p.focus_key.clone())
                .unwrap_or_default();
            self.fire_widget_event(
                &panel.panel_key,
                widget_key,
                "cancel".to_string(),
                serde_json::json!({}),
            );
            let _ = self.widget_registry.unmount(&panel.panel_key);
        }
        self.sidebar_sections.remove(index);
        self.renumber_sidebar_panels();
    }

    /// The sidebar column's width in columns, measured against the chrome
    /// beside the dock — the same number the frame lays it out at.
    pub(crate) fn sidebar_cols(&self) -> u16 {
        let dock_cols = self.dock_cols();
        self.active_window()
            .file_explorer_width
            .to_cols(self.terminal_width.saturating_sub(dock_cols))
    }

    /// The plugin section that owns the keyboard, if one does.
    pub(crate) fn focused_sidebar_panel(&self) -> Option<usize> {
        self.sidebar_sections
            .iter()
            .position(|s| s.panel.as_ref().is_some_and(|p| p.focused))
    }

    /// Take the keyboard back from any focused plugin section.
    pub(crate) fn blur_sidebar_panels(&mut self) {
        while let Some(i) = self.focused_sidebar_panel() {
            self.blur_floating_panel(super::PanelSlot::Sidebar(i));
        }
    }

    /// Give plugin section `index` the keyboard: the explorer's context and
    /// every other panel section give it up first, so exactly one chrome
    /// region has it.
    pub(crate) fn focus_sidebar_section(&mut self, index: usize) {
        use crate::input::keybindings::KeyContext;
        if self
            .sidebar_sections
            .get(index)
            .and_then(|s| s.panel.as_ref())
            .is_none()
        {
            return;
        }
        // Putting the keyboard in a section you cannot see is not a
        // state; this is the ask that `place_panel_in_sidebar` no longer
        // assumes (see `reveal_sidebar`).
        self.reveal_sidebar();
        if self.dock.as_ref().is_some_and(|d| d.focused) {
            self.blur_floating_panel(super::PanelSlot::Dock);
        }
        for i in 0..self.sidebar_sections.len() {
            if i != index
                && self.sidebar_sections[i]
                    .panel
                    .as_ref()
                    .is_some_and(|p| p.focused)
            {
                self.blur_floating_panel(super::PanelSlot::Sidebar(i));
            }
        }
        let win = self.active_window_mut();
        if win.key_context == KeyContext::FileExplorer {
            win.key_context = KeyContext::Normal;
        }
        if !self.sidebar_sections[index]
            .panel
            .as_ref()
            .is_some_and(|p| p.focused)
        {
            self.refocus_floating_panel(super::PanelSlot::Sidebar(index));
        }
    }

    /// `Action::FocusNextSidebarSection`: explorer → plugin sections in
    /// order → editor, and round again. A hidden sidebar is shown and its
    /// explorer focused, which is what the first step of the cycle is.
    pub(crate) fn focus_next_sidebar_section(&mut self) {
        use crate::input::keybindings::KeyContext;
        if !self.file_explorer_visible() {
            self.focus_file_explorer();
            return;
        }
        let current = match self.focused_sidebar_panel() {
            Some(i) => Some(i),
            None if self.active_window().key_context == KeyContext::FileExplorer => Some(0),
            None => None,
        };
        let next = (current.map(|i| i + 1).unwrap_or(0)..self.sidebar_sections.len())
            .find(|&i| i == 0 || self.sidebar_sections[i].panel.is_some());
        match (current, next) {
            (_, Some(0)) => self.focus_file_explorer(),
            (_, Some(i)) => self.focus_sidebar_section(i),
            // Past the last section: the editor.
            (Some(_), None) => {
                self.blur_sidebar_panels();
                self.active_window_mut().focus_editor();
            }
            // From the editor with nothing after the explorer to go to.
            (None, None) => self.focus_file_explorer(),
        }
    }

    /// Every panel section's registry entry names the sentinel buffer of the
    /// slot it is *now* in. Sections move when one is removed, so this runs
    /// after every removal, insertion and restore.
    pub(crate) fn renumber_sidebar_panels(&mut self) {
        for i in 0..self.sidebar_sections.len() {
            let Some(key) = self.sidebar_sections[i]
                .panel
                .as_ref()
                .map(|p| p.panel_key.clone())
            else {
                continue;
            };
            if let Some(st) = self.widget_registry.get_mut(&key) {
                st.buffer_id = super::PanelSlot::Sidebar(i).buffer_id();
            }
        }
    }

    /// Put `panel` into the sidebar as a section: the one already carrying
    /// its identity — a restored placeholder, or a remount — or a new one
    /// appended after the last. Shows the sidebar if it is hidden. Returns
    /// the section's index.
    pub(crate) fn place_panel_in_sidebar(
        &mut self,
        mut panel: super::FloatingWidgetState,
        title: String,
        rows: u16,
        closable: bool,
    ) -> usize {
        panel.placement = super::PanelPlacement::SidebarSection { rows };
        panel.fullscreen = false;
        let key = panel.panel_key.clone();
        let index = match self
            .sidebar_sections
            .iter()
            .position(|s| s.panel_key() == Some(&key))
        {
            Some(i) => i,
            None => {
                self.sidebar_sections.push(SidebarSection {
                    kind: SidebarSectionKind::Panel {
                        key: key.clone(),
                        title: title.clone(),
                        closable,
                    },
                    panel: None,
                    rows: 0,
                    collapsed: false,
                    squeezed: false,
                    dragged: false,
                    resolved: 0,
                });
                self.sidebar_sections.len() - 1
            }
        };
        let sec = &mut self.sidebar_sections[index];
        sec.kind = SidebarSectionKind::Panel {
            key,
            title,
            closable,
        };
        // The plugin's request, unless the user has already said otherwise.
        if !sec.dragged {
            sec.rows = rows;
        }
        sec.panel = Some(panel);
        self.renumber_sidebar_panels();
        index
    }

    /// Show the sidebar column, initialising the explorer that heads it.
    ///
    /// **Placing a section does not call this, and that is the point.**
    /// It used to: `place_panel_in_sidebar` opened the column on every
    /// mount, on the reasoning that a section is nothing if the column
    /// is hidden. True, and not worth what it cost.
    ///
    /// The cost is a user-facing one and it does not depend on any
    /// pane's layout: `markdown_toc` mounts a section for every Markdown
    /// buffer, so opening a `.md` file yanked open a thirty-five-column
    /// sidebar — and created a file explorer to head it — on a reader
    /// who had deliberately closed it. No plugin asked for that; every
    /// one of them said `startBlurred: true`, which is a plugin saying
    /// "do not take anything from the reader for this".
    ///
    /// (It also corrupted the welcome screen's own layout, because the
    /// page had composed against a width the mount then took away. That
    /// was the symptom that found this, not the reason for the change —
    /// a page can and does recompose on `viewport_changed`. Stealing the
    /// columns is the part that was wrong.)
    ///
    /// So revealing is the *ask*, not the placement: a mount that does
    /// not start blurred, a focus on a section, and re-anchoring a dock
    /// or centred panel into the column all say so out loud. A quiet
    /// mount waits in a hidden column and is there the moment the reader
    /// opens it — `an_outline_mounted_into_a_closed_sidebar_appears_when_it_is_opened`
    /// is that sentence as a test.
    pub(crate) fn reveal_sidebar(&mut self) {
        if self.active_window().file_explorer_visible {
            return;
        }
        self.active_window_mut().file_explorer_visible = true;
        if self.file_explorer().is_none() {
            self.init_file_explorer();
        }
        self.relayout();
    }

    /// Take a plugin section's panel out of the column, dropping the
    /// section. The caller re-anchors it elsewhere.
    pub(crate) fn take_panel_from_sidebar(
        &mut self,
        index: usize,
    ) -> Option<super::FloatingWidgetState> {
        if index == 0 || index >= self.sidebar_sections.len() {
            return None;
        }
        let panel = self.sidebar_sections[index].panel.take()?;
        self.sidebar_sections.remove(index);
        self.renumber_sidebar_panels();
        Some(panel)
    }

    /// What the workspace file records of the sections.
    pub(crate) fn sidebar_section_states(&self) -> Vec<crate::workspace::SectionState> {
        use crate::workspace::{SectionState, SectionStateKind};
        self.sidebar_sections
            .iter()
            .map(|s| SectionState {
                kind: match &s.kind {
                    SidebarSectionKind::Explorer => SectionStateKind::Explorer,
                    SidebarSectionKind::Panel { key, .. } => SectionStateKind::Panel {
                        plugin: key.plugin.clone(),
                        id: key.id,
                    },
                },
                title: match &s.kind {
                    SidebarSectionKind::Explorer => String::new(),
                    SidebarSectionKind::Panel { title, .. } => title.clone(),
                },
                rows: s.rows,
                collapsed: s.collapsed && !s.squeezed,
            })
            .collect()
    }

    /// Rebuild the sections from a workspace file.
    ///
    /// An empty list — every workspace written before sections existed —
    /// restores as exactly one explorer section filling the column. A panel
    /// section is restored by its `(plugin, id)` identity: a plugin that has
    /// already mounted it keeps its panel, one that has not gets a
    /// placeholder that shows "panel unavailable" until it does. Mounted
    /// sections the file does not name are kept after the ones it does, so
    /// a restore never unmounts anything.
    pub(crate) fn restore_sidebar_sections(&mut self, states: &[crate::workspace::SectionState]) {
        use crate::workspace::SectionStateKind;
        let mut old = std::mem::take(&mut self.sidebar_sections);
        let mut take = |pred: &dyn Fn(&SidebarSection) -> bool| -> Option<SidebarSection> {
            let i = old.iter().position(pred)?;
            Some(old.remove(i))
        };
        let mut next: Vec<SidebarSection> = Vec::new();
        let mut explorer = take(&|s| s.kind == SidebarSectionKind::Explorer)
            .unwrap_or_else(SidebarSection::explorer);
        let mut saw_explorer = false;
        for st in states {
            match &st.kind {
                SectionStateKind::Explorer if !saw_explorer => {
                    saw_explorer = true;
                    explorer.rows = st.rows;
                    explorer.collapsed = st.collapsed;
                    explorer.squeezed = false;
                    explorer.dragged = st.rows != 0;
                }
                SectionStateKind::Explorer => {}
                SectionStateKind::Panel { plugin, id } => {
                    let key = PanelKey::new(plugin.clone(), *id);
                    let mut sec =
                        take(&|s| s.panel_key() == Some(&key)).unwrap_or_else(|| SidebarSection {
                            kind: SidebarSectionKind::Panel {
                                key: key.clone(),
                                title: st.title.clone(),
                                closable: true,
                            },
                            panel: None,
                            rows: 0,
                            collapsed: false,
                            squeezed: false,
                            dragged: false,
                            resolved: 0,
                        });
                    sec.rows = st.rows;
                    sec.collapsed = st.collapsed;
                    sec.squeezed = false;
                    sec.dragged = st.rows != 0;
                    next.push(sec);
                }
            }
        }
        // The explorer is section 0 whatever the file says, and everything
        // the file did not name follows what it did.
        next.insert(0, explorer);
        next.extend(old.into_iter().filter(|s| s.panel.is_some()));
        self.sidebar_sections = next;
        self.renumber_sidebar_panels();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ext(rows: u16, collapsed: bool) -> Extent {
        Extent {
            rows,
            collapsed,
            squeezed: false,
        }
    }

    #[test]
    fn a_column_spends_one_row_per_header_and_one_on_the_bottom_border() {
        assert_eq!(body_rows(20, 1), 18, "the bordered box the explorer was");
        assert_eq!(body_rows(20, 3), 16);
        assert_eq!(body_rows(3, 3), 0, "nothing left, nothing negative");
        assert_eq!(body_rows(0, 1), 0);
    }

    /// One section, no request: it is the whole column. This is the
    /// default configuration and the byte-identical case.
    #[test]
    fn one_flexible_section_takes_the_column() {
        assert_eq!(distribute(18, &[ext(0, false)]), vec![18]);
        // And a stored request on a lone section is ignored — the last open
        // section is always the remainder.
        assert_eq!(distribute(18, &[ext(5, false)]), vec![18]);
    }

    /// The design's §3.4: a requested second section takes its rows, the
    /// tree takes the rest.
    #[test]
    fn a_requested_section_is_fixed_and_the_rest_share() {
        assert_eq!(distribute(20, &[ext(0, false), ext(8, false)]), vec![12, 8]);
        // Two fixed and one sharing: the sharer takes what is left.
        assert_eq!(
            distribute(20, &[ext(5, false), ext(0, false), ext(8, false)]),
            vec![5, 7, 8]
        );
    }

    /// Every section asked for rows: the last open one absorbs the
    /// remainder, so a drag on the last divider works as "last = remainder".
    #[test]
    fn when_every_section_is_fixed_the_last_open_one_is_the_remainder() {
        assert_eq!(distribute(20, &[ext(9, false), ext(8, false)]), vec![9, 11]);
        assert_eq!(
            distribute(20, &[ext(9, false), ext(30, false)]),
            vec![9, 11]
        );
    }

    #[test]
    fn sharers_split_equally_and_the_last_takes_the_odd_row() {
        assert_eq!(
            distribute(20, &[ext(0, false), ext(8, false), ext(0, false)]),
            vec![6, 8, 6]
        );
        assert_eq!(
            distribute(21, &[ext(0, false), ext(8, false), ext(0, false)]),
            vec![6, 8, 7]
        );
    }

    /// A collapsed section gets no rows and its rows go to the open ones
    /// below first — the last open section is the remainder.
    #[test]
    fn a_collapsed_section_gives_its_rows_to_the_open_ones() {
        assert_eq!(distribute(20, &[ext(9, true), ext(8, false)]), vec![0, 20]);
        assert_eq!(distribute(20, &[ext(9, false), ext(8, true)]), vec![20, 0]);
        assert_eq!(
            distribute(20, &[ext(5, false), ext(0, true), ext(8, false)]),
            vec![5, 0, 15]
        );
        assert_eq!(distribute(20, &[ext(9, true), ext(8, true)]), vec![0, 0]);
    }

    /// **Explicit heights survive a round trip.** The stored request is an
    /// input, never rewritten: collapse and re-open put the divider back.
    #[test]
    fn an_explicit_height_survives_collapse_and_reopen() {
        let open = [ext(9, false), ext(8, false)];
        let before = distribute(20, &open);
        let collapsed = [ext(9, false), ext(8, true)];
        assert_eq!(distribute(20, &collapsed), vec![20, 0]);
        assert_eq!(distribute(20, &open), before);
    }

    /// Fixed extents that would starve a sharer shrink from the bottom up,
    /// and never below one row.
    #[test]
    fn fixed_extents_shrink_from_the_bottom_to_leave_a_sharer_its_row() {
        assert_eq!(
            distribute(10, &[ext(0, false), ext(6, false), ext(6, false)]),
            vec![1, 6, 3]
        );
        assert_eq!(
            distribute(3, &[ext(0, false), ext(6, false), ext(6, false)]),
            vec![1, 1, 1]
        );
    }

    /// §3.7: under pressure the column collapses from the bottom up until
    /// what remains fits, and the tree is the last to go.
    #[test]
    fn squeeze_collapses_from_the_bottom_up() {
        let mut e = [ext(0, false), ext(8, false), ext(0, false)];
        squeeze(2, &mut e);
        assert_eq!(
            e.iter().map(|x| x.collapsed).collect::<Vec<_>>(),
            vec![false, false, true]
        );
        assert!(e[2].squeezed && !e[1].squeezed);
        squeeze(1, &mut e);
        assert_eq!(
            e.iter().map(|x| x.collapsed).collect::<Vec<_>>(),
            vec![false, true, true]
        );
        squeeze(0, &mut e);
        assert!(
            e.iter().all(|x| x.collapsed),
            "no rows: every header, no body"
        );
    }

    /// Only what pressure collapsed comes back, and it comes back top-down
    /// as the column grows.
    #[test]
    fn squeeze_restores_only_what_it_collapsed() {
        let mut e = [ext(0, false), ext(8, true), ext(0, false)];
        // The user collapsed [1]; pressure collapses [2].
        squeeze(1, &mut e);
        assert!(e[2].squeezed && e[2].collapsed);
        assert!(e[1].collapsed && !e[1].squeezed);
        squeeze(3, &mut e);
        assert!(!e[2].collapsed, "pressure's collapse is undone");
        assert!(e[1].collapsed, "the user's is not");
        // Growing back restores nearest the tree first.
        let mut e = [ext(0, true), ext(0, true), ext(0, true)];
        for x in e.iter_mut() {
            x.squeezed = true;
        }
        squeeze(2, &mut e);
        assert_eq!(
            e.iter().map(|x| x.collapsed).collect::<Vec<_>>(),
            vec![false, false, true]
        );
    }

    /// The divider tracks the pointer and both neighbours clamp at one row.
    #[test]
    fn a_drag_moves_rows_between_neighbours_and_clamps_at_one() {
        assert_eq!(drag(10, 8, 3), (13, 5));
        assert_eq!(drag(10, 8, -3), (7, 11));
        assert_eq!(drag(10, 8, 40), (17, 1));
        assert_eq!(drag(10, 8, -40), (1, 17));
        assert_eq!(drag(1, 0, 5), (1, 0), "nothing to divide");
    }
}
