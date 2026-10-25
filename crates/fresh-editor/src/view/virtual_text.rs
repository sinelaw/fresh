//! Virtual text rendering infrastructure
//!
//! Provides a system for rendering virtual text that doesn't exist in the buffer.
//! Used for inlay hints (type annotations, parameter names), git blame headers, etc.
//!
//! Two types of virtual text are supported:
//! - **Inline**: Text inserted before/after a character (e.g., `: i32` type hints)
//! - **Line**: Full lines inserted above/below a position (e.g., git blame headers)
//!
//! Virtual text is rendered during the render phase by reading from VirtualTextManager.
//! The buffer content remains unchanged - we just inject extra styled text during rendering.
//!
//! ## Architecture
//!
//! This follows an Emacs-like model where:
//! 1. Plugins add virtual text in response to buffer changes (async, fire-and-forget)
//! 2. Virtual text is stored persistently with marker-based position tracking
//! 3. Render loop reads virtual text synchronously from memory (no async waiting)
//!
//! This ensures frame coherence: render always sees a consistent snapshot of virtual text.

use ratatui::style::{Color, Style};
use std::collections::HashMap;

use crate::model::marker::{MarkerId, MarkerList};

/// Position relative to the character at the marker position
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VirtualTextPosition {
    // ─── Inline positions (within a line) ───
    /// Render before the character (e.g., parameter hints: `/*count=*/5`)
    BeforeChar,
    /// Render after the character (e.g., type hints: `x: i32`)
    AfterChar,

    // ─── Line positions (full lines) ───
    /// Render as a full line ABOVE the line containing this position
    /// Used for git blame headers, section separators, etc.
    /// These lines do NOT get line numbers in the gutter.
    LineAbove,
    /// Render as a full line BELOW the line containing this position
    /// Used for inline documentation, fold previews, etc.
    /// These lines do NOT get line numbers in the gutter.
    LineBelow,
}

impl VirtualTextPosition {
    /// Returns true if this is a line-level position (LineAbove/LineBelow)
    pub fn is_line(&self) -> bool {
        matches!(self, Self::LineAbove | Self::LineBelow)
    }

    /// Returns true if this is an inline position (BeforeChar/AfterChar)
    pub fn is_inline(&self) -> bool {
        matches!(self, Self::BeforeChar | Self::AfterChar)
    }
}

/// Which way a virtual-text marker moves when text is inserted at exactly
/// its position.
///
/// Only insertions *at* the marker are affected; everything else shifts the
/// same way either way.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MarkerGravity {
    /// Text inserted at the marker pushes it along, so the entry stays with
    /// the character it was anchored to. The default, and what an
    /// `AfterChar` entry needs — the #1572 end-of-line hint anchoring
    /// relies on travelling with its glyph.
    Right,
    /// Text inserted at the marker leaves it where it is, so the entry stays
    /// in front of what was typed. What a `BeforeChar` inlay hint needs:
    /// pressing Enter on the anchor of a `let d: Duration` hint used to drag
    /// the hint onto the next line until the debounced refresh corrected it
    /// (issue #722).
    Left,
}

/// Namespace for grouping virtual texts (for efficient bulk removal).
/// Similar to OverlayNamespace - plugins create a namespace once and use it for all their virtual texts.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VirtualTextNamespace(pub String);

impl VirtualTextNamespace {
    /// Create a namespace from a string (for plugin registration)
    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    /// Get the internal string representation
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// A piece of virtual text to render at a specific position
#[derive(Debug, Clone)]
pub struct VirtualText {
    /// Marker tracking the position in its class's marker list (auto-adjusts
    /// on edits)
    pub marker_id: MarkerId,
    /// The gravity the marker was created with.
    ///
    /// Kept alongside the id because the wrap index mirrors marker movement
    /// on its own snapshot of these anchors (`IndexDecorations::shift_for_edit`)
    /// and has to model each anchor the way the tree actually moves it — a
    /// snapshot that drifts from the live marker makes the index describe a
    /// line the renderer never draws.
    pub gravity: MarkerGravity,
    /// Text to display (for LineAbove/LineBelow, this is the full line content)
    pub text: String,
    /// Fallback styling, used when the theme-key fields below are unset OR
    /// the keys don't resolve in the active theme.  The renderer composes
    /// the final style by overlaying any resolved theme colours on top of
    /// this fallback (see [`VirtualText::resolved_style`]).
    pub style: Style,
    /// Optional theme key for the foreground colour (e.g.
    /// `"editor.line_number_fg"`).  Resolved on every render so the line
    /// follows live theme changes.
    pub fg_theme_key: Option<String>,
    /// Optional theme key for the background colour.
    pub bg_theme_key: Option<String>,
    /// Left-pad this hint so it ends at this column of its rendered row,
    /// measured while the row is spliced. `None` = the usual inlay padding.
    pub pad_to_column: Option<u32>,
    /// Where to render relative to the marker position
    pub position: VirtualTextPosition,
    /// Priority for ordering multiple items at same position (higher = later)
    pub priority: i32,
    /// Optional string identifier for this virtual text (for plugin use)
    pub string_id: Option<String>,
    /// Optional namespace for bulk removal (like Overlay's namespace)
    pub namespace: Option<VirtualTextNamespace>,
    /// Optional gutter glyph rendered in the line-number column on the
    /// FIRST visual row of this virtual line. Subsequent wrapped rows
    /// keep a blank gutter. `None` (the default) renders blank, which
    /// matches the legacy behaviour. Used by `live_diff` to place "-"
    /// directly on the deletion line itself instead of the source
    /// line that happens to follow it.
    pub gutter_glyph: Option<String>,
    /// Foreground color for `gutter_glyph`. Falls back to
    /// `theme.line_number_fg` when `None`.
    pub gutter_color: Option<Color>,
    /// Per-range modifier overlays applied on top of the base fg/bg.
    /// Offsets are byte offsets within `text`. Used e.g. by live-diff
    /// to bold + underline removed words on deletion virtual lines.
    pub text_overlays: Vec<fresh_core::api::VirtualLineTextOverlay>,
    /// Draw only in a pane showing the buffer in compose view.
    ///
    /// Decorations live on the buffer, but compose view is a property of a
    /// pane, so a plugin composing one pane (markdown_compose's table
    /// borders, code-block rails) would otherwise draw that frame around
    /// the raw source in a sibling pane. Every render pass filters on this
    /// one flag, so the passes cannot disagree about which rows exist: a
    /// line suppressed in the token stream but not in the wrap index makes
    /// the index describe rows the renderer never draws.
    pub compose_only: bool,
}

impl VirtualText {
    /// Resolve the on-screen `Style` for this entry against a live theme.
    ///
    /// Theme keys take precedence over the fallback `style`'s fg/bg.  If a
    /// key fails to resolve (e.g. the theme doesn't define it), the
    /// fallback colour is kept.  Modifiers from `style` (bold/italic/etc.)
    /// always survive.
    pub fn resolved_style(&self, theme: &crate::view::theme::Theme) -> Style {
        let mut style = self.style;
        // A key brings its text attributes along with its color.
        if let Some(ref key) = self.fg_theme_key {
            if let Some(color) = theme.resolve_theme_key(key) {
                style = style
                    .fg(color)
                    .add_modifier(theme.resolve_modifier_key(key));
            }
        }
        if let Some(ref key) = self.bg_theme_key {
            if let Some(color) = theme.resolve_theme_key(key) {
                style = style
                    .bg(color)
                    .add_modifier(theme.resolve_modifier_key(key));
            }
        }
        style
    }
}

/// The marker list an entry lives in. Each class has a list of its own, so
/// that a query for one kind of entry — "the lines drawn below this line",
/// "how many lines are drawn before this byte" — is a query of one tree that
/// holds nothing else: no filtering of other entries' markers, no scan.
///
/// Compose-only lines are classes of their own because a pane out of compose
/// view doesn't draw them, and its row counts must not include them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Class {
    Inline,
    Above,
    Below,
    ComposeAbove,
    ComposeBelow,
}

impl Class {
    const ALL: [Class; 5] = [
        Class::Inline,
        Class::Above,
        Class::Below,
        Class::ComposeAbove,
        Class::ComposeBelow,
    ];

    fn of(position: VirtualTextPosition, compose_only: bool) -> Self {
        match (position, compose_only) {
            (VirtualTextPosition::BeforeChar | VirtualTextPosition::AfterChar, _) => Class::Inline,
            (VirtualTextPosition::LineAbove, false) => Class::Above,
            (VirtualTextPosition::LineBelow, false) => Class::Below,
            (VirtualTextPosition::LineAbove, true) => Class::ComposeAbove,
            (VirtualTextPosition::LineBelow, true) => Class::ComposeBelow,
        }
    }

    fn index(self) -> usize {
        self as usize
    }

    /// The line classes a pane draws for `placement`: compose-only ones only
    /// in compose view.
    fn lines(placement: VirtualTextPosition, is_compose: bool) -> &'static [Class] {
        match (placement, is_compose) {
            (VirtualTextPosition::LineAbove, false) => &[Class::Above],
            (VirtualTextPosition::LineAbove, true) => &[Class::Above, Class::ComposeAbove],
            (VirtualTextPosition::LineBelow, false) => &[Class::Below],
            (VirtualTextPosition::LineBelow, true) => &[Class::Below, Class::ComposeBelow],
            _ => &[],
        }
    }
}

/// Unique identifier for a virtual text entry: its class and its marker in
/// that class's list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtualTextId {
    class: Class,
    marker: MarkerId,
}

impl VirtualTextId {
    /// The id as `(class, marker)` numbers, for a record that has to be
    /// stored as plain numbers (an undo step's displaced markers).
    pub(crate) fn to_parts(self) -> (u8, u64) {
        (self.class as u8, self.marker.0)
    }

    /// The id [`Self::to_parts`] gave these numbers.
    pub(crate) fn from_parts(class: u8, marker: u64) -> Option<Self> {
        Some(Self {
            class: *Class::ALL.get(usize::from(class))?,
            marker: MarkerId(marker),
        })
    }
}

/// One class's entries: their markers, and the entries by marker.
#[derive(Default)]
struct ClassStore {
    markers: MarkerList,
    texts: HashMap<MarkerId, VirtualText>,
}

impl ClassStore {
    /// The entries anchored in `[start, end)` in the list's order — position,
    /// then priority.
    fn in_range(&self, start: usize, end: usize) -> impl Iterator<Item = (usize, &VirtualText)> {
        self.markers
            .in_range(start, end)
            .into_iter()
            .filter_map(move |(marker, pos)| Some((pos, self.texts.get(&marker)?)))
    }

    /// The `skip`th entry anchored in `[start, end)` and up to `limit - 1`
    /// after it, found by rank: O(log n) each, whatever lies before them.
    fn slice(
        &self,
        start: usize,
        end: usize,
        skip: usize,
        limit: usize,
    ) -> impl Iterator<Item = (usize, &VirtualText)> {
        let first = self.markers.count_before(start).saturating_add(skip);
        let last = self
            .markers
            .count_before(end)
            .min(first.saturating_add(limit));
        (first..last).filter_map(move |k| {
            let (marker, pos) = self.markers.nth(k)?;
            Some((pos, self.texts.get(&marker)?))
        })
    }
}

/// Manages virtual text entries for a buffer
///
/// Each class of entry has a marker list of its own (see [`Class`]); its
/// interval tree tracks the entries' positions through edits and answers the
/// range, count and rank queries the views need. The entries are kept by the
/// marker that anchors them, so a marker found in a query is its entry.
pub struct VirtualTextManager {
    classes: [ClassStore; 5],
    /// Monotonic version, bumped on every mutation.  Folded into
    /// `pipeline_inputs_version` so that adding / removing virtual
    /// lines (e.g. markdown_compose's table borders) invalidates
    /// `LineWrapCache` / `WrapIndex` entries — same mechanism
    /// `SoftBreakManager` and `ConcealManager` use.
    version: u32,
}

/// Sort entries from more than one class by position, then priority — the
/// order each class's list keeps its own in.
fn by_position_then_priority(results: &mut [(usize, &VirtualText)]) {
    results.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.priority.cmp(&b.1.priority)));
}

impl VirtualTextManager {
    /// Create a new empty manager
    pub fn new() -> Self {
        Self {
            classes: Default::default(),
            version: 0,
        }
    }

    /// Monotonic version. Increments on every mutation to virtual text
    /// state. Used by `pipeline_inputs_version` to invalidate scroll-math
    /// caches keyed off `EditorState`.
    #[inline]
    pub fn version(&self) -> u32 {
        self.version
    }

    #[inline]
    fn bump_version(&mut self) {
        self.version = self.version.wrapping_add(1);
    }

    fn store(&self, class: Class) -> &ClassStore {
        &self.classes[class.index()]
    }

    /// Where the entry is now, or `None` if it is gone — used by async-paste
    /// to find where its placeholder ended up, so the pasted text lands there
    /// rather than where the cursor was at Ctrl+V time.
    pub fn position_of(&self, id: VirtualTextId) -> Option<usize> {
        self.store(id.class).markers.get_position(id.marker)
    }

    // ---- Following edits ----

    /// The entries anchored strictly inside `(start, end)`, with where they
    /// are: those a deletion of the range clamps to its start, so undoing it
    /// can put them back (see `EditorState::capture_displaced_markers`).
    pub fn strictly_inside(&self, start: usize, end: usize) -> Vec<(VirtualTextId, usize)> {
        let mut found = Vec::new();
        for class in Class::ALL {
            for (marker, pos) in self.store(class).markers.in_range(start + 1, end) {
                found.push((VirtualTextId { class, marker }, pos));
            }
        }
        found
    }

    /// Put an entry back where it was before an edit moved it (undo).
    pub fn set_position(&mut self, id: VirtualTextId, position: usize) {
        self.classes[id.class.index()]
            .markers
            .set_position(id.marker, position);
    }

    /// Move every entry for text inserted at `position`.
    pub fn adjust_for_insert(&mut self, position: usize, length: usize) {
        for store in &mut self.classes {
            store.markers.adjust_for_insert(position, length);
        }
    }

    /// Move every entry for text deleted at `position`.
    pub fn adjust_for_delete(&mut self, position: usize, length: usize) {
        for store in &mut self.classes {
            store.markers.adjust_for_delete(position, length);
        }
    }

    /// Move every entry for a batch of edits, as
    /// [`MarkerList::adjust_for_bulk_edits`] takes them.
    pub fn adjust_for_bulk_edits(&mut self, edits: &[(usize, usize, usize)]) {
        for store in &mut self.classes {
            store.markers.adjust_for_bulk_edits(edits);
        }
    }

    // ---- Adding ----

    /// The one way an entry enters the manager. The bump lives here rather
    /// than in each `add*` method so a new add path cannot forget it and
    /// leave the line-wrap cache serving a layout without the entry (#3311).
    fn insert(&mut self, position: usize, mut vtext: VirtualText) -> VirtualTextId {
        let class = Class::of(vtext.position, vtext.compose_only);
        let store = &mut self.classes[class.index()];
        let marker = store.markers.create_ordered(
            position,
            i64::from(vtext.priority),
            vtext.gravity == MarkerGravity::Left,
        );
        vtext.marker_id = marker;
        store.texts.insert(marker, vtext);
        self.bump_version();
        VirtualTextId { class, marker }
    }

    /// A fresh entry with every optional field unset.
    fn entry(
        text: String,
        style: Style,
        position: VirtualTextPosition,
        priority: i32,
        gravity: MarkerGravity,
    ) -> VirtualText {
        VirtualText {
            marker_id: MarkerId(0),
            gravity,
            text,
            style,
            fg_theme_key: None,
            bg_theme_key: None,
            pad_to_column: None,
            position,
            priority,
            string_id: None,
            namespace: None,
            gutter_glyph: None,
            gutter_color: None,
            text_overlays: Vec::new(),
            compose_only: false,
        }
    }

    /// Add a virtual text entry
    ///
    /// # Arguments
    /// * `position` - Byte offset in the buffer
    /// * `text` - Text to display
    /// * `style` - Styling for the text
    /// * `vtext_position` - Whether to render before or after the character
    /// * `priority` - Ordering priority (higher = later in render order)
    ///
    /// # Returns
    /// The ID of the created virtual text entry
    pub fn add(
        &mut self,
        position: usize,
        text: String,
        style: Style,
        vtext_position: VirtualTextPosition,
        priority: i32,
    ) -> VirtualTextId {
        let entry = Self::entry(text, style, vtext_position, priority, MarkerGravity::Right);
        self.insert(position, entry)
    }

    /// Add an inline virtual text entry whose foreground/background colours
    /// are stored as theme keys (resolved at render time so theme changes
    /// apply live).
    ///
    /// `style` is the fallback used when a theme key fails to resolve;
    /// `fg_theme_key` / `bg_theme_key` are the keys passed to
    /// `Theme::resolve_theme_key` (e.g. `"editor.line_number_fg"`).
    ///
    /// `gravity` decides what an insertion *at* the entry's own position
    /// does to it — see [`MarkerGravity`]. It is a parameter rather than a
    /// property of `vtext_position` because the two are not the same
    /// question: entries whose position survives edits (the async-paste
    /// placeholder, whose marker *is* the eventual insertion point) want to
    /// travel with the text, while an inlay hint rendered in front of its
    /// anchor wants to stay in front of what is typed there.
    #[allow(clippy::too_many_arguments)]
    pub fn add_with_theme_keys(
        &mut self,
        position: usize,
        text: String,
        style: Style,
        fg_theme_key: Option<String>,
        bg_theme_key: Option<String>,
        vtext_position: VirtualTextPosition,
        priority: i32,
        gravity: MarkerGravity,
    ) -> VirtualTextId {
        debug_assert!(
            vtext_position.is_inline(),
            "add_with_theme_keys requires BeforeChar or AfterChar"
        );
        let mut entry = Self::entry(text, style, vtext_position, priority, gravity);
        entry.fg_theme_key = fg_theme_key;
        entry.bg_theme_key = bg_theme_key;
        self.insert(position, entry)
    }

    /// Add a virtual text entry with a string identifier
    ///
    /// This is useful for plugins that need to track and remove virtual texts by name.
    pub fn add_with_id(
        &mut self,
        position: usize,
        text: String,
        style: Style,
        vtext_position: VirtualTextPosition,
        priority: i32,
        string_id: String,
    ) -> VirtualTextId {
        let mut entry = Self::entry(text, style, vtext_position, priority, MarkerGravity::Right);
        entry.string_id = Some(string_id);
        self.insert(position, entry)
    }

    /// String-id form of [`add_with_theme_keys`] — same as
    /// [`add_with_id`] but stores theme keys for live theme updates.
    #[allow(clippy::too_many_arguments)]
    pub fn add_with_id_and_theme_keys(
        &mut self,
        position: usize,
        text: String,
        style: Style,
        fg_theme_key: Option<String>,
        bg_theme_key: Option<String>,
        vtext_position: VirtualTextPosition,
        priority: i32,
        string_id: String,
        pad_to_column: Option<u32>,
        compose_only: bool,
    ) -> VirtualTextId {
        debug_assert!(
            vtext_position.is_inline(),
            "add_with_id_and_theme_keys requires BeforeChar or AfterChar"
        );
        let mut entry = Self::entry(text, style, vtext_position, priority, MarkerGravity::Right);
        entry.fg_theme_key = fg_theme_key;
        entry.bg_theme_key = bg_theme_key;
        entry.string_id = Some(string_id);
        entry.pad_to_column = pad_to_column;
        entry.compose_only = compose_only;
        self.insert(position, entry)
    }

    /// Add a virtual line (LineAbove or LineBelow) with namespace for bulk removal
    ///
    /// This is the primary API for features like git blame headers.
    ///
    /// # Arguments
    /// * `position` - Byte offset in the buffer (anchors the line to this position)
    /// * `text` - Full line content to display
    /// * `style` - Styling for the line
    /// * `placement` - LineAbove or LineBelow
    /// * `namespace` - Namespace for bulk removal (e.g., "git-blame")
    /// * `priority` - Ordering when multiple lines at same position
    pub fn add_line(
        &mut self,
        position: usize,
        text: String,
        style: Style,
        placement: VirtualTextPosition,
        namespace: VirtualTextNamespace,
        priority: i32,
    ) -> VirtualTextId {
        self.add_line_with_theme_keys(
            position,
            text,
            style,
            None,
            None,
            placement,
            namespace,
            priority,
            None,
            None,
            Vec::new(),
            false,
        )
    }

    /// Add a virtual line whose foreground/background colours are stored
    /// as theme keys (resolved at render time so theme changes apply
    /// live).
    ///
    /// `style` is the fallback used when a theme key fails to resolve;
    /// `fg_theme_key` / `bg_theme_key` are the keys passed to
    /// `Theme::resolve_theme_key` (e.g. `"editor.line_number_fg"`).
    #[allow(clippy::too_many_arguments)]
    pub fn add_line_with_theme_keys(
        &mut self,
        position: usize,
        text: String,
        style: Style,
        fg_theme_key: Option<String>,
        bg_theme_key: Option<String>,
        placement: VirtualTextPosition,
        namespace: VirtualTextNamespace,
        priority: i32,
        gutter_glyph: Option<String>,
        gutter_color: Option<Color>,
        text_overlays: Vec<fresh_core::api::VirtualLineTextOverlay>,
        compose_only: bool,
    ) -> VirtualTextId {
        debug_assert!(
            placement.is_line(),
            "add_line requires LineAbove or LineBelow"
        );
        let mut entry = Self::entry(text, style, placement, priority, MarkerGravity::Right);
        entry.fg_theme_key = fg_theme_key;
        entry.bg_theme_key = bg_theme_key;
        entry.namespace = Some(namespace);
        entry.gutter_glyph = gutter_glyph;
        entry.gutter_color = gutter_color;
        entry.text_overlays = text_overlays;
        entry.compose_only = compose_only;
        self.insert(position, entry)
    }

    // ---- Removing ----

    /// Remove the entries `matches` picks out of `classes`, anchored in
    /// `[start, end)` when a range is given; bumps the version if any went.
    fn remove_where(
        &mut self,
        classes: &[Class],
        range: Option<(usize, usize)>,
        matches: impl Fn(&VirtualText) -> bool,
    ) -> usize {
        let mut removed = 0;
        for &class in classes {
            let store = &mut self.classes[class.index()];
            let doomed: Vec<MarkerId> = match range {
                Some((start, end)) => store
                    .markers
                    .in_range(start, end)
                    .into_iter()
                    .map(|(marker, _)| marker)
                    .filter(|marker| store.texts.get(marker).is_some_and(&matches))
                    .collect(),
                None => store
                    .texts
                    .iter()
                    .filter(|(_, vtext)| matches(vtext))
                    .map(|(marker, _)| *marker)
                    .collect(),
            };
            for marker in doomed {
                store.texts.remove(&marker);
                store.markers.delete(marker);
                removed += 1;
            }
        }
        if removed > 0 {
            self.bump_version();
        }
        removed
    }

    /// Remove a virtual text entry by its string identifier
    pub fn remove_by_id(&mut self, string_id: &str) -> bool {
        self.remove_where(&Class::ALL, None, |vtext| {
            vtext.string_id.as_deref() == Some(string_id)
        }) > 0
    }

    /// Remove all virtual text entries whose string_id starts with the given prefix
    pub fn remove_by_prefix(&mut self, prefix: &str) {
        self.remove_where(&Class::ALL, None, |vtext| {
            vtext
                .string_id
                .as_deref()
                .is_some_and(|sid| sid.starts_with(prefix))
        });
    }

    /// Remove a virtual text entry
    pub fn remove(&mut self, id: VirtualTextId) -> bool {
        let store = &mut self.classes[id.class.index()];
        if store.texts.remove(&id.marker).is_none() {
            return false;
        }
        store.markers.delete(id.marker);
        self.bump_version();
        true
    }

    /// Clear all virtual text entries
    pub fn clear(&mut self) {
        if self.is_empty() {
            return;
        }
        for store in &mut self.classes {
            *store = ClassStore::default();
        }
        self.bump_version();
    }

    /// Remove all virtual text entries whose marker position lies within the
    /// half-open byte range `[start, end)`.
    ///
    /// This must be called BEFORE the buffer and the entries are adjusted for
    /// a deletion, otherwise the affected entries will already have been
    /// clamped to the deletion start and appear to fall outside the range.
    /// Used by the editor to drop stale inlay hints whose anchors have been
    /// erased by the user (a fresh LSP response will repopulate them if still
    /// applicable).
    ///
    /// Returns the number of entries removed.
    pub fn remove_in_range(&mut self, start: usize, end: usize) -> usize {
        self.remove_where(&Class::ALL, Some((start, end)), |_| true)
    }

    /// Clear all virtual texts in a namespace
    ///
    /// This is the primary way plugins remove their virtual texts (e.g., before updating blame data).
    pub fn clear_namespace(&mut self, namespace: &VirtualTextNamespace) {
        self.remove_where(&Class::ALL, None, |vtext| {
            vtext.namespace.as_ref() == Some(namespace)
        });
    }

    /// Remove virtual LINES in `namespace` whose anchor byte falls in
    /// `[start, end)`. The per-line analogue of `clear_namespace`, so a plugin
    /// can rebuild one line's virtual lines without dropping the rest of the
    /// namespace. Non-line virtual texts and other namespaces are left
    /// untouched.
    pub fn clear_lines_in_range(
        &mut self,
        namespace: &VirtualTextNamespace,
        start: usize,
        end: usize,
    ) {
        self.remove_where(
            &[
                Class::Above,
                Class::Below,
                Class::ComposeAbove,
                Class::ComposeBelow,
            ],
            Some((start, end)),
            |vtext| vtext.namespace.as_ref() == Some(namespace),
        );
    }

    /// Remove INLINE virtual texts (BeforeChar/AfterChar) whose string id
    /// starts with `id_prefix` and whose anchor falls in `[start, end)`.
    ///
    /// The inline counterpart of [`clear_lines_in_range`], and it exists for
    /// the same reason: a plugin rebuilding one line's decorations needs to
    /// drop that line's — and only that line's — previous ones, in the same
    /// command batch as the re-add, or the decoration strobes for a frame.
    /// Neither of the existing inline removals can do that: `remove_by_id`
    /// needs the exact id, which a plugin can only reconstruct from the
    /// position the decoration was *last* placed at, and `remove_by_prefix`
    /// takes the whole set, dropping the lines outside the current batch.
    ///
    /// Matching is by id prefix rather than namespace because the inline add
    /// path stores no namespace (only virtual *lines* carry one); the string
    /// id plugins already supply serves the same grouping purpose.
    ///
    /// Anchors are the entries' current positions, so a line that has
    /// shifted since the decoration was placed is matched where it is now —
    /// which is what makes the clear tolerant of the lag between a
    /// `lines_changed` hook and the plugin's reply.
    pub fn clear_inline_in_range(&mut self, id_prefix: &str, start: usize, end: usize) {
        self.remove_where(&[Class::Inline], Some((start, end)), |vtext| {
            vtext
                .string_id
                .as_ref()
                .is_some_and(|s| s.starts_with(id_prefix))
        });
    }

    // ---- Reading ----

    /// Get the number of virtual text entries
    pub fn len(&self) -> usize {
        self.classes.iter().map(|store| store.texts.len()).sum()
    }

    /// Check if there are no virtual text entries
    pub fn is_empty(&self) -> bool {
        self.classes.iter().all(|store| store.texts.is_empty())
    }

    /// Entries of `classes` anchored in `[start, end)`, by position then
    /// priority.
    fn query_classes(
        &self,
        classes: &[Class],
        start: usize,
        end: usize,
    ) -> Vec<(usize, &VirtualText)> {
        let mut results: Vec<(usize, &VirtualText)> = classes
            .iter()
            .flat_map(|class| self.store(*class).in_range(start, end))
            .collect();
        if classes.len() > 1 {
            by_position_then_priority(&mut results);
        }
        results
    }

    /// Query virtual texts in a byte range
    ///
    /// Returns a vector of (byte_position, &VirtualText) pairs, sorted by:
    /// 1. Byte position (ascending)
    /// 2. Priority (ascending, so higher priority renders later)
    ///
    /// # Arguments
    /// * `start` - Start byte offset (inclusive)
    /// * `end` - End byte offset (exclusive)
    pub fn query_range(&self, start: usize, end: usize) -> Vec<(usize, &VirtualText)> {
        self.query_classes(&Class::ALL, start, end)
    }

    /// Build a lookup map for efficient per-character access during rendering
    ///
    /// Returns a HashMap where keys are byte positions and values are vectors
    /// of virtual texts at that position, sorted by priority.
    pub fn build_lookup(&self, start: usize, end: usize) -> HashMap<usize, Vec<&VirtualText>> {
        let mut lookup: HashMap<usize, Vec<&VirtualText>> = HashMap::new();
        for (pos, vtext) in self.query_range(start, end) {
            lookup.entry(pos).or_default().push(vtext);
        }
        lookup
    }

    /// Query only virtual LINES (LineAbove/LineBelow) in a byte range
    ///
    /// Used by the render pipeline to inject header/footer lines.
    /// Returns (byte_position, &VirtualText) pairs sorted by position then priority.
    pub fn query_lines_in_range(&self, start: usize, end: usize) -> Vec<(usize, &VirtualText)> {
        self.query_classes(
            &[
                Class::Above,
                Class::Below,
                Class::ComposeAbove,
                Class::ComposeBelow,
            ],
            start,
            end,
        )
    }

    /// Every virtual LINE in the buffer a pane in or out of compose view
    /// (`is_compose`) draws, as [`Self::query_lines_in_range`] returns them.
    pub fn all_lines(&self, is_compose: bool) -> Vec<(usize, &VirtualText)> {
        let classes: &[Class] = if is_compose {
            &[
                Class::Above,
                Class::Below,
                Class::ComposeAbove,
                Class::ComposeBelow,
            ]
        } else {
            &[Class::Above, Class::Below]
        };
        self.query_classes(classes, 0, usize::MAX)
    }

    /// Query only INLINE virtual texts (BeforeChar/AfterChar) in a byte range
    ///
    /// Used by the render pipeline to inject inline hints.
    pub fn query_inline_in_range(&self, start: usize, end: usize) -> Vec<(usize, &VirtualText)> {
        self.query_classes(&[Class::Inline], start, end)
    }

    /// How many virtual lines a pane in or out of compose view draws for
    /// anchors before `position`: above and below their lines. O(log n)
    pub fn lines_before(&self, position: usize, is_compose: bool) -> usize {
        [
            VirtualTextPosition::LineAbove,
            VirtualTextPosition::LineBelow,
        ]
        .into_iter()
        .flat_map(|placement| Class::lines(placement, is_compose))
        .map(|class| self.store(*class).markers.count_before(position))
        .sum()
    }

    /// How many `placement` lines a pane in or out of compose view draws for
    /// anchors in `[start, end)`. O(log n)
    pub fn lines_in(
        &self,
        placement: VirtualTextPosition,
        is_compose: bool,
        start: usize,
        end: usize,
    ) -> usize {
        Class::lines(placement, is_compose)
            .iter()
            .map(|class| self.store(*class).markers.count_in(start, end))
            .sum()
    }

    /// The `placement` lines anchored in `[start, end)` that a pane in or out
    /// of compose view draws, by position then priority, from the `skip`th,
    /// at most `limit` of them: what a window that shows only some of a
    /// line's virtual lines needs, without reading the rest. Found by rank,
    /// O(log n) each, out of compose view; in it, the compose-only ones are
    /// merged in.
    pub fn lines_window(
        &self,
        placement: VirtualTextPosition,
        is_compose: bool,
        start: usize,
        end: usize,
        skip: usize,
        limit: usize,
    ) -> Vec<(usize, &VirtualText)> {
        match Class::lines(placement, is_compose) {
            [class] => self.store(*class).slice(start, end, skip, limit).collect(),
            classes => {
                let mut all = self.query_classes(classes, start, end);
                all.drain(..skip.min(all.len()));
                all.truncate(limit);
                all
            }
        }
    }
}

impl Default for VirtualTextManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::style::Color;

    fn hint_style() -> Style {
        Style::default().fg(Color::DarkGray)
    }

    /// Gravity is a per-entry choice, not something inferred from which
    /// side the entry renders on: an insertion at the entry's own position
    /// pushes a right-gravity entry along and leaves a left-gravity one
    /// where it is. Inlay hints ask for the left variant so an edit at a
    /// hint's anchor cannot drag the hint off its line (issue #722).
    #[test]
    fn gravity_decides_what_an_insertion_at_the_marker_does() {
        let mut manager = VirtualTextManager::new();

        let right = manager.add_with_theme_keys(
            6,
            "R".to_string(),
            hint_style(),
            None,
            None,
            VirtualTextPosition::BeforeChar,
            0,
            MarkerGravity::Right,
        );
        let left = manager.add_with_theme_keys(
            6,
            "L".to_string(),
            hint_style(),
            None,
            None,
            VirtualTextPosition::BeforeChar,
            0,
            MarkerGravity::Left,
        );

        // Ten bytes inserted at exactly byte 6 — the position both entries
        // are anchored to.
        manager.adjust_for_insert(6, 10);

        assert_eq!(
            manager.position_of(right),
            Some(16),
            "right gravity travels with the text inserted at it",
        );
        assert_eq!(
            manager.position_of(left),
            Some(6),
            "left gravity stays in front of the text inserted at it",
        );
    }

    /// The line-wrap cache keys on `version()`, so an add that leaves it
    /// unchanged can be served a layout without the new entry (#3311).
    /// Every add path is exercised on its own, with no removal in the
    /// same pass whose bump could cover for it.
    #[test]
    fn every_add_path_bumps_the_version() {
        type AddFn = fn(&mut VirtualTextManager);
        let paths: [(&str, AddFn); 6] = [
            ("add", |m| {
                m.add(
                    0,
                    "x".into(),
                    hint_style(),
                    VirtualTextPosition::AfterChar,
                    0,
                );
            }),
            ("add_with_theme_keys", |m| {
                m.add_with_theme_keys(
                    0,
                    "x".into(),
                    hint_style(),
                    None,
                    None,
                    VirtualTextPosition::AfterChar,
                    0,
                    MarkerGravity::Right,
                );
            }),
            ("add_with_id", |m| {
                m.add_with_id(
                    0,
                    "x".into(),
                    hint_style(),
                    VirtualTextPosition::AfterChar,
                    0,
                    "id".into(),
                );
            }),
            ("add_with_id_and_theme_keys", |m| {
                m.add_with_id_and_theme_keys(
                    0,
                    "x".into(),
                    hint_style(),
                    None,
                    None,
                    VirtualTextPosition::AfterChar,
                    0,
                    "id".into(),
                    None,
                    false,
                );
            }),
            ("add_line", |m| {
                m.add_line(
                    0,
                    "x".into(),
                    hint_style(),
                    VirtualTextPosition::LineAbove,
                    VirtualTextNamespace::from_string("ns".into()),
                    0,
                );
            }),
            ("add_line_with_theme_keys", |m| {
                m.add_line_with_theme_keys(
                    0,
                    "x".into(),
                    hint_style(),
                    None,
                    None,
                    VirtualTextPosition::LineAbove,
                    VirtualTextNamespace::from_string("ns".into()),
                    0,
                    None,
                    None,
                    Vec::new(),
                    false,
                );
            }),
        ];

        for (name, add) in paths {
            let mut manager = VirtualTextManager::new();
            let before = manager.version();
            add(&mut manager);
            assert_ne!(manager.version(), before, "{name} must bump the version");
        }
    }

    #[test]
    fn test_new_manager() {
        let manager = VirtualTextManager::new();
        assert_eq!(manager.len(), 0);
        assert!(manager.is_empty());
    }

    #[test]
    fn test_add_virtual_text() {
        let mut manager = VirtualTextManager::new();

        let id = manager.add(
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        assert_eq!(manager.len(), 1);
        assert!(!manager.is_empty());
        assert_eq!(manager.position_of(id), Some(10));
    }

    #[test]
    fn test_remove_virtual_text() {
        let mut manager = VirtualTextManager::new();

        let id = manager.add(
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        assert_eq!(manager.len(), 1);

        let removed = manager.remove(id);
        assert!(removed);
        assert_eq!(manager.len(), 0);

        // Marker should also be removed
        assert_eq!(manager.len(), 0);
    }

    #[test]
    fn test_remove_nonexistent() {
        let mut manager = VirtualTextManager::new();

        let removed = manager.remove(VirtualTextId::from_parts(0, 999).unwrap());
        assert!(!removed);
    }

    #[test]
    fn test_clear() {
        let mut manager = VirtualTextManager::new();

        manager.add(
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );
        manager.add(
            20,
            ": String".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        assert_eq!(manager.len(), 2);
        assert_eq!(manager.len(), 2);

        manager.clear();

        assert_eq!(manager.len(), 0);
        assert_eq!(manager.len(), 0);
    }

    /// Every query answers what a plain list of the entries — each with its
    /// position, followed through the same edits — answers: the range queries
    /// in position-then-priority order, the counts, and the windows the
    /// renderer takes. Edits are made the way the editor makes them: a
    /// deletion first drops the entries anchored in it (`apply_delete`), so
    /// none is clamped onto another's position.
    mod model {
        use super::*;
        use proptest::prelude::*;

        #[derive(Debug, Clone)]
        enum Op {
            Add {
                pos: usize,
                kind: u8,
                compose: bool,
                priority: i32,
                left: bool,
            },
            Remove(usize),
            RemoveInRange(usize, usize),
            ClearLinesInRange(usize, usize),
            ClearInlineInRange(usize, usize),
            Insert(usize, usize),
            Delete(usize, usize),
        }

        fn op() -> impl Strategy<Value = Op> {
            prop_oneof![
                4 => (0..60usize, 0..4u8, any::<bool>(), -3..3i32, any::<bool>()).prop_map(
                    |(pos, kind, compose, priority, left)| Op::Add { pos, kind, compose, priority, left }
                ),
                1 => (0..200usize).prop_map(Op::Remove),
                1 => (0..60usize, 0..15usize).prop_map(|(s, l)| Op::RemoveInRange(s, s + l)),
                1 => (0..60usize, 0..15usize).prop_map(|(s, l)| Op::ClearLinesInRange(s, s + l)),
                1 => (0..60usize, 0..15usize).prop_map(|(s, l)| Op::ClearInlineInRange(s, s + l)),
                2 => (0..60usize, 1..10usize).prop_map(|(p, l)| Op::Insert(p, l)),
                2 => (0..60usize, 1..10usize).prop_map(|(p, l)| Op::Delete(p, l)),
            ]
        }

        /// One entry of the model.
        #[derive(Debug, Clone)]
        struct Entry {
            id: VirtualTextId,
            pos: usize,
            position: VirtualTextPosition,
            compose: bool,
            priority: i32,
            left: bool,
            /// Unique, so results compare by identity.
            text: String,
        }

        fn kind(k: u8) -> VirtualTextPosition {
            [
                VirtualTextPosition::BeforeChar,
                VirtualTextPosition::AfterChar,
                VirtualTextPosition::LineAbove,
                VirtualTextPosition::LineBelow,
            ][usize::from(k)]
        }

        /// The model's entries matching `keep`, anchored in `[start, end)`, by
        /// position, then priority, then when they were added.
        fn expect(
            model: &[Entry],
            start: usize,
            end: usize,
            keep: impl Fn(&Entry) -> bool,
        ) -> Vec<(usize, String)> {
            let mut v: Vec<&Entry> = model
                .iter()
                .filter(|e| e.pos >= start && e.pos < end && keep(e))
                .collect();
            // Ties: entries of one class in the order they were added; a
            // query over several classes takes them class by class.
            v.sort_by_key(|e| (e.pos, e.priority, Class::of(e.position, e.compose).index()));
            v.into_iter().map(|e| (e.pos, e.text.clone())).collect()
        }

        fn got(results: Vec<(usize, &VirtualText)>) -> Vec<(usize, String)> {
            results
                .into_iter()
                .map(|(p, v)| (p, v.text.clone()))
                .collect()
        }

        proptest! {
            #[test]
            fn queries_match_a_list_of_the_entries(ops in prop::collection::vec(op(), 1..60)) {
                let mut manager = VirtualTextManager::new();
                let mut model: Vec<Entry> = Vec::new();
                let ns = VirtualTextNamespace::from_string("ns".to_string());
                for (n, op) in ops.into_iter().enumerate() {
                    match op {
                        Op::Add { pos, kind: k, compose, priority, left } => {
                            let position = kind(k);
                            let text = format!("#{n}");
                            let (compose, left) = if position.is_line() {
                                (compose, false)
                            } else {
                                (false, left)
                            };
                            let id = if position.is_line() {
                                manager.add_line_with_theme_keys(
                                    pos, text.clone(), hint_style(), None, None, position,
                                    ns.clone(), priority, None, None, Vec::new(), compose,
                                )
                            } else {
                                let gravity = if left { MarkerGravity::Left } else { MarkerGravity::Right };
                                manager.add_with_theme_keys(
                                    pos, text.clone(), hint_style(), None, None, position,
                                    priority, gravity,
                                )
                            };
                            model.push(Entry { id, pos, position, compose, priority, left, text });
                        }
                        Op::Remove(i) => {
                            if !model.is_empty() {
                                let e = model.remove(i % model.len());
                                prop_assert!(manager.remove(e.id));
                                prop_assert!(!manager.remove(e.id), "removed twice");
                            }
                        }
                        Op::RemoveInRange(s, e) => {
                            let before = model.len();
                            model.retain(|x| !(x.pos >= s && x.pos < e));
                            prop_assert_eq!(manager.remove_in_range(s, e), before - model.len());
                        }
                        Op::ClearLinesInRange(s, e) => {
                            manager.clear_lines_in_range(&ns, s, e);
                            model.retain(|x| !(x.position.is_line() && x.pos >= s && x.pos < e));
                        }
                        Op::ClearInlineInRange(s, e) => {
                            // Inline entries added here carry no string id.
                            manager.clear_inline_in_range("", s, e);
                        }
                        Op::Insert(p, l) => {
                            manager.adjust_for_insert(p, l);
                            for x in &mut model {
                                if x.pos > p || (x.pos == p && !x.left) {
                                    x.pos += l;
                                }
                            }
                        }
                        Op::Delete(p, l) => {
                            manager.remove_in_range(p, p + l);
                            model.retain(|x| !(x.pos >= p && x.pos < p + l));
                            manager.adjust_for_delete(p, l);
                            for x in &mut model {
                                if x.pos >= p + l {
                                    x.pos -= l;
                                }
                            }
                        }
                    }

                    prop_assert_eq!(manager.len(), model.len());
                    for x in &model {
                        prop_assert_eq!(manager.position_of(x.id), Some(x.pos), "{}", x.text);
                    }
                    for (s, e) in [(0, usize::MAX), (0, 20), (10, 40), (25, 26), (30, 30), (45, 200)] {
                        prop_assert_eq!(got(manager.query_range(s, e)), expect(&model, s, e, |_| true));
                        prop_assert_eq!(
                            got(manager.query_lines_in_range(s, e)),
                            expect(&model, s, e, |x| x.position.is_line())
                        );
                        prop_assert_eq!(
                            got(manager.query_inline_in_range(s, e)),
                            expect(&model, s, e, |x| x.position.is_inline())
                        );
                        for compose in [false, true] {
                            let drawn = |x: &Entry| compose || !x.compose;
                            prop_assert_eq!(
                                manager.lines_before(e, compose),
                                model.iter().filter(|x| x.position.is_line() && drawn(x) && x.pos < e).count()
                            );
                            for placement in [VirtualTextPosition::LineAbove, VirtualTextPosition::LineBelow] {
                                let all = expect(&model, s, e, |x| x.position == placement && drawn(x));
                                prop_assert_eq!(manager.lines_in(placement, compose, s, e), all.len());
                                for (skip, limit) in [(0, usize::MAX), (1, 2), (2, 1), (0, 0), (5, 3)] {
                                    let window: Vec<_> = all.iter().skip(skip).take(limit).cloned().collect();
                                    prop_assert_eq!(
                                        got(manager.lines_window(placement, compose, s, e, skip, limit)),
                                        window,
                                        "lines_window({:?}, {}, {}, {}, {}, {})",
                                        placement, compose, s, e, skip, limit
                                    );
                                }
                            }
                        }
                    }
                }
                manager.clear();
                prop_assert!(manager.is_empty() && manager.query_range(0, usize::MAX).is_empty());
            }
        }
    }

    #[test]
    fn test_query_range() {
        let mut manager = VirtualTextManager::new();

        // Add three virtual texts at positions 10, 20, 30
        manager.add(
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );
        manager.add(
            20,
            ": String".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );
        manager.add(
            30,
            ": bool".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        // Query range [15, 35) should return positions 20 and 30
        let results = manager.query_range(15, 35);
        assert_eq!(results.len(), 2);
        assert_eq!(results[0].0, 20);
        assert_eq!(results[0].1.text, ": String");
        assert_eq!(results[1].0, 30);
        assert_eq!(results[1].1.text, ": bool");

        // Query range [0, 15) should return position 10
        let results = manager.query_range(0, 15);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, 10);
        assert_eq!(results[0].1.text, ": i32");
    }

    #[test]
    fn test_query_empty_range() {
        let mut manager = VirtualTextManager::new();

        manager.add(
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        // Query range with no virtual texts
        let results = manager.query_range(100, 200);
        assert!(results.is_empty());
    }

    #[test]
    fn test_priority_ordering() {
        let mut manager = VirtualTextManager::new();

        // Add multiple virtual texts at the same position with different priorities
        manager.add(
            10,
            "low".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );
        manager.add(
            10,
            "high".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            10,
        );
        manager.add(
            10,
            "medium".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            5,
        );

        let results = manager.query_range(0, 20);
        assert_eq!(results.len(), 3);
        // Should be sorted by priority: 0, 5, 10
        assert_eq!(results[0].1.text, "low");
        assert_eq!(results[1].1.text, "medium");
        assert_eq!(results[2].1.text, "high");
    }

    #[test]
    fn test_build_lookup() {
        let mut manager = VirtualTextManager::new();

        manager.add(
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );
        manager.add(
            10,
            " = 5".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            1,
        );
        manager.add(
            20,
            ": String".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        let lookup = manager.build_lookup(0, 30);

        assert_eq!(lookup.len(), 2); // Two unique positions

        let at_10 = lookup.get(&10).unwrap();
        assert_eq!(at_10.len(), 2);
        assert_eq!(at_10[0].text, ": i32"); // priority 0
        assert_eq!(at_10[1].text, " = 5"); // priority 1

        let at_20 = lookup.get(&20).unwrap();
        assert_eq!(at_20.len(), 1);
        assert_eq!(at_20[0].text, ": String");
    }

    #[test]
    fn test_position_tracking_after_insert() {
        let mut manager = VirtualTextManager::new();

        manager.add(
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        // Insert 5 bytes before position 10
        manager.adjust_for_insert(5, 5);

        // Virtual text should now be at position 15
        let results = manager.query_range(0, 20);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, 15);
    }

    #[test]
    fn test_position_tracking_after_delete() {
        let mut manager = VirtualTextManager::new();

        manager.add(
            20,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        // Delete 5 bytes before position 20 (at position 10)
        manager.adjust_for_delete(10, 5);

        // Virtual text should now be at position 15
        let results = manager.query_range(0, 20);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, 15);
    }

    #[test]
    fn test_clear_lines_in_range_tolerates_offset_anchor() {
        // The per-line markdown table border pass clears `[byteStart, byteEnd)`
        // and re-adds. Under the async lines_changed lag a previously-emitted
        // border rides a few bytes ahead of the event's byteStart, so the clear
        // must be range-wide (not single-byte) to catch it — otherwise a stale
        // doubled separator is stranded. This is the regression guard for that.
        let mut manager = VirtualTextManager::new();
        let ns = VirtualTextNamespace::from_string("md-tb".to_string());

        // Two row borders: one for the row at byte 10, one for the row at 30.
        manager.add_line(
            10,
            "─".to_string(),
            hint_style(),
            VirtualTextPosition::LineAbove,
            ns.clone(),
            0,
        );
        manager.add_line(
            30,
            "─".to_string(),
            hint_style(),
            VirtualTextPosition::LineAbove,
            ns.clone(),
            0,
        );

        // An edit inserts 1 byte at pos 5: both borders ride forward to 11 / 31.
        manager.adjust_for_insert(5, 1);

        // The event still reports the first row at its pre-lag span [10, 20). A
        // single-byte clear at byte 10 would MISS the border now at 11; the
        // range clear catches it and leaves the far row (at 31) intact.
        manager.clear_lines_in_range(&ns, 10, 20);

        let remaining = manager.query_lines_in_range(0, 100);
        assert_eq!(
            remaining.len(),
            1,
            "offset border in range should be cleared"
        );
        assert_eq!(remaining[0].0, 31, "the far row's border must survive");
    }

    #[test]
    fn test_before_and_after_positions() {
        let mut manager = VirtualTextManager::new();

        manager.add(
            10,
            "/*param=*/".to_string(),
            hint_style(),
            VirtualTextPosition::BeforeChar,
            0,
        );
        manager.add(
            10,
            ": Type".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        let lookup = manager.build_lookup(0, 20);
        let at_10 = lookup.get(&10).unwrap();

        assert_eq!(at_10.len(), 2);
        // Both at same position, check they have different positions
        let before = at_10
            .iter()
            .find(|vt| vt.position == VirtualTextPosition::BeforeChar);
        let after = at_10
            .iter()
            .find(|vt| vt.position == VirtualTextPosition::AfterChar);

        assert!(before.is_some());
        assert!(after.is_some());
        assert_eq!(before.unwrap().text, "/*param=*/");
        assert_eq!(after.unwrap().text, ": Type");
    }
}
