use crate::model::marker::{MarkerId, MarkerList};
use ratatui::style::{Color, Style};
use std::collections::BTreeMap;

/// Position of a margin in the editor
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MarginPosition {
    /// Left margin (before the text)
    Left,
    /// Right margin (after the text)
    Right,
}

/// A line indicator displayed in the gutter's indicator column
/// Can be used for git status, breakpoints, bookmarks, etc.
///
/// Indicators are anchored to byte positions via markers, so they automatically
/// shift when text is inserted or deleted before them.
#[derive(Debug, Clone, PartialEq)]
pub struct LineIndicator {
    /// The symbol to display (e.g., "│", "●", "★")
    pub symbol: String,
    /// The color of the indicator
    pub color: Color,
    /// Priority for display when multiple indicators exist (higher wins)
    pub priority: i32,
    /// Marker ID anchoring this indicator to a byte position
    /// The line number is derived from this position at render time
    pub marker_id: MarkerId,
}

impl LineIndicator {
    /// Create a new line indicator (marker_id will be set when added to MarginManager)
    pub fn new(symbol: impl Into<String>, color: Color, priority: i32) -> Self {
        Self {
            symbol: symbol.into(),
            color,
            priority,
            marker_id: MarkerId(0), // Placeholder, set by MarginManager
        }
    }

    /// Create a line indicator with a specific marker ID
    pub fn with_marker(
        symbol: impl Into<String>,
        color: Color,
        priority: i32,
        marker_id: MarkerId,
    ) -> Self {
        Self {
            symbol: symbol.into(),
            color,
            priority,
            marker_id,
        }
    }
}

/// Content type for a margin at a specific line
#[derive(Debug, Clone, PartialEq)]
pub enum MarginContent {
    /// Simple text (e.g., line number)
    Text(String),
    /// Symbol with optional color (e.g., breakpoint, error indicator)
    Symbol { text: String, style: Style },
    /// Multiple items stacked (e.g., line number + breakpoint)
    Stacked(Vec<MarginContent>),
    /// Empty/cleared margin
    Empty,
}

impl MarginContent {
    /// Create a simple text margin content
    pub fn text(text: impl Into<String>) -> Self {
        Self::Text(text.into())
    }

    /// Create a symbol with styling
    pub fn symbol(text: impl Into<String>, style: Style) -> Self {
        Self::Symbol {
            text: text.into(),
            style,
        }
    }

    /// Create a colored symbol
    pub fn colored_symbol(text: impl Into<String>, color: Color) -> Self {
        Self::Symbol {
            text: text.into(),
            style: Style::default().fg(color),
        }
    }

    /// Check if this margin content is empty
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    /// Render this margin content to a string with width padding
    pub fn render(&self, width: usize) -> (String, Option<Style>) {
        match self {
            Self::Text(text) => {
                let padded = format!("{:>width$}", text, width = width);
                (padded, None)
            }
            Self::Symbol { text, style } => {
                let padded = format!("{:>width$}", text, width = width);
                (padded, Some(*style))
            }
            Self::Stacked(items) => {
                // For stacked items, render the last non-empty one
                for item in items.iter().rev() {
                    if !item.is_empty() {
                        return item.render(width);
                    }
                }
                (format!("{:>width$}", "", width = width), None)
            }
            Self::Empty => (format!("{:>width$}", "", width = width), None),
        }
    }
}

/// Configuration for a margin
#[derive(Debug, Clone, PartialEq)]
pub struct MarginConfig {
    /// Position of the margin (left or right)
    pub position: MarginPosition,

    /// Width of the margin in characters
    /// For left margin with line numbers, this is calculated dynamically
    pub width: usize,

    /// Whether this margin is enabled
    pub enabled: bool,

    /// Whether to show a separator (e.g., "│") after the margin
    pub show_separator: bool,

    /// Separator character(s)
    pub separator: String,

    /// Default style for the margin
    pub style: Style,

    /// Default separator style
    pub separator_style: Style,
}

impl MarginConfig {
    /// Create a default left margin config (for line numbers)
    pub fn left_default() -> Self {
        Self {
            position: MarginPosition::Left,
            width: 4, // Minimum 4 digits for line numbers
            enabled: true,
            show_separator: true,
            separator: " │ ".to_string(), // Separator with spaces: " │ " (space before for indicators, space after for readability)
            style: Style::default().fg(Color::DarkGray),
            separator_style: Style::default().fg(Color::DarkGray),
        }
    }

    /// Create a default right margin config
    pub fn right_default() -> Self {
        Self {
            position: MarginPosition::Right,
            width: 0,
            enabled: false,
            show_separator: false,
            separator: String::new(),
            style: Style::default(),
            separator_style: Style::default(),
        }
    }

    /// Calculate the total width including indicator column and separator
    /// Format: [indicator (1 char)][line_number (N chars)][separator (3 chars)]
    pub fn total_width(&self) -> usize {
        if self.enabled {
            // 1 char for indicator column + line number width + separator
            1 + self.width
                + if self.show_separator {
                    self.separator.chars().count()
                } else {
                    0
                }
        } else {
            0
        }
    }
}

/// A margin annotation for a specific line
#[derive(Debug, Clone)]
pub struct MarginAnnotation {
    /// The line number (0-indexed)
    pub line: usize,

    /// The margin position (left or right)
    pub position: MarginPosition,

    /// The content to display
    pub content: MarginContent,

    /// Optional ID for this annotation (for removal/updates)
    pub id: Option<String>,
}

impl MarginAnnotation {
    /// Create a new margin annotation
    pub fn new(line: usize, position: MarginPosition, content: MarginContent) -> Self {
        Self {
            line,
            position,
            content,
            id: None,
        }
    }

    /// Create an annotation with an ID
    pub fn with_id(
        line: usize,
        position: MarginPosition,
        content: MarginContent,
        id: String,
    ) -> Self {
        Self {
            line,
            position,
            content,
            id: Some(id),
        }
    }

    /// Helper: Create a line number annotation for the left margin
    pub fn line_number(line: usize) -> Self {
        Self::new(
            line,
            MarginPosition::Left,
            MarginContent::text(format!("{}", line + 1)), // 1-indexed display
        )
    }

    /// Helper: Create a breakpoint indicator
    pub fn breakpoint(line: usize) -> Self {
        Self::new(
            line,
            MarginPosition::Left,
            MarginContent::colored_symbol("●", Color::Red),
        )
    }

    /// Helper: Create an error indicator
    pub fn error(line: usize) -> Self {
        Self::new(
            line,
            MarginPosition::Left,
            MarginContent::colored_symbol("✗", Color::Red),
        )
    }

    /// Helper: Create a warning indicator
    pub fn warning(line: usize) -> Self {
        Self::new(
            line,
            MarginPosition::Left,
            MarginContent::colored_symbol("⚠", Color::Yellow),
        )
    }

    /// Helper: Create an info indicator
    pub fn info(line: usize) -> Self {
        Self::new(
            line,
            MarginPosition::Left,
            MarginContent::colored_symbol("ℹ", Color::Blue),
        )
    }
}

/// Manages margins and annotations for a buffer
/// This is similar to OverlayManager - a general-purpose primitive for margin decorations
///
/// Line indicators use byte-position markers that automatically adjust when the buffer
/// is edited. This ensures indicators stay anchored to the content they represent.
#[derive(Debug)]
pub struct MarginManager {
    /// Configuration for left margin
    pub left_config: MarginConfig,

    /// Configuration for right margin
    pub right_config: MarginConfig,

    /// Annotations per line (left margin)
    /// Uses BTreeMap for efficient range queries
    left_annotations: BTreeMap<usize, Vec<MarginAnnotation>>,

    /// Annotations per line (right margin)
    right_annotations: BTreeMap<usize, Vec<MarginAnnotation>>,

    /// Diagnostic indicators per line (displayed between line numbers and separator)
    /// Maps line number to (symbol, color) tuple
    diagnostic_indicators: BTreeMap<usize, (String, Color)>,

    /// Marker list for tracking indicator positions through edits
    /// Shared with the buffer's edit tracking
    indicator_markers: MarkerList,

    /// Line indicators stored by marker ID
    /// Maps marker_id -> (namespace -> indicator)
    /// The line number is computed at render time from the marker's byte position
    line_indicators: BTreeMap<u64, BTreeMap<String, LineIndicator>>,
}

impl MarginManager {
    /// Create a new margin manager with default settings
    pub fn new() -> Self {
        Self {
            left_config: MarginConfig::left_default(),
            right_config: MarginConfig::right_default(),
            left_annotations: BTreeMap::new(),
            right_annotations: BTreeMap::new(),
            diagnostic_indicators: BTreeMap::new(),
            indicator_markers: MarkerList::new(),
            line_indicators: BTreeMap::new(),
        }
    }

    /// Create a margin manager with line numbers disabled
    pub fn without_line_numbers() -> Self {
        let mut manager = Self::new();
        manager.left_config.width = 0;
        manager.left_config.enabled = false;
        manager
    }

    // =========================================================================
    // Edit Propagation - called when buffer content changes
    // =========================================================================

    /// Adjust all indicator markers after an insertion
    /// Call this when text is inserted into the buffer
    pub fn adjust_for_insert(&mut self, position: usize, length: usize) {
        self.indicator_markers.adjust_for_insert(position, length);
    }

    /// Adjust all indicator markers after a deletion
    /// Call this when text is deleted from the buffer
    pub fn adjust_for_delete(&mut self, position: usize, length: usize) {
        self.indicator_markers.adjust_for_delete(position, length);
    }

    /// Set a diagnostic indicator for a line
    pub fn set_diagnostic_indicator(&mut self, line: usize, symbol: String, color: Color) {
        self.diagnostic_indicators.insert(line, (symbol, color));
    }

    /// Remove diagnostic indicator for a line
    pub fn remove_diagnostic_indicator(&mut self, line: usize) {
        self.diagnostic_indicators.remove(&line);
    }

    /// Clear all diagnostic indicators
    pub fn clear_diagnostic_indicators(&mut self) {
        self.diagnostic_indicators.clear();
    }

    /// Get diagnostic indicator for a line
    pub fn get_diagnostic_indicator(&self, line: usize) -> Option<&(String, Color)> {
        self.diagnostic_indicators.get(&line)
    }

    /// Set a line indicator at a byte position for a specific namespace
    ///
    /// The indicator is anchored to the byte position and will automatically
    /// shift when text is inserted or deleted before it.
    ///
    /// Returns the marker ID that can be used to remove or update the indicator.
    pub fn set_line_indicator(
        &mut self,
        byte_offset: usize,
        namespace: String,
        mut indicator: LineIndicator,
    ) -> MarkerId {
        // Create a marker at this byte position (left affinity - stays before inserted text)
        let marker_id = self.indicator_markers.create(byte_offset, true);
        indicator.marker_id = marker_id;

        self.line_indicators
            .entry(marker_id.0)
            .or_default()
            .insert(namespace, indicator);

        marker_id
    }

    /// Remove line indicator for a specific namespace at a marker
    pub fn remove_line_indicator(&mut self, marker_id: MarkerId, namespace: &str) {
        if let Some(indicators) = self.line_indicators.get_mut(&marker_id.0) {
            indicators.remove(namespace);
            if indicators.is_empty() {
                self.line_indicators.remove(&marker_id.0);
                self.indicator_markers.delete(marker_id);
            }
        }
    }

    /// Clear all line indicators for a specific namespace
    pub fn clear_line_indicators_for_namespace(&mut self, namespace: &str) {
        // Collect marker IDs to delete (can't modify while iterating)
        let mut markers_to_delete = Vec::new();

        for (&marker_id, indicators) in self.line_indicators.iter_mut() {
            indicators.remove(namespace);
            if indicators.is_empty() {
                markers_to_delete.push(marker_id);
            }
        }

        // Delete empty marker entries and their markers
        for marker_id in markers_to_delete {
            self.line_indicators.remove(&marker_id);
            self.indicator_markers.delete(MarkerId(marker_id));
        }
    }

    /// Get the line indicator for a specific line number
    ///
    /// This looks up all indicators whose markers resolve to the given line.
    /// Returns the highest priority indicator if multiple exist on the same line.
    ///
    /// Note: This is O(n) in the number of indicators. For rendering, prefer
    /// `get_indicators_in_viewport` which is more efficient.
    pub fn get_line_indicator(
        &self,
        line: usize,
        get_line_fn: impl Fn(usize) -> usize,
    ) -> Option<&LineIndicator> {
        // Find all indicators on this line
        let mut best: Option<&LineIndicator> = None;

        for (&marker_id, indicators) in &self.line_indicators {
            if let Some(byte_pos) = self.indicator_markers.get_position(MarkerId(marker_id)) {
                let indicator_line = get_line_fn(byte_pos);
                if indicator_line == line {
                    // Found an indicator on this line, check if it's higher priority
                    for indicator in indicators.values() {
                        if best.is_none() || indicator.priority > best.unwrap().priority {
                            best = Some(indicator);
                        }
                    }
                }
            }
        }

        best
    }

    /// Get indicators within a viewport byte range
    ///
    /// Only queries markers within `viewport_start..viewport_end`, avoiding
    /// iteration over the entire indicator set.
    ///
    /// Returns a map of line_number -> highest priority indicator for that line.
    /// The `get_line_fn` converts byte offsets to line numbers.
    pub fn get_indicators_for_viewport(
        &self,
        viewport_start: usize,
        viewport_end: usize,
        get_line_fn: impl Fn(usize) -> usize,
    ) -> BTreeMap<usize, LineIndicator> {
        let mut by_line: BTreeMap<usize, LineIndicator> = BTreeMap::new();

        // Query only markers within the viewport byte range
        for (marker_id, byte_pos, _end) in self
            .indicator_markers
            .query_range(viewport_start, viewport_end)
        {
            // Look up the indicators for this marker
            if let Some(indicators) = self.line_indicators.get(&marker_id.0) {
                let line = get_line_fn(byte_pos);

                // Get highest priority indicator for this marker
                if let Some(indicator) = indicators.values().max_by_key(|ind| ind.priority) {
                    // Check if this is higher priority than existing indicator on this line
                    if let Some(existing) = by_line.get(&line) {
                        if indicator.priority > existing.priority {
                            by_line.insert(line, indicator.clone());
                        }
                    } else {
                        by_line.insert(line, indicator.clone());
                    }
                }
            }
        }

        by_line
    }

    /// Add an annotation to a margin
    pub fn add_annotation(&mut self, annotation: MarginAnnotation) {
        let annotations = match annotation.position {
            MarginPosition::Left => &mut self.left_annotations,
            MarginPosition::Right => &mut self.right_annotations,
        };

        annotations
            .entry(annotation.line)
            .or_insert_with(Vec::new)
            .push(annotation);
    }

    /// Remove all annotations with a specific ID
    pub fn remove_by_id(&mut self, id: &str) {
        // Remove from left annotations
        for annotations in self.left_annotations.values_mut() {
            annotations.retain(|a| a.id.as_deref() != Some(id));
        }

        // Remove from right annotations
        for annotations in self.right_annotations.values_mut() {
            annotations.retain(|a| a.id.as_deref() != Some(id));
        }

        // Clean up empty entries
        self.left_annotations.retain(|_, v| !v.is_empty());
        self.right_annotations.retain(|_, v| !v.is_empty());
    }

    /// Remove all annotations at a specific line
    pub fn remove_at_line(&mut self, line: usize, position: MarginPosition) {
        match position {
            MarginPosition::Left => {
                self.left_annotations.remove(&line);
            }
            MarginPosition::Right => {
                self.right_annotations.remove(&line);
            }
        }
    }

    /// Clear all annotations in a position
    pub fn clear_position(&mut self, position: MarginPosition) {
        match position {
            MarginPosition::Left => self.left_annotations.clear(),
            MarginPosition::Right => self.right_annotations.clear(),
        }
    }

    /// Clear all annotations
    pub fn clear_all(&mut self) {
        self.left_annotations.clear();
        self.right_annotations.clear();
    }

    /// Get all annotations at a specific line
    pub fn get_at_line(
        &self,
        line: usize,
        position: MarginPosition,
    ) -> Option<&[MarginAnnotation]> {
        let annotations = match position {
            MarginPosition::Left => &self.left_annotations,
            MarginPosition::Right => &self.right_annotations,
        };
        annotations.get(&line).map(|v| v.as_slice())
    }

    /// Get the content to render for a specific line in a margin.
    /// If `show_line_numbers` is true and position is Left, includes line number.
    pub fn render_line(
        &self,
        line: usize,
        position: MarginPosition,
        _buffer_total_lines: usize,
        show_line_numbers: bool,
    ) -> MarginContent {
        let annotations = match position {
            MarginPosition::Left => &self.left_annotations,
            MarginPosition::Right => &self.right_annotations,
        };

        // Get user annotations
        let user_annotations = annotations.get(&line).cloned().unwrap_or_default();

        // For left margin, combine with line numbers if enabled
        if position == MarginPosition::Left && show_line_numbers {
            let line_num = MarginContent::text(format!("{}", line + 1));

            if user_annotations.is_empty() {
                return line_num;
            }

            // Stack line number with user annotations
            let mut stack = vec![line_num];
            stack.extend(user_annotations.into_iter().map(|a| a.content));
            MarginContent::Stacked(stack)
        } else if let Some(annotation) = user_annotations.first() {
            annotation.content.clone()
        } else {
            MarginContent::Empty
        }
    }

    /// Update the left margin width based on buffer size.
    /// Only adjusts width when `show_line_numbers` is true.
    pub fn update_width_for_buffer(&mut self, buffer_total_lines: usize, show_line_numbers: bool) {
        if show_line_numbers {
            let digits = if buffer_total_lines == 0 {
                1
            } else {
                ((buffer_total_lines as f64).log10().floor() as usize) + 1
            };
            self.left_config.width = digits.max(4);
        }
    }

    /// Get the total width of the left margin (including separator)
    /// The separator includes the diagnostic indicator when present
    pub fn left_total_width(&self) -> usize {
        self.left_config.total_width()
    }

    /// Get the total width of the right margin (including separator)
    pub fn right_total_width(&self) -> usize {
        self.right_config.total_width()
    }

    /// Configure left margin layout for line number visibility.
    ///
    /// This adjusts `left_config.enabled` and `left_config.width` so that
    /// `left_total_width()` returns the correct gutter size for the given
    /// `show_line_numbers` setting. Called at render time with the per-split
    /// line number state.
    pub fn configure_for_line_numbers(&mut self, show_line_numbers: bool) {
        if !show_line_numbers {
            self.left_config.width = 0;
            self.left_config.enabled = false;
        } else {
            self.left_config.enabled = true;
            if self.left_config.width == 0 {
                self.left_config.width = 4;
            }
        }
    }

    /// Get the number of annotations in a position
    pub fn annotation_count(&self, position: MarginPosition) -> usize {
        match position {
            MarginPosition::Left => self.left_annotations.values().map(|v| v.len()).sum(),
            MarginPosition::Right => self.right_annotations.values().map(|v| v.len()).sum(),
        }
    }
}

impl Default for MarginManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_margin_content_text() {
        let content = MarginContent::text("123");
        let (rendered, style) = content.render(5);
        assert_eq!(rendered, "  123");
        assert!(style.is_none());
    }

    #[test]
    fn test_margin_content_symbol() {
        let content = MarginContent::colored_symbol("●", Color::Red);
        let (rendered, style) = content.render(3);
        assert_eq!(rendered, "  ●");
        assert!(style.is_some());
    }

    #[test]
    fn test_margin_config_total_width() {
        let mut config = MarginConfig::left_default();
        config.width = 4;
        config.separator = " │ ".to_string();
        assert_eq!(config.total_width(), 8); // 1 (indicator) + 4 (line num) + 3 (separator)

        config.show_separator = false;
        assert_eq!(config.total_width(), 5); // 1 (indicator) + 4 (line num)

        config.enabled = false;
        assert_eq!(config.total_width(), 0);
    }

    #[test]
    fn test_margin_annotation_helpers() {
        let line_num = MarginAnnotation::line_number(5);
        assert_eq!(line_num.line, 5);
        assert_eq!(line_num.position, MarginPosition::Left);

        let breakpoint = MarginAnnotation::breakpoint(10);
        assert_eq!(breakpoint.line, 10);
        assert_eq!(breakpoint.position, MarginPosition::Left);
    }

    #[test]
    fn test_margin_manager_add_remove() {
        let mut manager = MarginManager::new();

        // Add annotation
        let annotation = MarginAnnotation::line_number(5);
        manager.add_annotation(annotation);

        assert_eq!(manager.annotation_count(MarginPosition::Left), 1);

        // Add annotation with ID
        let annotation = MarginAnnotation::with_id(
            10,
            MarginPosition::Left,
            MarginContent::text("test"),
            "test-id".to_string(),
        );
        manager.add_annotation(annotation);

        assert_eq!(manager.annotation_count(MarginPosition::Left), 2);

        // Remove by ID
        manager.remove_by_id("test-id");
        assert_eq!(manager.annotation_count(MarginPosition::Left), 1);

        // Clear all
        manager.clear_all();
        assert_eq!(manager.annotation_count(MarginPosition::Left), 0);
    }

    #[test]
    fn test_margin_manager_render_line() {
        let mut manager = MarginManager::new();

        // Without annotations, should render line number when show_line_numbers=true
        let content = manager.render_line(5, MarginPosition::Left, 100, true);
        let (rendered, _) = content.render(4);
        assert!(rendered.contains("6")); // Line 5 is displayed as "6" (1-indexed)

        // Add a breakpoint annotation
        manager.add_annotation(MarginAnnotation::breakpoint(5));

        // Should now render stacked content (line number + breakpoint)
        let content = manager.render_line(5, MarginPosition::Left, 100, true);
        assert!(matches!(content, MarginContent::Stacked(_)));
    }

    #[test]
    fn test_margin_manager_update_width() {
        let mut manager = MarginManager::new();

        // Small buffer
        manager.update_width_for_buffer(99, true);
        assert_eq!(manager.left_config.width, 4); // Minimum 4

        // Medium buffer (4 digits)
        manager.update_width_for_buffer(1000, true);
        assert_eq!(manager.left_config.width, 4);

        // Large buffer (5 digits)
        manager.update_width_for_buffer(10000, true);
        assert_eq!(manager.left_config.width, 5);

        // Very large buffer (7 digits)
        manager.update_width_for_buffer(1000000, true);
        assert_eq!(manager.left_config.width, 7);
    }

    #[test]
    fn test_margin_manager_without_line_numbers() {
        let manager = MarginManager::without_line_numbers();
        assert!(!manager.left_config.enabled);

        let content = manager.render_line(5, MarginPosition::Left, 100, false);
        assert!(content.is_empty());
    }

    #[test]
    fn test_margin_position_left_right() {
        let mut manager = MarginManager::new();

        manager.add_annotation(MarginAnnotation::new(
            1,
            MarginPosition::Left,
            MarginContent::text("left"),
        ));

        manager.add_annotation(MarginAnnotation::new(
            1,
            MarginPosition::Right,
            MarginContent::text("right"),
        ));

        assert_eq!(manager.annotation_count(MarginPosition::Left), 1);
        assert_eq!(manager.annotation_count(MarginPosition::Right), 1);

        manager.clear_position(MarginPosition::Left);
        assert_eq!(manager.annotation_count(MarginPosition::Left), 0);
        assert_eq!(manager.annotation_count(MarginPosition::Right), 1);
    }

    // Helper: simulates a buffer where each line is 10 bytes (9 chars + newline)
    // Line 0 = bytes 0-9, Line 1 = bytes 10-19, etc.
    fn byte_to_line(byte_offset: usize) -> usize {
        byte_offset / 10
    }

    // Helper: get byte offset for start of a line
    fn line_to_byte(line: usize) -> usize {
        line * 10
    }

    #[test]
    fn test_line_indicator_basic() {
        let mut manager = MarginManager::new();

        // Add a line indicator at byte offset 50 (line 5 in our simulated buffer)
        let indicator = LineIndicator::new("│", Color::Green, 10);
        manager.set_line_indicator(line_to_byte(5), "git-gutter".to_string(), indicator);

        // Check it can be retrieved on line 5
        let retrieved = manager.get_line_indicator(5, byte_to_line);
        assert!(retrieved.is_some());
        let retrieved = retrieved.unwrap();
        assert_eq!(retrieved.symbol, "│");
        assert_eq!(retrieved.color, Color::Green);
        assert_eq!(retrieved.priority, 10);

        // Non-existent line should return None
        assert!(manager.get_line_indicator(10, byte_to_line).is_none());
    }

    #[test]
    fn test_line_indicator_multiple_namespaces() {
        let mut manager = MarginManager::new();

        // Add indicators from different namespaces at the same byte position (line 5)
        let git_indicator = LineIndicator::new("│", Color::Green, 10);
        let breakpoint_indicator = LineIndicator::new("●", Color::Red, 20);

        manager.set_line_indicator(line_to_byte(5), "git-gutter".to_string(), git_indicator);
        manager.set_line_indicator(
            line_to_byte(5),
            "breakpoints".to_string(),
            breakpoint_indicator,
        );

        // Should return the highest priority indicator
        let retrieved = manager.get_line_indicator(5, byte_to_line);
        assert!(retrieved.is_some());
        let retrieved = retrieved.unwrap();
        assert_eq!(retrieved.symbol, "●"); // Breakpoint has higher priority
        assert_eq!(retrieved.priority, 20);
    }

    #[test]
    fn test_line_indicator_clear_namespace() {
        let mut manager = MarginManager::new();

        // Add indicators on multiple lines
        manager.set_line_indicator(
            line_to_byte(1),
            "git-gutter".to_string(),
            LineIndicator::new("│", Color::Green, 10),
        );
        manager.set_line_indicator(
            line_to_byte(2),
            "git-gutter".to_string(),
            LineIndicator::new("│", Color::Yellow, 10),
        );
        manager.set_line_indicator(
            line_to_byte(3),
            "breakpoints".to_string(),
            LineIndicator::new("●", Color::Red, 20),
        );

        // Clear git-gutter namespace
        manager.clear_line_indicators_for_namespace("git-gutter");

        // Git gutter indicators should be gone
        assert!(manager.get_line_indicator(1, byte_to_line).is_none());
        assert!(manager.get_line_indicator(2, byte_to_line).is_none());

        // Breakpoint should still be there
        let breakpoint = manager.get_line_indicator(3, byte_to_line);
        assert!(breakpoint.is_some());
        assert_eq!(breakpoint.unwrap().symbol, "●");
    }

    #[test]
    fn test_line_indicator_remove_specific() {
        let mut manager = MarginManager::new();

        // Add two indicators at the same byte position (line 5)
        let git_marker = manager.set_line_indicator(
            line_to_byte(5),
            "git-gutter".to_string(),
            LineIndicator::new("│", Color::Green, 10),
        );
        let bp_marker = manager.set_line_indicator(
            line_to_byte(5),
            "breakpoints".to_string(),
            LineIndicator::new("●", Color::Red, 20),
        );

        // Remove just the git-gutter indicator
        manager.remove_line_indicator(git_marker, "git-gutter");

        // Should still have the breakpoint indicator on line 5
        let retrieved = manager.get_line_indicator(5, byte_to_line);
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().symbol, "●");

        // Remove the breakpoint indicator too
        manager.remove_line_indicator(bp_marker, "breakpoints");

        // Now no indicators on line 5
        assert!(manager.get_line_indicator(5, byte_to_line).is_none());
    }

    #[test]
    fn test_line_indicator_shifts_on_insert() {
        let mut manager = MarginManager::new();

        // Add indicator on line 5 (byte 50)
        manager.set_line_indicator(
            line_to_byte(5),
            "git-gutter".to_string(),
            LineIndicator::new("│", Color::Green, 10),
        );

        // Verify it's on line 5
        assert!(manager.get_line_indicator(5, byte_to_line).is_some());
        assert!(manager.get_line_indicator(6, byte_to_line).is_none());

        // Insert 10 bytes (one line) at the beginning
        manager.adjust_for_insert(0, 10);

        // Now indicator should be on line 6 (shifted down by 1)
        assert!(manager.get_line_indicator(5, byte_to_line).is_none());
        assert!(manager.get_line_indicator(6, byte_to_line).is_some());
    }

    #[test]
    fn test_line_indicator_shifts_on_delete() {
        let mut manager = MarginManager::new();

        // Add indicator on line 5 (byte 50)
        manager.set_line_indicator(
            line_to_byte(5),
            "git-gutter".to_string(),
            LineIndicator::new("│", Color::Green, 10),
        );

        // Verify it's on line 5
        assert!(manager.get_line_indicator(5, byte_to_line).is_some());

        // Delete first 20 bytes (2 lines)
        manager.adjust_for_delete(0, 20);

        // Now indicator should be on line 3 (shifted up by 2)
        assert!(manager.get_line_indicator(5, byte_to_line).is_none());
        assert!(manager.get_line_indicator(3, byte_to_line).is_some());
    }

    #[test]
    fn test_multiple_indicators_shift_together() {
        let mut manager = MarginManager::new();

        // Add indicators on lines 3, 5, and 7
        manager.set_line_indicator(
            line_to_byte(3),
            "git-gutter".to_string(),
            LineIndicator::new("│", Color::Green, 10),
        );
        manager.set_line_indicator(
            line_to_byte(5),
            "git-gutter".to_string(),
            LineIndicator::new("│", Color::Yellow, 10),
        );
        manager.set_line_indicator(
            line_to_byte(7),
            "git-gutter".to_string(),
            LineIndicator::new("│", Color::Red, 10),
        );

        // Insert 2 lines (20 bytes) at byte 25 (middle of line 2)
        // This should shift lines 3, 5, 7 -> lines 5, 7, 9
        manager.adjust_for_insert(25, 20);

        // Old positions should be empty
        assert!(manager.get_line_indicator(3, byte_to_line).is_none());

        // New positions should have indicators
        assert!(manager.get_line_indicator(5, byte_to_line).is_some());
        assert!(manager.get_line_indicator(7, byte_to_line).is_some());
        assert!(manager.get_line_indicator(9, byte_to_line).is_some());
    }
}
