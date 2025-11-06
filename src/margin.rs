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
            1 + self.width + if self.show_separator { self.separator.chars().count() } else { 0 }
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
    pub fn with_id(line: usize, position: MarginPosition, content: MarginContent, id: String) -> Self {
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
#[derive(Debug, Clone)]
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

    /// Whether to show line numbers by default
    pub show_line_numbers: bool,

    /// Diagnostic indicators per line (displayed between line numbers and separator)
    /// Maps line number to (symbol, color) tuple
    diagnostic_indicators: BTreeMap<usize, (String, Color)>,
}

impl MarginManager {
    /// Create a new margin manager with default settings
    pub fn new() -> Self {
        Self {
            left_config: MarginConfig::left_default(),
            right_config: MarginConfig::right_default(),
            left_annotations: BTreeMap::new(),
            right_annotations: BTreeMap::new(),
            show_line_numbers: true,
            diagnostic_indicators: BTreeMap::new(),
        }
    }

    /// Create a margin manager with line numbers disabled
    pub fn without_line_numbers() -> Self {
        let mut manager = Self::new();
        manager.show_line_numbers = false;
        manager
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
    pub fn get_at_line(&self, line: usize, position: MarginPosition) -> Option<&[MarginAnnotation]> {
        let annotations = match position {
            MarginPosition::Left => &self.left_annotations,
            MarginPosition::Right => &self.right_annotations,
        };
        annotations.get(&line).map(|v| v.as_slice())
    }

    /// Get the content to render for a specific line in a margin
    /// If show_line_numbers is true and position is Left, includes line number
    pub fn render_line(&self, line: usize, position: MarginPosition, buffer_total_lines: usize) -> MarginContent {
        let annotations = match position {
            MarginPosition::Left => &self.left_annotations,
            MarginPosition::Right => &self.right_annotations,
        };

        // Get user annotations
        let user_annotations = annotations.get(&line).cloned().unwrap_or_default();

        // For left margin, combine with line numbers if enabled
        if position == MarginPosition::Left && self.show_line_numbers {
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

    /// Update the left margin width based on buffer size
    /// This should be called when the buffer grows significantly
    pub fn update_width_for_buffer(&mut self, buffer_total_lines: usize) {
        if self.show_line_numbers {
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

    /// Enable or disable line numbers
    pub fn set_line_numbers(&mut self, enabled: bool) {
        self.show_line_numbers = enabled;
        if !enabled {
            self.left_config.width = 0;
            self.left_config.enabled = false;
        } else {
            self.left_config.enabled = true;
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
        manager.show_line_numbers = true;

        // Without annotations, should render line number
        let content = manager.render_line(5, MarginPosition::Left, 100);
        let (rendered, _) = content.render(4);
        assert!(rendered.contains("6")); // Line 5 is displayed as "6" (1-indexed)

        // Add a breakpoint annotation
        manager.add_annotation(MarginAnnotation::breakpoint(5));

        // Should now render stacked content (line number + breakpoint)
        let content = manager.render_line(5, MarginPosition::Left, 100);
        assert!(matches!(content, MarginContent::Stacked(_)));
    }

    #[test]
    fn test_margin_manager_update_width() {
        let mut manager = MarginManager::new();
        manager.show_line_numbers = true;

        // Small buffer
        manager.update_width_for_buffer(99);
        assert_eq!(manager.left_config.width, 4); // Minimum 4

        // Medium buffer (4 digits)
        manager.update_width_for_buffer(1000);
        assert_eq!(manager.left_config.width, 4);

        // Large buffer (5 digits)
        manager.update_width_for_buffer(10000);
        assert_eq!(manager.left_config.width, 5);

        // Very large buffer (7 digits)
        manager.update_width_for_buffer(1000000);
        assert_eq!(manager.left_config.width, 7);
    }

    #[test]
    fn test_margin_manager_without_line_numbers() {
        let manager = MarginManager::without_line_numbers();
        assert!(!manager.show_line_numbers);

        let content = manager.render_line(5, MarginPosition::Left, 100);
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
}
