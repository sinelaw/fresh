// Visual regression testing utilities

use ratatui::buffer::Buffer;
use ratatui::style::Color;
use serde::{Deserialize, Serialize};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// Metadata for a single step in a visual test flow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StepMetadata {
    pub step_num: usize,
    pub name: String,
    pub description: String,
    pub image_filename: String,
}

/// Metadata for an entire test flow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowMetadata {
    pub flow_name: String,
    pub category: String,
    pub description: String,
    pub steps: Vec<StepMetadata>,
}

/// Helper for capturing visual test flows
/// Each flow writes its own markdown file to avoid race conditions in parallel test execution
pub struct VisualFlow {
    flow_name: String,
    flow_name_sanitized: String, // For filenames (no spaces)
    category: String,
    description: String,
    steps: Vec<StepMetadata>,
    step_num: usize,
    auto_write: bool, // Whether to auto-write on drop
}

impl VisualFlow {
    /// Create a new visual flow
    pub fn new(flow_name: &str, category: &str, description: &str) -> Self {
        // Sanitize flow name for use in filenames (replace spaces with underscores)
        let flow_name_sanitized = flow_name.replace(' ', "_");

        Self {
            flow_name: flow_name.to_string(),
            flow_name_sanitized,
            category: category.to_string(),
            description: description.to_string(),
            steps: Vec::new(),
            step_num: 1,
            auto_write: true,
        }
    }

    /// Capture a step in the flow
    pub fn step(
        &mut self,
        buffer: &Buffer,
        cursor_pos: (u16, u16),
        step_name: &str,
        description: &str,
    ) -> io::Result<()> {
        let screen_text = buffer_to_string(buffer);
        let snapshot_name = format!("{}__{}", self.flow_name, step_name);

        // Take text snapshot with insta
        insta::assert_snapshot!(snapshot_name, &screen_text);

        // Generate SVG image with sanitized filename (no spaces)
        let image_filename = format!(
            "{}_{:02}_{}.svg",
            self.flow_name_sanitized, self.step_num, step_name
        );
        let image_path = PathBuf::from("docs/visual-regression/screenshots").join(&image_filename);

        // Only update image if needed
        if should_update_image(&image_path)? {
            render_buffer_to_svg(buffer, cursor_pos, &image_path)?;
        }

        // Track metadata
        self.steps.push(StepMetadata {
            step_num: self.step_num,
            name: step_name.to_string(),
            description: description.to_string(),
            image_filename,
        });

        self.step_num += 1;
        Ok(())
    }

    /// Finalize the flow and write its documentation file
    pub fn finalize(mut self) {
        self.write_documentation_file().ok();
        // Disable auto-write on drop since we've already written
        self.auto_write = false;
    }

    /// Write this flow's documentation to its own markdown file
    fn write_documentation_file(&self) -> io::Result<()> {
        if self.steps.is_empty() {
            return Ok(());
        }

        // Create docs/visual-regression/tests/ directory
        let docs_dir = PathBuf::from("docs/visual-regression/tests");
        fs::create_dir_all(&docs_dir)?;

        // Write individual test markdown file
        let test_file = docs_dir.join(format!("{}.md", self.flow_name_sanitized));
        let mut md = String::new();

        md.push_str(&format!("# {}\n\n", self.flow_name));
        md.push_str(&format!("**Category**: {}\n\n", self.category));
        if !self.description.is_empty() {
            md.push_str(&format!("*{}*\n\n", self.description));
        }
        md.push_str("---\n\n");

        for step in &self.steps {
            md.push_str(&format!("## Step {}: {}\n\n", step.step_num, step.name));
            md.push_str(&format!(
                "![{}](../screenshots/{})\n\n",
                step.name, step.image_filename
            ));
            md.push_str(&format!("*{}*\n\n", step.description));
        }

        fs::write(&test_file, md)?;

        Ok(())
    }
}

impl Drop for VisualFlow {
    fn drop(&mut self) {
        // Auto-write documentation when flow is dropped (unless explicitly finalized)
        if self.auto_write && !self.steps.is_empty() {
            self.write_documentation_file().ok();
        }
    }
}

/// Convert ratatui Buffer to string (same as EditorTestHarness::screen_to_string)
fn buffer_to_string(buffer: &Buffer) -> String {
    let (width, height) = (buffer.area.width, buffer.area.height);
    let mut result = String::new();

    for y in 0..height {
        for x in 0..width {
            let cell = &buffer[(x, y)];
            result.push_str(cell.symbol());
        }
        if y < height - 1 {
            result.push('\n');
        }
    }

    result
}

/// Check if image needs to be updated
fn should_update_image(image_path: &Path) -> io::Result<bool> {
    // Always update if image doesn't exist
    if !image_path.exists() {
        return Ok(true);
    }

    // Check if we're in update mode via environment variable
    if std::env::var("UPDATE_VISUAL_BASELINES").unwrap_or_default() == "always" {
        return Ok(true);
    }

    // Update if snapshot file is newer than image
    // Note: This is a simple heuristic. In practice, insta manages snapshots
    // so we'll update images whenever tests run to keep them in sync
    Ok(true)
}

/// Render a ratatui Buffer to SVG format
pub fn render_buffer_to_svg(
    buffer: &Buffer,
    cursor_pos: (u16, u16),
    path: &Path,
) -> io::Result<()> {
    const CHAR_WIDTH: u16 = 9;
    const CHAR_HEIGHT: u16 = 18;
    const FONT_SIZE: u16 = 14;

    let width = buffer.area.width;
    let height = buffer.area.height;
    let svg_width = width * CHAR_WIDTH;
    let svg_height = height * CHAR_HEIGHT;

    let mut svg = format!(
        r##"<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="{svg_width}" height="{svg_height}" viewBox="0 0 {svg_width} {svg_height}">
<style>
    .terminal {{ font-family: 'Courier New', 'Consolas', monospace; font-size: {FONT_SIZE}px; white-space: pre; }}
</style>
<rect width="100%" height="100%" fill="#000000"/>
"##
    );

    // Render cells
    for y in 0..height {
        for x in 0..width {
            let cell = &buffer[(x, y)];
            let style = cell.style();
            let symbol = cell.symbol();

            // Draw background if not default/black (skip black since SVG base is black)
            if let Some(bg) = style.bg {
                // Skip black backgrounds - let the SVG base black show through
                if !matches!(bg, Color::Black | Color::Reset) {
                    let bg_hex = color_to_hex(bg);
                    svg.push_str(&format!(
                        r#"  <rect x="{}" y="{}" width="{}" height="{}" fill="{}"/>"#,
                        x * CHAR_WIDTH,
                        y * CHAR_HEIGHT,
                        CHAR_WIDTH,
                        CHAR_HEIGHT,
                        bg_hex
                    ));
                    svg.push('\n');
                }
            }

            // Draw text
            if !symbol.trim().is_empty() {
                let fg = style.fg.unwrap_or(Color::White);
                let fg_hex = color_to_hex(fg);
                let escaped = xml_escape(symbol);

                // Add bold/italic/underline classes
                let classes = String::from("terminal");
                let mut style_str = String::new();

                if style.add_modifier.contains(ratatui::style::Modifier::BOLD) {
                    style_str.push_str("font-weight:bold;");
                }
                if style
                    .add_modifier
                    .contains(ratatui::style::Modifier::ITALIC)
                {
                    style_str.push_str("font-style:italic;");
                }
                if style
                    .add_modifier
                    .contains(ratatui::style::Modifier::UNDERLINED)
                {
                    style_str.push_str("text-decoration:underline;");
                }

                svg.push_str(&format!(
                    r#"  <text x="{}" y="{}" fill="{}" class="{}" style="{}">{}</text>"#,
                    x * CHAR_WIDTH + 1,
                    y * CHAR_HEIGHT + FONT_SIZE,
                    fg_hex,
                    classes,
                    style_str,
                    escaped
                ));
                svg.push('\n');
            }
        }
    }

    // Draw cursor
    let (cursor_x, cursor_y) = cursor_pos;
    svg.push_str(&format!(
        "  <!-- Cursor indicator -->\n  <rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"none\" stroke=\"#ffffff\" stroke-width=\"2\" opacity=\"0.8\"/>\n",
        cursor_x * CHAR_WIDTH,
        cursor_y * CHAR_HEIGHT,
        CHAR_WIDTH,
        CHAR_HEIGHT
    ));

    svg.push_str("</svg>");

    // Ensure parent directory exists
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path, svg)?;
    Ok(())
}

/// Convert ratatui Color to hex string
fn color_to_hex(color: Color) -> String {
    match color {
        Color::Reset => "#cccccc".to_string(),
        Color::Black => "#000000".to_string(),
        Color::Red => "#ff5555".to_string(),
        Color::Green => "#50fa7b".to_string(),
        Color::Yellow => "#f1fa8c".to_string(),
        Color::Blue => "#bd93f9".to_string(),
        Color::Magenta => "#ff79c6".to_string(),
        Color::Cyan => "#8be9fd".to_string(),
        Color::Gray => "#6272a4".to_string(),
        Color::DarkGray => "#44475a".to_string(),
        Color::LightRed => "#ff6e6e".to_string(),
        Color::LightGreen => "#69ff94".to_string(),
        Color::LightYellow => "#ffffa5".to_string(),
        Color::LightBlue => "#d6acff".to_string(),
        Color::LightMagenta => "#ff92df".to_string(),
        Color::LightCyan => "#a4ffff".to_string(),
        Color::White => "#ffffff".to_string(),
        Color::Rgb(r, g, b) => format!("#{r:02x}{g:02x}{b:02x}"),
        Color::Indexed(i) => {
            // Basic 16 colors approximation
            match i {
                0 => "#000000".to_string(),
                1 => "#ff5555".to_string(),
                2 => "#50fa7b".to_string(),
                3 => "#f1fa8c".to_string(),
                4 => "#bd93f9".to_string(),
                5 => "#ff79c6".to_string(),
                6 => "#8be9fd".to_string(),
                7 => "#cccccc".to_string(),
                8 => "#44475a".to_string(),
                9 => "#ff6e6e".to_string(),
                10 => "#69ff94".to_string(),
                11 => "#ffffa5".to_string(),
                12 => "#d6acff".to_string(),
                13 => "#ff92df".to_string(),
                14 => "#a4ffff".to_string(),
                15 => "#ffffff".to_string(),
                _ => "#cccccc".to_string(), // Fallback for 256 color mode
            }
        }
    }
}

/// Escape XML special characters
fn xml_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

// Note: Each test writes its own documentation file independently.
// No index generation or global aggregation needed - tests are fully parallel-safe.

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::backend::TestBackend;
    use ratatui::Terminal;

    #[test]
    fn test_buffer_to_svg() {
        let backend = TestBackend::new(20, 5);
        let mut terminal = Terminal::new(backend).unwrap();

        terminal
            .draw(|frame| {
                use ratatui::text::Text;
                use ratatui::widgets::Paragraph;

                let text = Text::raw("Hello, World!");
                let paragraph = Paragraph::new(text);
                frame.render_widget(paragraph, frame.area());
            })
            .unwrap();

        let buffer = terminal.backend().buffer();
        let temp_dir = tempfile::tempdir().unwrap();
        let svg_path = temp_dir.path().join("test.svg");

        render_buffer_to_svg(buffer, (0, 0), &svg_path).unwrap();
        assert!(svg_path.exists());

        let svg_content = fs::read_to_string(&svg_path).unwrap();
        // Each character is rendered in a separate <text> element, so check for individual chars
        assert!(svg_content.contains(">H<"));
        assert!(svg_content.contains(">e<"));
        assert!(svg_content.contains(">l<"));
        assert!(svg_content.contains(">o<"));
        assert!(svg_content.contains(">,<"));
        assert!(svg_content.contains(">W<"));
        assert!(svg_content.contains(">r<"));
        assert!(svg_content.contains(">d<"));
        assert!(svg_content.contains(">!</"));
        assert!(svg_content.contains("<svg"));
    }

    #[test]
    fn test_color_to_hex() {
        assert_eq!(color_to_hex(Color::Black), "#000000");
        assert_eq!(color_to_hex(Color::White), "#ffffff");
        assert_eq!(color_to_hex(Color::Rgb(255, 128, 64)), "#ff8040");
    }

    #[test]
    fn test_xml_escape() {
        assert_eq!(xml_escape("hello"), "hello");
        assert_eq!(xml_escape("<>&"), "&lt;&gt;&amp;");
        assert_eq!(xml_escape("\"test\""), "&quot;test&quot;");
    }
}
