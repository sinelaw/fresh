// Blog showcase animation capture utilities
//
// Captures multi-frame animations of the editor with key-press indicators
// and mouse cursor overlays. Used to generate animated GIFs for blog posts.

use ratatui::buffer::Buffer;
use ratatui::style::Color;
use serde::{Deserialize, Serialize};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// Frame metadata for GIF generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FrameInfo {
    pub index: usize,
    pub filename: String,
    pub duration_ms: u32,
    pub key_indicator: Option<String>,
    pub mouse_pos: Option<(u16, u16)>,
}

/// Showcase metadata written as JSON alongside frames
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShowcaseMetadata {
    pub name: String,
    pub title: String,
    pub description: String,
    pub width: u16,
    pub height: u16,
    pub frames: Vec<FrameInfo>,
}

/// Global speed multiplier for all showcase animations.
/// Values > 1.0 slow down, values < 1.0 speed up.
const SPEED_FACTOR: f32 = 2.0;

/// Builder for capturing animated blog showcases
pub struct BlogShowcase {
    name: String,
    title: String,
    description: String,
    frames_dir: PathBuf,
    blog_dir: PathBuf,
    frames: Vec<FrameInfo>,
    frame_index: usize,
    term_width: u16,
    term_height: u16,
    last_key: Option<String>,
}

impl BlogShowcase {
    /// Create a new blog showcase.
    ///
    /// `name` is used for directory naming (should be kebab-case, e.g. "multi-cursor").
    /// `title` is the human-readable title for the blog post.
    /// `description` is a short blurb about the feature.
    pub fn new(name: &str, title: &str, description: &str) -> Self {
        // Use CARGO_MANIFEST_DIR to resolve paths relative to workspace root
        // CARGO_MANIFEST_DIR points to crates/fresh-editor/, so go up two levels
        let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|p| p.parent())
            .expect("workspace root")
            .to_path_buf();
        let blog_dir = workspace_root.join("docs/blog").join(name);
        let frames_dir = blog_dir.join("frames");

        Self {
            name: name.to_string(),
            title: title.to_string(),
            description: description.to_string(),
            frames_dir,
            blog_dir,
            frames: Vec::new(),
            frame_index: 0,
            term_width: 0,
            term_height: 0,
            last_key: None,
        }
    }

    /// Capture a single animation frame.
    ///
    /// - `buffer`: the ratatui Buffer to render
    /// - `cursor_pos`: editor cursor position (col, row)
    /// - `key_indicator`: optional key combo label to show (e.g. "Ctrl+D", "Enter")
    /// - `mouse_pos`: optional mouse cursor position in terminal cells (col, row)
    /// - `duration_ms`: how long this frame should display in the GIF
    pub fn capture_frame(
        &mut self,
        buffer: &Buffer,
        cursor_pos: (u16, u16),
        key_indicator: Option<&str>,
        mouse_pos: Option<(u16, u16)>,
        duration_ms: u32,
    ) -> io::Result<()> {
        if key_indicator.is_some() {
            self.last_key = key_indicator.map(|s| s.to_string());
        }
        let effective_key = self.last_key.as_deref();

        self.term_width = buffer.area.width;
        self.term_height = buffer.area.height;

        fs::create_dir_all(&self.frames_dir)?;

        let filename = format!("frame_{:04}.svg", self.frame_index);
        let filepath = self.frames_dir.join(&filename);

        render_showcase_frame(buffer, cursor_pos, effective_key, mouse_pos, &filepath)?;

        self.frames.push(FrameInfo {
            index: self.frame_index,
            filename,
            duration_ms: (duration_ms as f32 * SPEED_FACTOR) as u32,
            key_indicator: effective_key.map(|s| s.to_string()),
            mouse_pos,
        });

        self.frame_index += 1;
        Ok(())
    }

    /// Capture multiple identical "hold" frames to create a pause effect.
    /// Useful for holding on a key indicator or final state.
    pub fn hold_frames(
        &mut self,
        buffer: &Buffer,
        cursor_pos: (u16, u16),
        key_indicator: Option<&str>,
        mouse_pos: Option<(u16, u16)>,
        count: usize,
        duration_ms_each: u32,
    ) -> io::Result<()> {
        for _ in 0..count {
            self.capture_frame(
                buffer,
                cursor_pos,
                key_indicator,
                mouse_pos,
                duration_ms_each,
            )?;
        }
        Ok(())
    }

    /// Finalize the showcase: write metadata JSON and blog post stub.
    /// Returns the path to the blog directory.
    pub fn finalize(self) -> io::Result<PathBuf> {
        if self.frames.is_empty() {
            return Ok(self.blog_dir);
        }

        // Write metadata JSON
        let metadata = ShowcaseMetadata {
            name: self.name.clone(),
            title: self.title.clone(),
            description: self.description.clone(),
            width: self.term_width,
            height: self.term_height,
            frames: self.frames.clone(),
        };
        let metadata_path = self.blog_dir.join("showcase.json");
        let json = serde_json::to_string_pretty(&metadata)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        fs::write(&metadata_path, json)?;

        // Write blog post markdown stub (only if it doesn't exist yet)
        let md_path = self.blog_dir.join("index.md");
        if !md_path.exists() {
            let md = format!(
                r#"---
title: "{title}"
outline: false
---

# {title}

{desc}

<div class="showcase-demo">
  <img src="./showcase.gif" alt="{title} demo" />
</div>

<!-- Generated by: cargo test --package fresh-editor --test e2e_tests blog_showcase_{name} -- --ignored -->
<!-- Then run: scripts/frames-to-gif.sh docs/blog/{name} -->
"#,
                title = self.title,
                desc = self.description,
                name = self.name,
            );
            fs::write(&md_path, md)?;
        }

        Ok(self.blog_dir)
    }
}

// ---------------------------------------------------------------------------
// SVG rendering with overlays
// ---------------------------------------------------------------------------

const CHAR_WIDTH: u16 = 9;
const CHAR_HEIGHT: u16 = 18;
const FONT_SIZE: u16 = 14;

/// Render a single animation frame to SVG with optional key indicator and mouse cursor.
fn render_showcase_frame(
    buffer: &Buffer,
    cursor_pos: (u16, u16),
    key_indicator: Option<&str>,
    mouse_pos: Option<(u16, u16)>,
    path: &Path,
) -> io::Result<()> {
    let width = buffer.area.width;
    let height = buffer.area.height;
    let svg_width = width * CHAR_WIDTH;
    let svg_height = height * CHAR_HEIGHT;

    let mut svg = format!(
        r##"<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="{svg_width}" height="{svg_height}" viewBox="0 0 {svg_width} {svg_height}">
<style>
    .terminal {{ font-family: 'DejaVu Sans Mono', 'Liberation Mono', 'JetBrains Mono', 'Courier New', 'Consolas', monospace; font-size: {FONT_SIZE}px; white-space: pre; }}
    .key-badge {{ font-family: 'DejaVu Sans', 'Liberation Sans', 'Arial', sans-serif; font-size: 20px; font-weight: 600; }}
</style>
<defs>
    <filter id="badge-shadow" x="-10%" y="-10%" width="120%" height="140%">
        <feDropShadow dx="1" dy="2" stdDeviation="2" flood-opacity="0.5"/>
    </filter>
</defs>
<rect width="100%" height="100%" fill="#1e1e2e"/>
"##
    );

    // Render terminal cells
    for y in 0..height {
        for x in 0..width {
            let cell = &buffer[(x, y)];
            let style = cell.style();
            let symbol = cell.symbol();

            // Background
            if let Some(bg) = style.bg {
                if !matches!(bg, Color::Reset) {
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

            // Text
            if !symbol.trim().is_empty() {
                let fg = style.fg.unwrap_or(Color::White);
                let fg_hex = color_to_hex(fg);
                let escaped = xml_escape(symbol);

                let mut extra_style = String::new();
                if style.add_modifier.contains(ratatui::style::Modifier::BOLD) {
                    extra_style.push_str("font-weight:bold;");
                }
                if style
                    .add_modifier
                    .contains(ratatui::style::Modifier::ITALIC)
                {
                    extra_style.push_str("font-style:italic;");
                }
                if style
                    .add_modifier
                    .contains(ratatui::style::Modifier::UNDERLINED)
                {
                    extra_style.push_str("text-decoration:underline;");
                }

                svg.push_str(&format!(
                    r#"  <text x="{}" y="{}" fill="{}" class="terminal" style="{}">{}</text>"#,
                    x * CHAR_WIDTH + 1,
                    y * CHAR_HEIGHT + FONT_SIZE,
                    fg_hex,
                    extra_style,
                    escaped
                ));
                svg.push('\n');
            }
        }
    }

    // Editor cursor
    let (cx, cy) = cursor_pos;
    svg.push_str(&format!(
        r##"  <rect x="{}" y="{}" width="{}" height="{}" fill="#cdd6f4" opacity="0.85"/>"##,
        cx * CHAR_WIDTH,
        cy * CHAR_HEIGHT,
        CHAR_WIDTH,
        CHAR_HEIGHT
    ));
    svg.push('\n');

    // Mouse cursor overlay
    if let Some((mx, my)) = mouse_pos {
        let px = mx * CHAR_WIDTH;
        let py = my * CHAR_HEIGHT;
        svg.push_str(&format!(
            r##"  <g transform="translate({px},{py})" opacity="0.95">
    <polygon points="0,0 0,18 5,14 9,21 12,19 8,12 14,12" fill="#ffffff" stroke="#000000" stroke-width="1.2" stroke-linejoin="round"/>
  </g>
"##
        ));
    }

    // Key indicator badge (bottom-right corner)
    if let Some(key) = key_indicator {
        // Escape for XML — key labels can contain `&`, `<`, etc.
        let badge_text = xml_escape(key);
        let text_len = key.len() as u16;
        let badge_w = text_len * 13 + 32;
        let badge_h: u16 = 36;
        let badge_x = svg_width - badge_w - 16;
        let badge_y = svg_height - badge_h - 14;
        let text_x = badge_x + badge_w / 2;
        let text_y = badge_y + 26;

        svg.push_str(&format!(
            r##"  <g filter="url(#badge-shadow)">
    <rect x="{badge_x}" y="{badge_y}" width="{badge_w}" height="{badge_h}" rx="6" ry="6" fill="#313244" stroke="#585b70" stroke-width="1.5"/>
    <text x="{text_x}" y="{text_y}" fill="#cdd6f4" class="key-badge" text-anchor="middle">{badge_text}</text>
  </g>
"##
        ));
    }

    svg.push_str("</svg>");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(path, svg)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers (Catppuccin Mocha palette)
// ---------------------------------------------------------------------------

fn color_to_hex(color: Color) -> String {
    match color {
        Color::Reset => "#cdd6f4".to_string(),
        Color::Black => "#1e1e2e".to_string(),
        Color::Red => "#f38ba8".to_string(),
        Color::Green => "#a6e3a1".to_string(),
        Color::Yellow => "#f9e2af".to_string(),
        Color::Blue => "#89b4fa".to_string(),
        Color::Magenta => "#f5c2e7".to_string(),
        Color::Cyan => "#94e2d5".to_string(),
        Color::Gray => "#6c7086".to_string(),
        Color::DarkGray => "#585b70".to_string(),
        Color::LightRed => "#f38ba8".to_string(),
        Color::LightGreen => "#a6e3a1".to_string(),
        Color::LightYellow => "#f9e2af".to_string(),
        Color::LightBlue => "#89b4fa".to_string(),
        Color::LightMagenta => "#f5c2e7".to_string(),
        Color::LightCyan => "#94e2d5".to_string(),
        Color::White => "#cdd6f4".to_string(),
        Color::Rgb(r, g, b) => format!("#{r:02x}{g:02x}{b:02x}"),
        Color::Indexed(i) => match i {
            0 => "#1e1e2e".to_string(),
            1 => "#f38ba8".to_string(),
            2 => "#a6e3a1".to_string(),
            3 => "#f9e2af".to_string(),
            4 => "#89b4fa".to_string(),
            5 => "#f5c2e7".to_string(),
            6 => "#94e2d5".to_string(),
            7 => "#bac2de".to_string(),
            8 => "#585b70".to_string(),
            9 => "#f38ba8".to_string(),
            10 => "#a6e3a1".to_string(),
            11 => "#f9e2af".to_string(),
            12 => "#89b4fa".to_string(),
            13 => "#f5c2e7".to_string(),
            14 => "#94e2d5".to_string(),
            15 => "#cdd6f4".to_string(),
            _ => "#bac2de".to_string(),
        },
    }
}

fn xml_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::backend::TestBackend;
    use ratatui::Terminal;

    #[test]
    fn test_showcase_frame_renders() {
        let backend = TestBackend::new(40, 10);
        let mut terminal = Terminal::new(backend).unwrap();

        terminal
            .draw(|frame| {
                use ratatui::text::Text;
                use ratatui::widgets::Paragraph;
                let text = Text::raw("Hello, Fresh!");
                let paragraph = Paragraph::new(text);
                frame.render_widget(paragraph, frame.area());
            })
            .unwrap();

        let buffer = terminal.backend().buffer();
        let temp_dir = tempfile::tempdir().unwrap();
        let svg_path = temp_dir.path().join("test_frame.svg");

        render_showcase_frame(buffer, (0, 0), Some("Ctrl+D"), Some((5, 3)), &svg_path).unwrap();
        assert!(svg_path.exists());

        let content = fs::read_to_string(&svg_path).unwrap();
        assert!(content.contains("Ctrl+D"));
        assert!(content.contains("polygon"));
        assert!(content.contains("<svg"));
    }

    #[test]
    fn test_showcase_finalize() {
        let temp_dir = tempfile::tempdir().unwrap();
        let test_name = "test-showcase";
        let blog_dir = temp_dir.path().join("docs/blog").join(test_name);

        let mut showcase = BlogShowcase::new(test_name, "Test Feature", "A test description.");
        // Override the blog_dir to use temp
        showcase.blog_dir = blog_dir.clone();
        showcase.frames_dir = blog_dir.join("frames");

        let backend = TestBackend::new(20, 5);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal
            .draw(|frame| {
                use ratatui::text::Text;
                use ratatui::widgets::Paragraph;
                frame.render_widget(Paragraph::new(Text::raw("hi")), frame.area());
            })
            .unwrap();

        let buffer = terminal.backend().buffer();
        showcase
            .capture_frame(buffer, (0, 0), None, None, 100)
            .unwrap();
        showcase
            .capture_frame(buffer, (1, 0), Some("a"), None, 80)
            .unwrap();

        let result = showcase.finalize().unwrap();
        assert!(result.join("showcase.json").exists());
        assert!(result.join("index.md").exists());
        assert!(result.join("frames/frame_0000.svg").exists());
        assert!(result.join("frames/frame_0001.svg").exists());
    }
}
