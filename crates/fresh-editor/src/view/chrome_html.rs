//! Web frontend renderer (PoC): render a [`ChromeSnapshot`] to a self-contained
//! HTML document — **chrome as DOM/CSS, text body as SVG `<text>`** — the markup
//! a Tauri/browser frontend would produce (see the "Browser / Tauri frontend"
//! design in `docs/internal/NON_TERMINAL_UI_RESEARCH.md`).
//!
//! Architecture this realizes (Direction A, Tauri-primary):
//!   - the **native Rust backend** owns the core (piece tree, LSP, plugins,
//!     real files, >4GB) and produces a [`ChromeSnapshot`] (already `Serialize`)
//!     plus the visible, styled text lines (a "line cache" — only what's on
//!     screen, per the xi-editor lesson);
//!   - the **webview** renders the *chrome* with DOM/CSS (semantic split tree →
//!     nested CSS-grid panes; native tabs/menus/popups, accessible for free via
//!     real DOM/ARIA) and the *text body* with **SVG `<text>`/`<tspan>`** runs
//!     for precise, zoomable, syntax-colored glyphs.
//!
//! This Rust function is the *reference* renderer / SSR seed: in production the
//! identical markup is produced by the webview's TS from the serialized
//! snapshot, but emitting it here lets us unit-test the structure and generate
//! an openable demo without a browser. DOM/CSS does the chrome layout (so we do
//! **not** compute pixel rects here — contrast [`super::chrome_layout`], which
//! is for the GPU/canvas path); only the SVG text lines are positioned.

use std::fmt::Write as _;

use super::chrome_snapshot::ChromeSnapshot;
use crate::view::split::SplitNode;

/// A styled run within a text line (one syntax color).
#[derive(Debug, Clone)]
pub struct WebRun {
    pub text: String,
    /// CSS color, e.g. `"#c586c0"`.
    pub color: String,
}

/// One visible text line: a sequence of styled runs.
#[derive(Debug, Clone, Default)]
pub struct WebLine {
    pub runs: Vec<WebRun>,
}

/// Rendering options (font metrics + theme) the frontend supplies.
#[derive(Debug, Clone, Copy)]
pub struct WebOptions {
    pub font_px: f32,
    pub line_height_px: f32,
    /// Advance width of one monospace cell, in px (for SVG glyph x-positions).
    pub char_w_px: f32,
    pub dark: bool,
}

impl Default for WebOptions {
    fn default() -> Self {
        Self {
            font_px: 13.0,
            line_height_px: 18.0,
            char_w_px: 7.8,
            dark: true,
        }
    }
}

/// HTML-escape text content / attribute values.
fn esc(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(c),
        }
    }
    out
}

/// Render a full standalone HTML document for `snapshot`, showing `lines` in
/// every leaf pane's content area (a single shared sample for the PoC; a real
/// frontend feeds each pane its own visible line window).
pub fn render_document(snapshot: &ChromeSnapshot, lines: &[WebLine], opts: WebOptions) -> String {
    let mut s = String::with_capacity(8 * 1024);
    s.push_str("<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n");
    s.push_str("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n");
    s.push_str("<title>Fresh — web chrome (PoC)</title>\n<style>\n");
    s.push_str(&css(opts));
    s.push_str("</style>\n</head>\n");
    let theme = if opts.dark { "dark" } else { "light" };
    let _ = write!(s, "<body class=\"fresh {theme}\">\n");

    // --- Menu bar (DOM/CSS, native-feeling) ---
    s.push_str("  <nav class=\"menubar\" role=\"menubar\">\n");
    for (i, label) in snapshot.menubar.iter().enumerate() {
        let _ = write!(
            s,
            "    <button class=\"menu\" role=\"menuitem\" data-menu=\"{i}\">{}</button>\n",
            esc(label)
        );
    }
    s.push_str("  </nav>\n");

    // --- Workspace: the split tree as nested CSS-grid panes ---
    s.push_str("  <main class=\"workspace\">\n");
    render_node(&mut s, &snapshot.split_layout, snapshot.active_buffer, lines, opts);
    s.push_str("  </main>\n");

    // --- Status bar (DOM/CSS) ---
    s.push_str("  <footer class=\"statusbar\" role=\"status\">");
    let _ = write!(
        s,
        "<span class=\"st-left\">buffer#{}</span><span class=\"st-right\">{} pane(s) · {} divider(s)</span>",
        snapshot.active_buffer,
        count_leaves(&snapshot.split_layout),
        snapshot.divider_count
    );
    s.push_str("</footer>\n");

    // --- Popups / overlays (native <dialog>-style DOM) ---
    for (i, kind) in snapshot.overlays.iter().enumerate() {
        let _ = write!(
            s,
            "  <div class=\"scrim\" data-popup=\"{i}\"><div class=\"popup\" role=\"dialog\" aria-label=\"{k}\"><header>{k}</header><div class=\"popup-body\">Native popup rendered from the chrome snapshot (DOM/CSS).</div></div></div>\n",
            k = esc(kind)
        );
    }

    s.push_str("</body>\n</html>\n");
    s
}

/// Recurse the split tree into nested CSS-grid divs (chrome layout is done by
/// the browser, not by us — we just emit the semantic structure).
fn render_node(
    s: &mut String,
    node: &SplitNode,
    active_buffer: usize,
    lines: &[WebLine],
    opts: WebOptions,
) {
    match node {
        SplitNode::Leaf { buffer_id, .. } => {
            render_pane(s, Some(buffer_id.0), active_buffer, lines, opts);
        }
        SplitNode::Grouped { layout, .. } => {
            render_node(s, layout, active_buffer, lines, opts);
        }
        SplitNode::Split {
            direction,
            first,
            second,
            ratio,
            ..
        } => {
            use fresh_core::SplitDirection::*;
            let a = (*ratio * 100.0).round() as i32;
            let b = 100 - a;
            let (cls, style) = match direction {
                Vertical => (
                    "split vertical",
                    format!("grid-template-columns:{a}fr 5px {b}fr"),
                ),
                Horizontal => (
                    "split horizontal",
                    format!("grid-template-rows:{a}fr 5px {b}fr"),
                ),
            };
            let _ = write!(s, "<div class=\"{cls}\" style=\"{style}\">");
            render_node(s, first, active_buffer, lines, opts);
            let dcls = match direction {
                Vertical => "divider vertical",
                Horizontal => "divider horizontal",
            };
            let _ = write!(s, "<div class=\"{dcls}\" role=\"separator\"></div>");
            render_node(s, second, active_buffer, lines, opts);
            s.push_str("</div>");
        }
    }
}

/// One leaf pane: a DOM tab bar + an SVG text body.
fn render_pane(
    s: &mut String,
    buffer_id: Option<usize>,
    active_buffer: usize,
    lines: &[WebLine],
    opts: WebOptions,
) {
    let active = buffer_id == Some(active_buffer);
    let label = match buffer_id {
        Some(b) => format!("buffer#{b}"),
        None => "(group)".to_string(),
    };
    let active_cls = if active { " active" } else { "" };
    s.push_str("<section class=\"pane\">");
    // Tab bar (DOM/CSS) — native tabs with a close affordance.
    let _ = write!(
        s,
        "<div class=\"tabbar\" role=\"tablist\"><div class=\"tab{active_cls}\" role=\"tab\" aria-selected=\"{active}\" data-buffer=\"{bid}\"><span class=\"tab-label\">{}</span><span class=\"tab-close\" data-close=\"{bid}\" aria-label=\"Close\">×</span></div></div>",
        esc(&label),
        bid = buffer_id.map(|b| b.to_string()).unwrap_or_default(),
    );
    // Content: SVG text body.
    s.push_str("<div class=\"content\">");
    render_svg_text(s, lines, opts);
    s.push_str("</div></section>");
}

/// Render the visible lines as an SVG `<text>` per line, one `<tspan>` per
/// styled run (syntax color). x positions are monospace cell advances.
fn render_svg_text(s: &mut String, lines: &[WebLine], opts: WebOptions) {
    s.push_str("<svg class=\"textbody\" xmlns=\"http://www.w3.org/2000/svg\" preserveAspectRatio=\"xMinYMin meet\">");
    let lh = opts.line_height_px;
    for (i, line) in lines.iter().enumerate() {
        let y = (i as f32 + 1.0) * lh - lh * 0.25; // baseline within the line box
        let _ = write!(s, "<text x=\"6\" y=\"{y:.1}\" xml:space=\"preserve\">");
        let mut col = 0usize;
        for run in &line.runs {
            let x = 6.0 + col as f32 * opts.char_w_px;
            let _ = write!(
                s,
                "<tspan x=\"{x:.1}\" fill=\"{}\">{}</tspan>",
                esc(&run.color),
                esc(&run.text)
            );
            col += run.text.chars().count();
        }
        s.push_str("</text>");
    }
    s.push_str("</svg>");
}

fn count_leaves(node: &SplitNode) -> usize {
    match node {
        SplitNode::Leaf { .. } => 1,
        SplitNode::Grouped { layout, .. } => count_leaves(layout),
        SplitNode::Split { first, second, .. } => count_leaves(first) + count_leaves(second),
    }
}

/// The stylesheet — modern dark/light chrome, monospace SVG text.
fn css(opts: WebOptions) -> String {
    format!(
        r#"  :root {{
    --bg:#1e1e1e; --bg2:#252526; --bg3:#2d2d30; --fg:#d4d4d4; --muted:#9aa0a6;
    --accent:#0a84ff; --border:#3a3a3a; --tab-active:#1e1e1e;
  }}
  .fresh.light {{
    --bg:#ffffff; --bg2:#f3f3f3; --bg3:#ececec; --fg:#1f1f1f; --muted:#6b7280;
    --accent:#0a84ff; --border:#dcdcdc; --tab-active:#ffffff;
  }}
  * {{ box-sizing:border-box; }}
  html,body {{ height:100%; margin:0; }}
  body.fresh {{
    display:flex; flex-direction:column; height:100vh;
    background:var(--bg); color:var(--fg);
    font:13px/1.4 -apple-system,Segoe UI,Roboto,system-ui,sans-serif;
    -webkit-font-smoothing:antialiased;
  }}
  .menubar {{ display:flex; gap:2px; background:var(--bg3); border-bottom:1px solid var(--border);
    padding:2px 4px; flex:0 0 auto; }}
  .menu {{ background:none; border:0; color:var(--fg); padding:4px 10px; border-radius:5px;
    font:inherit; cursor:default; }}
  .menu:hover {{ background:var(--accent); color:#fff; }}
  .workspace {{ flex:1 1 auto; display:flex; min-height:0; }}
  .workspace > * {{ flex:1 1 auto; }}
  .split {{ display:grid; min-height:0; min-width:0; width:100%; height:100%; }}
  .divider {{ background:var(--border); }}
  .divider.vertical {{ cursor:col-resize; }}
  .divider.horizontal {{ cursor:row-resize; }}
  .pane {{ display:flex; flex-direction:column; min-height:0; min-width:0;
    border:1px solid var(--border); background:var(--bg); }}
  .tabbar {{ display:flex; background:var(--bg2); border-bottom:1px solid var(--border);
    flex:0 0 auto; overflow:hidden; }}
  .tab {{ display:flex; align-items:center; gap:8px; padding:6px 10px; color:var(--muted);
    border-right:1px solid var(--border); cursor:default; max-width:220px; }}
  .tab.active {{ background:var(--tab-active); color:var(--fg);
    box-shadow:inset 0 2px 0 var(--accent); }}
  .tab-label {{ overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }}
  .tab-close {{ opacity:.55; border-radius:4px; padding:0 4px; }}
  .tab-close:hover {{ opacity:1; background:rgba(127,127,127,.3); }}
  .content {{ flex:1 1 auto; min-height:0; overflow:auto; }}
  svg.textbody {{ display:block; width:100%; height:100%;
    font-family:ui-monospace,SFMono-Regular,JetBrains Mono,Menlo,Consolas,monospace;
    font-size:{font}px; }}
  .statusbar {{ display:flex; justify-content:space-between; background:var(--accent);
    color:#fff; padding:2px 10px; font-size:12px; flex:0 0 auto; }}
  .scrim {{ position:fixed; inset:0; background:rgba(0,0,0,.35); display:flex;
    align-items:flex-start; justify-content:center; padding-top:12vh; }}
  .popup {{ background:var(--bg2); color:var(--fg); border:1px solid var(--border);
    border-radius:10px; min-width:480px; box-shadow:0 18px 60px rgba(0,0,0,.5); overflow:hidden; }}
  .popup header {{ padding:10px 14px; background:var(--bg3); border-bottom:1px solid var(--border);
    font-weight:600; }}
  .popup-body {{ padding:14px; color:var(--muted); }}
"#,
        font = opts.font_px
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn leaf(b: usize, id: usize) -> SplitNode {
        SplitNode::leaf(fresh_core::BufferId(b), fresh_core::SplitId(id))
    }

    fn sample_snapshot(layout: SplitNode, overlays: Vec<String>, active: usize) -> ChromeSnapshot {
        ChromeSnapshot {
            menubar: vec!["File".into(), "Edit".into(), "View".into()],
            split_layout: layout,
            tabs: vec![],
            overlays,
            divider_count: 0,
            active_buffer: active,
        }
    }

    fn sample_lines() -> Vec<WebLine> {
        vec![
            WebLine {
                runs: vec![
                    WebRun { text: "fn ".into(), color: "#569cd6".into() },
                    WebRun { text: "main".into(), color: "#dcdcaa".into() },
                    WebRun { text: "() {".into(), color: "#d4d4d4".into() },
                ],
            },
            WebLine {
                runs: vec![WebRun {
                    text: "  // <hello> & \"world\"".into(),
                    color: "#6a9955".into(),
                }],
            },
        ]
    }

    #[test]
    fn renders_full_chrome_dom_and_svg_text() {
        let s = sample_snapshot(leaf(1, 0), vec![], 1);
        let html = render_document(&s, &sample_lines(), WebOptions::default());

        // Chrome is DOM/CSS.
        assert!(html.contains("<nav class=\"menubar\""));
        assert!(html.contains(">File</button>") && html.contains(">View</button>"));
        assert!(html.contains("<section class=\"pane\">"));
        assert!(html.contains("class=\"tab active\""));
        assert!(html.contains("class=\"tab-close\""));
        assert!(html.contains("<footer class=\"statusbar\""));
        // Text body is SVG <text>/<tspan>.
        assert!(html.contains("<svg class=\"textbody\""));
        assert!(html.contains("<text x=\"6\"") && html.contains("<tspan"));
        assert!(html.contains("fill=\"#dcdcaa\"")); // syntax color survived
        assert!(html.contains("<!doctype html>"));
    }

    #[test]
    fn split_tree_becomes_nested_css_grid_with_dividers() {
        let tree = SplitNode::split(
            fresh_core::SplitDirection::Vertical,
            leaf(1, 1),
            SplitNode::split(
                fresh_core::SplitDirection::Horizontal,
                leaf(2, 2),
                leaf(3, 3),
                0.6,
                fresh_core::SplitId(4),
            ),
            0.5,
            fresh_core::SplitId(0),
        );
        let s = sample_snapshot(tree, vec![], 2);
        let html = render_document(&s, &sample_lines(), WebOptions::default());

        assert!(html.contains("class=\"split vertical\""));
        assert!(html.contains("grid-template-columns:50fr 5px 50fr"));
        assert!(html.contains("class=\"split horizontal\""));
        assert!(html.contains("grid-template-rows:60fr 5px 40fr"));
        assert!(html.contains("class=\"divider vertical\""));
        assert!(html.contains("class=\"divider horizontal\""));
        // 3 leaves → 3 panes.
        assert_eq!(html.matches("<section class=\"pane\">").count(), 3);
    }

    #[test]
    fn overlays_render_as_native_popups() {
        let s = sample_snapshot(leaf(1, 0), vec!["Popup".into(), "Menu".into()], 1);
        let html = render_document(&s, &sample_lines(), WebOptions::default());
        assert_eq!(html.matches("class=\"popup\"").count(), 2);
        assert!(html.contains("role=\"dialog\""));
    }

    #[test]
    fn html_is_escaped() {
        let s = sample_snapshot(leaf(1, 0), vec![], 1);
        let html = render_document(&s, &sample_lines(), WebOptions::default());
        // The comment line contains < > & " — must be escaped inside the SVG.
        assert!(html.contains("&lt;hello&gt; &amp; &quot;world&quot;"));
        assert!(!html.contains("<hello>"));
    }
}
