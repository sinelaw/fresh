//! Generates a standalone HTML demo of the web/Tauri chrome (DOM/CSS) + SVG
//! text body from a sample `ChromeSnapshot`. Open the output in any browser.
//!
//!   cargo run -p fresh-editor --example chrome_web_demo > docs/internal/chrome-web-demo.html
//!
//! See `docs/internal/NON_TERMINAL_UI_RESEARCH.md` (Browser / Tauri frontend).

use fresh::view::chrome_html::{render_document, WebLine, WebOptions, WebRun};
use fresh::view::chrome_snapshot::ChromeSnapshot;
use fresh::view::split::SplitNode;
use fresh_core::{BufferId, SplitDirection, SplitId};

fn run(text: &str, color: &str) -> WebRun {
    WebRun {
        text: text.to_string(),
        color: color.to_string(),
    }
}

fn main() {
    // A representative layout: left editor | right column split top/bottom.
    let layout = SplitNode::split(
        SplitDirection::Vertical,
        SplitNode::leaf(BufferId(1), SplitId(1)),
        SplitNode::split(
            SplitDirection::Horizontal,
            SplitNode::leaf(BufferId(2), SplitId(2)),
            SplitNode::leaf(BufferId(3), SplitId(3)),
            0.6,
            SplitId(4),
        ),
        0.55,
        SplitId(0),
    );

    let snapshot = ChromeSnapshot {
        menubar: vec![
            "File".into(),
            "Edit".into(),
            "Selection".into(),
            "View".into(),
            "Go".into(),
            "Help".into(),
        ],
        split_layout: layout,
        tabs: vec![],
        overlays: vec!["Command Palette".into()],
        divider_count: 2,
        active_buffer: 1,
    };

    // A few syntax-colored lines (VS Code Dark+ palette).
    let kw = "#569cd6";
    let func = "#dcdcaa";
    let string = "#ce9178";
    let comment = "#6a9955";
    let fg = "#d4d4d4";
    let lines = vec![
        WebLine {
            runs: vec![
                run("// chrome rendered as DOM/CSS, text as SVG <text>", comment),
            ],
        },
        WebLine { runs: vec![] },
        WebLine {
            runs: vec![
                run("pub fn ", kw),
                run("chrome_snapshot", func),
                run("(&self) -> ", fg),
                run("ChromeSnapshot", "#4ec9b0"),
                run(" {", fg),
            ],
        },
        WebLine {
            runs: vec![
                run("    let ", kw),
                run("root = self.split_manager().root();", fg),
            ],
        },
        WebLine {
            runs: vec![
                run("    let ", kw),
                run("label = ", fg),
                run("\"buffer\"", string),
                run(";", fg),
            ],
        },
        WebLine {
            runs: vec![run("}", fg)],
        },
    ];

    print!("{}", render_document(&snapshot, &lines, WebOptions::default()));
}
