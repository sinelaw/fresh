//! Launches the real-editor web bridge for the assembled `web-ui/` page (no mocks).
//!
//!   cargo run -p fresh-editor --example webui_server -- [ADDR] [FILES...]
//!
//! e.g.  cargo run -p fresh-editor --example webui_server -- 127.0.0.1:8137 src/lib.rs
//! then open http://127.0.0.1:8137 — the browser frontend is driven by a real
//! `Editor`; keystrokes go through `Editor::handle_key`.

use std::path::PathBuf;

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let addr = args.next().unwrap_or_else(|| "127.0.0.1:8137".to_string());
    let files: Vec<PathBuf> = args.map(PathBuf::from).collect();
    fresh::webui::run(&addr, &files)
}
