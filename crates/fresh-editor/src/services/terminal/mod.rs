//! Terminal emulation service for Fresh
//!
//! This module provides built-in terminal support using:
//! - `alacritty_terminal` for terminal emulation (VT100/ANSI parsing, grid management)
//! - `portable-pty` for cross-platform PTY management
//!
//! # Incremental Streaming Architecture
//!
//! The terminal uses an incremental streaming design that avoids O(n) work on mode
//! switches and session restore. The key insight is that scrollback history is append-only.
//!
//! ## Data Flow
//!
//! 1. **PTY Read Loop** (manager.rs): As PTY output arrives, `process_output()` updates
//!    the terminal grid, then `flush_new_scrollback()` appends any new scrollback lines
//!    to the backing file. Scrollback is written one line at a time as lines scroll off.
//!
//! 2. **Terminal → Scrollback** (terminal.rs: `sync_terminal_to_buffer`): Appends visible
//!    screen (~50 lines) to backing file, then loads it as read-only buffer.
//!    Performance: O(screen_size) ≈ 5ms.
//!
//! 3. **Scrollback → Terminal** (terminal.rs: `enter_terminal_mode`): Truncates backing
//!    file to `backing_file_history_end` (removes visible screen tail), resumes live
//!    rendering. Performance: O(1) ≈ 1ms.
//!
//! 4. **Session Save** (session.rs): `sync_all_terminal_backing_files()` appends visible
//!    screen to all terminal backing files before saving session metadata.
//!
//! 5. **Session Restore** (session.rs): `load_terminal_backing_file_as_buffer()` loads
//!    backing file directly (skips log replay). User starts in scrollback mode.
//!    Performance: O(1) ≈ 10ms (lazy load).
//!
//! ## Backing File Structure
//!
//! Located at `~/.local/share/fresh/terminals/{workdir}/fresh-terminal-{id}.txt`:
//!
//! - **Scrollback history** (top): Append-only, grows as lines scroll off screen
//! - **Visible screen** (bottom): Rewritable tail (~50 lines), present only in scrollback mode
//!
//! The `backing_file_history_end` offset marks where scrollback ends, used for truncation
//! when re-entering terminal mode.
//!
//! ## Module Responsibilities
//!
//! - `term.rs`: Terminal state and incremental streaming methods
//! - `manager.rs`: PTY lifecycle and read loop with streaming
//! - `../app/terminal.rs`: Mode switching logic
//! - `../app/session.rs`: Session save/restore integration

mod manager;
pub mod pty;
pub mod term;

pub use manager::{detect_shell, TerminalId, TerminalManager};
pub use term::{TerminalCell, TerminalState};
