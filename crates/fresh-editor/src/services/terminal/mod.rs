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
//! 3. **Scrollback → Terminal** (terminal.rs: `enter_terminal_mode`): Cuts the visible
//!    screen tail back out of the backing file (`cut_backing_file_tail`), resumes live
//!    rendering. Performance: O(scrollback streamed during the visit).
//!
//! 4. **Session Save** (workspace.rs: `sync_terminal_backing_files`): appends visible
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
//! - **Visible screen** (bottom): Rewritable tail (~50 lines), written on scroll-back
//!   entry and by session checkpoints
//!
//! The `backing_file_history_end` offset marks where scrollback ends, which is where
//! the tail starts.
//!
//! ## Who owns the tail
//!
//! Three actors write this file — the PTY read loop (on its own thread) and two
//! UI-thread paths, scroll-back entry and checkpoints, both through
//! `TerminalState::write_backing_file_tail` — and a fourth, leaving scroll-back,
//! cuts the tail back out. They stay consistent through `TerminalState`, whose lock
//! every one of them holds across its file work, and through two facts on it:
//! `backing_file_history_end` (the byte length of the scrollback prefix) and where
//! the tail ends, while there is one.
//!
//! Every handle onto these files is opened `O_APPEND` (`open_transcript_file`),
//! so no writer can land on top of another's bytes and the file's length is
//! always the read loop's write position.
//!
//! The tail is *temporary*, but only the UI thread can remove it: the read loop
//! must not change bytes a scroll-back view may be reading. So scrollback the read
//! loop streams while a tail is there goes in after it, and whoever next deals with
//! the tail cuts it out from between (`TerminalState::cut_backing_file_tail`): the
//! lines after it are read, the file is cut back to the history end, and they are
//! appended again. The file thus holds each line once — the scrollback, then the
//! screen — and never keeps a screen that later scrolled into history as well
//! (fresh#3151, #3645).
//!
//! Those rewrites change only bytes past the history end. Before one, the UI
//! thread loads those bytes of the terminal's scroll-back view into memory
//! (`Window::pin_terminal_view_tail`), so a view of a large file, which reads
//! it lazily, keeps showing what it showed — in every split showing it.
//!
//! ## Module Responsibilities
//!
//! - `term.rs`: Terminal state and incremental streaming methods
//! - `manager.rs`: PTY lifecycle and read loop with streaming
//! - `../app/terminal.rs`: Mode switching logic
//! - `../app/session.rs`: Session save/restore integration

pub mod manager;
pub mod path_link;
pub mod pty;
pub mod term;
#[cfg(windows)]
pub mod windows_shell;

pub use manager::{detect_shell, BackingMode, TerminalId, TerminalIdAllocator, TerminalManager};
pub use term::{BackingFile, BackingFileAt, PrependedHead, TerminalCell, TerminalState};
#[cfg(windows)]
pub use windows_shell::set_skip_app_execution_alias;
