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
//!    rendering. Performance: O(1) ≈ 1ms. The truncation is conditional on
//!    `TerminalState::backing_file_has_tail` — see "Who owns the tail" below.
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
//! - **Visible screen** (bottom): Rewritable tail (~50 lines), written on scroll-back
//!   entry and by session checkpoints
//!
//! The `backing_file_history_end` offset marks where scrollback ends, used for truncation
//! when re-entering terminal mode.
//!
//! ## Who owns the tail
//!
//! Three actors write this file — the PTY read loop (on its own thread) and two
//! UI-thread paths — and a fourth truncates it. They stay consistent through
//! `TerminalState`, whose lock every one of them holds across its file work, and
//! through two facts on it: `backing_file_history_end` (the byte length of the
//! scrollback prefix) and `backing_file_has_tail`.
//!
//! Every handle onto these files is opened `O_APPEND` (`open_transcript_file`),
//! so no writer can land on top of another's bytes and the file's length is
//! always the read loop's write position.
//!
//! The tail is *temporary*, but only the UI thread can remove it: the read loop
//! must not truncate a file a scroll-back view may be reading from. So when the
//! read loop streams scrollback while a tail is present, it cannot keep the tail
//! removable — the new lines sit past it — and instead **adopts** the tail as
//! scrollback (advancing `backing_file_history_end` past it and clearing the
//! flag). That leaves one duplicated screen in the transcript per overlap, which
//! nothing later removes; the alternative is truncating those lines away for
//! good, since `flush_new_scrollback` has already counted them as persisted and
//! never re-emits them (fresh#3151).
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

pub use manager::{detect_shell, BackingMode, TerminalId, TerminalManager};
pub use term::{PrependedHead, TerminalCell, TerminalState};
#[cfg(windows)]
pub use windows_shell::set_skip_app_execution_alias;
