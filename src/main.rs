use clap::Parser;
use crossterm::{
    cursor::SetCursorStyle,
    event::{
        poll as event_poll, read as event_read, Event as CrosstermEvent, KeyEvent, KeyEventKind,
        KeyboardEnhancementFlags, MouseEvent, PopKeyboardEnhancementFlags,
        PushKeyboardEnhancementFlags,
    },
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
#[cfg(target_os = "linux")]
use fresh::services::gpm::{gpm_to_crossterm, GpmClient};
use fresh::services::tracing_setup;
use fresh::{
    app::Editor, config, config_io::DirectoryContext, services::release_checker,
    services::signal_handler, services::warning_log::WarningLogHandle,
};
use ratatui::Terminal;
use std::{
    io::{self, stdout},
    path::PathBuf,
    time::Duration,
};

/// A high-performance terminal text editor
#[derive(Parser, Debug)]
#[command(name = "fresh")]
#[command(about = "A terminal text editor with multi-cursor support", long_about = None)]
#[command(version)]
struct Args {
    /// File to open
    #[arg(value_name = "FILE")]
    file: Option<PathBuf>,

    /// Disable plugin loading
    #[arg(long)]
    no_plugins: bool,

    /// Path to configuration file
    #[arg(long, value_name = "PATH")]
    config: Option<PathBuf>,

    /// Path to log file for editor diagnostics (default: system temp dir)
    #[arg(long, value_name = "PATH")]
    log_file: Option<PathBuf>,

    /// Enable event logging to the specified file
    #[arg(long, value_name = "LOG_FILE")]
    event_log: Option<PathBuf>,

    /// Don't restore previous session (start fresh)
    #[arg(long)]
    no_session: bool,
}

/// Parsed file location from CLI argument in file:line:col format
#[derive(Debug)]
struct FileLocation {
    path: PathBuf,
    line: Option<usize>,
    column: Option<usize>,
}

struct IterationOutcome {
    loop_result: io::Result<()>,
    update_result: Option<release_checker::ReleaseCheckResult>,
    restart_dir: Option<PathBuf>,
}

struct SetupState {
    config: config::Config,
    warning_log_handle: Option<WarningLogHandle>,
    terminal: Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    terminal_size: (u16, u16),
    file_location: Option<FileLocation>,
    file_to_open: Option<PathBuf>,
    show_file_explorer: bool,
    dir_context: DirectoryContext,
    current_working_dir: Option<PathBuf>,
    #[cfg(target_os = "linux")]
    gpm_client: Option<GpmClient>,
    #[cfg(not(target_os = "linux"))]
    gpm_client: Option<()>,
}

fn handle_first_run_setup(
    editor: &mut Editor,
    args: &Args,
    file_to_open: &Option<PathBuf>,
    file_location: &Option<FileLocation>,
    show_file_explorer: bool,
    warning_log_handle: &mut Option<WarningLogHandle>,
    session_enabled: bool,
) -> io::Result<()> {
    if let Some(log_path) = &args.event_log {
        tracing::trace!("Event logging enabled: {}", log_path.display());
        editor.enable_event_streaming(log_path)?;
    }

    if let Some(handle) = warning_log_handle.take() {
        editor.set_warning_log(handle.receiver, handle.path);
    }

    if session_enabled {
        match editor.try_restore_session() {
            Ok(true) => {
                tracing::info!("Session restored successfully");
            }
            Ok(false) => {
                tracing::debug!("No previous session found");
            }
            Err(e) => {
                tracing::warn!("Failed to restore session: {}", e);
            }
        }
    }

    if let Some(path) = &file_to_open {
        editor.open_file(path)?;

        if let Some(ref loc) = file_location {
            if let Some(line) = loc.line {
                editor.goto_line_col(line, loc.column);
            }
        }
    }

    if show_file_explorer {
        editor.show_file_explorer();
    }

    if editor.has_recovery_files().unwrap_or(false) {
        tracing::info!("Recovery files found from previous session, recovering...");
        match editor.recover_all_buffers() {
            Ok(count) if count > 0 => {
                tracing::info!("Recovered {} buffer(s)", count);
            }
            Ok(_) => {
                tracing::info!("No buffers to recover");
            }
            Err(e) => {
                tracing::warn!("Failed to recover buffers: {}", e);
            }
        }
    }

    Ok(())
}

/// Parse a file path that may include line and column information.
/// Supports formats:
/// - file.txt
/// - file.txt:10
/// - file.txt:10:5
/// - /path/to/file.txt:10:5
///
/// For Windows paths like C:\path\file.txt:10:5, we handle the drive letter
/// prefix properly using std::path APIs.
///
/// If the full path exists as a file, it's used as-is (handles files with colons in name).
fn parse_file_location(input: &str) -> FileLocation {
    use std::path::{Component, Path};

    let full_path = PathBuf::from(input);

    // If the full path exists as a file, use it directly
    // This handles edge cases like files named "foo:10"
    if full_path.is_file() {
        return FileLocation {
            path: full_path,
            line: None,
            column: None,
        };
    }

    // Check if the path has a Windows drive prefix using std::path
    let has_prefix = Path::new(input)
        .components()
        .next()
        .map(|c| matches!(c, Component::Prefix(_)))
        .unwrap_or(false);

    // Calculate where to start looking for :line:col
    // For Windows paths with prefix (e.g., "C:"), skip past the drive letter and colon
    let search_start = if has_prefix {
        // Find the first colon (the drive letter separator) and skip it
        input.find(':').map(|i| i + 1).unwrap_or(0)
    } else {
        0
    };

    // Find the last colon(s) that could be line:col
    let suffix = &input[search_start..];

    // Try to parse from the end: look for :col and :line patterns
    // We work backwards to find numeric suffixes
    let parts: Vec<&str> = suffix.rsplitn(3, ':').collect();

    match parts.as_slice() {
        // Could be "col", "line", "rest" or just parts of the path
        [maybe_col, maybe_line, rest] => {
            if let (Ok(line), Ok(col)) = (maybe_line.parse::<usize>(), maybe_col.parse::<usize>()) {
                // Both parsed as numbers: file:line:col
                let path_str = if has_prefix {
                    format!("{}{}", &input[..search_start], rest)
                } else {
                    rest.to_string()
                };
                return FileLocation {
                    path: PathBuf::from(path_str),
                    line: Some(line),
                    column: Some(col),
                };
            }
            // Fall through - not valid line:col format
        }
        // Could be "line", "rest" or just parts of the path
        [maybe_line, rest] => {
            if let Ok(line) = maybe_line.parse::<usize>() {
                // Parsed as number: file:line
                let path_str = if has_prefix {
                    format!("{}{}", &input[..search_start], rest)
                } else {
                    rest.to_string()
                };
                return FileLocation {
                    path: PathBuf::from(path_str),
                    line: Some(line),
                    column: None,
                };
            }
            // Fall through - not valid line format
        }
        _ => {}
    }

    // No valid line:col suffix found, treat the whole thing as a path
    FileLocation {
        path: full_path,
        line: None,
        column: None,
    }
}

fn initialize_app(args: &Args) -> io::Result<SetupState> {
    let log_file = args
        .log_file
        .clone()
        .unwrap_or_else(|| std::env::temp_dir().join("fresh.log"));
    let warning_log_handle = tracing_setup::init_global(&log_file);

    tracing::info!("Editor starting");

    signal_handler::install_signal_handlers();
    tracing::info!("Signal handlers installed");

    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic| {
        let _ = crossterm::execute!(stdout(), crossterm::event::DisableMouseCapture);
        let _ = stdout().execute(SetCursorStyle::DefaultUserShape);
        let _ = stdout().execute(PopKeyboardEnhancementFlags);
        let _ = disable_raw_mode();
        let _ = stdout().execute(LeaveAlternateScreen);
        original_hook(panic);
    }));

    // Determine working directory early for config loading
    let file_location = args
        .file
        .as_ref()
        .map(|p| parse_file_location(p.to_string_lossy().as_ref()));

    let (working_dir, file_to_open, show_file_explorer) = if let Some(ref loc) = file_location {
        if loc.path.is_dir() {
            (Some(loc.path.clone()), None, true)
        } else {
            (None, Some(loc.path.clone()), false)
        }
    } else {
        (None, None, false)
    };

    // Load config - checking working directory first, then system paths
    let effective_working_dir = working_dir
        .as_ref()
        .cloned()
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_default());

    let config = if let Some(config_path) = &args.config {
        match config::Config::load_from_file(config_path) {
            Ok(cfg) => cfg,
            Err(e) => {
                eprintln!(
                    "Error: Failed to load config from {}: {}",
                    config_path.display(),
                    e
                );
                return Err(io::Error::new(io::ErrorKind::InvalidData, e.to_string()));
            }
        }
    } else {
        config::Config::load_for_working_dir(&effective_working_dir)
    };

    enable_raw_mode()?;
    stdout().execute(EnterAlternateScreen)?;

    let keyboard_flags = KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
        | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS;
    let _ = stdout().execute(PushKeyboardEnhancementFlags(keyboard_flags));
    tracing::info!("Enabled keyboard enhancement flags: {:?}", keyboard_flags);

    #[cfg(target_os = "linux")]
    let gpm_client = match GpmClient::connect() {
        Ok(client) => client,
        Err(e) => {
            tracing::warn!("Failed to connect to GPM: {}", e);
            None
        }
    };
    #[cfg(not(target_os = "linux"))]
    let gpm_client: Option<()> = None;

    if gpm_client.is_none() {
        let _ = crossterm::execute!(stdout(), crossterm::event::EnableMouseCapture);
        tracing::info!("Enabled crossterm mouse capture");
    } else {
        tracing::info!("Using GPM for mouse capture, skipping crossterm mouse protocol");
    }

    let _ = stdout().execute(SetCursorStyle::BlinkingBlock);
    tracing::info!("Enabled blinking block cursor");

    let backend = ratatui::backend::CrosstermBackend::new(stdout());
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let size = terminal.size()?;
    tracing::info!("Terminal size: {}x{}", size.width, size.height);

    let dir_context = DirectoryContext::from_system()?;
    let current_working_dir = working_dir;

    Ok(SetupState {
        config,
        warning_log_handle,
        terminal,
        terminal_size: (size.width, size.height),
        file_location,
        file_to_open,
        show_file_explorer,
        dir_context,
        current_working_dir,
        gpm_client,
    })
}

#[cfg_attr(not(target_os = "linux"), allow(unused_variables))]
fn run_editor_iteration(
    editor: &mut Editor,
    session_enabled: bool,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    #[cfg(target_os = "linux")] gpm_client: &Option<GpmClient>,
) -> io::Result<IterationOutcome> {
    #[cfg(target_os = "linux")]
    let loop_result = run_event_loop(editor, terminal, session_enabled, gpm_client);
    #[cfg(not(target_os = "linux"))]
    let loop_result = run_event_loop(editor, terminal, session_enabled);

    if let Err(e) = editor.end_recovery_session() {
        tracing::warn!("Failed to end recovery session: {}", e);
    }

    let update_result = editor.get_update_result().cloned();
    let restart_dir = editor.take_restart_dir();

    Ok(IterationOutcome {
        loop_result,
        update_result,
        restart_dir,
    })
}

fn main() -> io::Result<()> {
    // Parse command-line arguments
    let args = Args::parse();

    let SetupState {
        config,
        mut warning_log_handle,
        mut terminal,
        terminal_size,
        file_location,
        file_to_open,
        show_file_explorer,
        dir_context,
        current_working_dir: initial_working_dir,
        #[cfg(target_os = "linux")]
        gpm_client,
        #[cfg(not(target_os = "linux"))]
        gpm_client,
    } = initialize_app(&args)?;

    let mut current_working_dir = initial_working_dir;
    let (terminal_width, terminal_height) = terminal_size;

    // Track whether this is the first run (for session restore, file open, etc.)
    let mut is_first_run = true;

    // Track whether we should restore session on restart (for project switching)
    let mut restore_session_on_restart = false;

    // Main editor loop - supports restarting with a new working directory
    // Returns (loop_result, last_update_result) tuple
    let (result, last_update_result) = loop {
        let first_run = is_first_run;
        let session_enabled = !args.no_session && file_to_open.is_none();

        let mut editor = Editor::with_working_dir(
            config.clone(),
            terminal_width,
            terminal_height,
            current_working_dir.clone(),
            dir_context.clone(),
            !args.no_plugins,
        )?;

        #[cfg(target_os = "linux")]
        if gpm_client.is_some() {
            editor.set_gpm_active(true);
        }

        if first_run {
            handle_first_run_setup(
                &mut editor,
                &args,
                &file_to_open,
                &file_location,
                show_file_explorer,
                &mut warning_log_handle,
                session_enabled,
            )?;
        } else {
            if restore_session_on_restart {
                match editor.try_restore_session() {
                    Ok(true) => {
                        tracing::info!("Session restored successfully");
                    }
                    Ok(false) => {
                        tracing::debug!("No previous session found");
                    }
                    Err(e) => {
                        tracing::warn!("Failed to restore session: {}", e);
                    }
                }
            }

            editor.show_file_explorer();
            editor.set_status_message(format!(
                "Switched to project: {}",
                current_working_dir
                    .as_ref()
                    .map(|p| p.display().to_string())
                    .unwrap_or_else(|| ".".to_string())
            ));
        }

        if let Err(e) = editor.start_recovery_session() {
            tracing::warn!("Failed to start recovery session: {}", e);
        }

        let iteration = run_editor_iteration(
            &mut editor,
            session_enabled,
            &mut terminal,
            #[cfg(target_os = "linux")]
            &gpm_client,
        )?;

        let update_result = iteration.update_result;
        let restart_dir = iteration.restart_dir;
        let loop_result = iteration.loop_result;

        drop(editor);

        if let Some(new_dir) = restart_dir {
            tracing::info!(
                "Restarting editor with new working directory: {}",
                new_dir.display()
            );
            current_working_dir = Some(new_dir);
            is_first_run = false;
            restore_session_on_restart = true; // Restore session for the new project
            terminal.clear()?;
            continue;
        }

        break (loop_result, update_result);
    };

    // Clean up terminal
    let _ = crossterm::execute!(stdout(), crossterm::event::DisableMouseCapture);
    let _ = stdout().execute(SetCursorStyle::DefaultUserShape);
    let _ = stdout().execute(PopKeyboardEnhancementFlags);
    disable_raw_mode()?;
    stdout().execute(LeaveAlternateScreen)?;

    // Check for updates after terminal is restored (using cached result)
    if let Some(update_result) = last_update_result {
        if update_result.update_available {
            eprintln!();
            eprintln!(
                "A new version of fresh is available: {} -> {}",
                release_checker::CURRENT_VERSION,
                update_result.latest_version
            );
            if let Some(cmd) = update_result.install_method.update_command() {
                eprintln!("Update with: {}", cmd);
            } else {
                eprintln!(
                    "Download from: https://github.com/sinelaw/fresh/releases/tag/v{}",
                    update_result.latest_version
                );
            }
            eprintln!();
        }
    }

    result
}

/// Main event loop
#[cfg(target_os = "linux")]
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    session_enabled: bool,
    gpm_client: &Option<GpmClient>,
) -> io::Result<()> {
    run_event_loop_common(editor, terminal, session_enabled, |timeout| {
        poll_with_gpm(gpm_client.as_ref(), timeout)
    })
}

/// Main event loop (non-Linux version without GPM)
#[cfg(not(target_os = "linux"))]
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    session_enabled: bool,
) -> io::Result<()> {
    run_event_loop_common(editor, terminal, session_enabled, |timeout| {
        if event_poll(timeout)? {
            Ok(Some(event_read()?))
        } else {
            Ok(None)
        }
    })
}

fn run_event_loop_common<F>(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    session_enabled: bool,
    mut poll_event: F,
) -> io::Result<()>
where
    F: FnMut(Duration) -> io::Result<Option<CrosstermEvent>>,
{
    use std::time::Instant;

    const FRAME_DURATION: Duration = Duration::from_millis(16); // 60fps
    let mut last_render = Instant::now();
    let mut needs_render = true;
    let mut pending_event: Option<CrosstermEvent> = None;

    loop {
        // Process async messages and poll for file changes (auto-revert, file tree)
        if editor.process_async_messages() {
            needs_render = true;
        }

        // Check mouse hover timer for LSP hover requests
        if editor.check_mouse_hover_timer() {
            needs_render = true;
        }

        // Check for warnings and open warning log if any occurred
        if editor.check_warning_log() {
            needs_render = true;
        }

        if let Err(e) = editor.auto_save_dirty_buffers() {
            tracing::debug!("Auto-save error: {}", e);
        }

        if editor.should_quit() {
            if session_enabled {
                if let Err(e) = editor.save_session() {
                    tracing::warn!("Failed to save session: {}", e);
                } else {
                    tracing::debug!("Session saved successfully");
                }
            }
            break;
        }

        if needs_render && last_render.elapsed() >= FRAME_DURATION {
            terminal.draw(|frame| editor.render(frame))?;
            last_render = Instant::now();
            needs_render = false;
        }

        let event = if let Some(e) = pending_event.take() {
            Some(e)
        } else {
            let timeout = if needs_render {
                FRAME_DURATION.saturating_sub(last_render.elapsed())
            } else {
                Duration::from_millis(50)
            };

            poll_event(timeout)?
        };

        let Some(event) = event else { continue };

        let (event, next) = coalesce_mouse_moves(event)?;
        pending_event = next;

        match event {
            CrosstermEvent::Key(key_event) => {
                if key_event.kind == KeyEventKind::Press {
                    handle_key_event(editor, key_event)?;
                    needs_render = true;
                }
            }
            CrosstermEvent::Mouse(mouse_event) => {
                if handle_mouse_event(editor, mouse_event)? {
                    needs_render = true;
                }
            }
            CrosstermEvent::Resize(w, h) => {
                editor.resize(w, h);
                needs_render = true;
            }
            _ => {}
        }
    }

    Ok(())
}

/// Poll for events from both GPM and crossterm (Linux with libgpm available)
#[cfg(target_os = "linux")]
fn poll_with_gpm(
    gpm_client: Option<&GpmClient>,
    timeout: Duration,
) -> io::Result<Option<CrosstermEvent>> {
    use nix::poll::{poll, PollFd, PollFlags, PollTimeout};
    use std::os::unix::io::{AsRawFd, BorrowedFd};

    // If no GPM client, just use crossterm polling
    let Some(gpm) = gpm_client else {
        return if event_poll(timeout)? {
            Ok(Some(event_read()?))
        } else {
            Ok(None)
        };
    };

    // Set up poll for both stdin (crossterm) and GPM fd
    let stdin_fd = std::io::stdin().as_raw_fd();
    let gpm_fd = gpm.fd();
    tracing::trace!("GPM poll: stdin_fd={}, gpm_fd={}", stdin_fd, gpm_fd);

    // SAFETY: We're borrowing the fds for the duration of the poll call
    let stdin_borrowed = unsafe { BorrowedFd::borrow_raw(stdin_fd) };
    let gpm_borrowed = unsafe { BorrowedFd::borrow_raw(gpm_fd) };

    let mut poll_fds = [
        PollFd::new(stdin_borrowed, PollFlags::POLLIN),
        PollFd::new(gpm_borrowed, PollFlags::POLLIN),
    ];

    // Convert timeout to milliseconds, clamping to u16::MAX (about 65 seconds)
    let timeout_ms = timeout.as_millis().min(u16::MAX as u128) as u16;
    let poll_timeout = PollTimeout::from(timeout_ms);
    let ready = poll(&mut poll_fds, poll_timeout)?;

    if ready == 0 {
        return Ok(None);
    }

    let stdin_revents = poll_fds[0].revents();
    let gpm_revents = poll_fds[1].revents();
    tracing::trace!(
        "GPM poll: ready={}, stdin_revents={:?}, gpm_revents={:?}",
        ready,
        stdin_revents,
        gpm_revents
    );

    // Check GPM first (mouse events are typically less frequent)
    if gpm_revents.is_some_and(|r| r.contains(PollFlags::POLLIN)) {
        tracing::trace!("GPM poll: GPM fd has data, reading event...");
        match gpm.read_event() {
            Ok(Some(gpm_event)) => {
                tracing::trace!(
                    "GPM event received: x={}, y={}, buttons={}, type=0x{:x}",
                    gpm_event.x,
                    gpm_event.y,
                    gpm_event.buttons.0,
                    gpm_event.event_type
                );
                if let Some(mouse_event) = gpm_to_crossterm(&gpm_event) {
                    tracing::trace!("GPM event converted to crossterm: {:?}", mouse_event);
                    return Ok(Some(CrosstermEvent::Mouse(mouse_event)));
                } else {
                    tracing::debug!("GPM event could not be converted to crossterm event");
                }
            }
            Ok(None) => {
                tracing::trace!("GPM poll: read_event returned None");
            }
            Err(e) => {
                tracing::warn!("GPM poll: read_event error: {}", e);
            }
        }
    }

    // Check stdin (crossterm events)
    if stdin_revents.is_some_and(|r| r.contains(PollFlags::POLLIN)) {
        // Use crossterm's read since it handles escape sequence parsing
        if event_poll(Duration::ZERO)? {
            return Ok(Some(event_read()?));
        }
    }

    Ok(None)
}

/// Handle a keyboard event
fn handle_key_event(editor: &mut Editor, key_event: KeyEvent) -> io::Result<()> {
    // Trace the full key event
    tracing::trace!(
        "Key event received: code={:?}, modifiers={:?}, kind={:?}, state={:?}",
        key_event.code,
        key_event.modifiers,
        key_event.kind,
        key_event.state
    );

    // Log the keystroke
    let key_code = format!("{:?}", key_event.code);
    let modifiers = format!("{:?}", key_event.modifiers);
    editor.log_keystroke(&key_code, &modifiers);

    // Delegate to the editor's handle_key method
    editor.handle_key(key_event.code, key_event.modifiers)?;

    Ok(())
}

/// Handle a mouse event
/// Returns true if a re-render is needed
fn handle_mouse_event(editor: &mut Editor, mouse_event: MouseEvent) -> io::Result<bool> {
    tracing::debug!(
        "Mouse event received: kind={:?}, column={}, row={}, modifiers={:?}",
        mouse_event.kind,
        mouse_event.column,
        mouse_event.row,
        mouse_event.modifiers
    );

    // Delegate to the editor's handle_mouse method
    editor.handle_mouse(mouse_event)
}

/// Skip stale mouse move events, return the latest one.
/// If we read a non-move event while draining, return it as pending.
fn coalesce_mouse_moves(
    event: CrosstermEvent,
) -> io::Result<(CrosstermEvent, Option<CrosstermEvent>)> {
    use crossterm::event::MouseEventKind;

    // Only coalesce mouse moves
    if !matches!(&event, CrosstermEvent::Mouse(m) if m.kind == MouseEventKind::Moved) {
        return Ok((event, None));
    }

    let mut latest = event;
    while event_poll(Duration::ZERO)? {
        let next = event_read()?;
        if matches!(&next, CrosstermEvent::Mouse(m) if m.kind == MouseEventKind::Moved) {
            latest = next; // Newer move, skip the old one
        } else {
            return Ok((latest, Some(next))); // Hit a click/key, save it
        }
    }
    Ok((latest, None))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_file_location_simple_path() {
        let loc = parse_file_location("foo.txt");
        assert_eq!(loc.path, PathBuf::from("foo.txt"));
        assert_eq!(loc.line, None);
        assert_eq!(loc.column, None);
    }

    #[test]
    fn test_parse_file_location_with_line() {
        let loc = parse_file_location("foo.txt:42");
        assert_eq!(loc.path, PathBuf::from("foo.txt"));
        assert_eq!(loc.line, Some(42));
        assert_eq!(loc.column, None);
    }

    #[test]
    fn test_parse_file_location_with_line_and_col() {
        let loc = parse_file_location("foo.txt:42:10");
        assert_eq!(loc.path, PathBuf::from("foo.txt"));
        assert_eq!(loc.line, Some(42));
        assert_eq!(loc.column, Some(10));
    }

    #[test]
    fn test_parse_file_location_absolute_path() {
        let loc = parse_file_location("/home/user/foo.txt:100:5");
        assert_eq!(loc.path, PathBuf::from("/home/user/foo.txt"));
        assert_eq!(loc.line, Some(100));
        assert_eq!(loc.column, Some(5));
    }

    #[test]
    fn test_parse_file_location_no_numbers_after_colon() {
        // If the suffix isn't a number, treat the whole thing as a path
        let loc = parse_file_location("foo:bar");
        assert_eq!(loc.path, PathBuf::from("foo:bar"));
        assert_eq!(loc.line, None);
        assert_eq!(loc.column, None);
    }

    #[test]
    fn test_parse_file_location_mixed_suffix() {
        // If only one part is a number, depends on position
        // "foo:10:bar" -> "bar" isn't a number, so no line:col parsing
        let loc = parse_file_location("foo:10:bar");
        assert_eq!(loc.path, PathBuf::from("foo:10:bar"));
        assert_eq!(loc.line, None);
        assert_eq!(loc.column, None);
    }

    #[test]
    fn test_parse_file_location_line_only_not_col() {
        // "foo:bar:10" -> "10" is col, "bar" isn't line, so no parsing
        let loc = parse_file_location("foo:bar:10");
        assert_eq!(loc.path, PathBuf::from("foo:bar:10"));
        assert_eq!(loc.line, None);
        assert_eq!(loc.column, None);
    }
}

// Property tests use Unix-style path generation strategy, skip on Windows
// where path parsing differs (drive letters like C: conflict with :line:col parsing)
#[cfg(all(test, not(windows)))]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    /// Generate a valid Unix-style file path (no colons in path components)
    fn unix_path_strategy() -> impl Strategy<Value = String> {
        prop::collection::vec("[a-zA-Z0-9._-]+", 1..5).prop_map(|components| components.join("/"))
    }

    proptest! {
        /// Property: If we construct "path:line:col", we should get back the path, line, and col
        #[test]
        fn roundtrip_line_col(
            path in unix_path_strategy(),
            line in 1usize..10000,
            col in 1usize..1000
        ) {
            let input = format!("{}:{}:{}", path, line, col);
            let loc = parse_file_location(&input);

            prop_assert_eq!(loc.path, PathBuf::from(&path));
            prop_assert_eq!(loc.line, Some(line));
            prop_assert_eq!(loc.column, Some(col));
        }

        /// Property: If we construct "path:line", we should get back the path and line
        #[test]
        fn roundtrip_line_only(
            path in unix_path_strategy(),
            line in 1usize..10000
        ) {
            let input = format!("{}:{}", path, line);
            let loc = parse_file_location(&input);

            prop_assert_eq!(loc.path, PathBuf::from(&path));
            prop_assert_eq!(loc.line, Some(line));
            prop_assert_eq!(loc.column, None);
        }

        /// Property: A path without any colon-number suffix returns the full path
        #[test]
        fn path_without_numbers_unchanged(
            path in unix_path_strategy()
        ) {
            let loc = parse_file_location(&path);

            prop_assert_eq!(loc.path, PathBuf::from(&path));
            prop_assert_eq!(loc.line, None);
            prop_assert_eq!(loc.column, None);
        }

        /// Property: line and column should always be non-zero when present
        /// (we parse as usize so 0 is valid, but the function doesn't filter)
        #[test]
        fn parsed_values_match_input(
            path in unix_path_strategy(),
            line in 0usize..10000,
            col in 0usize..1000
        ) {
            let input = format!("{}:{}:{}", path, line, col);
            let loc = parse_file_location(&input);

            prop_assert_eq!(loc.line, Some(line));
            prop_assert_eq!(loc.column, Some(col));
        }
    }
}
