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
use fresh::{
    app::script_control::ScriptControlMode, app::Editor, config, services::signal_handler,
};
use ratatui::Terminal;
use std::{
    io::{self, stdout},
    path::PathBuf,
    time::Duration,
};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

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

    /// Path to log file for editor diagnostics
    #[arg(long, value_name = "PATH", default_value = "/tmp/editor.log")]
    log_file: PathBuf,

    /// Enable event logging to the specified file
    #[arg(long, value_name = "LOG_FILE")]
    event_log: Option<PathBuf>,

    /// Enable script control mode (accepts JSON commands via stdin, outputs to stdout)
    #[arg(long)]
    script_mode: bool,

    /// Terminal width for script control mode (default: 80)
    #[arg(long, default_value = "80")]
    script_width: u16,

    /// Terminal height for script control mode (default: 24)
    #[arg(long, default_value = "24")]
    script_height: u16,

    /// Print script control mode command schema and exit
    #[arg(long)]
    script_schema: bool,

    /// Don't restore previous session (start fresh)
    #[arg(long)]
    no_session: bool,
}

fn main() -> io::Result<()> {
    // Parse command-line arguments
    let args = Args::parse();

    // Handle --script-schema flag
    if args.script_schema {
        println!("{}", fresh::app::script_control::get_command_schema());
        return Ok(());
    }

    // Handle script control mode
    if args.script_mode {
        // Initialize tracing for script mode - log to stderr so it doesn't interfere with JSON output on stdout
        tracing_subscriber::registry()
            .with(fmt::layer().with_writer(io::stderr))
            .with(EnvFilter::from_default_env().add_directive(tracing::Level::INFO.into()))
            .init();

        return run_script_control_mode(&args);
    }

    // Initialize tracing - log to a file to avoid interfering with terminal UI
    // Fall back to no logging if the log file can't be created
    if let Ok(log_file) = std::fs::File::create(&args.log_file) {
        tracing_subscriber::registry()
            .with(fmt::layer().with_writer(std::sync::Arc::new(log_file)))
            .with(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
            .init();
    }

    tracing::info!("Editor starting");

    // Install signal handlers for SIGTERM and SIGINT
    signal_handler::install_signal_handlers();
    tracing::info!("Signal handlers installed");

    // Set up panic hook to restore terminal
    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic| {
        let _ = stdout().execute(SetCursorStyle::DefaultUserShape);
        let _ = stdout().execute(PopKeyboardEnhancementFlags);
        let _ = disable_raw_mode();
        let _ = stdout().execute(LeaveAlternateScreen);
        original_hook(panic);
    }));

    // Load configuration
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
        config::Config::default()
    };

    // Set up terminal first
    enable_raw_mode()?;
    stdout().execute(EnterAlternateScreen)?;

    // Enable keyboard enhancement flags to support Shift+Up/Down and other modifier combinations
    // This uses the Kitty keyboard protocol for better key detection in supported terminals
    let keyboard_flags = KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
        | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS;
    let _ = stdout().execute(PushKeyboardEnhancementFlags(keyboard_flags));
    tracing::info!("Enabled keyboard enhancement flags: {:?}", keyboard_flags);

    // Enable mouse support
    let _ = crossterm::execute!(stdout(), crossterm::event::EnableMouseCapture);
    tracing::info!("Enabled mouse capture");

    // Enable blinking block cursor for the primary cursor in active split
    let _ = stdout().execute(SetCursorStyle::BlinkingBlock);
    tracing::info!("Enabled blinking block cursor");

    let backend = ratatui::backend::CrosstermBackend::new(stdout());
    let mut terminal = Terminal::new(backend)?;

    // Clear the terminal to ensure proper initialization
    terminal.clear()?;

    let size = terminal.size()?;
    tracing::info!("Terminal size: {}x{}", size.width, size.height);

    // Determine if the provided path is a directory or file
    let (working_dir, file_to_open, show_file_explorer) = if let Some(path) = &args.file {
        if path.is_dir() {
            // Path is a directory: use as working dir, don't open any file, show file explorer
            (Some(path.clone()), None, true)
        } else {
            // Path is a file: use current dir as working dir, open the file, don't auto-show explorer
            (None, Some(path.clone()), false)
        }
    } else {
        // No path provided: use current dir, no file, don't auto-show explorer
        (None, None, false)
    };

    // Create editor with actual terminal size and working directory
    let mut editor = if args.no_plugins {
        Editor::with_plugins_disabled(config, size.width, size.height, working_dir)?
    } else {
        Editor::with_working_dir(config, size.width, size.height, working_dir)?
    };

    // Enable event log streaming if requested
    if let Some(log_path) = &args.event_log {
        tracing::trace!("Event logging enabled: {}", log_path.display());
        editor.enable_event_streaming(log_path)?;
    }

    // Try to restore previous session (unless --no-session flag is set or a file was specified)
    let session_enabled = !args.no_session && file_to_open.is_none();
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

    // Open file if provided (this takes precedence over session)
    if let Some(path) = &file_to_open {
        editor.open_file(path)?;
    }

    // Show file explorer if directory was provided
    if show_file_explorer {
        editor.show_file_explorer();
    }

    // Check for recovery files from a crash and recover them
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

    // Start recovery session
    if let Err(e) = editor.start_recovery_session() {
        tracing::warn!("Failed to start recovery session: {}", e);
    }

    // Run the editor
    let result = run_event_loop(&mut editor, &mut terminal, session_enabled);

    // End recovery session (clean shutdown)
    if let Err(e) = editor.end_recovery_session() {
        tracing::warn!("Failed to end recovery session: {}", e);
    }

    // Clean up terminal
    let _ = crossterm::execute!(stdout(), crossterm::event::DisableMouseCapture);
    let _ = stdout().execute(SetCursorStyle::DefaultUserShape);
    let _ = stdout().execute(PopKeyboardEnhancementFlags);
    disable_raw_mode()?;
    stdout().execute(LeaveAlternateScreen)?;

    result
}

/// Run the editor in script control mode
fn run_script_control_mode(args: &Args) -> io::Result<()> {
    // Create script control mode instance
    let mut control = if let Some(path) = &args.file {
        if path.is_dir() {
            ScriptControlMode::with_working_dir(
                args.script_width,
                args.script_height,
                path.clone(),
            )?
        } else {
            let mut ctrl = ScriptControlMode::new(args.script_width, args.script_height)?;
            // Open the file if provided
            ctrl.open_file(path)?;
            ctrl
        }
    } else {
        ScriptControlMode::new(args.script_width, args.script_height)?
    };

    control.run()
}

/// Main event loop
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    session_enabled: bool,
) -> io::Result<()> {
    use std::time::Instant;

    const FRAME_DURATION: Duration = Duration::from_millis(16); // 60fps
    let mut last_render = Instant::now();
    let mut needs_render = true;
    let mut pending_event: Option<CrosstermEvent> = None; // For events read during coalescing

    loop {
        if editor.process_async_messages() {
            needs_render = true;
        }

        // Periodic auto-save for recovery
        if let Err(e) = editor.auto_save_dirty_buffers() {
            tracing::debug!("Auto-save error: {}", e);
        }

        if editor.should_quit() {
            // Save session before quitting (if enabled)
            if session_enabled {
                if let Err(e) = editor.save_session() {
                    tracing::warn!("Failed to save session: {}", e);
                } else {
                    tracing::debug!("Session saved successfully");
                }
            }
            break;
        }

        // Render at most 60fps
        if needs_render && last_render.elapsed() >= FRAME_DURATION {
            terminal.draw(|frame| editor.render(frame))?;
            last_render = Instant::now();
            needs_render = false;
        }

        // Get next event
        let event = if let Some(e) = pending_event.take() {
            Some(e)
        } else {
            let timeout = if pending_event.is_some() || needs_render {
                FRAME_DURATION.saturating_sub(last_render.elapsed())
            } else {
                Duration::from_millis(50)
            };
            if event_poll(timeout)? {
                Some(event_read()?)
            } else {
                None
            }
        };

        let Some(event) = event else { continue };

        // Coalesce mouse moves - skip stale ones, keep clicks/keys
        let (event, next) = coalesce_mouse_moves(event)?;
        pending_event = next;

        match event {
            CrosstermEvent::Key(key_event) => {
                // Only process key press events to avoid duplicate events on Windows
                // (Windows sends both Press and Release events, while Linux/macOS only send Press)
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

/// Handle a keyboard event
fn handle_key_event(editor: &mut Editor, key_event: KeyEvent) -> io::Result<()> {
    // Debug trace the full key event
    tracing::debug!(
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
