use clap::Parser;
use crossterm::{
    event::{
        poll as event_poll, read as event_read, Event as CrosstermEvent, KeyEvent,
        KeyboardEnhancementFlags, MouseEvent, PopKeyboardEnhancementFlags,
        PushKeyboardEnhancementFlags,
    },
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use fresh::{config, editor::Editor, signal_handler};
use ratatui::Terminal;
use std::{
    io::{self, stdout},
    path::PathBuf,
    time::Duration,
};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// A high-performance terminal text editor
#[derive(Parser, Debug)]
#[command(name = "editor")]
#[command(about = "A terminal text editor with multi-cursor support", long_about = None)]
struct Args {
    /// File to open
    #[arg(value_name = "FILE")]
    file: Option<PathBuf>,

    /// Enable event logging to the specified file
    #[arg(long, value_name = "LOG_FILE")]
    event_log: Option<PathBuf>,
}

fn main() -> io::Result<()> {
    // Parse command-line arguments
    let args = Args::parse();

    // Initialize tracing - log to a file to avoid interfering with terminal UI
    let log_file = std::fs::File::create("/tmp/editor.log").expect("Failed to create log file");

    tracing_subscriber::registry()
        .with(fmt::layer().with_writer(std::sync::Arc::new(log_file)))
        .with(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .init();

    tracing::info!("Editor starting");

    // Install signal handlers for SIGTERM and SIGINT
    signal_handler::install_signal_handlers();
    tracing::info!("Signal handlers installed");

    // Set up panic hook to restore terminal
    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic| {
        let _ = stdout().execute(PopKeyboardEnhancementFlags);
        let _ = disable_raw_mode();
        let _ = stdout().execute(LeaveAlternateScreen);
        original_hook(panic);
    }));

    // Load configuration
    let config = config::Config::default();

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
    let mut editor = Editor::with_working_dir(config, size.width, size.height, working_dir)?;

    // Enable event log streaming if requested
    if let Some(log_path) = &args.event_log {
        tracing::trace!("Event logging enabled: {}", log_path.display());
        editor.enable_event_streaming(log_path)?;
    }

    // Open file if provided
    if let Some(path) = &file_to_open {
        editor.open_file(path)?;
    }

    // Show file explorer if directory was provided
    if show_file_explorer {
        editor.show_file_explorer();
    }

    // Run the editor
    let result = run_event_loop(&mut editor, &mut terminal);

    // Clean up terminal
    let _ = crossterm::execute!(stdout(), crossterm::event::DisableMouseCapture);
    let _ = stdout().execute(PopKeyboardEnhancementFlags);
    disable_raw_mode()?;
    stdout().execute(LeaveAlternateScreen)?;

    result
}

/// Main event loop
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
) -> io::Result<()> {
    use std::time::Instant;

    // Frame rate limiting: target 60fps (16.67ms per frame)
    const FRAME_DURATION: Duration = Duration::from_millis(16);
    let mut last_render = Instant::now();
    let mut needs_render = true;

    loop {
        // Process async messages from tokio tasks (LSP, file watching, etc.)
        let async_needs_render = editor.process_async_messages();
        if async_needs_render {
            needs_render = true;
        }

        // Check if we should quit
        if editor.should_quit() {
            break;
        }

        // Render only if enough time has passed since last render (60fps cap)
        let now = Instant::now();
        let time_since_render = now.duration_since(last_render);
        if needs_render && time_since_render >= FRAME_DURATION {
            terminal.draw(|frame| editor.render(frame))?;
            last_render = now;
            needs_render = false;
        }

        // Calculate poll timeout based on whether we need to render
        let poll_timeout = if needs_render {
            // If we need to render, use remaining time in frame budget
            let time_since_last_render = Instant::now().duration_since(last_render);
            FRAME_DURATION.saturating_sub(time_since_last_render)
        } else {
            // If idle, wait up to 1 second (or until an event arrives)
            // This prevents busy-polling when there's nothing to do
            Duration::from_secs(1)
        };

        // Poll for events
        if event_poll(poll_timeout)? {
            match event_read()? {
                CrosstermEvent::Key(key_event) => {
                    handle_key_event(editor, key_event)?;
                    needs_render = true; // Schedule render for next frame
                }
                CrosstermEvent::Mouse(mouse_event) => {
                    handle_mouse_event(editor, mouse_event)?;
                    needs_render = true; // Schedule render for next frame
                }
                CrosstermEvent::Resize(width, height) => {
                    tracing::info!("Terminal resize event: {}x{}", width, height);
                    editor.resize(width, height);
                    needs_render = true; // Schedule render for next frame
                }
                _ => {
                    // Ignore other events
                }
            }
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
fn handle_mouse_event(editor: &mut Editor, mouse_event: MouseEvent) -> io::Result<()> {
    tracing::debug!(
        "Mouse event received: kind={:?}, column={}, row={}, modifiers={:?}",
        mouse_event.kind,
        mouse_event.column,
        mouse_event.row,
        mouse_event.modifiers
    );

    // Delegate to the editor's handle_mouse method
    editor.handle_mouse(mouse_event)?;

    Ok(())
}
