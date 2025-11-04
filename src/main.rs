mod buffer;
mod cache;
mod chunk_tree;
mod config;
mod cursor;
mod edit;
mod editor;
mod event;
mod keybindings;
mod line_cache;
mod persistence;
mod state;
mod viewport;
mod virtual_buffer;

use clap::Parser;
use crossterm::{
    event::{poll as event_poll, read as event_read, Event as CrosstermEvent, KeyEvent},
    terminal::{disable_raw_mode, enable_raw_mode, size as terminal_size, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use editor::Editor;
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
    let log_file = std::fs::File::create("/tmp/editor.log")
        .expect("Failed to create log file");

    tracing_subscriber::registry()
        .with(fmt::layer().with_writer(std::sync::Arc::new(log_file)))
        .with(EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()))
        .init();

    tracing::info!("Editor starting");

    // Set up panic hook to restore terminal
    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic| {
        let _ = disable_raw_mode();
        let _ = stdout().execute(LeaveAlternateScreen);
        original_hook(panic);
    }));

    // Load configuration
    let config = config::Config::default();

    // Set up terminal first
    enable_raw_mode()?;
    stdout().execute(EnterAlternateScreen)?;

    let backend = ratatui::backend::CrosstermBackend::new(stdout());
    let mut terminal = Terminal::new(backend)?;

    // Clear the terminal to ensure proper initialization
    terminal.clear()?;

    let size = terminal.size()?;
    tracing::info!("Terminal size: {}x{}", size.width, size.height);

    // Create editor with actual terminal size
    let mut editor = Editor::new(config, size.width, size.height)?;

    // Enable event log streaming if requested
    if let Some(log_path) = &args.event_log {
        eprintln!("Event logging enabled: {}", log_path.display());
        editor.enable_event_streaming(log_path)?;
    }

    // Open file if provided
    if let Some(path) = &args.file {
        editor.open_file(path)?;
    }

    // Run the editor
    let result = run_event_loop(&mut editor, &mut terminal);

    // Clean up terminal
    disable_raw_mode()?;
    stdout().execute(LeaveAlternateScreen)?;

    result
}

/// Main event loop
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
) -> io::Result<()> {
    loop {
        // Render the editor
        terminal.draw(|frame| editor.render(frame))?;

        // Check if we should quit
        if editor.should_quit() {
            break;
        }

        // Poll for events with timeout
        if event_poll(Duration::from_millis(100))? {
            match event_read()? {
                CrosstermEvent::Key(key_event) => {
                    handle_key_event(editor, key_event)?;
                }
                CrosstermEvent::Resize(width, height) => {
                    tracing::info!("Terminal resize event: {}x{}", width, height);
                    editor.resize(width, height);
                }
                _ => {
                    // Ignore other events (mouse, etc.)
                }
            }
        }
    }

    Ok(())
}

/// Handle a keyboard event
fn handle_key_event(editor: &mut Editor, key_event: KeyEvent) -> io::Result<()> {
    // Log the keystroke
    let key_code = format!("{:?}", key_event.code);
    let modifiers = format!("{:?}", key_event.modifiers);
    editor.log_keystroke(&key_code, &modifiers);

    // Delegate to the editor's handle_key method
    editor.handle_key(key_event.code, key_event.modifiers)?;

    Ok(())
}

