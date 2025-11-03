mod buffer;
mod chunk_tree;
mod config;
mod cursor;
mod editor;
mod event;
mod keybindings;
mod state;
mod viewport;

use clap::Parser;
use crossterm::{
    event::{poll as event_poll, read as event_read, Event as CrosstermEvent, KeyEvent},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use editor::Editor;
use ratatui::Terminal;
use std::{
    io::{self, stdout},
    path::PathBuf,
    time::Duration,
};

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

    // Set up panic hook to restore terminal
    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic| {
        let _ = disable_raw_mode();
        let _ = stdout().execute(LeaveAlternateScreen);
        original_hook(panic);
    }));

    // Load configuration
    let config = config::Config::default();

    // Set up terminal first to get the size
    enable_raw_mode()?;
    stdout().execute(EnterAlternateScreen)?;
    let backend = ratatui::backend::CrosstermBackend::new(stdout());
    let mut terminal = Terminal::new(backend)?;
    let size = terminal.size()?;

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

