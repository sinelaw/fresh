mod buffer;
mod chunk_tree;
mod config;
mod cursor;
mod editor;
mod event;
mod keybindings;
mod state;
mod viewport;

use crossterm::{
    event::{
        poll as event_poll, read as event_read, Event as CrosstermEvent, KeyCode, KeyEvent,
        KeyModifiers,
    },
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use editor::Editor;
use keybindings::Action;
use ratatui::Terminal;
use std::{
    io::{self, stdout},
    path::PathBuf,
    time::Duration,
};

fn main() -> io::Result<()> {
    // Set up panic hook to restore terminal
    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic| {
        let _ = disable_raw_mode();
        let _ = stdout().execute(LeaveAlternateScreen);
        original_hook(panic);
    }));

    // Parse command-line arguments
    let args: Vec<String> = std::env::args().collect();
    let file_path = args.get(1).map(PathBuf::from);

    // Load configuration
    let config = config::Config::default();

    // Create editor
    let mut editor = Editor::new(config)?;

    // Open file if provided
    if let Some(path) = file_path {
        editor.open_file(&path)?;
    }

    // Set up terminal
    enable_raw_mode()?;
    stdout().execute(EnterAlternateScreen)?;
    let backend = ratatui::backend::CrosstermBackend::new(stdout());
    let mut terminal = Terminal::new(backend)?;

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
    // Special handling for help page
    if editor.is_help_visible() {
        match (key_event.code, key_event.modifiers) {
            // Close help with Esc or Ctrl+H
            (KeyCode::Esc, KeyModifiers::NONE) | (KeyCode::Char('h'), KeyModifiers::CONTROL) => {
                editor.toggle_help();
                return Ok(());
            }
            // Scroll help with Up/Down
            (KeyCode::Up, KeyModifiers::NONE) => {
                editor.scroll_help(-1);
                return Ok(());
            }
            (KeyCode::Down, KeyModifiers::NONE) => {
                editor.scroll_help(1);
                return Ok(());
            }
            // Scroll help with PageUp/PageDown
            (KeyCode::PageUp, KeyModifiers::NONE) => {
                editor.scroll_help(-10);
                return Ok(());
            }
            (KeyCode::PageDown, KeyModifiers::NONE) => {
                editor.scroll_help(10);
                return Ok(());
            }
            // Ignore other keys in help mode
            _ => return Ok(()),
        }
    }

    // Convert the key event to an Action using the keybinding resolver
    // For now, we'll implement a simple direct mapping
    // TODO: Use editor's keybinding resolver

    let action = match (key_event.code, key_event.modifiers) {
        // Quit
        (KeyCode::Char('q'), KeyModifiers::CONTROL) => Action::Quit,

        // Help
        (KeyCode::Char('h'), KeyModifiers::CONTROL) => Action::ShowHelp,

        // Character insertion
        (KeyCode::Char(c), KeyModifiers::NONE) | (KeyCode::Char(c), KeyModifiers::SHIFT) => {
            Action::InsertChar(c)
        }

        // Newline and tab
        (KeyCode::Enter, KeyModifiers::NONE) => Action::InsertNewline,
        (KeyCode::Tab, KeyModifiers::NONE) => Action::InsertTab,

        // Basic movement
        (KeyCode::Left, KeyModifiers::NONE) => Action::MoveLeft,
        (KeyCode::Right, KeyModifiers::NONE) => Action::MoveRight,
        (KeyCode::Up, KeyModifiers::NONE) => Action::MoveUp,
        (KeyCode::Down, KeyModifiers::NONE) => Action::MoveDown,
        (KeyCode::Home, KeyModifiers::NONE) => Action::MoveLineStart,
        (KeyCode::End, KeyModifiers::NONE) => Action::MoveLineEnd,
        (KeyCode::Home, KeyModifiers::CONTROL) => Action::MoveDocumentStart,
        (KeyCode::End, KeyModifiers::CONTROL) => Action::MoveDocumentEnd,

        // Word movement
        (KeyCode::Left, KeyModifiers::CONTROL) => Action::MoveWordLeft,
        (KeyCode::Right, KeyModifiers::CONTROL) => Action::MoveWordRight,

        // Page navigation
        (KeyCode::PageUp, KeyModifiers::NONE) => Action::MovePageUp,
        (KeyCode::PageDown, KeyModifiers::NONE) => Action::MovePageDown,

        // Delete
        (KeyCode::Backspace, KeyModifiers::NONE) => Action::DeleteBackward,
        (KeyCode::Delete, KeyModifiers::NONE) => Action::DeleteForward,
        (KeyCode::Backspace, KeyModifiers::CONTROL) => Action::DeleteWordBackward,
        (KeyCode::Delete, KeyModifiers::CONTROL) => Action::DeleteWordForward,

        // Selection
        (KeyCode::Left, KeyModifiers::SHIFT) => Action::SelectLeft,
        (KeyCode::Right, KeyModifiers::SHIFT) => Action::SelectRight,
        (KeyCode::Up, KeyModifiers::SHIFT) => Action::SelectUp,
        (KeyCode::Down, KeyModifiers::SHIFT) => Action::SelectDown,
        (KeyCode::Home, KeyModifiers::SHIFT) => Action::SelectLineStart,
        (KeyCode::End, KeyModifiers::SHIFT) => Action::SelectLineEnd,
        (KeyCode::Char('a'), KeyModifiers::CONTROL) => Action::SelectAll,

        // Clipboard
        (KeyCode::Char('c'), KeyModifiers::CONTROL) => Action::Copy,
        (KeyCode::Char('x'), KeyModifiers::CONTROL) => Action::Cut,
        (KeyCode::Char('v'), KeyModifiers::CONTROL) => Action::Paste,

        // Undo/Redo
        (KeyCode::Char('z'), KeyModifiers::CONTROL) => Action::Undo,
        (KeyCode::Char('y'), KeyModifiers::CONTROL) => Action::Redo,

        // File operations
        (KeyCode::Char('s'), KeyModifiers::CONTROL) => Action::Save,

        // Scroll
        (KeyCode::Up, KeyModifiers::CONTROL) => Action::ScrollUp,
        (KeyCode::Down, KeyModifiers::CONTROL) => Action::ScrollDown,

        // Multi-cursor
        (KeyCode::Esc, KeyModifiers::NONE) => Action::RemoveSecondaryCursors,

        // Unknown
        _ => Action::None,
    };

    // Handle the action
    handle_action(editor, action)?;

    Ok(())
}

/// Handle an action by converting it to events and applying them
fn handle_action(editor: &mut Editor, action: Action) -> io::Result<()> {
    match action {
        // Special actions that don't use the event system
        Action::Quit => {
            editor.quit();
        }

        Action::Save => {
            editor.save()?;
        }

        Action::Copy => {
            editor.copy_selection();
        }

        Action::Cut => {
            editor.cut_selection();
        }

        Action::Paste => {
            editor.paste();
        }

        Action::Undo => {
            // Get the event log and undo
            if let Some(event) = editor.active_event_log_mut().undo() {
                if let Some(inverse) = event.inverse() {
                    editor.active_state_mut().apply(&inverse);
                }
            }
        }

        Action::Redo => {
            // Get the event log and redo
            let event_opt = editor.active_event_log_mut().redo().cloned();
            if let Some(event) = event_opt {
                editor.active_state_mut().apply(&event);
            }
        }

        Action::ShowHelp => {
            editor.toggle_help();
        }

        Action::None => {
            // Do nothing
        }

        // All other actions: convert to events and apply
        _ => {
            if let Some(events) = editor.action_to_events(action) {
                for event in events {
                    // Record the event in the log
                    editor.active_event_log_mut().append(event.clone());

                    // Apply the event to the state
                    editor.active_state_mut().apply(&event);
                }
            }
        }
    }

    Ok(())
}
