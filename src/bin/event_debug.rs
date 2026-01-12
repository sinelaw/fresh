//! A simple binary that listens for key/mouse events and prints them to stdout.
//! Useful for debugging input handling.
//!
//! Press Ctrl+C or 'q' to exit.

use crossterm::{
    cursor::MoveToColumn,
    event::{
        poll as event_poll, read as event_read, Event, KeyCode, KeyEventKind, KeyModifiers,
        KeyboardEnhancementFlags, PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
    },
    terminal::{disable_raw_mode, enable_raw_mode},
    ExecutableCommand,
};
use std::{
    fmt::Arguments,
    io::{self, stdout, Write},
    time::Duration,
};

fn main() -> io::Result<()> {
    println!("Event Debug Tool");
    println!("================");
    println!("Press Ctrl+C or 'q' to exit.\n");

    crossterm::terminal::enable_raw_mode()?;

    crossterm::execute!(
        std::io::stdout(),
        crossterm::event::EnableMouseCapture,
        crossterm::event::PushKeyboardEnhancementFlags(
            crossterm::event::KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
                | crossterm::event::KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS
        )
    )?;

    let result = run_event_loop();

    // Clean up
    let _ = crossterm::execute!(stdout(), crossterm::event::DisableMouseCapture);
    let _ = stdout().execute(PopKeyboardEnhancementFlags);
    disable_raw_mode()?;

    println!("\nExiting.");
    result
}

fn log_line(args: Arguments<'_>) {
    // Move to column 0 to avoid jumbled output
    let mut out = stdout();
    let _ = writeln!(out, "{args}");
    let _ = out.execute(MoveToColumn(0));
}

macro_rules! log_line {
    ($($arg:tt)*) => {
        log_line(format_args!($($arg)*))
    };
}

fn run_event_loop() -> io::Result<()> {
    loop {
        // Poll for events with a timeout
        if event_poll(Duration::from_millis(100))? {
            let event = event_read()?;

            match &event {
                Event::Key(key_event) => {
                    // Only process key press events (not release/repeat)
                    if key_event.kind == KeyEventKind::Press {
                        log_line!(
                            "Key: code={:?}, modifiers={:?}, kind={:?}, state={:?}",
                            key_event.code,
                            key_event.modifiers,
                            key_event.kind,
                            key_event.state
                        );

                        // Exit on Ctrl+C or 'q'
                        if key_event.code == KeyCode::Char('c')
                            && key_event.modifiers.contains(KeyModifiers::CONTROL)
                        {
                            break;
                        }
                        if key_event.code == KeyCode::Char('q') && key_event.modifiers.is_empty() {
                            break;
                        }
                    }
                }
                Event::Mouse(mouse_event) => {
                    log_line!(
                        "Mouse: kind={:?}, column={}, row={}, modifiers={:?}",
                        mouse_event.kind,
                        mouse_event.column,
                        mouse_event.row,
                        mouse_event.modifiers
                    );
                }
                Event::Resize(width, height) => {
                    log_line!("Resize: width={}, height={}", width, height);
                }
                Event::FocusGained => {
                    log_line!("Focus: Gained");
                }
                Event::FocusLost => {
                    log_line!("Focus: Lost");
                }
                Event::Paste(text) => {
                    log_line!("Paste: {:?}", text);
                }
            }
        }
    }

    Ok(())
}
