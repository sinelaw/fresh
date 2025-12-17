//! A simple binary that listens for key/mouse events and prints them to stdout.
//! Useful for debugging input handling.
//!
//! Press Ctrl+C or 'q' to exit.

use crossterm::{
    event::{
        poll as event_poll, read as event_read, Event, KeyCode, KeyEventKind, KeyModifiers,
        KeyboardEnhancementFlags, PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
    },
    terminal::{disable_raw_mode, enable_raw_mode},
    ExecutableCommand,
};
use std::{
    io::{self, stdout},
    time::Duration,
};

fn main() -> io::Result<()> {
    println!("Event Debug Tool");
    println!("================");
    println!("Press Ctrl+C or 'q' to exit.\n");

    // Enable raw mode
    enable_raw_mode()?;

    // Enable keyboard enhancement flags for better key detection
    let keyboard_flags = KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
        | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS;
    let _ = stdout().execute(PushKeyboardEnhancementFlags(keyboard_flags));

    // Enable mouse capture
    let _ = crossterm::execute!(stdout(), crossterm::event::EnableMouseCapture);

    let result = run_event_loop();

    // Clean up
    let _ = crossterm::execute!(stdout(), crossterm::event::DisableMouseCapture);
    let _ = stdout().execute(PopKeyboardEnhancementFlags);
    disable_raw_mode()?;

    println!("\nExiting.");
    result
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
                        println!(
                            "Key: code={:?}, modifiers={:?}, kind={:?}, state={:?}",
                            key_event.code, key_event.modifiers, key_event.kind, key_event.state
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
                    println!(
                        "Mouse: kind={:?}, column={}, row={}, modifiers={:?}",
                        mouse_event.kind,
                        mouse_event.column,
                        mouse_event.row,
                        mouse_event.modifiers
                    );
                }
                Event::Resize(width, height) => {
                    println!("Resize: width={}, height={}", width, height);
                }
                Event::FocusGained => {
                    println!("Focus: Gained");
                }
                Event::FocusLost => {
                    println!("Focus: Lost");
                }
                Event::Paste(text) => {
                    println!("Paste: {:?}", text);
                }
            }
        }
    }

    Ok(())
}
