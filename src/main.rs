extern crate crossterm;
extern crate ratatui;
use std::io;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{DefaultTerminal, Frame};

fn main() -> io::Result<()> {
    let terminal = ratatui::init();
    let result = run(terminal);
    ratatui::restore();
    result
}

fn run(mut terminal: DefaultTerminal) -> io::Result<()> {
    loop {
        terminal.draw(render)?;
        let event = event::read()?;

        match event {
            Event::Key(  KeyEvent{ code : KeyCode::Char('q'), modifiers: KeyModifiers::CONTROL, ..  } ) => break Ok(()),
            _ => {}
        }
    }
}

fn render(frame: &mut Frame) {
    frame.render_widget("hello world", frame.area());
}
