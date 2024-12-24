extern crate crossterm;
extern crate ratatui;
use std::io;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{DefaultTerminal, Frame};

struct State {
    text: Vec<char>,
}

impl State {
    fn run(&mut self, mut terminal: DefaultTerminal) -> io::Result<()> {
        loop {
            terminal.draw(|x| self.render(x))?;
            let event = event::read()?;

            match event {
                Event::Key(KeyEvent {
                    code: KeyCode::Char('q'),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                }) => break Ok(()),
                Event::Key(KeyEvent {
                    code: KeyCode::Char(c),
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.insert_char(c),
                Event::Key(KeyEvent {
                    code: KeyCode::Enter,
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.insert_char('\n'),
                _ => {}
            }
        }
    }

    fn insert_char(&mut self, c: char) {
        self.text.push(c);
    }

    fn render(&self, frame: &mut Frame) {
        let s: String = self.text.iter().collect();
        frame.render_widget(s, frame.area());
    }
}

fn main() -> io::Result<()> {
    let terminal = ratatui::init();
    let mut state: State = State { text: Vec::new() };
    let result = state.run(terminal);
    ratatui::restore();
    result
}
