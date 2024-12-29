extern crate crossterm;
extern crate ratatui;
use std::{io, iter::FromIterator};

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    layout::Position,
    text::{Line, Span, Text},
    DefaultTerminal, Frame,
};

struct State {
    lines: Vec<Vec<char>>,
    cursor: Position,
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
                    code: KeyCode::Backspace,
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.delete_prev_char(),

                Event::Key(KeyEvent {
                    code: KeyCode::Delete,
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.delete_next_char(),
                Event::Key(KeyEvent {
                    code: KeyCode::Enter,
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.insert_line(),
                Event::Key(KeyEvent {
                    code: KeyCode::Left,
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.move_left(),
                Event::Key(KeyEvent {
                    code: KeyCode::Right,
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.move_right(),

                Event::Key(KeyEvent {
                    code: KeyCode::Down,
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.move_down(),

                Event::Key(KeyEvent {
                    code: KeyCode::Up,
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.move_up(),

                _ => {}
            }
        }
    }

    fn insert_char(&mut self, c: char) {
        let line = self.lines.get_mut(self.cursor.y as usize);
        match line {
            None => {}
            Some(v) => {
                v.insert(self.cursor.x as usize, c);
                self.cursor.x += 1;
            }
        }
    }

    fn delete_prev_char(&mut self) {
        if self.cursor.x > 0 {
            self.cursor.x -= 1;
            let line = self.lines.get_mut(self.cursor.y as usize).unwrap();
            line.remove(self.cursor.x as usize);
        } else if self.cursor.y > 0 {
            self.cursor.y -= 1;
            let line = self.lines.remove((self.cursor.y + 1) as usize);
            let prev_line = self.lines.get_mut(self.cursor.y as usize).unwrap();
            self.cursor.x = prev_line.len() as u16;
            prev_line.extend(line);
        }
    }

    fn delete_next_char(&mut self) {
        let line = self.lines.get_mut(self.cursor.y as usize).unwrap();
        if self.cursor.x < line.len() as u16 {
            line.remove(self.cursor.x as usize);
        } else if self.cursor.y + 1 < self.lines.len() as u16 {
            let next_line = self.lines.remove((self.cursor.y + 1) as usize);
            let line = self.lines.get_mut(self.cursor.y as usize).unwrap();
            line.extend(next_line);
        }
    }

    fn insert_line(&mut self) {
        self.cursor.y += 1;
        self.cursor.x = 0;
        self.lines.insert(self.cursor.y as usize, vec![]);
    }

    fn render(&self, frame: &mut Frame) {
        fn to_line(l: &Vec<char>) -> Line<'_> {
            let content = l.iter().collect::<String>();
            Line::from(Span::raw(content))
        }
        frame.render_widget(
            Text::from_iter(self.lines.iter().map(to_line)),
            frame.area(),
        );
        frame.set_cursor_position(self.cursor);
    }

    fn move_left(&mut self) {
        if self.cursor.x > 0 {
            self.cursor.x -= 1;
        } else if self.cursor.y > 0 {
            self.cursor.y -= 1;
            let prev_line = self.lines.get(self.cursor.y as usize).unwrap();
            self.cursor.x = prev_line.len() as u16;
        }
    }

    fn move_right(&mut self) {
        let line = self.lines.get(self.cursor.y as usize).unwrap();
        if self.cursor.x < line.len() as u16 {
            self.cursor.x += 1;
        } else if self.cursor.y + 1 < self.lines.len() as u16 {
            self.cursor.y += 1;
            self.cursor.x = 0;
        }
    }

    fn move_up(&mut self) {
        if self.cursor.y == 0 {
            return;
        }
        self.cursor.y -= 1;
        let line = self.lines.get(self.cursor.y as usize).unwrap();
        self.cursor.x = std::cmp::min(self.cursor.x, line.len() as u16);
    }
    fn move_down(&mut self) {
        if self.cursor.y + 1 >= self.lines.len() as u16 {
            return;
        }
        self.cursor.y += 1;
        let line = self.lines.get(self.cursor.y as usize).unwrap();
        self.cursor.x = std::cmp::min(self.cursor.x, line.len() as u16);
    }
}

fn main() -> io::Result<()> {
    let terminal = ratatui::init();
    let mut state: State = State {
        lines: vec![vec![]],
        cursor: Position::new(0, 0),
    };
    let result = state.run(terminal);
    ratatui::restore();
    result
}
