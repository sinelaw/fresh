extern crate crossterm;
extern crate ratatui;
use std::{io, iter::FromIterator};

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    layout::Position,
    style::{Style, Stylize},
    text::{Line, Span, Text},
    DefaultTerminal, Frame,
};

struct State {
    // TODO
    // Map (even huge) files to memory
    // "lines" is a window into a small portion of the file starting at some offset
    // pageup/pagedown or scrolling up/down beyond end of screen, change the window being considered (load some lines from mmap, drop some)
    lines: Vec<Vec<char>>,
    cursor: Position, // relative to the screen (or current view window), not to the whole file
    insert_mode: bool,
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
                    code: KeyCode::Insert,
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.insert_mode = !self.insert_mode,

                Event::Key(KeyEvent {
                    code: KeyCode::Char(c),
                    modifiers: KeyModifiers::NONE,
                    ..
                }) => self.overwrite_or_insert_char(c),
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

    fn overwrite_or_insert_char(&mut self, c: char) {
        if self.insert_mode {
            self.insert_char(c);
            return;
        }
        let line = self.lines.get_mut(self.cursor.y as usize).unwrap();
        if let Some(elem) = line.get_mut(self.cursor.x as usize) {
            *elem = c;
            self.cursor.x += 1;
        } else {
            self.insert_char(c);
        }
    }

    fn insert_char(&mut self, c: char) {
        let line = self.lines.get_mut(self.cursor.y as usize).unwrap();
        line.insert(self.cursor.x as usize, c);
        self.cursor.x += 1;
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
        let line = self.lines.get_mut(self.cursor.y as usize).unwrap();
        let new_line = line.split_off(self.cursor.x as usize);
        self.cursor.y += 1;
        self.cursor.x = 0;
        self.lines.insert(self.cursor.y as usize, new_line);
    }

    fn render(&self, frame: &mut Frame) {
        let left_margin_width = self.left_margin_width();

        let render_line = |pair: (usize, &Vec<char>)| -> Line<'_> {
            let content = pair.1.iter().collect::<String>();
            let line_index = pair.0;
            Line::from(vec![
                Span::styled(
                    format!(
                        "{:>width$}",
                        (line_index + 1),
                        width = left_margin_width as usize
                    ),
                    Style::new().dark_gray(),
                ),
                Span::raw(" "),
                Span::raw(content),
            ])
        };

        frame.render_widget(
            Text::from_iter(self.lines.iter().enumerate().map(render_line)),
            frame.area(),
        );

        frame.set_cursor_position(Position::new(
            self.cursor.x + left_margin_width + 1,
            self.cursor.y,
        ));
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

    fn left_margin_width(&self) -> u16 {
        std::cmp::max(4, self.lines.len().to_string().len() as u16 + 1)
    }
}

fn main() -> io::Result<()> {
    let terminal = ratatui::init();
    let mut state: State = State {
        lines: vec![vec![]],
        cursor: Position::new(0, 0),
        insert_mode: true,
    };
    let result = state.run(terminal);
    ratatui::restore();
    result
}
