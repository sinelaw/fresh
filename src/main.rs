extern crate crossterm;
extern crate ratatui;
use std::{
    fs::OpenOptions,
    io::{self, Read, Seek},
    iter::FromIterator,
};

use crate::lines::LoadedLine;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    layout::{Position, Rect, Size},
    style::{Style, Stylize},
    text::{Line, Span, Text},
    DefaultTerminal, Frame,
};

mod chunks;
mod lines;
mod lines_iter;

// TODO
// How to represent edited content?
//
// Main problem: "insert char" pushes all remaining memory forward.
//
// Other editors:
// - emacs: gap buffer
// - vim: rope (https://github.com/vim/vim/blob/master/src/memline.c and https://github.com/vim/vim/blob/master/src/memfile.c)
//
// Idea:
// List of contiguous chunks: (disk_offset, data, is_modified).
// When a char is inserted in the middle of a chunk, split it into three: prev, cur, next.
// prev and next remain unchanged (can be dropped from memory and reloaded from disk).
// When saving the file, iterate chunks: write to disk, update disk_offset, set is_modified = false
// if disk_offset = write offset and is_modified = false, no need to write (skip the chunk).
// This can optimize writing huge files.

struct State {
    /// Content loaded from the file, may be a small portion of the entire file starting at some offset
    lines: Vec<LoadedLine>,

    /// Cursor position relative to loaded content (lines)
    cursor: Position,

    /// Offset of the visible part of the content
    window_offset: Position,

    /// If true, entering a character will insert it (pushing the rest of the line forward), otherwise will override the character at the cursor
    insert_mode: bool,

    /// Text to print at the status bar
    status_text: String,

    terminal_size: Size,

    file: Option<std::fs::File>,
    file_offset: u64,
}

impl State {
    fn load(&mut self) -> io::Result<()> {
        if let Some(ref mut f) = self.file {
            f.seek(io::SeekFrom::Start(self.file_offset))?;
            let mut buf = [0; 1024 * 1024];
            f.read(&mut buf)?;

            self.lines.clear();
            let line = LoadedLine::empty();
            self.lines.push(line);
            let mut y = 0;
            for byte in buf.iter() {
                let c: char = (*byte).into();
                if c == '\n' {
                    y += 1;
                    self.lines.push(LoadedLine::empty());
                } else {
                    self.lines[y].push(c);
                }
            }
        }
        Ok(())
    }

    fn run(&mut self, mut terminal: DefaultTerminal) -> io::Result<()> {
        loop {
            self.terminal_size = terminal.size()?;

            self.render(&mut terminal)?;

            let event = event::read()?;
            if !self.handle_event(event) {
                break Ok(());
            }
        }
    }

    fn render(
        &mut self,
        terminal: &mut ratatui::Terminal<ratatui::prelude::CrosstermBackend<io::Stdout>>,
    ) -> Result<(), io::Error> {
        self.status_text = format!("Line {}, Column {}", self.cursor.y, self.cursor.x);

        self.scroll_to_cursor(terminal.get_frame().area());

        terminal.draw(|x| self.draw_frame(x))?;
        Ok(())
    }

    fn pos_min_x_y(a: Position, b: Position) -> Position {
        return Position::new(a.x.min(b.x), a.y.min(b.y));
    }

    fn pos_max_x_y(a: Position, b: Position) -> Position {
        return Position::new(a.x.max(b.x), a.y.max(b.y));
    }

    fn scroll_to_cursor(&mut self, window_area: Rect) {
        // bring cursor into view
        let text_area = Rect::new(0, 0, window_area.width, window_area.height - 1);
        let left_margin_width = self.left_margin_width();

        let max_pos = Position::new(
            self.cursor
                .x
                .saturating_sub(text_area.width - 1 - left_margin_width - 1 /* to allow trailing cursor after last line character */),
            self.cursor.y.saturating_sub(text_area.height - 1),
        );
        self.window_offset = State::pos_max_x_y(self.window_offset, max_pos);
        self.window_offset = State::pos_min_x_y(self.window_offset, self.cursor);
        assert!(
            self.window_offset.y <= self.cursor.y,
            "window_offset={}, cursor={}",
            self.window_offset,
            self.cursor
        );
    }

    fn handle_event(&mut self, event: Event) -> bool {
        if let Event::Key(key_event) = event {
            return self.handle_key_event(key_event);
        }

        return true;
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) -> bool {
        match key_event {
            KeyEvent {
                code: KeyCode::Char('q'),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => return false,

            KeyEvent {
                code: KeyCode::Insert,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.insert_mode = !self.insert_mode,

            KeyEvent {
                code: KeyCode::Char(c),
                modifiers,
                ..
            } if modifiers == KeyModifiers::NONE || modifiers == KeyModifiers::SHIFT => {
                self.overwrite_or_insert_char(c)
            }

            KeyEvent {
                code: KeyCode::Backspace,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.delete_prev_char(),

            KeyEvent {
                code: KeyCode::Delete,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.delete_next_char(),

            KeyEvent {
                code: KeyCode::Home,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_to_line_start(),

            KeyEvent {
                code: KeyCode::End,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_to_line_end(),

            KeyEvent {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.insert_line(),

            KeyEvent {
                code: KeyCode::Left,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_left(),

            KeyEvent {
                code: KeyCode::Right,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_right(),

            KeyEvent {
                code: KeyCode::Left,
                modifiers: KeyModifiers::CONTROL,
                ..
            } => self.move_word_left(),

            KeyEvent {
                code: KeyCode::Right,
                modifiers: KeyModifiers::CONTROL,
                ..
            } => self.move_word_right(),

            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_down(),

            KeyEvent {
                code: KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_up(),

            KeyEvent {
                code: KeyCode::PageDown,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_page_down(),

            KeyEvent {
                code: KeyCode::PageUp,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_page_up(),

            _ => {}
        }

        return true;
    }

    fn overwrite_or_insert_char(&mut self, c: char) {
        if self.insert_mode {
            self.insert_char(c);
            return;
        }
        let line = self.lines.get_mut(self.cursor.y as usize).unwrap();
        if let Some(elem) = line.char_get_mut(self.cursor.x as usize) {
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
        self.lines
            .insert(self.cursor.y as usize, LoadedLine::from_vec(new_line));
    }

    fn draw_frame(&self, frame: &mut Frame) {
        let window_area = frame.area();
        let text_area = Rect::new(0, 0, window_area.width, window_area.height - 1);
        let status_area = Rect::new(0, window_area.height - 1, window_area.width, 1);
        let left_margin_width = self.left_margin_width();

        let render_line = |pair: (usize, &LoadedLine)| -> Line<'_> {
            let content = pair
                .1
                .chars_iter()
                .skip(self.window_offset.x as usize)
                .collect::<String>();
            let line_index = pair.0;
            Line::from(vec![
                Span::styled(
                    format!(
                        "{:>width$}",
                        (line_index + 1),
                        width = left_margin_width as usize
                    ),
                    if self.cursor.y as usize == line_index {
                        Style::new().white()
                    } else {
                        Style::new().dark_gray()
                    },
                ),
                Span::raw(" "),
                Span::raw(content),
            ])
        };

        frame.render_widget(
            Text::from_iter(
                self.lines
                    .iter()
                    .enumerate()
                    .skip(self.window_offset.y as usize)
                    .map(render_line),
            ),
            text_area,
        );

        frame.render_widget(self.status_text.clone(), status_area);

        frame.set_cursor_position(Position::new(
            self.cursor.x + left_margin_width + 1 - self.window_offset.x,
            self.cursor.y - self.window_offset.y,
        ));
    }

    fn move_left(&mut self) {
        if self.cursor.x > 0 {
            self.cursor.x -= 1;
        } else if self.cursor.y > 0 {
            self.cursor.y -= 1;
            let prev_line = self.get_current_line();
            self.cursor.x = prev_line.len() as u16;
        }
    }

    fn move_right(&mut self) {
        let line = self.get_current_line();
        if self.cursor.x < line.len() as u16 {
            self.cursor.x += 1;
        } else if self.cursor.y + 1 < self.lines.len() as u16 {
            self.cursor.y += 1;
            self.cursor.x = 0;
        }
    }

    fn move_word_left(&mut self) {
        if self.cursor.x == 0 {
            self.move_left();
            return;
        }
        let line = self.get_current_line();
        let start_char = line.char_get(self.cursor.x as usize - 1).unwrap();
        let is_whitespace = start_char.is_whitespace();
        for i in (0..self.cursor.x).rev() {
            if line.char_get(i as usize).unwrap().is_whitespace() != is_whitespace {
                self.cursor.x = i;
                return;
            }
        }
        self.cursor.x = 0;
    }

    fn move_word_right(&mut self) {
        let line_len = self.get_current_line().len() as u16;
        if self.cursor.x == line_len {
            self.move_right();
            return;
        }
        let line = self.get_current_line();
        let line_len = line.len() as u16;
        let start_char = line.char_get(self.cursor.x as usize).unwrap();
        let is_whitespace = start_char.is_whitespace();
        for i in self.cursor.x..line_len {
            if line.char_get(i as usize).unwrap().is_whitespace() != is_whitespace {
                self.cursor.x = i;
                return;
            }
        }
        self.cursor.x = line_len;
    }

    fn move_up(&mut self) {
        if self.cursor.y == 0 {
            return;
        }
        self.cursor.y -= 1;
        let line = self.get_current_line();
        self.cursor.x = std::cmp::min(self.cursor.x, line.len() as u16);
    }

    fn move_down(&mut self) {
        if self.cursor.y + 1 >= self.lines.len() as u16 {
            return;
        }
        self.cursor.y += 1;
        let line = self.get_current_line();
        self.cursor.x = std::cmp::min(self.cursor.x, line.len() as u16);
    }

    fn move_page_up(&mut self) {
        for _ in 0..self.lines_per_page() {
            self.move_up();
        }
    }

    fn move_page_down(&mut self) {
        for _ in 0..self.lines_per_page() {
            self.move_down();
        }
    }

    fn move_to_line_start(&mut self) {
        self.cursor.x = 0;
    }

    fn move_to_line_end(&mut self) {
        self.cursor.x = self.get_current_line().len() as u16;
    }

    fn lines_per_page(&self) -> u16 {
        return self.terminal_size.height - 1;
    }

    fn left_margin_width(&self) -> u16 {
        std::cmp::max(4, self.lines.len().to_string().len() as u16 + 1)
    }

    fn get_current_line(&self) -> &LoadedLine {
        self.lines.get(self.cursor.y as usize).unwrap()
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let file: Option<std::fs::File> = if args.len() > 1 {
        let filename = &args[1];
        Some(OpenOptions::new().read(true).open(filename)?)
    } else {
        None
    };
    let terminal = ratatui::init();
    let mut state: State = State {
        lines: vec![LoadedLine::empty()],
        window_offset: Position::new(0, 0),
        cursor: Position::new(0, 0),
        insert_mode: true,
        status_text: String::new(),
        file: file,
        file_offset: 0,
        terminal_size: terminal.size()?,
    };
    state.load()?;
    let result = state.run(terminal);
    ratatui::restore();
    result
}
