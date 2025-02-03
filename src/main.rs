#![allow(dead_code)]
mod chunk_tree;
mod lines;
mod logs;
mod memstore;
mod virtual_file;

extern crate crossterm;
extern crate ratatui;
extern crate tempfile;

use std::{
    fs::OpenOptions,
    io::{self, SeekFrom},
    iter::FromIterator,
};

use crate::lines::EditLine;
use crate::virtual_file::{LineCursor, VirtualFile};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    layout::{Position, Rect, Size},
    style::{Style, Stylize},
    text::{Line, Span, Text},
    DefaultTerminal, Frame,
};
use tree_sitter_highlight::{Highlight, HighlightConfiguration, HighlightEvent, Highlighter};
use virtual_file::LoadedLine;

// TODO
// How to represent edited content?
// Layers and deltas:
// Original file is base layer. Edits are additional layers on top (newer edits are higher layers).
// Changes represented in an incremental log that can be stored and loaded, used for undo/redo, etc.
const HIGHLIGHT_NAMES: [&str; 19] = [
    "comment",
    "attribute",
    "constant",
    "function.builtin",
    "function",
    "keyword",
    "operator",
    "property",
    "punctuation",
    "punctuation.bracket",
    "punctuation.delimiter",
    "string",
    "string.special",
    "tag",
    "type",
    "type.builtin",
    "variable",
    "variable.builtin",
    "variable.parameter",
];

struct State {
    /// Content loaded from the file, may be a small portion of the entire file starting at some offset
    lines: VirtualFile,

    line_index: LineCursor,

    /// Cursor position relative to ???
    cursor: Position,

    /// Offset of the visible part of the content
    window_offset: Position,

    /// If true, entering a character will insert it (pushing the rest of the line forward), otherwise will override the character at the cursor
    insert_mode: bool,

    /// Text to print at the status bar
    status_text: String,

    terminal_size: Size,

    highlighter: Highlighter,
    highlighter_config: HighlightConfiguration,
}

impl State {
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

        self.scroll_to_cursor();
        terminal.draw(|x| self.draw_frame(x))?;
        Ok(())
    }

    fn pos_min_x_y(a: Position, b: Position) -> Position {
        return Position::new(a.x.min(b.x), a.y.min(b.y));
    }

    fn pos_max_x_y(a: Position, b: Position) -> Position {
        return Position::new(a.x.max(b.x), a.y.max(b.y));
    }

    fn text_area(&self) -> Size {
        Size::new(self.terminal_size.width, self.terminal_size.height - 1)
    }

    fn scroll_to_cursor(&mut self) {
        // bring cursor into view
        let text_area = self.text_area();
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
                modifiers: KeyModifiers::CONTROL,
                ..
            } => self.move_to_file_start(),

            KeyEvent {
                code: KeyCode::End,
                modifiers: KeyModifiers::CONTROL,
                ..
            } => self.move_to_file_end(),

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

            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::CONTROL,
                ..
            } => self.scroll_down(),

            KeyEvent {
                code: KeyCode::Up,
                modifiers: KeyModifiers::CONTROL,
                ..
            } => self.scroll_up(),

            _ => {}
        }

        return true;
    }

    fn overwrite_or_insert_char(&mut self, c: char) {
        if self.insert_mode {
            self.insert_char(c);
            return;
        }
        if let Some(line) = self.lines.get_mut(&self.line_index) {
            if line.len() < (self.cursor.x as usize) {
                line.overwrite(self.cursor.x as usize, c);
                self.cursor.x += 1;
            } else {
                self.insert_char(c);
            }
        }
    }

    fn insert_char(&mut self, c: char) {
        if let Some(line) = self.lines.get_mut(&self.line_index) {
            line.insert(self.cursor.x as usize, c);
            self.cursor.x += 1;
        }
    }

    fn iter_line_prev(&mut self) {
        let prev_index: LineCursor = self.line_index;
        self.line_index = self.lines.prev_line(&self.line_index).unwrap_or(prev_index);
        if self.line_index != prev_index {
            self.cursor.y -= 1;
        }
    }

    fn iter_line_next(&mut self) -> bool {
        let prev_index: LineCursor = self.line_index;
        self.line_index = self.lines.next_line(&self.line_index).unwrap_or(prev_index);
        if self.line_index != prev_index {
            self.cursor.y += 1;
            return true;
        }
        return false;
    }

    fn delete_prev_char(&mut self) {
        if self.cursor.x > 0 {
            if let Some(line) = self.lines.get_mut(&self.line_index) {
                self.cursor.x -= 1;
                line.remove(self.cursor.x as usize);
            }
            return;
        }
        let removed_line_index = self.line_index;
        self.iter_line_prev();
        if let Some(line) = self.lines.remove(&removed_line_index) {
            if let Some(prev_line) = self.lines.get_mut(&self.line_index) {
                self.cursor.x = prev_line.len() as u16;
                prev_line.extend(line);
            } else {
                self.cursor.x = 0;
            }
        }
    }

    fn delete_next_char(&mut self) {
        if let Some(line) = self.lines.get_mut(&self.line_index) {
            if self.cursor.x < line.len() as u16 {
                line.remove(self.cursor.x as usize);
                return;
            }
            self.iter_line_next();
            if let Some(next_line) = self.lines.remove(&self.line_index) {
                if let Some(line) = self.lines.get_mut(&self.line_index) {
                    line.extend(next_line);
                }
            }
        }
    }

    fn insert_line(&mut self) {
        if let Some(line) = self.lines.get_mut(&self.line_index) {
            let new_line = line.split_off(self.cursor.x as usize);
            self.lines
                .insert_after(&self.line_index, EditLine::new(new_line));
            self.iter_line_next();
            self.cursor.x = 0;
        }
    }

    fn draw_frame(&mut self, frame: &mut Frame) {
        let window_area = frame.area();
        let text_area = self.text_area();
        let status_area = Rect::new(0, window_area.height - 1, window_area.width, 1);
        let mut left_margin_width = self.left_margin_width();
        let cursor = self.cursor;
        let lines_per_page = self.lines_per_page();
        let window_offset = self.window_offset;

        let editor_style = Style::new().bg(ratatui::style::Color::Black);
        let current_line_style = Style::new().bg(ratatui::style::Color::Rgb(30, 30, 30));
        let status_bar_style = Style::new().bg(ratatui::style::Color::Rgb(50, 50, 50));

        let highlighter = &mut self.highlighter;
        let highlighter_config = &mut self.highlighter_config;

        let lines: Vec<&LoadedLine> = self
            .lines
            .iter_at(
                &self
                    .line_index
                    .plus(self.window_offset.y as i64 - self.cursor.y as i64),
                lines_per_page as usize,
            )
            .collect();

        let mut bytes = vec![];
        for line in lines {
            bytes.extend_from_slice(line.line().str().as_bytes());
            bytes.push(b'\n');
        }

        let rendered_parts = Self::syntax_highlight(&bytes, highlighter, highlighter_config);
        /*
        let render_line = move |(window_index, loaded_line): (usize, &LoadedLine)| -> Line<'_> {
            let line_label = Self::line_label(loaded_line);
            let formatted_label =
                format!("{:>width$}", line_label, width = left_margin_width as usize);

            // Update margin width for the rest of the lines going forward:
            left_margin_width =
                u16::max(left_margin_width, formatted_label.len().try_into().unwrap());

            let is_current_line = (cursor.y - window_offset.y) as usize == window_index;

            let content = loaded_line
                .line()
                .chars_iter()
                .skip(window_offset.x as usize)
                .collect::<String>();

            rendered_parts
            /*
            let styled_content = Span::styled(
                content,
                if is_current_line {
                    current_line_style
                } else {
                    Style::new()
                },
            ); */
            /*
            Line::from(vec![
                Span::styled(
                    formatted_label,
                    if is_current_line {
                        Style::new().white()
                    } else {
                        Style::new().dark_gray()
                    },
                ),
                Span::raw(" "),
                styled_content,
            ]) */
        }; */

        frame.render_widget(
            //Text::from_iter(lines.map(render_line)).style(editor_style),
            Text::from_iter(rendered_parts).style(editor_style),
            Rect::new(0, 0, text_area.width, text_area.height),
        );

        self.status_text = format!(
            "cursor: {:?}, window_offset {:?}, text_area: {:?}",
            self.cursor,
            self.window_offset,
            self.text_area()
        );

        frame.render_widget(
            Text::from(self.status_text.clone()).style(status_bar_style),
            status_area,
        );

        frame.set_cursor_position(Position::new(
            self.cursor.x - self.window_offset.x, //self.cursor.x + left_margin_width + 1 - self.window_offset.x,
            self.cursor.y - self.window_offset.y,
        ));
    }

    fn syntax_highlight<'a>(
        bytes: &'a [u8],
        highlighter: &'a mut Highlighter,
        highlighter_config: &'a mut HighlightConfiguration,
    ) -> Vec<Line<'a>> {
        let regions = highlighter
            .highlight(&highlighter_config, bytes, None, |_| None)
            .unwrap();

        let mut highlights: Vec<Highlight> = vec![];
        let mut rendered_parts: Vec<Span<'_>> = vec![];
        let mut rendered_lines: Vec<Line<'_>> = vec![];
        //logs::log!("bytes:\n{}\n", String::from_utf8_lossy(bytes));
        for part in regions {
            match part.unwrap() {
                HighlightEvent::Source { start, end } => {
                    let s = highlights.last();
                    let content = String::from_utf8_lossy(&bytes[start..end]);
                    let part_style = highlight_to_style(s);

                    logs::log!("{:?} => [{}..{}] = {}", s, start, end, content);
                    for (idx, content_part) in content.split('\n').enumerate() {
                        if idx > 0 {
                            rendered_lines.push(Line::from(rendered_parts));
                            rendered_parts = vec![];
                        }
                        rendered_parts.push(Span::styled(content_part.to_owned(), part_style));
                    }
                }
                HighlightEvent::HighlightStart(highlight) => {
                    highlights.push(highlight);
                }
                HighlightEvent::HighlightEnd => {
                    highlights.pop();
                }
            }
        }
        rendered_lines.push(Line::from_iter(rendered_parts));
        rendered_lines
    }

    fn move_left(&mut self) {
        if self.cursor.x > 0 {
            self.cursor.x -= 1;
        } else if self.cursor.y > 0 {
            self.iter_line_prev();
            let prev_line = self.get_current_line();
            self.cursor.x = prev_line.map(|x| x.len() as u16).unwrap_or(0);
        }
    }

    fn move_right(&mut self) {
        if let Some(line) = self.get_current_line() {
            if self.cursor.x < line.len() as u16 {
                self.cursor.x += 1;
            } else {
                if self.iter_line_next() {
                    self.cursor.x = 0;
                }
            }
        }
    }

    fn move_word_left(&mut self) {
        if self.cursor.x == 0 {
            self.move_left();
            return;
        }
        if let Some(line) = self.get_current_line() {
            let start_char = line.char_get(self.cursor.x as usize - 1).unwrap();
            let is_whitespace = start_char.is_whitespace();
            for i in (0..self.cursor.x).rev() {
                if line.char_get(i as usize).unwrap().is_whitespace() != is_whitespace {
                    self.cursor.x = i;
                    return;
                }
            }
        }
        self.cursor.x = 0;
    }

    fn move_word_right(&mut self) {
        if let Some(line) = self.get_current_line() {
            let line_len = line.len() as u16;
            if self.cursor.x == line_len {
                self.move_right();
                return;
            }
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
    }

    fn move_up(&mut self) {
        self.iter_line_prev();
        if let Some(line) = self.get_current_line() {
            self.cursor.x = std::cmp::min(self.cursor.x, line.len() as u16);
        }
    }

    fn move_down(&mut self) {
        self.iter_line_next();
        if let Some(line) = self.get_current_line() {
            self.cursor.x = std::cmp::min(self.cursor.x, line.len() as u16);
        }
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

    fn scroll_down(&mut self) {
        if self.window_offset.y == self.cursor.y {
            self.move_down();
        }
        self.window_offset.y += 1;
    }

    fn scroll_up(&mut self) {
        if self.window_offset.y + self.text_area().height - 1 == self.cursor.y {
            self.move_up();
        }
        self.window_offset.y = self.window_offset.y.saturating_sub(1);
    }

    fn move_to_line_start(&mut self) {
        self.cursor.x = 0;
    }

    fn move_to_line_end(&mut self) {
        self.cursor.x = self.get_current_line().map(|x| x.len() as u16).unwrap_or(0);
    }

    fn lines_per_page(&self) -> u16 {
        return self.terminal_size.height - 1;
    }

    fn get_current_line(&self) -> Option<&EditLine> {
        self.lines.get(&self.line_index).map(|l| l.line())
    }

    fn move_to_file_start(&mut self) {
        self.line_index = self.lines.seek(SeekFrom::Start(0));
        self.cursor.y = 0;
    }

    fn move_to_file_end(&mut self) {
        self.line_index = self.lines.seek(SeekFrom::End(0));
        self.cursor.y = 0;

        // populate lines to fill the window
        for _ in 0..self.text_area().height {
            self.iter_line_prev();
        }
        // go to the last line
        for _ in 1..self.text_area().height {
            self.iter_line_next();
        }
    }

    fn left_margin_width(&self) -> u16 {
        let cur_line = self.lines.get(&self.line_index);
        let label_width: u16 = cur_line
            .map(|l| Self::line_label(l).len())
            .unwrap_or(0) // no line, pretend empty label
            .try_into()
            .unwrap();
        // Use the current line's label width + 1, but at least 5 characters:
        u16::max(7, label_width)
    }

    fn line_label(loaded_line: &LoadedLine) -> String {
        loaded_line
            .loaded_loc()
            .map(|l| format!("{:x}", l.loaded_offset))
            .unwrap_or("?".to_owned())
    }

    pub fn new(terminal: &DefaultTerminal, file: std::fs::File) -> State {
        let highlighter = Highlighter::new();

        let language = tree_sitter_rust::LANGUAGE;

        let mut highlighter_config = HighlightConfiguration::new(
            language.into(),
            "rust",
            tree_sitter_rust::HIGHLIGHTS_QUERY,
            tree_sitter_rust::INJECTIONS_QUERY,
            "",
        )
        .unwrap();

        highlighter_config.configure(&HIGHLIGHT_NAMES);

        let mut lines = VirtualFile::new(1024 * 1024, file);
        let line_index = lines.seek(SeekFrom::Start(0));

        State {
            //lines: vec![LoadedLine::empty()],
            window_offset: Position::new(0, 0),
            cursor: Position::new(0, 0),
            insert_mode: true,
            status_text: String::new(),
            terminal_size: terminal.size().unwrap(),
            line_index: line_index,
            lines,
            highlighter,
            highlighter_config,
        }
    }
}

fn highlight_to_style(s: Option<&Highlight>) -> Style {
    if let Some(h) = s {
        let i: usize = h.0;
        let name = HIGHLIGHT_NAMES[i];
        match name {
            "comment" => Style::new().green(),
            "attribute" => Style::new().red(),
            "constant" => Style::new().cyan(),
            "function.builtin" => Style::new(),
            "function" => Style::new().yellow(),
            "keyword" => Style::new().magenta(),
            "operator" => Style::new().white(),
            "property" => Style::new().blue(),
            "punctuation" => Style::new().gray(),
            "punctuation.bracket" => Style::new().light_yellow(),
            "punctuation.delimiter" => Style::new().light_yellow(),
            "string" => Style::new().light_red(),
            "string.special" => Style::new().light_red(),
            "tag" => Style::new().light_blue(),
            "type" => Style::new().light_green(),
            "type.builtin" => Style::new().light_green(),
            "variable" => Style::new().light_cyan(),
            "variable.builtin" => Style::new().light_cyan(),
            "variable.parameter" => Style::new().light_cyan(),
            _ => Style::new(),
        }
    } else {
        Style::new()
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let file: std::fs::File = {
        let filename = args.get(1).map_or("/tmp/editor_tmpfile.tmp", |v| v);
        OpenOptions::new()
            .create(true)
            .write(true)
            .read(true)
            .open(filename)?
    };

    let terminal = ratatui::init();

    let mut state: State = State::new(&terminal, file);
    //state.load()?;
    let result = state.run(terminal);
    ratatui::restore();
    result
}
