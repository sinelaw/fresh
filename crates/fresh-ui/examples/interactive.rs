//! The demo application, driven live in a real terminal.
//!
//! Where `examples/demo.rs` feeds a scripted sequence of inputs and prints each
//! frame, this one runs the whole loop against the terminal: it reads keys and
//! mouse events, translates them into `fresh_ui::Input`, dispatches them, and
//! folds the resulting display list into coloured cells with crossterm.
//!
//!     cargo run -p fresh-ui --example interactive
//!
//! Quit with Ctrl-Q. The library itself has no terminal dependency; crossterm
//! is a dev-dependency, so it is compiled only for examples and tests. This
//! file is an ordinary backend — a fold over `LayoutSpec::items` — and is the
//! shape a real host renderer takes.

#[path = "../tests/support/mod.rs"]
mod support;

use std::io::{self, Write};
use std::time::Duration;

use crossterm::event::{
    self, DisableMouseCapture, EnableMouseCapture, Event as CtEvent, KeyCode as CtKey,
    KeyEventKind, KeyModifiers, MouseButton as CtButton, MouseEventKind,
};
use crossterm::style::{Color, Print, ResetColor, SetBackgroundColor, SetForegroundColor};
use crossterm::{cursor, execute, queue, terminal};

use fresh_ui::{
    Draw, Input, KeyCode, KeyPress, Mods, MouseButton, Point, Rect, Scrim, Size, ThemeKey,
};

use support::demo::Demo;

fn main() -> io::Result<()> {
    let mut term = Terminal::enter()?;
    let (w, h) = terminal::size()?;
    let mut demo = Demo::new(Size::new(w, h));

    loop {
        term.draw(demo.ui.spec())?;

        // Block for the next event, but wake periodically so a background task
        // completing (the demo's simulated sync) is pumped in and redrawn.
        if !event::poll(Duration::from_millis(100))? {
            demo.pump();
            continue;
        }
        match event::read()? {
            CtEvent::Key(k) if k.kind != KeyEventKind::Release => {
                if k.code == CtKey::Char('q') && k.modifiers.contains(KeyModifiers::CONTROL) {
                    break;
                }
                if let Some(press) = translate_key(k.code, k.modifiers) {
                    demo.input(Input::Key(press));
                }
            }
            CtEvent::Mouse(m) => {
                if let Some(input) = translate_mouse(m) {
                    demo.input(input);
                }
            }
            CtEvent::Resize(w, h) => demo.resize(Size::new(w, h)),
            _ => {}
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Input translation: crossterm events into the library's own input vocabulary
// ---------------------------------------------------------------------------

fn mods(m: KeyModifiers) -> Mods {
    Mods {
        ctrl: m.contains(KeyModifiers::CONTROL),
        alt: m.contains(KeyModifiers::ALT),
        shift: m.contains(KeyModifiers::SHIFT),
    }
}

fn translate_key(code: CtKey, m: KeyModifiers) -> Option<KeyPress> {
    let code = match code {
        CtKey::Char(c) => KeyCode::Char(c),
        CtKey::Enter => KeyCode::Enter,
        CtKey::Esc => KeyCode::Esc,
        CtKey::Tab => KeyCode::Tab,
        CtKey::BackTab => KeyCode::BackTab,
        CtKey::Backspace => KeyCode::Backspace,
        CtKey::Delete => KeyCode::Delete,
        CtKey::Up => KeyCode::Up,
        CtKey::Down => KeyCode::Down,
        CtKey::Left => KeyCode::Left,
        CtKey::Right => KeyCode::Right,
        CtKey::Home => KeyCode::Home,
        CtKey::End => KeyCode::End,
        CtKey::PageUp => KeyCode::PageUp,
        CtKey::PageDown => KeyCode::PageDown,
        CtKey::F(n) => KeyCode::F(n),
        _ => return None,
    };
    Some(KeyPress::with(code, mods(m)))
}

fn button(b: CtButton) -> MouseButton {
    match b {
        CtButton::Left => MouseButton::Left,
        CtButton::Right => MouseButton::Right,
        CtButton::Middle => MouseButton::Middle,
    }
}

fn translate_mouse(m: event::MouseEvent) -> Option<Input> {
    let pos = Point::new(m.column as i32, m.row as i32);
    let mods = mods(m.modifiers);
    Some(match m.kind {
        MouseEventKind::Down(b) => Input::Press {
            pos,
            button: button(b),
            mods,
        },
        MouseEventKind::Up(b) => Input::Release {
            pos,
            button: button(b),
            mods,
        },
        MouseEventKind::Moved | MouseEventKind::Drag(_) => Input::Move { pos, mods },
        MouseEventKind::ScrollDown => Input::Wheel {
            pos,
            delta: 1,
            mods,
        },
        MouseEventKind::ScrollUp => Input::Wheel {
            pos,
            delta: -1,
            mods,
        },
        _ => return None,
    })
}

// ---------------------------------------------------------------------------
// The backend: fold a display list into coloured cells
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq)]
struct Cell {
    ch: char,
    fg: Color,
    bg: Color,
}

impl Default for Cell {
    fn default() -> Self {
        Cell {
            ch: ' ',
            fg: Color::Grey,
            bg: Color::Reset,
        }
    }
}

/// A terminal in raw mode with mouse capture, restored on drop so a panic
/// mid-frame does not leave the terminal wedged.
struct Terminal {
    out: io::Stdout,
    cells: Vec<Cell>,
    w: u16,
    h: u16,
}

impl Terminal {
    fn enter() -> io::Result<Self> {
        terminal::enable_raw_mode()?;
        let mut out = io::stdout();
        execute!(
            out,
            terminal::EnterAlternateScreen,
            EnableMouseCapture,
            cursor::Hide
        )?;
        let (w, h) = terminal::size()?;
        Ok(Terminal {
            out,
            cells: vec![Cell::default(); w as usize * h as usize],
            w,
            h,
        })
    }

    fn draw(&mut self, spec: &fresh_ui::LayoutSpec) -> io::Result<()> {
        let (w, h) = terminal::size()?;
        if (w, h) != (self.w, self.h) {
            self.w = w;
            self.h = h;
        }
        self.cells.clear();
        self.cells
            .resize(self.w as usize * self.h as usize, Cell::default());

        let frame = Rect::new(0, 0, self.w, self.h);
        for item in &spec.items {
            self.paint(item, frame);
        }
        self.flush(spec)
    }

    fn paint(&mut self, item: &fresh_ui::Item, frame: Rect) {
        let clip = item.clip.intersect(frame);
        let r = item.rect;
        let (fg, bg) = palette(&item.theme);
        match &item.draw {
            Draw::Fill => self.fill(r, ' ', fg, bg, clip),
            Draw::Scrim(Scrim::Opaque) => self.fill(frame, ' ', fg, Color::Black, frame),
            Draw::Scrim(Scrim::Dim) => self.dim(frame),
            Draw::Border => self.border(r, fg, bg, clip),
            Draw::Lines(lines) => {
                for (i, line) in lines.iter().enumerate() {
                    let y = r.y + i as i32;
                    for (j, ch) in line.chars().enumerate() {
                        self.put(r.x + j as i32, y, ch, fg, bg, clip);
                    }
                }
            }
            Draw::Scrollbar {
                offset,
                content,
                window,
            } => {
                let track = r.h.max(1);
                let thumb = ((*window as u32 * track as u32) / (*content).max(1)).max(1) as u16;
                let top = ((*offset * track as u32) / (*content).max(1)) as u16;
                for i in 0..track {
                    let ch = if i >= top && i < top + thumb {
                        '█'
                    } else {
                        '│'
                    };
                    self.put(r.x, r.y + i as i32, ch, fg, bg, clip);
                }
            }
            // A selection hint; nothing to draw in a terminal.
            Draw::Selectable => {}
            Draw::Host(_) => self.fill(r, '▒', fg, bg, clip),
        }
    }

    fn cell_mut(&mut self, x: i32, y: i32, clip: Rect) -> Option<&mut Cell> {
        if !clip.contains(Point::new(x, y)) {
            return None;
        }
        if x < 0 || y < 0 || x >= self.w as i32 || y >= self.h as i32 {
            return None;
        }
        let i = y as usize * self.w as usize + x as usize;
        self.cells.get_mut(i)
    }

    fn put(&mut self, x: i32, y: i32, ch: char, fg: Color, bg: Color, clip: Rect) {
        if let Some(c) = self.cell_mut(x, y, clip) {
            c.ch = ch;
            c.fg = fg;
            if bg != Color::Reset {
                c.bg = bg;
            }
        }
    }

    fn fill(&mut self, r: Rect, ch: char, fg: Color, bg: Color, clip: Rect) {
        for y in 0..r.h {
            for x in 0..r.w {
                self.put(r.x + x as i32, r.y + y as i32, ch, fg, bg, clip);
            }
        }
    }

    fn dim(&mut self, frame: Rect) {
        for y in 0..frame.h {
            for x in 0..frame.w {
                if let Some(c) = self.cell_mut(x as i32, y as i32, frame) {
                    c.bg = Color::DarkGrey;
                }
            }
        }
    }

    fn border(&mut self, r: Rect, fg: Color, bg: Color, clip: Rect) {
        if r.w < 2 || r.h < 2 {
            return;
        }
        let (l, t) = (r.x, r.y);
        let (right, bottom) = (r.right() - 1, r.bottom() - 1);
        for x in l..=right {
            self.put(x, t, '─', fg, bg, clip);
            self.put(x, bottom, '─', fg, bg, clip);
        }
        for y in t..=bottom {
            self.put(l, y, '│', fg, bg, clip);
            self.put(right, y, '│', fg, bg, clip);
        }
        self.put(l, t, '┌', fg, bg, clip);
        self.put(right, t, '┐', fg, bg, clip);
        self.put(l, bottom, '└', fg, bg, clip);
        self.put(right, bottom, '┘', fg, bg, clip);
    }

    fn flush(&mut self, spec: &fresh_ui::LayoutSpec) -> io::Result<()> {
        queue!(self.out, cursor::MoveTo(0, 0))?;
        let mut fg = Color::Reset;
        let mut bg = Color::Reset;
        queue!(self.out, ResetColor)?;
        for y in 0..self.h {
            queue!(self.out, cursor::MoveTo(0, y))?;
            for x in 0..self.w {
                let c = self.cells[y as usize * self.w as usize + x as usize];
                if c.fg != fg {
                    fg = c.fg;
                    queue!(self.out, SetForegroundColor(fg))?;
                }
                if c.bg != bg {
                    bg = c.bg;
                    queue!(self.out, SetBackgroundColor(bg))?;
                }
                queue!(self.out, Print(c.ch))?;
            }
        }
        queue!(self.out, ResetColor)?;
        if let Some(cur) = spec.cursor {
            if cur.visible {
                queue!(
                    self.out,
                    cursor::MoveTo(cur.pos.x.max(0) as u16, cur.pos.y.max(0) as u16),
                    cursor::Show
                )?;
            }
        } else {
            queue!(self.out, cursor::Hide)?;
        }
        self.out.flush()
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        let _ = execute!(
            self.out,
            cursor::Show,
            DisableMouseCapture,
            terminal::LeaveAlternateScreen
        );
        let _ = terminal::disable_raw_mode();
    }
}

/// Map a theme name — the library's only statement about appearance — to a
/// terminal colour. The library never interprets these; the backend does.
fn palette(theme: &ThemeKey) -> (Color, Color) {
    match theme.as_str() {
        "menubar" => (Color::Black, Color::Grey),
        "grip" => (Color::DarkGrey, Color::Reset),
        "sidebar" => (Color::White, Color::Reset),
        "list.row.selected" => (Color::Black, Color::Cyan),
        "button.focused" | "field.focused" | "toggle.focused" => (Color::Black, Color::Yellow),
        "button" | "field" | "toggle" => (Color::White, Color::DarkGrey),
        "accent" => (Color::Magenta, Color::Reset),
        "status" => (Color::Black, Color::Blue),
        "menu" | "dropdown" => (Color::White, Color::DarkGrey),
        "modal" => (Color::White, Color::DarkBlue),
        _ => (Color::Grey, Color::Reset),
    }
}
