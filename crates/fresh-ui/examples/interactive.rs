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
use crossterm::style::{Color, Print, SetBackgroundColor, SetForegroundColor};
use crossterm::{cursor, execute, queue, terminal};

use fresh_ui::Axis;
use fresh_ui::{
    Draw, Input, KeyCode, KeyPress, Mods, MouseButton, Point, Rect, Scrim, Size, ThemeKey,
};

use support::demo::{App, Demo};

fn main() -> io::Result<()> {
    let mut term = Terminal::enter()?;
    let (w, h) = terminal::size()?;
    // A long list, so the task viewport overflows and shows a scrollbar.
    let mut demo = Demo::with_app(App::seeded(250), Size::new(w, h));
    let redraw = |term: &mut Terminal, demo: &Demo| -> io::Result<()> {
        term.draw(demo.ui.spec(), demo.app.theme.name == "dark")
    };
    redraw(&mut term, &demo)?;

    loop {
        if demo.app.quit {
            break;
        }
        // An input redraws; an idle wake redraws only when the library reports
        // that a frame would change something — a background task delivering,
        // a ticker running. When nothing is pending, the loop touches neither
        // the tree nor the terminal, so a still screen stays perfectly still.
        if event::poll(Duration::from_millis(100))? {
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
                _ => continue,
            }
            redraw(&mut term, &demo)?;
        } else if demo.ui.needs_frame() {
            demo.pump();
            redraw(&mut term, &demo)?;
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
            axis: Axis::Vertical,
            mods,
        },
        MouseEventKind::ScrollUp => Input::Wheel {
            pos,
            delta: -1,
            axis: Axis::Vertical,
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
            fg: Color::Reset,
            bg: Color::Reset,
        }
    }
}

/// A terminal in raw mode with mouse capture, restored on drop so a panic
/// mid-frame does not leave the terminal wedged.
struct Terminal {
    out: io::Stdout,
    cells: Vec<Cell>,
    /// Floating surfaces found this frame, so a soft shadow can be cast under
    /// each after the tree is painted.
    shadows: Vec<Rect>,
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
            shadows: Vec::new(),
            w,
            h,
        })
    }

    fn draw(&mut self, spec: &fresh_ui::LayoutSpec, dark: bool) -> io::Result<()> {
        let (w, h) = terminal::size()?;
        if (w, h) != (self.w, self.h) {
            self.w = w;
            self.h = h;
        }
        let r = roles(dark);
        let ground = Cell {
            ch: ' ',
            fg: c(r.text),
            bg: c(r.base),
        };
        self.cells.clear();
        self.cells.resize(self.w as usize * self.h as usize, ground);
        self.shadows.clear();

        let frame = Rect::new(0, 0, self.w, self.h);
        for item in &spec.items {
            self.paint(item, frame, &r);
        }
        self.cast_shadows(frame, &r);
        self.flush(spec)
    }

    fn paint(&mut self, item: &fresh_ui::Item, frame: Rect, roles: &Roles) {
        let clip = item.clip.intersect(frame);
        let r = item.rect;
        let (fg, bg) = style(&item.theme, roles);
        match &item.draw {
            Draw::Fill => {
                if elevated(&item.theme) {
                    // A floating surface: it casts a shadow onto whatever is
                    // behind it, drawn once the whole frame is painted.
                    self.shadows.push(r);
                }
                // The drag handle between the sidebar and the content is a
                // themed fill; a backend is free to draw it as a seam.
                let ch = if item.theme.as_str() == "grip" {
                    '│'
                } else {
                    ' '
                };
                self.fill(r, ch, fg, bg, clip);
            }
            Draw::Scrim(Scrim::Opaque) => {
                self.fill(frame, ' ', c(roles.text), c(roles.base), frame)
            }
            Draw::Scrim(Scrim::Dim) => self.dim(frame, roles),
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
                let _ = window;
                let track = r.h.max(1);
                let (top, len) = Draw::scrollbar_thumb(*offset, *content, track);
                for i in 0..track {
                    let ch = if i >= top && i < top + len {
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
        if let Some(cell) = self.cell_mut(x, y, clip) {
            cell.ch = ch;
            cell.fg = fg;
            cell.bg = bg;
        }
    }

    fn fill(&mut self, r: Rect, ch: char, fg: Color, bg: Color, clip: Rect) {
        for y in 0..r.h {
            for x in 0..r.w {
                self.put(r.x + x as i32, r.y + y as i32, ch, fg, bg, clip);
            }
        }
    }

    fn dim(&mut self, frame: Rect, roles: &Roles) {
        for y in 0..frame.h {
            for x in 0..frame.w {
                if let Some(cell) = self.cell_mut(x as i32, y as i32, frame) {
                    // Push the whole frame toward the background so the modal
                    // above it is what the eye lands on. The text underneath
                    // stays faintly legible rather than vanishing.
                    cell.fg = c(roles.scrim_fg);
                    cell.bg = c(roles.scrim_bg);
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
        self.put(l, t, '╭', fg, bg, clip);
        self.put(right, t, '╮', fg, bg, clip);
        self.put(l, bottom, '╰', fg, bg, clip);
        self.put(right, bottom, '╯', fg, bg, clip);
    }

    /// A soft shadow one cell to the right and below a floating surface: the
    /// underlying cells are darkened, which reads as depth without drawing any
    /// glyph of its own.
    fn cast_shadows(&mut self, frame: Rect, roles: &Roles) {
        let rects = std::mem::take(&mut self.shadows);
        for r in rects {
            for y in (r.y + 1)..=r.bottom() {
                self.darken(r.right(), y, frame, roles);
            }
            for x in (r.x + 1)..=r.right() {
                self.darken(x, r.bottom(), frame, roles);
            }
        }
    }

    fn darken(&mut self, x: i32, y: i32, frame: Rect, roles: &Roles) {
        if let Some(cell) = self.cell_mut(x, y, frame) {
            cell.fg = c(roles.shadow);
            cell.bg = c(roles.shadow_bg);
            cell.ch = ' ';
        }
    }

    fn flush(&mut self, spec: &fresh_ui::LayoutSpec) -> io::Result<()> {
        // Hide the cursor for the whole repaint so it is never seen racing
        // across the screen as the cells are written; it is restored at the
        // end, at the position the frame asked for.
        queue!(self.out, cursor::Hide, cursor::MoveTo(0, 0))?;
        let mut fg = Color::Reset;
        let mut bg = Color::Reset;
        for y in 0..self.h {
            queue!(self.out, cursor::MoveTo(0, y))?;
            for x in 0..self.w {
                let cell = self.cells[y as usize * self.w as usize + x as usize];
                if cell.fg != fg {
                    fg = cell.fg;
                    queue!(self.out, SetForegroundColor(fg))?;
                }
                if cell.bg != bg {
                    bg = cell.bg;
                    queue!(self.out, SetBackgroundColor(bg))?;
                }
                queue!(self.out, Print(cell.ch))?;
            }
        }
        if let Some(cur) = spec.cursor.filter(|c| c.visible) {
            queue!(
                self.out,
                cursor::MoveTo(cur.pos.x.max(0) as u16, cur.pos.y.max(0) as u16),
                cursor::Show
            )?;
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

// ---------------------------------------------------------------------------
// The palette: theme names, mapped to a cohesive scheme in two modes
// ---------------------------------------------------------------------------
//
// A theme name is the library's only statement about appearance; the backend
// decides what it looks like — including whether the scheme is dark or light.
// The demo's "Toggle theme" command flips `app.theme`, and the loop hands the
// mode down here, so the same tree repaints in either palette.

/// The colours each role takes, chosen once per mode.
struct Roles {
    base: u8,
    panel: u8,
    bar: u8,
    elev: u8,
    modal: u8,
    text: u8,
    bright: u8,
    dim: u8,
    faint: u8,
    title: u8,
    hover: u8,
    blur_bg: u8,
    sel_bg: u8,
    sel_fg: u8,
    focus_bg: u8,
    focus_fg: u8,
    status_bg: u8,
    status_fg: u8,
    shadow: u8,
    shadow_bg: u8,
    scrim_fg: u8,
    scrim_bg: u8,
}

fn roles(dark: bool) -> Roles {
    if dark {
        Roles {
            base: 234,
            panel: 236,
            bar: 238,
            elev: 234,
            modal: 236,
            text: 250,
            bright: 253,
            dim: 244,
            faint: 240,
            title: 110,
            hover: 238,
            blur_bg: 237,
            sel_bg: 30,
            sel_fg: 231,
            focus_bg: 179,
            focus_fg: 235,
            status_bg: 24,
            status_fg: 231,
            shadow: 236,
            shadow_bg: 232,
            scrim_fg: 240,
            scrim_bg: 233,
        }
    } else {
        Roles {
            base: 255,
            panel: 253,
            bar: 251,
            elev: 255,
            modal: 254,
            text: 238,
            bright: 232,
            dim: 245,
            faint: 250,
            title: 25,
            hover: 252,
            blur_bg: 252,
            sel_bg: 74,
            sel_fg: 232,
            focus_bg: 222,
            focus_fg: 234,
            status_bg: 25,
            status_fg: 231,
            shadow: 250,
            shadow_bg: 250,
            scrim_fg: 247,
            scrim_bg: 254,
        }
    }
}

fn c(v: u8) -> Color {
    Color::AnsiValue(v)
}

/// Whether a theme names a floating surface — one that sits above the content
/// and casts a shadow.
fn elevated(theme: &ThemeKey) -> bool {
    matches!(
        theme.as_str(),
        "menu" | "menu.file" | "menu.go" | "dropdown" | "palette" | "modal"
    )
}

/// The foreground and background a theme paints in, in the given mode.
fn style(theme: &ThemeKey, r: &Roles) -> (Color, Color) {
    let (fg, bg) = match theme.as_str() {
        "app" => (r.text, r.base),
        "menubar" | "menu.file" | "menu.go" => (r.bright, r.bar),
        "sidebar" => (r.text, r.panel),
        "sidebar.title" => (r.title, r.panel),
        "grip" | "divider" => (r.faint, r.panel),
        "list.row" => (r.text, r.base),
        "list.row.hover" => (r.bright, r.hover),
        "list.row.selected" => (r.sel_fg, r.sel_bg),
        "list.row.selected.blur" => (r.text, r.blur_bg),
        "field" | "button" | "toggle" | "number" => (r.text, r.bar),
        "button.hover" | "toggle.hover" | "number.hover" => (r.bright, r.hover),
        "field.focused" | "button.focused" | "toggle.focused" | "number.focused" => {
            (r.focus_fg, r.focus_bg)
        }
        "button.disabled" => (r.faint, r.bar),
        "status" => (r.status_fg, r.status_bg),
        "menu" | "dropdown" | "palette" => (r.dim, r.elev),
        "modal" => (r.bright, r.modal),
        _ => (r.text, r.base),
    };
    (c(fg), c(bg))
}
