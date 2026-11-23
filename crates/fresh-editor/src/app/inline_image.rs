//! Inline images: a plugin attaches a picture to a buffer and the pane shows
//! it, drawn with `ratatui-image` — Kitty, Sixel or iTerm2 graphics where the
//! terminal speaks them, coloured half blocks everywhere else (GUI, web, a
//! plain terminal, tests).
//!
//! The terminal is asked once, by the process that owns it: the editor itself
//! when it runs in the terminal ([`detect`] then [`set_local_caps`]), or the
//! client of a daemon, which sends the answer in its hello. A daemon uses the
//! caps of the client its frames are sized for.
//!
//! The pictures go out as cell contents, so ratatui's diff carries them like
//! text. Kitty is told about a picture only once (with the first frame that
//! draws it), so anything that wipes the terminal must call
//! [`InlineImages::invalidate`] to have it told again.

use std::collections::HashMap;
use std::sync::OnceLock;

use fresh_core::BufferId;
use image::DynamicImage;
use ratatui::buffer::Buffer;
use ratatui::layout::{Rect, Size};
use ratatui::widgets::Widget;
use ratatui_image::picker::{Picker, ProtocolType};
use ratatui_image::protocol::Protocol;
use ratatui_image::{FilterType, FontSize, Image, Resize};

use crate::server::protocol::{ImageCaps, ImageProtocol};

/// `FRESH_IMAGE_PROTOCOL=halfblocks|sixel|kitty|iterm2` skips the terminal
/// query and uses that protocol; `FRESH_IMAGE_CELL=WxH` gives the cell size in
/// pixels to go with it (default 10x20).
const PROTOCOL_ENV: &str = "FRESH_IMAGE_PROTOCOL";
const CELL_ENV: &str = "FRESH_IMAGE_CELL";

/// Ask the terminal on stdin/stdout what it can show, before anything else
/// reads stdin. A terminal that doesn't answer within a second gets half
/// blocks. The terminal is left in the mode it was in.
pub fn detect() -> ImageCaps {
    if let Ok(name) = std::env::var(PROTOCOL_ENV) {
        let protocol = match name.as_str() {
            "sixel" => ImageProtocol::Sixel,
            "kitty" => ImageProtocol::Kitty,
            "iterm2" => ImageProtocol::Iterm2,
            _ => ImageProtocol::Halfblocks,
        };
        let cell_px = std::env::var(CELL_ENV)
            .ok()
            .and_then(|s| {
                let (w, h) = s.split_once('x')?;
                Some((w.parse().ok()?, h.parse().ok()?))
            })
            .unwrap_or(ImageCaps::default().cell_px);
        return ImageCaps { protocol, cell_px };
    }
    use std::io::IsTerminal;
    if !std::io::stdin().is_terminal() || !std::io::stdout().is_terminal() {
        return ImageCaps::default();
    }
    let caps = query::ask();
    tracing::info!("inline images: {caps:?}");
    caps
}

/// The terminal query. `ratatui-image` has one (`Picker::from_query_stdio`),
/// but when the terminal doesn't answer it gives up while its reading thread
/// goes on: that thread eats the next keystroke and then puts back the
/// terminal mode it found, undoing the raw mode set meanwhile. This sends the
/// same query and reads the answer here, with a deadline.
#[cfg(unix)]
mod query {
    use super::{ImageCaps, ImageProtocol};
    use ratatui_image::picker::cap_parser::{Parser, QueryStdioOptions, Response};
    use ratatui_image::picker::ProtocolType;
    use std::io::{Read, Write};
    use std::os::fd::AsRawFd;
    use std::time::{Duration, Instant};

    const TIMEOUT: Duration = Duration::from_millis(1000);

    fn env_has(name: &str, part: &str) -> bool {
        std::env::var(name).is_ok_and(|v| v.contains(part))
    }

    pub(super) fn ask() -> ImageCaps {
        // Inside tmux the pictures would need its passthrough; not done yet.
        if std::env::var("TMUX").is_ok_and(|v| !v.is_empty()) || env_has("TERM", "tmux") {
            return ImageCaps::default();
        }
        let mut options = QueryStdioOptions {
            timeout: TIMEOUT,
            ..Default::default()
        };
        // As ratatui-image: their Kitty placeholders and Sixel are unreliable.
        if std::env::var("WEZTERM_EXECUTABLE").is_ok_and(|v| !v.is_empty())
            || std::env::var("KONSOLE_VERSION").is_ok_and(|v| !v.is_empty())
        {
            options.blacklist_protocols = vec![ProtocolType::Kitty, ProtocolType::Sixel];
        }
        let iterm2 = [
            "iTerm",
            "WezTerm",
            "mintty",
            "vscode",
            "Tabby",
            "Hyper",
            "rio",
            "Bobcat",
            "WarpTerminal",
        ]
        .iter()
        .any(|t| env_has("TERM_PROGRAM", t))
            || env_has("LC_TERMINAL", "iTerm");

        let raw_before = crossterm::terminal::is_raw_mode_enabled().unwrap_or(false);
        if !raw_before && crossterm::terminal::enable_raw_mode().is_err() {
            return ImageCaps::default();
        }
        let responses = exchange(&Parser::query(false, options));
        if !raw_before {
            let _ = crossterm::terminal::disable_raw_mode();
        }
        let Some(responses) = responses else {
            return ImageCaps::default();
        };

        let mut caps = ImageCaps::default();
        // Unless the answer says otherwise: the window's pixels over its cells.
        if let Ok(w) = crossterm::terminal::window_size() {
            if w.width > 0 && w.height > 0 && w.columns > 0 && w.rows > 0 {
                caps.cell_px = (w.width / w.columns, w.height / w.rows);
            }
        }
        let (mut kitty, mut sixel) = (false, false);
        for r in &responses {
            match r {
                Response::Kitty => kitty = true,
                Response::Sixel => sixel = true,
                Response::CellSize(Some((w, h))) if *w > 0 && *h > 0 => caps.cell_px = (*w, *h),
                _ => {}
            }
        }
        caps.protocol = if kitty {
            ImageProtocol::Kitty
        } else if sixel {
            ImageProtocol::Sixel
        } else if iterm2 {
            ImageProtocol::Iterm2
        } else {
            ImageProtocol::Halfblocks
        };
        caps
    }

    /// Write `query` and collect the answers up to the status report that
    /// ends them; None if the terminal is silent past the deadline.
    fn exchange(query: &str) -> Option<Vec<Response>> {
        let mut out = std::io::stdout();
        out.write_all(query.as_bytes()).ok()?;
        out.flush().ok()?;
        let stdin = std::io::stdin();
        let fd = stdin.as_raw_fd();
        let deadline = Instant::now() + TIMEOUT;
        let mut parser = Parser::new();
        let mut responses = Vec::new();
        let mut buf = [0u8; 256];
        loop {
            let left = deadline.checked_duration_since(Instant::now())?;
            let mut pfd = libc::pollfd {
                fd,
                events: libc::POLLIN,
                revents: 0,
            };
            // SAFETY: one valid pollfd, for the duration of the call.
            let ready = unsafe { libc::poll(&mut pfd, 1, left.as_millis() as libc::c_int) };
            if ready < 0 {
                if std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted {
                    continue;
                }
                return None;
            }
            if ready == 0 {
                return None;
            }
            let n = stdin.lock().read(&mut buf).ok()?;
            if n == 0 {
                return None;
            }
            for &b in &buf[..n] {
                for r in parser.push(char::from(b)) {
                    if r == Response::Status {
                        return Some(responses);
                    }
                    responses.push(r);
                }
            }
        }
    }
}

#[cfg(not(unix))]
mod query {
    use super::ImageCaps;
    /// Not asked yet: half blocks.
    pub(super) fn ask() -> ImageCaps {
        ImageCaps::default()
    }
}

static LOCAL_CAPS: OnceLock<ImageCaps> = OnceLock::new();

/// Record what the terminal this process draws to can do (an editor running
/// in its own terminal). Editors created afterwards start from it.
pub fn set_local_caps(caps: ImageCaps) {
    let _ = LOCAL_CAPS.set(caps);
}

fn picker_for(caps: ImageCaps) -> Picker {
    // The non-deprecated constructors either ask the terminal or fix half
    // blocks; a daemon knows the answer from its client instead.
    #[allow(deprecated)]
    let mut picker = Picker::from_fontsize(FontSize::from(caps.cell_px));
    picker.set_protocol_type(match caps.protocol {
        ImageProtocol::Halfblocks => ProtocolType::Halfblocks,
        ImageProtocol::Sixel => ProtocolType::Sixel,
        ImageProtocol::Kitty => ProtocolType::Kitty,
        ImageProtocol::Iterm2 => ProtocolType::Iterm2,
    });
    picker
}

/// The part of a picture a pane shows: `x`, `y`, `w`, `h` as fractions of the
/// picture's width and height. [`View::WHOLE`] is all of it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct View {
    x: f32,
    y: f32,
    w: f32,
    h: f32,
}

impl View {
    pub(crate) const WHOLE: View = View {
        x: 0.0,
        y: 0.0,
        w: 1.0,
        h: 1.0,
    };

    /// A view inside the picture: sizes in (0, 1], moved back inside when
    /// they stick out. Values that aren't numbers give the whole picture.
    pub(crate) fn new(x: f32, y: f32, w: f32, h: f32) -> View {
        if ![x, y, w, h].iter().all(|v| v.is_finite()) {
            return View::WHOLE;
        }
        let w = w.clamp(1e-3, 1.0);
        let h = h.clamp(1e-3, 1.0);
        View {
            x: x.clamp(0.0, 1.0 - w),
            y: y.clamp(0.0, 1.0 - h),
            w,
            h,
        }
    }

    fn is_whole(self) -> bool {
        self.w >= 1.0 && self.h >= 1.0
    }

    /// The pixels of `image` this view covers, widened (or heightened) about
    /// its centre to the pane's shape `aspect` (width / height) where the
    /// picture has more to show, so a zoomed picture fills the pane.
    fn crop(self, image: &DynamicImage, aspect: f32) -> DynamicImage {
        let (iw, ih) = (image.width() as f32, image.height() as f32);
        let (mut w, mut h) = (self.w * iw, self.h * ih);
        if aspect.is_finite() && aspect > 0.0 {
            if w / h < aspect {
                w = (h * aspect).min(iw);
            } else {
                h = (w / aspect).min(ih);
            }
        }
        let cx = (self.x + self.w / 2.0) * iw;
        let cy = (self.y + self.h / 2.0) * ih;
        let x = (cx - w / 2.0).clamp(0.0, iw - w);
        let y = (cy - h / 2.0).clamp(0.0, ih - h);
        let px = (x.round() as u32).min(image.width().saturating_sub(1));
        let py = (y.round() as u32).min(image.height().saturating_sub(1));
        let pw = (w.round() as u32).clamp(1, image.width() - px);
        let ph = (h.round() as u32).clamp(1, image.height() - py);
        image.crop_imm(px, py, pw, ph)
    }
}

struct BufferImage {
    image: DynamicImage,
    /// Enlarge a picture smaller than the pane (otherwise it is only shrunk).
    scale_up: bool,
    /// The part shown; anything less than the whole is enlarged to fill.
    view: View,
    /// The picture encoded for the pane size and view it was last drawn at.
    encoded: Option<(Size, View, Protocol)>,
}

/// The pictures attached to buffers, and how to draw them on this terminal.
pub(crate) struct InlineImages {
    caps: ImageCaps,
    picker: Picker,
    images: HashMap<BufferId, BufferImage>,
}

impl Default for InlineImages {
    fn default() -> Self {
        let caps = LOCAL_CAPS.get().copied().unwrap_or_default();
        Self {
            caps,
            picker: picker_for(caps),
            images: HashMap::new(),
        }
    }
}

impl InlineImages {
    /// Draw for a terminal with these caps (a daemon calls it every frame).
    pub(crate) fn set_caps(&mut self, caps: ImageCaps) {
        if caps != self.caps {
            self.caps = caps;
            self.picker = picker_for(caps);
            self.invalidate();
        }
    }

    /// What plugins see of the terminal (`getImageSupport()`).
    pub(crate) fn support(&self) -> fresh_core::api::ImageSupport {
        fresh_core::api::ImageSupport {
            protocol: match self.caps.protocol {
                ImageProtocol::Halfblocks => "halfblocks",
                ImageProtocol::Sixel => "sixel",
                ImageProtocol::Kitty => "kitty",
                ImageProtocol::Iterm2 => "iterm2",
            }
            .to_string(),
            cell_width: self.caps.cell_px.0,
            cell_height: self.caps.cell_px.1,
        }
    }

    /// Re-encode every picture on its next paint: the terminal lost them.
    pub(crate) fn invalidate(&mut self) {
        for img in self.images.values_mut() {
            img.encoded = None;
        }
    }

    /// Attach the picture encoded in `bytes` (a file's contents, read where
    /// the file lives) to `buffer`, replacing any other. The buffer keeps the
    /// view it had (a reload stays zoomed in).
    pub(crate) fn set(
        &mut self,
        buffer: BufferId,
        bytes: &[u8],
        scale_up: bool,
    ) -> Result<(), String> {
        let image = image::load_from_memory(bytes).map_err(|e| e.to_string())?;
        let view = self.images.get(&buffer).map_or(View::WHOLE, |i| i.view);
        self.images.insert(
            buffer,
            BufferImage {
                image,
                scale_up,
                view,
                encoded: None,
            },
        );
        Ok(())
    }

    /// Show only `view` of `buffer`'s picture. False if it has none.
    pub(crate) fn set_view(&mut self, buffer: BufferId, view: View) -> bool {
        match self.images.get_mut(&buffer) {
            Some(img) => {
                img.view = view;
                true
            }
            None => false,
        }
    }

    pub(crate) fn remove(&mut self, buffer: BufferId) {
        self.images.remove(&buffer);
    }

    /// Paint `buffer`'s picture, centred, over `rect`. Nothing if it has none.
    pub(crate) fn paint(&mut self, buffer: BufferId, rect: Rect, buf: &mut Buffer) {
        let Some(img) = self.images.get_mut(&buffer) else {
            return;
        };
        if rect.width == 0 || rect.height == 0 {
            return;
        }
        let size = rect.as_size();
        let view = img.view;
        if img.encoded.as_ref().map(|(s, v, _)| (*s, *v)) != Some((size, view)) {
            let (source, enlarge) = if view.is_whole() {
                (img.image.clone(), img.scale_up)
            } else {
                let (cw, ch) = self.caps.cell_px;
                let aspect = (f32::from(rect.width) * f32::from(cw))
                    / (f32::from(rect.height) * f32::from(ch));
                (view.crop(&img.image, aspect), true)
            };
            let resize = if enlarge {
                Resize::Scale(Some(FilterType::Triangle))
            } else {
                Resize::Fit(Some(FilterType::Triangle))
            };
            match self.picker.new_protocol(source, size, resize) {
                Ok(p) => img.encoded = Some((size, view, p)),
                Err(e) => {
                    tracing::warn!("inline image for {buffer:?}: {e}");
                    img.encoded = None;
                    return;
                }
            }
        }
        let Some((_, _, proto)) = img.encoded.as_ref() else {
            return;
        };
        let drawn = proto.size();
        let x = rect.x + rect.width.saturating_sub(drawn.width) / 2;
        let y = rect.y + rect.height.saturating_sub(drawn.height) / 2;
        let area = Rect::new(
            x,
            y,
            drawn.width.min(rect.width),
            drawn.height.min(rect.height),
        );
        Image::new(proto).render(area, buf);
    }
}
