//! Incremental terminal input (VT/ANSI) parser.
//!
//! Turns a raw byte stream from a terminal into [`crossterm::event::Event`]s.
//! The editor runs the whole UI server-side and keeps the client ultra-light,
//! so all input parsing happens here on a byte stream that arrives in
//! arbitrarily-sized chunks (a single escape sequence is regularly split
//! across two reads at a pty/socket boundary).
//!
//! # Design: a DEC/ANSI state machine
//!
//! This is a small [Paul Williams DEC/ANSI parser][williams]-style state
//! machine rather than a "push a byte, re-scan the whole buffer" heuristic.
//! The distinction matters for one invariant that a rescan parser gets wrong:
//!
//! > **Bytes consumed inside a control sequence are never emitted as text.**
//!
//! A real terminal only prints from the *ground* state. Bytes that turn out
//! to belong to a malformed or unrecognised control sequence are *dropped*
//! (the sequence transitions back to ground), never re-dispatched as literal
//! keystrokes. Violating this is exactly how mouse-tracking reports could leak
//! into a focused embedded terminal as garbage input when a sequence desynced
//! at a read boundary (sinelaw/fresh#2745).
//!
//! Two rules keep the machine synchronised on malformed input:
//!
//! * **X10 mouse (`ESC [ M`)** is a fixed-width collector of exactly three raw
//!   bytes. Each byte is `value + 32`, so every valid coordinate byte is
//!   `>= 0x20`. A byte `< 0x20` (any C0 control, and in particular `ESC`)
//!   can never be a legitimate coordinate, so it means the mouse report was
//!   truncated: the machine abandons the malformed report (emitting nothing)
//!   and *reprocesses* that byte from ground — so a following `ESC` starts a
//!   clean new sequence instead of being swallowed as a coordinate.
//! * **A malformed CSI** (an unexpected control byte mid-sequence, an unknown
//!   final byte, or an over-long parameter run) drops to ground and is
//!   discarded, never printed.
//!
//! [williams]: https://vt100.net/emu/dec_ansi_parser

use crossterm::event::{
    Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
};

/// Bracketed-paste end marker.
const PASTE_END: &[u8] = b"\x1b[201~";

/// Upper bound on the parameter/intermediate bytes buffered for a single CSI
/// sequence. Real sequences are far shorter; anything longer is treated as a
/// runaway/malformed sequence and dropped. Bracketed-paste *content* is not
/// subject to this — it accumulates separately and is unbounded.
const MAX_CSI_PARAMS: usize = 128;

/// The parser's current state. Mirrors the relevant sub-states of the DEC/ANSI
/// state machine; printable/UTF-8 handling lives in `Ground`/`Utf8`.
#[derive(Debug, Clone, PartialEq, Eq)]
enum State {
    /// Normal state: printable bytes become characters, control bytes become
    /// key events, `ESC` begins an escape sequence.
    Ground,
    /// Seen a lone `ESC`; waiting for the next byte to disambiguate between a
    /// standalone Escape, a CSI (`[`), an SS3 (`O`), or Alt+key.
    Escape,
    /// Inside a CSI sequence (after `ESC [`). Parameter and intermediate bytes
    /// accumulate in `self.buffer`.
    Csi,
    /// Inside an SS3 sequence (after `ESC O`); the next byte is the final.
    Ss3,
    /// A malformed/over-long CSI: swallow bytes until the final byte, emitting
    /// nothing, then return to ground.
    CsiIgnore,
    /// Collecting the three raw coordinate bytes of an X10 mouse report.
    X10 { buf: [u8; 3], have: u8 },
    /// Accumulating a multi-byte UTF-8 character in `self.buffer`; `width` is
    /// the total expected length.
    Utf8 { width: u8 },
    /// Inside a bracketed paste (`ESC [ 200 ~` … `ESC [ 201 ~`); content
    /// accumulates in `self.paste`.
    Paste,
}

/// Incremental terminal-input parser.
///
/// Feed it raw bytes with [`InputParser::parse`]; it returns whatever complete
/// events those bytes produced and retains any partial sequence internally
/// until the rest arrives.
#[derive(Debug)]
pub struct InputParser {
    state: State,
    /// Bytes of the in-progress escape/CSI sequence or partial UTF-8 char.
    buffer: Vec<u8>,
    /// Accumulated bracketed-paste content (including the trailing end marker
    /// until it is recognised and stripped).
    paste: Vec<u8>,
}

impl Default for InputParser {
    fn default() -> Self {
        Self::new()
    }
}

impl InputParser {
    pub fn new() -> Self {
        Self {
            state: State::Ground,
            buffer: Vec::with_capacity(32),
            paste: Vec::new(),
        }
    }

    /// Parse a chunk of raw bytes into complete events.
    ///
    /// Partial sequences are retained across calls, so splitting a sequence
    /// across chunks at any byte boundary yields the same events as delivering
    /// it whole. Calling with an empty slice is a no-op; a buffered `ESC` is
    /// only resolved by the next byte or by an explicit [`InputParser::flush`].
    pub fn parse(&mut self, bytes: &[u8]) -> Vec<Event> {
        let mut events = Vec::new();
        for &byte in bytes {
            self.feed(byte, &mut events);
        }
        events
    }

    /// True while a lone `ESC` is buffered, waiting for the next byte to say
    /// whether it was the Escape key or the head of an escape sequence.
    ///
    /// A caller reading from a live tty uses this to decide when to
    /// [`flush`](InputParser::flush): a standalone Escape has no continuation,
    /// so once no more input arrives the ambiguity resolves to the key press.
    pub fn escape_pending(&self) -> bool {
        self.state == State::Escape
    }

    /// Resolve a buffered lone `ESC` as a standalone Escape key press.
    ///
    /// Returns the `Esc` key event (and returns to ground) when
    /// [`escape_pending`](InputParser::escape_pending) is true; empty
    /// otherwise. Only the `Escape` state is flushable — a partial CSI or
    /// UTF-8 sequence keeps waiting, since its bytes must never surface as
    /// literal keystrokes.
    pub fn flush(&mut self) -> Vec<Event> {
        if !self.escape_pending() {
            return Vec::new();
        }
        self.state = State::Ground;
        vec![Event::Key(KeyEvent::new(
            KeyCode::Esc,
            KeyModifiers::empty(),
        ))]
    }

    /// Process a single byte, appending any completed events to `out`.
    ///
    /// Some transitions "reprocess" the current byte from ground (e.g. an `ESC`
    /// that arrives mid-mouse-report). That is expressed by looping: the state
    /// is reset and the same byte is fed again, at most a couple of times.
    fn feed(&mut self, byte: u8, out: &mut Vec<Event>) {
        loop {
            match self.state {
                State::Ground => {
                    if !self.feed_ground(byte, out) {
                        continue;
                    }
                }
                State::Escape => {
                    if !self.feed_escape(byte, out) {
                        continue;
                    }
                }
                State::Csi => {
                    if !self.feed_csi(byte, out) {
                        continue;
                    }
                }
                State::Ss3 => self.feed_ss3(byte, out),
                State::CsiIgnore => {
                    if !self.feed_csi_ignore(byte) {
                        continue;
                    }
                }
                State::X10 { .. } => {
                    if !self.feed_x10(byte, out) {
                        continue;
                    }
                }
                State::Utf8 { .. } => self.feed_utf8(byte, out),
                State::Paste => self.feed_paste(byte, out),
            }
            break;
        }
    }

    /// Ground state. Returns `false` if the caller should reprocess `byte`
    /// (never happens here, but keeps the control-flow uniform).
    fn feed_ground(&mut self, byte: u8, out: &mut Vec<Event>) -> bool {
        match byte {
            0x1b => self.state = State::Escape,
            b if is_utf8_start_byte(b) => {
                self.buffer.clear();
                self.buffer.push(b);
                self.state = State::Utf8 {
                    width: utf8_char_width(b) as u8,
                };
            }
            b => out.push(byte_to_event(b)),
        }
        true
    }

    /// `Escape` state: a lone `ESC` was seen, this byte disambiguates it.
    fn feed_escape(&mut self, byte: u8, out: &mut Vec<Event>) -> bool {
        match byte {
            b'[' => {
                self.buffer.clear();
                self.state = State::Csi;
            }
            b'O' => self.state = State::Ss3,
            0x1b => {
                // First ESC was standalone; stay in Escape for the second one.
                out.push(Event::Key(KeyEvent::new(
                    KeyCode::Esc,
                    KeyModifiers::empty(),
                )));
            }
            other => {
                // Alt + key.
                out.push(Event::Key(KeyEvent::new(
                    byte_to_keycode(other),
                    KeyModifiers::ALT,
                )));
                self.state = State::Ground;
            }
        }
        true
    }

    /// `Csi` state: accumulate parameters/intermediates, dispatch on final.
    /// Returns `false` if `byte` must be reprocessed from ground.
    fn feed_csi(&mut self, byte: u8, out: &mut Vec<Event>) -> bool {
        match byte {
            // Parameter (0x30–0x3F) and intermediate (0x20–0x2F) bytes.
            0x20..=0x3f => {
                if self.buffer.len() >= MAX_CSI_PARAMS {
                    tracing::trace!("InputParser: over-long CSI, dropping");
                    self.state = State::CsiIgnore;
                } else {
                    self.buffer.push(byte);
                }
                true
            }
            // Final byte.
            0x40..=0x7e => {
                self.dispatch_csi(byte, out);
                true
            }
            // Anything else (C0 controls incl. ESC, DEL, high bytes) is not
            // valid inside a CSI: abandon the malformed sequence without
            // emitting its bytes, then reprocess this byte from ground.
            _ => {
                tracing::trace!(
                    "InputParser: malformed CSI (byte {:#04x}), dropping {} param byte(s)",
                    byte,
                    self.buffer.len(),
                );
                self.buffer.clear();
                self.state = State::Ground;
                false
            }
        }
    }

    /// `CsiIgnore`: swallow a malformed/over-long CSI until its final byte.
    /// Returns `false` if `byte` must be reprocessed from ground.
    fn feed_csi_ignore(&mut self, byte: u8) -> bool {
        match byte {
            0x20..=0x3f => true, // keep ignoring params/intermediates
            0x40..=0x7e => {
                self.state = State::Ground; // final byte: drop, done
                true
            }
            _ => {
                // Control/ESC/high byte: resync from ground.
                self.state = State::Ground;
                false
            }
        }
    }

    /// `Ss3` state: the byte after `ESC O` is the final byte.
    fn feed_ss3(&mut self, byte: u8, out: &mut Vec<Event>) {
        let keycode = match byte {
            b'P' => Some(KeyCode::F(1)),
            b'Q' => Some(KeyCode::F(2)),
            b'R' => Some(KeyCode::F(3)),
            b'S' => Some(KeyCode::F(4)),
            b'A' => Some(KeyCode::Up),
            b'B' => Some(KeyCode::Down),
            b'C' => Some(KeyCode::Right),
            b'D' => Some(KeyCode::Left),
            b'H' => Some(KeyCode::Home),
            b'F' => Some(KeyCode::End),
            _ => None,
        };
        if let Some(code) = keycode {
            out.push(Event::Key(KeyEvent::new(code, KeyModifiers::empty())));
        } else {
            tracing::trace!("InputParser: unknown SS3 final {:#04x}, dropping", byte);
        }
        self.state = State::Ground;
    }

    /// `X10` state: collect exactly three raw coordinate bytes.
    /// Returns `false` if `byte` must be reprocessed from ground.
    fn feed_x10(&mut self, byte: u8, out: &mut Vec<Event>) -> bool {
        // Every X10 byte is `value + 32`, so a byte below 0x20 cannot be a
        // valid coordinate: the report was truncated. Abandon it (emit
        // nothing) and reprocess this byte — crucially, a following ESC then
        // starts a clean new sequence instead of being eaten as a coordinate.
        if byte < 0x20 {
            tracing::trace!(
                "InputParser: truncated X10 mouse report (byte {:#04x}), dropping",
                byte,
            );
            self.state = State::Ground;
            return false;
        }
        let State::X10 { mut buf, mut have } = self.state else {
            unreachable!("feed_x10 called outside X10 state");
        };
        buf[have as usize] = byte;
        have += 1;
        if have == 3 {
            out.push(x10_mouse_event(buf));
            self.state = State::Ground;
        } else {
            self.state = State::X10 { buf, have };
        }
        true
    }

    /// `Utf8` state: accumulate the remaining bytes of a multi-byte character.
    fn feed_utf8(&mut self, byte: u8, out: &mut Vec<Event>) {
        self.buffer.push(byte);
        let width = match self.state {
            State::Utf8 { width } => width as usize,
            _ => unreachable!("feed_utf8 called outside Utf8 state"),
        };
        if self.buffer.len() < width {
            return;
        }
        // We have `width` bytes; decode them.
        match std::str::from_utf8(&self.buffer) {
            Ok(s) => {
                if let Some(c) = s.chars().next() {
                    out.push(Event::Key(KeyEvent::new(
                        KeyCode::Char(c),
                        KeyModifiers::empty(),
                    )));
                }
                self.buffer.clear();
                self.state = State::Ground;
            }
            Err(_) => {
                // Invalid sequence: emit the lead byte as-is (genuine input,
                // not a control sequence, so recovering it as a key is correct)
                // and reprocess the remaining bytes from ground.
                let rest: Vec<u8> = self.buffer.split_off(1);
                let lead = self.buffer[0];
                self.buffer.clear();
                self.state = State::Ground;
                out.push(byte_to_event(lead));
                for b in rest {
                    self.feed(b, out);
                }
            }
        }
    }

    /// `Paste` state: accumulate content until the end marker.
    fn feed_paste(&mut self, byte: u8, out: &mut Vec<Event>) {
        self.paste.push(byte);
        if self.paste.ends_with(PASTE_END) {
            let content_len = self.paste.len() - PASTE_END.len();
            let text = String::from_utf8_lossy(&self.paste[..content_len]).into_owned();
            self.paste.clear();
            self.state = State::Ground;
            out.push(Event::Paste(text));
        }
    }

    /// Dispatch a complete CSI sequence given its final byte. `self.buffer`
    /// holds the parameter/intermediate bytes. Resets state to ground unless
    /// the sequence opens a sub-machine (X10 mouse collection or paste).
    fn dispatch_csi(&mut self, final_byte: u8, out: &mut Vec<Event>) {
        // Default: back to ground once handled.
        let params = std::mem::take(&mut self.buffer);
        self.state = State::Ground;

        match final_byte {
            b'A' => out.push(key(KeyCode::Up, modifiers_of(&params))),
            b'B' => out.push(key(KeyCode::Down, modifiers_of(&params))),
            b'C' => out.push(key(KeyCode::Right, modifiers_of(&params))),
            b'D' => out.push(key(KeyCode::Left, modifiers_of(&params))),
            b'H' => out.push(key(KeyCode::Home, modifiers_of(&params))),
            b'F' => out.push(key(KeyCode::End, modifiers_of(&params))),
            // Modified F1–F4: xterm's SS3-derived form `CSI 1 ; <mod> {P,Q,R,S}`
            // (e.g. Shift+F3 = `ESC [ 1 ; 2 R`). *Unmodified* F1–F4 arrive as
            // SS3 (`ESC O P/Q/R/S`) and are decoded in `feed_ss3`; adding a
            // modifier is what promotes the SS3 prefix to a CSI with a `1;<mod>`
            // parameter, so only the modified variants land here. Without these
            // arms every Shift/Ctrl/Alt + F1–F4 was silently dropped (#699:
            // Shift+F3 "Find Previous" did nothing).
            //
            // The `1;<mod>` guard also separates these from a Cursor Position
            // Report (`CSI <row> ; <col> R`). fresh never requests a host CPR on
            // this input path — CPR only exists inside the integrated-terminal
            // emulator — so a CPR can't legitimately arrive here; the guard is
            // belt-and-suspenders that keeps a bare/foreign `CSI …R` from being
            // misread as F3.
            b'P' | b'Q' | b'R' | b'S' if is_modified_f1_f4(&params) => {
                let code = match final_byte {
                    b'P' => KeyCode::F(1),
                    b'Q' => KeyCode::F(2),
                    b'R' => KeyCode::F(3),
                    _ => KeyCode::F(4), // b'S'
                };
                out.push(key(code, modifiers_of(&params)));
            }
            b'~' => self.dispatch_tilde(&params, out),
            b'M' | b'm' => {
                if params.first() == Some(&b'<') {
                    if let Some(ev) = sgr_mouse_event(&params, final_byte == b'M') {
                        out.push(ev);
                    } else {
                        tracing::trace!("InputParser: malformed SGR mouse, dropping");
                    }
                } else if params.is_empty() && final_byte == b'M' {
                    // Legacy X10 mouse: three raw coordinate bytes follow.
                    self.state = State::X10 {
                        buf: [0; 3],
                        have: 0,
                    };
                } else {
                    tracing::trace!("InputParser: unrecognised CSI {:?} M/m, dropping", params);
                }
            }
            b'Z' => out.push(key(KeyCode::BackTab, KeyModifiers::SHIFT)),
            b'I' => out.push(Event::FocusGained),
            b'O' => out.push(Event::FocusLost),
            b'u' => self.dispatch_csi_u(&params, out),
            other => {
                tracing::trace!("InputParser: unknown CSI final {:#04x}, dropping", other);
            }
        }
    }

    /// Dispatch `CSI … ~` sequences (function keys, editing keys, paste start).
    fn dispatch_tilde(&mut self, params: &[u8], out: &mut Vec<Event>) {
        let params_str = std::str::from_utf8(params).unwrap_or("");
        let parts: Vec<&str> = params_str.split(';').collect();

        // xterm modifyOtherKeys mode 2: CSI 27 ; modifier ; keycode ~
        if parts.len() == 3 && parts[0] == "27" {
            let mods_param: u8 = first_subparam(parts[1]).parse().unwrap_or(1);
            let codepoint: u32 = first_subparam(parts[2]).parse().unwrap_or(0);
            let modifiers = modifiers_from_param(mods_param);
            if let Some(code) = functional_or_char(codepoint) {
                out.push(Event::Key(KeyEvent::new(code, modifiers)));
            }
            return;
        }

        let num: u8 = parts.first().and_then(|s| s.parse().ok()).unwrap_or(0);
        let mods = parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(1);
        let modifiers = modifiers_from_param(mods);

        // Bracketed paste start.
        if num == 200 {
            self.paste.clear();
            self.state = State::Paste;
            return;
        }
        // Stray paste-end outside paste mode: ignore gracefully.
        if num == 201 {
            out.push(Event::Key(KeyEvent::new(
                KeyCode::Null,
                KeyModifiers::empty(),
            )));
            return;
        }

        let keycode = match num {
            1 => KeyCode::Home,
            2 => KeyCode::Insert,
            3 => KeyCode::Delete,
            4 => KeyCode::End,
            5 => KeyCode::PageUp,
            6 => KeyCode::PageDown,
            7 => KeyCode::Home,
            8 => KeyCode::End,
            11 => KeyCode::F(1),
            12 => KeyCode::F(2),
            13 => KeyCode::F(3),
            14 => KeyCode::F(4),
            15 => KeyCode::F(5),
            17 => KeyCode::F(6),
            18 => KeyCode::F(7),
            19 => KeyCode::F(8),
            20 => KeyCode::F(9),
            21 => KeyCode::F(10),
            23 => KeyCode::F(11),
            24 => KeyCode::F(12),
            _ => {
                tracing::trace!("InputParser: unknown CSI {} ~, dropping", num);
                return;
            }
        };
        out.push(Event::Key(KeyEvent::new(keycode, modifiers)));
    }

    /// Dispatch `CSI codepoint ; modifiers u` (fixterms / kitty keyboard).
    fn dispatch_csi_u(&mut self, params: &[u8], out: &mut Vec<Event>) {
        let params_str = std::str::from_utf8(params).unwrap_or("");
        let parts: Vec<&str> = params_str.split(';').collect();
        let codepoint: u32 = parts
            .first()
            .and_then(|s| first_subparam(s).parse().ok())
            .unwrap_or(0);
        let mods_param: u8 = parts
            .get(1)
            .and_then(|s| first_subparam(s).parse().ok())
            .unwrap_or(1);
        let modifiers = modifiers_from_param(mods_param);
        if let Some(code) = functional_or_char(codepoint) {
            out.push(Event::Key(KeyEvent::new(code, modifiers)));
        }
    }
}

/// Build a `Key` event.
fn key(code: KeyCode, modifiers: KeyModifiers) -> Event {
    Event::Key(KeyEvent::new(code, modifiers))
}

/// The primary value of a CSI parameter field, discarding any kitty
/// keyboard-protocol sub-parameters after a `:`. fresh enables
/// `DISAMBIGUATE_ESCAPE_CODES | REPORT_ALTERNATE_KEYS`, so a key field can
/// arrive as `unicode:shifted:base` and a modifier field as `mods:event-type`;
/// we key off the primary (base) value of each.
fn first_subparam(field: &str) -> &str {
    field.split(':').next().unwrap_or(field)
}

/// Map a Unicode codepoint from CSI-u / modifyOtherKeys to a key code.
fn functional_or_char(codepoint: u32) -> Option<KeyCode> {
    Some(match codepoint {
        9 => KeyCode::Tab,
        13 => KeyCode::Enter,
        27 => KeyCode::Esc,
        127 => KeyCode::Backspace,
        cp => KeyCode::Char(char::from_u32(cp)?),
    })
}

/// True when a CSI parameter list has the modified-function-key shape
/// `1 ; <mod>` that xterm uses for Shift/Ctrl/Alt + F1–F4
/// (`CSI 1;<mod> {P,Q,R,S}`), with `<mod>` a real modifier value (2–16).
///
/// The leading `1` is the F1–F4 selector, not a coordinate, so this
/// distinguishes those keys from a Cursor Position Report
/// (`CSI <row>;<col> R`) and rejects the unmodified/`1;1` and bare `CSI R`
/// forms (unmodified F1–F4 come through SS3, never here).
fn is_modified_f1_f4(params: &[u8]) -> bool {
    let s = std::str::from_utf8(params).unwrap_or("");
    let mut fields = s.split(';');
    if fields.next() != Some("1") {
        return false;
    }
    matches!(
        fields
            .next()
            .and_then(|f| first_subparam(f).parse::<u8>().ok()),
        Some(2..=16)
    )
}

/// Parse the modifier field (`…;mods`) of a standard CSI parameter list.
fn modifiers_of(params: &[u8]) -> KeyModifiers {
    let params_str = std::str::from_utf8(params).unwrap_or("");
    if let Some(idx) = params_str.find(';') {
        if let Ok(mods) = first_subparam(&params_str[idx + 1..]).parse::<u8>() {
            return modifiers_from_param(mods);
        }
    }
    KeyModifiers::empty()
}

/// Decode an SGR mouse report body (`< Cb ; Cx ; Cy`) with the given press
/// state (`M` = press/motion, `m` = release). Returns `None` if malformed.
fn sgr_mouse_event(params: &[u8], pressed: bool) -> Option<Event> {
    // Skip the leading '<'.
    let params_str = std::str::from_utf8(params.get(1..)?).ok()?;
    let parts: Vec<&str> = params_str.split(';').collect();
    if parts.len() != 3 {
        return None;
    }
    let cb: u16 = parts[0].parse().unwrap_or(0);
    let cx: u16 = parts[1].parse().unwrap_or(1);
    let cy: u16 = parts[2].parse().unwrap_or(1);

    let button_bits = cb & 0b11;
    let button = match button_bits {
        0 => MouseButton::Left,
        1 => MouseButton::Middle,
        2 => MouseButton::Right,
        _ => MouseButton::Left, // 3 = no button (motion)
    };

    let modifiers = KeyModifiers::from_bits_truncate(
        if cb & 4 != 0 {
            KeyModifiers::SHIFT.bits()
        } else {
            0
        } | if cb & 8 != 0 {
            KeyModifiers::ALT.bits()
        } else {
            0
        } | if cb & 16 != 0 {
            KeyModifiers::CONTROL.bits()
        } else {
            0
        },
    );

    let kind = if cb & 32 != 0 {
        if cb & 64 != 0 {
            if cb & 1 != 0 {
                MouseEventKind::ScrollDown
            } else {
                MouseEventKind::ScrollUp
            }
        } else if button_bits == 3 {
            MouseEventKind::Moved
        } else {
            MouseEventKind::Drag(button)
        }
    } else if cb & 64 != 0 {
        if cb & 1 != 0 {
            MouseEventKind::ScrollDown
        } else {
            MouseEventKind::ScrollUp
        }
    } else if pressed {
        MouseEventKind::Down(button)
    } else {
        MouseEventKind::Up(button)
    };

    Some(Event::Mouse(MouseEvent {
        kind,
        column: cx.saturating_sub(1),
        row: cy.saturating_sub(1),
        modifiers,
    }))
}

/// Decode a legacy X10 mouse report from its three raw coordinate bytes.
fn x10_mouse_event(buf: [u8; 3]) -> Event {
    let cb = buf[0].wrapping_sub(32);
    let cx = buf[1].wrapping_sub(32);
    let cy = buf[2].wrapping_sub(32);
    let kind = match cb & 0b11 {
        0 => MouseEventKind::Down(MouseButton::Left),
        1 => MouseEventKind::Down(MouseButton::Middle),
        2 => MouseEventKind::Down(MouseButton::Right),
        _ => MouseEventKind::Up(MouseButton::Left), // 3 = release
    };
    Event::Mouse(MouseEvent {
        kind,
        column: cx as u16,
        row: cy as u16,
        modifiers: KeyModifiers::empty(),
    })
}

/// Returns true if `b` is the leading byte of a UTF-8 multi-byte sequence.
/// 0xC0 and 0xC1 are excluded per RFC 3629 (overlong encodings).
fn is_utf8_start_byte(b: u8) -> bool {
    matches!(b, 0xC2..=0xF7)
}

/// Total byte width of a UTF-8 sequence given its leading byte; 0 if invalid.
fn utf8_char_width(first_byte: u8) -> usize {
    match first_byte {
        0xC2..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF7 => 4,
        _ => 0,
    }
}

/// Convert a single ground-state byte to a key event, attaching CONTROL for
/// C0 control characters (except Tab, LF, CR and Esc, which are their own
/// keys).
fn byte_to_event(byte: u8) -> Event {
    let keycode = byte_to_keycode(byte);
    let modifiers = if byte < 32 && byte != 9 && byte != 10 && byte != 13 && byte != 27 {
        KeyModifiers::CONTROL
    } else {
        KeyModifiers::empty()
    };
    Event::Key(KeyEvent::new(keycode, modifiers))
}

/// Convert a byte to a `KeyCode`.
fn byte_to_keycode(byte: u8) -> KeyCode {
    match byte {
        0 => KeyCode::Char('@'), // Ctrl+@
        9 => KeyCode::Tab,
        10 | 13 => KeyCode::Enter,                          // LF or CR
        1..=26 => KeyCode::Char((b'a' + byte - 1) as char), // Ctrl+A..Ctrl+Z
        27 => KeyCode::Esc,
        28..=31 => KeyCode::Char((b'\\' + byte - 28) as char),
        32 => KeyCode::Char(' '),
        127 => KeyCode::Backspace,
        b if (32..127).contains(&b) => KeyCode::Char(b as char),
        _ => KeyCode::Null,
    }
}

/// Convert an xterm modifier parameter (1 + bitmask) to `KeyModifiers`.
fn modifiers_from_param(param: u8) -> KeyModifiers {
    let param = param.saturating_sub(1);
    KeyModifiers::from_bits_truncate(
        if param & 1 != 0 {
            KeyModifiers::SHIFT.bits()
        } else {
            0
        } | if param & 2 != 0 {
            KeyModifiers::ALT.bits()
        } else {
            0
        } | if param & 4 != 0 {
            KeyModifiers::CONTROL.bits()
        } else {
            0
        },
    )
}

#[cfg(test)]
mod proptests;
#[cfg(test)]
mod tests;
