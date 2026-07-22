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
    Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
};

/// Bracketed-paste end marker.
const PASTE_END: &[u8] = b"\x1b[201~";

/// Upper bound on the parameter/intermediate bytes buffered for a single CSI
/// sequence. Real sequences are far shorter; anything longer is treated as a
/// runaway/malformed sequence and dropped.
const MAX_CSI_PARAMS: usize = 128;

/// Upper bound on buffered bracketed-paste content. A paste-start (`ESC [ 200 ~`)
/// whose terminator never arrives would otherwise grow memory without limit and
/// swallow every subsequent keystroke. On reaching this bound the accumulated
/// content is emitted as a `Paste` event and the parser resyncs to ground. The
/// bound is far above any realistic interactive paste.
const MAX_PASTE: usize = 4 * 1024 * 1024;

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
    /// the total expected length. `alt` records that the character was
    /// introduced by an `ESC` prefix (Alt + the character).
    Utf8 { width: u8, alt: bool },
    /// Inside a bracketed paste (`ESC [ 200 ~` … `ESC [ 201 ~`); content
    /// accumulates in `self.paste`.
    Paste,
    /// Inside a string-type control sequence — DCS (`ESC P`), OSC (`ESC ]`),
    /// APC (`ESC _`), PM (`ESC ^`) or SOS (`ESC X`). Content is discarded until
    /// the terminator: `ST` (`ESC \`) or a legacy `BEL`. `saw_esc` records that
    /// the previous byte was an `ESC`, so the next byte can complete an `ST`.
    StringSeq { saw_esc: bool },
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
                State::Utf8 { .. } => {
                    if !self.feed_utf8(byte, out) {
                        continue;
                    }
                }
                State::Paste => self.feed_paste(byte, out),
                State::StringSeq { saw_esc } => {
                    if !self.feed_string(byte, saw_esc) {
                        continue;
                    }
                }
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
                    alt: false,
                };
            }
            // C1 controls, stray UTF-8 continuation bytes (0x80–0xBF), and
            // bytes that cannot begin a valid UTF-8 sequence (0xC0/0xC1,
            // 0xF5–0xFF) are noise on a UTF-8 input stream: discard them rather
            // than surfacing a `Null` key. (In particular this declines to
            // treat 0x9B as an 8-bit CSI, which is correct under UTF-8.)
            b if b >= 0x80 => {
                tracing::trace!("InputParser: discarding stray high byte {:#04x}", b);
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
            // String-type introducers: DCS (`P`), OSC (`]`), APC (`_`),
            // PM (`^`), SOS (`X`). These open a string whose content is
            // discarded until `ST`/`BEL`; without this arm the introducer and
            // its whole payload leaked out as `Alt+<introducer>` plus literal
            // characters (e.g. an OSC 52 clipboard or OSC 10/11 colour reply).
            b'P' | b']' | b'_' | b'^' | b'X' => {
                self.state = State::StringSeq { saw_esc: false };
            }
            0x1b => {
                // First ESC was standalone; stay in Escape for the second one.
                out.push(Event::Key(KeyEvent::new(
                    KeyCode::Esc,
                    KeyModifiers::empty(),
                )));
            }
            other if is_utf8_start_byte(other) => {
                // Alt + a multi-byte character: route the lead byte into the
                // UTF-8 collector with the Alt modifier pending, instead of
                // mangling it through `byte_to_keycode` (which only handles
                // ASCII and would report Alt+Null). e.g. Alt+é arrives as
                // `ESC 0xC3 0xA9` and must decode to Alt+'é'.
                self.buffer.clear();
                self.buffer.push(other);
                self.state = State::Utf8 {
                    width: utf8_char_width(other) as u8,
                    alt: true,
                };
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
            // Application-keypad forms (DECPAM). Without these the numeric
            // keypad went silent whenever application keypad mode was active.
            b'M' => Some(KeyCode::Enter),       // keypad Enter
            b'E' => Some(KeyCode::KeypadBegin), // keypad Begin (the "5" key)
            b'X' => Some(KeyCode::Char('=')),   // keypad Equal
            b'j' => Some(KeyCode::Char('*')),   // keypad Multiply
            b'k' => Some(KeyCode::Char('+')),   // keypad Add
            b'l' => Some(KeyCode::Char(',')),   // keypad Separator
            b'm' => Some(KeyCode::Char('-')),   // keypad Subtract
            b'n' => Some(KeyCode::Char('.')),   // keypad Decimal
            b'o' => Some(KeyCode::Char('/')),   // keypad Divide
            // Keypad digits 0–9 (`ESC O p` … `ESC O y`).
            b'p'..=b'y' => Some(KeyCode::Char((b'0' + (byte - b'p')) as char)),
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
    /// Returns `false` if `byte` must be reprocessed from ground.
    fn feed_utf8(&mut self, byte: u8, out: &mut Vec<Event>) -> bool {
        let (width, alt) = match self.state {
            State::Utf8 { width, alt } => (width as usize, alt),
            _ => unreachable!("feed_utf8 called outside Utf8 state"),
        };
        // Eager validation: every byte after the lead must be a UTF-8
        // continuation byte (0x80–0xBF). If it isn't, the character was
        // truncated — drop the partial character (it is noise, never a control
        // sequence) and reprocess this byte from ground (the X10 validity-floor
        // pattern), so an `ESC` landing mid-character starts a clean sequence
        // instead of being buffered and only noticed at decode time.
        if !(0x80..=0xbf).contains(&byte) {
            self.buffer.clear();
            self.state = State::Ground;
            return false;
        }
        self.buffer.push(byte);
        if self.buffer.len() < width {
            return true;
        }
        // We have `width` bytes; decode them.
        let modifiers = if alt {
            KeyModifiers::ALT
        } else {
            KeyModifiers::empty()
        };
        match std::str::from_utf8(&self.buffer) {
            Ok(s) => {
                if let Some(c) = s.chars().next() {
                    out.push(Event::Key(KeyEvent::new(KeyCode::Char(c), modifiers)));
                }
            }
            Err(_) => {
                // Width reached but still invalid (e.g. an overlong or
                // out-of-range encoding): drop the lead byte's worth and emit
                // nothing rather than a bogus key.
                tracing::trace!("InputParser: invalid UTF-8 sequence, dropping");
            }
        }
        self.buffer.clear();
        self.state = State::Ground;
        true
    }

    /// `StringSeq` state: discard the body of a DCS/OSC/APC/PM/SOS string until
    /// its terminator. Returns `false` if `byte` must be reprocessed from
    /// ground (an `ESC` that turned out to start a fresh sequence, not `ST`).
    fn feed_string(&mut self, byte: u8, saw_esc: bool) -> bool {
        if saw_esc {
            if byte == b'\\' {
                // `ST` (ESC \): string complete, discard it.
                self.state = State::Ground;
                true
            } else {
                // The `ESC` was not part of an `ST`: it begins a new escape
                // sequence. Resync through the Escape state, reprocessing this
                // byte as the disambiguator.
                self.state = State::Escape;
                false
            }
        } else {
            match byte {
                0x07 => self.state = State::Ground, // BEL: legacy OSC terminator
                0x1b => self.state = State::StringSeq { saw_esc: true },
                _ => {} // discard content byte
            }
            true
        }
    }

    /// `Paste` state: accumulate content until the end marker.
    fn feed_paste(&mut self, byte: u8, out: &mut Vec<Event>) {
        self.paste.push(byte);
        // Only the end marker's final byte (`~`) can complete the paste, so
        // guard the `ends_with` scan on it — otherwise every byte of a large
        // paste pays for a full marker comparison.
        if byte == b'~' && self.paste.ends_with(PASTE_END) {
            let content_len = self.paste.len() - PASTE_END.len();
            let text = sanitize_paste(&self.paste[..content_len]);
            self.paste.clear();
            self.state = State::Ground;
            out.push(Event::Paste(text));
            return;
        }
        if self.paste.len() >= MAX_PASTE {
            // Unterminated/oversized paste: emit what we have and resync to
            // ground, so the buffer is bounded and later keystrokes are not
            // swallowed indefinitely.
            tracing::trace!("InputParser: paste exceeded {} bytes, flushing", MAX_PASTE);
            let text = sanitize_paste(&self.paste);
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

        // A parameter list introduced by `?` or `>` is a device reply — a kitty
        // keyboard-flags report (`CSI ? <flags> u`), a Device Attributes
        // response, and so on — never a key press. Discard it before dispatch
        // rather than misdecoding it (the flags reply used to surface as a NUL
        // key). `<` is the SGR mouse introducer and is handled in the match.
        if matches!(params.first(), Some(b'?') | Some(b'>')) {
            tracing::trace!(
                "InputParser: discarding CSI reply, final {:#04x}",
                final_byte
            );
            return;
        }

        match final_byte {
            b'A' => out.push(key(KeyCode::Up, modifiers_of(&params))),
            b'B' => out.push(key(KeyCode::Down, modifiers_of(&params))),
            b'C' => out.push(key(KeyCode::Right, modifiers_of(&params))),
            b'D' => out.push(key(KeyCode::Left, modifiers_of(&params))),
            b'H' => out.push(key(KeyCode::Home, modifiers_of(&params))),
            b'F' => out.push(key(KeyCode::End, modifiers_of(&params))),
            // Keypad Begin (the center "5" key), `CSI E` / `CSI 1;<mod> E`.
            b'E' => out.push(key(KeyCode::KeypadBegin, modifiers_of(&params))),
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
            let mods_param: u16 = first_subparam(parts[1]).parse().unwrap_or(1);
            let codepoint: u32 = first_subparam(parts[2]).parse().unwrap_or(0);
            let modifiers = modifiers_from_param(mods_param);
            if let Some(code) = functional_or_char(codepoint) {
                out.push(Event::Key(KeyEvent::new(code, modifiers)));
            }
            return;
        }

        let num: u8 = parts.first().and_then(|s| s.parse().ok()).unwrap_or(0);
        let mods = parts
            .get(1)
            .and_then(|s| first_subparam(s).parse().ok())
            .unwrap_or(1);
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
        let mods_field = parts.get(1).copied().unwrap_or("");
        let mods_param: u16 = first_subparam(mods_field).parse().unwrap_or(1);
        let modifiers = modifiers_from_param(mods_param);
        // The modifier field may carry an event-type sub-parameter
        // (`mods:event-type`): 1=press, 2=repeat, 3=release. Preserve it as the
        // key event's kind so a release is not reported as a fresh press (which
        // would fire every keystroke twice once event-type reporting is on).
        let kind = event_kind_of(mods_field);
        if let Some(code) = functional_or_char(codepoint) {
            out.push(Event::Key(KeyEvent::new_with_kind(code, modifiers, kind)));
        }
    }
}

/// The event type encoded in the second sub-parameter of a kitty modifier field
/// (`mods:event-type`): 1=press (default), 2=repeat, 3=release.
fn event_kind_of(mods_field: &str) -> KeyEventKind {
    match mods_field
        .split(':')
        .nth(1)
        .and_then(|s| s.parse::<u8>().ok())
    {
        Some(2) => KeyEventKind::Repeat,
        Some(3) => KeyEventKind::Release,
        _ => KeyEventKind::Press,
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
///
/// The kitty keyboard protocol maps every non-printable key into the Unicode
/// Private Use Area (U+E000–U+F8FF). Those codepoints must never become
/// `Char` keys — that would insert an invisible PUA glyph into the buffer — so
/// the PUA range is resolved through [`kitty_functional_key`], which maps the
/// keys it knows and drops (returns `None` for) any other PUA codepoint.
fn functional_or_char(codepoint: u32) -> Option<KeyCode> {
    Some(match codepoint {
        9 => KeyCode::Tab,
        13 => KeyCode::Enter,
        27 => KeyCode::Esc,
        127 => KeyCode::Backspace,
        // Private Use Area: kitty functional keys, never printable characters.
        0xe000..=0xf8ff => return kitty_functional_key(codepoint),
        cp => KeyCode::Char(char::from_u32(cp)?),
    })
}

/// Map a kitty keyboard-protocol functional key (encoded as a Private Use Area
/// codepoint) to a crossterm [`KeyCode`]. Returns `None` for PUA codepoints the
/// protocol does not assign, so they are dropped rather than inserted as text.
fn kitty_functional_key(cp: u32) -> Option<KeyCode> {
    use crossterm::event::{MediaKeyCode as M, ModifierKeyCode as Mod};
    Some(match cp {
        57358 => KeyCode::CapsLock,
        57359 => KeyCode::ScrollLock,
        57360 => KeyCode::NumLock,
        57361 => KeyCode::PrintScreen,
        57362 => KeyCode::Pause,
        57363 => KeyCode::Menu,
        // F13–F35.
        57376..=57398 => KeyCode::F(13 + (cp - 57376) as u8),
        // Keypad digits 0–9.
        57399..=57408 => KeyCode::Char((b'0' + (cp - 57399) as u8) as char),
        57409 => KeyCode::Char('.'), // KP_DECIMAL
        57410 => KeyCode::Char('/'), // KP_DIVIDE
        57411 => KeyCode::Char('*'), // KP_MULTIPLY
        57412 => KeyCode::Char('-'), // KP_SUBTRACT
        57413 => KeyCode::Char('+'), // KP_ADD
        57414 => KeyCode::Enter,     // KP_ENTER
        57415 => KeyCode::Char('='), // KP_EQUAL
        57416 => KeyCode::Char(','), // KP_SEPARATOR
        57417 => KeyCode::Left,      // KP_LEFT
        57418 => KeyCode::Right,     // KP_RIGHT
        57419 => KeyCode::Up,        // KP_UP
        57420 => KeyCode::Down,      // KP_DOWN
        57421 => KeyCode::PageUp,    // KP_PAGE_UP
        57422 => KeyCode::PageDown,  // KP_PAGE_DOWN
        57423 => KeyCode::Home,      // KP_HOME
        57424 => KeyCode::End,       // KP_END
        57425 => KeyCode::Insert,    // KP_INSERT
        57426 => KeyCode::Delete,    // KP_DELETE
        57427 => KeyCode::KeypadBegin,
        // Media keys.
        57428 => KeyCode::Media(M::Play),
        57429 => KeyCode::Media(M::Pause),
        57430 => KeyCode::Media(M::PlayPause),
        57431 => KeyCode::Media(M::Reverse),
        57432 => KeyCode::Media(M::Stop),
        57433 => KeyCode::Media(M::FastForward),
        57434 => KeyCode::Media(M::Rewind),
        57435 => KeyCode::Media(M::TrackNext),
        57436 => KeyCode::Media(M::TrackPrevious),
        57437 => KeyCode::Media(M::Record),
        57438 => KeyCode::Media(M::LowerVolume),
        57439 => KeyCode::Media(M::RaiseVolume),
        57440 => KeyCode::Media(M::MuteVolume),
        // Standalone modifier keys.
        57441 => KeyCode::Modifier(Mod::LeftShift),
        57442 => KeyCode::Modifier(Mod::LeftControl),
        57443 => KeyCode::Modifier(Mod::LeftAlt),
        57444 => KeyCode::Modifier(Mod::LeftSuper),
        57445 => KeyCode::Modifier(Mod::LeftHyper),
        57446 => KeyCode::Modifier(Mod::LeftMeta),
        57447 => KeyCode::Modifier(Mod::RightShift),
        57448 => KeyCode::Modifier(Mod::RightControl),
        57449 => KeyCode::Modifier(Mod::RightAlt),
        57450 => KeyCode::Modifier(Mod::RightSuper),
        57451 => KeyCode::Modifier(Mod::RightHyper),
        57452 => KeyCode::Modifier(Mod::RightMeta),
        57453 => KeyCode::Modifier(Mod::IsoLevel3Shift),
        57454 => KeyCode::Modifier(Mod::IsoLevel5Shift),
        _ => return None,
    })
}

/// True when a CSI parameter list has the modified-function-key shape
/// `1 ; <mod>` that xterm uses for Shift/Ctrl/Alt + F1–F4
/// (`CSI 1;<mod> {P,Q,R,S}`), with `<mod>` a real modifier value (2–64).
///
/// The upper bound covers every combination of the six modifiers
/// (Shift/Alt/Ctrl/Super/Hyper/Meta → bitmask up to 63, parameter up to 64);
/// the previous cap of 16 dropped anything involving Hyper or Meta.
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
            .and_then(|f| first_subparam(f).parse::<u16>().ok()),
        Some(2..=64)
    )
}

/// Parse the modifier field (`…;mods`) of a standard CSI parameter list.
fn modifiers_of(params: &[u8]) -> KeyModifiers {
    let params_str = std::str::from_utf8(params).unwrap_or("");
    if let Some(idx) = params_str.find(';') {
        if let Ok(mods) = first_subparam(&params_str[idx + 1..]).parse::<u16>() {
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
    // The grammar is `Cb ; Cx ; Cy`, but some emulators append a trailing
    // separator before the terminator (`Cb ; Cx ; Cy ;`), which splits into a
    // trailing empty field. Accept three-or-more fields and read the first
    // three; fewer than three is genuinely malformed.
    if parts.len() < 3 {
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
        _ => MouseButton::Left, // 3 = no button; never used as a real button
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

    let kind = if cb & 64 != 0 {
        // Wheel: low bit selects up/down.
        if cb & 1 != 0 {
            MouseEventKind::ScrollDown
        } else {
            MouseEventKind::ScrollUp
        }
    } else if button_bits == 3 {
        // No button pressed. This is always motion, never a button event —
        // even with an `M` terminator and no motion bit, which is how
        // JediTerm-based emulators (that strip the motion bit) report free
        // mouse movement. Reading it as a left click produced a click flood on
        // every mouse move.
        MouseEventKind::Moved
    } else if cb & 32 != 0 {
        MouseEventKind::Drag(button)
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

/// Convert buffered bracketed-paste bytes to text, dropping stray C0/C1 control
/// characters that never belong in pasted text — they can corrupt the buffer,
/// and the bracketed-paste security note warns that embedded control bytes can
/// be used to smuggle commands. Tab, newline, carriage return and ESC are kept
/// so multi-line and styled (SGR-coloured) pastes survive intact.
fn sanitize_paste(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes)
        .chars()
        .filter(|&c| !c.is_control() || matches!(c, '\t' | '\n' | '\r' | '\u{1b}'))
        .collect()
}

/// Returns true if `b` is the leading byte of a UTF-8 multi-byte sequence.
/// Per RFC 3629 the valid lead-byte range is 0xC2–0xF4: 0xC0/0xC1 are overlong
/// two-byte leads, and 0xF5–0xFF would encode code points above U+10FFFF.
fn is_utf8_start_byte(b: u8) -> bool {
    matches!(b, 0xC2..=0xF4)
}

/// Total byte width of a UTF-8 sequence given its leading byte; 0 if invalid.
fn utf8_char_width(first_byte: u8) -> usize {
    match first_byte {
        0xC2..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF4 => 4,
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

/// Convert an xterm/kitty modifier parameter (1 + bitmask) to `KeyModifiers`.
///
/// The parameter is taken as `u16` because the kitty keyboard protocol's
/// maximum legal value is 256 (all of Shift/Alt/Ctrl/Super/Hyper/Meta plus
/// Caps Lock and Num Lock), which does not fit in a `u8` — parsing it as `u8`
/// overflowed and failed closed to "no modifiers". Caps Lock and Num Lock have
/// no `KeyModifiers` equivalent and are ignored.
fn modifiers_from_param(param: u16) -> KeyModifiers {
    let param = param.saturating_sub(1);
    let mut mods = KeyModifiers::empty();
    if param & 1 != 0 {
        mods |= KeyModifiers::SHIFT;
    }
    if param & 2 != 0 {
        mods |= KeyModifiers::ALT;
    }
    if param & 4 != 0 {
        mods |= KeyModifiers::CONTROL;
    }
    if param & 8 != 0 {
        mods |= KeyModifiers::SUPER;
    }
    if param & 16 != 0 {
        mods |= KeyModifiers::HYPER;
    }
    if param & 32 != 0 {
        mods |= KeyModifiers::META;
    }
    mods
}

#[cfg(test)]
mod proptests;
#[cfg(test)]
mod tests;
