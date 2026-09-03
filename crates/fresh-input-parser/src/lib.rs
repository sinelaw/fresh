//! Incremental terminal input (VT/ANSI) parser.
//!
//! Turns a raw byte stream from a terminal into [`Event`]s (crossterm's, plus
//! the keyboard-layout information a keymap needs — see [`KeyPress`]).
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

use std::ops::Deref;

use crossterm::event::{
    KeyCode, KeyEvent, KeyEventKind, KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
};

pub mod keypad;

/// A key press, as the terminal reported it, plus the character the key
/// actually types on the user's keyboard layout when the chord's own spelling
/// hides it.
///
/// A chord is described by two different things at once, and on a US layout
/// they happen to coincide so nothing forces the distinction:
///
/// * the **physical** key plus its modifiers — `Ctrl+Shift+7`; and
/// * the **character the key produces** in that state — `&` on US, `/` on
///   German (where `/` is Shift+7).
///
/// Terminals report both, and neither one alone is enough. The kitty keyboard
/// protocol sends `CSI 55:47;6u` for the German chord: base codepoint 55
/// (`7`), shifted codepoint 47 (`/`), modifiers Ctrl+Shift. Reporting only the
/// base means a German user pressing what is, to them, `Ctrl+/` gets
/// `Ctrl+Shift+7` — which `default.json` binds to `set_bookmark`, so the chord
/// silently set a bookmark instead of toggling a comment (sinelaw/fresh#2933).
/// Reporting only the shifted character is no better: it would break US
/// `Ctrl+Shift+<digit>` bindings, which are keyed on the digit.
///
/// So the parser reports the physical chord and hangs the layout character off
/// the side, and the *keymap* decides which reading wins — see
/// `Editor::handle_key_press`, which prefers the layout reading only when it is
/// actually bound. `layout_char` is `None` whenever the two readings agree,
/// which is every key on a US layout and most keys everywhere else.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct KeyPress {
    /// The chord as the terminal reported it: the physical key and its
    /// modifiers, with SHIFT still set.
    pub event: KeyEvent,
    /// The character the key types on the current layout, when that differs
    /// from `event.code`. The modifier that produced it (Shift) is spent on
    /// the character and must not be part of a chord built from it.
    pub layout_char: Option<char>,
}

impl KeyPress {
    /// A key press whose physical spelling is the only reading there is.
    pub fn new(event: KeyEvent) -> Self {
        Self {
            event,
            layout_char: None,
        }
    }

    /// A key press that also types `layout_char` on the current layout.
    pub fn with_layout_char(event: KeyEvent, layout_char: Option<char>) -> Self {
        Self { event, layout_char }
    }
}

impl From<KeyEvent> for KeyPress {
    fn from(event: KeyEvent) -> Self {
        Self::new(event)
    }
}

/// Reading a `KeyPress` as the key event it wraps keeps every existing
/// `press.code` / `press.modifiers` / `press.kind` call site working: the
/// layout character is extra information about the same press, not a different
/// kind of press.
impl Deref for KeyPress {
    type Target = KeyEvent;

    fn deref(&self) -> &KeyEvent {
        &self.event
    }
}

/// A terminal input event.
///
/// Mirrors [`crossterm::event::Event`] variant for variant, except that `Key`
/// carries a [`KeyPress`] rather than a bare `KeyEvent` so the layout character
/// survives the trip to the keymap. Convert with [`Event::into_crossterm`] at
/// any boundary that only speaks crossterm.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Event {
    FocusGained,
    FocusLost,
    Key(KeyPress),
    Mouse(MouseEvent),
    Paste(String),
    Resize(u16, u16),
}

impl Event {
    /// A key event with no separate layout character.
    pub fn key(event: KeyEvent) -> Self {
        Event::Key(KeyPress::new(event))
    }

    /// Drop the layout character and hand back a plain crossterm event.
    pub fn into_crossterm(self) -> crossterm::event::Event {
        use crossterm::event::Event as C;
        match self {
            Event::FocusGained => C::FocusGained,
            Event::FocusLost => C::FocusLost,
            Event::Key(press) => C::Key(press.event),
            Event::Mouse(mouse) => C::Mouse(mouse),
            Event::Paste(text) => C::Paste(text),
            Event::Resize(w, h) => C::Resize(w, h),
        }
    }
}

impl From<crossterm::event::Event> for Event {
    fn from(event: crossterm::event::Event) -> Self {
        use crossterm::event::Event as C;
        match event {
            C::FocusGained => Event::FocusGained,
            C::FocusLost => Event::FocusLost,
            C::Key(key) => Event::key(key),
            C::Mouse(mouse) => Event::Mouse(mouse),
            C::Paste(text) => Event::Paste(text),
            C::Resize(w, h) => Event::Resize(w, h),
        }
    }
}

/// Bracketed-paste end marker.
const PASTE_END: &[u8] = b"\x1b[201~";

/// Upper bound on the parameter/intermediate bytes buffered for a single CSI
/// sequence. Real sequences are far shorter; anything longer is treated as a
/// runaway/malformed sequence and dropped.
const MAX_CSI_PARAMS: usize = 128;

/// Upper bound on buffered bracketed-paste content. A paste-start (`ESC [ 200 ~`)
/// whose terminator never arrives would otherwise grow memory without limit and
/// swallow every subsequent keystroke. On reaching this bound the accumulated
/// content is emitted as a `Paste` event and the parser enters `PasteOverflow`,
/// discarding the runaway tail until an `ESC` lets it resync (so the tail is
/// neither buffered nor sprayed out as keystrokes). The bound is a pure safety
/// valve — far above any realistic interactive paste.
const MAX_PASTE: usize = 64 * 1024 * 1024;

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
    /// A bracketed paste that overran `MAX_PASTE`: its content has already been
    /// emitted, and the runaway tail is now discarded (not buffered, not
    /// emitted as keystrokes) until an `ESC` lets the machine resync.
    PasteOverflow,
    /// Inside a string-type control sequence — DCS (`ESC P`), OSC (`ESC ]`),
    /// APC (`ESC _`), PM (`ESC ^`) or SOS (`ESC X`). Content is discarded until
    /// the terminator: `ST` (`ESC \`) or a legacy `BEL`. `saw_esc` records that
    /// the previous byte was an `ESC`, so the next byte can complete an `ST`.
    ///
    /// `pending_alt` holds the introducer byte for as long as *nothing* has
    /// followed it — the window in which the two-byte prefix is still ambiguous
    /// between a real string sequence and a legacy-encoded Alt chord (a
    /// terminal without the kitty protocol transmits Alt+] as exactly `ESC ]`).
    /// The first body byte clears it: from then on this is unambiguously a
    /// string sequence and [`InputParser::flush`] must not tear it apart. See
    /// [`InputParser::escape_pending`].
    StringSeq {
        saw_esc: bool,
        pending_alt: Option<u8>,
    },
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

    /// True while an ambiguous escape prefix is buffered — bytes that read as
    /// the head of an escape sequence but equally as key press(es) a legacy
    /// terminal encodes with an `ESC` prefix. The next byte (or an idle
    /// [`flush`](InputParser::flush)) disambiguates. The ambiguous prefixes:
    ///
    /// * a lone `ESC` — the Escape key, or the head of any sequence;
    /// * `ESC [` with no parameter byte yet — Alt+[ on a legacy terminal, or a
    ///   CSI introducer (sinelaw/fresh#2930);
    /// * a string introducer (`ESC ]`, `ESC P`, `ESC _`, `ESC ^`, `ESC X`)
    ///   with no body byte yet — Alt+] (etc.) on a legacy terminal, or the
    ///   start of an OSC/DCS/APC/PM/SOS string (sinelaw/fresh#2930).
    ///
    /// A caller reading from a live tty uses this to decide when to
    /// [`flush`](InputParser::flush): a genuine control sequence's payload
    /// always follows its introducer immediately (same write burst, so at most
    /// one read-boundary/grace-window away), while a human Alt chord arrives
    /// alone. Once no more input arrives the ambiguity resolves to the key
    /// press. As soon as any payload byte follows, this returns `false` and
    /// the sequence is committed: it can then only complete or resync, never
    /// surface as keystrokes.
    pub fn escape_pending(&self) -> bool {
        match self.state {
            State::Escape => true,
            // `ESC [` alone: params live in `buffer` (cleared on entry), so an
            // empty buffer means nothing has followed the introducer yet.
            State::Csi => self.buffer.is_empty(),
            State::StringSeq {
                saw_esc: false,
                pending_alt: Some(_),
            } => true,
            _ => false,
        }
    }

    /// Resolve a buffered ambiguous escape prefix as the key press(es) a
    /// legacy terminal means by it: a lone `ESC` is the Escape key, `ESC [` is
    /// Alt+[, and a bare string introducer `ESC ]` / `ESC P` / `ESC _` /
    /// `ESC ^` / `ESC X` is Alt + that character (sinelaw/fresh#2930).
    ///
    /// Returns the resolved key event (and returns to ground) when
    /// [`escape_pending`](InputParser::escape_pending) is true; empty
    /// otherwise. Only the ambiguous prefixes are flushable — a CSI with
    /// parameters, a string sequence with body bytes, or a partial UTF-8
    /// character keeps waiting, since its bytes must never surface as literal
    /// keystrokes.
    pub fn flush(&mut self) -> Vec<Event> {
        let key = match self.state {
            State::Escape => KeyEvent::new(KeyCode::Esc, KeyModifiers::empty()),
            State::Csi if self.buffer.is_empty() => {
                KeyEvent::new(KeyCode::Char('['), KeyModifiers::ALT)
            }
            State::StringSeq {
                saw_esc: false,
                pending_alt: Some(introducer),
            } => KeyEvent::new(byte_to_keycode(introducer), KeyModifiers::ALT),
            _ => return Vec::new(),
        };
        self.state = State::Ground;
        vec![Event::key(key)]
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
                State::PasteOverflow => {
                    if !self.feed_paste_overflow(byte) {
                        continue;
                    }
                }
                State::StringSeq { saw_esc, .. } => {
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
            //
            // The introducer is remembered in `pending_alt` because the prefix
            // is still ambiguous until a body byte arrives: a legacy terminal
            // transmits Alt+] as exactly `ESC ]`, so a bare introducer with no
            // continuation must resolve to the Alt chord on idle `flush()`
            // rather than swallow all further input as string content
            // (sinelaw/fresh#2930).
            b'P' | b']' | b'_' | b'^' | b'X' => {
                self.state = State::StringSeq {
                    saw_esc: false,
                    pending_alt: Some(byte),
                };
            }
            0x1b => {
                // First ESC was standalone; stay in Escape for the second one.
                out.push(Event::key(KeyEvent::new(
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
                // Alt + key. The byte keeps whatever modifier it would carry
                // on its own, with Alt added: a C0 control byte after `ESC` is
                // Alt+Ctrl+<letter> (`ESC 0x10` = Alt+Ctrl+P), not Alt+<letter>.
                // Dropping the Control bit made the two indistinguishable, so
                // Alt+Ctrl+P silently ran the (bound) Alt+P command instead of
                // resolving to an unbound chord — sinelaw/fresh#2810.
                out.push(Event::key(KeyEvent::new(
                    byte_to_keycode(other),
                    c0_control_modifier(other) | KeyModifiers::ALT,
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
            //
            // Which keypad key each byte *is* is this encoding's own business;
            // what that key normalises to is `keypad`'s, so the codes come from
            // there rather than being spelled out a second time. A name that
            // did not resolve would take the key silently out of service, which
            // is what `ss3_application_keypad_forms` covers byte by byte.
            b'M' => crate::keypad::code_for_keysym("kp_enter"),
            b'E' => crate::keypad::code_for_keysym("kp_begin"),
            b'X' => crate::keypad::code_for_keysym("kp_equal"),
            b'j' => crate::keypad::code_for_keysym("kp_multiply"),
            b'k' => crate::keypad::code_for_keysym("kp_add"),
            b'l' => crate::keypad::code_for_keysym("kp_separator"),
            b'm' => crate::keypad::code_for_keysym("kp_subtract"),
            b'n' => crate::keypad::code_for_keysym("kp_decimal"),
            b'o' => crate::keypad::code_for_keysym("kp_divide"),
            // Keypad digits 0–9 (`ESC O p` … `ESC O y`).
            b'p'..=b'y' => crate::keypad::code_for_digit(byte - b'p'),
            _ => None,
        };
        if let Some(code) = keycode {
            out.push(Event::key(KeyEvent::new(code, KeyModifiers::empty())));
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
            // A report we can't represent yields no event, but its bytes are
            // still consumed here — they must never fall through to ground and
            // print as literal text (sinelaw/fresh#2745).
            if let Some(ev) = x10_mouse_event(buf) {
                out.push(ev);
            }
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
                    out.push(Event::key(KeyEvent::new(KeyCode::Char(c), modifiers)));
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
                0x1b => {
                    self.state = State::StringSeq {
                        saw_esc: true,
                        pending_alt: None,
                    }
                }
                _ => {
                    // Discard the content byte. Any body byte commits the
                    // prefix as a real string sequence: it is no longer
                    // flushable as an Alt chord.
                    self.state = State::StringSeq {
                        saw_esc: false,
                        pending_alt: None,
                    };
                }
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
            // Unterminated/oversized paste: emit what we have, then discard the
            // runaway tail (in `PasteOverflow`) until an `ESC` lets us resync.
            // Emitting here bounds memory; discarding rather than returning to
            // ground keeps the tail from being sprayed out as keystrokes.
            tracing::trace!("InputParser: paste exceeded {} bytes, flushing", MAX_PASTE);
            let text = sanitize_paste(&self.paste);
            self.paste.clear();
            self.state = State::PasteOverflow;
            out.push(Event::Paste(text));
        }
    }

    /// `PasteOverflow` state: discard the runaway tail of an over-long paste,
    /// emitting nothing, until an `ESC` arrives. Returns `false` on that `ESC`
    /// so it is reprocessed from ground and starts a clean new sequence.
    fn feed_paste_overflow(&mut self, byte: u8) -> bool {
        if byte == 0x1b {
            self.state = State::Ground;
            false
        } else {
            true // discard
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
            b'A' => out.push(csi_key(KeyCode::Up, &params)),
            b'B' => out.push(csi_key(KeyCode::Down, &params)),
            b'C' => out.push(csi_key(KeyCode::Right, &params)),
            b'D' => out.push(csi_key(KeyCode::Left, &params)),
            b'H' => out.push(csi_key(KeyCode::Home, &params)),
            b'F' => out.push(csi_key(KeyCode::End, &params)),
            // Keypad Begin (the center "5" key), `CSI E` / `CSI 1;<mod> E`.
            b'E' => out.push(csi_key(KeyCode::KeypadBegin, &params)),
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
                out.push(csi_key(code, &params));
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
            // Shift+Tab. The event type still rides in the modifier field for
            // emulators that send one here, so decode it the same way; the
            // modifiers are fixed because the key *is* the shifted Tab.
            b'Z' => out.push(Event::key(KeyEvent::new_with_kind(
                KeyCode::BackTab,
                KeyModifiers::SHIFT,
                kind_of(&params),
            ))),
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
                out.push(Event::key(KeyEvent::new(code, modifiers)));
            }
            return;
        }

        let num: u8 = parts.first().and_then(|s| s.parse().ok()).unwrap_or(0);
        let mods_field = parts.get(1).copied().unwrap_or("");
        let mods = first_subparam(mods_field).parse().unwrap_or(1);
        let modifiers = modifiers_from_param(mods);
        // As in `dispatch_csi_u`, the modifier field carries the kitty event
        // type as a sub-parameter (`CSI <num> ; <mods>:<event-type> ~`).
        let kind = event_kind_of(mods_field);

        // Bracketed paste start.
        if num == 200 {
            self.paste.clear();
            self.state = State::Paste;
            return;
        }
        // Stray paste-end outside paste mode: ignore gracefully.
        if num == 201 {
            out.push(Event::key(KeyEvent::new(
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
        out.push(Event::key(KeyEvent::new_with_kind(
            keycode, modifiers, kind,
        )));
    }

    /// Dispatch `CSI codepoint ; modifiers u` (fixterms / kitty keyboard).
    fn dispatch_csi_u(&mut self, params: &[u8], out: &mut Vec<Event>) {
        let params_str = std::str::from_utf8(params).unwrap_or("");
        let parts: Vec<&str> = params_str.split(';').collect();
        let key_field = parts.first().copied().unwrap_or("");
        let codepoint: u32 = first_subparam(key_field).parse().unwrap_or(0);
        let mods_field = parts.get(1).copied().unwrap_or("");
        let mods_param: u16 = first_subparam(mods_field).parse().unwrap_or(1);
        let mut modifiers = modifiers_from_param(mods_param);
        // The modifier field may carry an event-type sub-parameter
        // (`mods:event-type`): 1=press, 2=repeat, 3=release. Preserve it as the
        // key event's kind so a release is not reported as a fresh press (which
        // would fire every keystroke twice once event-type reporting is on).
        let kind = event_kind_of(mods_field);
        if let Some(mut code) = functional_or_char(codepoint) {
            // A shifted key must arrive as the character it types, exactly as
            // it does over the legacy encoding — the case (or symbol) carries
            // the shift, so the modifier bit goes with it.
            let mut layout_char = None;
            if let Some(shifted) = shifted_char(key_field, code, modifiers) {
                code = KeyCode::Char(shifted);
                modifiers.remove(KeyModifiers::SHIFT);
            } else {
                // The chord keeps its physical spelling, which on a non-US
                // layout can hide the character the user thinks they pressed.
                // Carry that character alongside so the keymap can consider it.
                layout_char = hidden_layout_char(key_field, code, modifiers);
            }
            out.push(Event::Key(KeyPress::with_layout_char(
                KeyEvent::new_with_kind(code, modifiers, kind),
                layout_char,
            )));
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

/// Build a `Key` event for a key that arrived in the *legacy* CSI form
/// (`CSI 1 ; <mods>:<event-type> {A,B,C,D,E,F,H,P,Q,R,S}`), decoding both the
/// modifiers and the event type from the modifier field.
///
/// Enabling the kitty protocol's event-type reporting does not move these keys
/// to the CSI-u form — they keep their legacy final byte and gain a
/// `:<event-type>` sub-parameter. Decoding only the modifiers here (and so
/// reporting every release as a press) is what made one arrow keypress move the
/// cursor twice (sinelaw/fresh#2796).
fn csi_key(code: KeyCode, params: &[u8]) -> Event {
    Event::key(KeyEvent::new_with_kind(
        code,
        modifiers_of(params),
        kind_of(params),
    ))
}

/// The primary value of a CSI parameter field, discarding any kitty
/// keyboard-protocol sub-parameters after a `:`. fresh enables
/// `DISAMBIGUATE_ESCAPE_CODES | REPORT_ALTERNATE_KEYS`, so a key field can
/// arrive as `unicode:shifted:base` and a modifier field as `mods:event-type`;
/// we key off the primary (base) value of each.
fn first_subparam(field: &str) -> &str {
    field.split(':').next().unwrap_or(field)
}

/// The character a shifted CSI-u key should be reported as, or `None` to keep
/// the key as its base codepoint plus the SHIFT modifier.
///
/// Over the legacy encoding a shifted key arrives as the character it types —
/// Shift+A is the byte `0x41`, Shift+2 is `0x40` — and nothing downstream ever
/// sees a SHIFT bit. Under `REPORT_ALL_KEYS_AS_ESCAPE_CODES` every printable
/// key moves to the CSI-u form instead, where the field is the *base* key
/// (`CSI 97:65;2u` for Shift+A: base 97, shifted 65). Reporting only the base
/// made every shifted key type its unshifted character — `A` inserted `a`, `@`
/// inserted `2` — which is what made the shift key look dead once the flag was
/// on (sinelaw/fresh#2880). Resolving the shifted character here keeps the two
/// encodings interchangeable, so keybindings, text insertion and the embedded
/// terminal's pty encoding all stay oblivious to which one the terminal speaks.
///
/// Two cases are deliberately left alone:
/// - **CONTROL held.** `Ctrl+Shift+A` must stay `Ctrl+Shift+a` so it resolves
///   to a `Ctrl+Shift+A` binding rather than collapsing onto `Ctrl+A` (which is
///   what uppercasing plus a dropped SHIFT would normalize to).
/// - **A shifted codepoint the terminal did not send** for anything but a
///   letter. Without `REPORT_ALTERNATE_KEYS` the shifted codepoint is absent,
///   and only the keyboard layout knows that Shift+2 is `@` — but Shift+letter
///   is the letter's uppercase on every Latin layout, so that much is safe to
///   derive. Non-letters keep their base plus SHIFT rather than guess.
fn shifted_char(key_field: &str, code: KeyCode, modifiers: KeyModifiers) -> Option<char> {
    if !modifiers.contains(KeyModifiers::SHIFT) || modifiers.contains(KeyModifiers::CONTROL) {
        return None;
    }
    let KeyCode::Char(base) = code else {
        return None;
    };
    match shifted_subparam(key_field) {
        // The terminal told us what the key types: take it, unless it is a
        // functional key's Private Use Area codepoint (never printable text).
        Some(codepoint) => match functional_or_char(codepoint)? {
            KeyCode::Char(shifted) => Some(shifted),
            _ => None,
        },
        None if modifiers == KeyModifiers::SHIFT => uppercase_letter(base),
        None => None,
    }
}

/// The character a key types on the current layout, when the chord's own
/// spelling does not say so — the [`KeyPress::layout_char`] of a CSI-u report.
///
/// This is the leftover of [`shifted_char`]: the terminal told us what the key
/// types, but the chord has to keep its base spelling anyway because CONTROL is
/// held (folding the shifted character in there would collapse `Ctrl+Shift+A`
/// onto `Ctrl+A`). On a US layout that discarded character is the one the
/// binding never wanted — `Ctrl+Shift+7` types `&`, and the binding is on the
/// digit. On a German layout it is the whole point: `Ctrl+Shift+7` types `/`,
/// and the user pressed it meaning `Ctrl+/`.
///
/// Both readings are therefore kept and the keymap picks; see [`KeyPress`].
/// `None` when the terminal reported no shifted codepoint (no
/// `REPORT_ALTERNATE_KEYS`, nothing to disagree about) or when it matches the
/// base, which is the US case for every letter.
fn hidden_layout_char(key_field: &str, code: KeyCode, modifiers: KeyModifiers) -> Option<char> {
    if !modifiers.contains(KeyModifiers::SHIFT) {
        return None;
    }
    let KeyCode::Char(base) = code else {
        return None;
    };
    match functional_or_char(shifted_subparam(key_field)?)? {
        // A letter's shifted codepoint is only its uppercase, which the chord's
        // own SHIFT bit already says — `ctrl+A` and `ctrl+shift+a` are one
        // binding. Reporting it would put every shifted keystroke through a
        // second keymap lookup that can only find the same thing.
        KeyCode::Char(shifted) if shifted != base && uppercase_letter(base) != Some(shifted) => {
            Some(shifted)
        }
        _ => None,
    }
}

/// The shifted codepoint of a kitty `unicode:shifted:base` key field, if the
/// terminal reported one. An empty sub-parameter (`97::29`, the way a key field
/// carries a base-layout code with no shifted code) is "not reported".
fn shifted_subparam(key_field: &str) -> Option<u32> {
    key_field.split(':').nth(1)?.parse().ok()
}

/// The single-character uppercase of a letter, or `None` if the key is not a
/// letter (`4`), is already uppercase, or uppercases to several characters
/// (`ß` → `SS`, which is not a keystroke).
fn uppercase_letter(base: char) -> Option<char> {
    if !base.is_alphabetic() {
        return None;
    }
    let mut upper = base.to_uppercase();
    match (upper.next(), upper.next()) {
        (Some(c), None) if c != base => Some(c),
        _ => None,
    }
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
        // The whole keypad, from the table `keypad` shares with the config
        // key-name parser — one definition, so a name that binds and a key
        // that arrives cannot drift apart.
        57399..=crate::keypad::KP_BEGIN => return crate::keypad::code_for_kitty_codepoint(cp),
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

/// Parse the event type out of the modifier field (`…;mods:event-type`) of a
/// standard CSI parameter list. Absent field or absent sub-parameter — every
/// sequence from a terminal that is not reporting event types — is a press.
fn kind_of(params: &[u8]) -> KeyEventKind {
    let params_str = std::str::from_utf8(params).unwrap_or("");
    match params_str.find(';') {
        Some(idx) => event_kind_of(&params_str[idx + 1..]),
        None => KeyEventKind::Press,
    }
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
    // `Cb` is a single byte in every mouse protocol, so anything non-numeric or
    // out of range is malformed. Defaulting it to 0 (as this used to) turns
    // garbage into a synthetic left-click at the reported cell.
    let cb: u8 = parts[0].parse().ok()?;
    let cx: u16 = parts[1].parse().unwrap_or(1);
    let cy: u16 = parts[2].parse().unwrap_or(1);

    // Bit 7 is xterm's high-button extension: the button number is
    // `(cb & 3) | (cb & 0xC0) >> 4`, so bit 7 marks buttons 8-15. `MouseButton`
    // has no representation for those, and their low bits alias onto
    // Left/Middle/Right — and, when bit 6 is set too (buttons 12-15), onto the
    // wheel. Decoding them would manufacture clicks and scrolls the user never
    // made, so drop them, as crossterm does.
    if cb & 0b1000_0000 != 0 {
        return None;
    }

    let button_bits = cb & 0b11;
    let button = match button_bits {
        0 => MouseButton::Left,
        1 => MouseButton::Middle,
        2 => MouseButton::Right,
        _ => MouseButton::Left, // 3 = no button; never used as a real button
    };

    let modifiers = mouse_modifiers(cb);

    let kind = if cb & 64 != 0 {
        match button_bits {
            0 => MouseEventKind::ScrollUp,
            1 => MouseEventKind::ScrollDown,
            2 => MouseEventKind::ScrollLeft,
            _ => MouseEventKind::ScrollRight,
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

/// Decode the Shift/Alt/Ctrl bits of a mouse report's `Cb` byte. The bit
/// layout is shared by the SGR and X10 encodings — only the transport differs.
fn mouse_modifiers(cb: u8) -> KeyModifiers {
    let mut modifiers = KeyModifiers::empty();
    if cb & 4 != 0 {
        modifiers |= KeyModifiers::SHIFT;
    }
    if cb & 8 != 0 {
        modifiers |= KeyModifiers::ALT;
    }
    if cb & 16 != 0 {
        modifiers |= KeyModifiers::CONTROL;
    }
    modifiers
}

/// Decode a legacy X10 mouse report from its three raw coordinate bytes.
///
/// The button byte carries the same `Cb` layout as an SGR report (button in
/// bits 0-1, Shift/Alt/Ctrl in bits 2-4, motion in bit 5, wheel in bit 6, high
/// buttons in bit 7); only the transport differs. Each field is a single byte
/// biased by 32, and there is no `m` release terminator — a release is
/// reported as button 3, so which button came up is simply not encoded.
///
/// Returns `None` for reports that have no `MouseEventKind` counterpart.
fn x10_mouse_event(buf: [u8; 3]) -> Option<Event> {
    let cb = buf[0].wrapping_sub(32);
    let cx = buf[1].wrapping_sub(32);
    let cy = buf[2].wrapping_sub(32);

    // Buttons 8-15 are unrepresentable — see `sgr_mouse_event`.
    if cb & 0b1000_0000 != 0 {
        return None;
    }

    let button_bits = cb & 0b11;
    let button = match button_bits {
        0 => MouseButton::Left,
        1 => MouseButton::Middle,
        2 => MouseButton::Right,
        _ => MouseButton::Left, // 3 = no button / release
    };

    let kind = if cb & 64 != 0 {
        // Wheel: buttons 4-7 are up, down, left and right.
        match button_bits {
            0 => MouseEventKind::ScrollUp,
            1 => MouseEventKind::ScrollDown,
            2 => MouseEventKind::ScrollLeft,
            _ => MouseEventKind::ScrollRight,
        }
    } else if cb & 32 != 0 {
        // Motion. With a button held this is a drag; button 3 means no button
        // is down, i.e. bare pointer movement under DECSET 1003. Reading the
        // whole class as a press produced a click flood on every mouse move —
        // the X10 half of the same bug `sgr_mouse_event` documents.
        if button_bits == 3 {
            MouseEventKind::Moved
        } else {
            MouseEventKind::Drag(button)
        }
    } else if button_bits == 3 {
        MouseEventKind::Up(MouseButton::Left)
    } else {
        MouseEventKind::Down(button)
    };

    Some(Event::Mouse(MouseEvent {
        kind,
        // X10 coordinates are 1-based, exactly like SGR's: the byte is
        // `coordinate + 32` with the top-left cell at 1,1. Dropping the `- 1`
        // (as this used to) placed every legacy-protocol click one cell down
        // and to the right of the cell the user aimed at. `saturating_sub`
        // rather than `- 1` for the same reason as in `sgr_mouse_event`: an
        // out-of-spec 0 coordinate must not underflow (sinelaw/fresh#2732).
        column: u16::from(cx).saturating_sub(1),
        row: u16::from(cy).saturating_sub(1),
        modifiers: mouse_modifiers(cb),
    }))
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

/// CONTROL for C0 control characters — except Tab, LF, CR and Esc, which are
/// their own keys and carry no modifier.
///
/// Shared by the ground-state mapping and the `ESC <byte>` (Alt+key) mapping so
/// both agree that `0x10` is Ctrl+P.
fn c0_control_modifier(byte: u8) -> KeyModifiers {
    if byte < 32 && byte != 9 && byte != 10 && byte != 13 && byte != 27 {
        KeyModifiers::CONTROL
    } else {
        KeyModifiers::empty()
    }
}

/// Convert a single ground-state byte to a key event, attaching CONTROL for
/// C0 control characters (except Tab, LF, CR and Esc, which are their own
/// keys).
fn byte_to_event(byte: u8) -> Event {
    Event::key(KeyEvent::new(
        byte_to_keycode(byte),
        c0_control_modifier(byte),
    ))
}

/// Convert a byte to a `KeyCode`.
fn byte_to_keycode(byte: u8) -> KeyCode {
    match byte {
        0 => KeyCode::Char('@'), // Ctrl+@
        9 => KeyCode::Tab,
        10 | 13 => KeyCode::Enter,                          // LF or CR
        1..=26 => KeyCode::Char((b'a' + byte - 1) as char), // Ctrl+A..Ctrl+Z
        27 => KeyCode::Esc,
        28..=30 => KeyCode::Char((b'\\' + byte - 28) as char), // Ctrl+\, Ctrl+], Ctrl+^
        // `US`. A legacy terminal sends 0x1F for Ctrl+/, Ctrl+7 and Ctrl+_
        // alike, so the parser has to pick one spelling for the chord. It picks
        // `/` because that is the key people actually press for it (no Shift
        // needed) and because it is what the same chord reports under the kitty
        // protocol (`CSI 47;5u`), so both kinds of terminal resolve to one
        // binding. Deriving this arithmetically with 0x1C–0x1E gave `Ctrl+_`,
        // which no keymap binds, so `ctrl+/` fired on kitty and nowhere else
        // (sinelaw/fresh#2933).
        31 => KeyCode::Char('/'),
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
