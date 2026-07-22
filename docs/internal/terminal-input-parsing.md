# Terminal Input Parsing

Purpose: explain how Fresh turns the raw byte stream arriving from a terminal
into `crossterm::event::Event`s — why Fresh parses those bytes itself instead of
letting crossterm do it, the state machine that does the parsing, the invariant
that machine exists to preserve, and the protocol coverage it still lacks.

Everything in §1–§4 is IMPLEMENTED. §5 records the xterm/kitty protocol coverage
the parser gained by closing an earlier gap register (derived from the xterm
control-sequence reference and the kitty keyboard protocol specification),
together with the few limitations that remain by design.

This doc covers the *byte stream → `Event`* stage only. What happens to an
`Event` afterwards — key translation, modal dispatch, keybinding resolution — is
[input-keybindings-actions.md](input-keybindings-actions.md).

---

## 1. Why Fresh parses its own input

Fresh still uses crossterm for terminal *output*: raw mode, the ratatui backend,
and the DECSET writes that enable mouse capture, bracketed paste, and keyboard
enhancements. Only the *input* side is Fresh's own, in the `fresh-input-parser`
crate.

The reason is structural, not stylistic. Fresh hosts embedded terminals, so
bytes that the parser fails to recognise do not merely produce a wrong keystroke
— they are forwarded verbatim into a focused child pty. A parser that recovers
from a malformed sequence by re-emitting its bytes as literal keystrokes will,
under a mouse-motion flood, spray escape-sequence fragments into whatever
program the user is running.

crossterm's Unix parser is built as *accumulate bytes, re-parse the whole buffer
after each one*. Three consequences of that design made it unusable for Fresh:

- **Escape resolution by read size.** A lone trailing `ESC` is ambiguous: the
  Escape key, or the head of a sequence. crossterm resolves it by asking whether
  the last `read()` filled its buffer exactly. When it didn't, the `ESC` is
  declared standalone — so a mouse report split at its own `ESC` boundary emits
  a phantom Escape, and the continuation arrives on the next read in ground
  state, where `[<35;…M` is just text.
- **Error recovery resumes mid-sequence.** On a parse failure the buffer is
  cleared at the point of failure and parsing resumes from ground, so the
  *remainder* of the abandoned sequence is interpreted as literal keys. The
  meta-prefixed arrow form (`ESC ESC [ A`, sent for Alt+Up) trips this with no
  malformed input at all.
- **Panics on hostile coordinates.** Several mouse and cursor-report paths do
  unchecked `- 1` on unsigned coordinates. A report of column 0, or a truncated
  coordinate byte, underflows — a panic in debug, a wrap to 65535 in release.
  Fresh had to wrap crossterm's parser in `catch_unwind` with a
  consecutive-panic budget to survive this.

These are the same defect class the DEC/ANSI state machine was designed to
prevent, which is what Fresh implements instead.

## 2. The invariant

> **Bytes consumed inside a control sequence are never emitted as text.**

A real terminal prints only from the ground state. Bytes that turn out to belong
to a malformed or unrecognised control sequence are *dropped* — the machine
returns to ground and discards them — never re-dispatched as keystrokes.
Everything in the parser's structure follows from holding this line, because
violating it is precisely how mouse reports leak into an embedded terminal.

## 3. The state machine

A [Paul Williams DEC/ANSI parser][williams]-shaped machine. Bytes are fed one at
a time; a transition may consume the byte or hand it back to be reprocessed from
ground, which is how the machine resynchronises without losing a byte that
legitimately starts a new sequence.

| State | Role | Exit |
|-------|------|------|
| Ground | Printable bytes become characters, C0 controls become key events; stray C1/continuation/invalid-lead bytes are dropped | `ESC` → Escape; UTF-8 lead byte → Utf8 |
| Escape | A lone `ESC`, awaiting disambiguation | `[` → Csi; `O` → Ss3; `P ] _ ^ X` → StringSeq; another `ESC` → emit Escape and stay; a UTF-8 lead byte → Utf8 (Alt); anything else → Alt+key |
| Csi | Accumulating parameter/intermediate bytes | Final byte → dispatch; unexpected control byte → drop to ground and reprocess |
| CsiIgnore | A malformed or over-long CSI being swallowed | Final byte → drop; control byte → resync from ground |
| Ss3 | After `ESC O`; the next byte is the final (cursor, F1–F4, application keypad) | Always → Ground |
| X10 | Collecting the three raw coordinate bytes of a legacy mouse report | Three bytes → mouse event; any byte `< 0x20` → abandon and reprocess |
| Utf8 | Accumulating a multi-byte character; each byte must be a continuation byte | Continuation byte fails eager check → abandon and reprocess; width reached → decode |
| Paste | Inside bracketed paste, accumulating content | End marker → `Paste` event; over `MAX_PASTE` bytes → flush and resync |
| StringSeq | Inside a DCS/OSC/APC/PM/SOS string, discarding content | `ST` (`ESC \`) or `BEL` → drop and return to ground; a non-`ST` `ESC` → resync from Escape |

Two resync rules do the heavy lifting on malformed input:

- **X10 mouse is a fixed-width collector with a validity floor.** Every
  coordinate byte is `value + 32`, so a byte below `0x20` cannot be a legitimate
  coordinate: the report was truncated. The machine abandons it, emitting
  nothing, and reprocesses that byte from ground — so an `ESC` arriving
  mid-report starts a clean new sequence instead of being eaten as a coordinate.
- **A malformed CSI drops to ground and is discarded.** An unexpected control
  byte mid-sequence, an unknown final byte, or a parameter run longer than the
  cap all end the sequence without emitting its bytes.

Coordinate arithmetic saturates rather than wrapping, and no buffer is indexed
without a length check, so no input can panic the parser. This is covered by
property tests: chunk-invariance (splitting a sequence at any byte boundary
yields the same events), no-panic over arbitrary byte streams, well-formed
round-trips at every split point, and — for the two invariants in §5 — that a
string-type sequence with any body is swallowed at every split, and that no PUA
codepoint ever becomes a character key.

### Resolving a standalone Escape

The machine holds a lone `ESC` in the Escape state until the next byte
disambiguates it. That is right for a byte stream and wrong for a live tty,
where a standalone Escape has no continuation and nothing would ever arrive to
resolve it — the key press produces no event, and the *next* key press is
consumed as the disambiguator and reported as Alt+key.

So the parser exposes the ambiguity rather than guessing at it: a caller can ask
whether an `ESC` is pending and explicitly flush it as an Escape key press. Only
the Escape state is flushable — a partial CSI or UTF-8 sequence keeps waiting,
because flushing those would break the §2 invariant.

The tty reader resolves it against the file descriptor: with an `ESC` pending it
polls once more for a continuation (a `read()` can land mid-sequence), and
flushes the Escape key press if none arrives within a short grace window well
below human key-repeat latency. Crucially the decision is time-based, not
read-size-based, so it cannot fire in the middle of a sequence that is still
streaming in.

## 4. The three input paths

All three feed the same parser, which is the point — a parsing defect fixed once
is fixed everywhere.

- **Unix host (standalone editor).** A tty reader owns stdin in raw mode and
  reads bytes directly. Because it bypasses crossterm's event source, it also
  installs its own `SIGWINCH` handler to synthesize resize events; focus and
  bracketed-paste events need no special handling because they arrive in the
  byte stream. It coalesces runs of mouse-move events down to the latest one so
  a motion flood costs one event per read batch.
- **Session server.** The client is deliberately ultra-light and forwards raw
  bytes; the server parses them per-client. Sequences split across socket reads
  are the normal case here, not an edge case.
- **Windows.** A dedicated reader thread drains the console buffer as fast as
  possible and hands VT byte batches to the parser. This path additionally has
  to strip mouse sequences that the Windows console corrupts by dropping their
  leading `ESC` — see the notes in the winterm crate.

## 5. Protocol coverage (closed gaps)

These behaviours were once the parser's known distance from the protocols it
consumes; they are now handled. Grouped by the blast radius they used to carry —
the first group could put bytes into a buffer or a child pty, the failure the
parser exists to prevent — each is covered by a test that fails without the fix.

### Was emitting sequence bytes as text

- **DCS / OSC / APC / PM / SOS string states.** The Escape state routes the
  string introducers (`P ] _ ^ X`) into a `StringSeq` state that discards the
  payload until `ST` (`ESC \`) or a legacy `BEL`. Previously every introducer
  but `[`/`O` fell through to the Alt+key arm and dumped its whole payload as
  literal characters — OSC 52 clipboard responses, OSC 10/11 colour queries,
  DECRQSS/XTGETTCAP/XTVERSION replies, and kitty graphics APC all hit this. A
  non-`ST` `ESC` inside a string resyncs a fresh sequence rather than leaking.
- **Kitty functional keys map out of the Private Use Area.** The CSI-u
  dispatcher resolves the PUA range (U+E000–U+F8FF) through a functional-key
  table — lock/menu keys, F13–F35, the full keypad, media keys, standalone
  modifier keys — and *drops* any unassigned PUA codepoint. Nothing in the PUA
  range can become a character key, so no invisible glyph reaches the buffer.
- **C1 bytes and stray UTF-8 continuation bytes are discarded**, not surfaced as
  `Null` keys. (`0x9B` is still declined as an 8-bit CSI, which is correct under
  UTF-8.)

### Was producing wrong or lost keys

- **Free mouse motion from JediTerm-based terminals reads as motion.** A
  no-button code (3) is always motion, never a button event — even with an `M`
  terminator and no motion bit, which is how those emulators report movement.
  The old decoder mapped it to a left button-down, flooding clicks on every move.
- **SGR reports tolerate a trailing separator** before the terminator; the
  decoder now reads the first three fields instead of requiring exactly three.
- **Kitty event types are honoured.** The modifier field's press/repeat/release
  sub-parameter becomes the key event's kind, so a release is no longer reported
  as a fresh press (which would double every keystroke with event reporting on).
- **Modifier decoding is complete.** Shift/Alt/Ctrl/Super/Hyper/Meta are all
  mapped, and the modifier field is parsed as `u16` so its maximum legal value
  (256) no longer overflows and fails closed. Caps Lock / Num Lock have no
  `KeyModifiers` equivalent and are ignored.
- **Application-keypad SS3 forms decode** — keypad Enter, Begin, and the
  digit/operator keys — so the numeric keypad works under application keypad
  mode. `CSI E` (keypad Begin) has a dispatch arm, and the modified-F1–F4 guard
  now reaches Hyper and Meta.
- **Alt + non-ASCII decodes correctly.** An `ESC`-prefixed UTF-8 lead byte routes
  into the character collector (carrying Alt) instead of being converted raw.
- **`?`/`>`-introduced replies are discarded** before dispatch (kitty
  keyboard-flags report, Device Attributes responses) instead of decoding as a
  NUL key.

### Robustness

- **The paste buffer is bounded.** On an unterminated or oversized paste (over
  `MAX_PASTE`), the accumulated content is flushed as a `Paste` event and the
  parser resyncs to ground, so it can neither grow without limit nor swallow
  every subsequent keystroke.
- **The UTF-8 collector validates continuation bytes eagerly** (the X10
  validity-floor pattern): a non-continuation byte abandons the partial
  character and is reprocessed from ground, so an `ESC` mid-character resyncs
  immediately. The lead-byte range is tightened to RFC 3629 (0xC2–0xF4).
- **Paste content is sanitised** of stray C0/C1 control bytes; tab, newline,
  carriage return and `ESC` are kept so multi-line and styled pastes survive.
  One limitation is inherent to bracketed paste and remains by design: the
  payload is delimited by the first `ESC [ 201 ~`, so a nested end marker still
  ends the paste early — the protocol offers no in-band way to prevent it.

---

## References

- [XTerm Control Sequences][ctlseqs] — the authoritative catalogue of CSI, SS3,
  DCS/OSC string types, mouse tracking modes, and `modifyOtherKeys`.
- [Comprehensive keyboard handling in terminals][kitty-kbd] — the kitty keyboard
  protocol: the progressive-enhancement flag stack, the primary and functional
  CSI-u formats, modifier arithmetic, and the PUA key registry.
- [A parser for DEC's ANSI-compatible video terminals][williams] — the state
  machine this parser is shaped after.

[ctlseqs]: https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
[kitty-kbd]: https://sw.kovidgoyal.net/kitty/keyboard-protocol/
[williams]: https://vt100.net/emu/dec_ansi_parser
