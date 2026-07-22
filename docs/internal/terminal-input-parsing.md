# Terminal Input Parsing

Purpose: explain how Fresh turns the raw byte stream arriving from a terminal
into `crossterm::event::Event`s — why Fresh parses those bytes itself instead of
letting crossterm do it, the state machine that does the parsing, the invariant
that machine exists to preserve, and the protocol coverage it still lacks.

Everything in §1–§4 is IMPLEMENTED. §5 is a gap register: protocol behaviour the
parser does not handle today, derived from the xterm control-sequence reference
and the kitty keyboard protocol specification. Gaps are not bugs filed against a
plan — they are the known distance between the parser and the protocols it
consumes.

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
| Ground | Printable bytes become characters, C0 controls become key events | `ESC` → Escape; UTF-8 lead byte → Utf8 |
| Escape | A lone `ESC`, awaiting disambiguation | `[` → Csi; `O` → Ss3; another `ESC` → emit Escape and stay; anything else → Alt+key |
| Csi | Accumulating parameter/intermediate bytes | Final byte → dispatch; unexpected control byte → drop to ground and reprocess |
| CsiIgnore | A malformed or over-long CSI being swallowed | Final byte → drop; control byte → resync from ground |
| Ss3 | After `ESC O`; the next byte is the final | Always → Ground |
| X10 | Collecting the three raw coordinate bytes of a legacy mouse report | Three bytes → mouse event; any byte `< 0x20` → abandon and reprocess |
| Utf8 | Accumulating a multi-byte character | Width reached → decode |
| Paste | Inside bracketed paste, accumulating content | End marker → `Paste` event |

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
yields the same events), no-panic over arbitrary byte streams, and well-formed
round-trips at every split point.

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

## 5. Gap register

Known distance between the parser and the protocols it consumes. Ordered by
blast radius: the first group can put bytes into a buffer or a child pty, which
is the failure the parser exists to prevent.

### Emits sequence bytes as text

- **No DCS / OSC / APC / PM / SOS string states.** The Escape state understands
  `[`, `O`, and `ESC`; every other introducer falls through to the Alt+key arm,
  which emits `Alt+<introducer>` and then the entire payload as literal
  characters. Any string-type reply that can arrive on stdin hits this: OSC 52
  clipboard responses, OSC 10/11 colour queries, DECRQSS and XTGETTCAP and
  XTVERSION replies, kitty graphics APC. The fix is a string-accumulation state
  that swallows to `ST`, with BEL accepted as a legacy OSC terminator.
- **Kitty functional keys become Private Use Area characters.** The protocol
  maps every non-printable key into the PUA — lock and menu keys, F13–F35, the
  full keypad, media keys, standalone modifier keys, and a placeholder for
  "layout value unavailable". The CSI-u dispatcher has no table for these, so
  they fall through to a character conversion and get *inserted into the buffer*
  as invisible characters. The rule to enforce is that nothing in the PUA range
  may become a character key.
- **C1 bytes and stray UTF-8 continuation bytes produce `Null` key events**
  rather than being discarded. (Declining to treat `0x9B` as an 8-bit CSI is
  correct under UTF-8 and should stay.)

### Wrong or lost keys

- **Free mouse motion from JediTerm-based terminals reads as a click.** Those
  emulators strip the motion bit, sending the no-button code with a press
  terminator. The SGR decoder maps the no-button value to the left button and
  the press terminator to a button-down, producing a click flood on every mouse
  move. A no-button code can never be a press.
- **SGR reports with a trailing separator before the terminator are dropped.**
  The decoder requires exactly three parameter fields; the optional trailing
  separator is part of the grammar.
- **Event types are ignored.** The kitty modifier field carries a press / repeat
  / release sub-parameter, which is stripped and always reported as a press.
  Harmless while event-type reporting stays off, but it is a live config flag —
  enabled, every keystroke would fire twice.
- **Modifier decoding is incomplete.** Only Shift, Alt and Ctrl are mapped;
  Super, Hyper and Meta are dropped, and the Caps Lock / Num Lock state bits
  have no equivalent. The modifier field is also parsed into a type too narrow
  to hold its maximum legal value, which fails closed to "no modifiers".
- **Application-keypad SS3 forms are dropped.** The SS3 state covers F1–F4,
  arrows, Home and End, but not keypad Enter, keypad Begin, or the keypad
  digit/operator forms — so the numeric keypad goes silent whenever application
  keypad mode is active.
- **`CSI E` (keypad Begin) has no dispatch arm**, and the modified-F1–F4 guard
  caps the modifier value below Hyper and Meta.
- **Alt + non-ASCII is garbage.** The Alt arm converts the following byte
  directly instead of routing a UTF-8 lead byte into the character collector.
- **A kitty flags query reply is decoded as a NUL key.** More generally, a
  parameter list introduced by `?` or `>` is a *reply*, never a key, and should
  be discarded before dispatch.

### Robustness

- **The paste buffer is unbounded and cannot be flushed.** The parameter cap
  protects CSI sequences only. A paste-start with no terminator grows memory
  without limit and swallows every subsequent keystroke, and the flush path
  rescues only the Escape state.
- **The UTF-8 collector does not validate continuation bytes eagerly.** It
  accumulates the expected width and only then decodes, so an `ESC` landing
  inside a truncated character is buffered before the failure is noticed. It
  recovers — the invalid path reprocesses the trailing bytes — but this is the
  same hazard the X10 validity floor already handles properly. The lead-byte
  test also accepts values that cannot begin a valid codepoint.
- **Paste content is not sanitized.** The payload may contain control bytes, and
  the protocol's own security note warns that a nested end marker can be used to
  force a premature exit from paste state.

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
