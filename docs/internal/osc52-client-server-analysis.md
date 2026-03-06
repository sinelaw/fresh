# OSC 52 in Client-Server Session Attach Mode: Analysis

## The Problem

OSC 52 clipboard copy does not work when the editor runs in client-server
(daemon/session) mode. The root cause is straightforward: the clipboard service
writes OSC 52 escape sequences directly to **the server process's `stdout()`**,
which is detached from any terminal. The sequences never reach the client's
terminal emulator.

### Code Path (current)

```
User yanks text
  → Clipboard::copy() called in the server process
    → execute!(stdout(), CopyToClipboard::to_clipboard_from(&text))   // clipboard.rs:112
    → stdout().flush()                                                  // clipboard.rs:118
```

In daemon mode, the server's stdout is `/dev/null` (or a log file). The OSC 52
bytes vanish. The client relay loop (`relay_unix.rs` / `relay_windows.rs`) only
forwards bytes it reads from the **data socket**, not from the server's stdout.

### What the Design Doc Intended

The design doc (`session-persistence-design.md:604-651`) correctly identifies this
issue and prescribes:

```
Server generates:    \x1b]52;c;BASE64_TEXT\x07
     ↓
Server writes to data socket
     ↓
Client relays to stdout (no parsing)
     ↓
Terminal emulator receives and sets system clipboard
```

The infrastructure to do this **already exists**: `Editor::queue_escape_sequences()`
and `Editor::take_pending_escape_sequences()` (in `app/mod.rs:3206-3212`). This
mechanism is already used for cursor style changes in session mode
(`app/input.rs:3322-3324`). But the clipboard code was never wired into it.

---

## The Fix (Straightforward Path)

When `session_mode` is true, instead of writing OSC 52 to `stdout()`, the
clipboard service should produce the raw escape sequence bytes and the editor
should route them via `queue_escape_sequences()`.

Concretely:

1. **`Clipboard::copy()`** should return (or store) the OSC 52 bytes rather
   than writing to stdout when in session mode, OR
2. **The copy call site** (`app/input_dispatch.rs:328-331`) should generate and
   queue the OSC 52 sequence itself after calling `clipboard.copy()`.

Option 2 is simpler since it mirrors the cursor-style pattern at
`app/input.rs:3322-3324` and avoids threading session-mode awareness into the
clipboard service.

### Sketch

```rust
// In input_dispatch.rs, after clipboard.copy(text):
if self.session_mode && self.clipboard.use_osc52() {
    let encoded = base64::encode(&text);
    let osc52 = format!("\x1b]52;c;{}\x07", encoded);
    self.queue_escape_sequences(osc52.as_bytes());
}
```

The queued bytes flow through `take_pending_escape_sequences()` →
`render_and_broadcast()` → client data socket → client stdout → terminal
emulator. This path is already proven to work for cursor style sequences.

---

## Trade-offs of the Direct Fix

| Aspect | Pro | Con |
|--------|-----|-----|
| Simplicity | Minimal code change, mirrors existing pattern | None significant |
| Correctness | Bytes reach the terminal that the user is looking at | Multiple attached clients all receive the OSC 52 (usually fine — they all see the same editor) |
| Arboard fallback | Still works for non-session mode | In session/daemon mode, arboard likely fails (no display server access) — but that's the status quo |
| Testing | Easy to unit test (check queued bytes) | Integration testing requires a terminal that supports OSC 52 |

---

## Deeper Trade-offs and Alternatives

### Alternative 1: Route OSC 52 Through a Control Message

Instead of embedding OSC 52 in the data stream, define a new `ServerControl`
variant:

```rust
enum ServerControl {
    // ...existing variants...
    SetClipboard { text: String },
}
```

The client would then generate and write the OSC 52 sequence locally.

**Pros:**
- Client can choose the best clipboard method (OSC 52, pbcopy, xclip, etc.)
- Client knows its own terminal's capabilities
- Cleaner separation: data stream = rendering, control stream = side-effects
- Client could use arboard directly (it has display server access)

**Cons:**
- More protocol surface area
- Client needs clipboard logic (currently zero clipboard code in client)
- Protocol versioning concern (old client + new server)

### Alternative 2: Hybrid — Data Stream + Client-Side Clipboard

Use the data stream for OSC 52 (the simple fix) but also send a control message.
The client can then *additionally* try arboard/native clipboard for terminals
that don't support OSC 52.

**Pros:**
- Maximum compatibility
- OSC 52 works for terminals that support it (zero client changes)
- Control message enables arboard fallback on the client side

**Cons:**
- Dual mechanisms could result in double-copy (minor, harmless)
- More complexity

### Alternative 3: Client-Only Clipboard (No Server Involvement)

Have the client intercept specific escape sequences in the data stream and
handle clipboard operations itself, or use an out-of-band mechanism.

**Pros:**
- Server stays clipboard-agnostic

**Cons:**
- Client must parse the data stream (currently it's a dumb relay — this is a
  valuable property to preserve)
- Fragile; any change to escape sequence format breaks it

### Alternative 4: Do Nothing for OSC 52 — Rely on Arboard Only

Disable OSC 52 in session mode entirely and rely on arboard in the server.

**Pros:**
- No escape sequence routing needed

**Cons:**
- Arboard **does not work** in daemon mode on most setups. The daemonized server
  has no `$DISPLAY` / `$WAYLAND_DISPLAY` access. This is explicitly called out
  in the design doc. This alternative does not solve the problem.

---

## Recommendation

**Start with the direct fix** (route OSC 52 through `queue_escape_sequences`).
It is:

- Minimal code change (~10 lines)
- Uses existing, proven infrastructure
- Matches the design doc's intent exactly
- Unblocks the common case (modern terminals all support OSC 52)

If broader clipboard compatibility becomes important later, layer on
Alternative 2 (add a `SetClipboard` control message so the client can
additionally try native clipboard methods). But that's a separate, additive
change — not a prerequisite.

### Open Questions

1. **Multi-client semantics:** When multiple clients are attached, all receive
   the OSC 52 sequence. This is probably fine (they're all viewing the same
   editor state), but worth noting.

2. **OSC 52 size limits:** Some terminals cap the payload at ~100KB. For very
   large yanks, should we skip OSC 52 and show a status message? The current
   non-session code doesn't handle this either, so it's not a regression.

3. **Should `Clipboard::copy()` skip writing to stdout when `session_mode` is
   true?** Currently it writes to the server's stdout (harmless but wasteful).
   Cleanest approach: add a `session_mode` flag to `Clipboard` and suppress
   the direct `stdout()` write, letting the caller handle routing.
