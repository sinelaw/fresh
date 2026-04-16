# Editor modules refactor plan

Target: break up the four mega-files in `crates/fresh-editor/src/app/` into
strongly self-contained modules that do not share state with each other via
scattered `impl Editor` blocks.

Current sizes:

| File                     | Lines | Top-level types                              | Methods on `Editor`                                                 |
|--------------------------|------:|----------------------------------------------|---------------------------------------------------------------------|
| `mod.rs`                 | 9 605 | 11 (incl. `Editor` itself, ~67 field groups) | ~158 in one giant `impl`                                            |
| `render.rs`              | 5 394 | 1 enum + 1 const                             | 42                                                                  |
| `input.rs`               | 4 138 | **0**                                        | ~50 (`handle_key` 276 lines, `handle_action` **1 162 lines / 204 arms**) |
| `buffer_management.rs`   | 3 464 | **0**                                        | 47 public + 16 private                                              |

## 1. Why the existing pattern is the wrong model

The codebase's current convention — files like `lsp_actions.rs`,
`popup_actions.rs`, `clipboard.rs` that contain `impl Editor { … }` — looks
modular but isn't. Every one of those files can read and write *any* of
`Editor`'s ~67 field clusters. A test for "clipboard logic" still requires
constructing a whole `Editor`. Renaming a buffer field still ripples through
every "module". The files are partitioned, the *state* is not.

Real modularity needs three things the current code lacks:

1. **State ownership.** Each subsystem owns its own data in its own struct.
   Other subsystems can't reach in.
2. **Explicit dependencies.** When subsystem A needs something from B, it
   appears in the function signature — not as `self.b_field`.
3. **No `impl Editor` outside the editor file.** That single rule prevents
   the "scattered god object" pattern from re-emerging.

The goal is *not* "smaller files with `impl Editor` blocks". It is: replace
`Editor` with a small composition of owned subsystem structs, give each
subsystem a narrow public API, and confine cross-subsystem coordination to a
thin orchestration layer.

## 2. Architectural principles

- **Rule 1 (hard):** Only `app/editor.rs` may contain `impl Editor`. That
  file holds the struct definition, `new`, `tick`, and the explicit
  cross-subsystem orchestrators (`save_active`, `open_file`, `close_buffer`,
  `process_async_message`, etc.). Nothing else.
- **Rule 2 (hard):** Each module exposes a `pub struct FooState` (or just
  `Foo`) plus methods on *that* struct. No method in a subsystem takes
  `&mut Editor`.
- **Rule 3:** Cross-subsystem reads/writes happen by one of four explicit
  mechanisms (§4). Reaching across via a back-pointer is forbidden.
- **Rule 4:** Pure helpers (regex, coordinate math, layout math, color
  computation) are *free functions* in the relevant module — never methods
  on a state struct, never on `Editor`.
- **Rule 5:** Render is "build a model → draw the model". Building reads
  state; drawing is pure. Split the file along that line.
- **Rule 6:** Input is "key → `Action` enum → dispatch per arm". Each arm
  is one line that calls one subsystem.

These aren't aesthetic preferences — they're what makes individual modules
testable and reviewable in isolation. Rule 1 is the keystone: without it,
all the others get eroded the next time someone needs "just one quick field".

## 3. Target shape of `Editor`

After the refactor, `Editor` becomes a struct of subsystems — *not* a flat
soup of fields. Each field is a self-contained module type defined
elsewhere:

```rust
// app/editor.rs — the ONLY file with `impl Editor`
pub struct Editor {
    // Core data
    pub buffers:        BufferRegistry,
    pub splits:         SplitState,
    pub view:           ViewState,            // window flags, terminal size
    pub theme:          ThemeState,
    pub config:         Config,

    // Language services
    pub grammar:        GrammarState,
    pub lsp:            LspState,
    pub completion:     CompletionState,
    pub semantic:       SemanticTokensState,
    pub hover:          HoverState,

    // Modal UI
    pub prompt:         PromptState,
    pub menu:           MenuState,
    pub file_explorer:  FileExplorerState,
    pub search:         SearchState,          // search + replace + scan
    pub macros:         MacroState,

    // Editing services
    pub clipboard:      ClipboardState,
    pub bookmarks:      BookmarkState,
    pub history:        PositionHistory,
    pub keybindings:    KeybindingState,

    // I/O & background
    pub fs:             FilesystemHandles,
    pub auto_revert:    AutoRevertState,
    pub recovery:       RecoveryState,
    pub stdin_stream:   StdinStreamingState,
    pub async_io:       AsyncIoState,         // runtime, bridge, queues
    pub plugins:        PluginState,

    // Cross-cutting
    pub status:         StatusState,
    pub session:        SessionState,
    pub events:         EventBroadcaster,
    pub update_check:   UpdateChecker,
}
```

~28 fields, all owned subsystems. Compare to the current 67 raw field
clusters with no encapsulation. Crucially, Rust's split-borrow rules let
you borrow `&mut editor.buffers` and `&mut editor.lsp` simultaneously — the
orchestrator code that needs both compiles cleanly.

Each subsystem lives in `app/<name>/` as a directory module:

```
app/lsp/
    mod.rs                  // pub struct LspState + its small public API
    diagnostics.rs          // free fns / private helpers operating on LspState
    progress.rs
    requests.rs
    server_lifecycle.rs
    confirmation_popup.rs   // builds a PopupModel; doesn't show it
```

Inside `app/lsp/`, files freely access `LspState`'s private fields. Outside
`app/lsp/`, you can only call its public methods. That is what
"self-contained" means in this design.

## 4. Four mechanisms for cross-subsystem coordination

The hard problem the current code dodges by using a god object: how does
the search subsystem move the cursor? How does saving notify the LSP? Pick
one of these four explicit patterns per case. **Don't add a fifth.**

### (a) Orchestrator with split borrows

For operations whose *whole purpose* is to combine two or three subsystems.
Lives in `app/editor.rs`:

```rust
impl Editor {
    pub fn save_active(&mut self) -> Result<()> {
        let id = self.splits.active_buffer();
        let path = self.buffers.path_of(id)?;
        let bytes = self.buffers.serialize(id)?;
        self.fs.write(&path, &bytes)?;
        self.lsp.notify_did_save(id, &path);
        self.recovery.mark_clean(id);
        Ok(())
    }
}
```

Five short lines of explicit coordination, instead of a 200-line
`&mut self` method that reaches into 20 fields. Use this for the named
flows: open/save/close, focus changes, applying an edit event.

### (b) Read-only context bundle

When a subsystem method needs to *read* several others — common in render
and in build-prompt-suggestions code:

```rust
pub struct RenderCtx<'a> {
    pub buffers: &'a BufferRegistry,
    pub splits:  &'a SplitState,
    pub theme:   &'a ThemeState,
    pub view:    &'a ViewState,
    pub config:  &'a Config,
}
```

Built once at the top of `render`, passed by reference into each render fn.
Render fns become pure functions of `(RenderCtx, area) -> Vec<Span>`. They
are unit-testable without ever constructing an `Editor`.

### (c) Effects returned, applied by the caller

When a subsystem method's *primary work* is internal but it has side
effects elsewhere. Replace "this method mutates the LSP and shows a popup"
with:

```rust
pub enum Effect {
    OpenFile(PathBuf),
    Status(String),
    LspNotifyDidOpen(BufferId),
    ShowPopup(PopupModel),
    Quit,
}

impl SearchState {
    pub fn confirm(&mut self, query: &str) -> Vec<Effect> { ... }
}
```

The orchestrator (`editor_tick` or `handle_action`) drains the returned
`Vec<Effect>` and applies each. This keeps `SearchState` testable with zero
dependencies and makes every side effect visible at the call site.

### (d) Event bus (already partially present via `EventBroadcaster`)

For genuinely fan-out cross-cutting events: buffer changed, file saved,
LSP server connected. Subsystems publish; subsystems subscribe. Reserve
this for events with ≥3 unrelated consumers — overusing it makes data flow
opaque.

**Decision rule.** Use (a) for known flows, (b) for read-many fan-in,
(c) for "write-mostly with side effects", (d) only for truly broadcast
lifecycle events. If a method needs *none* of these, it belongs entirely
inside one subsystem and is self-contained.
