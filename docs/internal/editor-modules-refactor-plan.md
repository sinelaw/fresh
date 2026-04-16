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

## 5. What each big file becomes

### 5.1 `mod.rs` (9 605 lines) → `app/editor.rs` (~600 lines) + ~25 subsystem dirs

`app/editor.rs` keeps:

- The `Editor` struct definition (subsystem composition).
- `Editor::new`, `with_working_dir`, `for_test`.
- `editor_tick` (the per-frame tick — calls `self.lsp.tick(&mut self.buffers)`,
  `self.async_io.drain(...)`, etc.).
- The named cross-subsystem orchestrators: `save_active`, `open_file`,
  `close_buffer`, `set_active_buffer`, `apply_event_to_active_buffer`,
  `process_async_message` — each one a short coordinator using mechanism (a).

Everything else moves into a subsystem directory under `app/`. Concretely,
the 11 currently-defined types in `mod.rs`:

| Currently in `mod.rs`                                                                    | Moves to                                   |
|------------------------------------------------------------------------------------------|--------------------------------------------|
| `PendingGrammar`                                                                         | `app/grammar/mod.rs` (private)             |
| `SemanticTokenRangeRequest`, `SemanticTokensFullRequestKind`, `SemanticTokenFullRequest` | `app/semantic/mod.rs` (private)            |
| `FoldingRangeRequest`                                                                    | `app/lsp/folding.rs`                       |
| `DabbrevCycleState`                                                                      | `app/completion/dabbrev.rs`                |
| `PendingFileOpen`                                                                        | `app/async_io/file_opens.rs`               |
| `SearchScanState`, `LineScanState`                                                       | `app/search/scan.rs` and `app/buffers/line_scan.rs` |
| `StdinStreamingState`                                                                    | `app/stdin_stream/mod.rs`                  |
| `Editor`                                                                                 | `app/editor.rs`                            |
| `editor_tick`, `normalize_path`                                                          | `editor.rs` and `app/fs/path.rs` respectively |

The ~158 methods in the giant `impl Editor` redistribute by which subsystem
owns the data they touch. Methods that touch one subsystem become methods
on that subsystem. Methods that touch two or three become orchestrators on
`Editor`. Methods that are actually pure (e.g. `normalize_path`) become free
functions.

### 5.2 `render.rs` (5 394 lines) → `app/view/` (~3 500 lines, split four ways)

The structural mistake in the current `render.rs` is that it both *gathers*
state and *draws* it in the same 1 105-line method, while also containing
search-navigation logic, scroll synchronization, and overlay/popup mutation.
Those don't belong together.

Split along Rule 5:

```
app/view/
    mod.rs              // pub fn render(ctx: &RenderCtx, frame: &mut Frame)
    model.rs            // pure data: ViewModel, PaneModel, GutterModel, StatusModel
    build.rs            // ViewModel::build(ctx: &RenderCtx) -> ViewModel    (pure)
    panes.rs            // free fn render_pane(model: &PaneModel, area, frame)
    gutter.rs           //         render_gutter(...)
    status_bar.rs       //         render_status_bar(...) + compose_lsp_status (pure)
    tabs.rs             //         render_tabs, render_tab_context_menu, render_drop_zone
    popups.rs           //         render_popups(model: &[PopupModel], ...)
    prompt_overlay.rs   //         render_prompt_popups(...)
    hover_overlay.rs    //         render_hover_highlights(...)
    file_explorer.rs    //         render_explorer (it already exists separately)
```

Render functions take **only** what they draw — `RenderCtx` for fan-in
reads, never `&Editor`. They contain no mutation. They're testable by
handing in a constructed `ViewModel`.

The mutating logic currently buried in `render.rs` extracts to its rightful
subsystem:

| Current `render.rs` content                                                                                                     | Moves to                                                                |
|---------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| `add_overlay`, `remove_overlay*`, `clear_overlays`                                                                              | `app/buffers/overlays.rs` (overlays live on buffers)                    |
| `show_popup`, `hide_popup`, `dismiss_transient_popups`, `scroll_popup`, `clear_popups`, popup nav                               | `app/menu/popups.rs`                                                    |
| `show_lsp_confirmation_popup`, `handle_lsp_confirmation_response`, `notify_lsp_current_file_opened`                             | `app/lsp/confirmation.rs`                                               |
| `build_search_regex`, `move_cursor_to_match`, `find_next/previous`, `find_match_in_direction`, `expand_regex_replacement` (~600 lines, pure) | `app/search/{regex.rs,navigation.rs,replace.rs}` — pure ones as free fns |
| `action_to_events`, `handle_visual_line_movement`, `collect_lsp_changes`, `calculate_line_info_for_event`                       | `app/editor.rs` (orchestrators) and `app/view/visual_movement.rs` (pure ones) |
| `sync_scroll_groups`, `pre_sync_ensure_visible`                                                                                 | `app/splits/scroll_sync.rs`                                             |
| `notify_lsp_save`                                                                                                               | `app/lsp/notifications.rs`                                              |
| `recompute_layout` (mirrors `render` for macro replay)                                                                          | call `ViewModel::build` from the macro path; one less duplicate         |

### 5.3 `input.rs` (4 138 lines) → `app/input/` (~800 lines) + redistributed handlers

The current `handle_action` is 1 162 lines and 204 arms because each arm is
a mini-implementation. Under Rule 6 each arm should be one line.

```
app/input/
    mod.rs              // KeyDispatcher::dispatch(key, ctx) -> Option<Action>
    context.rs          // get_key_context (pure)
    key_to_action.rs    // resolve key+chord+mode -> Action
    chord.rs            // chord state (currently a field on Editor)
    scrollbar.rs        // scrollbar drag math (PURE FNS, see below)
    mouse_geometry.rs   // screen_to_buffer_position, fold_toggle_byte_from_position (pure)
    settings_prompt.rs  // SettingsPromptBuilder used by all 14 settings prompts
```

`handle_action` itself moves to `app/editor.rs` and becomes a flat
dispatcher:

```rust
impl Editor {
    pub fn handle_action(&mut self, action: Action) -> Result<()> {
        match action {
            Action::Save             => self.save_active(),
            Action::Cut              => self.clipboard.cut(&mut self.buffers, &self.splits),
            Action::Search           => self.search.start_prompt(&mut self.prompt),
            Action::CompletionAccept => self.completion.accept(&mut self.buffers, &self.splits),
            Action::SetTheme         => self.theme.start_select_prompt(&mut self.prompt),
            // ... 200 more single-line arms
        }
    }
}
```

**No arm contains logic.** All 1 162 lines of inline work distribute to the
subsystems they belong to. The settings-prompt cluster (14 nearly-identical
`start_*_prompt` / `apply_*` / `save_to_config` triples, ~850 lines)
collapses onto a `SettingsPromptBuilder<T>` parametrized over the option
type.

The scrollbar code (~900 lines, 7 methods) is currently `&mut self` because
it's lazy — it actually only needs
`(buffer_lines, viewport, click_position) -> new_viewport`. Rewrite as pure
functions in `app/input/scrollbar.rs`; the 900 lines collapse to maybe 400.

`screen_to_buffer_position` and friends are *already* effectively static —
they just happen to be `&self` for convenience. Promote to free functions
taking `(layout, click) -> Position`; this immediately decouples them.

### 5.4 `buffer_management.rs` (3 464 lines) → `app/buffers/` (~1 200 lines) + redistributed

The misnomer is that this file isn't really "buffer management" — it's a
grab bag of every operation that *touches* a buffer. Split by the actual
owner of the state:

```
app/buffers/
    mod.rs              // BufferRegistry: open/close/lookup/serialize, owns buffers map
    config_resolve.rs   // resolve_line_wrap_for_buffer etc. (pure)
    navigation.rs       // goto_line_col, select_range, goto_byte_offset (on registry)
    line_scan.rs        // LineScanManager owns line_scan_state
```

Then redirect everything else to its rightful module:

| Currently in `buffer_management.rs`                                                                                                     | Moves to                                                             |
|-----------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------|
| `open_file_preview`, `is_buffer_preview`, `current_preview`                                                                             | `app/preview/mod.rs` (new, owns the preview field)                   |
| `open_stdin_buffer`, `poll_stdin_streaming`, `complete_stdin_streaming`, `is_stdin_streaming`, `create_virtual_buffer`                  | `app/stdin_stream/mod.rs`                                            |
| `process_search_scan`, `process_search_scan_batch`, `finish_search_scan`                                                                | `app/search/scan.rs`                                                 |
| `show_warnings_popup`, `show_lsp_status_popup`, `build_and_show_lsp_status_popup` (315 lines)                                           | `app/lsp/status_popup.rs` — extract `LspStatusPopupBuilder` that takes `(&LspConfig, &LspServerStatuses, &LspProgress, &Theme)` and *returns* a `PopupModel`. No `Editor` access. |
| `schedule_hot_exit_recovery`, `queue_file_open`, `process_pending_file_opens`, `take_completed_waits`, `remove_wait_tracking`           | `app/async_io/file_opens.rs`                                         |
| `open_help_manual`, `open_keyboard_shortcuts`                                                                                           | `app/help/mod.rs`                                                    |
| `open_file`, `open_local_file`, `open_file_with_encoding`, `reload_with_encoding`, `restore_global_file_state`, `save_file_state_on_close` | `app/editor.rs` orchestrator (mechanism a) — buffers + splits + lsp + grammar + plugins + filesystem |
| `close_buffer`, `force_close_buffer`, `close_buffer_internal`, the six `close_*_tab*` variants                                          | Same — orchestrators in `editor.rs`, each broken into named sub-steps (terminal cleanup, focus history adjust, LSP token cleanup, preview adjust) |
| `switch_buffer`, `next_buffer`, `prev_buffer`, `cycle_tab`, `navigate_back`, `navigate_forward`                                         | `app/splits/navigation.rs` (these change focus, which is owned by SplitState) |
| `get_mouse_hover_state`, `has_transient_popup`, `force_check_mouse_hover`                                                               | `app/hover/mod.rs`                                                   |

What's left in `app/buffers/` is what genuinely belongs to "the registry of
open buffers": create, lookup, serialize, basic navigation. ~1 200 lines,
with clear ownership.

## 6. Handling the realities

Three aspects of the current code make the refactor non-trivial: Rust's
borrow-checker, three orchestrator methods that touch almost everything,
and the coexistence of the old and new patterns during migration.

### 6.1 Borrow-checker

Subsystem methods take `&mut self` (the subsystem). Cross-subsystem
orchestrators look like `editor.x.foo(&mut editor.y)`. Rust's split-borrow
rules permit this **as long as the orchestrator destructures** — the
typical form:

```rust
let Editor { ref mut buffers, ref mut lsp, ref splits, .. } = *self;
buffers.do_a(splits);
lsp.do_b(buffers);
```

This works and is the standard pattern. There are two cases where it
doesn't:

- A subsystem method needs to call back into the orchestrator. **Don't** —
  return an `Effect` (mechanism c).
- A subsystem method needs a third subsystem only conditionally. Pass it
  through a `Ctx` (mechanism b) or split into two methods.

A related pitfall: *don't* make subsystems hold back-references to each
other (no `Rc<RefCell<...>>` cycles, no `Arc<Editor>` on any subsystem).
All cross-subsystem access must be visible in a function signature. That's
the whole point of Rule 2.

### 6.2 The three cross-cutting orchestrators

Three methods in today's code touch almost every field. They are the hard
cases — attempting to "move" them atomically is where refactors of this
shape typically break. Plan each one explicitly.

**`process_async_messages` (mod.rs L4760, ~909 lines, ~20 fields).**
Becomes a `match` over message kind where each arm calls one subsystem:

```rust
fn handle_async(&mut self, msg: AsyncMessage) {
    match msg {
        AsyncMessage::LspResponse(r)    => self.lsp.handle_response(r, &mut self.buffers),
        AsyncMessage::FileOpened(o)     => self.open_file_from_async(o),
        AsyncMessage::PluginCommand(c)  => self.plugins.handle_command(c, /* ctx */),
        AsyncMessage::SearchProgress(p) => self.search.advance_scan(p),
        AsyncMessage::FsPoll(p)         => self.auto_revert.apply_poll(p, &mut self.buffers),
        // ...
    }
}
```

Each arm is one line. The 900 lines of inline handling distribute to the
subsystem that owns the response type. `AsyncMessage` itself becomes an
enum in `app/async_io/messages.rs` with one variant per subsystem that can
receive async work.

**`render` (render.rs L150, ~1 105 lines).** Becomes "build a `ViewModel`
(pure fan-in read, ~200 lines), then walk the model (each render fn ~50
lines, no mutation)". A new contributor can read `gutter.rs` understanding
only `GutterModel`. This is the transformation described in §5.2 — the key
insight is that today's `render` is an *assembler* and a *drawer* fused
together; split them.

**`handle_action` (input.rs L285, ~1 162 lines, 204 arms).** Becomes a flat
dispatcher (shown in §5.3). All inline logic moves to subsystem methods.
If even 200 one-liners becomes unwieldy later, consider a `phf::Map<
ActionKind, fn(&mut Editor)>` — but only after the per-subsystem APIs are
sized correctly. Premature table-dispatch is its own anti-pattern.

### 6.3 Invariants that must survive the refactor

Some of the god-object behaviour currently holds invariants implicitly.
Make these explicit before splitting:

- **Active-buffer consistency.** Several methods set `active_buffer`,
  adjust viewport, update position history, and notify LSP in one atomic
  sequence. Under the new design this becomes `Editor::set_active_buffer`,
  a named orchestrator. Any subsystem that wants to change focus goes
  through it.
- **Preview promotion.** Opening a file with preview, then doing anything
  "committing" (splitting, editing, explicit save), promotes the preview
  into a real tab. Today this is scattered as `promote_current_preview()`
  calls at the top of ~15 methods. Under the new design, `PreviewState`
  owns this and `Editor` orchestrators call `self.preview.promote_if_any(
  &mut self.buffers, &mut self.splits)` as a named prelude.
- **Undo batching.** `apply_action_as_events` currently wraps a sequence
  of events in an undo boundary. The new design moves this wrapping into
  `BufferRegistry::apply_events_as_batch(events)` — the transactional
  shape is explicit.
- **Event logging.** Every buffer mutation goes through `log_and_apply_event`
  so the event stream can be replayed. The new `BufferRegistry::apply_event`
  is the single choke-point that logs.

These invariants used to hold because one struct owned everything and could
enforce discipline ad hoc. Post-refactor they hold because there is exactly
one named choke-point per invariant, in a named file, covered by a test.

## 7. Sequencing without a flag day

The end state above is structural — you don't get there in one PR. But you
can get there incrementally without ever leaving `main` in a broken state,
by going subsystem-by-subsystem in this order. Each phase lands as one or
more commits; each commit compiles, passes tests, and is individually
revertable.

### Phase 1 — Free pure helpers (no state extraction)

Extract the functions that are already effectively pure but happen to be
`&self` methods for convenience. No new types, no new state ownership.

Candidates (from the survey):

- `compose_lsp_status` (render.rs L30–146) — already pure, just move.
- `expand_regex_replacement` (render.rs L3432–4019, ~600 lines) — pure
  backreference expansion.
- `build_search_regex`, `build_replace_regex`, `get_regex_match_len`.
- `screen_to_buffer_position`, `adjust_content_rect_for_compose`,
  `fold_toggle_byte_from_position` (input.rs L2359–2596).
- `smart_home_visual_line` (render.rs L4022–4072).
- `normalize_path` (mod.rs L126–154).
- Scrollbar math helpers: `calculate_scrollbar_jump_visual`,
  `calculate_scrollbar_drag_relative_visual`.
- `resolve_line_wrap_for_buffer`, `resolve_page_view_for_buffer`,
  `resolve_wrap_column_for_buffer` (buffer_management.rs L26–73).
- `byte_to_2d` (currently in clipboard.rs).

Each of these becomes a free function in an appropriate module file. ~1 500
lines decoupled with effectively zero risk. This phase also *proves* the
pure-function pattern and surfaces any hidden state dependencies before
touching real structs.

**Risk:** trivial. **Blast radius:** each call site changes from
`self.foo(...)` to `module::foo(...)`.

### Phase 2 — Leaf subsystems

Extract the smallest state clusters that are touched by ≤ 5 call sites.
These establish the subsystem pattern before tackling anything cross-cutting.

In order of increasing complexity:

1. `MacroState` (4 fields: `macros`, `macro_recording`, `last_macro_register`,
   `macro_playing`). Record/replay cluster.
2. `BookmarkState` (2 fields: `bookmarks`, `active_custom_contexts`).
3. `HoverState` (~4 fields: `hover_symbol_range`, `hover_symbol_overlay`,
   `mouse_hover_screen_position`, `pending_hover_request`).
4. `ClipboardState` (just `clipboard`, already externally-typed).
5. `StdinStreamingState` (1 field, plus the 5 methods in buffer_management.rs).
6. `LineScanManager` (1 field, 6 methods).
7. `SearchScanManager` (1 field, 3 methods).
8. `UpdateChecker` (1 field, a couple of methods).
9. `BackgroundFade` + `ThemeCache` into `ThemeState`.

Each lands as its own commit. Template for each:

1. Create `app/<name>/mod.rs` with the state struct and moved methods.
2. Add the field to `Editor`.
3. Remove the old fields and the `impl Editor` methods that operated on them.
4. Update call sites (typically `self.foo` → `self.<name>.foo`).
5. `cargo build && cargo test` green.

**Risk:** low per subsystem. **Blast radius:** ≤ 10 call sites each.

### Phase 3 — Render: ViewModel + split into `app/view/`

Introduce `ViewModel` as pure data. Build it inside the current `render`
method, then progressively move each sub-render (gutter, tabs, status bar,
popups) to take only its slice of the model. Each sub-render move is a
separate commit.

Order within the phase:

1. Define `ViewModel`, `PaneModel`, `GutterModel`, `StatusModel`,
   `TabModel`, `PopupModel` in `app/view/model.rs`. No behaviour change.
2. Introduce `RenderCtx<'a>` read-only bundle.
3. Move `compose_lsp_status` into `status_bar.rs` (free fn).
4. Extract `render_status_bar(model: &StatusModel, theme: &ThemeState, ...)`
   to `app/view/status_bar.rs`.
5. Extract `render_gutter`.
6. Extract `render_tabs`, `render_tab_context_menu`, `render_tab_drop_zone`.
7. Extract `render_popups`, popup nav.
8. Extract `render_prompt_popups`.
9. Extract `render_hover_highlights`.
10. Delete `recompute_layout` — call `ViewModel::build` from the macro replay
    path instead.

**Risk:** medium. The main render method is the single biggest hotspot; do
it step-by-step with visual regression tests running between each commit.
The existing `visual-regression` harness (under `docs/visual-regression`) is
the safety net.

### Phase 4 — Flatten `handle_action`

Not a single commit — one commit per arm group. Each commit absorbs ~10–20
`Action::*` arms into their respective subsystem, shrinking the giant match
by that much.

Order: easy groups first (clipboard, macros, bookmarks, theme selection) to
establish the pattern, then the harder ones (search, LSP, completion,
prompt lifecycle).

**Risk:** low per commit. Each arm is isolated; regressions surface as
single broken actions. **Blast radius:** one action per commit.

### Phase 5 — Input subsystem extraction

With `handle_action` already thin, extract the remaining input code:

1. Move scrollbar pure fns to `app/input/scrollbar.rs`.
2. Move `mouse_geometry` pure fns.
3. Extract `SettingsPromptBuilder<T>` and collapse the 14 settings-prompt
   triples into it.
4. Move `get_key_context` and `handle_key` to `app/input/dispatch.rs`.
5. Move chord state into `app/input/chord.rs` (own field on Editor).

**Risk:** low-medium.

### Phase 6 — Buffer-management redistribution

With subsystems in place, redirect the contents of `buffer_management.rs`
to their rightful owners:

1. `PreviewState` (owns preview promotion invariant).
2. Move the 5 stdin streaming methods to `app/stdin_stream/`.
3. Move `LspStatusPopupBuilder` to `app/lsp/status_popup.rs` as a pure
   builder returning a `PopupModel`.
4. Move tab-navigation methods to `app/splits/navigation.rs`.
5. Rewrite `open_file` / `close_buffer` as named orchestrators in
   `app/editor.rs` with sub-steps (terminal cleanup, focus history, LSP
   cleanup, preview adjust).

**Risk:** medium. `close_buffer_internal` is 234 lines with 10+ field
touches — break into named local helpers on `Editor` first, then move each
helper's body onto the subsystem that owns the state it manipulates.

### Phase 7 — Cross-cutting subsystems

The remaining big state clusters, tackled last because they are read by
many orchestrators:

1. `SearchState` (consolidates both the search prompt and the scan manager).
2. `CompletionState` + `SemanticTokensState`.
3. `PromptState` + `FileOpenState` (consolidate existing `prompt_actions.rs`,
   `file_open*.rs`).
4. `PluginState` (large — last).
5. `LspState` (largest — very last; ~25 fields, read by render, async
   messages, save orchestrator).

**Risk:** medium-high. These are cross-cutting and will surface the most
`&mut`/borrow rearrangement work. Phases 1–6 have already built up the
machinery (Effects, RenderCtx, named orchestrators) so by this point the
pattern is well-established.

### Phase 8 — Structural cleanup

- Delete `app/*_actions.rs` files that have been fully absorbed.
- Move `mod.rs` content to `app/editor.rs`; leave `mod.rs` as a module
  index (`pub mod editor; pub use editor::Editor;` plus sub-module
  re-exports).
- Audit: `rg "impl Editor" crates/fresh-editor/src/app/` must return only
  `app/editor.rs`. This is the acceptance criterion for Rule 1.
- Audit: no subsystem file contains `use super::Editor` (only
  `app/editor.rs` imports subsystem types, not the other way round).

### Expected outcome

After all phases:

| File / directory     | Before  | After    |
|----------------------|--------:|---------:|
| `app/mod.rs`         | 9 605   | ~50 (re-exports) |
| `app/editor.rs`      | —       | ~600 (struct + orchestrators) |
| `app/view/`          | 5 394   | ~3 500 (split ~11 ways) |
| `app/input/`         | 4 138   | ~800 (split ~7 ways) |
| `app/buffers/`       | 3 464   | ~1 200 |
| `app/lsp/`           | existing| ~2 800 absorbs scattered logic |
| `app/search/`, etc.  | —       | each subsystem 200–800 |
| **Total `app/`**     | ~45 flat files, 22 601 LoC in 4 mega-files | ~28 subsystem dirs, largest single file ~800 LoC |

Acceptance criteria: Rule 1 audit passes; each subsystem has unit tests
that construct only its own state; the render fns are pure and testable
against a hand-constructed `ViewModel`; `handle_action`'s body fits on one
screen.
