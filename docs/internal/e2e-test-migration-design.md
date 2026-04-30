# E2E Test Migration — From Imperative Harness to Declarative Theorems

**Status:** Phases 1, 2, 3, A, B, C, D all landed on
`claude/e2e-test-migration-design-HxHlO`. Test suite green (67 passing,
0 ignored). Framework proven by finding *and fixing* three real
production bugs (proptest found two; theorem migration found a third).
**Branch:** `claude/e2e-test-migration-design-HxHlO`
**Owner:** TBD
**Scope:** `crates/fresh-editor/tests/e2e/*` (~220 files)

## What's landed (cumulative)

| Track | Commit | Lines | Result |
|---|---|---|---|
| 0 — design | `48a6c92` | +950 docs | This document |
| 1 — `EditorTestApi` seam | `af066ad` | +295 | Trait + `Caret` projection on `Editor`, `harness.api_mut()`, isolation lint script, smoke test |
| 2 — `BufferTheorem` + PoC | `448fd4f` | +245 | Runner framework, `tests/semantic/case_conversion.rs` rewritten as a 12-line theorem |
| 3 — Multi-cursor + Trace + minimal Layout | `d93d483` | +298 | Multi-cursor coverage, undo-roundtrip runner, viewport_top_byte observable |
| Result-shape refactor | `a22a47d` | +388 | `check_*` returns `Result<(), TheoremFailure>`; `assert_*` is a thin panicking wrapper. Enables external drivers (fuzzers, generators, proof-search) without `catch_unwind` |
| A — proptest properties | `53ec62c` | +356 | 3 properties driven by `check_*`; **found two real production bugs** (`actions.rs:1613` smart-dedent panic, `state.rs:462` delete-backward OOB) in 70s of fuzzing |
| B — E2E migrations | `9de5787` | +302 | sort_lines (3), indent_dedent (3), select_to_paragraph (2), smart_home (2). Theorem revealed an unstated `SortLines` selection-clearing asymmetry. |
| C — observables on demand | (in B commit) | — | `TerminalSize` + `assert_buffer_theorem_with_terminal` added because smart_home's wrap variant needs custom dimensions |
| D1 — serde failures | `4925f8a` | ~40 | `TheoremFailure: Serialize + Deserialize`; JSON round-trip meta-test. External drivers can write to dashboards / CI artifacts / replay logs without string parsing. |
| **Bug fixes (1)** | `d95b9d1` | +13/-21 | Both latent bugs fixed: `prefix_bytes.last()` instead of OOB indexing in actions.rs; `.min(deleted_text.len())` clamp in state.rs. All `#[ignore]` annotations removed. |
| **Track B (continued)** | `ad4887a` | +364/-30 | `duplicate_line` (5 theorems), `emacs_actions` (8 + 1 #[ignore]'d bug). Theorem migration of `test_open_line_basic` revealed a 3rd bug: `Action::OpenLine` advances the cursor instead of staying put (Emacs C-o intent). |
| **Bug fixes (2)** | (next commit) | small | OpenLine handler emits a follow-up `Event::MoveCursor` to restore the cursor position. The `#[ignore]`'d theorem flips to a passing regression test plus a "type-after-OpenLine inserts on the original line" companion. |

**Final test count:** 67 passing, 0 ignored, 0 failing. Every formerly-
`#[ignore]`d property and regression repro is now permanent coverage.

**13 declarative theorems** under `tests/semantic/`, each one
mathematically pinning down behavior the imperative originals were
silent or vague about (selection clearing, cursor position after
sort, exact byte ranges of select-to-paragraph).

**Two latent production bugs found by the property tests, now fixed in `d95b9d1`:**

1. `actions.rs:1613` — smart-dedent: `prefix_bytes[prefix_len - 1]`
   panicked when `cursor.position` was stale (past buffer end) so
   `slice_bytes(line_start..cursor.position)` returned fewer bytes
   than `prefix_len` implied. **Fix:** use `prefix_bytes.last()` and
   guard on `!prefix_bytes.is_empty()`.
2. `state.rs:462` — `DeleteBackward`'s newline-counter:
   `deleted_text[..bytes_before_cursor]` panicked when `range.len()`
   exceeded `deleted_text.len()` (stale range end past buffer).
   **Fix:** also clamp with `.min(deleted_text.len())`.

Both were the same family — cursor position out of sync with buffer
state after a chain of selection-replace + deletion actions on
whitespace-only content. Option (b) from the original recommendation
(fix the call sites) was chosen over option (a) (force layout
reconciliation between actions in `handle_execute_actions`) because
the call-site fixes are local, defensive, and don't slow down the
batch dispatch path.

**Reachability that motivated the fix:**

| Path | Renders / reconciles between actions? | Crashed before fix? |
|---|---|---|
| Interactive keystrokes (verified in tmux on the release binary) | yes (per keystroke) | no |
| Macro replay (`play_macro` calls `recompute_layout` per action) | yes | no |
| Property test `dispatch_seq` (no render between) | no | **yes** |
| `handle_execute_actions` in `app/plugin_dispatch.rs` (vi-mode count prefixes like `3dw`, plugin-driven action batches) | **no** | **yes** |

The bugs were *latent* in the sense that the only production path
that reached them was the plugin/vi-count-prefix dispatch loop. The
property tests' `evaluate_actions` mimics that path exactly — no
render reconciliation between actions — which is what made them
findable. The `play_macro` implementation in `app/macro_actions.rs`
already calls `recompute_layout` between every action specifically
to avoid this class of bug; that load-bearing comment was the
first hint that the underlying invariant violation was real and
known to be fragile.

`diagnosis_bug{1,2}_does_not_panic_with_render_between` in
`tests/semantic/regressions.rs` confirmed the diagnosis: the same
shrunk repros pass cleanly when `harness.render()` is called between
every action. They remain in the suite as permanent guards against
regression of the layout-reconciliation invariant.

This validates the framework's premise: declarative theorem testing
with a typed-failure external driver is materially better at finding
bugs than imperative E2E. The two bugs found here are the kind that
*cannot* be reached by typing on the keyboard — they live behind
plugin and vi-mode dispatch — so even an exhaustive imperative suite
that drives every keystroke would never have surfaced them.

**Deferred deliberately:** the full `RenderSnapshot` design from §9.1
and the issue-#1147-style Class B rewrites that depend on it; the
language-detection migrations (`toggle_comment`, ~30 similar tests)
that need a `load_buffer_from_text_named` test API extension. Each
should land alongside the first theorem that demonstrably needs it.

---

## 1. Motivation

The current E2E suite drives the editor through a virtual `crossterm`
keyboard, a `ratatui` `TestBackend`, and explicit `harness.render()` cycles.
A typical test reads:

```rust
harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
for _ in 0..5 {
    harness.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
}
harness.render().unwrap();
harness.send_key(KeyCode::Char('u'), KeyModifiers::ALT).unwrap();
harness.render().unwrap();
assert_eq!(harness.get_buffer_content().unwrap(), "HELLO world");
```

This shape has three structural problems:

1. **Coupling to physical keys.** `KeyCode::Char('u') + ALT` is a property of
   the default keymap, not of the case-conversion feature. Tests break when
   shortcuts move.
2. **Coupling to the render loop.** Every state mutation needs a `render()`
   to "settle" output, slowing the suite and obscuring intent.
3. **Coupling to UI screen-scraping.** Many assertions read characters out
   of the `ratatui::Buffer`, conflating logic bugs with rendering bugs.

The proposed style replaces the trio (`send_key`, `render`, screen-scrape)
with a pure data structure (a *Theorem*) declaring `(initial state, action
sequence, expected final state)`, evaluated by a runner that touches no
terminal.

## 2. Existing Seams (production already exposes most of what we need)

A reconnaissance pass on the editor crate found we do **not** need to refactor
production:

| Need | Existing API | Where |
|---|---|---|
| Semantic alphabet | `pub enum Action` | `src/input/keybindings.rs:305` |
| Apply one action headlessly | `Editor::dispatch_action_for_tests(action)` | `src/app/editor_init.rs:1327` (`#[doc(hidden)]`, already `pub`) |
| Read buffer text | `editor.active_state().buffer.to_string()` | `src/app/mod.rs:1265` |
| Read cursors | `editor.active_cursors()` returning `&Cursors` | `src/app/mod.rs:1277` |
| Read viewport | `editor.active_viewport()` | `src/app/mod.rs:1294` |

`Action` already covers the cases the inspiration sketch's `BufferAction`
covers — `MoveLeft`, `SelectRight`, `ToUpperCase`, `Undo`, `Redo`,
`AddCursorNextMatch`, etc. — and is already a serializable data enum
(`Debug, Clone, PartialEq, Eq, Serialize, Deserialize`). It is the
"alphabet" of the system.

`dispatch_action_for_tests` routes through the same `handle_action` path
the production input layer uses, so semantic-level coverage is identical
to keystroke-level coverage for actions that don't depend on modal UI
state (popups, menus, prompts).

**Conclusion:** Phase 1's "minimal API exposure" is mostly *re-export*
work, not new surface — *but the seams must be wrapped*; see §2.1.

### 2.1 Tests bind to a named test API, not arbitrary internals

A non-negotiable design principle: theorem tests **never** reach into
`editor.active_state()`, `editor.active_cursors()`, or
`editor.active_viewport()` directly. Those accessors are production
internals; if the test suite depends on their exact shape, refactoring
them becomes a cross-cutting churn (this is half of why the current
harness is sticky).

Instead, all observation flows through one explicit, versioned, named
surface:

```rust
// Test-only module on the editor. ~100 LOC. Zero behavior, all reads.
//
// crates/fresh-editor/src/test_api.rs   (or `app/test_api.rs`)
#[doc(hidden)]
#[cfg(any(test, feature = "test-api"))]
pub mod test_api {
    use crate::input::keybindings::Action;

    /// The single entry point for test-driven mutation.
    pub trait EditorTestApi {
        // ── Drive ────────────────────────────────────────────────
        fn dispatch(&mut self, action: Action);
        fn dispatch_seq(&mut self, actions: &[Action]);

        // ── Class A: pure state observables ──────────────────────
        fn buffer_text(&self) -> String;
        fn primary_caret(&self) -> Caret;
        fn carets(&self) -> Vec<Caret>;
        fn selection_text(&self) -> String;

        // ── Class B: layout observables (Phase 3) ────────────────
        fn render_snapshot(&mut self) -> RenderSnapshot;

        // ── Class C: styled observables (Phase 3+) ───────────────
        fn styled_frame(&mut self, theme: &Theme) -> StyledFrame;
    }

    /// Small projection over Cursor: only the fields tests assert on.
    /// Hides sticky_column, deselect_on_move, block_anchor unless the
    /// test explicitly asks (variant constructor).
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Caret {
        pub position: usize,
        pub anchor:   Option<usize>,
    }

    pub struct RenderSnapshot { /* §9.1 */ }
    pub struct StyledFrame    { /* §9.5 */ }
    pub struct Theme          { /* opaque handle */ }
}
```

`Editor` implements `EditorTestApi`. **No other accessors are reachable
from theorem tests.** Concretely:

- `tests/semantic/**` may `use fresh::test_api::*;` and nothing else
  from the editor.
- `tests/semantic/**` may **not** `use fresh::app::Editor`,
  `fresh::model::cursor::Cursor`, `fresh::model::buffer::Buffer`, or
  `fresh::view::viewport::Viewport`.
- The runner type (`assert_buffer_theorem`) holds an
  `&mut dyn EditorTestApi`, not an `&mut Editor`.

This buys us four things:

1. **Refactor freedom.** Internal renames (`active_state` → `state_for`
   …) don't touch a single test.
2. **Explicit observation contract.** A reviewer reading `test_api.rs`
   sees the entire dependency surface in one file.
3. **Forces "what does a test need to see?" to be a design question.**
   If a theorem can't be expressed against `EditorTestApi`, the right
   reflex is to *propose adding an observable*, not to bypass the API.
4. **Caps the migration's reverse-coupling.** Production code can never
   accidentally depend on something tests rely on, because the test
   API is one-directional.

A minimal Phase 2 only exposes `dispatch` + `buffer_text` +
`primary_caret` + `carets` + `selection_text`. That is sufficient for
the case-conversion PoC. Everything else (`RenderSnapshot`,
`StyledFrame`, modal observables) is added *only when the next theorem
type lands*, with a code review check that the new entry is the
smallest sufficient observable.

The runner sketch in §5.2 is updated accordingly: it does **not** read
from `h.editor()`; it reads from `h.test_api()` (or, if the harness is
itself made to implement `EditorTestApi`, from `h` directly).

## 3. The One Real Caveat: Viewport Scroll Depends On Rendering

Several E2E tests (notably `issue_1147_wrapped_line_nav`,
`scroll_*`, `line_wrap_scroll_bugs`) assert on `viewport.top_byte`.
That field is only reconciled by `Viewport::ensure_visible_in_layout`
(`src/view/viewport.rs:993`), which consumes `ViewLine`s computed by the
render pipeline. Without a render, `top_byte` does not move when the
cursor moves.

This means **two classes of tests** exist, and only one is fully
"renderless":

- **Class A — pure state.** Buffer text, cursor positions, selection,
  undo/redo, multi-cursor layout, indent/dedent, case conversion,
  duplicate-line, sort-lines, smart-home (text-only assertions),
  toggle-comment. ≈ 60 % of the suite by file count.

- **Class B — viewport / layout.** Anything asserting on `top_byte`,
  `top_line_number`, scrollbar geometry, screen cursor `(x, y)`, visible
  rows, virtual-line positioning. Needs the layout pipeline.

The migration handles each class differently (§5). Class A is the PoC.

## 4. Target Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Theorem<Domain>                                            │
│  ─────────────────                                          │
│  description:    &'static str                               │
│  initial:        Domain::State                              │
│  actions:        Vec<Domain::Cmd>                           │
│  expected:       Domain::Expectation                        │
└─────────────────────────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  fn assert_theorem<D: Domain>(t: Theorem<D>)                │
│   1. instantiate Editor offscreen (no terminal draws)       │
│   2. seed Domain::State into Editor                         │
│   3. for each cmd: editor.dispatch_action_for_tests(cmd)    │
│   4. read Domain::Observable out of editor                  │
│   5. assert observable == expected                          │
└─────────────────────────────────────────────────────────────┘
```

A `Domain` trait describes *what kind of test this is*: pure-buffer,
viewport, multi-cursor, undo-trace, etc. Each domain carries its own
`State`, `Cmd` (an alias for `Action` or a subset), `Expectation`, and
projection function. This avoids one bloated `Theorem` struct that
accumulates `Option<…>` fields for every possible assertion.

### 4.1 Why we don't introduce a parallel `HeadlessEditorCore`

The inspiration sketch shows a separate `HeadlessEditorCore` that
re-implements buffer, cursor, and wrap math. Building a second core would
*add* a verification problem (does the core agree with the editor?)
rather than remove one. Since `Editor` already runs headlessly when fed
through `dispatch_action_for_tests` (no terminal IO occurs unless
`terminal.draw` is called), the cheaper move is to **reuse the editor as
the denotation**. The runner just skips the rendering step.

### 4.2 Production code changes (Phase 1)

Strictly additive, gated behind an existing `#[doc(hidden)]` test API.

1. **No new types.** Use `Action` as the command alphabet.
2. **One new accessor** if needed, on `Editor`:
   ```rust
   #[doc(hidden)]
   pub fn dispatch_action_sequence_for_tests(&mut self, actions: &[Action]) {
       for a in actions { let _ = self.handle_action(a.clone()); }
       let _ = self.process_async_messages();
   }
   ```
   This is purely a convenience over a loop and can be skipped if the
   runner calls `dispatch_action_for_tests` itself.
3. **Cursor projection helper** (test-only) so theorems can express
   expected cursors without depending on the in-tree `Cursor` struct's
   default fields:
   ```rust
   #[doc(hidden)]
   pub fn cursor_snapshot_for_tests(&self) -> Vec<CursorSnapshot> { … }
   ```
   This *is* new surface — but only ~30 lines, marked test-only,
   and never reachable from `main.rs`.

That's the entire production diff for Phase 1: roughly two `#[doc(hidden)]`
methods. The terminal application path is untouched.

## 5. The PoC (Phase 2)

### 5.1 Test selected for the PoC

`tests/e2e/case_conversion.rs::test_to_uppercase` (lines 5–39).

**Why this one:**

- Pure Class A: only buffer text and cursor are observed.
- Touches three independent concerns the architecture must cover:
  cursor movement (`MoveLineStart`), selection extension (`SelectRight`
  ×5), and a transformation (`ToUpperCase`).
- Has an obvious "lift to repetition" opportunity: `for _ in 0..5
  { send_key(Right, SHIFT) }` becomes `Repeat(SelectRight, 5)`.
- Failure modes are unambiguous (text + selection range), so a green
  test is convincing evidence the architecture works.

The current test (38 lines, four `harness.render()` calls) becomes:

```rust
#[test]
fn theorem_to_uppercase_selection() {
    assert_buffer_theorem(BufferTheorem {
        description:      "Alt+U uppercases the selected range and leaves selection intact",
        initial_text:     "hello world",
        initial_cursor:   Caret::EndOfBuffer,
        actions:          vec![
            Action::MoveLineStart,
            Repeat(Action::SelectRight, 5),
            Action::ToUpperCase,
        ],
        expected_text:    "HELLO world",
        expected_primary: CursorExpect::range(0, 5),
        expected_extra_cursors: vec![],
    });
}
```

No `KeyCode`. No `render()`. No `harness.get_selected_text()` round-trip.

### 5.2 PoC code structure

```
crates/fresh-editor/tests/
├── common/
│   └── theorem/
│       ├── mod.rs              ← Domain trait + Theorem<D> + Repeat helper
│       ├── buffer_theorem.rs   ← Class A: text + cursor assertions
│       └── runner.rs           ← assert_buffer_theorem
└── e2e_theorems/
    └── case_conversion.rs      ← rewritten test (PoC)
```

`buffer_theorem.rs` (sketch, ~80 LOC) — **only imports from
`fresh::test_api`, never from `fresh::app` / `fresh::model` /
`fresh::view`:**

```rust
use fresh::test_api::{EditorTestApi, Caret, Action};
use crate::common::harness::EditorTestHarness; // hosts the headless Editor

#[derive(Clone)]
pub enum InitialCaret {
    StartOfBuffer,
    EndOfBuffer,
    Byte(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub struct CursorExpect {
    pub position: usize,
    pub anchor: Option<usize>,
}

impl CursorExpect {
    pub fn at(p: usize) -> Self { Self { position: p, anchor: None } }
    pub fn range(anchor: usize, position: usize) -> Self {
        Self { position, anchor: Some(anchor) }
    }
}

pub struct BufferTheorem {
    pub description:            &'static str,
    pub initial_text:            &'static str,
    pub initial_cursor:          InitialCaret,
    pub actions:                 Vec<Action>,
    pub expected_text:           &'static str,
    pub expected_primary:        CursorExpect,
    pub expected_extra_cursors:  Vec<CursorExpect>,
}

pub fn assert_buffer_theorem(t: BufferTheorem) {
    let mut h = EditorTestHarness::new(80, 24).unwrap();
    let _fix = h.load_buffer_from_text(t.initial_text).unwrap();

    // EditorTestHarness exposes &mut dyn EditorTestApi via h.api_mut().
    let api: &mut dyn EditorTestApi = h.api_mut();

    seed_initial_caret(api, t.initial_cursor, t.initial_text.len());
    api.dispatch_seq(&t.actions);
    // No render() call. Ever.

    assert_eq!(api.buffer_text(), t.expected_text,
               "buffer text mismatch in: {}", t.description);

    let primary = api.primary_caret();
    assert_eq!(primary, t.expected_primary.into(), "primary caret in: {}", t.description);

    let extras: Vec<_> = api.carets().into_iter().skip(1).collect();
    assert_eq!(extras.len(), t.expected_extra_cursors.len(), …);
    for (got, want) in extras.iter().zip(&t.expected_extra_cursors) {
        assert_eq!(*got, want.clone().into(), …);
    }
}
```

The harness change is small — add one method:

```rust
// crates/fresh-editor/tests/common/harness.rs
impl EditorTestHarness {
    pub fn api_mut(&mut self) -> &mut dyn fresh::test_api::EditorTestApi {
        &mut self.editor   // because Editor: EditorTestApi
    }
}
```

That's the *only* surface the new test directory needs.

`Repeat` is a tiny helper, not a new variant on `Action`:

```rust
pub fn Repeat(a: Action, n: usize) -> impl Iterator<Item = Action> { … }

// usage:
actions.extend(Repeat(Action::SelectRight, 5));
```

We *could* push `Repeat` into the production enum, but that adds a
variant to a 600-case enum that production code doesn't need. Keeping
it in the test layer is cheaper and more honest.

### 5.3 What the PoC proves

- `Action` + `dispatch_action_for_tests` is sufficient to express a real
  bug-class test (case conversion is in production, has a bug history).
- Tests run faster: zero `terminal.draw` cycles, no `process_async_messages`
  per keystroke, no shadow-string mirroring.
- The test is keymap-agnostic: changing the Alt+U binding doesn't break it.
- Test reads as a *specification* of the feature, not a transcript of a
  user session.

### 5.4 What the PoC does **not** address (intentionally)

- Viewport / scroll assertions (Class B). Held back to Phase 3.
- Modal-UI tests (command palette, file open prompt, settings tree).
  These need an additional vocabulary item (e.g.,
  `OpenCommandPalette / FilterTo("duplicate line") / ConfirmSelection`).
- Plugin-driven actions (these route through async dispatch and may need
  one extra `process_async_messages()` call inside the runner).
- LSP and filesystem-dependent tests (need fakes; orthogonal to the
  semantic-test idea).

## 6. Phase 3 — Expansion plan

After the PoC merges and is reviewed, expansion proceeds **per
domain**, each adding *one* new theorem type alongside `BufferTheorem`:

| Domain | New theorem | Approx. tests covered | New API surface |
|---|---|---|---|
| Cursor & text mutation | `BufferTheorem` (PoC) | ~80 | 0 |
| Multi-cursor | `MultiCursorTheorem` | ~25 | 0 |
| Undo / redo trace | `TraceIsomorphismTheorem` (forward + `undo_all`) | ~15 | 0 |
| Modal popups | `ModalTheorem` (`Open`, `Filter`, `Confirm`) | ~30 | 1 helper for prompt state |
| Viewport / scroll | `LayoutTheorem` (single explicit `render_for_layout()` call inside the runner; everything else still declarative) | ~40 | 0 |
| Theme projection | `ProjectionTheorem<State, View>` (pure function over `Theme + State`) | ~10 | 1 pure projection function per UI surface |

For Class B (viewport), the runner *does* invoke a layout pass — but
crucially **once at the end**, not after every action. This is the
minimal bridge that lets us keep declarative tests while honoring the
fact that scroll is layout-dependent. The runner shape:

```rust
pub fn assert_layout_theorem(t: LayoutTheorem) {
    let mut h = EditorTestHarness::new(t.width, t.height).unwrap();
    let _fix = h.load_buffer_from_text(t.initial_text).unwrap();
    for a in t.actions { h.editor_mut().dispatch_action_for_tests(a); }
    h.render().unwrap(); // single, terminal-side-effect-free layout pass
    assert_eq!(h.top_byte(), t.expected_top_byte, …);
}
```

Migration is **incremental and reversible**: old `EditorTestHarness`
tests continue to compile and run side by side with new theorem tests.
A test is migrated when:

1. It fits a domain.
2. Its domain has a runner.
3. The author judges the rewrite reads more clearly than the original.

Tests that don't migrate (e.g., genuinely visual regression tests, GUI
mouse-drag flows) stay imperative — that's fine. The goal is **not**
100 % migration; the goal is to remove keymap and render coupling
*where they aren't actually being tested*.

## 7. Tension with `CONTRIBUTING.md` rule #2 — and how to resolve it

`CONTRIBUTING.md` currently states:

> **E2E Tests Observe, Not Inspect**: Any new user flow must include an
> end-to-end test that drives keyboard/mouse events and asserts only on
> rendered output. Do not call accessors that return model, view, or
> context state — if an invariant isn't visible on screen, cover it with
> a unit test on the component.

The theorem-style tests this design proposes are, by that definition,
**unit tests on the editor component**, not E2E tests. The rule was
written to prevent two real failure modes:

1. **False-green drift.** An "E2E" test that pokes internal state and
   never renders can pass while the user-visible output is broken.
2. **Bug class blindness.** Cursor blink, selection highlight, scrollbar
   geometry, theme contrast, gutter alignment — none of these surface
   in `editor.active_state()` and would never be caught by state-only
   assertions.

The migration must **not** weaken this protection. The resolution:

1. **Rename, don't reclassify.** Theorem tests live under
   `tests/semantic/` (or `tests/component/`), not `tests/e2e/`. They
   are explicitly *component-level* tests on `Editor` as a state
   machine. The directory name is the contract: a reader knows
   immediately what guarantees the test does and does not provide.
2. **The `tests/e2e/` directory keeps its current rule.** Anything in
   `tests/e2e/` continues to drive keys/mouse and assert on rendered
   output. We do not migrate `tests/e2e/` files into theorems by
   *moving* them; we *add* a semantic-test sibling and, only when the
   semantic test fully covers the bug, optionally retire the E2E one
   case-by-case during review. Most E2E tests will stay.
3. **Update the contributing rule** to make the categories explicit:
   ```
   E2E (tests/e2e/): drive input, assert on rendered output. Required
       for any new user flow. Cover GUI/render/keymap concerns.
   Semantic (tests/semantic/): apply Action sequences, assert on
       Editor state. Required for any new editor-logic invariant
       that is not visible on screen, *and* allowed as a faster
       redundant proof for bugs already covered by an E2E.
   Property/shadow (tests/property_*, tests/shadow_*): unchanged.
   ```

This keeps the *intent* of rule #2 — "if it isn't on screen, you didn't
test the user-facing thing" — while letting us put logic-only
invariants (case conversion preserves selection range; multi-cursor
undo is atomic; smart-home toggles between two specific byte offsets)
in a faster, clearer harness. **Whenever a bug has both a logic and a
visual symptom, both tests are required.**

The PoC in §5 should be revised accordingly: the new test goes under
`tests/semantic/case_conversion.rs`, *not* `tests/e2e_theorems/`. The
existing `tests/e2e/case_conversion.rs::test_to_uppercase` stays.

## 8. Testing rendering issues

The migration is explicitly **not** a strategy for testing rendering.
Anything that can break in the renderer — color, contrast, glyph
choice, cursor visibility, gutter width, scrollbar position, line-wrap
indent, syntax highlighting — needs a test that actually runs the
render pipeline. Theorem tests cover state. Below is how each render
concern stays covered:

### 8.1 Existing render-side coverage (kept as-is)

| Concern | Existing harness |
|---|---|
| Frame contents (cell-level) | `harness.render() + harness.buffer()` / `screen_to_string()` |
| ANSI escape correctness | `harness.render_real()` / `render_real_incremental()` (vt100 parser) |
| Visual regression (themes, snapshots) | `tests/common/visual_testing.rs`, `tests/common/snapshots/` |
| Hardware cursor show/hide | `harness.render_observing_cursor()` |
| Multi-cursor secondary cursor styling | `harness.find_all_cursors()` |
| Theme screenshots | `tests/e2e/theme_screenshots.rs` |

None of these change. Theorem-style tests *cannot* replace them and
shouldn't try.

### 8.2 Three new render-test patterns the migration *adds*

Once Class A theorems exist, three render-targeted patterns become
cheap to write and should be standard practice:

#### A. Pure projection theorems (`ProjectionTheorem<S, V>`)

For UI surfaces that are pure functions of state — settings tree
nodes, status-bar segments, tab labels, gutter cells, diff-hunk
markers — extract the projection function:

```rust
fn project_settings_node(node: &SettingsNode, theme: &Theme) -> CellStyle { … }
```

…and test it in isolation. This is the
`theorem_settings_label_projection` pattern from the inspiration
sketch. It catches *render bugs* (foreground == background, cursor
visible while not editing, wrong fg in selection) without driving keys
*or* running the full layout. The test runs in microseconds.

The pre-condition is that the projection function exists as a pure
function in production. Some surfaces don't yet — extracting them is a
small, additive refactor. **No production refactor is on the critical
path for Phase 2;** projection theorems are a Phase 3+ pattern.

#### B. Layout theorems (`LayoutTheorem`)

Apply the action sequence headlessly, then run **one** layout pass and
assert on layout-level observables (`top_byte`, `top_view_line_offset`,
visible row → byte mapping, soft-wrap row count). This is described in
§6 (`assert_layout_theorem`) and is the right shape for issues like
#1147 (viewport scrolls when it shouldn't).

What this catches that pure state can't: incorrect viewport
reconciliation, wrap-row miscounts, gutter-width drift.

What it doesn't catch: anything below the layout layer (color,
attributes, glyph rendering). Those need pattern (C).

#### C. Render-diff theorems (`RenderTheorem`)

For visual bugs that survive the layout (e.g., scrollbar in the wrong
column, overlay color confusion, off-by-one row), apply the action
sequence, render once, and assert on a small, *named* slice of the
buffer:

```rust
RenderTheorem {
    description: "Scrollbar uses theme.scrollbar.fg, not theme.text.fg (issue #1554)",
    initial_text: …,
    actions: vec![Action::MovePageDown],
    width: 80, height: 24,
    inspect: Inspect::ColumnFg { col: 79, row_range: 2..22 },
    expected: ExpectedFg::All(theme.scrollbar.fg),
}
```

The runner takes the `Inspect` enum and pulls out exactly the cells
that matter, comparing them to the expected color/symbol/modifier.
This is the "snapshot test, but tightly scoped" shape: easier to read
than a full screen diff, less brittle than asserting on a substring of
`screen_to_string()`.

`Inspect` variants would start small:
`Cell { x, y }`, `Row { y }`, `Column { x }`, `Region(Rect)`,
`HardwareCursor`. The runner returns a typed result so the assertion
reads as data, not as ad-hoc string parsing.

This is the smallest delta from current screen-scraping practice — the
test still calls `terminal.draw` once — but moves the assertion from
"does this substring appear somewhere?" to "what fg does cell (79, 5)
have?". That precision is what catches theme regressions.

### 8.3 What stays imperative, forever

Some tests will never become declarative without losing their value:

- **Visual regression / golden-image tests.** A bug like "the gutter
  glyph for a fold marker shifted by one column" is best detected by a
  byte-for-byte comparison of a saved screenshot. These already exist
  in `docs/visual-regression/` and `tests/common/visual_testing.rs`.
- **Animations.** Cursor blink, scroll smoothing, fade-out highlights —
  the test's subject *is* the temporal evolution of the rendered
  buffer.
- **GUI-mode tests.** `tests/e2e/gui.rs` exercises the
  `winit/wgpu` layer; theorem tests can't reach it.
- **Crossterm / terminal-emulator integration.** ANSI escape
  generation, focus events, OSC 52, mouse-encoding. These are
  *rendering*, by definition.

Migration plans must enumerate these and **not** convert them.

## 9. The middle layer — view model between state and pixels

Both the §3 caveat (viewport scroll only settles during render) and the
§8 render-test patterns point to the same gap: there is no named,
stable, *observable* layer between `EditorState` and the styled
`ratatui::Buffer`. Today's pipeline collapses several conceptual stages
into a single `render()` call:

```
EditorState  ──[ layout ]──▶  ViewLine[]  ──[ style + glyph ]──▶  ratatui::Buffer
   (data)       (mostly                       (theme +                (cells
                pure)                          symbols)                with
                                                                       fg/bg)
```

`ViewLine` exists internally but is not a publicly testable artifact,
and crucially it doesn't include cross-cutting things tests care about
(scrollbar thumb position, hardware cursor row/col, popup placement,
fold-indicator column). Tests therefore choose between two unappealing
extremes: state-only (misses display bugs) or buffer-cell scraping
(brittle, theme-coupled, breaks when the gutter layout changes).

A **named view-model layer** would be the right test target for a large
subset of "looks wrong" bugs. Below are four candidate shapes,
discussed in increasing scope.

### 9.1 Option A — `RenderSnapshot` (smallest, most pragmatic)

A typed struct produced by the *layout* phase, before any colors or
glyphs. Roughly:

```rust
pub struct RenderSnapshot {
    pub width: u16,
    pub height: u16,
    pub viewport: ViewportSnapshot,           // top_byte, top_view_line_offset, scroll thumb
    pub gutter:   GutterSnapshot,             // per-row { line_number, fold_marker, diagnostic }
    pub rows:     Vec<RowSnapshot>,           // per visible content row, semantic segments
    pub hw_cursor: Option<(u16, u16)>,        // screen cell of the primary cursor
    pub secondary_cursors: Vec<(u16, u16)>,
    pub decorations: Vec<DecorationSnapshot>, // diagnostics, search highlights, multi-cursor etc.
    pub popups:   Vec<PopupSnapshot>,         // {kind, area, content_lines}
    pub status:   StatusBarSnapshot,
    pub tabs:     TabBarSnapshot,
}

pub struct RowSnapshot {
    pub view_line: usize,                     // index into the buffer's ViewLine sequence
    pub source_byte_range: Option<Range<usize>>,
    pub kind: RowKind,                        // Source | WrappedContinuation | Virtual { plugin }
    pub segments: Vec<Segment>,
}

pub enum Segment {
    Text { byte_range: Range<usize>, role: TextRole },  // role = Normal | Selection | Match | Inactive
    Whitespace { kind: WsKind },
    Tab { stops: u16 },
    WrapMarker,
    Conceal { replacement: String },
}
```

Crucially: **no colors, no theme.** Roles are semantic
(`TextRole::Selection`), not pigment.

Tests target a `RenderSnapshot` to assert claims like:

- "After PageDown, row 0 shows view_line 24, not 25." (issue #1147)
- "The fold marker for line 12 is on screen row 7, column 4."
- "After Ctrl+End, hw_cursor is on the last source byte's row."
- "The scrollbar thumb covers rows 18..22 of the content area."
- "Search match decoration covers the byte range 142..147 on row 9."

Theme regressions are *out of scope* for `RenderSnapshot`-targeted
tests; they get a separate styling-layer test (§9.5).

**Cost:** A new pass `EditorState → RenderSnapshot` already exists in
spirit inside the renderer; the work is to factor it out cleanly. ~300
LOC of additive code, plus a `pub fn snapshot_for_tests(&mut self) ->
RenderSnapshot` accessor on `Editor`.

**Win:** The 40-ish viewport/scroll tests (Class B) become declarative.
The render-diff theorem pattern from §8.2 (C) targets `RenderSnapshot`
instead of `ratatui::Buffer`, gaining theme-independence.

### 9.2 Option B — Per-surface view models

Instead of one big `RenderSnapshot`, expose one view model per
top-level UI surface, behind small traits:

```rust
trait HasViewModel { type Vm; fn view_model(&self) -> Self::Vm; }

impl HasViewModel for TabBar    { type Vm = TabBarVm;    … }
impl HasViewModel for StatusBar { type Vm = StatusBarVm; … }
impl HasViewModel for Gutter    { type Vm = GutterVm;    … }
impl HasViewModel for SettingsPanel { type Vm = SettingsTreeVm; … }
```

Each surface tests *its own* view model in isolation:

```rust
let vm = harness.editor().tab_bar_vm();
assert_eq!(vm.tabs[1].title, "main.rs");
assert_eq!(vm.tabs[1].state, TabVmState::ModifiedActive);
```

**Pro vs. Option A:** Surface-local. Adding a new surface doesn't
require extending one mega-struct. Closer to how the code is already
organized (per-widget modules under `view/ui/`).

**Con:** Cross-surface invariants (e.g. "popup area doesn't overlap
status bar") need a coordinator step on top. Test discoverability is
worse — there's no single "what does the screen show?" object.

### 9.3 Option C — `EditorView` as algebraic data type

Treat the entire UI as an immutable ADT computed from state:

```rust
pub enum EditorView {
    Buffer { tabs: TabBarVm, body: BufferBodyVm, status: StatusBarVm, popups: Vec<PopupVm> },
    Settings(SettingsVm),
    FileBrowser(FileBrowserVm),
    Splash(SplashVm),
}
```

This is the most denotational of the four — closest to "the screen *is*
a function of state" — and makes mode transitions explicit (you can't
have a settings tree visible while in `EditorView::Buffer`, by
construction).

**Pro:** Forces every UI mode to be reified, which would catch real
bugs (e.g., the "popup is open but its input handler is dead"
race-condition class).

**Con:** Big upfront refactor; touches every UI module. Not
incrementally adoptable. The migration would block on it.

### 9.4 Option D — Semantic cell-grid (style-free buffer)

A `ratatui::Buffer`-shaped grid where cells carry **roles**, not
colors:

```rust
pub struct SemanticCell {
    pub symbol: String,
    pub role: CellRole,             // Normal | Selection | Cursor | LineNumber | …
    pub tags:  TagSet,              // {InMatchHighlight, InFold, OnVirtualLine}
}
```

Render is then `(SemanticCell grid + Theme) → styled cells`. Tests on
the grid catch "wrong cell got the cursor role" without coupling to
colors; tests on the styled cells catch theme bugs.

**Pro:** Drop-in for existing screen-scraping patterns. Minimal change
to assertion shape.

**Con:** Grid-shaped data is the *least* declarative form — you still
write `assert grid[12][4].role == Cursor` rather than `assert
hw_cursor == (4, 12)`. Better than today, worse than (A)/(B).

### 9.5 Recommendation

**Adopt Option A (`RenderSnapshot`) in Phase 3, after the Class A PoC
is merged.** Justification:

- It's the smallest unit of architectural change that unblocks the
  largest test category (Class B viewport/scroll).
- It composes with Option B: the snapshot's surface fields can be
  individual view models if a surface earns one.
- It does **not** require Option C's mode-ADT refactor and is
  forward-compatible with it (a future `EditorView` would *contain* a
  `RenderSnapshot`).
- It is theme-free, which protects against the existing screen-scraping
  brittleness the migration is trying to escape.

A typical Phase-3 layout-theorem then looks like:

```rust
LayoutTheorem {
    description: "Issue #1147: Up arrow at end-of-file does not scroll viewport",
    initial_text: ISSUE_1147_CONTENT,
    width: 80, height: 25,
    actions: vec![
        Action::MoveDocumentEnd,
        repeat(Action::MoveUp, 4),
    ],
    expect: |s: &RenderSnapshot| {
        assert_eq!(s.viewport.top_byte, ISSUE_1147_FINAL_TOP_BYTE);
        assert!(s.hw_cursor.unwrap().1 < (s.height - 4));
    },
}
```

The two theme-sensitive tests (`Bug #2: Foreground maps to selection_bg`
in the inspiration sketch) get a separate `StyleTheorem` that pairs a
`RenderSnapshot` with a `Theme` and asserts on the styled output:

```rust
StyleTheorem {
    snapshot: built_above,
    theme:    Theme::high_contrast(),
    expect:   |styled| assert_ne!(styled.cell(4, 12).fg, styled.cell(4, 12).bg),
}
```

This three-layer split — `State → RenderSnapshot → StyledFrame` —
gives every test a precise target:

| Test target | Layer | What it catches | What it can't catch |
|---|---|---|---|
| `BufferTheorem` | State | logic, cursor math, undo, multi-cursor | anything visual |
| `LayoutTheorem` | RenderSnapshot | viewport, gutter columns, popup placement, hw cursor row/col | colors, glyph choice, ANSI |
| `StyleTheorem` | StyledFrame | theme contrast, role-to-color mapping, modifier flags | terminal-emulator quirks |
| Existing E2E | Terminal | ANSI escape correctness, end-to-end user flow | (final backstop) |

The migration starts at the top and stops at the row that gives
diminishing returns. Phase 2 (PoC) only commits to the top row;
Phase 3 adds `RenderSnapshot` if and only if Class B tests prove the
investment is worth it.

## 10. Risks & non-goals

- **Risk: Action coverage gaps.** A test might exercise a path triggered
  only by a keymap (e.g., a chord that produces multiple events). Mitigation:
  if no `Action` exists, that's a finding — production should expose one,
  not the test should fall back to `KeyCode`.
- **Risk: Cursor-snapshot drift.** The `Cursor` struct has fields
  (`sticky_column`, `deselect_on_move`) that tests usually shouldn't care
  about. The `CursorExpect`/`CursorSnapshot` helpers project away these
  fields. We accept that two cursors with different `sticky_column`
  compare equal in a `BufferTheorem`; tests that care use
  `MultiCursorTheorem`.
- **Non-goal: rewriting the editor core.** No changes to buffer, cursor,
  viewport, or input dispatch.
- **Non-goal: replacing property tests.** `tests/property_*.rs` and
  `tests/shadow_model_*.rs` already operate at the model layer and stay
  as they are.
- **Non-goal: deleting `EditorTestHarness`.** It remains the host for
  the headless `Editor` instance and provides fixture loading,
  filesystem isolation, etc. The runner is a thin layer over it.

## 8. Acceptance criteria for Phase 2 (PoC)

Phase 2 is "done" when, on the migration branch:

- [ ] `crates/fresh-editor/tests/common/theorem/mod.rs` exists with
      `BufferTheorem` and `assert_buffer_theorem`.
- [ ] `crates/fresh-editor/tests/e2e_theorems/case_conversion.rs`
      contains a rewritten `theorem_to_uppercase_selection` test.
- [ ] The new test passes.
- [ ] The original `test_to_uppercase` is **left in place** and still
      passes (proof of additivity).
- [ ] Production diff is ≤ 150 LOC, all behind `#[doc(hidden)]` or
      `#[cfg(any(test, feature = "test-api"))]`. The diff is dominated
      by the `test_api` module from §2.1.
- [ ] No new dependency added to `Cargo.toml`.
- [ ] No `harness.render()` call in the new test.
- [ ] No `crossterm::KeyCode` import in the new test.
- [ ] **No `use fresh::app::…`, `use fresh::model::…`, or `use
      fresh::view::…` in `tests/semantic/**`.** Only `fresh::test_api`
      and the harness are reachable. CI lint or a tidy script enforces
      this.

## 9. Open questions for review

1. **Should `Repeat` be a real `Action` variant?** Pro: makes macros and
   plugin replay simpler. Con: introduces nesting into a thus-far flat
   enum, and `Action` is `Serialize` (so we'd need to think about JSON
   shape). *Recommendation: keep `Repeat` test-side for now.*
2. **Should the runner accept a `Vec<Action>` or `&[Action]`?** Owned
   `Vec` reads better in declarative theorems; `&[_]` allows reuse of
   action sequences across theorems. *Recommendation: take `Vec`,
   document `theorem.actions = base_actions.to_vec(); …` for sharing.*
3. **Should we add a `theorem!` macro?** Removes boilerplate but adds
   a layer of indirection. *Recommendation: defer until ≥ 5 theorems
   exist and the boilerplate is real.*
4. **Class B runner: render once, or expose `Editor::layout_for_tests()`?**
   The latter is more principled (no terminal at all) but requires
   factoring out the layout pass from the render pass. *Recommendation:
   render-once for Phase 3 to limit scope; consider extraction later if
   the test count justifies it.*

## 10. Appendix — file-level inventory

For triage in Phase 3. Counts approximate (`ls tests/e2e | wc -l = 224`,
some are non-test support files).

```
Pure buffer/cursor (Class A, ~80):
  basic.rs, case_conversion.rs, sort_lines.rs, smart_home.rs,
  duplicate_line.rs, indent_dedent.rs, toggle_comment.rs,
  triple_click.rs, select_to_paragraph.rs, undo_redo.rs,
  block_selection.rs, multicursor.rs, …

Viewport / scroll (Class B, ~40):
  issue_1147_wrapped_line_nav.rs, scroll_clearing.rs,
  scroll_wrapped_reach_last_line.rs, scrolling.rs,
  line_wrap_scroll_bugs.rs, search_center_on_scroll.rs,
  search_viewport_stall_after_wrap.rs, ctrl_end_wrapped.rs,
  horizontal_scrollbar.rs, scroll_sync, …

Modal UI (palette / settings / file picker, ~30):
  command_palette.rs, file_browser.rs, file_explorer.rs,
  settings_*.rs (multiple), keybinding_editor.rs, action_popup_global.rs

Plugin / LSP / filesystem (~40):
  language_features_e2e.rs, hot_exit_*.rs, slow_filesystem.rs,
  remote_*.rs, universal_lsp.rs, dabbrev_completion.rs

Visual / theme / rendering (kept imperative, ~30):
  theme_screenshots.rs, visual_regression.rs, theme.rs,
  cursor_style_rendering.rs, blog_showcases.rs

GUI / terminal-emulator / mouse-flow (kept imperative, ~10):
  gui.rs, terminal*.rs, tab_drag.rs, ansi_cursor.rs
```

The rough triage suggests ≈ 45 % of the suite (Class A + Class B) is
mechanically migratable to declarative theorems; another ~15 % (modal
UI) needs a small additional vocabulary; the rest stays imperative
because what they test *is* the rendering / GUI / external behavior.
