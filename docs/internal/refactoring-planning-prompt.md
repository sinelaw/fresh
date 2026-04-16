# Prompt: Plan a refactoring in the Fresh codebase

This file is an LLM prompt. Give it (or paste it) to an LLM tasked with
producing a refactoring plan for some portion of this codebase. It encodes
the structure, principles, and rigour the project expects from a plan — the
shape established by
[`editor-modules-refactor-plan.md`](editor-modules-refactor-plan.md) and
[`split-rendering-refactor-plan.md`](split-rendering-refactor-plan.md),
which are the canonical worked examples.

When adapting the output to a new target, read both reference plans first.
They are the contract; this prompt is the scaffolding.

---

## The primary anti-pattern this codebase is fighting

**A single big struct with dozens of flat fields, touched by `impl`
blocks scattered across many files.**

The symptom: `impl Editor { … }` appears in `lsp_actions.rs`,
`popup_actions.rs`, `clipboard.rs`, `buffer_management.rs`, and a dozen
more. It looks modular. It isn't: every one of those methods can read
and write *any* of `Editor`'s ~67 fields. The files are partitioned;
the *state* is not. A unit test for clipboard still needs a full
`Editor`. Renaming a buffer field still ripples everywhere.

**The refactor is fundamentally about grouping fields.** Not about
moving `impl` blocks into new directories — that's cosmetic. The job is:

1. **Find the god struct** — one with dozens of fields and many scattered
   `impl` blocks across the codebase.
2. **Cluster its fields by concern.** Which fields are read/written
   together? Which `impl`-file implicitly "owns" a cluster (even though
   it can touch all fields)? Those clusters are the latent subsystems.
3. **Extract each cluster into its own struct** (e.g. `MacroState`,
   `BookmarkState`, `LspState`, `ClipboardState`) in its own file, with
   `impl` blocks on *that new struct*.
4. **Replace the raw fields on the god struct with a single field per
   new sub-struct.** The god struct no longer holds
   `macros: Vec<Macro>, macro_recording: bool, last_macro_register:
   Option<char>, macro_playing: bool` — it holds `macros: MacroState`.
   It is now a composition of ~25 owned subsystems, not a flat bag of
   ~67 fields.
5. **Migrate each scattered `impl Editor` method** to `impl <SubStruct>`
   on the struct that owns the fields it touches. Only methods that
   genuinely need to combine multiple subsystems stay as
   `impl <GodStruct>` orchestrators.

The god struct doesn't need to contain those fields directly — that's
the whole point. After the refactor, it contains sub-structs that
contain those fields. The consequence — the observable success criterion
— is that only one file contains `impl <GodStruct>`. But the *mechanism*
is the field clustering and composition, not the `impl`-move. A plan
that describes new directories without naming the field clusters being
extracted is missing the actual work.

If your plan ends with `impl Editor` blocks still scattered across many
files, or with the god struct still holding the same flat fields under
a new directory layout, your plan is wrong. Everything else in this
document serves this transformation.

## Role

You are planning a structural refactoring of a specific file, module, or
subsystem in the `sinelaw/fresh` Rust codebase (a terminal IDE/editor).
You are **not** making behavioural changes. You are **not** adding features.
The goal is to make the code easier to read, test, and evolve — without a
flag day, without leaving `main` broken at any commit, and without adding
speculative abstractions.

The deliverable is a single Markdown document living at
`docs/internal/<target>-refactor-plan.md`. A reader of that document should
be able to execute the refactor in PR-sized commits without asking you
follow-up questions.

## What the user gives you

Either:
- a target file / module / subsystem name (e.g. `app/mod.rs`,
  `view/ui/split_rendering.rs`, `lsp/client.rs`), or
- a problem statement (e.g. "the input-handling path spans four files and
  nobody can find anything").

If the scope is ambiguous, ask one clarifying question before planning.
Prefer narrower scopes — a plan that covers one 8k-line file is more useful
than a plan that covers "the editor".

## Before you write anything: measure

A plan that can't cite numbers is a wishlist. Do the measuring yourself
using `Grep`, `Glob`, and `Read`. The measurements directly feed the
field-clustering work in §3 of your plan.

1. **Field inventory on the god struct.** Read the struct definition and
   list every field. Cite the total count. This is your universe — every
   field must end up in exactly one sub-struct (or stay on the god struct
   because it's genuinely shared).

2. **Scattered-`impl` audit.** Run
   `rg -l "impl <TargetType>\b" <target-dir>` and list every file that
   matches. In this codebase that is a long list, and that is exactly the
   problem. Cite the count.

3. **Field-access matrix (the core measurement).** For each file in the
   scattered-`impl` list, which fields of the god struct does it read or
   write? Use Grep with patterns like `self\.<field>` per field per file.
   You don't need a full matrix — you need to identify clusters:

   - Fields touched only by `macro_actions.rs` → `MacroState` cluster.
   - Fields touched only by `bookmark_actions.rs` → `BookmarkState`.
   - Fields touched by 3+ unrelated files → candidates to stay on the god
     struct or to become a shared read-only context.
   - Fields touched by one file that *also* touches 20 other fields → the
     file has mixed concerns; look at sub-groups of its methods.

4. **Method distribution.** How many methods live in each scattered-`impl`
   file? How many fields does each file's methods touch? A file whose
   methods collectively touch >50% of the god struct's fields is doing
   orchestration disguised as a concern.

5. **Largest methods.** A 1,000+ line `handle_action`, `render`, or
   `process_async_messages` is its own category — call these out
   separately, as they will need individual plans in §7.

6. **Shared "mega-struct" contexts.** Types with >10 fields that are
   passed between functions (e.g. `SelectionContext`,
   `LineRenderInput`). These are the same anti-pattern at a smaller
   scale and should appear in the measurements table.

7. **External call sites** of the module's public API (so you know the
   blast radius of any signature changes).

Put the raw measurements in tables in §1 of your plan. The headline
tables are (a) the scattered-`impl` file list and (b) the proposed
field clusters. If you can't measure something cheaply, say so — don't
guess.

## Principles the plan must uphold

These are the same principles the two reference plans are built on. Apply
them — and, where a principle is already well-stated in one of the
reference plans, quote it by reference rather than re-deriving it.

1. **Single `impl` file per god type (the load-bearing rule).** Only one
   file may contain `impl <GodType>`. Everywhere else, you own a
   subsystem struct and put methods on *that*. This is not an aesthetic
   preference — it is the keystone. Without it, every other rule gets
   eroded the next time someone needs "just one quick field". Enforce it
   with a grep audit in the success criteria.
2. **State ownership.** Each subsystem owns its own data in its own type.
   Other subsystems cannot reach in. A subsystem method takes
   `&mut self` meaning the subsystem — never `&mut Editor`.
3. **Explicit dependencies.** When A needs something from B, it appears in
   the function signature. Not `self.b_field`. Not `Rc<RefCell<B>>`. Not a
   back-pointer. A function signature.
4. **Pure helpers are free functions.** Regex building, coordinate math,
   layout math, colour computation, path normalisation: these are not
   methods. They take inputs and return outputs.
5. **Render is build-model → draw-model.** Gathering state and drawing it
   are different phases in different files. Drawing is pure.
6. **Dispatch is one line per arm.** `handle_action`-style match blocks
   contain no logic; each arm calls one subsystem method.
7. **Quarantine coupling.** If some piece of shared state genuinely must be
   passed around (e.g. `SelectionContext` in the rendering plan), put the
   files that touch it in their own subdirectory so the coupling is visible
   from `ls`.
8. **No flag days.** Every phase, and every commit within a phase, must
   compile and pass tests. Each phase is individually revertable.
9. **No speculative abstraction.** Don't introduce a trait for a single
   impl. Don't parameterise over a type with one instantiation. Don't add
   feature flags for backwards compat. If the refactor changes a signature,
   change the call sites.
10. **Don't add comments/docs/tests for code you didn't touch.** The
    refactor's scope is structural. Resist grooming.

The reference plans elevate some of these to "hard rules" with numbers
(Rule 1, Rule 2, …). Do the same — pick the 3–6 rules that matter most for
*your* target and name them. Numbered rules are load-bearing: they give
reviewers something to point at.

## Required sections

Produce a document with these sections, in this order. Each section has a
prescribed purpose; don't skip and don't add new top-level sections unless
the target genuinely demands it.

### 1. Context & measurements

A short paragraph naming the target file(s) and the problem, followed by a
table of concrete measurements (as described above). If the target has a
single mega-method or mega-struct, list its subparts with line counts. No
prose without numbers in this section.

### 2. Why the current shape is wrong (diagnosis)

One to three paragraphs. What specifically makes the current code hard to
work with? Options include:

- God-object coupling (every method in every file can touch every field).
- Mixed concerns (one function both gathers state and renders it).
- A mega-struct passed between files, hiding dependencies.
- Scattered `impl` blocks that look modular but aren't.
- Duplicated logic across similar-but-not-identical code paths.

Name the specific instances (with line numbers or method names). Avoid
generic "it's big" diagnoses — a 5000-line file isn't automatically a
problem; five different concerns fused in a 5000-line file is.

### 3. Proposed field clusters (the core of the plan)

**This is the heart of the refactor.** List every proposed sub-struct,
and for each: which fields of the god struct it absorbs, which scattered
`impl` files today are its implicit home, and a one-line description of
its concern. Example row shape from the editor-modules plan:

```
| New sub-struct | God-struct fields absorbed | Current impl home | Concern |
| MacroState     | macros, macro_recording, last_macro_register, macro_playing | macro_actions.rs | Macro record/replay |
| BookmarkState  | bookmarks, active_custom_contexts | (scattered in mod.rs) | Bookmark navigation |
| LspState       | lsp_config, lsp_servers, lsp_progress, … (25 fields) | lsp_*.rs files | Language-server lifecycle |
```

Every field on the current god struct must appear in exactly one row
(or be explicitly called out as "remains on the god struct" with a
reason). That exhaustiveness is what makes the plan real.

Show the before/after struct definitions side by side. Before: 67 flat
fields. After: ~25 fields, each a sub-struct. The diff is the
deliverable.

### 4. Architectural principles (the hard rules)

Pick 3–6 principles from the list above (or your target's equivalents) and
state them as numbered "Rule N" clauses. Make at least one rule a hard
invariant that can be mechanically checked (grep audit, file-size cap, etc.).
Rule 1 should always be the single-`impl`-file rule for the god type.

### 5. Target shape

Show — in code — what the end state looks like. Minimum content:

- The directory layout after the refactor (`tree`-style).
- The god struct after composition (~25 sub-struct fields, not ~67 raw
  fields).
- A representative sub-struct in full, with its `impl` block, to
  establish the pattern.
- For each coordination pattern you'll use, a 5-line code example.
- A visibility table: which modules may import what (ideally phrased so a
  grep can verify it).

If the plan doesn't show the target shape concretely enough that a
contributor could start today, it's not detailed enough.

### 6. Coordination mechanisms

Enumerate the small, fixed set of patterns you will use to cross subsystem
boundaries. The editor-modules plan names four: orchestrator with split
borrows, read-only context bundle, effects returned by the caller, event
bus. The split-rendering plan names one (quarantined shared carriers).

**Name them, and don't add a fifth mid-refactor.** Decision rules for
"which mechanism for which case" go here.

### 7. File-by-file / method-by-method mapping

A table (or tables) mapping "currently here" → "moves to". Every non-trivial
piece of logic in the target must appear in a row. If you haven't surveyed
the target well enough to fill this table, the plan isn't ready.

Example row shapes from the references:

```
| Currently in mod.rs             | Moves to                           |
| `SearchScanState`, `LineScanState` | `app/search/scan.rs` and `app/buffers/line_scan.rs` |
```

### 8. Handling the realities

Every refactor has 2–4 genuinely hard cases that a naive plan glosses over.
Name them explicitly and describe how you'll handle each. Common categories:

- **Borrow checker.** Where will `&mut self` splits need to destructure
  `Editor { ref mut a, ref mut b, .. }`? Where will you need
  `Effect`-returning methods instead of direct mutation?
- **Cross-cutting mega-methods.** Are there 2–3 methods that touch almost
  everything (like `render`, `handle_action`, `process_async_messages`)?
  Plan each one individually — show what its final shape looks like.
- **Implicit invariants.** What behaviours today rely on "one struct owns
  everything, so ordering is trivial"? Enumerate them (active-buffer
  consistency, undo batching, event-log choke-point, preview promotion,
  etc.) and name the single post-refactor call site that enforces each.
- **Coexistence during migration.** How will old and new patterns coexist
  on `main` between phases? (Usually: old methods become thin delegators
  until the last phase deletes them.)

### 9. Phased execution

One phase per PR-sized unit of work. Every phase must:
- Compile and pass tests on its own.
- Be individually revertable.
- Have a stated risk level (low / medium / high) and blast radius.

Canonical phase ordering (adapt as needed):

1. **Pure helpers first.** Extract functions that are already effectively
   pure but happen to be `&self` methods. Zero risk, establishes the
   pattern, surfaces hidden dependencies early.
2. **Leaf subsystems.** Smallest state clusters, fewest call sites first.
3. **Build-vs-draw split** (if render is in scope).
4. **Flatten dispatchers** (if a mega-match like `handle_action` is in
   scope) — one commit per arm group.
5. **Redistribute grab-bag files** (like `buffer_management.rs`).
6. **Cross-cutting subsystems last** — search, completion, LSP, plugins.
7. **Structural cleanup.** Delete now-empty `*_actions.rs` files, enforce
   the `impl` audit, shrink `mod.rs` to re-exports.

For each phase: list the exact steps, cite the risk, and name the test
coverage you'll rely on (unit tests, visual-regression harness, etc.).

### 10. Success criteria

Measurable, mechanically-checkable criteria. Minimum:

- **`impl` audit.** `rg "impl <GodType>"` across the target returns only
  the single expected file. Non-negotiable.
- **Field count on the god struct drops to the sub-struct count.** State
  the target: e.g. "from 67 flat fields to ≤28 sub-struct fields".
- **No raw-field leakage.** For each extracted sub-struct, a grep for
  `self\.<old_field_name>` outside the owning module returns zero hits.
- File-size cap (no file >N lines in the refactored module).
- Public-API preservation claim (or an explicit list of signature changes
  and their call-site updates).
- All existing tests green at each phase boundary.

### 11. Risks & mitigations (optional, include if non-trivial)

A short list of "this could go wrong, here's what saves us". Local
bookkeeping that's easy to silently break (cursor placement, ANSI parser
state, undo boundaries) belongs here. If a risk has no mitigation beyond
"be careful", say so — don't invent a mitigation.

## Style notes

- Numbers, tables, and code blocks carry the plan. Prose is glue between
  them, not the payload.
- Name concrete methods, files, structs, and line numbers. `foo.rs L100–L400`
  is worth ten paragraphs of "the large method in foo".
- When you state a rule, state it as a rule (hard, numbered). When you
  state a preference, say "prefer". Don't blur the two.
- No emojis. No marketing-speak. No "leverage", "robust", "comprehensive".
- If a section would be empty or trivial for this target, say so in one
  line and move on — don't pad.
- US English. Second-person imperative ("Move X to Y", not "X will be
  moved to Y") matches the existing docs.

## Non-goals (do not do these)

- Do not write the code. The plan is the deliverable.
- Do not propose new features, even tangentially useful ones.
- Do not propose renaming types or files unless the rename is load-bearing
  for the refactor.
- Do not propose introducing new dependencies (crates, frameworks).
- Do not propose a "big-bang" rewrite. If the only phase ordering you can
  find is "do everything at once", the plan is wrong.
- Do not propose a plan whose acceptance criterion is "looks better".
  Acceptance criteria are mechanical.

## Self-check before you submit

Ask yourself these questions. If the answer to any of them is no, revise.

- Did I measure before I planned — specifically, did I list every field
  on the god struct and identify which scattered-`impl` file touches
  which fields?
- Does §3 (field clusters) account for **every** field on the god
  struct? Is every field either assigned to a sub-struct or explicitly
  kept on the god struct with a stated reason?
- Can every claim in §2 (diagnosis) be grounded in a line number or
  method name?
- Does §5 (target shape) show both the shrunken god struct (~25
  sub-struct fields) and at least one representative sub-struct in full?
- Does §7 (mapping table) account for every method over ~50 lines in
  the target?
- Does each phase in §9 compile on its own?
- Does §10 include a grep audit that makes "only one file has
  `impl <GodType>`" mechanically verifiable?
- If I handed this plan to a contributor who doesn't know the history,
  could they start on Phase 1 today without asking me a question?

If yes to all: submit the plan as
`docs/internal/<target>-refactor-plan.md`.

## Reference plans

Read these in full before drafting your own. The structure, tone, and
level of concreteness they exhibit are what this prompt asks for.

- `docs/internal/editor-modules-refactor-plan.md` — four mega-files in
  `crates/fresh-editor/src/app/`. Shows the god-object decomposition case
  and the four-coordination-mechanisms framework.
- `docs/internal/split-rendering-refactor-plan.md` — one 8,635-line file.
  Shows the quarantine strategy (physically segregate coupled code into a
  subdirectory) and the leaf-modules-first phasing.
