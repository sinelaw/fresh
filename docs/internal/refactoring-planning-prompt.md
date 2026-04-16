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
using `Grep`, `Glob`, and `Read`. Gather:

- Lines per relevant file (`wc -l` via Bash, or the Read line count).
- Counts and sizes of the largest methods / `impl` blocks in the target.
- Count of fields on any struct that looks like a god object.
- Count of methods on each `impl` block.
- Which files contain `impl <TargetType>` (use Grep). If more than one,
  that's a smell — note it.
- Any obvious shared "mega-struct" types (contexts with >10 fields passed
  around between functions).
- External call sites of the module's public API (so you know the blast
  radius of signature changes).

Put the raw measurements in a table in §1 of your plan. If you can't
measure something cheaply, say so — don't guess.

## Principles the plan must uphold

These are the same principles the two reference plans are built on. Apply
them — and, where a principle is already well-stated in one of the
reference plans, quote it by reference rather than re-deriving it.

1. **State ownership.** Each subsystem owns its own data in its own type.
   Other subsystems cannot reach in.
2. **Explicit dependencies.** When A needs something from B, it appears in
   the function signature. Not `self.b_field`. Not `Rc<RefCell<B>>`. Not a
   back-pointer. A function signature.
3. **Single `impl` file per type.** Only one file may contain
   `impl <GodType>`. Enforce it with a grep audit listed in the success
   criteria.
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

- God-object coupling (every method can touch every field).
- Mixed concerns (one function both gathers state and renders it).
- A mega-struct passed between files, hiding dependencies.
- Scattered `impl` blocks that look modular but aren't.
- Duplicated logic across similar-but-not-identical code paths.

Name the specific instances (with line numbers or method names). Avoid
generic "it's big" diagnoses — a 5000-line file isn't automatically a
problem; five different concerns fused in a 5000-line file is.

### 3. Architectural principles (the hard rules)

Pick 3–6 principles from the list above (or your target's equivalents) and
state them as numbered "Rule N" clauses. Make at least one rule a hard
invariant that can be mechanically checked (grep audit, file-size cap, etc.).

### 4. Target shape

Show — in code — what the end state looks like. This is the most important
section. Minimum content:

- The directory layout after the refactor (`tree`-style).
- The key struct(s) with their fields.
- For each coordination pattern you'll use, a 5-line code example.
- A visibility table: which modules may import what (ideally phrased so a
  grep can verify it).

If the plan doesn't show the target shape concretely enough that a
contributor could start today, it's not detailed enough.

### 5. Coordination mechanisms

Enumerate the small, fixed set of patterns you will use to cross subsystem
boundaries. The editor-modules plan names four: orchestrator with split
borrows, read-only context bundle, effects returned by the caller, event
bus. The split-rendering plan names one (quarantined shared carriers).

**Name them, and don't add a fifth mid-refactor.** Decision rules for
"which mechanism for which case" go here.

### 6. File-by-file / method-by-method mapping

A table (or tables) mapping "currently here" → "moves to". Every non-trivial
piece of logic in the target must appear in a row. If you haven't surveyed
the target well enough to fill this table, the plan isn't ready.

Example row shapes from the references:

```
| Currently in mod.rs             | Moves to                           |
| `SearchScanState`, `LineScanState` | `app/search/scan.rs` and `app/buffers/line_scan.rs` |
```

### 7. Handling the realities

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

### 8. Phased execution

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

### 9. Success criteria

Measurable, mechanically-checkable criteria. Minimum:

- A grep audit that must return an expected set (often empty) of results.
  Example from the references: `rg "impl Editor" crates/fresh-editor/src/app/`
  must return only `app/editor.rs`.
- File-size cap (no file >N lines in the refactored module).
- Public-API preservation claim (or an explicit list of signature changes
  and their call-site updates).
- All existing tests green at each phase boundary.

### 10. Risks & mitigations (optional, include if non-trivial)

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

- Did I measure before I planned?
- Can every claim in §2 (diagnosis) be grounded in a line number or
  method name?
- Does §4 (target shape) contain at least one code block showing the end
  state of the key type?
- Does §6 (mapping table) account for every method/type over ~50 lines in
  the target?
- Does each phase in §8 compile on its own?
- Is at least one success criterion in §9 mechanically checkable?
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
