# Contributing

Thanks for contributing!

## Workspace Structure

| Crate | Purpose |
|-------|---------|
| `fresh-editor` | Editor library + `fresh` binary (terminal UI, LSP, plugins, …) |
| `fresh-gui` | Standalone windowed backend (winit + wgpu). Defines the `GuiApplication` trait; no dependency on `fresh-editor`. |
| `fresh-core` | Shared core types and plugin API |
| `fresh-languages` | Tree-sitter language grammars |
| `fresh-plugin-runtime` | QuickJS-based plugin runtime |
| `fresh-plugin-api-macros` | Proc-macros for the plugin API |
| `fresh-parser-js` | JavaScript/TypeScript parser |

The `gui` feature on `fresh-editor` pulls in `fresh-gui` as an optional dependency.
When it is disabled (the default), no windowing or GPU crates are compiled.

## Build Profiles & Binary Size

The default `release` build optimizes for size already (`opt-level = "z"`, fat
LTO, `codegen-units = 1`). Two extra knobs are available for producing a
**minimal `fresh` binary** without affecting the normal release:

1. **`min-size` profile** (`[profile.min-size]` in the root `Cargo.toml`)
   inherits `release` and additionally sets `panic = "abort"` (drops unwinding
   tables) and `strip = true` (strips symbols + debuginfo).

2. **Feature trimming.** The size-relevant features on `fresh-editor` are:
   - `plugins` / `embed-plugins` — the QuickJS plugin runtime and the plugins
     embedded into the binary.
   - `tree-sitter` — the ~19 generated tree-sitter grammar crates plus the
     AST-based features (precise indentation, scope-aware reference
     highlighting). When disabled, highlighting falls back to syntect
     (TextMate) grammars and indentation to pattern-based heuristics. The
     shared `Language` / `HighlightCategory` types still come from
     `fresh-languages`, so syntect highlighting is unaffected.

   All three are on by default. To build the smallest binary (syntect-only
   highlighting, no plugins, no tree-sitter):

   ```sh
   cargo build --profile min-size --no-default-features --features runtime
   # -> target/min-size/fresh
   ```

   The normal release is unchanged:

   ```sh
   cargo build --release        # default features, full functionality
   ```

## Commit Hygiene

- Commit messages must describe the **motivation / goal** of each commit, not just what changed
- Prefer separate commits for unrelated bug fixes and new functionality when it's cheap to split; don't force a split when the fix is tightly coupled to the feature
- If your change touches GUI code, also verify: `cargo check --all-targets --features gui`
- Individual commits should pass `cargo check --all-targets`, `cargo fmt`, and `cargo clippy` (the crate denies several lints that `check` alone misses)
- If ignoring a return value (let _ = ... pattern), be sure it's legit in that specific case, and that the return value indeed can be safely ignored.

## Testing

1. **Reproduce Before Claiming**: Every behavioral claim in a commit (bug fix *or* new feature) must be backed by a test that fails (or times out) without the change. The same test should pass with the fix. The reproducer test can be in the same commit as the fix or in a separate commit, as long as the above is true (fails without fix, passes with fix).

2. **E2E Tests Observe, Not Inspect**: Any new user flow must include an end-to-end test that drives keyboard/mouse events and asserts only on rendered output. Do not call accessors that return model, view, or context state — if an invariant isn't visible on screen, cover it with a unit test on the component.

3. **No timeouts or time-sensitive tests**: Use "semantic waiting" (waiting for specific state changes/events) instead of fixed timers to ensure test stability. Wait indefinitely, don't put timeouts inside tests (cargo nextest will timeout externally).

4. **Test isolation**: Tests should run in parallel. Use the internal clipboard mode in tests to isolate them from the host system and prevent flakiness in CI. Same for other external resources (temp files, etc. should all be isolated between tests, under a per-test temporary workdir).

**Tip**: For manual reproduction/validation you can use tmux + send-keys + render-pane to script ad-hoc tests on the UI, for example when trying to reproduce an issue. This can help understand how to write an e2e test.

## Code Guidelines

1. **Cross-Platform Consistency**: Don't hard-code platform-variant primitives — newlines, path separators, line endings, case sensitivity. Consider and use the buffer mode (CRLF vs LF, language, etc), `std::path` APIs, and their relatives.

2. **Avoid full-buffer scans**: The editor is designed to handle huge files via lazy, viewport-localized operations. Prefer algorithms that operate on visible/relevant ranges rather than scanning the entire buffer.

3. **LSP**: Ensure LSP interactions follow the correct lifecycle (e.g., `didOpen` must always precede other requests to avoid server-side errors). Use the appropriate existing helpers for this pattern.

4. **Use the `FileSystem` trait for all filesystem access**: Never use `std::fs` directly in editor code. The `FileSystem` trait (`model/filesystem.rs`) abstracts over local and remote (SSH) filesystems. Use it for reading files, listing directories, checking metadata, etc. This ensures features work transparently on remote hosts.

5. **Use `ProcessSpawner` for spawning external commands**: Never use `std::process::Command` directly. The `ProcessSpawner` trait (`services/remote/spawner.rs`) routes process execution through either `LocalProcessSpawner` or `RemoteProcessSpawner`, so plugins and core features (like `git ls-files` in the file finder) work on remote hosts.

6. **Regenerate plugin types and schemas**: After modifying the plugin API or config types:
   - **TypeScript definitions** (`plugins/lib/fresh.d.ts`): Auto-generated from Rust types with `#[derive(TS)]`. Run: `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`
   - **JSON schemas** (`plugins/config-schema.json`, `plugins/schemas/theme.schema.json`): Auto-generated from Rust types with `#[derive(JsonSchema)]`. Run: `./scripts/gen_schema.sh`
   - **Package schema** (`plugins/schemas/package.schema.json`): Auto-generated from Rust types with `#[derive(JsonSchema)]`. Run: `./scripts/gen_schema.sh`

7. **Type check plugins**: Run `crates/fresh-editor/plugins/check-types.sh` (requires `tsc`)

8. **Enumerate cross-cutting state**: Before shipping a mutation, list every other subsystem that holds a reference to what you changed (open buffers, LSP sessions, cursors, cached IDs, background watchers) and update or invalidate them. Stale references are the single most common source of follow-up PRs.

9. **Narrow recovery paths**: When you add a fallback or retry, trigger it on the *specific* error it was designed for, not on `Err(_)` or catch-all branches. Broad recovery silently hides correctness bugs.

10. **Locale keys go in every locale**: i18n `t!()` keys - update *all* files under `crates/fresh-editor/locales/` with real translations. Don't commit English placeholders.

11. **Re-read through the owner, not a stale snapshot**: After changing state others cache (config, layout, cursor-derived values), it's usually safest to refresh it through the path that owns it before reading the effect. If a test can't see the change on screen, treat it as suspect.

12. **When a bug recurs, consider centralizing**: A class that keeps coming back (stale cache, off-screen cursor, missing gate) often belongs in one shared primitive rather than another per-site patch — and gates/ordering are usually best enforced at a single fork.

13. **Watch the other variants**: A fix for the local path often needs the remote one too — likewise daemon vs direct, the trimmed/`gui` feature sets vs default, and other platforms. Prefer branching on the real distinction over assuming the common case.

14. **Cleanup usually means teardown**: Cancel/abort/drop paths generally need to actually stop the work (kill the child, release the resource), not just discard a late value.

15. **Lean toward unrepresentable over asserted**: Where practical, prefer types that rule out invalid states (non-`Clone`, per-owner ownership, enums) over runtime guards, and avoid exposing half-initialized state.

16. **Be wary of fixes you can't reproduce**: Without a reproducer it's easy to fix the wrong thing — and a reproducer that passes vacuously (an already-satisfied wait, a timeout) proves little.

17. **Avoid raw byte-offset string slicing**: Prefer the shared char-boundary/width helpers over `s[..n]` — ad-hoc truncation has been a recurring source of multibyte panics.
