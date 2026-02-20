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

## Commit Hygiene

- Commit messages must describe the **motivation / goal** of each commit, not just what changed
- Separate bug fixes from new functionality into distinct commits
- Individual commits should pass `cargo check --all-targets` and `cargo fmt`
- If your change touches GUI code, also verify: `cargo check --all-targets --features gui`

## Testing

1. **Reproduce Before Fixing**: When fixing bugs, add tests (e2e if it's a user-facing bug) — **first** reproduce the issue in a test that fails or times out, **then** add the fix that makes the test pass.

2. **E2E Tests for New Flows**: Any new user flow or feature must include an end-to-end (e2e) test. E2E tests send keyboard/mouse events and examine the final rendered output, do not examine internal state.

3. **No timeouts or time-sensitive tests**: Use "semantic waiting" (waiting for specific state changes/events) instead of fixed timers to ensure test stability. Wait indefinitely, don't put timeouts inside tests (cargo nextest will timeout externally).

4. **Test isolation**: Tests should run in parallel. Use the internal clipboard mode in tests to isolate them from the host system and prevent flakiness in CI. Same for other external resources (temp files, etc. should all be isolated between tests, under a per-test temporary workdir).

**Tip**: For manual reproduction/validation you can use tmux + send-keys + render-pane to script ad-hoc tests on the UI, for example when trying to reproduce an issue. This can help understand how to write an e2e test.

## Code Guidelines

1. **Cross-Platform Consistency**: Avoid hard-coding newline or CRLF related logic, consider the buffer mode.

2. **Avoid full-buffer scans**: The editor is designed to handle huge files via lazy, viewport-localized operations. Prefer algorithms that operate on visible/relevant ranges rather than scanning the entire buffer.

3. **LSP**: Ensure LSP interactions follow the correct lifecycle (e.g., `didOpen` must always precede other requests to avoid server-side errors). Use the appropriate existing helpers for this pattern.

4. **Regenerate plugin types and schemas**: After modifying the plugin API or config types:
   - **TypeScript definitions** (`plugins/lib/fresh.d.ts`): Auto-generated from Rust types with `#[derive(TS)]`. Run: `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`
   - **JSON schemas** (`plugins/config-schema.json`, `plugins/schemas/theme.schema.json`): Auto-generated from Rust types with `#[derive(JsonSchema)]`. Run: `./scripts/gen_schema.sh`
   - **Package schema** (`plugins/schemas/package.schema.json`): Manually maintained - edit directly when adding new language pack fields

5. **Type check plugins**: Run `crates/fresh-editor/plugins/check-types.sh` (requires `tsc`)
