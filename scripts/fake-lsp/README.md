# fake-lsp

Tiny LSP server stubs used by the fresh-editor e2e tests. They speak just
enough of the LSP wire protocol (Content-Length framing + a handful of
methods) to drive a single editor flow without needing a real language
server installed.

## Tools

- `bin/fake-pylsp` — pretends to be `pylsp`. Logs every URI it receives
  (tagged with the request method) to
  `<FAKE_DEVCONTAINER_STATE>/fake_lsp_uris`, and answers
  `textDocument/definition` with a configurable `Location`.

## Pinning the definition target

Tests can pin what the LSP returns for `textDocument/definition` by
writing the desired values into the per-test state dir before
triggering the request:

- `fake_lsp_definition_uri`  — full URI (e.g. `file:///workspaces/proj/util.py`)
- `fake_lsp_definition_line` — 0-based line number (default `5`)
- `fake_lsp_definition_character` — 0-based column (default `0`)

If the files are absent the defaults above kick in. The same state dir
is what `scripts/fake-devcontainer/lib/fake-state.sh` uses, so the
log/config files sit next to `exec_history` and `containers/<id>/`.

## Wiring into a test

The fake LSP runs on the host via the fake docker shim. The shim's
parent (the test process) sets `PATH` so `command -v fake-pylsp`
resolves; tests that capture a `userEnvProbe` PATH must include this
bin dir in the captured value, otherwise the in-container
`command_exists` probe will report the binary as missing.
