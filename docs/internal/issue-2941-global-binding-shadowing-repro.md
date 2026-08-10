# Issue #2941 — global keybindings shadow context-specific ones: reproduction

Interactive reproduction (tmux, release build, isolated `XDG_CONFIG_HOME`/
`XDG_DATA_HOME`, `--no-plugins --no-init --no-restore`) of the resolver-order
problem reported in issue #2941: `KeybindingResolver::resolve`
(`crates/fresh-editor/src/input/keybindings.rs`) probes the *global* tier
before the *context* tier — both for default bindings (default-global before
default-context) and for user bindings (custom-global before custom-context).
Any chord bound in both tiers resolves to the global entry, and the narrower
binding is unreachable.

All three scenarios below were reproduced by driving the real TUI in tmux and
confirmed with `RUST_LOG=fresh::input::keybindings=trace` resolver logs.

## The colliding entries ship in the default keymap

`crates/fresh-editor/keymaps/default.json` binds these chords in two tiers at
once (vscode/macos keymaps inherit them):

| Chord | `when: global`            | narrower binding                                  |
|-------|---------------------------|---------------------------------------------------|
| Alt+F | `menu_open {name: File}`  | `normal`: `move_word_right`, `prompt`: `prompt_move_word_right` |
| Alt+E | `menu_open {name: Edit}`  | `prompt`: `file_browser_toggle_detect_encoding`   |
| Alt+G | `menu_open {name: Go}`    | `prompt`: `live_grep_toggle_regex`                |
| Alt+H | `menu_open {name: Help}`  | `prompt`: `live_grep_toggle_ignored`              |

The `prompt` rows still work in practice only because of a hard-coded
workaround: `dispatch_modal_input` (`crates/fresh-editor/src/app/input_dispatch.rs`,
"Use resolve_in_context_only to bypass Global bindings") intercepts Alt+Char
while a prompt is open and consults the Prompt context directly, skipping the
resolver's normal ordering. The `normal` row has no such rescue.

## Scenario A — stock editor: `Alt+F → move_word_right` (normal) is dead

Open any file, stock config. Buffer line 1: `alpha bravo charlie delta echo
foxtrot`, cursor at Ln 1, Col 1.

- Press **Alt+B** repeatedly (its mirror, `move_word_left`, which has **no**
  global collision): works — status bar shows Col 39 → 32 → 27.
- Press **Alt+F**: the **File menu opens**; cursor stays at Col 27.
  `move_word_right` never fires.

Trace log:

```
KeybindingResolver.resolve: code=Char('f'), modifiers=KeyModifiers(ALT), context=Normal
  -> Found in default global bindings: MenuOpen("File")
```

The default-Normal tier holding `move_word_right` is never consulted.
One can argue the menu mnemonic *should* win here — but then the
`normal`/`prompt` Alt+F entries in the same keymap are dead weight, and
nothing warns about it.

## Scenario B — `menu_bar_mnemonics: false`: Alt+F becomes a fully dead key

This is the clearest user-facing bug, because it breaks a documented promise.
The config option says (`crates/fresh-editor/src/config.rs`):

> Whether menu bar mnemonics (Alt+letter shortcuts) are enabled. …
> **Disabling this frees up Alt+letter keybindings for other actions.**

But the gate is applied at *dispatch*, not resolution
(`crates/fresh-editor/src/app/input.rs`):

```rust
Action::MenuOpen(menu_name) => {
    if self.config.editor.menu_bar_mnemonics {
        self.handle_menu_open(&menu_name);
    }
}
```

With `{"editor": {"menu_bar_mnemonics": false}}`:

- Press **Alt+F** three times: no menu, **no cursor movement** — status bar
  pinned at Ln 1, Col 1. The chord still resolves to the global
  `MenuOpen("File")` (trace shows three `-> Found in default global bindings:
  MenuOpen("File")` hits), which dispatch then silently drops.
- **Alt+B** in the same session still moves by words (Col 39 → 27).

So disabling mnemonics does *not* free up the keys: the global entries keep
winning in the resolver and turn into no-ops, and the keymap's own
`Alt+F → move_word_right` (normal) remains unreachable. This matches the
"silently no-op'd (mnemonics off)" remark in the issue-#2720 comment above
`resolve()`.

## Scenario C — user config: custom-global shadows custom-context too

`--config` with only:

```json
{
  "keybindings": [
    { "key": "n", "modifiers": ["alt"], "action": "move_down",       "when": "global" },
    { "key": "n", "modifiers": ["alt"], "action": "move_word_right", "when": "normal" }
  ]
}
```

In the buffer (context **Normal** — exactly where the narrower binding
applies), **Alt+N** moves the cursor *down* (Ln 1 → 2 → 3); `move_word_right`
never fires:

```
  -> Found in custom global bindings: MoveDown
```

So the issue title holds within a single tier as well: a broad `global`
fallback makes a narrower `normal` binding for the same chord unreachable,
with no warning. (Note the fix for #2720 already ordered *custom-context*
above *default-global*, so a user override of a default mnemonic works; the
remaining problems are global-vs-context *within* each tier, and the
mnemonics-off dead keys of Scenario B.)

`resolve_chord` has the same shape, in a worse order: it probes custom-global,
then **default-global**, then custom-context — so for multi-key chords a
built-in global chord would even shadow a user's context-specific chord
(the single-key ordering fixed by #2720).

## Repro mechanics (for re-running)

```sh
cargo build --release --bin fresh
tmux new-session -d -s repro -x 120 -y 32
printf 'alpha bravo charlie delta echo foxtrot\nsecond line with more words here\n' > /tmp/work.txt
tmux send-keys -t repro "XDG_CONFIG_HOME=/tmp/xc XDG_DATA_HOME=/tmp/xd \
  RUST_LOG=fresh::input::keybindings=trace \
  target/release/fresh --no-plugins --no-init --no-restore \
  --log-file /tmp/fresh-trace.log /tmp/work.txt" Enter
# Alt+F / Alt+B are sent as: tmux send-keys -t repro M-f   /   M-b
# cursor position read from the status bar via: tmux capture-pane -t repro -p
```

Scenario B adds `--config` pointing at the two-line mnemonics-off JSON above;
Scenario C uses the custom-keybindings JSON instead.
