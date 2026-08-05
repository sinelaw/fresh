# Guided Code Tours

> **Palette:** `Tour: Open Workspace Tour...`, `Tour: Load Definition...`, `Tour: Next Step`, `Tour: Previous Step`, `Tour: Exit`
>
> **Keys:** `Ctrl+Alt+N` / `Ctrl+Alt+P` step the tour from anywhere in the editor
>
> **Built-in guide:** `fresh --cmd help tour`

A tour is a walkthrough of a codebase, defined in a JSON file and played inside the editor. It opens as a panel in the Utility Dock: the list of steps on the left, the current step's explanation on the right. Each step points at a file and a line range. When you move to a step, Fresh opens that file in the main area above, scrolls to the range and highlights it, so the code the step is talking about is on screen while you read about it.

![A tour open in the dock, step 4 of 5, with the step's lines highlighted in the source file above](/images/code-tour-panel.png)

*Step 4 of 5. The step's range is highlighted in `router.rs` above; the rail on the left ticks off the steps already visited.*

## Where the JSON comes from

Write the manifest by hand if you want to, but the recommendation is not to. LLMs and coding agents are very good at producing this file on demand, and asking for one takes a sentence:

> "Give me a tour of this PR — start at the entry point and walk through the three files that changed."

> "I have never read the scheduler. Build me a tour of it, ten steps, focused on how a job gets picked up."

An agent launched with **Teach agent the Fresh CLI** (see [Scripting and Agent Control](./scripting.md)) knows how to write the manifest *and* how to open it, so the tour appears in your dock without you touching a file:

```ts
editor.writeFile("/repo/.fresh-tour.json", JSON.stringify(manifest, null, 2));
return await editor.getPluginApi("code-tour").openTour(".fresh-tour.json");
```

Generating a tour on the spot rather than maintaining one has two advantages. It is always current — it is written against the code as it is now, not as it was when someone last updated the file. And it is written for the person asking: you can ask for more or less detail, for a focus on the parts you do not know, or for a tour of only the code a specific change touched.

Checking tours into source control is still worth it for the ones that pay off repeatedly — an onboarding tour of the main request path, an architecture tour that ships with the repo. Both routes produce the same file.

## What tours are good for

- **Reviewing a pull request.** A diff tells you what changed; a tour of the diff tells you why, in the order that makes sense, with each hunk shown in its real surroundings rather than as three lines of context.
- **Onboarding onto a codebase**, or onto an area of it you have not worked in. A tour is a reading order plus commentary, which is the thing a new person is missing.
- **Explaining your own change** to someone else, or to yourself in three months.
- **Bug reports and incident write-ups** — walk the code path that failed.

## Opening a tour

Two commands from the palette (`Ctrl+P`):

- **Tour: Open Workspace Tour...** scans the well-known locations in the workspace. One match opens directly; several offer a picker.
- **Tour: Load Definition...** takes a path to any tour file.

Discovery looks at `.fresh-tour.json`, `.tour`, `main.tour`, `.vscode/main.tour`, and the directories `.tours/`, `.vscode/tours/`, `.github/tours/` (nested directories included).

A script or an agent opens one directly:

```sh
echo 'return editor.getPluginApi("code-tour").openTour(".fresh-tour.json")' \
  | "$FRESH_BIN" --cmd script run
```

![The workspace tour picker listing three tours found in the repository](/images/code-tour-picker.png)

*Each row is a manifest found in one of the well-known locations, with its step count.*

## Moving through a tour

The panel has the step rail on the left and the step's text on the right, a bar showing the current step's file and line range, and a row of buttons: **Jump to code ⏎**, **Re-highlight**, **◀ Prev**, **Next ▶**, **✕ Exit**. On the last step **Next ▶** reads **✓ Finish**.

- Click a step in the rail to jump to it. The file opens and the range highlights.
- `n` / `Space` / `→` for the next step, `p` / `Backspace` / `←` for the previous one, while the panel has focus.
- `Ctrl+Alt+N` and `Ctrl+Alt+P` step the tour **from the editor split**, so you can be reading the code with the panel still up and not have to click back into it.
- `Enter` (**Jump to code ⏎**) moves the keyboard into the source file at the step's location.
- `g` focuses the step rail, `r` re-paints the highlight, `q` or `Escape` closes the tour.
- The explanation panel is a real text area: click to place a caret, drag or `Shift`+arrows to select, `Ctrl+C` to copy exactly what is rendered. It stays read-only.

Step text is markdown, rendered through the same engine as LSP hover docs — headings, lists, emphasis, links and syntax-highlighted code fences all work.

![The tour panel: step rail on the left, markdown explanation with a syntax-highlighted code fence on the right](/images/code-tour-steps.png)

*The step rail, the location bar, the rendered markdown, and the key hints along the bottom.*

## The files it opens are just files

Everything the tour opens is an ordinary buffer in Fresh. Nothing is locked or special-cased.

- Navigate with LSP — go to definition, find references, hover — straight out of a tour step.
- Edit and save. Fixing a typo you spotted mid-tour does not end the tour.
- Open other files, run a search, go read something else entirely. The tour panel stays docked where it was.
- Come back to it whenever, and continue from the step you were on.

The tour lives in its own buffer in the dock, so you can close it like any other buffer and reopen it later. Several tours can be open at once — one dock tab each — which is what you want when a tour of the change refers to a tour of the subsystem it changes.

An unfinished tour comes back on the next launch, at the step you left it on. Closing a tour forgets it, so it does not reappear.

## Manifest format

`.fresh-tour.json`:

```json
{
  "title": "Request pipeline",
  "description": "How a request reaches the handler",
  "schema_version": "1.0",
  "commit_hash": "a1b2c3d",
  "steps": [
    {
      "step_id": 1,
      "title": "Entry point",
      "file_path": "src/main.rs",
      "lines": [1, 40],
      "explanation": "## Where it starts\n\nThe listener is built here.\n\n- binds the socket\n- spawns the accept loop"
    }
  ]
}
```

| Field | Required | Meaning |
|---|---|---|
| `title` | yes | Shown on the dock tab. |
| `description` | yes | One line about what the tour covers. |
| `schema_version` | yes | `"1.0"`. |
| `steps` | yes | Ordered list, at least one. |
| `commit_hash` | no | The commit the tour was written against; drives the drift indicator. |

Each step:

| Field | Required | Meaning |
|---|---|---|
| `step_id` | yes | Unique, 1-indexed. |
| `title` | yes | Shown in the step rail. |
| `file_path` | yes | Relative to the project root. |
| `lines` | yes | `[start, end]`, 1-indexed and inclusive. |
| `explanation` | yes | Markdown, shown in the description panel. |
| `overlay_config` | no | Highlight style — block or line, and whether to dim the surrounding code. |

`fresh --cmd help tour` prints a field reference generated from the schema the binary actually validates against, so it cannot drift from the build you are running. `fresh --cmd help tour --schema` dumps the raw JSON schema, which is the thing to hand to a validator or to an agent writing a manifest.

## VS Code CodeTour files

`.tour` files from the [CodeTour](https://github.com/microsoft/codetour) extension load as well; the format is detected from the content, so there is nothing to configure. On load:

- `line`, `selection` and `pattern` anchors become highlighted line ranges. Patterns are resolved against the file's current content, so those steps survive edits to the file.
- Steps with prose and no code location keep their text and show as content-only steps.
- Untitled steps are named from the first heading in their description.
- A commit-hash `ref` feeds the drift indicator.

## When the code has moved on

A tour is written against a state of the repo, and repos move.

- If the manifest records a `commit_hash` and you are on a different commit, the panel says so: `recorded at <sha> · you are on <sha>`. The tour still plays.
- If a step's file is not in the working tree, the panel says `<file> is not in this working tree` and the step's text stays readable.
- `r` (**Re-highlight**) re-paints a step's highlight after you have edited the file under it.

This is the other argument for generating tours on demand rather than maintaining them: a tour produced ten seconds ago has nothing to drift from.

## See also

- [Scripting and Agent Control](./scripting.md) — how an agent writes and opens a tour for you
- [Git](./git.md) — reviewing diffs and hunks
- [LSP Integration](./lsp.md) — navigation from inside a tour step
- [Navigation](./navigation.md)
