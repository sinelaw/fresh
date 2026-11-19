# Orchestrator dock vs. the Claude Desktop session sidebar: a UX and layout comparison

> _Review note, 2026-11-19. Compares the left-hand session list in the Claude
> Desktop app's Code tab (and claude.ai/code) with Fresh's Orchestrator dock.
> The focus is visual layout and interaction: row anatomy, truncation,
> right-edge alignment, status, pinning and grouping. Feature parity comes
> second. It ends with ranked recommendations and code pointers._

## Outcome

Implemented in sinelaw/fresh#3826. That PR covers recommendations 1–7 here,
plus the following, which went further than this review recommended or
differs from it:

- **The status symbol moved to the right after all.** It now shares one
  constant right-hand column with the location marker, after the git
  summary. Idle and unopened-worktree rows leave that cell blank (no `·`
  or `○`), so the symbols that remain are the ones that ask for attention.
- **"clean" and "· on-disk worktree" are gone from the dock.**
- **New: By date grouping,** dated by the last time you worked in a
  workspace (typing or editing), not the last time you looked at it.
- **New: an agent waiting on a first-run screen now shows "needs you",**
  and an agent with no conversation offers **Restart**, not **Resume**.

## Method and confidence

**Fresh.** I installed **Fresh 0.5.9** with the getfresh.dev one-liner
(`curl … scripts/install.sh | sh`, universal musl build). I drove it
interactively in a 140×40 tmux pane, using keystrokes and SGR mouse clicks and
drags. The test repo had a `main` checkout, three worktrees (two with
deliberately long branch names) and a Claude Code agent workspace. Every Fresh
claim below was either seen on screen or read in
`crates/fresh-editor/plugins/orchestrator.ts` and
`crates/fresh-editor-core/src/widgets/render.rs`.

**Claude Desktop.** The app can't run in this container. The baseline is the
behaviour you described (the label fades as it nears the right edge; label on
the left, status and location icons pinned and aligned on the right), checked
against the official docs, the Desktop changelog, and user-filed issues on
`anthropics/claude-code`. Confidence tags:

| Tag | Meaning |
| --- | --- |
| **[doc]** | Official docs or changelog |
| **[issue]** | Community report; may describe an older build |
| **[obs]** | Your observation of the current app |

The sidebar changed a lot between June and November 2026. The Oct 13–15
"sections" release (2.37337.1 → 2.42304.0) moved the status dot and PR icon
from the left of the title to the right edge, and drew pushback (issues
#102280, #102237).

---

## 1. Row anatomy side by side

### Claude Desktop (Code tab), current build

```
┌ Sidebar ─────────────────────────────┐
│ + New session                        │
│ ▾ Pinned                             │
│   </> Fix login redirect lo░░   ⎇ ●  │  ← title fades out; icons fixed right
│ ▾ fresh                         +    │  ← group header: chevron, "+" on hover
│   </> Refactor dock card rend░░ ⎇ ●  │
│   </> Bump deps                 ⎇ ○  │
│       cloud · env-name               │  ← cloud rows: environment subtitle
└──────────────────────────────────────┘
```

- **Left:** a `</>` session glyph, then the title. **[issue #102280]**
- **Right:** the status dot and git/PR icon, flush right, in one column down
  the list. **[obs]**, **[issue #107674]**
- **Truncation:** the title fades out (a gradient mask) before the right-hand
  icons, instead of ending in "…". **[obs]**, **[issue #104557]**
- **Hover:** shows a menu with Archive (and Delete on the web). **[doc]**
- **Right-click → Open in:** Split view, New window, Cloud, or an editor. **[doc]**
- **Rename:** done from the title bar, not the row. **[doc]**

### Fresh Orchestrator dock: Compact view (the default)

From tmux, dock 38 columns wide:

```
Orchestrator  Menu ▾                 ×│
[ + New ]                     / search│
──────────────────────────────────────│
· proj                                │
○ feat/rate-limit-very-long-branch-na…│
○ fix/auth-bypass-in-login-handler-fo…│
○ perf/db-pool  · on-disk worktree    │
· proj-1 · claude                     │
▼ Claude  (1)                         │   ← agents running outside Fresh
 * fresh-66                           │
          ↑↓ switch  →← fold          │   ← key-hint footer
          Enter edit  F2 menu         │
```

- **Left:** the state glyph (`● ✓ * · ! ○ ?`), an optional remote glyph
  (`⇅` ssh, `⎈` k8s, `⬢` devcontainer), then the name.
- **Right:** nothing. Compact rows have no right-aligned group at all.
  Secondary tags such as `· on-disk worktree`, the remote target or "creating…"
  trail after the name on the same line.
- **Truncation:** a hard end cut with `…` at the dock edge. Whatever trails the
  name is cut first: `fix/auth-bypass-in-login-handler-fo…` lost both the end
  of the branch and its `on-disk worktree` tag.

### Fresh Orchestrator dock: Cards view

Same dock, 55 columns wide (excerpt):

```
╭─────────────────────────────────────────────────────╮
│○ feat/rate-limit-very-long-branch-name-for-truncati…│   ← "clean" gone entirely
│▣ proj                             · on-disk worktree│
╰─────────────────────────────────────────────────────╯
╭─────────────────────────────────────────────────────╮
│○ fix/auth-bypass-in-login-handler-for-legacy-sso cl…│   ← status cut mid-word
│▣ proj                             · on-disk worktree│
╰─────────────────────────────────────────────────────╯
╭─────────────────────────────────────────────────────╮
│○ perf/db-pool                                  clean│   ← short name: aligned
│▣ proj                             · on-disk worktree│
╰─────────────────────────────────────────────────────╯
```

The same cards with the dock dragged to 26 columns:

```
╭───────────────────────╮│
│· proj            clean││
│▸ main                 ││
╰───────────────────────╯│
╭───────────────────────╮│
│○ feat/rate-limit-very…││   ← right group lost
│▣ proj · on-disk workt…││   ← right group cut
╰───────────────────────╯│
╭────────────────────────╯   ← active card opens into the editor
│· proj-1 · claude clean
│▸ proj-1
╰────────────────────────╮
```

- Each card has two content rows, each with a left group and a right group.
- The host right-aligns the right group (`align: "between"` in
  `render_tree_card`, `render.rs:2931`).
- The active workspace's card is drawn **without its right border**, so it
  joins the editor like an attached tab. This is a nice touch Claude Desktop
  has no equivalent of.

---

## 2. The visual details you called out

### 2.1 Label truncation: fade vs. ellipsis

| | Claude Desktop | Fresh dock |
| --- | --- | --- |
| How the label ends | Fades into the background before the right-hand icons **[obs]** | Hard cut ending in `…` |
| What gets cut | Only the title; the icons are never touched | The **end of the row**, which is the status (see 2.2) |
| Where the cut lands | Before a reserved icon column | At the dock border |
| Width awareness | CSS, so always exact | Exact at the host for cards; the plugin *estimates* width for the branch cap (`cardInnerColsEstimate`, `orchestrator.ts:2567`) |

The fade has a practical advantage beyond looks: it doesn't spend a column on
`…`, and the eye reads a fade as "continues" rather than as part of the name.
A terminal can't do alpha. On a truecolor theme you can get close by blending
the last 2–3 cells' foreground toward the row background (for example 66% and
33% of the original colour). It falls back to `…` on 256-colour themes.

### 2.2 Right-edge indicators that always line up

**This is the biggest difference.**

- **Claude Desktop** reserves the right-hand column first and gives the title
  whatever is left, so the status and PR icons sit in a straight column on
  every row whatever the title's length. **[obs]**
- **Fresh** does the reverse:
  - `render_tree_card` pads the gap between the two groups only when the row
    fits.
  - On overflow it leaves one separating space and "falls through to the
    usual end-truncation" (comment at `render.rs:2934-2937`).
  - Because the right group is the *end* of the row, it is what gets cut.
    Above: `…legacy-sso cl…` (status cut mid-word) and
    `feat/rate-limit-very-long-branch-name-for-truncati…` (status gone
    entirely).
  - The plugin works around this on line 2 by capping the branch with an
    estimated width (`sessionCardExtraLines`, `orchestrator.ts:2650`), but not
    on line 1, where the workspace name sits. Line 1's right group (git
    summary or `↵ Retry`) is the one that disappears.
  - In **Compact** view there is no right column at all, so there is nothing
    to align.

**Net effect:** at a glance, Claude's list scans as two clean columns, label
and status. Fresh's cards scan as aligned only while every name fits, and
compact never puts status on the right.

### 2.3 Where the status glyph lives

- **Fresh:** the state glyph sits in a fixed **left** margin
  (`STATUS_MARGIN_W = 2`, `orchestrator.ts:3679`). The left edge is perfectly
  aligned, so it scans as a column in every view and at every width, and it
  can never be truncated.
- **Claude Desktop:** the glyph moved left → right in October. Several users
  asked for it back because the left position "formed a scannable column"
  (**[issue #102280/#102692]**, still open).
- **Verdict:** Fresh's left placement is the stronger default for *state*. The
  right edge is better for secondary facts (location, PR, diff). Claude now
  puts everything on the right.

### 2.4 Status vocabulary and colour

| State | Fresh dock | Claude Desktop |
| --- | --- | --- |
| Needs you | **●** red, the only glyph meant to pull the eye | Dot: orange, yellow or red depending on build **[issue]**; "needs-input marker" **[doc]** |
| Working | **\*** amber | Blue or orange dot, or a shimmer on the row **[issue #102126]** |
| Finished, unseen | **✓** green | Unread toggle exists **[doc]**; colour varies by build |
| Idle | **·** dim | Grey dot, or no dot |
| Failed | **!** with the reason and **Retry** on the row | Red, in the Projects/agent view **[doc]** |
| Not opened yet | **○** (on-disk worktree) | n/a |
| Unknown or exited | **?** | n/a |

- **Fresh** encodes state by **shape and colour**.
- **Claude's** sidebar dots are about 8px and colour-only, a reported WCAG
  1.4.1 failure (**[issue #83570]**, which proposed exactly Fresh's approach:
  spinner, `!`, `✓`).
- **Observed in Fresh:** while Claude Code sat on its first-run theme picker
  and then its *"Do you trust this folder?"* prompt, the dock showed **·
  idle**, not **● needs you**. Those prompts block the agent as much as a
  permission prompt does. After Claude exited, the row became **?** and the
  status bar offered `⟳ Resume claude (exit 1)`. Resuming failed with "No
  conversation found with session ID …", because the agent had exited before
  any conversation existed.

### 2.5 Location indicator

- **Claude:**
  - Branch/PR state is an icon on the right; on pinned rows it is replaced by
    `</>` (**[issue #103393]**).
  - Cloud rows add an environment subtitle, which users complain truncates the
    title while showing the redundant environment in full (**[issue
    #102830]**).
  - Diff stats and CI live in the session's side panel, not the row **[doc]**.
- **Fresh:**
  - Compact shows location only as a left remote glyph plus a trailing target
    that gets cut.
  - Cards show it properly on line 2: `▸ branch` (or `▣ project`) on the left
    and the PR badge or `on-disk worktree` on the right.
  - Line 1's right group carries the git summary (`clean`, `↑2 +14`).
  - Cards are richer than Claude's rows, but cost **4 screen rows each** with
    borders, against Claude's one. In 40 rows, Fresh's card view showed five
    workspaces.

### 2.6 Group headers and nesting

- **Fresh:**
  - Folders and external-agent groups (`▼ Claude  (1)`) have a disclosure
    chevron and a count.
  - Their children are indented 2 columns, and their cards are inset.
  - A folded group rolls up `●n ✓n`, so it still signals "needs you".
- **Claude:** its most-upvoted sidebar complaint is that group headers look
  like session rows and children aren't indented (**[issue #102631]**, about
  17 👍). Users patch it with DevTools CSS.
- **Verdict:** Fresh already solves this.

### 2.7 Hover and affordances

- **Claude** hides row actions (the `⋯` menu, Archive) until hover. That keeps
  rows clean, and the icon column stays put.
- **Fresh** has no hover state on rows. Actions come from right-click or `F2`
  (Visit, Rename, Move to Folder, Archive, Delete). The key-hint footer
  (`↑↓ switch →← fold / Enter edit F2 menu`) makes this discoverable, at a cost
  of two permanent rows.

---

## 3. Feature comparison

| Area | Claude Desktop sidebar | Fresh Orchestrator dock |
| --- | --- | --- |
| **Pin sessions** | ✅ Pinned section at the top; first 20, then "Show more" **[doc]** | ❌ No pin-to-top in the dock. The only pins are Workboard *column* pins (`⊙`), a different idea. |
| **Archive** | Hover menu or ⌘K; opt-in auto-archive when a PR merges or closes; "Archive all" **[doc]** | Row menu → Archive (asks first; the worktree is set aside); restore only via script |
| **Delete** | Web ⋯ menu; "Delete all" **[doc]** | Row menu → Delete, choosing whether the worktree goes; the branch is always kept |
| **Rename** | From the title bar **[doc]** | Row menu → Rename…; otherwise auto-named `<workspace> · <terminal title>` |
| **Grouping** | Group by Date, Status, Folder, Environment or None; custom sections **[issue #104573]** | Hand-made nested folders (drag or Move to Folder); external groups per agent; project filter. No auto group by status or date in the dock (the Workboard has it). |
| **Sort** | Group-and-sort menu; project drag-reorder; A→Z **[doc]** | Stable order (deliberately doesn't reshuffle); no sort control |
| **Search** | ⌘K palette search across sessions, including archived **[doc]**; no in-sidebar box | `/` filters the dock in place |
| **Switching** | Click, Ctrl+Tab, ⌘⇧[ ] **[doc]** | `↑`/`↓` **switches live as you move**; `Alt+O` moves focus in and out |
| **Attention routing** | OS notification; push via Dispatch **[doc]** | Status-bar beacon `● 2 waiting ✓ 1 done`, Jump to Attention / Jump Back, optional bell |
| **Collapse sidebar** | ⌘B **[doc]** | `×`, View menu, Toggle Dock; remembered across runs |
| **Resize** | Not documented | Drag the divider (minimum 24 cols); cards re-fit |
| **Density** | One row per session (+ subtitle for cloud) | Compact: 1 row. Cards: 4 rows. |
| **Parallel / nested** | Child sessions under the parent; split view **[doc]** | Worktree per workspace; runs in the daemon; background launch (`Alt+Enter`) |
| **Sessions from elsewhere** | Local, SSH and cloud in one list | Lists Claude/Codex sessions running elsewhere (CLI, Desktop, cloud) under their own groups; Import sessions scans transcripts and tmux panes |
| **Overview board** | Projects overview / agent view **[doc]** | Workboard: Inbox, Projects, Folders and kanban lenses |

---

## 4. Where Fresh is ahead

1. **Shape-coded state in a fixed left column.** It is accessible, scannable,
   and can't be truncated.
2. **Live switching on arrow keys** plus Jump to Attention / Jump Back.
3. **A distinct, indented group hierarchy** with a folded "needs you" roll-up.
4. **The active card joins the editor**, so "where am I" needs no second
   marker.
5. **A resizable dock** and two densities.
6. **Failed creates stay in the list** with the reason and `↵ Retry`.
7. **External sessions**, including Claude Desktop's own, appear in the same
   list.

## 5. Where Claude Desktop is ahead (visually)

1. **The right-hand icon column is never pushed off or cut.**
2. **The fade-out title** wastes no `…` column and reads as "continues".
3. **Pinning.**
4. **Row actions on hover** keep rows clean without a hint footer.
5. **One-line rows that still carry status and location** at the right edge.

---

## 6. Recommendations, ranked

1. **Reserve the right group before truncating the left (cards).**
   - In `render_tree_card` (`render.rs:2931-2965`), when an `align: "between"`
     row overflows, truncate the **left** group to
     `inner_width − width(right) − 1` and add the `…` (or fade) there.
   - The right group always survives, and every row's right edge stays in one
     column at any dock width.
   - This also retires the plugin-side estimate in `sessionCardExtraLines` and
     `cardInnerColsEstimate` (see `orchestrator-layout-asks.md`).
   - Priority: high. It is the defect visible in the captures above.
2. **Give compact rows a right slot.**
   - Use `cardSplitRow`-style `between` rows in `sessionNodeEntry` for one
     narrow right facet: the PR number, `↑n`, or the location (`⇅ host`).
   - Leave the state glyph on the left. This matches Claude's one-line
     density without giving up Fresh's left status column.
3. **Fade instead of `…`, behind a theme capability check.**
   - On truecolor, blend the last 2–3 cells of the truncated left group toward
     the row background.
   - Otherwise keep `…`.
   - Owned by the host, so lists, the Workboard and the file explorer could
     share it.
4. **Pin to top.**
   - Add **Pin / Unpin** to the row menu (`F2`) and a **Pinned** pseudo-folder
     at the top of the dock that ignores the project filter.
   - Persist it like folders.
   - Pick a glyph or wording that doesn't collide with the Workboard's `⊙`
     column pins.
5. **Treat agent first-run and trust prompts as "needs you".**
   - Claude Code's theme picker and folder-trust prompt showed `·` idle.
   - Either recognise them, or have Fresh pre-trust worktrees it created
     itself.
   - Also hide or relabel `⟳ Resume` when the agent exited before a
     conversation existed, since resuming then fails.
6. **Hover affordance (optional).**
   - If the host gets mouse-motion events, show a `⋯` in the right slot on
     hover, replacing the facet the way Claude does.
   - This would let the `F2`/`→←` hint footer collapse to one row or hide
     after first use.
7. **Optional relative time.**
   - Neither dock shows time. The Workboard already tracks time in state, so a
     `5m` facet in the right slot (or on card line 2) is cheap.

## Sources

- **Claude Code Desktop docs:** <https://code.claude.com/docs/en/desktop>
- **Claude Code on the web:** <https://code.claude.com/docs/en/claude-code-on-the-web>
- **Agent view (state vocabulary, PR label at the right edge):** <https://code.claude.com/docs/en/agent-view>
- **Projects overview:** <https://code.claude.com/docs/en/claude-projects>
- **VS Code extension (hover rename/archive, status dots):** <https://code.claude.com/docs/en/vs-code>
- **Desktop changelog (pinned list and Show more, ⌘B, sections release, Archive all):** <https://claude.com/docs/cowork/changelog>
- **Desktop redesign announcement (Apr 2026):** <https://claude.com/blog/claude-code-desktop-redesign>
- **Community issues on `anthropics/claude-code`:**
  - #102280, #102237, #102692: status dot moved to the right
  - #103393: PR icon missing on pinned rows
  - #102830: environment subtitle truncates the title
  - #104557: title gradient
  - #102126: shimmer replaced the dots
  - #102631: headers vs rows
  - #104573: group-and-sort options
  - #107674: right-side icons, PR state on hover
  - #83570: colour-only dots
- **Fresh:**
  - `docs/features/orchestrator.md`
  - `docs/features/workboard.md`
  - `crates/fresh-editor/plugins/orchestrator.ts`, especially:
    - `sessionNodeEntry` (~2473)
    - `cardSplitRow` (~2540)
    - `sessionCardExtraLines` (~2613)
    - `STATE_SYMBOL` (~3664)
  - `crates/fresh-editor-core/src/widgets/render.rs`: `render_tree_card` (~2896)
