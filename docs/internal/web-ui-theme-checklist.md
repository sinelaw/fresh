# Web UI theme checklist

Purpose: an exhaustive, element-by-element visual checklist for **building and
reviewing a web theme** (`web-ui/css/9x-theme-*.css` + tokens in
`web-ui/js/15-theme.js`). Every native chrome surface the web UI renders is
listed with the visual dimensions that must be deliberately styled —
**surface/fill, text, secondary text, font, padding, margin, gaps, borders /
hairlines, radius, icons, shadow, and the hover / selected / disabled / focused
states**. Walk it top-to-bottom against a live build in each theme; anything you
can't tick is an unstyled or mis-styled element.

See `web-ui/README.md` §"Web themes" for the mechanism and
[web-ui.md](web-ui.md) for the architecture. The buffer interior is **not** a
theme surface — it is painted from the editor's **TUI colour theme** (see
[config-themes-settings.md](config-themes-settings.md)); this doc covers the
**chrome** only, plus the handful of buffer-adjacent surfaces a web theme does
own (pane background, caret, scrollbar, separators).

---

## 0. How to use this

1. Build and serve (`web-ui/test/run.sh` starts the bridge; or `fresh --web`).
2. Switch to the theme under review (the top-right pill, `Ctrl/Cmd+Alt+T`, or
   `window.fresh.setWebTheme('name')` from the console).
3. Drive every surface into view (menus, palette, settings, popups, the dock,
   context menus, the trust dialog…). `web-ui/test/drive.mjs` exercises all of
   them and drops screenshots in `$SHOTS`; capturing per-theme screenshots there
   is the fastest way to review.
4. For each element below, verify **every** listed dimension in **both** the
   light and dark ends of the theme (if it has them) and against the **default
   dark and a light TUI colour theme** (chrome tokens the theme leaves to
   `applyTheme()` follow the TUI palette).

**Golden rules (learned the hard way):**

- **Never let a chrome control inherit `var(--bg)`.** `--bg` is the *buffer*
  background (kept at the TUI theme's dark colour, deliberately). Several
  controls default to it — `.w-text` (search field), `.settings-modal .set-pill`
  / `.set-field` — and render as **black chips** on a light chrome. Repoint them
  at a chrome surface.
- **The buffer stays monospace.** `svg.cells` is pinned to `--mono-family`; a
  theme may repoint the chrome `--font-family` at a proportional stack, but must
  not touch the cell font. Set `letter-spacing:normal` on chrome when using a
  proportional font (the grid's `--cell-tracking` is for monospace).
- **Menu/dropdown items are absolutely positioned at cell rects** (`place()`),
  so you cannot inset their highlight with margin — use an inset `::before`
  pill (see §4b).
- **Selection language is not one thing.** macOS, for example, uses a *soft
  neutral* selection in persistent sidebars and *accent* only in menus /
  buttons / the focused control. Decide the language per surface class (§20).
- **Decorative circles need a transparent stop.** A radial-gradient dot with no
  `transparent` end fills its box and renders a **square** (this bit the macOS
  traffic lights). Fade to transparent at the radius.

---

## 1. Design tokens (the palette contract)

A theme is (a) a `theme-<name>` body class driving structural CSS and (b) an
inline token set layered over `applyTheme()` by `applyWebTheme()`. Verify the
token set is complete and internally consistent.

- [ ] **Backgrounds:** `--bg` (buffer — usually leave to TUI), `--bg2`
  (popovers/menus base), `--bg3` (menu/sidebar base), `--shell` (frame bars).
- [ ] **Text:** `--fg` (label), `--muted` (secondary label), `--status-fg`.
- [ ] **Elevated surfaces:** `--surface`, `--surface-2` (derived from bg/fg by
  default; override if the theme wants specific greys).
- [ ] **Lines:** `--border`, `--hairline`, `--hairline-strong`.
- [ ] **Interaction:** `--hover`, `--sel`, `--sel-ring`, `--menuhi`.
- [ ] **Accent:** `--accent`, `--ui-accent`, `--on-ui-accent`, `--on-accent`,
  `--on-sel`, `--ok` (status/connection dot green).
- [ ] **Shape/depth:** `--r-sm`, `--r-md`, `--r-lg`, `--shadow`.
- [ ] **Fonts:** `--font-family` (chrome — safe to change), `--mono-family`
  (buffer — never change), `--font-size` (live, zoom-driven).
- [ ] **Readability:** every `--on-*` colour is legible on its paired fill
  (check the accent-filled primary button, toggles, selected rows).
- [ ] **Density (optional):** `webThemeScale` multiplier (Compact uses 0.92);
  confirm the grid re-fits after a theme switch.

---

## 2. Global / window shell

- [ ] **Body background** (`body`, and any theme-specific desktop/wallpaper):
  fills gaps behind translucent chrome; correct in light & dark.
- [ ] **`#app` geometry:** inset (Cosmos bezel) vs full-bleed (macOS/Compact).
  If inset for a decorative frame, `APPY`/`APPX` still map cells correctly
  (hit-testing exact).
- [ ] **Decorative window frame** (theme-specific — Cosmos `#device`, macOS
  `#mactitle`): shown only for its theme, `pointer-events:none`, correct height,
  hidden on `body.mobile`; `#app` inset matches its height.
- [ ] **Window controls** ("traffic lights" / equivalent): true **circles**
  (transparent stop!), correct diameter and centre spacing, colours, sheen/rim.
- [ ] **Document title** (if any): correct text, weight, colour, ellipsis,
  updates on tab switch (`syncMacTitle`).
- [ ] **Font:** chrome family applied; buffer still monospace; `letter-spacing`
  correct for the family; chrome font-size reads right relative to the buffer.

---

## 3. Menu bar (`.menubar`, `.menu`)

- [ ] **Bar:** background (`--shell`/theme), bottom border/hairline, height
  (one cell row), horizontal padding, clip-path (Cosmos dock gap only).
- [ ] **Items (`.menu`):** text colour, weight, per-item horizontal padding,
  hover fill (`--hover`), cell-position alignment intact.
- [ ] **Open item (`.menu.open`):** fill (accent or theme), readable text.
- [ ] **Left affordance** (traffic lights inside the bar vs a separate title
  bar): no overlap with the first menu label.

## 4a. Menu dropdown panel (`.dropdown`, `.dropdown.submenu`)

- [ ] **Panel:** material/fill (menus should be **near-opaque** — barely show
  content behind), border/hairline, radius, shadow.
- [ ] **Flush under the bar** (no border-row gap); submenu panels edge-to-edge
  with the parent (no overlap seam).

## 4b. Dropdown items (`.mitem`, `.msep`, `.mlabel`)

- [ ] **Item (`.mitem`):** text colour, height (`line-height:CH`), horizontal
  padding, gap between label and accelerator.
- [ ] **Highlight (`.mitem.hi`):** the inset rounded pill (via `::before`, since
  items are cell-positioned) — inset amount, radius, accent fill, readable text.
- [ ] **Label parts:** `.check` (checkmark column width), `.lab` gap, `.accel`
  (secondary colour; readable on the highlight), `.arrow` (submenu chevron).
- [ ] **Disabled (`.mitem.disabled`):** muted text, no highlight on hover.
- [ ] **Separator (`.msep`):** hairline, inset margins.
- [ ] **Label row (`.mlabel`):** muted, italic.

## 5. Tabs (`.tabbar`, `.tab`)

- [ ] **Bar:** background, bottom hairline, overflow clip.
- [ ] **Inactive tab:** text (muted), background, right divider hairline,
  padding, gap.
- [ ] **Active tab:** background (lifted surface), text (full `--fg`), the
  active indicator (bottom/top accent rule — height, colour, radius).
- [ ] **Hover (non-active):** background lift, name brought to full `--fg`.
- [ ] **Modified marker (`.tab .dot`):** only occupies space when dirty; colour.
- [ ] **Close button (`.tab .x`):** shape (macOS = **circle**), size, glyph
  colour, hover fill (stronger than the tab-body hover).

## 6. Status bar (`.statusbar`)

- [ ] **Bar:** background, top hairline, text colour (`--status-fg`), padding.
- [ ] **Text segment (`.txt`):** colour (`--fg`), ellipsis.
- [ ] **Interactive segment (`.seg`):** radius, hover fill, transition.
- [ ] **Connection dot** (theme `::before`): `--ok` green, glow; the reconnect
  pill overrides when the socket drops.

---

## 7. File explorer / sidebar (`.fileexplorer`)

- [ ] **Panel:** background (`--bg3`/`--shell`/sidebar token), right
  border/hairline.
- [ ] **Title (`.fx-title`):** colour (muted), weight, letter-spacing,
  case (Title vs UPPER — match the theme's reference), padding.
- [ ] **List (`.fx-list`):** side padding (the selection-pill gutter), scroll.
- [ ] **Row (`.fx-row`):** text colour, line-height (row height / breathing
  room), horizontal padding, gap, per-row margin, radius.
- [ ] **Selection (`.fx-row.sel`):** the selection **language** for a persistent
  sidebar (soft neutral vs accent — §20); label colour on it (dark vs
  `--on-sel`).
- [ ] **Hover (`.fx-row:not(.sel):hover`):** `--hover` fill.
- [ ] **Icons (`.fx-icon`, `.ficon`):** size (~14px), file colour (muted),
  folder colour (`.fx-icon.dir` = accent), colour **through** selection.
- [ ] **Chevron (`.fx-chev`):** width, colour, colour through selection.
- [ ] **Name (`.fx-name`, `.dir`):** dir weight (600), ellipsis/scroll.
- [ ] **Indentation:** tree depth spacing reads cleanly.

## 8. Editor buffer & pane surfaces

The cells are TUI-theme-owned; a web theme owns only the surrounding surfaces.

- [ ] **Pane background (`.pane-content`, `.pane-gutter`):** `--bg` (keep TUI);
  any theme separator (e.g. an inset top hairline for a "content well").
- [ ] **Caret (`.caret`):** colour, width (scales with zoom), blink.
- [ ] **Scrollbar (`.scrollbar .thumb` + `::-webkit-scrollbar*`):** track
  transparent, thumb colour (hairline-strong), hover, radius, width (density).
- [ ] **Separators / resize grips (`.separator`, `.resize-grip`):** hairline
  fill, hover accent.
- [ ] **Native text selection (`::selection`, `body.natsel`):** highlight tint.
- [ ] Buffer readability: syntax colours are the TUI theme's — a light chrome
  with a dark buffer is intentional; confirm it reads as a deliberate "content
  well", not a mistake.

---

## 9. Command palette / picker (`.palette`)

- [ ] **Card:** material/fill (vibrancy for big surfaces), border, radius,
  shadow; **centered** (`.palette.centered`) vs **bottom-sheet** placement both
  styled; the `.modal-scrim` behind the centered card.
- [ ] **Title band (`.ptitle` / `.ptitlebar` + `.ptbclose`):** weight, colour,
  divider, the close `×` (hover).
- [ ] **Input row (`.pinput`):** background (transparent over the material),
  padding, `.pmsg`/`.status`/`.count` muted colours, the query `.q` + `.caret2`
  blink. The **`N / M` match count** (`.count`) sits right-aligned and muted.
- [ ] **Row (`.prow`):** padding, gap, `.ptext` (command name), `.pdesc`
  (inline muted description, ellipsis), `.pkey` accelerator chip (border,
  radius, background, padding) right-aligned.
- [ ] **Selection (`.prow.sel`):** fill (accent for this active surface),
  `.pdesc` readable on it, and the `.pkey` chip's **border re-tints** on the
  selected row (don't leave a grey chip on an accent fill).
- [ ] **Preview pane (`.ppreview svg.cells`):** buffer cells; background `--bg`.
- [ ] **Search-option chips (`.psearchopts`, `.psopt`, `.psbox`, `.pskey`):**
  pill border/radius, on-state accent tint + check, key chip.
- [ ] **Input-only mode (`.palette.input-only`)** (Open File): bottom-hugging
  bar, no list.

## 10. Popups (`.popup`)

- [ ] **Panel:** near-opaque menu material, border, radius, shadow.
- [ ] **Title/desc (`.popup-title`, `.popup-desc`):** raised band, bottom
  hairline, colours.
- [ ] **Body/rows (`.popup-body`, `.popup-row`):** padding, `.ptext2`,
  `.pdetail` (muted), `.picon`.
- [ ] **Selection (`.popup-row.sel`):** highlight (inset for menu-like popups),
  `.pdetail` readable; disabled rows.
- [ ] **Line rows (`.popup-line`):** whitespace-pre, colour.

## 11. Context menu (`.ctxmenu`)

- [ ] **Panel:** menu material, border, radius, shadow, vertical padding.
- [ ] **Item (`.ctxitem`):** padding; selection (inset highlight); hover.

---

## 12. Plugin widgets — orchestrator dock (`.widget-surface.w-dock`)

- [ ] **Panel:** background (sidebar token / glass), border/right-border,
  radius, shadow, padding; geometry (Cosmos insets the card; others flush —
  gated in `65-widgets.js` on the active theme).
- [ ] **Panel title / `.fx-title`:** muted.
- [ ] **New Task button (`.w-button.primary`):** accent fill/gradient, border,
  text, radius.
- [ ] **Search field (`.w-text`, `.w-text-input`, placeholder):** chrome
  surface (**not `--bg`**), border, radius, padding; optional leading
  magnifier (`::before`); placeholder muted.
- [ ] **Filters row — pulls & buttons (`.w-button`, `view:`/`All ▾` pills):**
  chrome surface, border, radius; not black chips.
- [ ] **Toggles (`.w-toggle`, `.w-box`, `.on`):** sliding switch geometry, off
  track (neutral gray), on track (accent), knob; label colour, on-label colour.
- [ ] **Divider (`.w-divider`):** hairline, margin.
- [ ] **Tree row (`.w-tree-row`):** padding (row height), whitespace-pre
  indentation; disclosure (`.w-tree-disc`) & checkbox (`.w-tree-check`) colours.
- [ ] **Tree continuation row (`.w-tree-xrow`):** muted detail line.
- [ ] **Tree/list card (`.w-tree-card`, `.w-list-card`):** border, radius,
  padding, margin, background.
- [ ] **Selection — ROW vs CARD (§20):** thin rows vs multi-line cards may use
  *different* selection weights; a selected card must not double-fill its inner
  row; xrow stays muted on a selected card.
- [ ] **List rows/cards (`.w-list-row`, `.w-list-cards`):** as above.
- [ ] **Hint bar (`.w-hintbar`, `.w-hint b`):** muted; the accented key hints.
- [ ] **Sections (`.w-section`, `.w-section-label`):** border, radius, padding,
  label colour.
- [ ] **List/tree view modes:** check both the **card** and **compact/list**
  view (`view:` cycles them) — rows, selection and density in each.

## 13. Plugin toolbar (`.ptoolbar`) — e.g. live-grep

- [ ] **Strip:** padding, bottom hairline.
- [ ] **Toggles:** sliding switches (as §12), on-label accent.
- [ ] **Provider pill / buttons:** chrome surface, border, radius.
- [ ] **Rows/labels/gaps:** alignment, muted vs accent labels, `.w-row` gaps.

## 14. Floating modal & context (`.widget-surface.w-floatingModal`)

- [ ] **Centered modal (New Workspace, etc.):** material, border, radius,
  shadow; exactly one `.modal-scrim` behind it (dims + blocks the dock).
- [ ] **Anchored context menu (`.anchored`):** a **real menu**, not a stack of
  bordered buttons — borderless rows (`.w-button` de-bordered), inset highlight
  on the active item, near-opaque material; a transparent `.scrim-clear`
  backdrop that still catches outside-clicks.
- [ ] **Header / "Esc to close":** muted.
- [ ] **Dialog form fields (`.w-text` inside a modal):** chrome surface, focus
  ring.

---

## 15. Settings modal (`.settings-modal`)

- [ ] **Modal card:** ~80% viewport, material/fill, border, radius, shadow.
- [ ] **Title band (`.set-title`):** raised surface, weight, bottom hairline.
- [ ] **Category sidebar (`.set-cats`, `.set-cat`, `.set-cat-sec`):** width,
  right border, row padding/radius, section colour; **selected category**
  (`.set-cat.sel`) selection language; focus ring (`.set-cats.focus`).
- [ ] **Item list (`.set-items`, `.set-item`):** padding, per-item radius/margin,
  divider handling; **focused item** (`.set-item.sel`) — a **subtle** form-row
  selection (tint + dark label), *not* a saturated bar.
- [ ] **Section header (`.set-section`):** accent, uppercase, letter-spacing.
- [ ] **Item head/name/desc (`.set-item-head`, `.set-name`, `.set-desc`):**
  layout gap, name flex, desc muted + wrap.
- [ ] **Control column (`.set-ctl-wrap`, `.set-ctl`, `.set-dim`):** right-align,
  dim italic for complex types.
- [ ] **Toggle (`.set-switch`, `.on`):** off track neutral (scope `:not(.on)`),
  on track accent, knob colour (`--on-accent`).
- [ ] **Stepper (`.set-step`, `.set-num-v`):** border, radius.
- [ ] **Pill / field (`.set-pill`, `.set-field`):** chrome surface (**not
  `--bg`** → black chip), border, radius, ellipsis.
- [ ] **Dropdown (`.set-dd`, `.set-dd-row`, `.set-dd-row.sel`):** popover
  material, border; rows borderless; selected row accent.
- [ ] **Composite list/map (`.set-item-block .set-list`, `.set-list-row`,
  `.set-list-head`, `.set-list-label`, `.set-list-sub`, `.set-list-badge`,
  `.set-list-add`):** the "Name │ value" columns line up, key column width,
  value muted, header dimmed, add row (blue "+ Add…"), badge accent, row
  hairlines, `.sel` on rows.
- [ ] **Search (`.set-search`, `.set-sresult`, `.set-sresult.sel`):** accent
  mark; result selection scrolls into view.
- [ ] **Dual list (`.set-dual-*`):** columns, active column border, row
  selection, buttons.
- [ ] **Footer + buttons (`.set-footer`, `.btn`, `.btn.primary`,
  `.btn.danger`, `.btn.sel`):** raised band; push buttons (chrome surface,
  border); primary accent fill + readable text; danger; focused button.
- [ ] **Entry / add dialog (`.set-entry`, `.set-overlay`):** overlay material,
  border, fields.

## 16. Keybinding editor (`.kbedit`)

- [ ] **Modal:** material, border, radius, shadow; title band; header/footer
  bands.
- [ ] **Table (`.kb-table`, `.kb-row`, `.kb-row.kb-head`, `.kb-row.kb-section`):**
  header background, section accent, row padding/radius, `.sel` selection, hover.
- [ ] **Cells (`.kb-key`, `.kb-action`, `.kb-context`, `.kb-source`,
  `.kb-chip`):** key chips (border, on-state accent), monospace where relevant.
- [ ] **Search (`.kb-search`, `.kb-search b`):** accent mark, focus ring.
- [ ] **Buttons / fields (`.kb-btn`, `.kb-btn.focus`, `.kb-field`,
  `.kb-field.focus`, `.kb-ac-row`):** chrome surfaces, accent focus, readable
  on-accent.
- [ ] **Add/edit dialog (`.kb-dialog`, `.kb-field`, `.kb-overlay`,
  `.kb-autocomplete`):** overlay material, fields, autocomplete rows.

## 17. Trust dialog (`.trustdialog`)

- [ ] **Modal:** material, border, radius, shadow.
- [ ] **Options (`.td-opt`, `.td-opt.sel`, `.td-odesc`):** row selection,
  description readable on it.
- [ ] **Path (`.td-path`):** monospace, ellipsis, colour.
- [ ] **Buttons (`.td-btn`, `.td-btn.primary`):** push vs accent-filled.

## 18. Aux modals (`.auxmodal`)

- [ ] **Modal / title / footer:** bands, hairlines, material.
- [ ] **Lines (`.am-line`, `.am-line.sel`):** selection.

---

## 19. Theme switcher (`#themeswitch`) — frontend-owned

- [ ] **Pill (`#themebtn`, `.ts-dot`, `.ts-name`):** surface, border, radius,
  shadow, the swatch dot; hover; hidden on `body.mobile`.
- [ ] **Menu (`#thememenu`, `.ts-head`, `.ts-row`, `.ts-row.on`, `.ts-check`,
  `.ts-label`, `.ts-desc`):** popover material, header, row padding/radius,
  hover, on-row tint + check, label/desc colours.
- [ ] All token-driven → re-colours per theme automatically; verify in each.

## 20. Selection & interaction language (cross-cutting)

Decide and verify a **consistent language** across surfaces:

- [ ] **Persistent sidebars** (file explorer, dock): soft/neutral vs accent —
  pick per the theme's reference (macOS = soft neutral + dark label; Cosmos =
  translucent accent pill).
- [ ] **Active/transient surfaces** (menus, palette, context menus, dropdown
  rows, focused settings category): accent fill + `--on-sel`/white.
- [ ] **Form rows** (a focused settings item): subtle tint, never a saturated
  bar.
- [ ] **Menu/context items**: inset rounded highlight (`::before`), not
  edge-to-edge.
- [ ] **Hover** is quieter than **selected** everywhere (`--hover`).
- [ ] **Focus rings** (`.w-toggle.focus`, `.w-button.focus`, `.kb-search.focus`,
  `.set-*.focus`): soft accent ring, consistent.
- [ ] **On-accent readability**: labels/icons on every accent fill are legible
  (buttons, toggles, selected rows, badges).

## 21. Shape, depth & rhythm (cross-cutting)

- [ ] **Radius scale** (`--r-sm/md/lg`) applied consistently to pills, fields,
  buttons, cards, panels, rows.
- [ ] **Hairlines**: one low-contrast rule everywhere a structural line is
  drawn (no stray hard greys); strength consistent (`--hairline` vs
  `-strong`).
- [ ] **Shadows / elevation**: floating surfaces share one elevation language
  (`--shadow`); menus lighter than modals if the theme distinguishes them.
- [ ] **Materials**: menus/popovers near-opaque; large surfaces may use
  vibrancy (backdrop-blur) — and provide an `@supports not (backdrop-filter)`
  opaque fallback.
- [ ] **Spacing rhythm**: list side-gutters, row padding and gaps are even
  within a surface and comparable across surfaces of the same kind.
- [ ] **Icons**: consistent size and stroke; muted by default, accent for
  structure (folders), correct colour through selection.

## 22. Motion / FX (`30-render.js`, cosmos/theme CSS)

- [ ] **Entrance** (palette/dock/explorer): fade/slide only on *appear*, not on
  per-keystroke rebuilds; honours `prefers-reduced-motion`.
- [ ] **Scrim** (`.modal-scrim`, `.fx-in`): fade; `position:fixed;inset:0` for
  centered modals in every theme (not gated to one theme).
- [ ] **Workspace switch cut** (`.fx-cut`): tone/duration.
- [ ] No animation restarts the caret blink or re-slides an already-open surface.

## 23. Mobile shell (`body.mobile`, `70-mobile.css`)

Desktop chrome is replaced with touch bars; verify the theme's tokens still read:

- [ ] **Header (`.m-header`, `.m-logo`, `.m-file`, `.m-icon`, `.m-icon.on`):**
  surface, spark accent, active-icon accent.
- [ ] **Overflow sheet (`.m-sheet`, `.m-sheet-item`, `.m-sheet-ic`):** material,
  active fill, the theme/view-pref rows.
- [ ] **Bottom stack (`.m-bottom`, `.m-syms`, `.m-sym`, `.m-save`):** surfaces,
  save accent.
- [ ] **Mods/nav/status (`.m-mods`, `.m-mod.on`, `.m-nav-item.active`,
  `.m-status`, `.m-seg.trust`):** armed-modifier accent, active-nav accent,
  trust segment accent.
- [ ] **Sheets** (explorer/palette/settings full-width): materials, insets.
- [ ] Theme pill hidden; the switch lives in the ⋮ sheet.

## 24. Robustness (cross-cutting, per theme)

- [ ] **Both TUI palettes**: switch the editor's colour theme (dark ↔ light) and
  confirm the chrome tokens the web theme *didn't* override still read (they
  follow `applyTheme()`).
- [ ] **Zoom** (`Ctrl+=`/`Ctrl+0`) and **density scale**: chrome scales with the
  buffer; nothing clips or overflows.
- [ ] **No black chips**: grep the rendered UI for any control still on `--bg`.
- [ ] **No cell/SVG leakage**: chrome is native HTML; only `svg.cells` (buffer)
  and `.ficon` (decorative) are SVG.
- [ ] **Playwright suite green** (`web-ui/test/run.sh`): the theme assertions
  (body class, bezel/title-bar swap, buffer-stays-monospace, density, switcher)
  plus the 140+ chrome assertions.
- [ ] **Cosmos unchanged**: the default theme is byte-for-byte identical (its
  rules are gated to `body.theme-cosmos`); a new theme must not regress it.

---

## 25. Screenshot-observed detail catalog

Concrete elements pulled from per-theme screenshots (`web-ui/test/drive.mjs`
`$SHOTS`, plus focused captures of the title bar, tabs, toolbar, popup, context
menu and the dock in each view mode). These are the easy-to-miss bits — verify
each renders correctly and legibly in the theme under review.

**Status bar** (bottom): left cluster `Trusted · Local · Ln N, Col N · <message>`
and right cluster `LF · UTF-8 · Rust · LSP (off) · Palette: Ctrl+P`. Mixed
muted (`.seg`) and full (`.txt`) weights; the trailing shortcut hint reads as
emphasized. Confirm the message area (`File explorer ready` / `Search
cancelled` / `Cancelled`) is muted and doesn't jump.

**Palette:** title band (`Command Palette`) + close `×`; `>` prompt; right
`N / M` count; per-row inline description; per-row accelerator chip; the
selected row's chip re-tints. Frosted (Cosmos, content shows through) vs
near-opaque (macOS).

**Tabs:** active-tab accent rule — Cosmos draws it on the **top** edge (merges
with the buffer below), macOS on the **bottom**. Close `×` circular (macOS) vs
rounded-square. Dirty dot only when modified.

**File explorer:** file rows are `◇ name` (muted glyph); folders are `▸ 📁 name`
bold with an **accent** folder glyph and a disclosure chevron; the header is the
project name (`FRESH` upper in Cosmos, `fresh` Title Case in macOS). Selected
row: teal translucent pill (Cosmos) vs soft neutral pill + dark label (macOS).

**Orchestrator dock:**
- Header `Orchestrator`; primary `New Task… ▾`; search field (with a leading
  magnifier in macOS); `▸ Filters` disclosure.
- Filters expanded: a `view: card` / `view: compact` cycle pill, an `All ▾`
  scope pull, `all worktrees` and `show empty` sliding toggles, `Move…` and
  `Manage` buttons.
- Workspace **card**: line 1 `· <name> ▣ <branch>`, line 2 `▸ <task/branch>` +
  a right-aligned **git diff stat** (`+N −M`, additions green / deletions red)
  or a status word (`clean` / `detached`). Selection = soft fill + faint accent
  edge; the diff stat colours must stay legible on the selection.
- **List/compact** view: the card collapses to a flat `· <name>` row.
- Bottom **hint bar**: `↕ switch  →← fold` / `Enter edit  F2 menu` — plain text
  with the **keys** (`Enter`, `F2`) in the accent colour.

**Live-grep toolbar:** `Search in:` then toggles `Files / Ignored / Buffers /
Terminals`, each a sliding switch + label + a muted `Alt+<key>` hint; on-toggles
colour their label with the accent. `Match:` `Word / Regex`. `Provider:` a
value pill (`git-grep`) + `Alt+M save matches`. Empty result reads `0 / 0`.

**Status popup** (e.g. `Remote: Local`): bold title row, an action row
(`Create Dev Container Config`) with the menu-selection highlight, and a muted
`Dismiss (Esc)` footer row.

**Context menu** (dock right-click): a muted **target header** (`fresh`), action
rows (`Visit… / Move to Folder… / Archive / Delete`) as borderless menu rows
with an inset highlight on the active one, and a muted `Esc to close` footer.

**Cosmos-only decorative chrome** (for reference, gated to `.theme-cosmos`):
machined bezel with corner **screws**, top-right status **LEDs** (green/amber),
engraved side-rail legends + running LED dots, silkscreen `SPEC: COSMOS-991` /
`PROJECT: INFERNU CORE` labels; the glass dock's green **New Task** pill and
frosted search `⌕`. A new theme replaces this frame wholesale — none of it
should leak.

**Aspirational, from the macOS reference screenshot** (not yet built — candidate
polish, evaluate if added): right-aligned **count badges** on sidebar rows;
a **monochrome SF-symbol toolbar** icon set; translucent **vibrancy** on
sidebars (content behind shows through); rounded translucent **cards** for
floating panels; avatar/initial circles; 2-line previews with progressive
muting (title darker than preview). A floating rounded **window** over a desktop
(vs our maximized full-bleed) is a structural variant, essentially what Cosmos
already does.

## 26. Reference values (macOS Big Sur / Sonoma)

Known-good numbers for a macOS-family theme, for calibration:

- **Traffic lights:** 12px circles on 20px centres, ~20px from the window's
  left edge, in a ~28px title bar. Modern fills `#ff5f57` / `#febc2e` /
  `#28c840`; documented older set (with rims) `#ed6a5f`/`#e24b41`,
  `#f6be50`/`#e1a73e`, `#61c555`/`#2dac2f`; unfocused `#dddddd`. Each dot needs
  a **transparent stop** at the radius (or it renders square) and a subtle
  top-lit sheen.
- **Accent (controlAccent, Blue):** light selection/emphasis `#0063e1`–`#007aff`;
  dark `#0a84ff`.
- **Text:** `labelColor` = black @ ~85% (light) / white @ ~85% (dark);
  `secondaryLabel` ≈ 50–55%.
- **Windows/surfaces:** light window `~#ececec` (systemGray6); dark window/
  toolbar `~#2a2a2c`, content `~#1e1e1e`.
- **Hairlines / separators:** ~9% black (light) / ~8–10% white (dark).
- **Radii:** control ~6px, card/panel ~8–11px; window corner ~10px.
- **Font:** SF (system) chrome at ~13px; the buffer stays monospace.
- **Selection:** context-dependent — a *light* sidebar (Finder/Mail) uses a
  **soft neutral** pill with a dark label; a *dark, branded* sidebar (Bear) uses
  a **filled accent** pill with white text; a content list may mark selection
  with a **left accent bar** instead of a fill. Accent + white is otherwise
  reserved for menus, buttons, and the *focused* list. Pick per surface and keep
  it consistent.
- **Buttons:** push buttons are near-**flat solid accent** with a faint top
  sheen (`inset 0 1px 0 rgba(255,255,255,.22)`), **not** a gradient — cf. the
  "Try now" pill and Control Center toggles.
- **Panels / cards:** Big Sur floating surfaces are **generously rounded**
  (~12px for palettes/modals/cards; Control Center cards ~14–16px) with heavy
  backdrop-blur vibrancy and a soft shadow. Menus stay tighter (~6–8px).
- **Materials:** Control Center / notification cards are the signature look —
  translucent, rounded, floating; the global menu bar and dock are translucent
  too. (Our maximized full-bleed app has no desktop behind it, so sidebars are
  opaque; a floating-window variant would enable true vibrancy — that's what the
  Cosmos shell already does.)

References used for calibration: a real **Mail** window (light, soft-gray
sidebar selection, count badges, monochrome toolbar) and a faithful **Big Sur
clone** (light + dark — Control Center cards, dark-sidebar filled-accent
selection, flat accent buttons, rounded translucent panels).

---

## Appendix — surface → file map

| Surface | CSS | Builder (JS) | Scene projection (Rust) |
|---|---|---|---|
| tokens / base | `10-base.css` | `15-theme.js`, `20-cells.js` `applyTheme` | — |
| menu bar / dropdown | `20-chrome.css` | `40-menu.js` | `menu_view` |
| tabs | `20-chrome.css` | `45-tabs.js` | `tab_bar_view` |
| status bar | `20-chrome.css` | `55-status.js` | `status_view` |
| palette / picker | `30-palette.css` | `50-palette.js` | `palette_view` |
| file explorer | `35-editor.css` | `70-panels.js` | `file_explorer_view` |
| pane / caret / scrollbar | `35-editor.css` | `30-render.js` | pane cells |
| popups | `50-popups.css` | `60-popups.js` | `popups_view` |
| widgets / dock / settings / kbedit | `40-widgets.css` | `65-widgets.js` | `widgets_view`, `settings_view`, `keybinding_editor_view` |
| trust dialog | `40-widgets.css` | `70-panels.js` | `trust_dialog_view` |
| context menu | `40-widgets.css` | `65-widgets.js` | `context_menu_view` |
| polish / hairlines / selection | `60-polish.css` | — | — |
| mobile | `70-mobile.css` | `75-app.js` | (mirrors tabs/status) |
| skin (navy/teal base) | `80-skin.css` | — | — |
| Cosmos shell | `90-cosmos.css` | `10-core.js` (`layoutShell`) | — |
| theme switcher | `91-theme-switch.css` | `15-theme.js` | — |
| macOS theme | `92-theme-macos.css` | `15-theme.js` tokens | — |
| Compact theme | `94-theme-compact.css` | `15-theme.js` tokens | — |
