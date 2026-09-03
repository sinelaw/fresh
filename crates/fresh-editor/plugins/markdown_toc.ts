/// <reference path="./lib/fresh.d.ts" />
// Markdown Table of Contents Plugin
// A "Contents" section in the sidebar, under the file explorer, listing the
// headings of the active Markdown buffer as a tree. The selected row follows
// the cursor (or the viewport), a click jumps the editor to the heading, and
// the disclosure glyphs fold the outline — optionally the buffer too.
//
// This is the first consumer of the sidebar-sections API
// (docs/internal/sidebar-sections-design.md §5).
//
// Both Markdown modes, one plugin
// ------------------------------
// Compose/preview is a *view mode* on the same buffer: `markdown_compose.ts`
// sets `ViewMode::PageView` through `setViewMode(bufferId, "compose")`, which
// conceals the `#` markers, hides the line-number gutter and soft-wraps lines
// — but does not move a single byte of the source. So this plugin never looks
// at `view_mode`, never assumes line numbers are visible, and never reasons in
// screen rows: every position it holds is a source byte offset from its own
// scan of `getBufferText`, and every hook it consumes (`cursor_moved`,
// `viewport_changed`) reports bytes. The jump uses the source line the scan
// recorded for the heading, which `openFile`/`scrollToLineCenter` accept
// whether or not the gutter is drawn. The only thing a compose toggle costs is
// one rescan, done for trust rather than correctness (§5.7).
//
// Settings
// --------
// Declared with `editor.defineConfig*` and read back with
// `editor.getPluginConfig()`, which is how the other bundled plugins expose
// their settings (dashboard.ts, orchestrator.ts, vi_mode.ts). They live under
// `plugins.markdown_toc.settings` in config.json and in the Settings UI under
// "Plugin Settings → markdown_toc". The design note names them `toc.*`;
// the mapping is:
//
//   toc.follow      → follow      "cursor" | "scroll"   (default "cursor")
//   toc.fold_buffer → foldBuffer  boolean               (default false)
//   toc.auto_open   → autoOpen    boolean               (default true)
//   toc.rows        → rows        integer               (default 10)
//
// Host API this plugin depends on:
//   mountSidebarSection(id, spec, title, rows, opts)   mount as a sidebar section
//   updateFloatingWidget(id, spec)                     replace the spec
//   unmountFloatingWidget(id)                          remove the section
//   floatingPanelControl(id, "sidebar_rows", n)        requested rows
//   floatingPanelControl(id, "focus" | "blur", 0)      as for the dock
//   widgetMutate(id, { kind: "setSelectedIndex" | "setExpandedKeys", ... })
//   widget_event { panel_id, widget_key, event_type, payload }  hits
//
// The host seam, as shipped
// -------------------------
// Three facts about the host shape what follows; they were found when the
// plugin met the real `mountSidebarSection` and are stated so nobody has to
// find them again.
//
// * A focused section does not change the active split. Keyboard focus in
//   the sidebar is a chrome fact, not a split fact, so "the pane does not
//   have focus" (§5.5) cannot be read off `getActiveSplitId()` alone. The
//   host tells the plugin instead: `widget_event { focus }` when the section
//   takes the keyboard (a press on it, the focus cycle, or this plugin's own
//   `floatingPanelControl("focus")`) and `{ blur }` when it gives it up (Esc,
//   a press outside the column, the explorer taking it). `sectionFocused`
//   mirrors those two events. What the plugin still cannot see is the
//   *explorer* holding the keyboard — no hook reports the key context — so
//   scrolling while the file tree is focused follows the cursor, not the
//   viewport. That is the one residual gap against §5.5.
// * A click and a keyboard move both arrive as `select`, but a click's
//   payload is tagged `via: "click"` and an arrow's is not. That is exactly
//   the distinction §5.6 draws: a click previews (jump, keyboard stays in
//   the sidebar); Up/Down/PageUp/PageDown and Left-to-parent browse the
//   outline without moving the buffer cursor until Enter (`activate`).
// * A jump the section asked for scrolls the pane, and the pane reports that
//   as `viewport_changed` like any other scroll. Without a guard the
//   viewport-follow rule would then move the selection off the row the
//   reader just chose (a centred heading is rarely the top heading), so a
//   jump remembers its target for a moment and the first viewport change
//   that still shows it is taken as the jump's own.
const editor = getEditor();

// =============================================================================
// Settings
// =============================================================================

type FollowMode = "cursor" | "scroll";

editor.defineConfigEnum("follow", {
  values: ["cursor", "scroll"] as const,
  default: "cursor",
  description:
    "Which heading the Contents panel highlights: the one containing the cursor ('cursor'), or the one at the top of the viewport ('scroll'). The 'Markdown: Contents — Follow Cursor/Scroll' command flips it for the session.",
});
editor.defineConfigBoolean("foldBuffer", {
  default: false,
  description:
    "Collapsing a heading in the Contents panel also folds its section in the buffer (and expanding unfolds it).",
});
editor.defineConfigBoolean("autoOpen", {
  default: true,
  description:
    "Open the Contents panel when a Markdown buffer becomes active and close it when the active buffer is not Markdown. 'Markdown: Toggle Table of Contents' opens it regardless.",
});
editor.defineConfigInteger("rows", {
  default: 10,
  minimum: 1,
  maximum: 200,
  description: "Rows the Contents section asks for in the sidebar. A drag on the section divider overrides it.",
});

interface TocSettings {
  follow?: FollowMode;
  foldBuffer?: boolean;
  autoOpen?: boolean;
  rows?: number;
}

// Re-read on demand rather than cached: a Settings-UI edit lands in the
// config snapshot straight away, so the next event honours it.
function settings(): TocSettings {
  return (editor.getPluginConfig() ?? {}) as TocSettings;
}

// The "Follow Cursor/Scroll" command's session override; `null` defers to the
// configured value.
let followOverride: FollowMode | null = null;

function followMode(): FollowMode {
  if (followOverride !== null) return followOverride;
  return settings().follow === "scroll" ? "scroll" : "cursor";
}

function foldBufferEnabled(): boolean {
  return settings().foldBuffer === true;
}

function autoOpenEnabled(): boolean {
  return settings().autoOpen !== false;
}

const DEFAULT_ROWS = 10;

function requestedRows(): number {
  const rows = settings().rows;
  return typeof rows === "number" && rows >= 1 ? Math.floor(rows) : DEFAULT_ROWS;
}

// =============================================================================
// The scan — pure functions over the buffer text
// =============================================================================
//
// `scanTocHeadings`, `headingLevel`, `looksLikeFence` and `utf8Length` are
// copied from `markdown_compose.ts` (its `scanHeadings` feeds the scrollbar's
// heading markers from the same walk). Plugins can share code only through
// `plugins/lib/`, and the compose scanner is shaped for markers — it returns
// `ScrollbarMarker`s, capped at `HEADING_MARKER_MAX_LEVEL`, with no titles —
// so moving it there would mean reworking the compose plugin as well. The
// ~40 lines are duplicated instead; the fence rule is the one thing that must
// stay in step between the two.

/** One heading of the document. Positions are source byte offsets. */
interface Heading {
  /** Byte offset of the heading line's first byte — the tree key. */
  byte: number;
  /** 0-indexed source line of the heading. */
  line: number;
  /** ATX level, 1-6. */
  level: number;
  /** The heading text without its `#` markers. */
  title: string;
  /** True when deeper headings follow before the next same-or-higher one. */
  hasChildren: boolean;
  /** Last 0-indexed line of the heading's section (exclusive of the next
   *  same-or-higher heading), for folding. */
  endLine: number;
  /** Byte offset just past the heading line's newline — the first byte a
   *  buffer fold hides. */
  bodyStartByte: number;
  /** Byte offset of the end of the section's last line (its newline
   *  excluded), for folding. */
  endByte: number;
}

interface TocScan {
  headings: Heading[];
  lineCount: number;
}

/** Whether a line reads as a fence delimiter from its own text alone. */
function looksLikeFence(content: string): boolean {
  return /^\s*(?:`{3,}|~{3,})/.test(content);
}

/** Heading level of a source line (1-6), or 0 when it isn't an ATX heading. */
function headingLevel(content: string): number {
  const m = content.match(/^\s*(#{1,6})\s+\S/);
  return m ? m[1].length : 0;
}

/** The heading text of an ATX heading line: markers, surrounding whitespace
 *  and an optional closing `#` run stripped — a contents list is not the
 *  source. */
function headingTitle(content: string): string {
  return content
    .replace(/^\s*#{1,6}\s+/, "")
    .replace(/\s+#+\s*$/, "")
    .trim();
}

/** UTF-8 byte length; positions are byte offsets, JS indices are not. */
function utf8Length(str: string): number {
  let n = 0;
  for (let i = 0; i < str.length; i++) {
    const code = str.charCodeAt(i);
    if (code <= 0x7f) n += 1;
    else if (code <= 0x7ff) n += 2;
    else if (code >= 0xd800 && code <= 0xdfff) { n += 4; i++; }
    else n += 3;
  }
  return n;
}

/**
 * The headings in `text`, skipping fenced code blocks.
 *
 * A `#` at the start of a line inside a fence is a comment in whatever
 * language the block is written in — `# Binary package (recommended)` in a
 * bash block is the common case — not a heading. A closer has to match its
 * opener's character, so a ``` inside a ~~~ block is content, not a
 * delimiter. Same rule as `scanHeadings` in markdown_compose.ts.
 *
 * Every heading is returned with its section extent (`endLine`, `endByte`)
 * and `hasChildren`, so the tree and the folds come from one walk.
 */
function scanTocHeadings(text: string): TocScan {
  const headings: Heading[] = [];
  const lines = text.split("\n");
  // Byte offset of the end of each line (newline excluded), for `endByte`.
  const lineEnds: number[] = new Array(lines.length);
  let offset = 0;
  let inFence = false;
  let fenceChar = "";

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const lineBytes = utf8Length(line);
    lineEnds[i] = offset + lineBytes;
    if (looksLikeFence(line)) {
      const char = line.trimStart()[0];
      if (!inFence) {
        inFence = true;
        fenceChar = char;
      } else if (char === fenceChar) {
        inFence = false;
      }
    } else if (!inFence) {
      const level = headingLevel(line);
      if (level > 0) {
        headings.push({
          byte: offset,
          line: i,
          level,
          title: headingTitle(line),
          hasChildren: false,
          endLine: lines.length - 1,
          bodyStartByte: offset + lineBytes + 1,
          endByte: 0,
        });
      }
    }
    offset += lineBytes + 1; // +1 for the "\n" the split consumed
  }

  // Section extents and children: a heading's section runs to the line
  // before the next heading of the same or a higher level; it has children
  // when the very next heading is deeper.
  for (let i = 0; i < headings.length; i++) {
    const h = headings[i];
    let end = lines.length - 1;
    for (let j = i + 1; j < headings.length; j++) {
      if (headings[j].level <= h.level) {
        end = headings[j].line - 1;
        break;
      }
    }
    h.endLine = end;
    h.endByte = lineEnds[end];
    h.hasChildren = i + 1 < headings.length && headings[i + 1].level > h.level;
  }

  return { headings, lineCount: lines.length };
}

/**
 * Index of the last heading at or before `byte`, or -1 when `byte` precedes
 * every heading. `headings` is sorted by `byte` (the scan emits it that way),
 * so this is a binary search — the fast path `cursor_moved` runs on every
 * keystroke.
 */
function headingIndexAtOrBefore(headings: readonly Heading[], byte: number): number {
  let lo = 0;
  let hi = headings.length - 1;
  let found = -1;
  while (lo <= hi) {
    const mid = (lo + hi) >> 1;
    if (headings[mid].byte <= byte) {
      found = mid;
      lo = mid + 1;
    } else {
      hi = mid - 1;
    }
  }
  return found;
}

// Byte cap for the one-shot scan, from the editor's own large-file setting —
// the same bound `prescanHeadingMarkers` uses for the scrollbar marks.
function scanByteLimit(): number {
  const cfg = editor.getConfig() as { editor?: { large_file_threshold_bytes?: number } } | null;
  const v = cfg?.editor?.large_file_threshold_bytes;
  return typeof v === "number" && v > 0 ? v : 1048576;
}

// =============================================================================
// State
// =============================================================================

// Plugin-local: the host keys panels by (plugin, id), so a constant is all the
// uniqueness this single section needs.
const PANEL_ID = 1;
const TREE_KEY = "toc";

interface TocState {
  bufferId: number;
  path: string;
  headings: Heading[];
  /** Expansion is plugin-owned (`Tree.expandedKeys` is initial-only). */
  expanded: Set<string>;
  /** Keys whose section is currently folded in the buffer (foldBuffer). */
  folded: Set<string>;
  /** Row currently selected — mirrored so a rescan can re-select. */
  selected: number;
  /** The buffer grew past the scan cap; `headings` is the last good scan. */
  stale: boolean;
  /** `is_composing_in_any_split` at the last scan — a flip triggers one. */
  composing: boolean;
}

let toc: TocState | null = null;
let mounted = false;
/** Whether the section holds the keyboard — mirrored from the host's
 *  `focus` / `blur` widget events (see "The host seam" above). */
let sectionFocused = false;
/** Title the section was mounted with; a change needs a remount. */
let mountedTitle = "";
let rescanTimer: number | null = null;
/** Bumped per full scan so a stale `await` cannot publish over a newer one. */
let scanGeneration = 0;

const RESCAN_DEBOUNCE_MS = 300;

function isMarkdownFile(path: string): boolean {
  return path.endsWith(".md") || path.endsWith(".markdown");
}

function isComposingInAnySplit(bufferId: number): boolean {
  const info = editor.getBufferInfo(bufferId);
  return info != null && info.is_composing_in_any_split;
}

// =============================================================================
// The spec
// =============================================================================

function buildSpec(state: TocState): WidgetSpec {
  return {
    kind: "tree",
    key: TREE_KEY,
    nodes: state.headings.map((h) => ({
      text: { text: h.title },
      depth: h.level - 1,
      hasChildren: h.hasChildren,
    })),
    // Keys are byte offsets rather than titles: two headings can share a
    // title and none can share an offset.
    itemKeys: state.headings.map((h) => String(h.byte)),
    // Driven by sync (`setSelectedIndex`), never by the spec.
    selectedIndex: -1,
    // Initial only; `setExpandedKeys` thereafter.
    expandedKeys: Array.from(state.expanded),
    checkable: false,
    itemHeight: 1,
    cardBorders: false,
    // The sidebar is a couple of dozen columns wide: one column per level.
    indentCols: 1,
  };
}

function sectionTitle(state: TocState | null): string {
  return state?.stale ? editor.t("panel.title_stale") : editor.t("panel.title");
}

// =============================================================================
// Mount / update / unmount
// =============================================================================

function mountSection(): void {
  if (!toc) return;
  const title = sectionTitle(toc);
  if (mounted && title === mountedTitle) {
    editor.updateFloatingWidget(PANEL_ID, buildSpec(toc));
  } else {
    // First mount, or the title changed (stale ↔ fresh): the title is fixed
    // at mount time, so this is a remount. Mounted blurred — the section is
    // reference material, and taking the keyboard from the editor on every
    // buffer switch would be hostile.
    if (mounted) editor.unmountFloatingWidget(PANEL_ID);
    editor.mountSidebarSection(PANEL_ID, buildSpec(toc), title, requestedRows(), {
      closable: true,
      startBlurred: true,
    });
    mounted = true;
    mountedTitle = title;
    sectionFocused = false;
  }
  pushExpanded();
  pushSelected(toc.selected);
}

function unmountSection(): void {
  if (mounted) {
    editor.unmountFloatingWidget(PANEL_ID);
    mounted = false;
    mountedTitle = "";
    sectionFocused = false;
  }
}

function pushSelected(index: number): void {
  if (!toc || !mounted) return;
  toc.selected = index;
  editor.widgetMutate(PANEL_ID, {
    kind: "setSelectedIndex",
    widgetKey: TREE_KEY,
    index,
  } satisfies WidgetMutation);
}

function pushExpanded(): void {
  if (!toc || !mounted) return;
  editor.widgetMutate(PANEL_ID, {
    kind: "setExpandedKeys",
    widgetKey: TREE_KEY,
    keys: Array.from(toc.expanded),
  } satisfies WidgetMutation);
}

// =============================================================================
// Keeping it current
// =============================================================================

/**
 * Full scan of `bufferId` and (re)publish the section.
 *
 * `lines_changed` reports the viewport, not the document, so the whole text
 * is read — bounded by the large-file threshold. Over the cap the panel keeps
 * the headings it last saw and says so in its title.
 */
async function rescan(bufferId: number): Promise<void> {
  const info = editor.getBufferInfo(bufferId);
  if (!info || !isMarkdownFile(info.path)) return;

  const generation = ++scanGeneration;
  const limit = scanByteLimit();
  let text: string;
  try {
    text = await editor.getBufferText(bufferId, 0, limit + 1);
  } catch (e) {
    editor.debug(`Contents scan skipped for buffer ${bufferId}: ${e}`);
    return;
  }
  // The await gave the user time to switch buffers or close this one; a
  // newer scan, or none, is now the truth.
  if (generation !== scanGeneration) return;
  if (!toc || toc.bufferId !== bufferId) return;

  if (utf8Length(text) > limit) {
    editor.debug(`Contents scan skipped for buffer ${bufferId}: over ${limit} bytes`);
    toc.stale = true;
    mountSection();
    return;
  }

  const scan = scanTocHeadings(text);
  // Expansion survives a rescan by key. A key is a byte offset, so an edit
  // above a heading renames it and it comes back expanded — the default —
  // while headings below the edit keep whatever the user chose.
  const known = new Set(toc.headings.map((h) => String(h.byte)));
  const expanded = new Set<string>();
  for (const h of scan.headings) {
    const key = String(h.byte);
    if (!h.hasChildren) continue;
    if (!known.has(key) || toc.expanded.has(key)) expanded.add(key);
  }
  const folded = new Set<string>();
  for (const key of toc.folded) if (scan.headings.some((h) => String(h.byte) === key)) folded.add(key);

  toc.headings = scan.headings;
  toc.expanded = expanded;
  toc.folded = folded;
  toc.stale = false;
  toc.composing = info.is_composing_in_any_split;
  toc.selected = headingIndexAtOrBefore(toc.headings, editor.getCursorPosition());
  mountSection();
  publishFoldingRanges();
}

/** Track `bufferId` in the section (mounting it) and scan it. */
function track(bufferId: number, path: string): void {
  if (rescanTimer !== null) {
    editor.clearInterval(rescanTimer);
    rescanTimer = null;
  }
  if (!toc || toc.bufferId !== bufferId) {
    toc = {
      bufferId,
      path,
      headings: [],
      expanded: new Set(),
      folded: new Set(),
      selected: -1,
      stale: false,
      composing: isComposingInAnySplit(bufferId),
    };
  }
  void rescan(bufferId);
}

/** Debounced rescan for the tracked buffer: edits arrive per keystroke; the
 *  rescan waits for a pause. */
function scheduleRescan(): void {
  if (!toc) return;
  if (rescanTimer !== null) editor.clearInterval(rescanTimer);
  rescanTimer = editor.setTimeout(RESCAN_DEBOUNCE_MS, "markdownTocRescan");
}

function markdownTocRescan(): void {
  rescanTimer = null;
  if (!toc) return;
  void rescan(toc.bufferId);
}
registerHandler("markdownTocRescan", markdownTocRescan);

/**
 * There is no view-mode hook: a compose toggle is noticed on the next event
 * that concerns the tracked buffer, and answered with one rescan (the
 * document did not change, but a stale panel across a mode switch erodes
 * trust — §5.7).
 */
function noticeComposeFlip(bufferId: number): void {
  if (!toc || toc.bufferId !== bufferId) return;
  const composing = isComposingInAnySplit(bufferId);
  if (composing === toc.composing) return;
  toc.composing = composing;
  void rescan(bufferId);
}

// =============================================================================
// Folding (toc.fold_buffer)
// =============================================================================

/** Publish every heading's section as a toggleable fold range, in the shape
 *  an LSP `foldingRange` response takes, so the standard fold keybinding and
 *  the panel's disclosure glyphs agree on the ranges. */
function publishFoldingRanges(): void {
  if (!toc || !foldBufferEnabled()) return;
  const ranges = toc.headings
    .filter((h) => h.endLine > h.line)
    .map((h) => ({ startLine: h.line, endLine: h.endLine, kind: "region" }));
  editor.setFoldingRanges(toc.bufferId, ranges);
}

/** Re-apply the buffer folds from `toc.folded`. There is no per-range unfold
 *  in the API, so an expand clears every fold and re-adds the rest. */
function applyBufferFolds(): void {
  if (!toc) return;
  editor.clearFolds(toc.bufferId);
  for (const h of toc.headings) {
    if (!toc.folded.has(String(h.byte))) continue;
    if (h.endByte <= h.bodyStartByte) continue;
    editor.addFold(toc.bufferId, h.bodyStartByte, h.endByte);
  }
}

// =============================================================================
// Navigation
// =============================================================================

/** The heading a jump the section asked for is centring, and until when the
 *  viewport change it causes is still taken as that jump's rather than as
 *  the reader scrolling (see "The host seam" above). */
let jumpTarget: { heading: Heading; until: number } | null = null;
const JUMP_SETTLE_MS = 500;

function rememberJump(heading: Heading): void {
  jumpTarget = { heading, until: Date.now() + JUMP_SETTLE_MS };
}

/** Put the cursor on the heading and centre it in the pane showing the
 *  buffer — the existing "jump to a location" pair. The two halves count
 *  lines differently: `scrollToLineCenter` from 0, `openFile` from 1 (it is
 *  the go-to-line prompt's convention, column included). */
function jumpTo(heading: Heading): void {
  if (!toc) return;
  rememberJump(heading);
  const split = splitShowingBuffer();
  if (split !== null) editor.scrollToLineCenter(split, toc.bufferId, heading.line);
  editor.openFile(toc.path, heading.line + 1, 1);
}

/** Show the heading in the pane without moving the cursor — what browsing
 *  the outline with the arrows does in `scroll` mode. */
function scrollTo(heading: Heading): void {
  if (!toc) return;
  const split = splitShowingBuffer();
  if (split === null) return;
  rememberJump(heading);
  editor.scrollToLineCenter(split, toc.bufferId, heading.line);
}

/** The split to jump in: the active one when it shows the tracked buffer,
 *  else the first that does. */
function splitShowingBuffer(): number | null {
  if (!toc) return null;
  const info = editor.getBufferInfo(toc.bufferId);
  const active = editor.getActiveSplitId();
  const splits = info?.splits ?? [];
  if (splits.includes(active)) return active;
  return splits.length > 0 ? splits[0] : null;
}

editor.on("widget_event", (e) => {
  if (e.panel_id !== PANEL_ID || !toc) return;
  const payload = (e.payload ?? {}) as {
    index?: unknown;
    key?: unknown;
    expanded?: unknown;
    via?: unknown;
  };

  if (e.event_type === "focus") {
    sectionFocused = true;
    return;
  }
  if (e.event_type === "blur") {
    sectionFocused = false;
    return;
  }
  if (e.event_type === "cancel") {
    // The section's ×: the host already unmounted it.
    mounted = false;
    mountedTitle = "";
    sectionFocused = false;
    return;
  }

  if (e.event_type === "select" || e.event_type === "activate") {
    const index = typeof payload.index === "number" ? payload.index : -1;
    const heading = toc.headings[index];
    if (!heading) return;
    toc.selected = index;
    if (e.event_type === "activate") {
      // Enter: the jump is the destination, so the keyboard goes with it.
      jumpTo(heading);
      editor.floatingPanelControl(PANEL_ID, "blur", 0);
      const split = splitShowingBuffer();
      if (split !== null) editor.focusSplit(split);
    } else if (payload.via === "click") {
      // A click previews: the cursor lands on the heading, the keyboard
      // stays in the sidebar — the same split the explorer makes between
      // preview and open. The host answers "focus" with a `focus` event,
      // but set the mirror now so nothing between the two reads it stale.
      jumpTo(heading);
      editor.floatingPanelControl(PANEL_ID, "focus", 0);
      sectionFocused = true;
    } else if (followMode() === "scroll") {
      // An arrow key in scroll mode: the pane shows the heading, the
      // cursor stays where it was.
      scrollTo(heading);
    }
    // An arrow key in cursor mode: browsing the outline is not editing —
    // the buffer cursor does not move until Enter (§5.6). The host has
    // already moved the row band; `toc.selected` mirrors it above.
    return;
  }

  if (e.event_type === "expand") {
    if (typeof payload.key !== "string") return;
    const key = payload.key;
    const expanded = typeof payload.expanded === "boolean" ? payload.expanded : !toc.expanded.has(key);
    if (expanded) toc.expanded.add(key);
    else toc.expanded.delete(key);
    pushExpanded();
    if (foldBufferEnabled()) {
      if (expanded) toc.folded.delete(key);
      else toc.folded.add(key);
      applyBufferFolds();
    }
    return;
  }
});

// =============================================================================
// Sync
// =============================================================================

editor.on("cursor_moved", (data) => {
  if (!toc || data.buffer_id !== toc.bufferId || !mounted) return;
  noticeComposeFlip(data.buffer_id);
  if (followMode() !== "cursor") return;
  const index = headingIndexAtOrBefore(toc.headings, data.new_position);
  if (index !== toc.selected) pushSelected(index);
});

editor.on("viewport_changed", (data) => {
  if (!toc || data.buffer_id !== toc.bufferId || !mounted) return;
  noticeComposeFlip(data.buffer_id);
  // A scroll the section itself asked for is not the reader scrolling: the
  // row they chose stays selected as long as the pane still shows it.
  if (jumpTarget !== null) {
    const { heading, until } = jumpTarget;
    jumpTarget = null;
    if (Date.now() <= until && data.top_byte <= heading.byte) {
      const index = toc.headings.indexOf(heading);
      if (index >= 0 && index !== toc.selected) pushSelected(index);
      return;
    }
  }
  // The viewport top is what the reader is looking at when the pane does
  // not have focus (they are in the sidebar, or reading in another split),
  // or whenever `follow` pins scroll mode. A focused section leaves the
  // active split alone, so its own focus is the plugin's to know.
  const paneFocused = !sectionFocused && data.split_id === editor.getActiveSplitId();
  if (paneFocused && followMode() !== "scroll") return;
  const index = headingIndexAtOrBefore(toc.headings, data.top_byte);
  if (index !== toc.selected) pushSelected(index);
});

// =============================================================================
// Buffer lifecycle
// =============================================================================

function onMarkdownBufferActive(bufferId: number, path: string): void {
  if (autoOpenEnabled() || mounted) track(bufferId, path);
}

editor.on("after_file_open", (data) => {
  if (!isMarkdownFile(data.path)) return;
  // Only the buffer the user is looking at: a background open (a session
  // restore, `openFileInBackground`) is followed by `buffer_activated` if it
  // becomes visible.
  if (editor.getActiveBufferId() !== data.buffer_id) return;
  onMarkdownBufferActive(data.buffer_id, data.path);
});

editor.on("buffer_activated", (data) => {
  const info = editor.getBufferInfo(data.buffer_id);
  if (info && isMarkdownFile(info.path)) {
    onMarkdownBufferActive(data.buffer_id, info.path);
    return;
  }
  // Not markdown: auto-open closes the section; a manually opened one keeps
  // showing the last Markdown buffer until toggled.
  if (autoOpenEnabled()) {
    unmountSection();
    toc = null;
  }
});

editor.on("buffer_closed", (data) => {
  if (!toc || toc.bufferId !== data.buffer_id) return;
  unmountSection();
  toc = null;
});

editor.on("after_insert", (data) => {
  if (!toc || data.buffer_id !== toc.bufferId) return;
  scheduleRescan();
});

editor.on("after_delete", (data) => {
  if (!toc || data.buffer_id !== toc.bufferId) return;
  scheduleRescan();
});

editor.on("config_changed", () => {
  if (!mounted) return;
  editor.floatingPanelControl(PANEL_ID, "sidebar_rows", requestedRows());
  publishFoldingRanges();
});

// =============================================================================
// Commands
// =============================================================================

/** Toggle the section for the active Markdown buffer, regardless of
 *  `autoOpen`. */
function markdownTocToggle(): void {
  if (mounted) {
    unmountSection();
    editor.setStatus(editor.t("status.toc_off"));
    return;
  }
  const bufferId = editor.getActiveBufferId();
  const info = editor.getBufferInfo(bufferId);
  if (!info || !isMarkdownFile(info.path)) {
    editor.setStatus(editor.t("status.not_markdown_file"));
    return;
  }
  track(bufferId, info.path);
  editor.setStatus(editor.t("status.toc_on"));
}
registerHandler("markdownTocToggle", markdownTocToggle);

/** Flip `follow` between cursor and scroll for this session. */
function markdownTocToggleFollow(): void {
  followOverride = followMode() === "cursor" ? "scroll" : "cursor";
  editor.setStatus(
    followOverride === "scroll" ? editor.t("status.follow_scroll") : editor.t("status.follow_cursor"),
  );
  // Re-seed the selection from the new source straight away.
  if (!toc || !mounted) return;
  if (followOverride === "cursor") {
    pushSelected(headingIndexAtOrBefore(toc.headings, editor.getCursorPosition()));
  } else {
    const vp = editor.getViewport();
    if (vp) pushSelected(headingIndexAtOrBefore(toc.headings, vp.topByte));
  }
}
registerHandler("markdownTocToggleFollow", markdownTocToggleFollow);

editor.registerCommand(
  "%cmd.toggle_toc",
  "%cmd.toggle_toc_desc",
  "markdownTocToggle",
  null,
);

editor.registerCommand(
  "%cmd.toggle_follow",
  "%cmd.toggle_follow_desc",
  "markdownTocToggleFollow",
  null,
);

// Initialization
editor.debug("Markdown TOC plugin loaded - use 'Markdown: Toggle Table of Contents' command");
