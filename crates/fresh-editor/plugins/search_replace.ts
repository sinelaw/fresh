/// <reference path="./lib/fresh.d.ts" />
import {
  button,
  col,
  flexSpacer,
  hintBar,
  key as widgetKey,
  parseHintString,
  raw,
  row,
  spacer,
  textInput,
  textInputChar,
  toggle,
  tree,
  treeNode,
  type TreeNode,
  type WidgetAction,
  WidgetPanel,
  type WidgetSpec,
} from "./lib/widgets.ts";

const editor = getEditor();

/**
 * Multi-File Search & Replace Plugin
 *
 * Compact two-line control bar + hierarchical match tree.
 * Direct inline editing of search/replace fields (no prompts).
 * Navigation uses state-managed selectedIndex (like theme_editor).
 */

// =============================================================================
// Types
// =============================================================================

interface SearchResult {
  match: GrepMatch;
  selected: boolean;
}

interface FileGroup {
  relPath: string;
  absPath: string;
  expanded: boolean;
  matches: SearchResult[];
}

type FocusPanel = "query" | "options" | "matches";
type QueryField = "search" | "replace";

interface PanelState {
  resultsBufferId: number;
  sourceSplitId: number;
  resultsSplitId: number;
  searchResults: SearchResult[];
  fileGroups: FileGroup[];
  searchPattern: string;
  replaceText: string;
  // Navigation
  focusPanel: FocusPanel;
  queryField: QueryField;
  optionIndex: number;
  matchIndex: number;
  // Options
  caseSensitive: boolean;
  useRegex: boolean;
  wholeWords: boolean;
  // Layout
  viewportWidth: number;
  // State
  busy: boolean;
  truncated: boolean;
  // Inline editing cursor position
  cursorPos: number;
  // Virtual scroll offset for matches tree
  scrollOffset: number;
  // Per-file expansion state mirrored from the Tree widget's host
  // instance state. The widget owns expansion (host re-renders on
  // disclosure click / Right / Left without the plugin reacting);
  // this set is only read by the plugin's `activate` handler so
  // Enter on a file row can toggle expansion via
  // `panel.setExpandedKeys`. Both sets are cleared at the start of
  // every fresh search.
  expandedFileKeys: Set<string>;
  // Memo of file-row keys we've already seen during the current
  // search. Used by `buildMatchListSpec` to auto-expand newly-
  // discovered files (default = expanded) without overriding user
  // collapse state on previously-seen files.
  knownFileKeys: Set<string>;
  // Widget panel handle. The panel mounts a `Col[Raw{body}, HintBar{hints}]`
  // spec — the body keeps the existing hand-rolled rendering for now,
  // and the footer is built by the host's HintBar widget so its keys are
  // styled consistently with every other plugin's footer (theme-keyed
  // `ui.help_key_fg`). Subsequent migration passes will pull the
  // search/replace inputs, the toggles, and the match tree out of
  // `Raw` and into typed widgets. See
  // `docs/internal/plugin-widget-library-design.md` §10.
  widgetPanel: WidgetPanel | null;
}
let panel: PanelState | null = null;

const MAX_RESULTS = 10000;
const MIN_WIDTH = 60;
const DEFAULT_WIDTH = 100;
const SEARCH_DEBOUNCE_MS = 150;

let searchDebounceGeneration = 0;

// =============================================================================
// Colors
// =============================================================================

type RGB = [number, number, number];

const C = {
  border: [80, 80, 100] as RGB,
  label: [160, 160, 180] as RGB,
  value: [255, 255, 255] as RGB,
  inputBg: [40, 40, 55] as RGB,
  statusOk: [100, 200, 100] as RGB,
  statusDim: [120, 120, 140] as RGB,
  toggleOn: [100, 200, 100] as RGB,
  toggleOff: [100, 100, 120] as RGB,
  button: [80, 140, 220] as RGB,
  buttonFg: [255, 255, 255] as RGB,
  filePath: [220, 160, 80] as RGB,
  fileIcon: [100, 180, 220] as RGB,
  lineNum: [120, 120, 140] as RGB,
  matchBg: [0, 140, 160] as RGB,
  matchFg: [255, 255, 255] as RGB,
  selectedBg: [45, 50, 70] as RGB,
  checkOn: [100, 200, 100] as RGB,
  checkOff: [100, 100, 120] as RGB,
  dim: [90, 90, 110] as RGB,
  expandIcon: [140, 140, 160] as RGB,
  separator: [60, 60, 75] as RGB,
  help: [100, 100, 120] as RGB,
  cursor: [255, 255, 255] as RGB,
  cursorBg: [200, 200, 200] as RGB,
};

// =============================================================================
// Helpers
// =============================================================================

// Codepoint outside the 7-bit ASCII range — used to fast-path
// `byteLen` and `charLen` for the common case (file paths, source
// code) where the JS string's `.length` already equals both the
// UTF-8 byte count and the codepoint count.
const NON_ASCII = /[^\x00-\x7f]/;

function isAscii(s: string): boolean {
  return !NON_ASCII.test(s);
}

function byteLen(s: string): number {
  if (isAscii(s)) return s.length;
  return editor.utf8ByteLength(s);
}

/** Count display columns (codepoints; approximation for monospace terminal). */
function charLen(s: string): number {
  if (isAscii(s)) return s.length;
  let len = 0;
  for (const _c of s) { len++; }
  return len;
}

function padStr(s: string, width: number): string {
  const len = charLen(s);
  if (len >= width) return s;
  return s + " ".repeat(width - len);
}

/** Truncate to at most maxLen display columns (codepoint-aware). */
function truncate(s: string, maxLen: number): string {
  // ASCII fast path: JS substring is equivalent to codepoint slice
  // and skipping the for-of avoids the QuickJS iterator overhead.
  if (isAscii(s)) {
    if (s.length <= maxLen) return s;
    if (maxLen <= 3) return s.slice(0, maxLen);
    return s.slice(0, maxLen - 3) + "...";
  }
  const sLen = charLen(s);
  if (sLen <= maxLen) return s;
  if (maxLen <= 3) {
    let result = "";
    let count = 0;
    for (const c of s) {
      if (count >= maxLen) break;
      result += c;
      count++;
    }
    return result;
  }
  let result = "";
  let count = 0;
  for (const c of s) {
    if (count >= maxLen - 3) break;
    result += c;
    count++;
  }
  return result + "...";
}

// Get the active field's text
function getActiveFieldText(): string {
  if (!panel) return "";
  return panel.queryField === "search" ? panel.searchPattern : panel.replaceText;
}

// Set the active field's text
function setActiveFieldText(text: string): void {
  if (!panel) return;
  if (panel.queryField === "search") {
    panel.searchPattern = text;
  } else {
    panel.replaceText = text;
  }
}

// =============================================================================
// Mode — uses allowTextInput for inline editing (supports all keyboard layouts)
// =============================================================================

// Only explicit bindings for special keys; character input is handled via
// allowTextInput which dispatches unbound characters as mode_text_input events.
const modeBindings: [string, string][] = [
  ["Return", "search_replace_enter"],
  ["Space", "search_replace_space"],
  ["Tab", "search_replace_tab"],
  ["S-Tab", "search_replace_shift_tab"],
  ["Up", "search_replace_nav_up"],
  ["Down", "search_replace_nav_down"],
  ["Left", "search_replace_nav_left"],
  ["Right", "search_replace_nav_right"],
  ["M-c", "search_replace_toggle_case"],
  ["M-r", "search_replace_toggle_regex"],
  ["M-w", "search_replace_toggle_whole_word"],
  ["M-Return", "search_replace_replace_all"],
  ["S-Return", "search_replace_replace_scoped"],
  ["Escape", "search_replace_close"],
  ["Backspace", "search_replace_backspace"],
  ["Delete", "search_replace_delete"],
  ["Home", "search_replace_home"],
  ["End", "search_replace_end"],
];

editor.defineMode("search-replace-list", modeBindings, true, true);

// Printable input flows through the widget runtime: mode_text_input
// → widgetCommand(textInputChar(text)) → host computes new value +
// cursor on the focused TextInput → widget_event "change" → plugin
// updates its model from the event payload (see the widget_event
// handler at the bottom of the file).
function mode_text_input(args: { text: string }): void {
  if (!panel || !args?.text) return;
  panel.widgetPanel?.command(textInputChar(args.text));
}
registerHandler("mode_text_input", mode_text_input);

// =============================================================================
// File grouping
// =============================================================================

function getRelativePath(filePath: string): string {
  const cwd = editor.getCwd();
  if (filePath.startsWith(cwd)) {
    return filePath.slice(cwd.length + 1);
  }
  return filePath;
}

function getFileExtBadge(path: string): string {
  const dot = path.lastIndexOf(".");
  if (dot < 0) return "  ";
  const ext = path.slice(dot + 1).toUpperCase();
  if (ext.length <= 2) return ext.padEnd(2);
  return ext.slice(0, 2);
}

function buildFileGroups(results: SearchResult[]): FileGroup[] {
  const map = new Map<string, SearchResult[]>();
  const order: string[] = [];
  for (const r of results) {
    const key = r.match.file;
    if (!map.has(key)) {
      map.set(key, []);
      order.push(key);
    }
    map.get(key)!.push(r);
  }
  return order.map(absPath => ({
    relPath: getRelativePath(absPath),
    absPath,
    expanded: true,
    matches: map.get(absPath)!,
  }));
}

interface FlatItem {
  type: "file" | "match";
  fileIndex: number;
  matchIndex?: number;
}

// Emit every file row + every match row in declaration order. The
// Tree widget filters out descendants of collapsed nodes at render
// time — the plugin always sends the full hierarchy. Plugin code
// that needs to map a `selected_index` back to the underlying match
// (e.g. `doReplaceScoped`) walks this same flat list.
function buildFlatItems(): FlatItem[] {
  if (!panel) return [];
  const items: FlatItem[] = [];
  for (let fi = 0; fi < panel.fileGroups.length; fi++) {
    items.push({ type: "file", fileIndex: fi });
    const group = panel.fileGroups[fi];
    for (let mi = 0; mi < group.matches.length; mi++) {
      items.push({ type: "match", fileIndex: fi, matchIndex: mi });
    }
  }
  return items;
}

// =============================================================================
// Get actual viewport width
// =============================================================================

function getViewportWidth(): number {
  const vp = editor.getViewport();
  if (vp && vp.width > 0) return vp.width;
  return DEFAULT_WIDTH;
}

function getViewportHeight(): number {
  const vp = editor.getViewport();
  if (vp && vp.height > 0) return vp.height;
  return 30;
}

// =============================================================================
// Panel content builder — compact two-line control bar + match tree
// =============================================================================

// Build the typed Row spec for the options line (3 toggles + Replace
// All button). Was previously hand-built into entries with manual
// byte-offset overlay arithmetic (see git history pre-widget); now
// dispatched through the host's Toggle/Button widgets so styling,
// theme keys, and focus affordance match every other plugin.
function buildOptionsRowSpec(): WidgetSpec {
  if (!panel) return col();
  const { focusPanel, optionIndex, caseSensitive, useRegex, wholeWords } = panel;
  const W = Math.max(MIN_WIDTH, panel.viewportWidth - 2);
  const oFocus = focusPanel === "options";

  // The flex Spacer fills whatever's left of the row so the
  // "Replace All" button right-aligns regardless of label width or
  // panel width. No more byteLen-summing of labels.
  const caseLabel = editor.t("panel.case_toggle");
  const regexLabel = editor.t("panel.regex_toggle");
  const wholeLabel = editor.t("panel.whole_toggle");
  const replLabel = editor.t("panel.replace_all_btn");
  void oFocus;
  void optionIndex;

  return row(
    spacer(1),
    toggle(caseSensitive, caseLabel, { key: "case" }),
    spacer(2),
    toggle(useRegex, regexLabel, { key: "regex" }),
    spacer(2),
    toggle(wholeWords, wholeLabel, { key: "whole" }),
    flexSpacer(),
    button(replLabel, { intent: "primary", key: "replaceAll" }),
  );
}

// Build the typed Row spec for line 1 (search + replace fields with
// trailing match-count stats). Was previously hand-rolled with two
// `buildFieldDisplay` calls + manual cursor overlays; now uses the
// host's TextInput widget for both fields (theme-keyed focus + input
// background, cursor highlight at the right byte position). The
// match-stats portion stays in Raw because it has bespoke
// truncated-warning styling (`[255, 180, 50]`) and isn't a control.
function buildLine1Spec(): WidgetSpec {
  if (!panel) return col();
  const { searchPattern, replaceText, focusPanel, queryField, cursorPos, truncated } = panel;
  const totalMatches = panel.searchResults.length;
  const fileCount = panel.fileGroups.length;
  const qFocusSearch = focusPanel === "query" && queryField === "search";
  const qFocusReplace = focusPanel === "query" && queryField === "replace";
  const searchVal = searchPattern || "";
  const replaceVal = replaceText || "";
  // The plugin tracks `cursorPos` as a character offset; the widget
  // wants a UTF-8 byte offset. For ASCII they're equal; for the
  // multi-byte case we convert via byteLen of the prefix.
  const searchCursorByte = qFocusSearch ? byteLen(searchVal.substring(0, cursorPos)) : -1;
  const replaceCursorByte = qFocusReplace ? byteLen(replaceVal.substring(0, cursorPos)) : -1;
  const searchLabel = editor.t("panel.search_label");
  const replLabel = editor.t("panel.replace_label");

  const truncatedSuffix = truncated ? " " + editor.t("panel.limited") : "";
  const matchStats = totalMatches > 0
    ? "  " + editor.t("panel.match_stats", { count: String(totalMatches), files: String(fileCount) }) + truncatedSuffix
    : (searchPattern ? "  " + editor.t("panel.no_matches") : "");

  // Build the matchStats inline-overlay-styled Raw cell for the row.
  // Truncated case keeps the warning-color tail; otherwise the whole
  // stats string uses the ok/dim color depending on result presence.
  const matchStatsEntries: TextPropertyEntry[] = [];
  if (matchStats.length > 0) {
    const overlays: InlineOverlay[] = [];
    if (truncated && totalMatches > 0) {
      const statsWithoutSuffix = "  " + editor.t("panel.match_stats", {
        count: String(totalMatches),
        files: String(fileCount),
      });
      const countEnd = byteLen(statsWithoutSuffix);
      overlays.push({ start: 0, end: countEnd, style: { fg: C.statusOk } });
      overlays.push({
        start: countEnd,
        end: countEnd + byteLen(truncatedSuffix),
        style: { fg: [255, 180, 50] as RGB, bold: true },
      });
    } else {
      overlays.push({
        start: 0,
        end: byteLen(matchStats),
        style: { fg: totalMatches > 0 ? C.statusOk : C.statusDim },
      });
    }
    matchStatsEntries.push({ text: matchStats, inlineOverlays: overlays });
  }

  return row(
    spacer(1),
    textInput(searchVal, {
      label: searchLabel,
      focused: qFocusSearch,
      cursorByte: searchCursorByte,
      fieldWidth: 25,
      key: "searchField",
    }),
    spacer(2),
    textInput(replaceVal, {
      label: replLabel,
      focused: qFocusReplace,
      cursorByte: replaceCursorByte,
      fieldWidth: 25,
      key: "replaceField",
    }),
    raw(matchStatsEntries),
  );
}

// Stable key for a flat tree item — used as the List item key so
// click events bounce back to the same logical match across
// re-renders. File rows use `file:<n>`; match rows use
// `match:<file>/<m>`.
function flatItemKey(item: FlatItem): string {
  if (item.type === "file") return `file:${item.fileIndex}`;
  return `match:${item.fileIndex}/${item.matchIndex}`;
}

// Render one flat tree item as a single TextPropertyEntry. The
// Tree widget owns the indent (depth * 2 spaces) + disclosure glyph
// (▶ / ▼) prefix and the selection bg — this function emits *just*
// the row's content starting from offset 0 of the row's body. Files
// pass `depth: 0, hasChildren: true`; matches pass `depth: 1,
// hasChildren: false` (see `buildMatchListSpec`).
//
// Overlay offsets are emitted in character (codepoint) units; the
// host converts to byte offsets after applying `padToChars`. This
// keeps the per-row work proportional to the visible content,
// without the per-overlay `editor.utf8ByteLength` bridge calls
// that otherwise dominate hot-path renders.
function renderFlatItemEntry(item: FlatItem, W: number): TextPropertyEntry {
  if (!panel) return { text: "" };
  if (item.type === "file") {
    const group = panel.fileGroups[item.fileIndex];
    const badge = getFileExtBadge(group.relPath);
    const matchCount = group.matches.length;
    const selectedInFile = group.matches.filter(m => m.selected).length;
    // The widget prefixes ` ▶ ` / ` ▼ ` (4 cols) before this body;
    // pad budget = W - 4 (the widget's prefix consumes 4 cols at
    // depth 0).
    const fileLineText = `${badge} ${group.relPath} (${selectedInFile}/${matchCount})`;
    const badgeChars = charLen(badge);
    const pathStart = badgeChars + 1; // " " separator (always 1 codepoint)
    const pathEnd = pathStart + charLen(group.relPath);

    const overlays: InlineOverlay[] = [
      { start: 0, end: badgeChars, style: { fg: C.fileIcon, bold: true }, unit: "char" },
      { start: pathStart, end: pathEnd, style: { fg: C.filePath }, unit: "char" },
    ];

    return {
      text: fileLineText,
      padToChars: Math.max(0, W - 4),
      properties: { type: "file-row", fileIndex: item.fileIndex },
      inlineOverlays: overlays,
    };
  }
  // Match row. The Tree widget's prefix at depth=1 is 6 cols
  // (4 indent + 2 alignment). Use the remaining width for content.
  const group = panel.fileGroups[item.fileIndex];
  const result = group.matches[item.matchIndex!];
  const checkbox = result.selected ? "[v]" : "[ ]";
  const location = `${group.relPath}:${result.match.line}`;
  const context = result.match.context.trim();
  const prefixText = `${checkbox} `;
  const innerWidth = Math.max(0, W - 6); // host prefix consumes 6 cols
  const prefixChars = charLen(prefixText);
  const locationChars = charLen(location);
  const maxCtx = innerWidth - prefixChars - locationChars - 3;
  const displayCtx = truncate(context, Math.max(10, maxCtx));
  const matchLineText = `${prefixText}${location} - ${displayCtx}`;

  const inlines: InlineOverlay[] = [];
  // checkbox is "[v]" or "[ ]" — always 3 ASCII chars.
  const cbEnd = checkbox.length;
  inlines.push({ start: 0, end: cbEnd, style: { fg: result.selected ? C.checkOn : C.checkOff }, unit: "char" });
  const locStart = cbEnd + 1; // " "
  const locEnd = locStart + locationChars;
  inlines.push({ start: locStart, end: locEnd, style: { fg: C.lineNum }, unit: "char" });

  if (panel.searchPattern) {
    const ctxStart = locEnd + 3; // " - " is always 3 ASCII chars
    highlightMatches(displayCtx, panel.searchPattern, ctxStart, panel.useRegex, panel.caseSensitive, inlines);
  }

  return {
    text: matchLineText,
    padToChars: innerWidth,
    properties: { type: "match-row", fileIndex: item.fileIndex, matchIndex: item.matchIndex },
    inlineOverlays: inlines.length > 0 ? inlines : undefined,
  };
}

// Build the typed spec for the matches body — either a Tree widget
// (when there are matches) or a Raw cell with the empty/prompt
// message. The Tree widget owns scroll, selection styling, click
// routing, and host-managed expand/collapse — the plugin sends
// the *full* hierarchy on every render and the host filters
// children of collapsed file rows.
function buildMatchListSpec(): WidgetSpec {
  if (!panel) return col();
  const W = Math.max(MIN_WIDTH, panel.viewportWidth - 2);
  const totalMatches = panel.searchResults.length;

  if (panel.searchPattern && totalMatches === 0) {
    return raw([{
      text: padStr("  " + editor.t("panel.no_matches"), W),
      properties: { type: "empty" },
      style: { fg: C.dim },
    }]);
  }
  if (!panel.searchPattern) {
    return raw([{
      text: padStr("  " + editor.t("panel.type_pattern"), W),
      properties: { type: "empty" },
      style: { fg: C.dim },
    }]);
  }

  const flatItems = buildFlatItems();
  const itemKeys = flatItems.map(flatItemKey);
  // Track the file-row keys present in this render. Newly-discovered
  // file groups are auto-added to `expandedFileKeys` (default state =
  // expanded). Files the user has collapsed remain absent from the
  // set; we never re-add a key that's already known but currently
  // collapsed, since `clearedThisRender` would tag them as "first
  // time seen". Tracking is via the per-search reset in
  // `performSearch`: at the start of a search the set is empty, so
  // every file is auto-added on its first appearance, then user
  // collapse events remove them.
  const nodes: TreeNode[] = flatItems.map((item, i) => {
    const entry = renderFlatItemEntry(item, W);
    if (item.type === "file") {
      const k = itemKeys[i];
      if (!panel!.knownFileKeys.has(k)) {
        panel!.knownFileKeys.add(k);
        panel!.expandedFileKeys.add(k);
      }
      return treeNode(entry, { depth: 0, hasChildren: true });
    }
    return treeNode(entry, { depth: 1, hasChildren: false });
  });
  const selectedIndex = panel.focusPanel === "matches" ? panel.matchIndex : -1;
  // Tree visible rows = panel viewport height minus the chrome
  // (line 1 + options row + separator + footer = 4 rows) — same
  // calculation that sized the previous List.
  const fixedRows = 5;
  const visibleRows = Math.max(3, getViewportHeight() - fixedRows);

  return tree({
    nodes,
    itemKeys,
    selectedIndex,
    visibleRows,
    expandedKeys: [...panel.expandedFileKeys],
    key: "matchTree",
  });
}

// Phase selector for `buildPanelEntries`. The hand-rolled options
// row and line-1 query fields were extracted into typed widget specs
// (`buildOptionsRowSpec`, `buildLine1Spec`); this parameter lets
// callers ask for the body before the options row ("preOptions"),
// the body after it ("postOptions"), or — for tests / fallback
// paths — both with no gap ("all"). Today "preOptions" is empty
// because line 1 lives in `buildLine1Spec`; the parameter remains
// for symmetry and to keep the boundary explicit.
type BuildPhase = "all" | "preOptions" | "postOptions";

function buildPanelEntries(phase: BuildPhase = "all"): TextPropertyEntry[] {
  if (!panel) return [];
  const { searchPattern, replaceText, searchResults, fileGroups, focusPanel, queryField,
    optionIndex, caseSensitive, useRegex, wholeWords, cursorPos } = panel;
  // The line-1 + options-row variables are still destructured for
  // readability with the rest of the function but are now consumed
  // by `buildLine1Spec()` and `buildOptionsRowSpec()` (composed into
  // the spec at update time).
  void searchPattern;
  void replaceText;
  void searchResults;
  void fileGroups;
  void focusPanel;
  void queryField;
  void cursorPos;
  void optionIndex;
  void caseSensitive;
  void useRegex;
  void wholeWords;

  const W = Math.max(MIN_WIDTH, panel.viewportWidth - 2);
  const entries: TextPropertyEntry[] = [];

  const totalMatches = searchResults.length;
  const fileCount = fileGroups.length;

  // ── Line 1 (search/replace fields + match-count stats) is now
  //    rendered by `buildLine1Spec()` — see updatePanelContent. The
  //    pre-options phase therefore returns no entries; the spec
  //    composes the typed Row directly between the col children. ──

  // ── Line 2 (options toggles + Replace All button) is now rendered
  //    by the host as a `Row { Toggle, Toggle, Toggle, Spacer, Button }`
  //    spec — see `buildOptionsRowSpec` and `updatePanelContent`.
  //    `buildPanelEntries` is split into a "pre-options" half (this
  //    function up to here) and a "post-options" tail (everything from
  //    the separator onward). `updatePanelContent` weaves the spec
  //    between them so the visual order stays identical to before. ──
  if (phase === "preOptions") return entries;
  // ── For phase==="postOptions", also drop the line-1 entry pushed
  //    above so the caller can compose: `col(raw(pre), optionsRow,
  //    raw(post), hintBar)` without duplicating line 1.
  if (phase === "postOptions") entries.length = 0;

  // ── Separator ──
  const sepChar = "─";
  const matchesLabel = totalMatches > 0
    ? " " + editor.t("panel.matches_count", { count: String(totalMatches), files: String(fileCount) }) + (panel.truncated ? " " + editor.t("panel.limited") : "") + " "
    : " " + editor.t("panel.matches_title") + " ";
  const sepRemaining = W - charLen(matchesLabel);
  const sepLeft = Math.floor(sepRemaining / 2);
  const sepRight = sepRemaining - sepLeft;
  const sepLine = (sepLeft > 0 ? sepChar.repeat(sepLeft) : "") + matchesLabel + (sepRight > 0 ? sepChar.repeat(sepRight) : "");
  entries.push({
    text: sepLine + "\n",
    properties: { type: "separator" },
    style: { fg: C.separator },
    inlineOverlays: [{
      start: byteLen(sepChar.repeat(sepLeft)),
      end: byteLen(sepChar.repeat(sepLeft) + matchesLabel),
      style: { fg: C.label, bold: true },
    }],
  });

  // ── Matches tree is now rendered by `buildMatchListSpec()` —
  //    see `updatePanelContent`. The List widget owns scroll
  //    offset (auto-clamps to keep selection in view) and click
  //    routing. ──

  // The help footer is no longer pushed here — it's now rendered by
  // the host's HintBar widget (see updatePanelContent).
  return entries;
}

// Build the hint entries for the panel footer.
//
// Source of truth is the existing `panel.help` i18n string (format:
// `Tab:section  ↑↓:nav  …`); `parseHintString` splits it into typed
// `HintEntry[]` so the host's HintBar widget can style the keys
// portion via the `ui.help_key_fg` theme key — matching every other
// plugin's footer.
function buildHelpHints(): HintEntry[] {
  // Source of truth is the existing `panel.help` i18n string. The
  // pre-widget version appended a `↑↓` scroll indicator computed
  // from `panel.scrollOffset`; the List widget now owns scroll
  // state, so the plugin no longer knows the scroll position.
  // Scroll feedback is implicit (the visible window of items shifts
  // visibly when navigating); explicit indicators can come back as
  // a List-emitted prop once needed.
  return parseHintString(editor.t("panel.help"));
}

// Build field display string: [value] with cursor
function buildFieldDisplay(value: string, cursorPos: number, maxLen: number): string {
  const display = value.length > maxLen ? value.slice(0, maxLen - 1) + "…" : value;
  if (cursorPos >= 0) {
    // Show cursor as underscore or pipe at position
    return "[" + display + "]";
  }
  return "[" + display + "]";
}

// Add cursor overlay at the right byte position within a field
function addCursorOverlay(value: string, cursorPos: number, fieldByteStart: number, overlays: InlineOverlay[]): void {
  if (cursorPos < 0) return;
  const beforeCursor = value.substring(0, cursorPos);
  const cursorBytePos = fieldByteStart + byteLen(beforeCursor);
  // Highlight the character at cursor position (or the closing bracket if at end)
  const charAtCursor = cursorPos < value.length ? value.charAt(cursorPos) : "]";
  const cursorByteEnd = cursorBytePos + byteLen(charAtCursor);
  overlays.push({ start: cursorBytePos, end: cursorByteEnd, style: { fg: [0, 0, 0], bg: C.cursorBg } });
}

// Highlight search pattern occurrences in a display string.
//
// `baseCharOffset` is in character (codepoint) units within the
// containing row. Overlays are emitted in the same unit so the
// host can convert them to byte offsets in one pass. For ASCII
// `text` (the common case for source-code matches) overlay math
// uses JS string indices directly without the bridge calls a
// byte-offset path would require.
function highlightMatches(text: string, pattern: string, baseCharOffset: number, isRegex: boolean, caseSensitive: boolean, overlays: InlineOverlay[]): void {
  if (!pattern) return;
  // ASCII-only fast path: JS `indexOf` returns codepoint-equivalent
  // offsets (no surrogate pairs), so `idx` is the char offset
  // directly. The non-ASCII path uses `charLen(prefix)` to convert.
  const textAscii = isAscii(text);
  const patternChars = isAscii(pattern) ? pattern.length : charLen(pattern);
  try {
    if (!isRegex) {
      let searchText = text;
      let searchPat = pattern;
      if (!caseSensitive) {
        searchText = text.toLowerCase();
        searchPat = pattern.toLowerCase();
      }
      let pos = 0;
      while (pos < searchText.length) {
        const idx = searchText.indexOf(searchPat, pos);
        if (idx < 0) break;
        const startChar = baseCharOffset + (textAscii ? idx : charLen(text.substring(0, idx)));
        const endChar = startChar + patternChars;
        overlays.push({ start: startChar, end: endChar, style: { bg: C.matchBg, fg: C.matchFg }, unit: "char" });
        pos = idx + pattern.length;
      }
    } else {
      const flags = caseSensitive ? "g" : "gi";
      const re = new RegExp(pattern, flags);
      let m;
      while ((m = re.exec(text)) !== null) {
        if (m[0].length === 0) { re.lastIndex++; continue; }
        const startChar = baseCharOffset + (textAscii ? m.index : charLen(text.substring(0, m.index)));
        const matchChars = isAscii(m[0]) ? m[0].length : charLen(m[0]);
        const endChar = startChar + matchChars;
        overlays.push({ start: startChar, end: endChar, style: { bg: C.matchBg, fg: C.matchFg }, unit: "char" });
      }
    }
  } catch (_e) { /* invalid regex */ }
}

// =============================================================================
// Panel update
// =============================================================================

function updatePanelContent(): void {
  if (!panel) return;
  // Refresh viewport width each time
  panel.viewportWidth = getViewportWidth();

  // Migration step 4 (see docs/internal/plugin-widget-library-design.md
  // §10): the entire visible panel is now typed widgets except for
  // a single `Raw` separator entry.
  //
  //   * `Row{ Spacer, TextInput, Spacer, TextInput, Raw{ stats } }`
  //                                       — search/replace inputs +
  //                                       trailing match-count stats.
  //   * `Row{ Toggle, Toggle, Toggle, Spacer, Button }`
  //                                       — case/regex/whole + Replace All.
  //   * `Raw{ separator entry }`         — matches divider.
  //   * `List{ ... }` or `Raw{empty msg}` — virtual-scrolled match
  //                                       rows (host owns scroll +
  //                                       selection styling +
  //                                       click routing).
  //   * `HintBar{ ... }`                  — keyboard-hint footer.
  if (!panel.widgetPanel) {
    panel.widgetPanel = new WidgetPanel(panel.resultsBufferId);
  }
  panel.widgetPanel.set(
    col(
      buildLine1Spec(),
      buildOptionsRowSpec(),
      raw(buildPanelEntries("postOptions")),
      buildMatchListSpec(),
      hintBar(buildHelpHints()),
    ),
  );
  // The Tree's `expandedKeys` field on the spec is initial-only —
  // `mountWidgetPanel` seeds the host's instance state, and
  // `updateWidgetPanel` ignores it (instance state is authoritative
  // after first render). So we push expansion changes through the
  // explicit mutator on every update; this covers the case where
  // a new file group enters the result set in a later search and
  // needs to be force-expanded by default. The mutator is a no-op
  // when the tree isn't mounted yet (first `set()` call).
  if (panel.searchPattern && panel.searchResults.length > 0) {
    panel.widgetPanel.setExpandedKeys(
      "matchTree",
      [...panel.expandedFileKeys],
    );
  }
}

// =============================================================================
// Search
// =============================================================================

/** Current search generation — incremented on each new search to discard stale results. */
let currentSearchGeneration = 0;

/**
 * Perform a streaming search. Results arrive incrementally per-file via the
 * progress callback and are merged into the panel state as they arrive.
 * Returns the final complete list of results.
 */
async function performSearch(pattern: string, silent?: boolean): Promise<SearchResult[]> {
  if (!panel) return [];

  const generation = ++currentSearchGeneration;
  // Each fresh search resets the per-file expansion set: previous
  // results may have included files that don't appear in the new
  // result set, and the user's collapse state for the *previous*
  // result set isn't meaningful for the new one.
  panel.expandedFileKeys.clear();
  panel.knownFileKeys.clear();
  let lastUiUpdate = Date.now();
  const UI_UPDATE_INTERVAL_MS = 100; // Force maximum 10 UI updates per second

  try {
    const fixedString = !panel.useRegex;
    let allResults: SearchResult[] = [];

    // Whole-word filtering is done Rust-side so maxResults is respected correctly
    const result = await editor.grepProjectStreaming(
      pattern,
      {
        fixedString,
        caseSensitive: panel.caseSensitive,
        maxResults: MAX_RESULTS,
        wholeWords: panel.wholeWords,
      },
      (matches: GrepMatch[], done: boolean) => {
        // Discard if a newer search has started
        if (generation !== currentSearchGeneration || !panel) return;

        if (matches.length > 0) {
          // Use push loop instead of allResults.concat() to save massive memory allocations
          for (const m of matches) {
            allResults.push({ match: m, selected: true });
          }
          panel.searchResults = allResults;
        }

        const now = Date.now();
        // Only trigger the expensive UI rebuild if enough time passed or stream finished
        if (done || now - lastUiUpdate > UI_UPDATE_INTERVAL_MS) {
          panel.fileGroups = buildFileGroups(allResults);
          updatePanelContent();
          lastUiUpdate = now;
        }
      }
    );

    // Final state
    if (generation !== currentSearchGeneration || !panel) return allResults;

    panel.truncated = !!(result && (result as any).truncated);

    if (!silent) {
      if (allResults.length === 0) {
        editor.setStatus(editor.t("status.no_matches", { pattern }));
      } else if (panel.truncated) {
        editor.setStatus(editor.t("status.found_matches", { count: String(allResults.length) }) + " " + editor.t("panel.limited"));
      } else {
        editor.setStatus(editor.t("status.found_matches", { count: String(allResults.length) }));
      }
    }
    return allResults;
  } catch (e) {
    if (!silent) {
      editor.setStatus(editor.t("status.search_error", { error: String(e) }));
    }
    return [];
  }
}

// =============================================================================
// Panel lifecycle
// =============================================================================

async function openPanel(): Promise<void> {
  // Try to pre-fill search from editor selection
  let prefill = "";
  try {
    const cursor = editor.getPrimaryCursor();
    if (cursor && cursor.selection) {
      const start = Math.min(cursor.selection.start, cursor.selection.end);
      const end = Math.max(cursor.selection.start, cursor.selection.end);
      if (end - start > 0 && end - start < 200) {
        const bufferId = editor.getActiveBufferId();
        const text = await editor.getBufferText(bufferId, start, end);
        if (text && !text.includes("\n")) {
          prefill = text;
        }
      }
    }
  } catch (_e) { /* no selection */ }

  if (panel) {
    panel.focusPanel = "query";
    panel.queryField = "search";
    if (prefill) panel.searchPattern = prefill;
    panel.cursorPos = panel.searchPattern.length;
    updatePanelContent();
    return;
  }

  const sourceSplitId = editor.getActiveSplitId();

  panel = {
    resultsBufferId: 0,
    sourceSplitId,
    resultsSplitId: 0,
    searchResults: [],
    fileGroups: [],
    searchPattern: prefill,
    replaceText: "",
    focusPanel: "query",
    queryField: "search",
    optionIndex: 0,
    matchIndex: 0,
    caseSensitive: false,
    useRegex: false,
    wholeWords: false,
    viewportWidth: DEFAULT_WIDTH,
    busy: false,
    truncated: false,
    cursorPos: prefill.length,
    scrollOffset: 0,
    expandedFileKeys: new Set<string>(),
    knownFileKeys: new Set<string>(),
    widgetPanel: null,
  };

  try {
    const result = await editor.createVirtualBufferInSplit({
      name: "*Search/Replace*",
      mode: "search-replace-list",
      readOnly: true,
      entries: buildPanelEntries(),
      ratio: 0.6,
      panelId: "search-replace-panel",
      // Opt into the Utility Dock (issue #1796 / Section 2 of
      // docs/internal/tui-editor-layout-design.md). When the dock
      // already exists, the editor swaps the dock's active buffer
      // to the search-replace panel instead of spawning a new split.
      role: "utility_dock",
      showLineNumbers: false,
      showCursors: false,
      editingDisabled: true,
    });
    panel.resultsBufferId = result.bufferId;
    panel.resultsSplitId = result.splitId ?? editor.getActiveSplitId();
    editor.debug(`Search/Replace: panel opened, bufferId=${result.bufferId}, splitId=${result.splitId}`);

    // Now we have the split, refresh width
    panel.viewportWidth = getViewportWidth();
    updatePanelContent();
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    editor.setStatus(editor.t("status.failed_open_panel"));
    editor.debug(`ERROR: createVirtualBufferInSplit failed: ${errorMessage}`);
    panel = null;
  }
}

// =============================================================================
// Replacements
// =============================================================================

async function executeReplacements(results?: SearchResult[]): Promise<string> {
  if (!panel) return "";
  const toReplace = results || panel.searchResults.filter(r => r.selected);
  if (toReplace.length === 0) {
    return editor.t("status.no_selected");
  }

  const fileGroups: Map<string, Array<[number, number]>> = new Map();
  for (const result of toReplace) {
    const file = result.match.file;
    if (!fileGroups.has(file)) {
      fileGroups.set(file, []);
    }
    fileGroups.get(file)!.push([result.match.byteOffset, result.match.length]);
  }

  let filesModified = 0;
  let replacementsCount = 0;
  const errors: string[] = [];

  const keys: string[] = [];
  fileGroups.forEach((_v, k) => keys.push(k));
  for (const filePath of keys) {
    const matches = fileGroups.get(filePath)!;
    try {
      const result = await editor.replaceInFile(filePath, matches, panel.replaceText);
      replacementsCount += result.replacements;
      if (result.replacements > 0) filesModified++;
    } catch (e) {
      errors.push(`${filePath}: ${e instanceof Error ? e.message : String(e)}`);
    }
  }

  if (errors.length > 0) {
    editor.debug(`Replacement errors: ${errors.join(", ")}`);
    return editor.t("status.replaced_with_errors", { files: String(filesModified), errors: String(errors.length) });
  }
  return editor.t("status.replaced", { count: String(replacementsCount), files: String(filesModified) });
}

// =============================================================================
// Re-search
// =============================================================================

async function rerunSearch(): Promise<void> {
  if (!panel || !panel.searchPattern) return;
  if (panel.busy) return; // guard against re-entrant search
  panel.truncated = false;
  panel.busy = true;
  panel.matchIndex = 0;
  panel.scrollOffset = 0;
  const results = await performSearch(panel.searchPattern);
  // performSearch already updates panel.searchResults/fileGroups incrementally;
  // just ensure final state is consistent
  if (panel) {
    panel.searchResults = results;
    panel.fileGroups = buildFileGroups(results);
    panel.busy = false;
    updatePanelContent();
  }
}

function rerunSearchDebounced(): void {
  const gen = ++searchDebounceGeneration;
  editor.delay(SEARCH_DEBOUNCE_MS).then(() => {
    if (gen === searchDebounceGeneration) {
      rerunSearch();
    }
  });
}

// Same as rerunSearch but doesn't update status bar (preserves replacement message)
async function rerunSearchQuiet(): Promise<void> {
  if (!panel || !panel.searchPattern) return;
  if (panel.busy) return;
  panel.busy = true;
  const results = await performSearch(panel.searchPattern, true);
  if (panel) {
    panel.searchResults = results;
    panel.fileGroups = buildFileGroups(results);
    panel.matchIndex = 0;
    panel.scrollOffset = 0;
    panel.busy = false;
    updatePanelContent();
  }
}

// =============================================================================
// Text editing handlers (inline editing of query fields)
// =============================================================================

// All editing / navigation keys route through the widget runtime
// via the smart `Key` dispatch — the host knows which widget is
// focused and routes accordingly (Backspace into TextInput; Up/Down
// across List rows; Enter/Space activate Toggle/Button/List;
// printable Space inserts into TextInput; Tab/Shift+Tab cycles
// focus). See WidgetAction::Key for the full table.
function dispatch(action: WidgetAction): void {
  panel?.widgetPanel?.command(action);
}

registerHandler("search_replace_backspace", () => dispatch(widgetKey("Backspace")));
registerHandler("search_replace_delete",    () => dispatch(widgetKey("Delete")));
registerHandler("search_replace_home",      () => dispatch(widgetKey("Home")));
registerHandler("search_replace_end",       () => dispatch(widgetKey("End")));
registerHandler("search_replace_nav_left",  () => dispatch(widgetKey("Left")));
registerHandler("search_replace_nav_right", () => dispatch(widgetKey("Right")));
registerHandler("search_replace_nav_up",    () => dispatch(widgetKey("Up")));
registerHandler("search_replace_nav_down",  () => dispatch(widgetKey("Down")));

// Tab / Shift+Tab now cycle focus through the host's tabbable
// widget set (declared in spec via `key`s — searchField,
// replaceField, case, regex, whole, replaceAll, matchTree).
// The host re-renders with focus styling on the new widget; the
// plugin needn't track focusPanel/queryField/optionIndex anymore
// (the legacy fields linger in PanelState until the rest of the
// plugin migrates off them).
registerHandler("search_replace_tab",       () => dispatch(widgetKey("Tab")));
registerHandler("search_replace_shift_tab", () => dispatch(widgetKey("Shift+Tab")));

// Left/Right route through the smart-key dispatcher: the host
// expands/collapses Tree nodes (when the matchTree is focused) or
// moves the TextInput cursor (when a search/replace field is
// focused). Plugin no longer needs separate file-row expand
// handling.

// Global option toggles (Alt+C, Alt+R, Alt+W)
function search_replace_toggle_case(): void {
  if (!panel) return;
  panel.caseSensitive = !panel.caseSensitive;
  updatePanelContent();
  rerunSearchDebounced();
}
registerHandler("search_replace_toggle_case", search_replace_toggle_case);

function search_replace_toggle_regex(): void {
  if (!panel) return;
  panel.useRegex = !panel.useRegex;
  updatePanelContent();
  rerunSearchDebounced();
}
registerHandler("search_replace_toggle_regex", search_replace_toggle_regex);

function search_replace_toggle_whole_word(): void {
  if (!panel) return;
  panel.wholeWords = !panel.wholeWords;
  updatePanelContent();
  rerunSearchDebounced();
}
registerHandler("search_replace_toggle_whole_word", search_replace_toggle_whole_word);

function search_replace_replace_all(): void {
  doReplaceAll();
}
registerHandler("search_replace_replace_all", search_replace_replace_all);

function search_replace_replace_scoped(): void {
  doReplaceScoped();
}
registerHandler("search_replace_replace_scoped", search_replace_replace_scoped);

// =============================================================================
// Action handlers
// =============================================================================

// Enter / Space route to the widget runtime. The host decides what
// each does based on the focused widget kind:
//   * Toggle (case/regex/whole) → fires `widget_event` "toggle".
//   * Button (replaceAll)       → fires `widget_event` "activate".
//   * Tree   (matchTree)        → fires `widget_event` "activate"
//                                  with the focused row's index/key.
//                                  Plugin handler opens the match
//                                  for leaf rows or toggles
//                                  expansion for file rows.
//   * TextInput + Space         → inserts " " (fires "change").
//   * TextInput + Enter         → no-op (plugin can still bind a
//                                  separate handler if it wants
//                                  Enter to mean "submit").
// Per-event handling lives in the `widget_event` listener below.
registerHandler("search_replace_enter", () => dispatch(widgetKey("Enter")));
registerHandler("search_replace_space", () => dispatch(widgetKey("Space")));

async function doReplaceAll(): Promise<void> {
  if (!panel || panel.busy) return;
  const selected = panel.searchResults.filter(r => r.selected);
  if (selected.length === 0) {
    editor.setStatus(editor.t("status.no_items_selected"));
    return;
  }
  // Confirm before applying.  Replacements write to disk immediately; Undo
  // only covers files that remain open in this session (see bug #1 report).
  const fileCount = new Set(selected.map(r => r.match.file)).size;
  const confirmed = await editor.prompt(
    editor.t("prompt.confirm_replace", {
      count: String(selected.length),
      files: String(fileCount),
    }),
    "",
  );
  if (confirmed === null) {
    editor.setStatus(editor.t("status.replace_cancelled"));
    return;
  }
  panel.busy = true;
  editor.setStatus(editor.t("status.replacing", { count: String(selected.length) }));
  const statusMsg = await executeReplacements(selected);
  editor.setStatus(statusMsg);
  // Clear stale results before re-searching: the byte offsets in
  // `panel.searchResults` now point at positions in the pre-replacement
  // file and must never be re-used (see bug #4 — a second Alt+Enter would
  // otherwise corrupt files by writing into moved offsets).  We also drop
  // `busy` so rerunSearchQuiet doesn't bail out on its own guard.
  panel.searchResults = [];
  panel.fileGroups = [];
  panel.busy = false;
  await rerunSearchQuiet();
  updatePanelContent();
}

async function doReplaceScoped(): Promise<void> {
  if (!panel || panel.busy || panel.focusPanel !== "matches") return;
  const flat = buildFlatItems();
  const item = flat[panel.matchIndex];
  if (!item) return;

  let toReplace: SearchResult[] = [];
  if (item.type === "file") {
    toReplace = panel.fileGroups[item.fileIndex].matches.filter(m => m.selected);
  } else {
    const result = panel.fileGroups[item.fileIndex].matches[item.matchIndex!];
    if (result.selected) toReplace = [result];
  }

  if (toReplace.length === 0) {
    editor.setStatus(editor.t("status.no_selected"));
    return;
  }

  const fileCount = new Set(toReplace.map(r => r.match.file)).size;
  const confirmed = await editor.prompt(
    editor.t("prompt.confirm_replace", {
      count: String(toReplace.length),
      files: String(fileCount),
    }),
    "",
  );
  if (confirmed === null) {
    editor.setStatus(editor.t("status.replace_cancelled"));
    return;
  }

  panel.busy = true;
  editor.setStatus(editor.t("status.replacing", { count: String(toReplace.length) }));
  const statusMsg = await executeReplacements(toReplace);
  editor.setStatus(statusMsg);
  // See doReplaceAll — clear stale offsets and drop busy before re-searching.
  panel.searchResults = [];
  panel.fileGroups = [];
  panel.busy = false;
  await rerunSearchQuiet();
  updatePanelContent();
}

function search_replace_close(): void {
  if (!panel) return;
  panel.widgetPanel?.unmount();
  editor.closeBuffer(panel.resultsBufferId);
  if (panel.resultsSplitId !== panel.sourceSplitId) {
    editor.closeSplit(panel.resultsSplitId);
  }
  panel = null;
  editor.setStatus(editor.t("status.closed"));
}
registerHandler("search_replace_close", search_replace_close);

// =============================================================================
// Command entry point
// =============================================================================

function start_search_replace(): void {
  openPanel();
}
registerHandler("start_search_replace", start_search_replace);

// =============================================================================
// Event handlers (resize updates width)
// =============================================================================



editor.on("resize", (data) => {
  if (!panel) return;
  // Try viewport first (gives actual split width), fall back to terminal width estimate
  const vp = editor.getViewport();
  if (vp && vp.width > 0) {
    panel.viewportWidth = vp.width;
  } else {
    // Approximate: panel split is ~40% of terminal (ratio=0.6 means source gets 60%)
    panel.viewportWidth = Math.floor(data.width * 0.4);
  }
  updatePanelContent();
});

// Prompt handlers (in case prompts are opened externally for this panel - gracefully handle)

editor.on("prompt_cancelled", (args) => {
  if (!args.prompt_type.startsWith("search-replace-")) return true;
  return true;
});

// If the panel's virtual buffer is closed externally (via the × button,
// the Close Buffer/Close Tab commands, or anything else), reset the
// plugin's internal state so the next invocation of `openPanel` creates
// a fresh buffer/split instead of trying to update a buffer that no
// longer exists (which silently no-ops and leaves the user with no UI).

editor.on("buffer_closed", (args) => {
  if (panel && args.buffer_id === panel.resultsBufferId) {
    panel.widgetPanel?.unmount();
    panel = null;
  }
});

// Click → semantic event. The host hit-tests mouse clicks against the
// mounted widget panel and fires `widget_event` for clicks that land
// on a Toggle or Button. We dispatch on `widget_key` (set in
// `buildOptionsRowSpec`); the existing keyboard-driven path
// (Alt+C / Alt+R / Alt+W / Alt+Ret) still works unchanged.
//
// Mouse-click on a toggle should also focus it, so the user's next
// Tab cycle starts from the clicked control. We do that by syncing
// `focusPanel`/`optionIndex` to the clicked widget before applying
// the state change.
editor.on("widget_event", (args) => {
  if (!panel || args.panel_id !== panel.widgetPanel?.id()) return;

  // `change` — fired for TextInput edits (Backspace, Delete,
  // arrows, Home/End, mode_text_input). Payload carries the new
  // value and cursor byte offset. The host already updated the
  // widget's instance state in place; we just sync the plugin's
  // model. **No** `updatePanelContent()` here — the widget has
  // already painted, and the rest of the spec doesn't depend on
  // the field value. This is the IPC fast path discussed in §3
  // of the design doc Q&A.
  if (args.event_type === "change") {
    const payload = args.payload as
      | { value?: string; cursorByte?: number }
      | undefined;
    if (typeof payload?.value !== "string") return;
    const cursorByte = typeof payload.cursorByte === "number"
      ? payload.cursorByte
      : payload.value.length;
    if (args.widget_key === "searchField") {
      panel.searchPattern = payload.value;
      panel.cursorPos = byteToCharOffset(payload.value, cursorByte);
      rerunSearchDebounced();
    } else if (args.widget_key === "replaceField") {
      panel.replaceText = payload.value;
      panel.cursorPos = byteToCharOffset(payload.value, cursorByte);
    }
    return;
  }

  // `select` — fired when the user clicks a Tree row or the host
  // moves selection (Up/Down). The host already updated the
  // tree's selectedIndex in instance state; mirror it into the
  // plugin model and skip re-emit.
  if (args.event_type === "select") {
    const idx = (args.payload as { index?: number } | undefined)?.index;
    if (typeof idx === "number") {
      panel.matchIndex = idx;
    }
    return;
  }

  // `expand` — fired when the host changes a Tree node's
  // expansion state (Right/Left key, or click on the disclosure
  // glyph). Mirror the change into our local set so a subsequent
  // file-row Enter (which goes through `setExpandedKeys`) reads
  // the right state.
  if (args.event_type === "expand") {
    const payload = args.payload as
      | { key?: string; expanded?: boolean }
      | undefined;
    if (typeof payload?.key === "string" && typeof payload.expanded === "boolean") {
      if (payload.expanded) panel.expandedFileKeys.add(payload.key);
      else panel.expandedFileKeys.delete(payload.key);
    }
    return;
  }

  // `activate` — fired by Enter/Space on a focused Button or Tree.
  // For the Replace All button: run replace. For the matchTree:
  // open the focused match's source location, or toggle expansion
  // for file rows (so Enter is a shortcut for Right/Left/click).
  if (args.event_type === "activate") {
    if (args.widget_key === "replaceAll") {
      doReplaceAll();
      return;
    }
    if (args.widget_key === "matchTree") {
      const idx = (args.payload as { index?: number } | undefined)?.index;
      if (typeof idx !== "number") return;
      const flat = buildFlatItems();
      const item = flat[idx];
      if (!item) return;
      if (item.type === "file") {
        const k = `file:${item.fileIndex}`;
        if (panel.expandedFileKeys.has(k)) {
          panel.expandedFileKeys.delete(k);
        } else {
          panel.expandedFileKeys.add(k);
        }
        panel.widgetPanel?.setExpandedKeys(
          "matchTree",
          [...panel.expandedFileKeys],
        );
      } else {
        const group = panel.fileGroups[item.fileIndex];
        const result = group.matches[item.matchIndex!];
        editor.openFileInSplit(
          panel.sourceSplitId,
          result.match.file,
          result.match.line,
          result.match.column,
        );
      }
      return;
    }
  }

  // `toggle` — fired by Enter/Space on a Toggle and by mouse click.
  // The host fires the event but doesn't mutate the spec's
  // `checked` field — the plugin owns its model and pushes the
  // new state back via the targeted `setChecked` mutator (cheaper
  // than a full spec re-emit). The search rerun happens
  // independently on debounce; when it finishes it re-emits the
  // full spec with new matches.
  if (args.event_type === "toggle") {
    const newChecked = (args.payload as { checked?: boolean } | undefined)
      ?.checked;
    if (typeof newChecked !== "boolean") return;
    switch (args.widget_key) {
      case "case":
        panel.caseSensitive = newChecked;
        panel.widgetPanel?.setChecked("case", newChecked);
        rerunSearchDebounced();
        break;
      case "regex":
        panel.useRegex = newChecked;
        panel.widgetPanel?.setChecked("regex", newChecked);
        rerunSearchDebounced();
        break;
      case "whole":
        panel.wholeWords = newChecked;
        panel.widgetPanel?.setChecked("whole", newChecked);
        rerunSearchDebounced();
        break;
    }
  }
});

// Convert a UTF-8 byte offset into a JS-string character offset,
// because the host's TextInput cursor model uses bytes (matching the
// inline-overlay coordinate space) but the plugin's existing code
// stores `panel.cursorPos` as a char offset. Pure walk over the
// string until we hit `byteOffset`.
function byteToCharOffset(value: string, byteOffset: number): number {
  let bytes = 0;
  for (let i = 0; i < value.length; i++) {
    if (bytes >= byteOffset) return i;
    bytes += byteLen(value[i]);
  }
  return value.length;
}

editor.registerCommand(
  "%cmd.search_replace",
  "%cmd.search_replace_desc",
  "start_search_replace",
  null
);

editor.debug("Search & Replace plugin loaded");
