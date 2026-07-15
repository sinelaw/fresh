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
  type StyledSegment,
  styledRow,
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
  // Interval-marker anchor (issue #2583). For a match whose file is open we
  // register a marker at its byte range; the editor keeps it shifted across
  // every edit kind — including bulk paste / multi-cursor / type-over-
  // selection / query-replace / LSP code-action rewrites that apply as a
  // single BulkEdit and bypass the after_insert/after_delete hooks. These
  // hold the key + buffer so we can read the live position back and delete
  // the marker later. Absent for matches in files that aren't open (those
  // can't be edited, so the grep position stays valid as a fallback).
  markerKey?: string;
  markerBufferId?: number;
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
  // Optional comma-separated file globs. Basename patterns such as `*.ts`
  // match at any depth; path patterns such as `src/**` match paths relative
  // to the workspace root.
  fileGlob: string;
  // Navigation
  focusPanel: FocusPanel;
  queryField: QueryField;
  optionIndex: number;
  matchIndex: number;
  // Options
  caseSensitive: boolean;
  useRegex: boolean;
  wholeWords: boolean;
  // Scope (§1): when false, results are restricted to the source buffer.
  // `sourceBufferPath` is the absolute path of the buffer that was
  // active when the panel opened; `sourceBufferRelPath` is the
  // cwd-relative display form. An empty path means the source buffer is
  // unnamed/unsaved — in that case scope filtering falls back to
  // `sourceBufferId`, and the host searches that buffer's in-memory
  // content directly (it has no on-disk file the project walk can find).
  allFiles: boolean;
  sourceBufferPath: string;
  sourceBufferRelPath: string;
  // Buffer id of the source buffer, used to scope/replace unnamed buffers
  // by id when there's no path to match on. 0 means "unknown".
  sourceBufferId: number;
  // Layout
  viewportWidth: number;
  // State
  busy: boolean;
  /** True once the current `searchPattern` has been used to run a real
   * search to completion. Reset whenever the pattern is mutated (or a
   * search-affecting toggle changes). Distinguishes "user is typing,
   * no search has happened" from "search ran and found nothing", so we
   * don't show a misleading "No matches" placeholder before any work
   * has been done. See §17 of
   * `docs/internal/search-replace-scope-replan-on-widgets.md`. */
  searchPerformed: boolean;
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

/** Most-recent-first history of search patterns, capped at HISTORY_MAX.
 *  Up arrow in the search field walks back into older entries; Down
 *  walks forward. Persistence across editor restarts is a follow-up.
 *  See §11 of docs/internal/search-replace-scope-replan-on-widgets.md. */
const searchHistory: string[] = [];
const HISTORY_MAX = 20;
/** -1 = not navigating history. 0..searchHistory.length-1 = currently
 *  displaying the history entry at that index. */
let historyIndex = -1;
/** Whatever the user had in the search field before they pressed Up
 *  to enter history-walk mode. Restored when they Down past the most
 *  recent history entry. */
let historySavedPattern: string | null = null;
/** Most recent widget_event we saw a widget_key for. Used to decide
 *  whether Up/Down should walk history (when focus appears to be on
 *  the search field) or fall through to the widget runtime. The
 *  widget runtime doesn't expose focus directly to the plugin, but
 *  every event that's relevant (change/select/toggle/activate/expand)
 *  carries widget_key. Best-effort proxy. */
let lastFocusedWidget: string | null = null;

function historyPush(pattern: string): void {
  if (!pattern) return;
  const existing = searchHistory.indexOf(pattern);
  if (existing === 0) return;
  if (existing > 0) searchHistory.splice(existing, 1);
  searchHistory.unshift(pattern);
  if (searchHistory.length > HISTORY_MAX) {
    searchHistory.length = HISTORY_MAX;
  }
}

// "Has the user settled on this query?" lives on a separate timer
// from the 150ms search debounce. Pushing to history on every
// debounce tick captures intermediate prefixes (typing "foo bar"
// in fits and starts → "f", "fo", "foo", … all end up in history).
// Wait 2 seconds of pattern-stability before pushing; any change
// cancels the pending push.
const HISTORY_SETTLE_MS = 2000;
let historySettleGeneration = 0;
function scheduleHistoryPush(pattern: string): void {
  if (!pattern) return;
  const gen = ++historySettleGeneration;
  editor.delay(HISTORY_SETTLE_MS).then(() => {
    if (gen !== historySettleGeneration) return;
    if (!panel || panel.searchPattern !== pattern) return;
    if (historyIndex >= 0) return; // walking history; not user input
    historyPush(pattern);
  });
}

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

function byteLen(s: string): number {
  return editor.utf8ByteLength(s);
}

/** Count display columns (codepoints; approximation for monospace terminal). */
function charLen(s: string): number {
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
    if (panel.searchPattern !== text) {
      // Pattern changed → any cached result no longer applies. See §17.
      panel.searchPerformed = false;
    }
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
  ["PageUp", "search_replace_nav_page_up"],
  ["PageDown", "search_replace_nav_page_down"],
  ["Left", "search_replace_nav_left"],
  ["Right", "search_replace_nav_right"],
  ["M-c", "search_replace_toggle_case"],
  ["M-r", "search_replace_toggle_regex"],
  ["M-w", "search_replace_toggle_whole_word"],
  ["M-Return", "search_replace_replace_all"],
  ["S-Return", "search_replace_replace_scoped"],
  // Match navigation (issue #2434). Also bound in keymaps/default.json
  // for the editor (normal) context; binding them in the panel mode too
  // means the same key advances matches whether focus is in the panel
  // or back in the source buffer after an edit.
  ["C-M-Right", "search_replace_next_match"],
  ["C-M-Left", "search_replace_prev_match"],
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

/** Natural ("human") ordering of two display paths (#2435). Splits each
 *  string into alternating non-digit / digit runs and compares digit
 *  runs by numeric value so `file2` sorts before `file10` rather than
 *  after it. Non-digit runs compare case-insensitively first (so `B`
 *  doesn't jump ahead of `a`) with a case-sensitive tiebreak to keep
 *  the order total and stable. Path separators are ordinary characters,
 *  which keeps each directory's files grouped together. */
function naturalCompare(a: string, b: string): number {
  const re = /\d+|\D+/g;
  const ap = a.match(re) ?? [];
  const bp = b.match(re) ?? [];
  const n = Math.min(ap.length, bp.length);
  for (let i = 0; i < n; i++) {
    const x = ap[i];
    const y = bp[i];
    const xNum = x.charCodeAt(0) >= 48 && x.charCodeAt(0) <= 57;
    const yNum = y.charCodeAt(0) >= 48 && y.charCodeAt(0) <= 57;
    if (xNum && yNum) {
      const dx = parseInt(x, 10);
      const dy = parseInt(y, 10);
      if (dx !== dy) return dx < dy ? -1 : 1;
      // Equal value but different width (leading zeros): shorter first.
      if (x.length !== y.length) return x.length - y.length;
    } else {
      const xl = x.toLowerCase();
      const yl = y.toLowerCase();
      if (xl !== yl) return xl < yl ? -1 : 1;
      if (x !== y) return x < y ? -1 : 1;
    }
  }
  return ap.length - bp.length;
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
  const groups = order.map(absPath => ({
    relPath: getRelativePath(absPath),
    absPath,
    expanded: true,
    matches: map.get(absPath)!,
  }));
  groups.sort((a, b) => naturalCompare(a.relPath, b.relPath));
  return groups;
}

/** Re-order `panel.fileGroups` into natural path order in place,
 *  preserving each file's expansion state across the re-index. The
 *  streaming search path appends files in completion order (the project
 *  walk runs them through a parallel pool, so arrival order is
 *  effectively random); call this once a search finishes so the
 *  reviewer sees a stable, sorted list (#2435). Expansion keys are
 *  positional (`file:<index>`), so the set is rebuilt from the files
 *  that were expanded before the sort. */
function sortFileGroups(): void {
  if (!panel) return;
  const expandedPaths = new Set<string>();
  for (let i = 0; i < panel.fileGroups.length; i++) {
    if (panel.expandedFileKeys.has(`file:${i}`)) {
      expandedPaths.add(panel.fileGroups[i].absPath);
    }
  }
  panel.fileGroups.sort((a, b) => naturalCompare(a.relPath, b.relPath));
  panel.expandedFileKeys.clear();
  panel.knownFileKeys.clear();
  for (let i = 0; i < panel.fileGroups.length; i++) {
    const key = `file:${i}`;
    panel.knownFileKeys.add(key);
    if (expandedPaths.has(panel.fileGroups[i].absPath)) {
      panel.expandedFileKeys.add(key);
    }
  }
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
  const { focusPanel, optionIndex, caseSensitive, useRegex, wholeWords, allFiles } = panel;
  const W = Math.max(MIN_WIDTH, panel.viewportWidth - 2);
  const oFocus = focusPanel === "options";

  const caseLabel = editor.t("panel.case_toggle");
  const regexLabel = editor.t("panel.regex_toggle");
  const wholeLabel = editor.t("panel.whole_toggle");
  const allFilesLabel = editor.t("panel.all_files_toggle");
  // Replace All button label tracks scope (§1):
  //   * allFiles=true  → "Replace All (Alt+Ret)"
  //   * allFiles=false → "Replace All in <file> (Alt+Ret)"
  // sourceBufferRelPath is empty for an unsaved buffer, in which
  // case we fall back to the all-files label since restricting to
  // a path-less buffer can't match anything anyway.
  const replLabel = (!allFiles && panel.sourceBufferRelPath)
    ? editor.t("panel.replace_all_in_file_btn", { file: panel.sourceBufferRelPath })
    : editor.t("panel.replace_all_btn");
  void oFocus;
  void optionIndex;
  void W;

  return row(
    spacer(1),
    toggle(allFiles, allFilesLabel, { key: "allFiles" }),
    spacer(2),
    toggle(caseSensitive, caseLabel, { key: "case" }),
    spacer(2),
    toggle(useRegex, regexLabel, { key: "regex" }),
    spacer(2),
    toggle(wholeWords, wholeLabel, { key: "whole" }),
    flexSpacer(),
    button(replLabel, { intent: "primary", key: "replaceAll" }),
  );
}

// Build the scope-info row shown only when allFiles=false. Tells the
// user which single file the search is restricted to. When allFiles=true
// the function returns an empty col() (the spec composer skips it).
function buildScopeRowSpec(): WidgetSpec {
  if (!panel) return col();
  if (panel.allFiles) return col();
  const label = panel.sourceBufferRelPath
    ? editor.t("panel.scope_row_file", { file: panel.sourceBufferRelPath })
    : editor.t("panel.scope_row_unnamed");
  return raw([{
    text: " " + label,
    properties: { type: "scope-row" },
    style: { fg: C.label, italic: true },
  }]);
}

// Build the typed Row spec for line 1 (search + replace fields with
// trailing match-count stats). Was previously hand-rolled with two
// `buildFieldDisplay` calls + manual cursor overlays; now uses the
// host's TextInput widget for both fields (theme-keyed focus + input
// background, cursor highlight at the right byte position). The
// match-stats portion stays in Raw because it has bespoke
// truncated-warning styling (`[255, 180, 50]`) and isn't a control.
// Build just the matchStats text + inline overlays. Pulled out of
// `buildLine1Spec` so the streaming pump can refresh it via the
// `setRawEntries` mutation on the keyed `matchStats` raw widget —
// without re-emitting the full panel spec (which forces js_to_json
// over the entire 5 000-node tree and blocks the JS thread).
function buildMatchStatsEntries(): TextPropertyEntry[] {
  if (!panel) return [];
  const totalMatches = panel.searchResults.length;
  const fileCount = panel.fileGroups.length;
  const truncated = panel.truncated;
  const truncatedSuffix = truncated ? " " + editor.t("panel.limited") : "";
  let matchStats = "";
  if (totalMatches > 0) {
    matchStats = "  " + editor.t("panel.match_stats", { count: String(totalMatches), files: String(fileCount) }) + truncatedSuffix;
  } else if (panel.busy && panel.searchPattern) {
    matchStats = "  " + editor.t("panel.searching");
  } else if (panel.searchPattern && panel.searchPerformed && !panel.busy) {
    matchStats = "  " + editor.t("panel.no_matches");
  }
  if (matchStats.length === 0) return [];
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
  return [{ text: matchStats, inlineOverlays: overlays }];
}

function buildLine1Spec(): WidgetSpec {
  if (!panel) return col();
  const { searchPattern, replaceText, focusPanel, queryField, cursorPos } = panel;
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
    raw(buildMatchStatsEntries(), "matchStats"),
  );
}

// Workspace-file filter, matching the dedicated files-to-include field in
// VS Code's search panel. It stays on its own row so useful path globs aren't
// squeezed by the search/replace fields and match stats.
function buildFileGlobRowSpec(): WidgetSpec {
  if (!panel) return col();
  return row(
    spacer(1),
    textInput(panel.fileGlob, {
      label: editor.t("panel.files_label"),
      focused: false,
      cursorByte: -1,
      fieldWidth: 54,
      key: "fileGlobField",
    }),
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
// Row content is described as a sequence of styled segments rather
// than a pre-rendered string + offset overlays. The host concats
// segments and computes the byte offsets natively in Rust, so the
// plugin doesn't count codepoints or bytes for layout-piece widths
// at all. Per-row freeform overlays (e.g. pattern-match highlights
// inside the context substring) ride on the relevant segment via
// its `overlays` field, addressed in char units relative to that
// segment alone.
function renderFlatItemEntry(item: FlatItem, W: number): TextPropertyEntry {
  if (!panel) return { text: "" };
  if (item.type === "file") {
    const group = panel.fileGroups[item.fileIndex];
    const badge = getFileExtBadge(group.relPath);
    const matchCount = group.matches.length;
    const selectedInFile = group.matches.filter(m => m.selected).length;
    return styledRow(
      [
        { text: badge, style: { fg: C.fileIcon, bold: true } },
        { text: " " },
        { text: group.relPath, style: { fg: C.filePath } },
        { text: ` (${selectedInFile}/${matchCount})` },
      ],
      {
        // Host prefix at depth 0: disclosure (▶/▼) + space + checkbox
        // ([v]/[ ]) + space = 6 cols.
        padToChars: Math.max(0, W - 6),
        properties: { type: "file-row", fileIndex: item.fileIndex },
      },
    );
  }
  // Match row. The Tree widget's prefix at depth=1 is 6 cols
  // (4 indent + 2 alignment). Use the remaining width for content.
  const group = panel.fileGroups[item.fileIndex];
  const result = group.matches[item.matchIndex!];
  const location = `${group.relPath}:${result.match.line}`;
  // Hard-cap the context length BEFORE any per-codepoint work below.
  // Minified CSS / JSON / single-line generated files routinely have
  // match context strings 5 000-50 000 chars long. The downstream
  // `truncate()` does `for (const c of s)` (per-codepoint iteration
  // + O(N²) string concatenation in QuickJS); at 5 000 chars and 50
  // items per flush that adds up to several hundred ms of JS work
  // per pump iteration, blocking Tab and other queued requests.
  // A panel viewport is at most a few hundred chars wide, so anything
  // past ~512 chars is invisible anyway.
  const CONTEXT_HARD_CAP = 512;
  const rawCtx = result.match.context;
  const context = (rawCtx.length > CONTEXT_HARD_CAP
    ? rawCtx.slice(0, CONTEXT_HARD_CAP)
    : rawCtx).trim();
  // Host prefix consumes:
  //   indent (depth=1) = 2
  //   leaf-alignment   = 2 (in lieu of disclosure glyph)
  //   checkbox + space = 4 ([v] + " ")
  // Total: 8 cols.
  const innerWidth = Math.max(0, W - 8);

  // Best-effort context budget: enough room for the fixed leading
  // pieces plus " - " plus the context itself. JS `.length` gives
  // UTF-16 code-unit counts which match codepoint counts for the
  // overwhelmingly-ASCII case (paths + line numbers); slight
  // over-counting on rare non-BMP filenames just trims a little
  // more of the context, which is fine.
  const maxCtx = innerWidth - location.length - 3;
  const displayCtx = truncate(context, Math.max(10, maxCtx));

  // Pattern-match highlights inside the context substring. Emitted
  // in segment-local char units; the host shifts them by the
  // context segment's char start during entry concatenation.
  const ctxOverlays: InlineOverlay[] = [];
  if (panel.searchPattern) {
    highlightMatches(displayCtx, panel.searchPattern, panel.useRegex, panel.caseSensitive, ctxOverlays);
  }

  const segments: StyledSegment[] = [
    { text: location, style: { fg: C.lineNum } },
    { text: " - " },
    { text: displayCtx, overlays: ctxOverlays },
  ];

  return styledRow(segments, {
    padToChars: innerWidth,
    properties: { type: "match-row", fileIndex: item.fileIndex, matchIndex: item.matchIndex },
  });
}

// Convert a slice of `FlatItem`s into the corresponding TreeNodes.
// Pulled out of `buildMatchListSpec` so the streaming path can use it
// to build deltas for `appendTreeNodes` — it must produce nodes
// identical to the full-spec rebuild for the same items, so
// auto-expand of first-seen file rows happens here.
function flatItemsToTreeNodes(
  flatItems: FlatItem[],
  itemKeys: string[],
  W: number,
): TreeNode[] {
  return flatItems.map((item, i) => {
    const entry = renderFlatItemEntry(item, W);
    if (item.type === "file") {
      const k = itemKeys[i];
      if (!panel!.knownFileKeys.has(k)) {
        panel!.knownFileKeys.add(k);
        panel!.expandedFileKeys.add(k);
      }
      // File-row checkbox derives from children: checked iff every
      // match in this file is selected.
      const fileChecked = panel!.fileGroups[item.fileIndex].matches.every(m => m.selected);
      return treeNode(entry, { depth: 0, hasChildren: true, checked: fileChecked });
    }
    const matchSelected = panel!.fileGroups[item.fileIndex]
      .matches[item.matchIndex!].selected;
    return treeNode(entry, { depth: 1, hasChildren: false, checked: matchSelected });
  });
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

  // Empty-state branches: pristine / searching / no-results /
  // pattern-set-but-no-search-yet. See §17 of
  // docs/internal/search-replace-scope-replan-on-widgets.md.
  //
  // When the pattern is mutated while a previous search's results are
  // still in panel.searchResults, render the stale results (fall
  // through to the Tree branch below) until the next search
  // completes — dropping back to "Type a search pattern above"
  // mid-edit feels jumpy.
  const emptyState = (key: string) =>
    raw([{
      text: padStr("  " + editor.t(key), W),
      properties: { type: "empty" },
      style: { fg: C.dim },
    }]);
  if (!panel.searchPattern) {
    return emptyState("panel.type_pattern");
  }
  if (panel.busy && totalMatches === 0) {
    return emptyState("panel.searching");
  }
  if (totalMatches === 0 && panel.searchPerformed && !panel.busy) {
    return emptyState("panel.no_matches");
  }
  if (totalMatches === 0) {
    // Pattern in flight but no search has run yet (and no cached
    // results). Same friendly hint as pristine.
    return emptyState("panel.type_pattern");
  }

  const flatItems = buildFlatItems();
  const itemKeys = flatItems.map(flatItemKey);
  const nodes = flatItemsToTreeNodes(flatItems, itemKeys, W);
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
    checkable: true,
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

// Append pattern-match highlight overlays (one per occurrence) to
// `overlays`. Offsets are in char (codepoint) units within `text`
// itself — the caller is expected to attach `overlays` to a
// segment whose body equals `text`, so the host shifts them into
// entry-coordinate space during segment resolution.
//
// `text` and `pattern` are treated as JS UTF-16 strings. For BMP
// content (which includes nearly all source code) UTF-16 code unit
// indices and Unicode codepoint indices coincide, so `indexOf` /
// `RegExp.exec` indices map directly to char offsets without a
// per-overlay codepoint walk.
function highlightMatches(text: string, pattern: string, isRegex: boolean, caseSensitive: boolean, overlays: InlineOverlay[]): void {
  if (!pattern) return;
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
        overlays.push({ start: idx, end: idx + pattern.length, style: { bg: C.matchBg, fg: C.matchFg }, unit: "char" });
        pos = idx + pattern.length;
      }
    } else {
      const flags = caseSensitive ? "g" : "gi";
      const re = new RegExp(pattern, flags);
      let m;
      while ((m = re.exec(text)) !== null) {
        if (m[0].length === 0) { re.lastIndex++; continue; }
        overlays.push({ start: m.index, end: m.index + m[0].length, style: { bg: C.matchBg, fg: C.matchFg }, unit: "char" });
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
  //   * `HintBar{ ... }`                  — keyboard-hint row, sits
  //                                       directly above the matches
  //                                       section.
  //   * `Raw{ separator entry }`         — matches divider.
  //   * `List{ ... }` or `Raw{empty msg}` — virtual-scrolled match
  //                                       rows (host owns scroll +
  //                                       selection styling +
  //                                       click routing). Last child
  //                                       so the match list is the
  //                                       final thing in the buffer.
  if (!panel.widgetPanel) {
    panel.widgetPanel = new WidgetPanel(panel.resultsBufferId);
  }
  panel.widgetPanel.set(
    col(
      buildLine1Spec(),
      buildFileGlobRowSpec(),
      buildOptionsRowSpec(),
      buildScopeRowSpec(),
      hintBar(buildHelpHints()),
      raw(buildPanelEntries("postOptions"), "separator"),
      buildMatchListSpec(),
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
/** The active search handle, kept so a superseding search can cancel it. */
let activeSearchHandle: SearchHandle | null = null;
/** Pump cadence between successive `take()` drains (ms). The host writes
 * matches at full speed; this knob bounds the UI rebuild rate. */
const SEARCH_PUMP_INTERVAL_MS = 50;
/** Number of `buildFlatItems()` entries the streaming path has already
 *  pushed to the host via `appendTreeNodes`. Zero means "no streaming
 *  append has happened for the current search"; the first batch of
 *  results will do a full `updatePanelContent()` instead so the Tree
 *  exists for subsequent appends. Reset at the start of each search
 *  and after `batch.done` (which forces a full re-emit). */
let lastStreamingFlatCount = 0;

/** Absolute-path → index-into-`panel.fileGroups`, maintained while a
 *  search is streaming so each new match locates its file group in
 *  O(1) instead of triggering a full `buildFileGroups(allResults)`
 *  rebuild (which is O(N) per batch and pins the JS event loop on
 *  large result sets). Cleared at the start of each search. */
let streamingFileIndexByPath: Map<string, number> | null = null;

/** Carryover queue: matches the host handed us in a `take()` but that
 *  we haven't processed yet because the batch was too big to drain in
 *  a single pump iteration. Drained CHUNK at a time inside the pump
 *  loop; `take()` is only re-called once this is empty so we don't
 *  flood the queue.  Reset at the start of each search. */
let pendingMatches: GrepMatch[] = [];

/** Pending tree-append delta that hasn't been flushed to the host yet.
 *  Each pump chunk pushes its `FlatItem[]` here; the loop coalesces
 *  several chunks worth before firing one `appendTreeNodes` IPC, so
 *  the host's main thread isn't pinned servicing ~20 ms IPCs back to
 *  back during a long streaming search. */
let pendingTreeDeltaItems: FlatItem[] = [];
/** Parallel pending list of new file-row keys whose expansion state
 *  must be pushed to the host on the next flush. */
let pendingNewExpandedKeys: string[] = [];
/** Wall-clock ms of the last UI flush (the last appendTreeNodes IPC).
 *  Compared against UI_FLUSH_INTERVAL_MS to decide when to flush. */
let lastUiFlush = 0;
/** Don't flush more often than this. */
const UI_FLUSH_INTERVAL_MS = 80;
/** Hard cap on each `appendTreeNodes` flush payload. Each TreeNode in
 *  the payload costs ~60 µs in `js_to_json` + `serde_json::from_value`
 *  on the JS thread (measured: AppendTreeNodes(1296) = 88 ms).
 *  Larger payloads → longer per-iteration JS block → user input
 *  (Tab, typed char, Esc) waits in the plugin thread's request
 *  channel. Keeping the cap at ~100 keeps each flush ≤ 10 ms so
 *  queued Tab requests can interleave between pump iterations. */
const UI_FLUSH_MAX_DELTA = 100;

/**
 * Perform a streaming search using a pull-based handle. The host writes
 * matches at full speed into shared state; this loop drains them via
 * `handle.take()` and rebuilds the UI between drains. There are no
 * per-chunk callbacks crossing the FFI boundary, so the host's main
 * thread is free to process input and render between pumps.
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
  // New search → reset the streaming-append checkpoint. The first
  // batch of results will trigger a full `updatePanelContent()`
  // (mounting the empty Tree); subsequent batches append deltas to
  // that mounted Tree.
  lastStreamingFlatCount = 0;
  streamingFileIndexByPath = new Map();
  pendingMatches = [];
  pendingTreeDeltaItems = [];
  pendingNewExpandedKeys = [];
  lastUiFlush = 0;
  // Reset accumulating state so a re-search (debounce from typing,
  // toggle flip, scope change) starts from empty rather than
  // appending to the previous run's results. Drop the previous run's
  // anchor markers first so they don't leak on the source buffers.
  clearMatchMarkers();
  panel.searchResults = [];
  panel.fileGroups = [];

  // Cancel any in-flight search before kicking off a new one. Without
  // this the prior search would keep walking the project until it
  // hit max_results, wasting CPU.
  if (activeSearchHandle) {
    try { activeSearchHandle.cancel(); } catch (_e) { /* ignore */ }
    activeSearchHandle = null;
  }

  try {
    const fixedString = !panel.useRegex;
    const allResults: SearchResult[] = [];

    // Whole-word filtering is done Rust-side so maxResults is respected correctly
    const handle = editor.beginSearch(pattern, {
      fixedString,
      caseSensitive: panel.caseSensitive,
      maxResults: MAX_RESULTS,
      wholeWords: panel.wholeWords,
      fileGlob: panel.fileGlob,
      // Lets the host search an unnamed/unsaved source buffer in-memory;
      // it has no on-disk file the project walk could otherwise reach.
      sourceBufferId: panel.sourceBufferId,
    });
    activeSearchHandle = handle;

    let truncated = false;
    let producerError: string | null = null;

    while (true) {
      // Discard the in-flight search if a newer one started while we slept.
      if (generation !== currentSearchGeneration || !panel) {
        try { handle.cancel(); } catch (_e) { /* ignore */ }
        return allResults;
      }

      // Drain matches in chunks of at most CHUNK per pump iteration.
      // `pendingMatches` accumulates anything the host gave us that we
      // haven't processed yet. This caps the per-iteration synchronous
      // JS work at O(CHUNK) so the event loop yields back to the host
      // promptly — without this, a single batch of 3000+ matches takes
      // ~700ms of JS time and queues every user keypress (Tab, typed
      // chars, Esc) for the duration of the search.
      //
      // Only call `handle.take()` when our queue is empty, so the host
      // doesn't keep us flooded; the producer pauses when the take()
      // returns nothing left to drain.
      let batchDone = false;
      let batchTruncated = false;
      let batchError: string | null = null;
      if (pendingMatches.length === 0) {
        const batch = handle.take();
        batchDone = batch.done;
        batchTruncated = batch.truncated;
        batchError = batch.error ?? null;
        for (const m of batch.matches) pendingMatches.push(m);
      }
      // Hard cap on per-iteration work. Each match in the chunk turns
      // into a TreeNode in the `appendTreeNodes` flush, and each
      // TreeNode costs ~60 µs in `js_to_json` + `from_value` on the
      // JS thread. Keeping the chunk small means each pump iteration
      // stays ≤ ~10 ms — short enough that queued Tab/typed-char
      // requests interleave smoothly between iterations.
      const CHUNK = 80;
      const chunkSize = Math.min(CHUNK, pendingMatches.length);
      const chunk = pendingMatches.splice(0, chunkSize);
      const moreInQueue = pendingMatches.length > 0;
      const deltaItems: FlatItem[] = [];
      const newExpandedKeys: string[] = []; // file rows added this batch
      for (const m of chunk) {
        // §1 scope filter: when scope is "current file only", drop
        // matches from any other source. Done client-side because the
        // host grep API is project-wide. A named buffer matches by path;
        // an unnamed/unsaved buffer (empty sourceBufferPath) matches by
        // buffer id instead — the host tags its in-memory matches with it.
        if (!panel.allFiles) {
          const sameFile = !!panel.sourceBufferPath && m.file === panel.sourceBufferPath;
          const sameBuffer = !!panel.sourceBufferId && m.bufferId === panel.sourceBufferId;
          if (!sameFile && !sameBuffer) continue;
        }
        const result: SearchResult = { match: m, selected: true };
        // Anchor the match to its buffer if the file is already open, so
        // edits made while stepping keep it in sync (#2583). Files opened
        // later are handled by the after_file_open hook.
        ensureMatchMarker(result);
        allResults.push(result);
        let fileIdx = streamingFileIndexByPath?.get(m.file);
        if (fileIdx === undefined) {
          fileIdx = panel.fileGroups.length;
          streamingFileIndexByPath?.set(m.file, fileIdx);
          panel.fileGroups.push({
            relPath: getRelativePath(m.file),
            absPath: m.file,
            expanded: true,
            matches: [],
          });
          deltaItems.push({ type: "file", fileIndex: fileIdx });
          const fileKey = `file:${fileIdx}`;
          panel.expandedFileKeys.add(fileKey);
          panel.knownFileKeys.add(fileKey);
          newExpandedKeys.push(fileKey);
        }
        const matchIdx = panel.fileGroups[fileIdx].matches.length;
        panel.fileGroups[fileIdx].matches.push(result);
        deltaItems.push({ type: "match", fileIndex: fileIdx, matchIndex: matchIdx });
      }
      panel.searchResults = allResults;
      // Coalesce the per-chunk delta into a pending buffer. Each
      // `appendTreeNodes` IPC costs ~20 ms on the host (spec mutation
      // + Tree-visible-rows recompute + virtual-buffer repaint).
      // Flushing every 250-match chunk means 20+ IPCs over a 5 000-
      // match search — that pile of host main-thread work is exactly
      // when queued Tab / typed-key events sit waiting. Flush only
      // every UI_FLUSH_INTERVAL_MS so the user sees the result list
      // grow at a steady ~5 Hz while leaving the host free to dispatch
      // input events between flushes.
      for (const it of deltaItems) pendingTreeDeltaItems.push(it);
      for (const k of newExpandedKeys) pendingNewExpandedKeys.push(k);
      const nowMs = Date.now();
      const producerFinished = batchDone && pendingMatches.length === 0;
      const dueToFlush =
        producerFinished ||
        pendingTreeDeltaItems.length >= UI_FLUSH_MAX_DELTA ||
        nowMs - lastUiFlush >= UI_FLUSH_INTERVAL_MS;
      if (
        dueToFlush &&
        pendingTreeDeltaItems.length > 0 &&
        panel.widgetPanel &&
        lastStreamingFlatCount > 0
      ) {
        const W = Math.max(MIN_WIDTH, panel.viewportWidth - 2);
        const flushed = pendingTreeDeltaItems;
        const flushedNewExp = pendingNewExpandedKeys;
        pendingTreeDeltaItems = [];
        pendingNewExpandedKeys = [];
        const newItemKeys = flushed.map(flatItemKey);
        const newNodes = flatItemsToTreeNodes(flushed, newItemKeys, W);
        panel.widgetPanel.appendTreeNodes("matchTree", newNodes, newItemKeys);
        lastStreamingFlatCount += flushed.length;
        lastUiFlush = nowMs;
        if (flushedNewExp.length > 0) {
          panel.widgetPanel.setExpandedKeys(
            "matchTree",
            [...panel.expandedFileKeys],
          );
        }
      } else if (lastStreamingFlatCount === 0 && panel.fileGroups.length > 0) {
        // First time we have any results — mount the Tree via a full
        // panel update. Subsequent batches use the cheap append path.
        // Also drain the pending buffer into the spec since
        // updatePanelContent rebuilds from `panel.fileGroups` directly.
        pendingTreeDeltaItems = [];
        pendingNewExpandedKeys = [];
        updatePanelContent();
        lastStreamingFlatCount = panel.fileGroups.length + panel.searchResults.length;
        lastUiFlush = nowMs;
      }
      if (producerFinished) {
        // Streaming finished. The tree is already current in the host
        // via the per-batch `appendTreeNodes` mutations — its nodes
        // don't need refreshing. The only state that drifted is the
        // small chrome strings: the matchStats label next to the
        // input fields, and the "Matches (N in M files)" header in
        // the separator. Update them in place via `setRawEntries`
        // (a few-hundred-byte mutation) instead of re-emitting the
        // full panel spec — the latter would force `js_to_json` over
        // every TreeNode (~447 bytes × 5 000 nodes = 2.2 MB) and
        // block the JS thread for ~1 second, exactly when user input
        // piles up unread in the request channel. See the
        // RESOLVE_CB_DONE dur_us=1095122 case in the perf trace.
        if (panel.widgetPanel) {
          if (panel.fileGroups.length === 0) {
            // Special case: 0 matches. The matches body is an
            // empty-state `raw()`, not a `tree()` — we have to swap
            // widget kinds, which `setRawEntries` alone can't do.
            // The full re-emit here is cheap because the tree is
            // empty (no per-node serialization cost).
            updatePanelContent();
          } else {
            panel.widgetPanel.setRawEntries("matchStats", buildMatchStatsEntries());
            panel.widgetPanel.setRawEntries("separator", buildPanelEntries("postOptions"));
          }
        }
        lastStreamingFlatCount = 0;
        pendingTreeDeltaItems = [];
        pendingNewExpandedKeys = [];
      }
      // Also refresh the matchStats label on every streaming flush so
      // the count updates in real time as results stream in.
      if (!producerFinished && dueToFlush && panel.widgetPanel) {
        panel.widgetPanel.setRawEntries("matchStats", buildMatchStatsEntries());
        panel.widgetPanel.setRawEntries("separator", buildPanelEntries("postOptions"));
      }

      if (producerFinished) {
        truncated = batchTruncated;
        producerError = batchError;
        break;
      }

      // Yield to the JS event loop between chunks. `delay(0)` is
      // enough — it lets queued plugin handlers (Tab, typed input,
      // Esc) run between our streaming work. When there's no
      // carryover, wait the usual pump interval so we don't hot-loop
      // on `handle.take()`.
      const yieldMs = moreInQueue ? 0 : SEARCH_PUMP_INTERVAL_MS;
      await editor.delay(yieldMs);
    }

    if (activeSearchHandle === handle) {
      activeSearchHandle = null;
    }

    // Final state
    if (generation !== currentSearchGeneration || !panel) return allResults;

    if (producerError) {
      throw new Error(producerError);
    }

    panel.truncated = truncated;

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

async function openPanel(opts?: { allFiles?: boolean }): Promise<void> {
  // Try to pre-fill search from editor selection
  let prefill = "";
  let sourceBufferPath = "";
  let sourceBufferId = 0;
  try {
    const activeId = editor.getActiveBufferId();
    sourceBufferId = activeId;
    sourceBufferPath = editor.getBufferPath(activeId) || "";
    const cursor = editor.getPrimaryCursor();
    if (cursor && cursor.selection) {
      const start = Math.min(cursor.selection.start, cursor.selection.end);
      const end = Math.max(cursor.selection.start, cursor.selection.end);
      if (end - start > 0 && end - start < 200) {
        const text = await editor.getBufferText(activeId, start, end);
        if (text && !text.includes("\n")) {
          prefill = text;
        }
      }
    }
  } catch (_e) { /* no selection / no buffer */ }

  const allFiles = opts?.allFiles ?? true;
  const sourceBufferRelPath = sourceBufferPath ? getRelativePath(sourceBufferPath) : "";

  if (panel) {
    panel.focusPanel = "query";
    panel.queryField = "search";
    if (prefill) panel.searchPattern = prefill;
    panel.cursorPos = panel.searchPattern.length;
    // Re-opening from a different file/scope refreshes scope context.
    panel.allFiles = allFiles;
    panel.sourceBufferPath = sourceBufferPath;
    panel.sourceBufferRelPath = sourceBufferRelPath;
    panel.sourceBufferId = sourceBufferId;
    updatePanelContent();
    if (panel.searchPattern) rerunSearchDebounced();
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
    fileGlob: "",
    focusPanel: "query",
    queryField: "search",
    optionIndex: 0,
    matchIndex: 0,
    caseSensitive: false,
    useRegex: false,
    wholeWords: false,
    allFiles,
    sourceBufferPath,
    sourceBufferRelPath,
    sourceBufferId,
    viewportWidth: DEFAULT_WIDTH,
    busy: false,
    searchPerformed: false,
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
      // The panel manages its own scrolling (the match Tree owns its
      // scroll window), so the buffer must not be user-scrollable —
      // otherwise a scrollbar appears and dragging it scrolls the
      // search inputs off-screen, revealing empty space below (#2434).
      scrollable: false,
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

  // Group edits per target. Matches in an open buffer carry a non-zero
  // bufferId; key by it so an unnamed buffer (whose `file` is a shared
  // "[No Name]" label) still resolves to the right buffer rather than
  // colliding with another unnamed buffer's matches. On-disk files
  // (bufferId 0) key by path and are opened/saved by the host as before.
  type Group = { filePath: string; bufferId: number; matches: Array<[number, number]> };
  const groups: Map<string, Group> = new Map();
  for (const result of toReplace) {
    const bufferId = result.match.bufferId || 0;
    const key = bufferId > 0 ? `buf:${bufferId}` : result.match.file;
    let group = groups.get(key);
    if (!group) {
      group = { filePath: result.match.file, bufferId, matches: [] };
      groups.set(key, group);
    }
    group.matches.push([result.match.byteOffset, result.match.length]);
  }

  let filesModified = 0;
  let replacementsCount = 0;
  const errors: string[] = [];

  const groupList: Group[] = [];
  groups.forEach((g) => groupList.push(g));
  for (const group of groupList) {
    try {
      const result = await editor.replaceInFile(
        group.filePath,
        group.matches,
        panel.replaceText,
        group.bufferId
      );
      replacementsCount += result.replacements;
      if (result.replacements > 0) filesModified++;
    } catch (e) {
      errors.push(`${group.filePath}: ${e instanceof Error ? e.message : String(e)}`);
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
  // No `panel.busy` early-return: if a search is already running for
  // an older pattern (e.g. user typed "pr" then "proj"), we want the
  // newer search to start NOW, not after the old one finishes
  // walking the project. `performSearch` increments
  // `currentSearchGeneration` and cancels the prior handle; the older
  // in-flight `performSearch` sees the gen mismatch on its next
  // pump tick and bails out without writing to panel state.
  searchDebounceGeneration++;
  // Capture the generation this rerunSearch will own once performSearch
  // increments it. If a newer rerunSearch slots in while we're awaiting,
  // currentSearchGeneration moves past `myGen` and we know not to
  // finalize busy/searchPerformed for this stale invocation.
  const myGen = currentSearchGeneration + 1;
  panel.truncated = false;
  panel.busy = true;
  panel.matchIndex = 0;
  panel.scrollOffset = 0;
  await performSearch(panel.searchPattern);
  // performSearch maintains panel.searchResults / panel.fileGroups
  // incrementally during streaming and pushes matchStats / separator
  // updates via cheap targeted mutations on batch.done — no full
  // spec re-emit needed here. Only finalize busy + searchPerformed
  // if we are still the latest search; the busy flip drives the
  // "No matches" empty-state branch in `buildMatchListSpec`, so
  // when totalMatches===0 we also need to refresh the matches Raw
  // (a tiny mutation) so the user sees the empty-state label.
  if (panel && currentSearchGeneration === myGen) {
    panel.busy = false;
    panel.searchPerformed = true;
    if (panel.widgetPanel) {
      if (panel.searchResults.length === 0) {
        // Zero matches: the match-list body is an empty-state `raw()`
        // last rendered while `busy` was still true → "Searching…".
        // `setRawEntries("matchStats", …)` only refreshes the small
        // count label next to the inputs, not the body, so without a
        // full re-emit the body stays stuck on "Searching…". Re-emit
        // now that the flags are final so it shows "No matches".
        // Cheap: the tree is empty (no per-node serialization cost).
        updatePanelContent();
      } else {
        // The tree was built incrementally in file-arrival order while
        // streaming. Now that the search is finished, re-order the file
        // groups into natural path order and re-emit once so the
        // reviewer sees a stable, sorted list (#2435). This is the only
        // full re-emit of the run — it happens after streaming has
        // stopped and the user is about to start reviewing, so it
        // doesn't compete with input the way per-batch re-emits would.
        sortFileGroups();
        updatePanelContent();
      }
    }
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
  searchDebounceGeneration++;
  panel.busy = true;
  const results = await performSearch(panel.searchPattern, true);
  if (panel) {
    panel.searchResults = results;
    panel.fileGroups = buildFileGroups(results);
    panel.matchIndex = 0;
    panel.scrollOffset = 0;
    panel.busy = false;
    panel.searchPerformed = true;
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
/** Apply a stored history entry to the search field. Mutates the
 *  widget's value via setValue so the host instance state stays in
 *  sync with the plugin's panel.searchPattern, and triggers a
 *  debounced re-search. See §11. */
function applyHistoryEntry(text: string): void {
  if (!panel || !panel.widgetPanel) return;
  panel.searchPattern = text;
  panel.cursorPos = text.length;
  panel.searchPerformed = false;
  panel.widgetPanel.setValue("searchField", text, byteLen(text));
  rerunSearchDebounced();
}

/** Whether Up/Down should be intercepted for history walk (instead of
 *  being passed to the focused widget). True only when the most recent
 *  widget_event indicated focus was on the search field. */
function shouldInterceptForHistory(): boolean {
  return lastFocusedWidget === "searchField" || lastFocusedWidget === null;
}

registerHandler("search_replace_nav_up", () => {
  if (!panel) return;
  if (shouldInterceptForHistory()) {
    if (searchHistory.length === 0) return;
    if (historyIndex < 0) {
      // Entering history walk — snapshot what the user had typed so
      // a Down past the most recent entry restores it.
      historySavedPattern = panel.searchPattern;
      historyIndex = 0;
    } else if (historyIndex < searchHistory.length - 1) {
      historyIndex += 1;
    } else {
      return; // already at the oldest entry
    }
    applyHistoryEntry(searchHistory[historyIndex]);
    return;
  }
  dispatch(widgetKey("Up"));
});
registerHandler("search_replace_nav_down", () => {
  if (!panel) return;
  if (shouldInterceptForHistory() && historyIndex >= 0) {
    if (historyIndex > 0) {
      historyIndex -= 1;
      applyHistoryEntry(searchHistory[historyIndex]);
      return;
    }
    // Down past the most recent entry → exit history walk and restore
    // whatever the user had typed before they hit Up.
    historyIndex = -1;
    const restore = historySavedPattern ?? "";
    historySavedPattern = null;
    applyHistoryEntry(restore);
    return;
  }
  dispatch(widgetKey("Down"));
});
registerHandler("search_replace_nav_page_up",   () => dispatch(widgetKey("PageUp")));
registerHandler("search_replace_nav_page_down", () => dispatch(widgetKey("PageDown")));

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

/** Lock against re-entrant Replace All / Replace Scoped. Set as soon
 *  as doReplaceAll/doReplaceScoped enters and cleared in a try/finally
 *  around the whole flow. Without this, a user mashing Alt+Enter
 *  during a streaming search produces N stacked confirmation prompts
 *  once the search finishes — the host queues each keystroke and
 *  drains them all when the JS event loop frees up; by then
 *  `panel.busy` is false so the busy guard doesn't fire. */
let replaceInProgress = false;

async function doReplaceAll(): Promise<void> {
  if (!panel) return;
  if (panel.busy) {
    // Search is still streaming. Don't block silently — tell the user
    // to wait, and don't queue the replace. (The host's event
    // dispatcher would otherwise hold the keystroke and run it when
    // the pump finishes, which feels like an unexplained delay.)
    editor.setStatus(editor.t("status.replace_wait_for_search"));
    return;
  }
  if (replaceInProgress) {
    // First Alt+Enter is already showing its prompt or running the
    // rewrites. Drop the duplicate so we don't stack prompts.
    return;
  }
  replaceInProgress = true;
  try {
    await doReplaceAllInner();
  } finally {
    replaceInProgress = false;
  }
}

async function doReplaceAllInner(): Promise<void> {
  if (!panel) return;
  const selected = panel.searchResults.filter(r => r.selected);
  if (selected.length === 0) {
    editor.setStatus(editor.t("status.no_items_selected"));
    return;
  }
  // The user committed to this pattern by triggering Replace All —
  // a clear "settle" signal, so commit it to history now even if the
  // 2s scheduleHistoryPush hasn't fired yet.
  if (historyIndex < 0) historyPush(panel.searchPattern);
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
  clearMatchMarkers();
  panel.searchResults = [];
  panel.fileGroups = [];
  panel.busy = false;
  await rerunSearchQuiet();
  updatePanelContent();
}

async function doReplaceScoped(): Promise<void> {
  if (!panel || panel.focusPanel !== "matches") return;
  if (panel.busy) {
    editor.setStatus(editor.t("status.replace_wait_for_search"));
    return;
  }
  if (replaceInProgress) return;
  replaceInProgress = true;
  try {
    await doReplaceScopedInner();
  } finally {
    replaceInProgress = false;
  }
}

async function doReplaceScopedInner(): Promise<void> {
  if (!panel) return;
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
  // Same as doReplaceAll: explicit commit, push immediately.
  if (historyIndex < 0) historyPush(panel.searchPattern);

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
  clearMatchMarkers();
  panel.searchResults = [];
  panel.fileGroups = [];
  panel.busy = false;
  await rerunSearchQuiet();
  updatePanelContent();
}

// =============================================================================
// Match navigation (issue #2434)
// =============================================================================
//
// Reviewing a large result set match-by-match used to cost three
// keystrokes each: Down (select the next row in the panel), Enter
// (open it in the editor — which moves focus to the source split), and
// Alt+] to return to the panel before repeating. These two commands
// collapse that to a single keystroke usable *from the editor*: advance
// the panel's selection to the next/previous match row, open it in the
// source split (cursor lands on the match, ready to edit), and leave
// focus where it is. Press again to move to the next one. Mirrors the
// next/prev idiom in diff_nav.ts.

/** 1-based ordinal of the match row at flat index `flatIdx` among all
 *  match rows (file-header rows don't count). Used for "Match n/total"
 *  status feedback. */
function matchOrdinal(flat: FlatItem[], flatIdx: number): number {
  let count = 0;
  for (let i = 0; i <= flatIdx && i < flat.length; i++) {
    if (flat[i].type === "match") count++;
  }
  return count;
}

/** Flat index of the next (dir=1) / previous (dir=-1) match row
 *  relative to `from`, skipping file-header rows and wrapping around.
 *  Returns -1 when there are no match rows at all. */
function adjacentMatchIndex(flat: FlatItem[], from: number, dir: 1 | -1): number {
  const n = flat.length;
  if (n === 0) return -1;
  // Normalize the start so the first step lands on a real neighbour
  // even when `from` is out of range or sits on a file header.
  let start = from;
  if (start < 0 || start >= n) start = dir === 1 ? -1 : n;
  for (let step = 1; step <= n; step++) {
    const idx = (((start + dir * step) % n) + n) % n;
    if (flat[idx].type === "match") return idx;
  }
  return -1;
}

// =============================================================================
// Anchor match positions to buffer edits with interval markers (issue #2583)
//
// A match's line/column/byteOffset are captured once, at grep time. The
// "step through matches and edit each" workflow (#2434) edits the buffer
// between steps, so without anchoring the cursor lands on stale pre-edit
// positions. Rather than re-deriving edit deltas in the plugin — which
// can't even see bulk edits: paste, multi-cursor, type-over-selection,
// query-replace and LSP code-action rewrites apply as a single BulkEdit
// that bypasses the after_insert/after_delete hooks — we register an
// interval marker at each match's byte range in its open buffer. The editor
// keeps the marker shifted across *every* edit kind (the CoordMap mechanism
// that markdown_compose and #2602's diagnostics also ride). At jump time we
// read the marker's live start byte and convert it back to (line, column).
//
// Matches in files that aren't open have no buffer to anchor to, but they
// also can't be edited, so their grep positions stay valid as a fallback.
// =============================================================================

let nextMatchMarkerId = 0;

/** Register an interval marker for `result` when its file is open and it has
 *  no marker yet. The marker spans the match's byte range; the editor keeps
 *  it shifted. No-op for closed files (no buffer to anchor to). */
function ensureMatchMarker(result: SearchResult): void {
  if (result.markerKey) return;
  const bid = result.match.bufferId || editor.findBufferByPath(result.match.file);
  if (!bid || bid <= 0) return;
  const start = result.match.byteOffset;
  const key = `sr:${nextMatchMarkerId++}`;
  if (editor.createMarker(bid, key, start, start + result.match.length, null)) {
    result.markerKey = key;
    result.markerBufferId = bid;
  }
}

/** Register markers for every match in a file's buffer — used when a file is
 *  opened after the search captured it, so later edits to it still track. */
function ensureMarkersForPath(path: string): void {
  if (!panel || !path) return;
  for (const r of panel.searchResults) {
    if (r.match.file === path) ensureMatchMarker(r);
  }
}

/** Delete every marker we created and forget the keys. Called whenever the
 *  result set is torn down (re-search, panel close) so markers never leak.
 *  (The editor also drops a buffer's markers when it closes.) */
function clearMatchMarkers(): void {
  if (!panel) return;
  for (const r of panel.searchResults) {
    if (r.markerKey && r.markerBufferId) {
      editor.deleteMarker(r.markerBufferId, r.markerKey);
    }
    r.markerKey = undefined;
    r.markerBufferId = undefined;
  }
}

/** Convert a byte offset in `bufferId` to a 1-indexed (line, column), where
 *  column is a 1-indexed *byte* column — matching how grep produces positions
 *  (`column = byteOffset - lineStart + 1`), so it round-trips through
 *  `openFileInSplit` unchanged, including for multibyte content. */
async function byteOffsetToLineColumn(
  bufferId: number,
  offset: number,
): Promise<{ line: number; column: number }> {
  const prefix = await editor.getBufferText(bufferId, 0, offset);
  let line = 1;
  let lastNewline = -1;
  for (let i = 0; i < prefix.length; i++) {
    if (prefix.charCodeAt(i) === 10) {
      line++;
      lastNewline = i;
    }
  }
  const column = byteLen(prefix.slice(lastNewline + 1)) + 1;
  return { line, column };
}

/** Resolve a match to its *current* (line, column): the live marker position
 *  when the buffer is open, else the grep position. Writes the resolved value
 *  back onto the match so panel rows show the shifted line number on the next
 *  render too. */
async function resolveMatchTarget(
  result: SearchResult,
): Promise<{ line: number; column: number }> {
  if (result.markerKey && result.markerBufferId) {
    const marker = editor.getMarker(result.markerBufferId, result.markerKey) as
      | { start: number }
      | null;
    if (marker && typeof marker.start === "number") {
      const pos = await byteOffsetToLineColumn(result.markerBufferId, marker.start);
      result.match.line = pos.line;
      result.match.column = pos.column;
      return pos;
    }
  }
  return { line: result.match.line, column: result.match.column };
}

/** Open a match in the source split at its current (edit-tracked) position. */
async function openResultLocation(result: SearchResult): Promise<void> {
  if (!panel) return;
  const { line, column } = await resolveMatchTarget(result);
  editor.openFileInSplit(panel.sourceSplitId, result.match.file, line, column);
}

/** Open the match at flat index `flatIdx` in the source split and sync
 *  the panel's selection/expansion so the row is highlighted and
 *  scrolled into view. `openFileInSplit` focuses the source split, so
 *  the user can edit the match immediately and press the nav key again
 *  to move on — no detour back through the panel. */
async function jumpToMatch(flat: FlatItem[], flatIdx: number): Promise<void> {
  if (!panel) return;
  const item = flat[flatIdx];
  if (!item || item.type !== "match") return;
  panel.matchIndex = flatIdx;
  // Keep the selection meaningful for a subsequent full re-render and
  // for Shift+Enter (replace-scoped), which both key off focusPanel.
  panel.focusPanel = "matches";
  // Ensure the file group is expanded so the selected row is visible.
  const fileKey = `file:${item.fileIndex}`;
  panel.expandedFileKeys.add(fileKey);
  if (panel.widgetPanel) {
    panel.widgetPanel.setExpandedKeys("matchTree", [...panel.expandedFileKeys]);
    panel.widgetPanel.setSelectedIndex("matchTree", flatIdx);
  }
  // Opening a result is a clear "this is the search I wanted" signal —
  // commit the pattern to history (matches the Enter/activate path).
  if (historyIndex < 0) historyPush(panel.searchPattern);
  const group = panel.fileGroups[item.fileIndex];
  const result = group.matches[item.matchIndex!];
  await openResultLocation(result);
}

function navigateMatch(dir: 1 | -1): void {
  if (!panel || panel.searchResults.length === 0) {
    editor.setStatus(
      editor.t(panel?.busy ? "panel.searching" : "status.no_active_search"),
    );
    return;
  }
  const flat = buildFlatItems();
  const idx = adjacentMatchIndex(flat, panel.matchIndex, dir);
  if (idx < 0) {
    editor.setStatus(editor.t("status.no_active_search"));
    return;
  }
  // The panel selection updates synchronously (inside jumpToMatch, before its
  // first await); the source-split cursor lands a tick later once the marker
  // position is resolved. Status reflects the target immediately either way.
  jumpToMatch(flat, idx).catch((e) => editor.error(`search-replace: ${e}`));
  editor.setStatus(editor.t("status.match_position", {
    n: String(matchOrdinal(flat, idx)),
    total: String(panel.searchResults.length),
  }));
}

function search_replace_next_match(): void {
  navigateMatch(1);
}
registerHandler("search_replace_next_match", search_replace_next_match);

function search_replace_prev_match(): void {
  navigateMatch(-1);
}
registerHandler("search_replace_prev_match", search_replace_prev_match);

// Act on a tree row at flat index `idx`: open a match in the source
// split, or toggle a file header's expansion. Shared by Enter/activate
// on the tree and a mouse click on a row (issue #2434 follow-up: a
// click on a result should jump to it, just like Enter).
async function activateTreeItem(idx: number): Promise<void> {
  if (!panel) return;
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
    panel.widgetPanel?.setExpandedKeys("matchTree", [...panel.expandedFileKeys]);
  } else {
    // Opening a result is a "this is the search I wanted" signal —
    // commit it to history immediately, regardless of how long the
    // pattern has been stable. See §11 follow-up.
    if (historyIndex < 0) historyPush(panel.searchPattern);
    const group = panel.fileGroups[item.fileIndex];
    const result = group.matches[item.matchIndex!];
    await openResultLocation(result);
  }
}

function search_replace_close(): void {
  if (!panel) return;
  // If the user actually ran a search to completion with this
  // pattern (results were observed) and isn't walking history,
  // treat panel-close as a settle and commit to history. The
  // searchPerformed guard avoids capturing half-typed patterns
  // that never made it past the empty-state.
  if (
    historyIndex < 0
    && panel.searchPattern
    && panel.searchPerformed
  ) {
    historyPush(panel.searchPattern);
  }
  // Drop the anchor markers we registered on the source buffers so they
  // don't linger after the panel is gone.
  clearMatchMarkers();
  const sourceSplitId = panel.sourceSplitId;
  panel.widgetPanel?.unmount();
  editor.closeBuffer(panel.resultsBufferId);
  if (panel.resultsSplitId !== panel.sourceSplitId) {
    editor.closeSplit(panel.resultsSplitId);
  }
  panel = null;
  // Restore focus to the split the user came from. Without this,
  // `getActiveBufferId()` on the next invocation can return the
  // utility dock's leftover buffer, and the §1 current-file scope
  // shows "(unsaved buffer)" instead of the real filename.
  editor.focusSplit(sourceSplitId);
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

// §1: open the panel with scope already restricted to the active
// buffer. Useful when the user wants single-file search/replace from
// the keymap without flipping the toggle by hand.
function start_search_replace_in_buffer(): void {
  openPanel({ allFiles: false });
}
registerHandler("start_search_replace_in_buffer", start_search_replace_in_buffer);

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
    // The results buffer went away → tear down. Drop our anchor markers on
    // the (still-open) source buffers first so they don't leak.
    clearMatchMarkers();
    panel.widgetPanel?.unmount();
    panel = null;
  }
});

// When a file is opened after the search captured it (e.g. the user steps
// into a result whose file wasn't open), register anchor markers for that
// file's matches so subsequent edits to it keep them in sync (#2583).
editor.on("after_file_open", (args) => {
  if (panel) ensureMarkersForPath(args.path);
  return true;
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

  // Track most-recent focused widget so Up/Down can decide whether to
  // walk search history (search field) or pass through to the widget
  // runtime (matches tree, toggles, button). The widget runtime
  // doesn't expose focus to the plugin directly; this best-effort
  // proxy is good enough for the history-walk gesture. See §11.
  if (typeof args.widget_key === "string" && args.widget_key.length > 0) {
    lastFocusedWidget = args.widget_key;
  }

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
      if (panel.searchPattern !== payload.value) {
        // Pattern mutated by the user; cached "no matches" / result
        // set no longer reflects this query. See §17.
        panel.searchPerformed = false;
        // User-driven typing exits any in-flight history walk so a
        // subsequent Up doesn't snap back to a history entry under
        // the cursor. See §11.
        historyIndex = -1;
        historySavedPattern = null;
        panel.searchPattern = payload.value;
        panel.cursorPos = byteToCharOffset(payload.value, cursorByte);
        updatePanelContent();
        rerunSearchDebounced();
        scheduleHistoryPush(payload.value);
        return;
      }
      // Cursor-only update (Left/Right arrows, Home/End, click reposition):
      // the search field's text is unchanged, so don't re-run the search
      // or perturb the history-settle timer. Just sync the plugin's
      // cached cursor position so the next render shows the cursor in
      // the right place.
      panel.cursorPos = byteToCharOffset(payload.value, cursorByte);
    } else if (args.widget_key === "replaceField") {
      panel.replaceText = payload.value;
      panel.cursorPos = byteToCharOffset(payload.value, cursorByte);
    } else if (args.widget_key === "fileGlobField") {
      if (panel.fileGlob !== payload.value) {
        panel.fileGlob = payload.value;
        panel.searchPerformed = false;
        rerunSearchDebounced();
      }
    }
    return;
  }

  // `select` — fired when the user clicks a Tree row or the host
  // moves selection (Up/Down). The host already updated the
  // tree's selectedIndex in instance state; mirror it into the
  // plugin model. A *mouse click* (payload `via: "click"`) should
  // also act on the row — open the match / toggle the file — so
  // clicking a result jumps to it in the editor, matching user
  // expectation (issue #2434 follow-up). Keyboard arrow-moves carry
  // no `via` marker and only move the highlight.
  if (args.event_type === "select") {
    const payload = args.payload as { index?: number; via?: string } | undefined;
    const idx = payload?.index;
    if (typeof idx === "number") {
      panel.matchIndex = idx;
      if (payload?.via === "click") {
        activateTreeItem(idx).catch((e) => editor.error(`search-replace: ${e}`));
      }
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
      activateTreeItem(idx).catch((e) => editor.error(`search-replace: ${e}`));
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
      case "allFiles":
        // Scope flip (§1). Push the new checked state back to the
        // widget instance and rebuild the whole spec so the scope
        // row + Replace All button label switch in lock-step. Then
        // re-run the search so the results pane reflects the new
        // scope (the search itself is project-wide; filtering
        // happens in performSearch).
        panel.allFiles = newChecked;
        panel.widgetPanel?.setChecked("allFiles", newChecked);
        updatePanelContent();
        rerunSearchDebounced();
        break;
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
      case "matchTree": {
        // The `[v]`/`[ ]` glyph on a tree row was clicked. Plugin
        // owns the source-of-truth (`result.selected`) — flip it
        // and push the new spec state via the targeted mutator.
        // For file rows we cascade to every child match so a
        // single click on the file checkbox checks/unchecks the
        // whole file's matches at once.
        const idx = (args.payload as { index?: number } | undefined)?.index;
        if (typeof idx !== "number") return;
        applyMatchTreeToggle(idx, newChecked);
        break;
      }
    }
  }
});

/// Toggle the selected state of a match-tree row at `idx` to
/// `newChecked`. For a match row, just flips that match. For a
/// file header, cascades to every child match. Updates the host's
/// view via `setCheckedKeys` (one call per row that changed
/// glyph) so the next render reflects the new state without a
/// full spec re-emit.
function applyMatchTreeToggle(idx: number, newChecked: boolean): void {
  if (!panel) return;
  const flat = buildFlatItems();
  const item = flat[idx];
  if (!item) return;
  if (item.type === "match") {
    const fileGroup = panel.fileGroups[item.fileIndex];
    fileGroup.matches[item.matchIndex!].selected = newChecked;
    const matchKey = flatItemKey(item);
    panel.widgetPanel?.setCheckedKeys("matchTree", newChecked, [matchKey]);
    // The file header's checked glyph is derived (all-or-nothing).
    // After flipping a single match, recompute and push the file
    // row's new state so it stays in sync with its children.
    const fileAllSelected = fileGroup.matches.every(m => m.selected);
    const fileKey = flatItemKey({ type: "file", fileIndex: item.fileIndex });
    panel.widgetPanel?.setCheckedKeys("matchTree", fileAllSelected, [fileKey]);
  } else {
    // File row — cascade to every child.
    const fileGroup = panel.fileGroups[item.fileIndex];
    for (const m of fileGroup.matches) m.selected = newChecked;
    const fileKey = flatItemKey(item);
    const matchKeys = fileGroup.matches.map((_, mi) =>
      flatItemKey({ type: "match", fileIndex: item.fileIndex, matchIndex: mi })
    );
    panel.widgetPanel?.setCheckedKeys(
      "matchTree",
      newChecked,
      [fileKey, ...matchKeys],
    );
  }
}

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

editor.registerCommand(
  "%cmd.search_replace_in_buffer",
  "%cmd.search_replace_in_buffer_desc",
  "start_search_replace_in_buffer",
  null
);

// Match navigation (issue #2434). Always visible — these operate on the
// last search's results and are most useful from the editor while
// reviewing matches, so no panel context is required. Default bindings
// live in keymaps/default.json (Ctrl+Alt+Right / Ctrl+Alt+Left).
editor.registerCommand(
  "%cmd.next_match",
  "%cmd.next_match_desc",
  "search_replace_next_match",
  null
);

editor.registerCommand(
  "%cmd.prev_match",
  "%cmd.prev_match_desc",
  "search_replace_prev_match",
  null
);

editor.debug("Search & Replace plugin loaded");
