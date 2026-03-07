/// <reference path="./lib/fresh.d.ts" />
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
  // Inline editing cursor position
  cursorPos: number;
  // Virtual scroll offset for matches tree
  scrollOffset: number;
}
let panel: PanelState | null = null;

const MAX_RESULTS = 200;
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
    // Take first maxLen codepoints
    let result = "";
    let count = 0;
    for (const c of s) {
      if (count >= maxLen) break;
      result += c;
      count++;
    }
    return result;
  }
  // Take first (maxLen-3) codepoints + "..."
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
  ["C-Return", "search_replace_replace_all"],
  ["S-Return", "search_replace_replace_scoped"],
  ["Escape", "search_replace_close"],
  ["Backspace", "search_replace_backspace"],
  ["Delete", "search_replace_delete"],
  ["Home", "search_replace_home"],
  ["End", "search_replace_end"],
];

editor.defineMode("search-replace-list", "normal", modeBindings, true, true);

// Single handler for all character input (any keyboard layout, including Unicode)
function insertCharAtCursor(ch: string): void {
  if (!panel || panel.focusPanel !== "query") return;
  const text = getActiveFieldText();
  const pos = panel.cursorPos;
  setActiveFieldText(text.slice(0, pos) + ch + text.slice(pos));
  panel.cursorPos = pos + ch.length;
  updatePanelContent();
}

// Handler for mode_text_input events dispatched by the mode system
function mode_text_input(args: { text: string }): void {
  if (args && args.text) {
    insertCharAtCursor(args.text);
  }
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

function buildFlatItems(): FlatItem[] {
  if (!panel) return [];
  const items: FlatItem[] = [];
  for (let fi = 0; fi < panel.fileGroups.length; fi++) {
    const group = panel.fileGroups[fi];
    items.push({ type: "file", fileIndex: fi });
    if (group.expanded) {
      for (let mi = 0; mi < group.matches.length; mi++) {
        items.push({ type: "match", fileIndex: fi, matchIndex: mi });
      }
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

function buildPanelEntries(): TextPropertyEntry[] {
  if (!panel) return [];
  const { searchPattern, replaceText, searchResults, fileGroups, focusPanel, queryField,
    optionIndex, caseSensitive, useRegex, wholeWords, cursorPos } = panel;

  const W = Math.max(MIN_WIDTH, panel.viewportWidth - 2);
  const entries: TextPropertyEntry[] = [];

  const totalMatches = searchResults.length;
  const fileCount = fileGroups.length;

  // ── Line 1: Query fields + match count ──
  const qFocusSearch = focusPanel === "query" && queryField === "search";
  const qFocusReplace = focusPanel === "query" && queryField === "replace";

  // Build search field display with cursor
  const searchVal = searchPattern || "";
  const replaceVal = replaceText || "";
  const searchCursorPos = qFocusSearch ? cursorPos : -1;
  const replaceCursorPos = qFocusReplace ? cursorPos : -1;

  const searchDisp = buildFieldDisplay(searchVal, searchCursorPos, 25);
  const replDisp = buildFieldDisplay(replaceVal, replaceCursorPos, 25);

  const searchLabel = " " + editor.t("panel.search_label") + " ";
  const replSep = "  " + editor.t("panel.replace_label") + " ";
  const matchStats = totalMatches > 0
    ? "  " + editor.t("panel.match_stats", { count: String(totalMatches), files: String(fileCount) })
    : (searchPattern ? "  " + editor.t("panel.no_matches") : "");

  const line1Text = searchLabel + searchDisp + replSep + replDisp + matchStats;
  const line1 = padStr(line1Text, W);

  const line1Overlays: InlineOverlay[] = [];
  // Search label
  line1Overlays.push({ start: byteLen(" "), end: byteLen(searchLabel), style: { fg: C.label } });
  // Search value
  const svStart = byteLen(searchLabel);
  const svEnd = svStart + byteLen(searchDisp);
  line1Overlays.push({ start: svStart, end: svEnd, style: { fg: C.value, bg: qFocusSearch ? C.inputBg : undefined } });
  // Cursor highlight in search field
  if (qFocusSearch) {
    addCursorOverlay(searchVal, searchCursorPos, svStart + byteLen("["), line1Overlays);
  }
  // Replace label
  const rlStart = svEnd;
  const rlEnd = rlStart + byteLen(replSep);
  line1Overlays.push({ start: rlStart, end: rlEnd, style: { fg: C.label } });
  // Replace value
  const rvStart = rlEnd;
  const rvEnd = rvStart + byteLen(replDisp);
  line1Overlays.push({ start: rvStart, end: rvEnd, style: { fg: C.value, bg: qFocusReplace ? C.inputBg : undefined } });
  // Cursor highlight in replace field
  if (qFocusReplace) {
    addCursorOverlay(replaceVal, replaceCursorPos, rvStart + byteLen("["), line1Overlays);
  }
  // Stats
  if (matchStats) {
    const msStart = rvEnd;
    const msEnd = msStart + byteLen(matchStats);
    line1Overlays.push({ start: msStart, end: msEnd, style: { fg: totalMatches > 0 ? C.statusOk : C.statusDim } });
  }

  entries.push({
    text: line1 + "\n",
    properties: { type: "query-line" },
    inlineOverlays: line1Overlays,
  });

  // ── Line 2: Options toggles + Replace All button ──
  const optCase = (caseSensitive ? "[v]" : "[ ]") + " " + editor.t("panel.case_toggle");
  const optRegex = (useRegex ? "[v]" : "[ ]") + " " + editor.t("panel.regex_toggle");
  const optWhole = (wholeWords ? "[v]" : "[ ]") + " " + editor.t("panel.whole_toggle");
  const replBtn = "[" + editor.t("panel.replace_all_btn") + "]";

  const line2Text = " " + optCase + "  " + optRegex + "  " + optWhole + "    " + replBtn;
  const line2 = padStr(line2Text, W);

  const line2Overlays: InlineOverlay[] = [];
  const oFocus = focusPanel === "options";

  let pos = byteLen(" ");
  function addToggleOverlay(text: string, idx: number): void {
    const isOn = text.startsWith("[v]");
    const isFoc = oFocus && optionIndex === idx;
    const checkEnd = pos + byteLen(text.substring(0, 3));
    line2Overlays.push({ start: pos, end: checkEnd, style: { fg: isOn ? C.toggleOn : C.toggleOff, bold: isFoc } });
    const labelEnd = pos + byteLen(text);
    line2Overlays.push({ start: checkEnd, end: labelEnd, style: { fg: C.label, bg: isFoc ? C.selectedBg : undefined, bold: isFoc } });
    pos = labelEnd + byteLen("  ");
  }

  addToggleOverlay(optCase, 0);
  addToggleOverlay(optRegex, 1);
  addToggleOverlay(optWhole, 2);

  // Replace All button
  pos = byteLen(line2Text) - byteLen(replBtn);
  const btnFoc = oFocus && optionIndex === 3;
  line2Overlays.push({
    start: pos, end: pos + byteLen(replBtn),
    style: { fg: btnFoc ? C.buttonFg : C.button, bg: btnFoc ? C.button : undefined, bold: btnFoc },
  });

  entries.push({
    text: line2 + "\n",
    properties: { type: "options-line" },
    inlineOverlays: line2Overlays,
  });

  // ── Separator ──
  const sepChar = "─";
  const matchesLabel = totalMatches > 0
    ? " " + editor.t("panel.matches_count", { count: String(totalMatches), files: String(fileCount) }) + " "
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

  // ── Matches tree (virtual-scrolled) ──
  // Build all tree lines first, then show only a viewport-sized slice.
  // The control bar above (query + options + separator) is always visible.
  const allTreeLines: TextPropertyEntry[] = [];
  let selectedLineIdx = -1;

  if (searchPattern && totalMatches === 0) {
    allTreeLines.push({
      text: padStr("  " + editor.t("panel.no_matches"), W) + "\n",
      properties: { type: "empty" },
      style: { fg: C.dim },
    });
  } else if (!searchPattern) {
    allTreeLines.push({
      text: padStr("  " + editor.t("panel.type_pattern"), W) + "\n",
      properties: { type: "empty" },
      style: { fg: C.dim },
    });
  } else {
    let flatIdx = 0;
    for (let fi = 0; fi < fileGroups.length; fi++) {
      const group = fileGroups[fi];
      const isFileSelected = focusPanel === "matches" && panel.matchIndex === flatIdx;
      if (isFileSelected) selectedLineIdx = allTreeLines.length;
      const expandIcon = group.expanded ? "v" : ">";
      const badge = getFileExtBadge(group.relPath);
      const matchCount = group.matches.length;
      const selectedInFile = group.matches.filter(m => m.selected).length;
      const fileLineText = ` ${expandIcon} ${badge} ${group.relPath} (${selectedInFile}/${matchCount})`;

      const fileOverlays: InlineOverlay[] = [];
      const eiStart = byteLen(" ");
      const eiEnd = eiStart + byteLen(expandIcon);
      fileOverlays.push({ start: eiStart, end: eiEnd, style: { fg: C.expandIcon } });
      const bgStart = eiEnd + byteLen(" ");
      const bgEnd = bgStart + byteLen(badge);
      fileOverlays.push({ start: bgStart, end: bgEnd, style: { fg: C.fileIcon, bold: true } });
      const fpStart = bgEnd + byteLen(" ");
      const fpEnd = fpStart + byteLen(group.relPath);
      fileOverlays.push({ start: fpStart, end: fpEnd, style: { fg: C.filePath } });

      allTreeLines.push({
        text: padStr(fileLineText, W) + "\n",
        properties: { type: "file-row", fileIndex: fi },
        style: isFileSelected ? { bg: C.selectedBg } : undefined,
        inlineOverlays: fileOverlays,
      });
      flatIdx++;

      if (group.expanded) {
        for (let mi = 0; mi < group.matches.length; mi++) {
          const result = group.matches[mi];
          const isMatchSelected = focusPanel === "matches" && panel.matchIndex === flatIdx;
          if (isMatchSelected) selectedLineIdx = allTreeLines.length;
          const checkbox = result.selected ? "[v]" : "[ ]";
          const location = `${group.relPath}:${result.match.line}`;
          const context = result.match.context.trim();
          const prefixText = `   ${isMatchSelected ? ">" : " "} ${checkbox} `;
          const maxCtx = W - charLen(prefixText) - charLen(location) - 3;
          const displayCtx = truncate(context, Math.max(10, maxCtx));
          const matchLineText = `${prefixText}${location} - ${displayCtx}`;

          const inlines: InlineOverlay[] = [];
          const cbStart = byteLen(`   ${isMatchSelected ? ">" : " "} `);
          const cbEnd = cbStart + byteLen(checkbox);
          inlines.push({ start: cbStart, end: cbEnd, style: { fg: result.selected ? C.checkOn : C.checkOff } });
          const locStart = cbEnd + byteLen(" ");
          const locEnd = locStart + byteLen(location);
          inlines.push({ start: locStart, end: locEnd, style: { fg: C.lineNum } });

          if (panel.searchPattern) {
            const ctxStart = locEnd + byteLen(" - ");
            highlightMatches(displayCtx, panel.searchPattern, ctxStart, panel.useRegex, panel.caseSensitive, inlines);
          }

          allTreeLines.push({
            text: padStr(matchLineText, W) + "\n",
            properties: { type: "match-row", fileIndex: fi, matchIndex: mi },
            style: isMatchSelected ? { bg: C.selectedBg } : undefined,
            inlineOverlays: inlines.length > 0 ? inlines : undefined,
          });
          flatIdx++;
        }
      }
    }
  }

  // Virtual scroll: fixed rows = query(1) + options(1) + separator(1) + help(1) + tab bar(1) = 5
  const fixedRows = 5;
  const treeVisibleRows = Math.max(3, getViewportHeight() - fixedRows);

  // Adjust scroll offset to keep selected line visible
  if (selectedLineIdx >= 0) {
    if (selectedLineIdx < panel.scrollOffset) {
      panel.scrollOffset = selectedLineIdx;
    }
    if (selectedLineIdx >= panel.scrollOffset + treeVisibleRows) {
      panel.scrollOffset = selectedLineIdx - treeVisibleRows + 1;
    }
  }
  const maxOffset = Math.max(0, allTreeLines.length - treeVisibleRows);
  if (panel.scrollOffset > maxOffset) panel.scrollOffset = maxOffset;
  if (panel.scrollOffset < 0) panel.scrollOffset = 0;

  const visibleLines = allTreeLines.slice(panel.scrollOffset, panel.scrollOffset + treeVisibleRows);
  for (const line of visibleLines) entries.push(line);

  // Scroll indicators
  const canScrollUp = panel.scrollOffset > 0;
  const canScrollDown = panel.scrollOffset + treeVisibleRows < allTreeLines.length;
  const scrollHint = canScrollUp || canScrollDown
    ? "  " + (canScrollUp ? "↑" : " ") + (canScrollDown ? "↓" : " ")
    : "";

  // ── Help bar ──
  const helpText = " " + editor.t("panel.help") + scrollHint;
  entries.push({
    text: truncate(helpText, W) + "\n",
    properties: { type: "help" },
    style: { fg: C.help },
  });

  return entries;
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

// Highlight search pattern occurrences in a display string
function highlightMatches(text: string, pattern: string, baseByteOffset: number, isRegex: boolean, caseSensitive: boolean, overlays: InlineOverlay[]): void {
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
        const startByte = baseByteOffset + byteLen(text.substring(0, idx));
        const endByte = startByte + byteLen(text.substring(idx, idx + pattern.length));
        overlays.push({ start: startByte, end: endByte, style: { bg: C.matchBg, fg: C.matchFg } });
        pos = idx + pattern.length;
      }
    } else {
      const flags = caseSensitive ? "g" : "gi";
      const re = new RegExp(pattern, flags);
      let m;
      while ((m = re.exec(text)) !== null) {
        if (m[0].length === 0) { re.lastIndex++; continue; }
        const startByte = baseByteOffset + byteLen(text.substring(0, m.index));
        const endByte = startByte + byteLen(m[0]);
        overlays.push({ start: startByte, end: endByte, style: { bg: C.matchBg, fg: C.matchFg } });
      }
    }
  } catch (_e) { /* invalid regex */ }
}

// =============================================================================
// Panel update
// =============================================================================

function updatePanelContent(): void {
  if (panel) {
    // Refresh viewport width each time
    panel.viewportWidth = getViewportWidth();
    editor.setVirtualBufferContent(panel.resultsBufferId, buildPanelEntries());
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
      (matches: GrepMatch[], _done: boolean) => {
        // Discard if a newer search has started
        if (generation !== currentSearchGeneration || !panel) return;

        const newResults: SearchResult[] = matches.map(m => ({ match: m, selected: true }));

        if (newResults.length > 0) {
          allResults = allResults.concat(newResults);
          panel.searchResults = allResults;
          panel.fileGroups = buildFileGroups(allResults);
          updatePanelContent();
        }
      }
    );

    // Final state
    if (generation !== currentSearchGeneration || !panel) return allResults;

    if (!silent) {
      if (allResults.length === 0) {
        editor.setStatus(editor.t("status.no_matches", { pattern }));
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
    cursorPos: prefill.length,
    scrollOffset: 0,
  };

  try {
    const result = await editor.createVirtualBufferInSplit({
      name: "*Search/Replace*",
      mode: "search-replace-list",
      readOnly: true,
      entries: buildPanelEntries(),
      ratio: 0.6,
      panelId: "search-replace-panel",
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

function search_replace_backspace(): void {
  if (!panel || panel.focusPanel !== "query") return;
  const text = getActiveFieldText();
  const pos = panel.cursorPos;
  if (pos <= 0) return;
  setActiveFieldText(text.slice(0, pos - 1) + text.slice(pos));
  panel.cursorPos = pos - 1;
  updatePanelContent();
}
registerHandler("search_replace_backspace", search_replace_backspace);

function search_replace_delete(): void {
  if (!panel || panel.focusPanel !== "query") return;
  const text = getActiveFieldText();
  const pos = panel.cursorPos;
  if (pos >= text.length) return;
  setActiveFieldText(text.slice(0, pos) + text.slice(pos + 1));
  updatePanelContent();
}
registerHandler("search_replace_delete", search_replace_delete);

function search_replace_home(): void {
  if (!panel || panel.focusPanel !== "query") return;
  panel.cursorPos = 0;
  updatePanelContent();
}
registerHandler("search_replace_home", search_replace_home);

function search_replace_end(): void {
  if (!panel || panel.focusPanel !== "query") return;
  panel.cursorPos = getActiveFieldText().length;
  updatePanelContent();
}
registerHandler("search_replace_end", search_replace_end);

// =============================================================================
// Navigation handlers
// =============================================================================

function search_replace_nav_down(): void {
  if (!panel) return;
  if (panel.focusPanel === "query") {
    if (panel.queryField === "search") {
      panel.queryField = "replace";
      panel.cursorPos = panel.replaceText.length;
    }
    updatePanelContent();
  } else if (panel.focusPanel === "options") {
    if (panel.optionIndex < 3) { panel.optionIndex++; updatePanelContent(); }
  } else {
    const flat = buildFlatItems();
    if (panel.matchIndex < flat.length - 1) { panel.matchIndex++; updatePanelContent(); }
  }
}
registerHandler("search_replace_nav_down", search_replace_nav_down);

function search_replace_nav_up(): void {
  if (!panel) return;
  if (panel.focusPanel === "query") {
    if (panel.queryField === "replace") {
      panel.queryField = "search";
      panel.cursorPos = panel.searchPattern.length;
    }
    updatePanelContent();
  } else if (panel.focusPanel === "options") {
    if (panel.optionIndex > 0) { panel.optionIndex--; updatePanelContent(); }
  } else {
    if (panel.matchIndex > 0) { panel.matchIndex--; updatePanelContent(); }
  }
}
registerHandler("search_replace_nav_up", search_replace_nav_up);

function search_replace_tab(): void {
  editor.debug("search_replace_tab CALLED, panel=" + (panel ? "yes" : "null"));
  if (!panel) return;
  if (panel.focusPanel === "query") {
    if (panel.queryField === "search") {
      // Search → Replace
      panel.queryField = "replace";
      panel.cursorPos = panel.replaceText.length;
      updatePanelContent();
      return;
    } else {
      // Replace → Options
      panel.focusPanel = "options";
    }
  } else if (panel.focusPanel === "options") {
    panel.focusPanel = "matches";
  } else {
    // Matches → Query/Search
    panel.focusPanel = "query";
    panel.queryField = "search";
    panel.cursorPos = panel.searchPattern.length;
  }
  updatePanelContent();
}
registerHandler("search_replace_tab", search_replace_tab);

function search_replace_shift_tab(): void {
  if (!panel) return;
  if (panel.focusPanel === "matches") {
    panel.focusPanel = "options";
  } else if (panel.focusPanel === "options") {
    panel.focusPanel = "query";
    panel.queryField = "replace";
    panel.cursorPos = panel.replaceText.length;
  } else {
    if (panel.queryField === "replace") {
      panel.queryField = "search";
      panel.cursorPos = panel.searchPattern.length;
    } else {
      panel.focusPanel = "matches";
    }
  }
  updatePanelContent();
}
registerHandler("search_replace_shift_tab", search_replace_shift_tab);

function search_replace_nav_left(): void {
  if (!panel) return;
  // When in query panel, move cursor left
  if (panel.focusPanel === "query") {
    if (panel.cursorPos > 0) {
      panel.cursorPos--;
      updatePanelContent();
    }
    return;
  }
  if (panel.focusPanel !== "matches") return;
  const flat = buildFlatItems();
  const item = flat[panel.matchIndex];
  if (!item) return;
  if (item.type === "file") {
    if (panel.fileGroups[item.fileIndex].expanded) {
      panel.fileGroups[item.fileIndex].expanded = false;
      updatePanelContent();
    }
  } else {
    for (let i = panel.matchIndex - 1; i >= 0; i--) {
      if (flat[i].type === "file" && flat[i].fileIndex === item.fileIndex) {
        panel.matchIndex = i;
        updatePanelContent();
        break;
      }
    }
  }
}
registerHandler("search_replace_nav_left", search_replace_nav_left);

function search_replace_nav_right(): void {
  if (!panel) return;
  // When in query panel, move cursor right
  if (panel.focusPanel === "query") {
    const text = getActiveFieldText();
    if (panel.cursorPos < text.length) {
      panel.cursorPos++;
      updatePanelContent();
    }
    return;
  }
  if (panel.focusPanel !== "matches") return;
  const flat = buildFlatItems();
  const item = flat[panel.matchIndex];
  if (!item) return;
  if (item.type === "file" && !panel.fileGroups[item.fileIndex].expanded) {
    panel.fileGroups[item.fileIndex].expanded = true;
    updatePanelContent();
  }
}
registerHandler("search_replace_nav_right", search_replace_nav_right);

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

function search_replace_enter(): void {
  editor.debug("search_replace_enter CALLED, panel=" + (panel ? "yes" : "null"));
  if (!panel) return;
  if (panel.focusPanel === "query") {
    // Enter in query field = confirm and run search
    if (panel.queryField === "search") {
      // Move to replace field
      panel.queryField = "replace";
      panel.cursorPos = panel.replaceText.length;
      updatePanelContent();
    } else {
      // Confirm replace field and run search
      if (panel.searchPattern) {
        rerunSearch().then(() => {
          if (panel) {
            panel.focusPanel = "matches";
            panel.matchIndex = 0;
  panel.scrollOffset = 0;
            updatePanelContent();
          }
        });
      }
    }
  } else if (panel.focusPanel === "options") {
    if (panel.optionIndex === 3) {
      doReplaceAll();
    } else {
      search_replace_space();
    }
  } else {
    const flat = buildFlatItems();
    const item = flat[panel.matchIndex];
    if (!item) return;
    if (item.type === "file") {
      panel.fileGroups[item.fileIndex].expanded = !panel.fileGroups[item.fileIndex].expanded;
      updatePanelContent();
    } else {
      const group = panel.fileGroups[item.fileIndex];
      const result = group.matches[item.matchIndex!];
      editor.openFileInSplit(panel.sourceSplitId, result.match.file, result.match.line, result.match.column);
    }
  }
}
registerHandler("search_replace_enter", search_replace_enter);

function search_replace_space(): void {
  if (!panel) return;
  if (panel.focusPanel === "query") {
    // Space in query field = insert space character
    insertCharAtCursor(" ");
    return;
  }
  if (panel.focusPanel === "options") {
    if (panel.optionIndex === 0) { panel.caseSensitive = !panel.caseSensitive; updatePanelContent(); rerunSearchDebounced(); }
    else if (panel.optionIndex === 1) { panel.useRegex = !panel.useRegex; updatePanelContent(); rerunSearchDebounced(); }
    else if (panel.optionIndex === 2) { panel.wholeWords = !panel.wholeWords; updatePanelContent(); rerunSearchDebounced(); }
    else if (panel.optionIndex === 3) { doReplaceAll(); }
    return;
  }
  if (panel.focusPanel === "matches") {
    const flat = buildFlatItems();
    const item = flat[panel.matchIndex];
    if (!item) return;
    if (item.type === "file") {
      const group = panel.fileGroups[item.fileIndex];
      const allSelected = group.matches.every(m => m.selected);
      for (const m of group.matches) m.selected = !allSelected;
    } else {
      const group = panel.fileGroups[item.fileIndex];
      group.matches[item.matchIndex!].selected = !group.matches[item.matchIndex!].selected;
    }
    updatePanelContent();
  }
}
registerHandler("search_replace_space", search_replace_space);

async function doReplaceAll(): Promise<void> {
  if (!panel || panel.busy) return;
  const selected = panel.searchResults.filter(r => r.selected);
  if (selected.length === 0) {
    editor.setStatus(editor.t("status.no_items_selected"));
    return;
  }
  panel.busy = true;
  editor.setStatus(editor.t("status.replacing", { count: String(selected.length) }));
  const statusMsg = await executeReplacements(selected);
  editor.setStatus(statusMsg);
  await rerunSearchQuiet();
  panel.busy = false;
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

  panel.busy = true;
  editor.setStatus(editor.t("status.replacing", { count: String(toReplace.length) }));
  const statusMsg = await executeReplacements(toReplace);
  editor.setStatus(statusMsg);
  await rerunSearchQuiet();
  panel.busy = false;
  updatePanelContent();
}

function search_replace_close(): void {
  if (!panel) return;
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

function onSearchReplaceResize(data: { width: number; height: number }): void {
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
}
registerHandler("onSearchReplaceResize", onSearchReplaceResize);

editor.on("resize", "onSearchReplaceResize");

// Prompt handlers (in case prompts are opened externally for this panel - gracefully handle)
function onSearchReplacePromptCancelled(args: { prompt_type: string }): boolean {
  if (!args.prompt_type.startsWith("search-replace-")) return true;
  return true;
}
registerHandler("onSearchReplacePromptCancelled", onSearchReplacePromptCancelled);
editor.on("prompt_cancelled", "onSearchReplacePromptCancelled");

editor.registerCommand(
  "%cmd.search_replace",
  "%cmd.search_replace_desc",
  "start_search_replace",
  null
);

editor.debug("Search & Replace plugin loaded");
