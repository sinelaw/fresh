/// <reference path="./lib/fresh.d.ts" />

import { Finder, FilterSource, defaultFuzzyFilter, DisplayEntry } from "./lib/finder.ts";

interface SymbolItem {
  name: string;
  kind: number;
  // Full extent of the symbol (used to find the symbol enclosing the
  // cursor for preselection).
  startLine: number;
  endLine: number;
  // Precise position of the symbol *name* (LSP selectionRange). This is
  // where the cursor jumps to and what gets the overlay highlight.
  //
  // `nameCharacter` is a UTF-16 offset, as LSP reports it, until
  // `attachLineText` rewrites it to a byte column for the finder. Only the
  // finder's symbols go through that, and only the untouched ones are fit to
  // hand to `setBreadcrumbs`, which expects LSP coordinates.
  nameLine: number;
  nameCharacter: number;
  // Byte offset of the start of `nameLine`, resolved once up front. We
  // can't resolve it later via getLineStartPosition because that targets
  // the *active* buffer, which is no longer ours by the time a result is
  // confirmed (the prompt has torn down). -1 means "unknown".
  lineStartByte: number;
  // The raw source line the name lives on, used to render a snippet with
  // the matching word highlighted in the results list.
  lineText: string;
}

// Overlay namespace for the "current symbol" highlight painted in the
// buffer while moving through the results list.
const OVERLAY_NS = "lsp_symbol_nav";

// Styling shared by the in-buffer overlay and the in-list snippet
// highlight so the matched word reads the same in both places.
const MATCH_STYLE = {
  fg: "search.match_fg",
  bg: "search.match_bg",
  bold: true,
};

function getKindLabel(kind: number): string {
  switch (kind) {
    case 1:
      return "file";
    case 2:
      return "mod";
    case 3:
      return "ns";
    case 4:
      return "pkg";
    case 5:
      return "class";
    case 6:
      return "method";
    case 7:
      return "prop";
    case 8:
      return "field";
    case 9:
      return "construct";
    case 10:
      return "enum";
    case 11:
      return "iface";
    case 12:
      return "fn";
    case 13:
      return "var";
    case 14:
      return "const";
    case 22:
      return "enum-mem";
    case 23:
      return "struct";
    case 24:
      return "event";
    case 25:
      return "op";
    case 26:
      return "type-param";
    default:
      return "item";
  }
}

let cachedBufferId: number | null = null;
let cachedFilePath: string = "";
let cachedLanguage: string | undefined = undefined;
let cachedCursorPosition = 0;
let cachedCursorLine = 0;
// Set true once the user confirms a result with Enter, so the
// cursor-restore in onClose doesn't undo the committed jump.
let confirmed = false;

let preloadedSymbols: SymbolItem[] = [];

function clearOverlay(bufferId: number | null): void {
  if (bufferId === null) return;
  editor.clearNamespace(bufferId, OVERLAY_NS);
}

/**
 * Reveal a symbol in the buffer.
 *
 * - "preview" (browsing the list): paint an overlay marker over the name.
 * - "select" (confirming with Enter): drop the marker; the move sticks.
 *
 * Synchronous and addressed to an explicit buffer id (not the active
 * buffer) so it lands correctly even when invoked on confirm, after the
 * prompt — and our buffer's active status — has been torn down.
 */
function navigateToSymbol(
  bufferId: number | null,
  sym: SymbolItem,
  mode: "preview" | "select",
): void {
  if (bufferId === null || sym.lineStartByte < 0) return;

  const pos = sym.lineStartByte + sym.nameCharacter;
  // Move the cursor in both modes: the editor keeps the cursor on screen
  // every frame, so a scroll without a cursor move is immediately undone
  // for off-screen symbols. Moving the cursor is what makes the viewport
  // follow. The pre-open cursor is restored if the user cancels.
  editor.setBufferCursor(bufferId, pos);
  editor.scrollBufferToLine(bufferId, sym.nameLine);

  clearOverlay(bufferId);
  if (mode === "preview") {
    // The overlay is placed in bytes, so the name is measured in bytes too —
    // `.length` is UTF-16 units and ends the highlight inside a wide glyph.
    editor.addOverlay(
      bufferId,
      OVERLAY_NS,
      pos,
      pos + editor.utf8ByteLength(sym.name),
      MATCH_STYLE,
    );
  }
}

/**
 * The server's symbols, as reported. No line is read here: the LSP line and
 * character are enough for the breadcrumb trail, which is the caller that
 * wants every symbol in the document.
 */
async function fetchSymbols(
  filePath: string,
  language: string,
): Promise<SymbolItem[]> {
  const uri = editor.pathToFileUri(filePath);
  const result = await editor.sendLspRequest(
    language,
    "textDocument/documentSymbol",
    {
      textDocument: { uri },
    },
  );

  return parseSymbols(result);
}

/**
 * The same, with each symbol's source line attached — what the finder needs
 * to render a snippet and to jump precisely. Only the finder asks for this,
 * and only for a list the user opened.
 */
async function loadSymbols(
  filePath: string,
  language: string,
  bufferId: number,
): Promise<SymbolItem[]> {
  try {
    const symbols = await fetchSymbols(filePath, language);
    await attachLineText(symbols, bufferId);
    return symbols;
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    editor.setStatus(`LSP symbols failed: ${msg}`);
    return [];
  }
}

/**
 * Fill in each symbol's source line and explicit-buffer byte offset. Read each
 * distinct declaration line once; never snapshot or scan the full buffer.
 * Passing `bufferId` to the line-position APIs avoids active-buffer races.
 *
 * This rewrites `nameCharacter` from a UTF-16 offset to a byte column, so a
 * symbol that has been through here is no longer in LSP coordinates.
 */
async function attachLineText(symbols: SymbolItem[], bufferId: number): Promise<void> {
  if (symbols.length === 0) return;

  const lines = new Map<number, { start: number; text: string }>();
  const uniqueLines = Array.from(new Set(symbols.map((sym) => sym.nameLine)));
  await Promise.all(uniqueLines.map(async (line) => {
    const [start, end] = await Promise.all([
      editor.getLineStartPosition(line, bufferId),
      editor.getLineEndPosition(line, bufferId),
    ]);
    if (start === null || end === null || end < start) return;
    const text = await editor.getBufferText(bufferId, start, end);
    lines.set(line, { start, text: text.replace(/\r$/, "") });
  }));

  for (const sym of symbols) {
    const line = lines.get(sym.nameLine);
    if (!line) {
      sym.lineText = "";
      continue;
    }
    sym.lineStartByte = line.start;
    sym.lineText = line.text;

    // Refine the name column. LSP `SymbolInformation` reports the start
    // of the whole declaration (e.g. the `def`/indentation), not the
    // name — so locate the name on the line, searching from the
    // reported column, to land the cursor and overlay exactly on it.
    //
    // `nameCharacter` leaves this loop as a byte column either way: the name's
    // own when it is on the line, the reported column converted when it is
    // not. Leaving a UTF-16 column behind made `lineStartByte + nameCharacter`
    // add a byte offset to a UTF-16 one, which lands off the name on any line
    // with a wide character before it.
    let idx = sym.lineText.indexOf(sym.name, sym.nameCharacter);
    if (idx < 0) idx = sym.lineText.indexOf(sym.name);
    if (idx < 0) idx = Math.min(sym.nameCharacter, sym.lineText.length);
    sym.nameCharacter = editor.utf8ByteLength(sym.lineText.slice(0, idx));
  }
}

/**
 * Split the symbol's source line into styled spans, highlighting the
 * matched word. Leading/trailing whitespace is trimmed for a compact
 * snippet. Falls back to searching for the name if `nameCharacter`
 * doesn't line up (e.g. SymbolInformation ranges that span the whole
 * declaration).
 */
function buildSnippetSpans(sym: SymbolItem): StyledText[] | undefined {
  const raw = sym.lineText;
  if (!raw) return undefined;

  let nameStart = sym.nameCharacter;
  if (raw.slice(nameStart, nameStart + sym.name.length) !== sym.name) {
    nameStart = raw.indexOf(sym.name);
  }

  const trimmed = raw.replace(/^\s+/, "");
  const trimOffset = raw.length - trimmed.length;

  if (nameStart < trimOffset) {
    // Couldn't locate the name on the line — show the plain snippet.
    return [{ text: trimmed.replace(/\s+$/, "") }];
  }

  const before = raw.slice(trimOffset, nameStart);
  const mid = raw.slice(nameStart, nameStart + sym.name.length);
  const after = raw.slice(nameStart + sym.name.length).replace(/\s+$/, "");

  const spans: StyledText[] = [];
  if (before) spans.push({ text: before });
  spans.push({ text: mid, style: MATCH_STYLE });
  if (after) spans.push({ text: after });
  return spans;
}

function format(sym: SymbolItem): DisplayEntry {
  const trimmed = sym.lineText ? sym.lineText.trim() : `line ${sym.nameLine + 1}`;
  return {
    label: `[${getKindLabel(sym.kind)}] ${sym.name}`,
    description: trimmed,
    descriptionSpans: buildSnippetSpans(sym),
  };
}

function findMatchingSymbolIndex(symbols: SymbolItem[], cursorLine: number): number {
  let bestIdx = -1;
  let bestSpan = Number.MAX_SAFE_INTEGER;
  let bestStartLine = Number.MAX_SAFE_INTEGER;
  let bestStartChar = Number.MAX_SAFE_INTEGER;

  for (let i = 0; i < symbols.length; i++) {
    const sym = symbols[i];
    if (sym.startLine <= cursorLine && cursorLine <= sym.endLine) {
      const span = sym.endLine - sym.startLine;
      if (
        span < bestSpan ||
        (span === bestSpan && sym.startLine < bestStartLine) ||
        (span === bestSpan && sym.startLine === bestStartLine && sym.nameCharacter < bestStartChar)
      ) {
        bestIdx = i;
        bestSpan = span;
        bestStartLine = sym.startLine;
        bestStartChar = sym.nameCharacter;
      }
    }
  }
  return bestIdx;
}

const finder = new Finder(editor, {
  id: "lsp_symbols",
  preview: false,
  format,
  onSelect: (sym) => {
    // Commit the jump — this is the only place the cursor moves. The flag
    // guards onClose in case it ever also fires on confirm.
    confirmed = true;
    navigateToSymbol(cachedBufferId, sym, "select");
  },
  onSelectionChanged: (sym) => {
    navigateToSymbol(cachedBufferId, sym, "preview");
  },
  onClose: () => {
    // Cancelled: drop the marker and restore the cursor to where it was
    // before the finder opened (preview moved it as the user browsed).
    clearOverlay(cachedBufferId);
    if (!confirmed && cachedBufferId !== null) {
      editor.setBufferCursor(cachedBufferId, cachedCursorPosition);
      editor.scrollBufferToLine(cachedBufferId, cachedCursorLine);
    }
  },
});

const finderSource: FilterSource<SymbolItem> = {
  mode: "filter",
  load: async () => preloadedSymbols,
  filter: (items, query) => {
    const filtered = defaultFuzzyFilter(
      items,
      query,
      format,
      100,
    );

    filtered.sort((a, b) => a.startLine - b.startLine);

    return filtered;
  },
};

async function openSymbolsListHandler(): Promise<void> {
  cachedBufferId = editor.getActiveBufferId();

  if (cachedBufferId === null) {
    return;
  }

  cachedLanguage = editor.getBufferInfo(cachedBufferId)?.language;

  if (!cachedLanguage) {
    return;
  }

  cachedFilePath = editor.getBufferPath(cachedBufferId);

  if (!cachedFilePath) {
    return;
  }

  cachedCursorPosition = editor.getCursorPosition();
  cachedCursorLine = editor.getCursorLine();
  confirmed = false;
  clearOverlay(cachedBufferId);

  // Pre-load symbols to determine matching index for preselection
  const symbols = await loadSymbols(cachedFilePath, cachedLanguage, cachedBufferId);
  const matchIdx = findMatchingSymbolIndex(symbols, cachedCursorLine);
  preloadedSymbols = symbols;

  finder.prompt({
    title: "Go to symbol: ",
    source: finderSource,
    initialSelectedIndex: matchIdx >= 0 ? matchIdx : undefined,
  });

  // Preview (and highlight) the preselected symbol right away — the
  // selection-changed event only fires on later arrow-key movement.
  if (matchIdx >= 0) {
    navigateToSymbol(cachedBufferId, symbols[matchIdx], "preview");
  }
}

registerHandler("goto_lsp_symbol", openSymbolsListHandler);

function parseSymbols(result: unknown): SymbolItem[] {
  const symbols: SymbolItem[] = [];

  if (!result) return symbols;

  function append(items: unknown[]): void {
    for (const item of items) {
      if (typeof item !== "object" || item === null) continue;

      const raw = item as Record<string, unknown>;
      const kind = Number(raw.kind) || 0;
      const name = String(raw.name ?? "");

      if (!name) continue;

      // Full extent of the symbol.
      let startLine = 0;
      let endLine = 0;
      // Precise position of the name.
      let nameLine = 0;
      let nameCharacter = 0;

      if ("location" in raw && typeof raw.location === "object") {
        // SymbolInformation: a single range; the name position is its start.
        const loc = raw.location as Record<string, unknown>;
        if ("range" in loc && typeof loc.range === "object") {
          const range = loc.range as Record<string, unknown>;
          const start = range.start as Record<string, unknown>;
          const end = range.end as Record<string, unknown>;

          startLine = typeof start.line === "number" ? start.line : 0;
          endLine = typeof end.line === "number" ? end.line : startLine;
          nameLine = startLine;
          nameCharacter = typeof start.character === "number" ? start.character : 0;
        }
      } else {
        // Hierarchical DocumentSymbol: `range` is the full extent,
        // `selectionRange` is the name. Use `range` for enclosing-symbol
        // detection and `selectionRange` for the precise cursor target.
        if ("range" in raw && typeof raw.range === "object") {
          const range = raw.range as Record<string, unknown>;
          const start = range.start as Record<string, unknown>;
          const end = range.end as Record<string, unknown>;
          startLine = typeof start.line === "number" ? start.line : 0;
          endLine = typeof end.line === "number" ? end.line : startLine;
          nameLine = startLine;
          nameCharacter = typeof start.character === "number" ? start.character : 0;
        }
        if ("selectionRange" in raw && typeof raw.selectionRange === "object") {
          const selectionRange = raw.selectionRange as Record<string, unknown>;
          const start = selectionRange.start as Record<string, unknown>;
          nameLine = typeof start.line === "number" ? start.line : nameLine;
          nameCharacter = typeof start.character === "number" ? start.character : nameCharacter;
        }
      }

      symbols.push({
        name,
        kind,
        startLine,
        endLine,
        nameLine,
        nameCharacter,
        lineStartByte: -1,
        lineText: "",
      });

      // DocumentSymbol responses are hierarchical. Keep every descendant so
      // both the finder and the breadcrumb trail can expose nested context.
      if (Array.isArray(raw.children)) append(raw.children);
    }
  }

  if (Array.isArray(result)) append(result);

  symbols.sort((a, b) => a.startLine - b.startLine);

  return symbols;
}

const breadcrumbSymbols = new Map<number, SymbolItem[]>();
const breadcrumbRefreshGeneration = new Map<number, number>();
// How many event-driven retries a buffer whose fetch failed still has. User
// actions — an edit, a revert, switching to the tab — are never rationed.
const MAX_TRAIL_RETRIES = 3;
const breadcrumbRetries = new Map<number, number>();

/**
 * The symbol kinds that are *not* a scope — what a breadcrumb trail leaves
 * out. A class, a function or a module is somewhere you can *be*; a variable
 * is something you are next to.
 *
 * Without this, servers that report locals put them in the trail: pylsp
 * turns a comprehension into `Store > total_value > s > n`. Set
 * `editor.breadcrumb_all_symbols` to see everything the server reports.
 *
 * Named by what it drops rather than by what it keeps, because the kinds a
 * server uses for a scope vary: rust-analyzer reports an `impl` block as
 * `Object`, so an allow-list of the obvious scopes silently costs every Rust
 * method the type it belongs to.
 */
const NON_SCOPE_KINDS = new Set([
  1, // file
  7, // property
  8, // field
  13, // variable
  14, // constant
  15, // string
  16, // number
  17, // boolean
  18, // array
  20, // key
  21, // null
  22, // enum member
  24, // event
  25, // operator
  26, // type parameter
]);

// `getConfig` walks the whole merged config into a fresh JS object graph, so
// the flag is read once and kept until the config changes. The trail is
// recomputed on every caret move; the setting is not going to have changed.
let everyKind: boolean | null = null;

function showsEveryKind(): boolean {
  if (everyKind === null) {
    const cfg = editor.getConfig() as
      | { editor?: { breadcrumb_all_symbols?: boolean } }
      | null;
    everyKind = cfg?.editor?.breadcrumb_all_symbols === true;
  }
  return everyKind;
}

function breadcrumbTrail(symbols: SymbolItem[], cursorLine: number): SymbolItem[] {
  // Read the setting once, not once per symbol.
  const showsAll = showsEveryKind();
  const containing = symbols.filter(
    (sym) =>
      sym.startLine <= cursorLine &&
      cursorLine <= sym.endLine &&
      (showsAll || !NON_SCOPE_KINDS.has(sym.kind)),
  );
  containing.sort((a, b) => {
    const spanA = a.endLine - a.startLine;
    const spanB = b.endLine - b.startLine;
    return spanB - spanA || a.startLine - b.startLine || a.nameCharacter - b.nameCharacter;
  });

  const trail: SymbolItem[] = [];
  for (const sym of containing) {
    const parent = trail[trail.length - 1];
    if (!parent || (parent.startLine <= sym.startLine && sym.endLine <= parent.endLine)) {
      if (!parent || parent.name !== sym.name || parent.startLine !== sym.startLine || parent.endLine !== sym.endLine) {
        trail.push(sym);
      }
    }
  }
  return trail;
}

function publishBreadcrumbs(bufferId: number, cursorLine: number): void {
  const symbols = breadcrumbSymbols.get(bufferId) ?? [];
  const items = breadcrumbTrail(symbols, cursorLine).map((sym) => ({
    label: sym.name,
    line: sym.nameLine,
    character: sym.nameCharacter,
  }));
  editor.setBreadcrumbs(bufferId, items);
}

async function refreshBreadcrumbs(bufferId: number): Promise<void> {
  const generation = (breadcrumbRefreshGeneration.get(bufferId) ?? 0) + 1;
  breadcrumbRefreshGeneration.set(bufferId, generation);
  const info = editor.getBufferInfo(bufferId);
  const language = info?.language;
  const filePath = editor.getBufferPath(bufferId);
  if (!language || !filePath) {
    breadcrumbSymbols.delete(bufferId);
    editor.setBreadcrumbs(bufferId, []);
    return;
  }

  let symbols: SymbolItem[];
  try {
    symbols = await fetchSymbols(filePath, language);
  } catch {
    // Leave the cache unset rather than recording "no symbols" — the server
    // may simply not be up yet, and a later refresh can still fill it in.
    return;
  }
  if (breadcrumbRefreshGeneration.get(bufferId) !== generation) return;
  breadcrumbSymbols.set(bufferId, symbols);
  if (editor.getActiveBufferId() === bufferId) {
    publishBreadcrumbs(bufferId, editor.getCursorLine());
  }
}

function scheduleBreadcrumbRefresh(bufferId: number): void {
  const generation = (breadcrumbRefreshGeneration.get(bufferId) ?? 0) + 1;
  breadcrumbRefreshGeneration.set(bufferId, generation);
  void (async () => {
    await editor.delay(250);
    if (breadcrumbRefreshGeneration.get(bufferId) !== generation) return;
    // refreshBreadcrumbs owns the next generation number.
    await refreshBreadcrumbs(bufferId);
  })();
}

editor.on("buffer_activated", (data) => {
  const cached = breadcrumbSymbols.get(data.buffer_id);
  if (cached) publishBreadcrumbs(data.buffer_id, editor.getCursorLine());
  else editor.setBreadcrumbs(data.buffer_id, []);
  scheduleBreadcrumbRefresh(data.buffer_id);
});

editor.on("cursor_moved", (data) => {
  // The trail follows *the* caret, which is the primary one — not cursor 0.
  // Adding a cursor makes the new one primary, so its id is whatever was
  // handed out last. `line` is 1-indexed here; LSP ranges are 0-indexed.
  if (data.is_primary) publishBreadcrumbs(data.buffer_id, data.line - 1);
});

editor.on("config_changed", () => {
  // Drop the cached flag and redraw with it, so toggling the setting shows.
  everyKind = null;
  const bufferId = editor.getActiveBufferId();
  if (breadcrumbSymbols.has(bufferId)) {
    publishBreadcrumbs(bufferId, editor.getCursorLine());
  }
});

editor.on("after_insert", (data) => scheduleBreadcrumbRefresh(data.buffer_id));
editor.on("after_delete", (data) => scheduleBreadcrumbRefresh(data.buffer_id));
editor.on("after_file_revert", (data) => scheduleBreadcrumbRefresh(data.buffer_id));
editor.on("buffer_closed", (data) => {
  breadcrumbSymbols.delete(data.buffer_id);
  breadcrumbRefreshGeneration.delete(data.buffer_id);
  breadcrumbRetries.delete(data.buffer_id);
});

/**
 * Retry a trail whose fetch failed, at most `MAX_TRAIL_RETRIES` times.
 *
 * A failed fetch deliberately leaves no cache entry, so "never came back" and
 * "has no server at all" look identical from here — a plain text buffer asks
 * a server that does not exist, is refused, and stays missing forever. The
 * budget is what separates them: a trail that can be fetched needs a retry or
 * two, and one that never can stops asking.
 */
function retryTrail(bufferId: number): void {
  if (breadcrumbSymbols.has(bufferId)) return;
  const spent = breadcrumbRetries.get(bufferId) ?? 0;
  if (spent >= MAX_TRAIL_RETRIES) return;
  breadcrumbRetries.set(bufferId, spent + 1);
  scheduleBreadcrumbRefresh(bufferId);
}

// The server was not up when we asked; now it is. Only its own language's
// buffers are worth re-asking, and the server is already running, so this
// starts nothing.
editor.on("lsp_ready", (data) => {
  for (const info of editor.listBuffers()) {
    if (info.language === data.language) retryTrail(info.id);
  }
});

// The other way a fetch fails: the server was up and the request still did
// not come back. No event covers that, so take the next sign of life.
//
// Scoped to the buffer the diagnostics are *for*: a push from one language's
// server is no reason to ask another language's server for anything, and
// `sendLspRequest` spawns one to find out.
editor.on("diagnostics_updated", (data) => {
  for (const info of editor.listBuffers()) {
    if (info.path && editor.pathToFileUri(info.path) === data.uri) {
      retryTrail(info.id);
    }
  }
});

editor.on("ready", () => {
  const bufferId = editor.getActiveBufferId();
  if (bufferId !== null) scheduleBreadcrumbRefresh(bufferId);
});

editor.registerCommand(
  "%cmd.goto_lsp_symbol",
  "%cmd.goto_lsp_symbol_desc",
  "goto_lsp_symbol",
);

editor.debug("LSP navigation plugin loaded");
