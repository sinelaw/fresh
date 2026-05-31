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
 * Reveal a symbol in the buffer by moving the cursor to its name.
 *
 * - "preview" (browsing the list): also scroll the name toward the top
 *   third and paint an overlay marker over it.
 * - "select" (confirming with Enter): move the cursor and drop the marker.
 *
 * Why scroll only on preview: `scrollBufferToLine` latches the viewport's
 * "skip ensure-visible" flag so a plugin scroll isn't immediately undone.
 * That's right while the prompt is open and re-previewing each keystroke,
 * but on confirm the prompt tears down and the latch would stay stuck —
 * freezing the viewport so it no longer follows the cursor. On confirm we
 * rely on `setBufferCursor` alone, which runs the normal ensure-visible
 * pass (no latch): the cursor lands on the name and the viewport tracks it
 * normally afterward.
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
  editor.setBufferCursor(bufferId, pos);

  clearOverlay(bufferId);
  if (mode === "preview") {
    // The cursor move alone doesn't reposition the viewport while the
    // prompt owns focus, so scroll explicitly to keep the previewed
    // symbol visible.
    editor.scrollBufferToLine(bufferId, sym.nameLine);
    editor.addOverlay(bufferId, OVERLAY_NS, pos, pos + sym.name.length, MATCH_STYLE);
  }
}

async function loadSymbols(filePath: string, language: string): Promise<SymbolItem[]> {
  try {
    const uri = editor.pathToFileUri(filePath);
    const result = await editor.sendLspRequest(
      language,
      "textDocument/documentSymbol",
      {
        textDocument: { uri },
      },
    );

    const symbols = parseSymbols(result);

    await attachLineText(symbols);

    return symbols;
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    editor.setStatus(`LSP symbols failed: ${msg}`);
    return [];
  }
}

/**
 * Fill in each symbol's source line so the finder can show a snippet with
 * the matching word highlighted. Reads the single line each symbol name
 * lives on (start..end byte range), in parallel.
 */
async function attachLineText(symbols: SymbolItem[]): Promise<void> {
  if (symbols.length === 0 || cachedBufferId === null) return;
  const bufferId = cachedBufferId;

  await Promise.all(
    symbols.map(async (sym) => {
      const start = await editor.getLineStartPosition(sym.nameLine);
      const end = await editor.getLineEndPosition(sym.nameLine);
      if (start === null || end === null || end < start) {
        sym.lineText = "";
        return;
      }
      sym.lineStartByte = start;
      const text = await editor.getBufferText(bufferId, start, end);
      sym.lineText = text.replace(/\r$/, "");

      // Refine the name column. LSP `SymbolInformation` reports the start
      // of the whole declaration (e.g. the `def`/indentation), not the
      // name — so locate the name on the line, searching from the
      // reported column, to land the cursor and overlay exactly on it.
      let idx = sym.lineText.indexOf(sym.name, sym.nameCharacter);
      if (idx < 0) idx = sym.lineText.indexOf(sym.name);
      if (idx >= 0) sym.nameCharacter = idx;
    }),
  );
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
    // Only setBufferCursor — it ensure-visibles the restored position
    // without latching skip-ensure-visible, so the viewport keeps
    // following the cursor afterward (see navigateToSymbol).
    clearOverlay(cachedBufferId);
    if (!confirmed && cachedBufferId !== null) {
      editor.setBufferCursor(cachedBufferId, cachedCursorPosition);
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
  const symbols = await loadSymbols(cachedFilePath, cachedLanguage);
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

  if (Array.isArray(result)) {
    for (const item of result) {
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
    }
  }

  symbols.sort((a, b) => a.startLine - b.startLine);

  return symbols;
}

editor.registerCommand(
  "%cmd.goto_lsp_symbol",
  "%cmd.goto_lsp_symbol_desc",
  "goto_lsp_symbol",
);

editor.debug("LSP navigation plugin loaded");
