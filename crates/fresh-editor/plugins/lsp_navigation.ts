/// <reference path="./lib/fresh.d.ts" />

import { Finder, FilterSource, defaultFuzzyFilter, DisplayEntry } from "./lib/finder.ts";

interface SymbolItem {
  name: string;
  kind: number;
  startLine: number;
  startCharacter: number;
  endLine: number;
}

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

let preloadedSymbols: SymbolItem[] = [];

async function navigateToSymbol(bufferId: number, sym: SymbolItem): Promise<void> {
  if (bufferId === null) return;

  const bytePos = await editor.getLineStartPosition(sym.startLine);

  if (bytePos === null) return;

  editor.setBufferCursor(bufferId, bytePos + (sym.startCharacter ?? 0));

  editor.scrollBufferToLine(bufferId, sym.startLine);
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

    return symbols;
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    editor.setStatus(`LSP symbols failed: ${msg}`);
    return [];
  }
}

function format(sym: SymbolItem): DisplayEntry {
  return {
    label: `[${getKindLabel(sym.kind)}] ${sym.name}`,
    description: `line ${sym.startLine + 1}`,
  }
};

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
        (span === bestSpan && sym.startLine === bestStartLine && sym.startCharacter < bestStartChar)
      ) {
        bestIdx = i;
        bestSpan = span;
        bestStartLine = sym.startLine;
        bestStartChar = sym.startCharacter;
      }
    }
  }
  return bestIdx;
}

const finder = new Finder(editor, {
  id: "lsp_symbols",
  preview: false,
  format,
  onSelect: async (sym) => {
    await navigateToSymbol(cachedBufferId, sym);
  },
  onSelectionChanged: async (sym) => {
    await navigateToSymbol(cachedBufferId, sym);
  },
  onClose: () => {
    editor.setBufferCursor(cachedBufferId, cachedCursorPosition);
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

  // Pre-load symbols to determine matching index for preselection
  const symbols = await loadSymbols(cachedFilePath, cachedLanguage);
  const matchIdx = findMatchingSymbolIndex(symbols, cachedCursorLine);
  preloadedSymbols = symbols;

  finder.prompt({
    title: "Go to symbol: ",
    source: finderSource,
    initialSelectedIndex: matchIdx >= 0 ? matchIdx : undefined,
  });
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

      let startLine = 1;
      let startCharacter = 0;
      let endLine = 1;

      if ("location" in raw && typeof raw.location === "object") {
        const loc = raw.location as Record<string, unknown>;

        if ("range" in loc && typeof loc.range === "object") {
          const range = loc.range as Record<string, unknown>;
          const start = range.start as Record<string, unknown>;
          const end = range.end as Record<string, unknown>;

          startLine = typeof start.line === "number" ? start.line : 0;
          startCharacter = typeof start.character === "number" ? start.character : 0;
          endLine = typeof end.line === "number" ? end.line : startLine;
        }
      } else if ("selectionRange" in raw) {
        // Hierarchical DocumentSymbol has both range (full extent)
        // and selectionRange (name extent). Prefer range for endLine.
        if ("range" in raw && typeof raw.range === "object") {
          const range = raw.range as Record<string, unknown>;
          const start = range.start as Record<string, unknown>;
          const end = range.end as Record<string, unknown>;
          startLine = typeof start.line === "number" ? start.line : 0;
          startCharacter = typeof start.character === "number" ? start.character : 0;
          endLine = typeof end.line === "number" ? end.line : startLine;
        } else {
          const selectionRange = raw.selectionRange as Record<string, unknown>;
          const start = selectionRange.start as Record<string, unknown>;
          const end = selectionRange.end as Record<string, unknown>;
          startLine = typeof start.line === "number" ? start.line : 0;
          startCharacter = typeof start.character === "number" ? start.character : 0;
          endLine = typeof end.line === "number" ? end.line : startLine;
        }
      }

      symbols.push({
        name,
        kind,
        startLine,
        startCharacter,
        endLine,
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
