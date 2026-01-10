/// <reference path="./lib/fresh.d.ts" />

/**
 * Find References Plugin
 *
 * Displays LSP find references results using the Finder abstraction
 * with panel mode for static data display.
 *
 * Key features:
 * - Panel mode for displaying reference results
 * - groupBy: "file" for organized display
 * - syncWithEditor for bidirectional cursor sync
 */

import { Finder, getRelativePath } from "./lib/finder.ts";

const editor = getEditor();

// Maximum number of results to display
const MAX_RESULTS = 100;

// Reference item structure from LSP
interface ReferenceLocation {
  file: string;
  line: number;
  column: number;
}

// Line text cache for previews
const lineCache: Map<string, string[]> = new Map();

// Create the finder instance for panel mode
const finder = new Finder<ReferenceLocation>(editor, {
  id: "references",
  format: (ref) => {
    const displayPath = getRelativePath(editor, ref.file);
    const key = `${ref.file}:${ref.line}:${ref.column}`;
    const lineText = lineTexts.get(key) || "";
    const trimmedLine = lineText.trim();

    // Preview text
    const maxPreviewLen = 60;
    const preview =
      trimmedLine.length > maxPreviewLen
        ? trimmedLine.slice(0, maxPreviewLen - 3) + "..."
        : trimmedLine;

    return {
      label: `${ref.line}:${ref.column}`,
      description: preview,
      location: {
        file: ref.file,
        line: ref.line,
        column: ref.column,
      },
    };
  },
  groupBy: "file",
  syncWithEditor: true,
  onSelect: (ref) => {
    const displayPath = getRelativePath(editor, ref.file);
    editor.setStatus(`Jumped to ${displayPath}:${ref.line}`);
  },
});

// Global line texts map (populated before showing panel)
let lineTexts = new Map<string, string>();

/**
 * Load line text for references (for preview display)
 */
async function loadLineTexts(
  references: ReferenceLocation[]
): Promise<Map<string, string>> {
  const result = new Map<string, string>();

  // Group references by file
  const fileRefs: Map<string, ReferenceLocation[]> = new Map();
  for (const ref of references) {
    if (!fileRefs.has(ref.file)) {
      fileRefs.set(ref.file, []);
    }
    fileRefs.get(ref.file)!.push(ref);
  }

  // Load each file and extract lines
  for (const [filePath, refs] of fileRefs) {
    try {
      let lines = lineCache.get(filePath);
      if (!lines) {
        const content = await editor.readFile(filePath);
        lines = content.split("\n");
        lineCache.set(filePath, lines);
      }

      for (const ref of refs) {
        const lineIndex = ref.line - 1;
        if (lineIndex >= 0 && lineIndex < lines.length) {
          const key = `${ref.file}:${ref.line}:${ref.column}`;
          result.set(key, lines[lineIndex]);
        }
      }
    } catch {
      // If file can't be read, skip
    }
  }

  return result;
}

/**
 * Show references panel with the given results
 */
async function showReferences(
  symbol: string,
  references: ReferenceLocation[]
): Promise<void> {
  // Limit results
  const limitedRefs = references.slice(0, MAX_RESULTS);

  // Clear and reload line cache
  lineCache.clear();
  lineTexts = await loadLineTexts(limitedRefs);

  // Build title
  const count = references.length;
  const limitNote = count > MAX_RESULTS ? ` (first ${MAX_RESULTS})` : "";
  const title = `References to '${symbol}': ${count}${limitNote}`;

  // Show panel
  await finder.panel({
    title,
    items: limitedRefs,
    ratio: 0.3,
  });
}

// Handle lsp_references hook
globalThis.on_lsp_references = function (data: {
  symbol: string;
  locations: ReferenceLocation[];
}): void {
  editor.debug(`Received ${data.locations.length} references for '${data.symbol}'`);

  if (data.locations.length === 0) {
    editor.setStatus(`No references found for '${data.symbol}'`);
    return;
  }

  showReferences(data.symbol, data.locations);
};

// Register the hook handler
editor.on("lsp_references", "on_lsp_references");

// Export close function for command palette
globalThis.hide_references_panel = function (): void {
  finder.close();
  lineCache.clear();
};

// Register commands
editor.registerCommand(
  "%cmd.show_references",
  "%cmd.show_references_desc",
  "show_references_panel",
  "normal"
);

editor.registerCommand(
  "%cmd.hide_references",
  "%cmd.hide_references_desc",
  "hide_references_panel",
  "normal"
);

// Plugin initialization
editor.setStatus("Find References plugin ready");
editor.debug("Find References plugin initialized (using Finder abstraction)");
