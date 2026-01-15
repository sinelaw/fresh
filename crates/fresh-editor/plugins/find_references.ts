/// <reference path="./lib/fresh.d.ts" />

/**
 * Find References Plugin
 *
 * Displays LSP find references results using the Finder abstraction
 * with filter mode for unified prompt-based UX.
 */

import { Finder, getRelativePath } from "./lib/finder.ts";

const editor = getEditor();

// Reference location from LSP
interface ReferenceLocation {
  file: string;
  line: number;
  column: number;
  content?: string;
}

// Create the finder instance - same UX as grep plugins
const finder = new Finder<ReferenceLocation>(editor, {
  id: "references",
  format: (ref) => {
    const displayPath = getRelativePath(editor, ref.file);
    const content = ref.content?.trim() ?? "";
    const description =
      content.length > 60 ? content.substring(0, 57) + "..." : content;

    return {
      label: `${displayPath}:${ref.line}`,
      description,
      location: {
        file: ref.file,
        line: ref.line,
        column: ref.column,
      },
    };
  },
  preview: true,
  maxResults: 100,
});

// Pending references for the current prompt
let pendingRefs: ReferenceLocation[] = [];

/**
 * Load line content for references
 */
async function loadLineContent(
  refs: ReferenceLocation[]
): Promise<ReferenceLocation[]> {
  const result: ReferenceLocation[] = [];
  const fileCache = new Map<string, string[]>();

  for (const ref of refs) {
    let lines: string[];
    const cached = fileCache.get(ref.file);
    if (cached !== undefined) {
      lines = cached;
    } else {
      try {
        const content = await editor.readFile(ref.file);
        lines = content.split("\n");
      } catch {
        lines = [];
      }
      fileCache.set(ref.file, lines);
    }

    const lineIndex = ref.line - 1;
    const lineContent =
      lineIndex >= 0 && lineIndex < lines.length ? lines[lineIndex] : "";

    result.push({ ...ref, content: lineContent });
  }

  return result;
}

// Handle lsp_references hook
globalThis.on_lsp_references = async function (data: {
  symbol: string;
  locations: ReferenceLocation[];
}): Promise<void> {
  editor.debug(
    `Received ${data.locations.length} references for '${data.symbol}'`
  );

  if (data.locations.length === 0) {
    editor.setStatus(`No references found for '${data.symbol}'`);
    return;
  }

  // Load line content for descriptions
  pendingRefs = await loadLineContent(data.locations);

  // Use prompt mode with filter source - same UX as grep plugins
  finder.prompt({
    title: `References to '${data.symbol}' (${data.locations.length})`,
    source: {
      mode: "filter",
      load: async () => pendingRefs,
    },
  });
};

// Register the hook handler
editor.on("lsp_references", "on_lsp_references");

// Close function for command palette
globalThis.close_references = function (): void {
  finder.close();
};

editor.debug("Find References plugin loaded (using Finder abstraction)");
