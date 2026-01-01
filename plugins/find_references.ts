/// <reference path="../types/fresh.d.ts" />

/**
 * Find References Plugin (TypeScript)
 *
 * Displays LSP find references results in a virtual buffer split view.
 * Listens for lsp_references hook from the editor and shows results.
 * Uses cursor movement for navigation (Up/Down/j/k work naturally).
 */

// Panel state
let panelOpen = false;
let referencesBufferId: number | null = null;
let sourceSplitId: number | null = null;
let referencesSplitId: number | null = null; // Track the split we created
let currentReferences: ReferenceItem[] = [];
let currentSymbol: string = "";
let lineCache: Map<string, string[]> = new Map(); // Cache file contents

// Maximum number of results to display
const MAX_RESULTS = 100;

// Reference item structure
interface ReferenceItem {
  file: string;
  line: number;
  column: number;
  lineText?: string; // Cached line text
}

// Define the references mode with minimal keybindings
// Navigation uses normal cursor movement (arrows, j/k work naturally)
editor.defineMode(
  "references-list",
  null, // no parent mode
  [
    ["Return", "references_goto"],
    ["q", "references_close"],
    ["Escape", "references_close"],
  ],
  true // read-only
);

// Get relative path for display
function getRelativePath(filePath: string): string {
  const cwd = editor.getCwd();
  if (filePath.startsWith(cwd)) {
    return filePath.slice(cwd.length + 1); // Remove cwd and leading /
  }
  return filePath;
}

// Format a reference for display with line preview
function formatReference(item: ReferenceItem): string {
  const displayPath = getRelativePath(item.file);
  const location = `${displayPath}:${item.line}:${item.column}`;

  // Truncate location if too long, leaving room for line text
  const maxLocationLen = 50;
  const truncatedLocation = location.length > maxLocationLen
    ? "..." + location.slice(-(maxLocationLen - 3))
    : location.padEnd(maxLocationLen);

  // Get line text preview (truncated)
  const lineText = item.lineText || "";
  const trimmedLine = lineText.trim();
  const maxLineLen = 60;
  const displayLine = trimmedLine.length > maxLineLen
    ? trimmedLine.slice(0, maxLineLen - 3) + "..."
    : trimmedLine;

  return `  ${truncatedLocation}  ${displayLine}\n`;
}

// Build entries for the virtual buffer
function buildPanelEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header with symbol name
  const totalCount = currentReferences.length;
  const limitNote = totalCount >= MAX_RESULTS ? editor.t("panel.limited", { max: String(MAX_RESULTS) }) : "";
  const symbolDisplay = currentSymbol ? `'${currentSymbol}'` : "symbol";
  entries.push({
    text: `═══ ${editor.t("panel.header", { symbol: symbolDisplay, count: String(totalCount), limit: limitNote })} ═══\n`,
    properties: { type: "header" },
  });

  if (currentReferences.length === 0) {
    entries.push({
      text: "  " + editor.t("panel.no_references") + "\n",
      properties: { type: "empty" },
    });
  } else {
    // Add each reference
    for (let i = 0; i < currentReferences.length; i++) {
      const ref = currentReferences[i];
      entries.push({
        text: formatReference(ref),
        properties: {
          type: "reference",
          index: i,
          location: {
            file: ref.file,
            line: ref.line,
            column: ref.column,
          },
        },
      });
    }
  }

  // Footer
  entries.push({
    text: `───────────────────────────────────────────────────────────────────────────────\n`,
    properties: { type: "separator" },
  });
  entries.push({
    text: editor.t("panel.help") + "\n",
    properties: { type: "help" },
  });

  return entries;
}

// Load line text for references
async function loadLineTexts(references: ReferenceItem[]): Promise<void> {
  // Group references by file
  const fileRefs: Map<string, ReferenceItem[]> = new Map();
  for (const ref of references) {
    if (!fileRefs.has(ref.file)) {
      fileRefs.set(ref.file, []);
    }
    fileRefs.get(ref.file)!.push(ref);
  }

  // Load each file and extract lines
  for (const [filePath, refs] of fileRefs) {
    try {
      // Check cache first
      let lines = lineCache.get(filePath);
      if (!lines) {
        const content = await editor.readFile(filePath);
        lines = content.split("\n");
        lineCache.set(filePath, lines);
      }

      // Set line text for each reference
      for (const ref of refs) {
        const lineIndex = ref.line - 1; // Convert 1-based to 0-based
        if (lineIndex >= 0 && lineIndex < lines.length) {
          ref.lineText = lines[lineIndex];
        } else {
          ref.lineText = "";
        }
      }
    } catch (error) {
      // If file can't be read, leave lineText empty
      for (const ref of refs) {
        ref.lineText = "";
      }
    }
  }
}

// Show references panel
async function showReferencesPanel(symbol: string, references: ReferenceItem[]): Promise<void> {
  // Only save the source split ID if panel is not already open
  // (avoid overwriting it with the references split ID on subsequent calls)
  if (!panelOpen) {
    sourceSplitId = editor.getActiveSplitId();
  }

  // Limit results
  const limitedRefs = references.slice(0, MAX_RESULTS);

  // Set references and symbol
  currentSymbol = symbol;
  currentReferences = limitedRefs;

  // Load line texts for preview
  await loadLineTexts(currentReferences);

  // Build panel entries
  const entries = buildPanelEntries();

  // Create or update virtual buffer in horizontal split
  // The panel_id mechanism will reuse the existing buffer/split if it exists
  try {
    referencesBufferId = await editor.createVirtualBufferInSplit({
      name: "*References*",
      mode: "references-list",
      read_only: true,
      entries: entries,
      ratio: 0.7, // Original pane takes 70%, references takes 30%
      panel_id: "references-panel",
      show_line_numbers: false,
      show_cursors: true, // Enable cursor for navigation
    });

    panelOpen = true;
    // Track the references split (it becomes active after creation)
    referencesSplitId = editor.getActiveSplitId();

    const limitMsg = references.length > MAX_RESULTS
      ? editor.t("status.showing_first", { max: String(MAX_RESULTS) })
      : "";
    editor.setStatus(
      editor.t("status.found_references", { count: String(references.length), limit: limitMsg })
    );
    editor.debug(`References panel opened with buffer ID ${referencesBufferId}, split ID ${referencesSplitId}`);
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    editor.setStatus(editor.t("status.failed_open_panel"));
    editor.debug(`ERROR: createVirtualBufferInSplit failed: ${errorMessage}`);
  }
}

// Handle lsp_references hook
globalThis.on_lsp_references = function (data: { symbol: string; locations: ReferenceItem[] }): void {
  editor.debug(`Received ${data.locations.length} references for '${data.symbol}'`);

  if (data.locations.length === 0) {
    editor.setStatus(editor.t("status.no_references", { symbol: data.symbol }));
    return;
  }

  // Clear line cache for fresh results
  lineCache.clear();

  // Show the references panel
  showReferencesPanel(data.symbol, data.locations);
};

// Register the hook handler
editor.on("lsp_references", "on_lsp_references");

// Handle cursor movement to show current reference info
globalThis.on_references_cursor_moved = function (data: {
  buffer_id: number;
  cursor_id: number;
  old_position: number;
  new_position: number;
}): void {
  // Only handle cursor movement in our references buffer
  if (referencesBufferId === null || data.buffer_id !== referencesBufferId) {
    return;
  }

  // Get cursor line to determine which reference is selected
  // getCursorLine() returns the line for the active buffer
  const cursorLine = editor.getCursorLine();

  // Line 0 is header, lines 1 to N are references
  const refIndex = cursorLine - 1;

  if (refIndex >= 0 && refIndex < currentReferences.length) {
    editor.setStatus(editor.t("status.reference_index", { current: String(refIndex + 1), total: String(currentReferences.length) }));
  }
};

// Register cursor movement handler
editor.on("cursor_moved", "on_references_cursor_moved");

// Hide references panel
globalThis.hide_references_panel = function (): void {
  if (!panelOpen) {
    return;
  }

  if (referencesBufferId !== null) {
    editor.closeBuffer(referencesBufferId);
  }

  // Close the split we created (if it exists and is different from source)
  if (referencesSplitId !== null && referencesSplitId !== sourceSplitId) {
    editor.closeSplit(referencesSplitId);
  }

  panelOpen = false;
  referencesBufferId = null;
  sourceSplitId = null;
  referencesSplitId = null;
  currentReferences = [];
  currentSymbol = "";
  lineCache.clear();
  editor.setStatus(editor.t("status.closed"));
};

// Navigation: go to selected reference (based on cursor position)
globalThis.references_goto = function (): void {
  if (currentReferences.length === 0) {
    editor.setStatus(editor.t("status.no_references_to_jump"));
    return;
  }

  if (sourceSplitId === null) {
    editor.setStatus(editor.t("status.source_split_unavailable"));
    return;
  }

  if (referencesBufferId === null) {
    return;
  }

  // Get text properties at cursor position
  const props = editor.getTextPropertiesAtCursor(referencesBufferId);
  editor.debug(`references_goto: props.length=${props.length}, referencesBufferId=${referencesBufferId}, sourceSplitId=${sourceSplitId}`);

  if (props.length > 0) {
    editor.debug(`references_goto: props[0]=${JSON.stringify(props[0])}`);
    const location = props[0].location as
      | { file: string; line: number; column: number }
      | undefined;
    if (location) {
      editor.debug(`references_goto: opening ${location.file}:${location.line}:${location.column} in split ${sourceSplitId}`);
      // Open file in the source split, not the references split
      editor.openFileInSplit(
        sourceSplitId,
        location.file,
        location.line,
        location.column || 0
      );
      const displayPath = getRelativePath(location.file);
      editor.setStatus(editor.t("status.jumped_to", { file: displayPath, line: String(location.line) }));
    } else {
      editor.debug(`references_goto: no location in props[0]`);
      editor.setStatus(editor.t("status.move_cursor"));
    }
  } else {
    editor.debug(`references_goto: no props found at cursor`);
    editor.setStatus(editor.t("status.move_cursor"));
  }
};

// Close the references panel
globalThis.references_close = function (): void {
  globalThis.hide_references_panel();
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
editor.setStatus(editor.t("status.ready"));
editor.debug("Find References plugin initialized");
