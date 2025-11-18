/// <reference path="../types/fresh.d.ts" />

/**
 * Find References Plugin (TypeScript)
 *
 * Displays LSP find references results in a virtual buffer split view.
 * Listens for lsp_references hook from the editor and shows results.
 */

// Panel state
let panelOpen = false;
let referencesBufferId: number | null = null;
let sourceSplitId: number | null = null;
let currentReferences: ReferenceItem[] = [];
let selectedIndex = 0;

// Reference item structure
interface ReferenceItem {
  file: string;
  line: number;
  column: number;
}

// Define the references mode with keybindings
editor.defineMode(
  "references-list",
  null, // no parent mode
  [
    ["Return", "references_goto"],
    ["n", "references_next"],
    ["p", "references_prev"],
    ["j", "references_next"],
    ["k", "references_prev"],
    ["q", "references_close"],
    ["Escape", "references_close"],
  ],
  true // read-only
);

// Format a reference for display
function formatReference(item: ReferenceItem, index: number): string {
  const marker = index === selectedIndex ? ">" : " ";
  // Show relative path if possible
  const cwd = editor.getCwd();
  let displayPath = item.file;
  if (displayPath.startsWith(cwd)) {
    displayPath = displayPath.slice(cwd.length + 1); // Remove cwd and leading /
  }
  return `${marker} ${displayPath}:${item.line}:${item.column}\n`;
}

// Build entries for the virtual buffer
function buildPanelEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  entries.push({
    text: "═══ References ═══\n",
    properties: { type: "header" },
  });

  if (currentReferences.length === 0) {
    entries.push({
      text: "  No references found\n",
      properties: { type: "empty" },
    });
  } else {
    // Add each reference
    for (let i = 0; i < currentReferences.length; i++) {
      const ref = currentReferences[i];
      entries.push({
        text: formatReference(ref, i),
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

  // Footer with count
  entries.push({
    text: `───────────────────────\n`,
    properties: { type: "separator" },
  });
  entries.push({
    text: `Total: ${currentReferences.length} reference(s)\n`,
    properties: { type: "summary" },
  });

  return entries;
}

// Update the panel content
function updatePanelContent(): void {
  if (referencesBufferId !== null) {
    const entries = buildPanelEntries();
    editor.setVirtualBufferContent(referencesBufferId, entries);
  }
}

// Show references panel
async function showReferencesPanel(references: ReferenceItem[]): Promise<void> {
  // Close existing panel if open
  if (panelOpen && referencesBufferId !== null) {
    editor.closeBuffer(referencesBufferId);
  }

  // Save the current split ID before creating the references split
  sourceSplitId = editor.getActiveSplitId();

  // Set references
  currentReferences = references;
  selectedIndex = 0;

  // Build panel entries
  const entries = buildPanelEntries();

  // Create virtual buffer in horizontal split
  try {
    referencesBufferId = await editor.createVirtualBufferInSplit({
      name: "*References*",
      mode: "references-list",
      read_only: true,
      entries: entries,
      ratio: 0.7, // Original pane takes 70%, references takes 30%
      panel_id: "references-panel",
      show_line_numbers: false,
      show_cursors: true,
    });

    panelOpen = true;
    editor.setStatus(
      `Found ${currentReferences.length} reference(s) - Press RET to jump, n/p to navigate, q to close`
    );
    editor.debug(`References panel opened with buffer ID ${referencesBufferId}`);
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    editor.setStatus("Failed to open references panel");
    editor.debug(`ERROR: createVirtualBufferInSplit failed: ${errorMessage}`);
  }
}

// Handle lsp_references hook
globalThis.on_lsp_references = function (data: { locations: ReferenceItem[] }): void {
  editor.debug(`Received ${data.locations.length} references`);

  if (data.locations.length === 0) {
    editor.setStatus("No references found");
    return;
  }

  // Show the references panel
  showReferencesPanel(data.locations);
};

// Register the hook handler
editor.on("lsp_references", "on_lsp_references");

// Hide references panel
globalThis.hide_references_panel = function (): void {
  if (!panelOpen) {
    return;
  }

  if (referencesBufferId !== null) {
    editor.closeBuffer(referencesBufferId);
  }

  panelOpen = false;
  referencesBufferId = null;
  sourceSplitId = null;
  selectedIndex = 0;
  currentReferences = [];
  editor.setStatus("References panel closed");
};

// Navigation: go to selected reference
globalThis.references_goto = function (): void {
  if (currentReferences.length === 0) {
    editor.setStatus("No references to jump to");
    return;
  }

  if (sourceSplitId === null) {
    editor.setStatus("Source split not available");
    return;
  }

  const bufferId = editor.getActiveBufferId();
  const props = editor.getTextPropertiesAtCursor(bufferId);

  if (props.length > 0) {
    const location = props[0].location as
      | { file: string; line: number; column: number }
      | undefined;
    if (location) {
      // Open file in the source split, not the references split
      editor.openFileInSplit(
        sourceSplitId,
        location.file,
        location.line,
        location.column || 0
      );
      editor.setStatus(`Jumped to ${location.file}:${location.line}`);
    } else {
      editor.setStatus("No location info for this reference");
    }
  } else {
    // Fallback: use selectedIndex
    const ref = currentReferences[selectedIndex];
    if (ref) {
      editor.openFileInSplit(sourceSplitId, ref.file, ref.line, ref.column);
      editor.setStatus(`Jumped to ${ref.file}:${ref.line}`);
    }
  }
};

// Navigation: next reference
globalThis.references_next = function (): void {
  if (currentReferences.length === 0) return;

  selectedIndex = (selectedIndex + 1) % currentReferences.length;
  updatePanelContent();
  editor.setStatus(`Reference ${selectedIndex + 1}/${currentReferences.length}`);
};

// Navigation: previous reference
globalThis.references_prev = function (): void {
  if (currentReferences.length === 0) return;

  selectedIndex =
    selectedIndex > 0 ? selectedIndex - 1 : currentReferences.length - 1;
  updatePanelContent();
  editor.setStatus(`Reference ${selectedIndex + 1}/${currentReferences.length}`);
};

// Close the references panel
globalThis.references_close = function (): void {
  globalThis.hide_references_panel();
};

// Register commands
editor.registerCommand(
  "Show References Panel",
  "Display current references",
  "show_references_panel",
  "normal"
);

editor.registerCommand(
  "Hide References Panel",
  "Close the references panel",
  "hide_references_panel",
  "normal"
);

// Plugin initialization
editor.setStatus("Find References plugin loaded");
editor.debug("Find References plugin initialized");
