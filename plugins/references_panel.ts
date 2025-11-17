/// <reference path="../types/fresh.d.ts" />

/**
 * References Panel Plugin (TypeScript)
 *
 * Displays all references to the symbol under cursor using LSP.
 * Shows results in a virtual buffer split view with navigation.
 */

// Panel state
let panelOpen = false;
let referencesBufferId: number | null = null;
let currentReferences: ReferenceItem[] = [];
let selectedIndex = 0;

// Reference item structure
interface ReferenceItem {
  uri: string;
  line: number;
  character: number;
  endLine: number;
  endCharacter: number;
  // Computed display fields
  displayPath: string;
  preview: string;
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
  return `${marker} ${item.displayPath}:${item.line + 1}:${item.character + 1}\n`;
}

// Build entries for the virtual buffer
function buildPanelEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  entries.push({
    text: "=== References ===\n",
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
            file: ref.displayPath,
            line: ref.line + 1, // Convert to 1-indexed for openFile
            column: ref.character,
          },
        },
      });
    }
  }

  // Footer with count
  entries.push({
    text: `-------------------\n`,
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

// Convert URI to display path
function uriToPath(uri: string): string {
  // Remove file:// prefix if present
  if (uri.startsWith("file://")) {
    return uri.substring(7);
  }
  return uri;
}

// Show references panel
async function showReferencesPanel(): Promise<void> {
  if (panelOpen) {
    editor.setStatus("References panel already open");
    return;
  }

  // Get current buffer info
  const bufferId = editor.getActiveBufferId();
  const filePath = editor.getBufferPath(bufferId);

  if (!filePath) {
    editor.setStatus("No file open");
    return;
  }

  // Get cursor position
  const cursor = editor.getPrimaryCursor();
  if (!cursor) {
    editor.setStatus("No cursor position");
    return;
  }

  // Convert file path to URI
  const uri = `file://${filePath}`;

  editor.setStatus("Finding references...");

  try {
    // Call LSP find references
    const locations = await editor.findReferences(
      uri,
      cursor.line,
      cursor.column,
      true // include declaration
    );

    // Convert to reference items
    currentReferences = locations.map((loc) => ({
      uri: loc.uri,
      line: loc.line,
      character: loc.character,
      endLine: loc.end_line,
      endCharacter: loc.end_character,
      displayPath: uriToPath(loc.uri),
      preview: "", // Could add line preview in future
    }));

    selectedIndex = 0;

    // Build panel entries
    const entries = buildPanelEntries();

    // Create virtual buffer in horizontal split
    const success = editor.createVirtualBufferInSplit({
      name: "*References*",
      mode: "references-list",
      read_only: true,
      entries: entries,
      ratio: 0.7, // Original pane takes 70%, references takes 30%
      panel_id: "references-panel",
      show_line_numbers: false,
      show_cursors: true,
    });

    if (success) {
      panelOpen = true;
      referencesBufferId = editor.getActiveBufferId();
      editor.setStatus(
        `Found ${currentReferences.length} reference(s) - Press RET to jump, n/p to navigate, q to close`
      );
      editor.debug("References panel opened with virtual buffer split");
    } else {
      editor.setStatus("Failed to open references panel");
      editor.debug("ERROR: createVirtualBufferInSplit returned false");
    }
  } catch (error) {
    editor.setStatus(`Error finding references: ${error}`);
    editor.debug(`ERROR: findReferences failed: ${error}`);
  }
}

// Hide references panel
globalThis.hide_references_panel = function (): void {
  if (!panelOpen) {
    editor.setStatus("References panel not open");
    return;
  }

  panelOpen = false;
  referencesBufferId = null;
  selectedIndex = 0;
  currentReferences = [];
  editor.setStatus("References panel closed");
};

// Toggle references panel
globalThis.toggle_references_panel = async function (): Promise<void> {
  if (panelOpen) {
    globalThis.hide_references_panel();
  } else {
    await showReferencesPanel();
  }
};

// Show references for symbol under cursor
globalThis.find_references = async function (): Promise<void> {
  // Close existing panel if open
  if (panelOpen) {
    globalThis.hide_references_panel();
  }
  await showReferencesPanel();
};

// Navigation: go to selected reference
globalThis.references_goto = function (): void {
  if (currentReferences.length === 0) {
    editor.setStatus("No references to jump to");
    return;
  }

  const bufferId = editor.getActiveBufferId();
  const props = editor.getTextPropertiesAtCursor(bufferId);

  if (props.length > 0) {
    const location = props[0].location as
      | { file: string; line: number; column: number }
      | undefined;
    if (location) {
      editor.openFile(location.file, location.line, location.column || 0);
      editor.setStatus(`Jumped to ${location.file}:${location.line}`);
    } else {
      editor.setStatus("No location info for this reference");
    }
  } else {
    // Fallback: use selectedIndex
    const ref = currentReferences[selectedIndex];
    if (ref) {
      editor.openFile(ref.displayPath, ref.line + 1, ref.character);
      editor.setStatus(`Jumped to ${ref.displayPath}:${ref.line + 1}`);
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
  "Find References",
  "Find all references to symbol under cursor",
  "find_references",
  "normal"
);

editor.registerCommand(
  "Hide References Panel",
  "Close the references panel",
  "hide_references_panel",
  "normal"
);

editor.registerCommand(
  "Toggle References Panel",
  "Toggle references panel visibility",
  "toggle_references_panel",
  "normal"
);

// Plugin initialization
editor.setStatus("References Panel plugin loaded (TypeScript)");
editor.debug("References Panel plugin initialized - 3 commands registered");
