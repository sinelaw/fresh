/// <reference path="./lib/fresh.d.ts" />

/**
 * Diagnostics Panel Plugin
 *
 * Interactive diagnostics panel showing LSP diagnostics with:
 * - Real-time updates when diagnostics change
 * - Filter by current file or show all files
 * - Cursor navigation with highlighting
 * - Enter to jump to diagnostic location
 */

// =============================================================================
// Types and Interfaces
// =============================================================================

interface DiagnosticLocation {
  file: string;
  line: number;
  column: number;
}

interface DiagnosticLineMapping {
  panelLine: number;  // 1-based line in panel
  location: DiagnosticLocation;
}

interface DiagnosticsState {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null;
  sourceSplitId: number | null;  // The split that was active when panel opened
  sourceBufferId: number | null;
  showAllFiles: boolean;
  cachedContent: string;
  // Maps panel line numbers to diagnostic locations for sync
  lineMappings: DiagnosticLineMapping[];
  // Whether the panel itself is focused (freezes source buffer tracking)
  panelFocused: boolean;
  // Current cursor line in the panel (1-indexed)
  panelCursorLine: number;
}

// =============================================================================
// State Management
// =============================================================================

const state: DiagnosticsState = {
  isOpen: false,
  bufferId: null,
  splitId: null,
  sourceSplitId: null,
  sourceBufferId: null,
  showAllFiles: false,  // Default to filtering by current file
  cachedContent: "",
  lineMappings: [],
  panelFocused: false,
  panelCursorLine: 1,
};

// =============================================================================
// Color Definitions
// =============================================================================

const colors = {
  error: [255, 100, 100] as [number, number, number],
  warning: [255, 200, 100] as [number, number, number],
  info: [100, 200, 255] as [number, number, number],
  hint: [150, 150, 150] as [number, number, number],
  file: [180, 180, 255] as [number, number, number],
  location: [150, 255, 150] as [number, number, number],
  header: [255, 200, 100] as [number, number, number],
  selected: [80, 80, 120] as [number, number, number],
};

// =============================================================================
// Mode Definition
// =============================================================================

editor.defineMode(
  "diagnostics-list",
  "normal",
  [
    ["Return", "diagnostics_goto"],
    ["Tab", "diagnostics_goto"],
    ["a", "diagnostics_toggle_all"],
    ["r", "diagnostics_refresh"],
    ["q", "diagnostics_close"],
    ["Escape", "diagnostics_close"],
  ],
  true
);

// =============================================================================
// Helpers
// =============================================================================

function severityIcon(severity: number): string {
  switch (severity) {
    case 1: return "[E]";
    case 2: return "[W]";
    case 3: return "[I]";
    case 4: return "[H]";
    default: return "[?]";
  }
}

function uriToPath(uri: string): string {
  if (uri.startsWith("file://")) {
    return uri.slice(7);
  }
  return uri;
}

function getActiveFileUri(): string | null {
  const bufferId = state.sourceBufferId ?? editor.getActiveBufferId();
  const path = editor.getBufferPath(bufferId);
  if (!path) return null;
  return "file://" + path;
}

function entriesToContent(entries: TextPropertyEntry[]): string {
  return entries.map(e => e.text).join("");
}

// =============================================================================
// Panel Content Building
// =============================================================================

function buildPanelEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  const diagnostics = editor.getAllDiagnostics();

  // Clear and rebuild line mappings
  state.lineMappings = [];

  const activeUri = getActiveFileUri();
  const filterUri = state.showAllFiles ? null : activeUri;

  // Filter diagnostics
  const filtered = filterUri
    ? diagnostics.filter(d => d.uri === filterUri)
    : diagnostics;

  // Group by file
  const byFile = new Map<string, TsDiagnostic[]>();
  for (const diag of filtered) {
    const existing = byFile.get(diag.uri) || [];
    existing.push(diag);
    byFile.set(diag.uri, existing);
  }

  // Sort files, with active file first if filtering
  const files = Array.from(byFile.keys()).sort((a, b) => {
    if (activeUri) {
      if (a === activeUri) return -1;
      if (b === activeUri) return 1;
    }
    // Simple string comparison (localeCompare has ICU issues in Deno)
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  // Header (line 1)
  let filterLabel: string;
  if (state.showAllFiles) {
    filterLabel = "All Files";
  } else if (activeUri) {
    const fileName = editor.pathBasename(uriToPath(activeUri));
    filterLabel = fileName;
  } else {
    filterLabel = "Current File";
  }
  entries.push({
    text: `Diagnostics (${filterLabel}):\n`,
    properties: { type: "header" },
  });

  let currentPanelLine = 2; // Start after header

  if (filtered.length === 0) {
    entries.push({
      text: "  No diagnostics\n",
      properties: { type: "empty" },
    });
    currentPanelLine++;
  } else {
    let diagIndex = 0;
    for (const uri of files) {
      const fileDiags = byFile.get(uri) || [];
      const filePath = uriToPath(uri);
      const fileName = editor.pathBasename(filePath);

      // File header (blank line + filename)
      entries.push({
        text: `\n${fileName}:\n`,
        properties: { type: "file-header", uri },
      });
      currentPanelLine += 2; // blank line + file header

      // Sort diagnostics by line, then severity
      fileDiags.sort((a, b) => {
        const lineDiff = a.range.start.line - b.range.start.line;
        if (lineDiff !== 0) return lineDiff;
        return a.severity - b.severity;
      });

      for (const diag of fileDiags) {
        const icon = severityIcon(diag.severity);
        const line = diag.range.start.line + 1;
        const col = diag.range.start.character + 1;
        const msg = diag.message.split("\n")[0]; // First line only

        const location: DiagnosticLocation = {
          file: filePath,
          line: line,
          column: col,
        };

        // Track mapping for cursor sync
        state.lineMappings.push({
          panelLine: currentPanelLine,
          location: location,
        });

        entries.push({
          text: `  ${icon} ${line}:${col} ${msg}\n`,
          properties: {
            type: "diagnostic",
            index: diagIndex,
            severity: diag.severity,
            location: location,
          },
        });
        diagIndex++;
        currentPanelLine++;
      }
    }
  }

  // Summary
  const errorCount = filtered.filter(d => d.severity === 1).length;
  const warningCount = filtered.filter(d => d.severity === 2).length;
  const infoCount = filtered.filter(d => d.severity === 3).length;

  entries.push({
    text: "\n",
    properties: { type: "blank" },
  });
  entries.push({
    text: `${errorCount}E ${warningCount}W ${infoCount}I | a: toggle filter | r: refresh | RET: goto | q: close\n`,
    properties: { type: "footer" },
  });

  return entries;
}

// =============================================================================
// Highlighting
// =============================================================================

function applyHighlighting(): void {
  if (state.bufferId === null) return;

  const bufferId = state.bufferId;
  editor.clearNamespace(bufferId, "diag");

  const content = state.cachedContent;
  if (!content) return;

  const lines = content.split("\n");
  const cursorLine = state.panelCursorLine;

  let byteOffset = 0;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];
    const lineStart = byteOffset;
    const lineEnd = byteOffset + line.length;
    const isCurrentLine = (lineIdx + 1) === cursorLine;
    const isDiagnosticLine = line.trim().startsWith("[");

    // Highlight current line if it's a diagnostic line (entire line gets background)
    if (isCurrentLine && isDiagnosticLine) {
      editor.addOverlay(
        bufferId, "diag", lineStart, lineEnd,
        colors.selected[0], colors.selected[1], colors.selected[2],
        true, true, false
      );
    }

    // Header highlighting
    if (line.startsWith("Diagnostics")) {
      editor.addOverlay(
        bufferId, "diag", lineStart, lineEnd,
        colors.header[0], colors.header[1], colors.header[2],
        true, true, false
      );
    }

    // File header highlighting
    if (line.endsWith(":") && !line.startsWith("Diagnostics") && !line.startsWith(" ")) {
      editor.addOverlay(
        bufferId, "diag", lineStart, lineEnd,
        colors.file[0], colors.file[1], colors.file[2],
        false, true, false
      );
    }

    // Severity icon highlighting
    const iconMatch = line.match(/^\s+\[([EWIH?])\]/);
    if (iconMatch) {
      const iconStart = lineStart + line.indexOf("[");
      const iconEnd = iconStart + 3;

      let color: [number, number, number];
      switch (iconMatch[1]) {
        case "E": color = colors.error; break;
        case "W": color = colors.warning; break;
        case "I": color = colors.info; break;
        case "H": color = colors.hint; break;
        default: color = colors.hint;
      }

      editor.addOverlay(
        bufferId, "diag", iconStart, iconEnd,
        color[0], color[1], color[2],
        false, true, false
      );

      // Location highlighting (line:col after icon)
      const locMatch = line.match(/\[.\]\s+(\d+:\d+)/);
      if (locMatch && locMatch.index !== undefined) {
        const locStart = lineStart + line.indexOf(locMatch[1]);
        const locEnd = locStart + locMatch[1].length;
        editor.addOverlay(
          bufferId, "diag", locStart, locEnd,
          colors.location[0], colors.location[1], colors.location[2],
          false, false, false
        );
      }
    }

    byteOffset += line.length + 1;
  }
}

function updatePanel(): void {
  if (state.bufferId === null) return;

  const entries = buildPanelEntries();
  state.cachedContent = entriesToContent(entries);
  editor.setVirtualBufferContent(state.bufferId, entries);
  applyHighlighting();
}

// =============================================================================
// Commands
// =============================================================================

globalThis.show_diagnostics_panel = async function(): Promise<void> {
  if (state.isOpen) {
    // If already open, just focus the panel
    if (state.splitId !== null) {
      editor.focusSplit(state.splitId);
    }
    return;
  }

  state.sourceSplitId = editor.getActiveSplitId();
  state.sourceBufferId = editor.getActiveBufferId();
  state.panelFocused = false;

  const entries = buildPanelEntries();
  state.cachedContent = entriesToContent(entries);

  // Create a horizontal split below the current buffer
  const result = await editor.createVirtualBufferInSplit({
    name: "*Diagnostics*",
    mode: "diagnostics-list",
    read_only: true,
    entries: entries,
    ratio: 0.7,  // Source keeps 70%, panel takes 30%
    direction: "horizontal",  // Split below
    panel_id: "diagnostics",  // Enable idempotent updates
    show_line_numbers: false,
    show_cursors: true,
    editing_disabled: true,
  });

  if (result.buffer_id !== null) {
    state.isOpen = true;
    state.bufferId = result.buffer_id;
    state.splitId = result.split_id ?? null;
    state.panelFocused = true;  // Panel starts focused
    applyHighlighting();

    const diagnostics = editor.getAllDiagnostics();
    editor.setStatus(`Diagnostics: ${diagnostics.length} items | a: toggle filter | RET: goto | q: close`);
  } else {
    state.sourceSplitId = null;
    state.sourceBufferId = null;
    editor.setStatus("Failed to open diagnostics panel");
  }
};

globalThis.diagnostics_close = function(): void {
  if (!state.isOpen) return;

  // Capture values before clearing state
  const splitId = state.splitId;
  const sourceSplitId = state.sourceSplitId;
  const sourceBufferId = state.sourceBufferId;
  const bufferId = state.bufferId;

  // Clear state FIRST to prevent event handlers from trying to update
  state.isOpen = false;
  state.bufferId = null;
  state.splitId = null;
  state.sourceSplitId = null;
  state.sourceBufferId = null;
  state.cachedContent = "";
  state.panelFocused = false;

  // Try to close the split first
  let splitClosed = false;
  if (splitId !== null) {
    splitClosed = editor.closeSplit(splitId);
  }

  // If split couldn't be closed (only split), switch buffer back to source
  if (!splitClosed && splitId !== null && sourceBufferId !== null) {
    editor.setSplitBuffer(splitId, sourceBufferId);
  }

  // Always delete the virtual buffer completely (removes from all splits)
  if (bufferId !== null) {
    editor.closeBuffer(bufferId);
  }

  // Focus back on the source split
  if (sourceSplitId !== null) {
    editor.focusSplit(sourceSplitId);
  }

  editor.setStatus("Diagnostics panel closed");
};

globalThis.diagnostics_goto = function(): void {
  if (!state.isOpen || state.bufferId === null) return;

  const props = editor.getTextPropertiesAtCursor(state.bufferId);

  if (props.length > 0) {
    const location = props[0].location as { file: string; line: number; column: number } | undefined;
    if (location) {
      // Close panel first, then open file
      const file = location.file;
      const line = location.line;
      const col = location.column;

      globalThis.diagnostics_close();
      editor.openFile(file, line, col);
      editor.setStatus(`Jumped to ${editor.pathBasename(file)}:${line}`);
      return;
    }
  }

  editor.setStatus("Move cursor to a diagnostic line");
};

globalThis.diagnostics_toggle_all = function(): void {
  if (!state.isOpen) return;

  state.showAllFiles = !state.showAllFiles;
  updatePanel();

  const label = state.showAllFiles ? "All Files" : "Current File";
  editor.setStatus(`Showing: ${label}`);
};

globalThis.diagnostics_refresh = function(): void {
  if (!state.isOpen) return;

  updatePanel();
  editor.setStatus("Diagnostics refreshed");
};

globalThis.toggle_diagnostics_panel = function(): void {
  if (state.isOpen) {
    globalThis.diagnostics_close();
  } else {
    globalThis.show_diagnostics_panel();
  }
};

// =============================================================================
// Event Handlers
// =============================================================================

// Find the panel line that matches a source file location
function findPanelLineForLocation(file: string, sourceLine: number): number | null {
  // Find the first diagnostic on this source line for this file
  for (const mapping of state.lineMappings) {
    if (mapping.location.file === file && mapping.location.line === sourceLine) {
      return mapping.panelLine;
    }
  }
  return null;
}

// Convert a 1-based line number to byte offset in the cached content
function lineToByteOffset(lineNumber: number): number {
  const lines = state.cachedContent.split("\n");
  let offset = 0;
  for (let i = 0; i < lineNumber - 1 && i < lines.length; i++) {
    offset += lines[i].length + 1; // +1 for newline
  }
  return offset;
}

// Sync the panel cursor to match a source location
function syncPanelCursorToSourceLine(file: string, sourceLine: number): void {
  if (state.bufferId === null) return;

  const panelLine = findPanelLineForLocation(file, sourceLine);
  if (panelLine !== null) {
    // Convert panel line number to byte offset and move cursor
    const byteOffset = lineToByteOffset(panelLine);
    state.panelCursorLine = panelLine;
    editor.setBufferCursor(state.bufferId, byteOffset);
    applyHighlighting();
  }
}

globalThis.on_diagnostics_cursor_moved = function(data: {
  buffer_id: number;
  cursor_id: number;
  old_position: number;
  new_position: number;
  line: number;
}): void {
  if (!state.isOpen || state.bufferId === null) return;

  // If cursor moved in the diagnostics panel, update the tracked line and highlighting
  if (data.buffer_id === state.bufferId) {
    state.panelCursorLine = data.line;
    applyHighlighting();
    return;
  }

  // If cursor moved in the source buffer and panel is not focused, sync the panel cursor
  if (data.buffer_id === state.sourceBufferId && !state.panelFocused) {
    const path = editor.getBufferPath(data.buffer_id);
    if (path) {
      // Use the line number from the hook (1-indexed)
      syncPanelCursorToSourceLine(path, data.line);
    }
  }
};

globalThis.on_diagnostics_updated = function(_data: {
  uri: string;
  count: number;
}): void {
  if (!state.isOpen) return;
  updatePanel();
};

globalThis.on_diagnostics_buffer_activated = function(data: {
  buffer_id: number;
}): void {
  if (!state.isOpen) return;

  // If the diagnostics panel became active, mark as focused (freeze source buffer)
  if (data.buffer_id === state.bufferId) {
    state.panelFocused = true;
    return;
  }

  // If we're focusing a different buffer and were in the panel, mark as unfocused
  if (state.panelFocused) {
    state.panelFocused = false;
  }

  // Update source buffer and refresh the panel to show diagnostics for the new buffer
  state.sourceBufferId = data.buffer_id;
  updatePanel();
};

// Register event handlers
editor.on("cursor_moved", "on_diagnostics_cursor_moved");
editor.on("diagnostics_updated", "on_diagnostics_updated");
editor.on("buffer_activated", "on_diagnostics_buffer_activated");

// =============================================================================
// Command Registration
// =============================================================================

editor.registerCommand(
  "Show Diagnostics Panel",
  "Open the diagnostics panel",
  "show_diagnostics_panel",
  "normal"
);

editor.registerCommand(
  "Toggle Diagnostics Panel",
  "Toggle the diagnostics panel",
  "toggle_diagnostics_panel",
  "normal"
);

// =============================================================================
// Initialization
// =============================================================================

editor.setStatus("Diagnostics Panel plugin loaded");
editor.debug("Diagnostics Panel plugin initialized");
