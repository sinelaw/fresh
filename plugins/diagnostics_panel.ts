/// <reference path="./lib/fresh.d.ts" />

/**
 * Diagnostics Panel Plugin
 *
 * Uses the Finder abstraction with livePanel mode for reactive diagnostics display.
 * Supports toggling between current file and all files.
 *
 * Key features:
 * - livePanel mode for reactive data updates
 * - Toggle between current file and all files (press 'a')
 * - groupBy: "file" for organized display
 * - syncWithEditor for bidirectional cursor sync
 */

import { Finder, createLiveProvider, getRelativePath, type FinderProvider } from "./lib/finder.ts";

const editor = getEditor();

// Diagnostic item with severity
interface DiagnosticItem {
  uri: string;
  file: string;
  line: number;
  column: number;
  message: string;
  severity: number; // 1=error, 2=warning, 3=info, 4=hint
  source?: string;
}

// State
let showAllFiles = false;
let sourceBufferId: number | null = null;
let isOpen = false;

// Convert severity number to string
function severityToString(severity: number): "error" | "warning" | "info" | "hint" {
  switch (severity) {
    case 1:
      return "error";
    case 2:
      return "warning";
    case 3:
      return "info";
    case 4:
      return "hint";
    default:
      return "info";
  }
}

// Convert URI to file path
function uriToPath(uri: string): string {
  if (uri.startsWith("file://")) {
    return uri.slice(7);
  }
  return uri;
}

// Get diagnostics based on current filter
function getDiagnostics(): DiagnosticItem[] {
  const diagnostics = editor.getAllDiagnostics();

  // Get active file URI for filtering
  let activeUri: string | null = null;
  if (sourceBufferId !== null) {
    const path = editor.getBufferPath(sourceBufferId);
    if (path) {
      activeUri = "file://" + path;
    }
  }

  // Filter diagnostics
  const filterUri = showAllFiles ? null : activeUri;
  const filtered = filterUri
    ? diagnostics.filter((d) => d.uri === filterUri)
    : diagnostics;

  // Sort by file, then line, then severity
  filtered.sort((a, b) => {
    // File comparison
    if (a.uri !== b.uri) {
      // Active file first
      if (activeUri) {
        if (a.uri === activeUri) return -1;
        if (b.uri === activeUri) return 1;
      }
      return a.uri < b.uri ? -1 : 1;
    }
    // Line comparison
    const lineDiff = a.range.start.line - b.range.start.line;
    if (lineDiff !== 0) return lineDiff;
    // Severity comparison
    return a.severity - b.severity;
  });

  // Convert to DiagnosticItem
  return filtered.map((diag) => ({
    uri: diag.uri,
    file: uriToPath(diag.uri),
    line: diag.range.start.line + 1,
    column: diag.range.start.character + 1,
    message: diag.message.split("\n")[0], // First line only
    severity: diag.severity,
    source: diag.source ?? undefined,
  }));
}

// Create the live provider
const provider = createLiveProvider(getDiagnostics);

// Create the finder instance
const finder = new Finder<DiagnosticItem>(editor, {
  id: "diagnostics",
  format: (d) => ({
    label: `${d.line}:${d.column} ${d.message}`,
    location: {
      file: d.file,
      line: d.line,
      column: d.column,
    },
    severity: severityToString(d.severity),
    metadata: { uri: d.uri, message: d.message },
  }),
  groupBy: "file",
  syncWithEditor: true,
  onSelect: (d) => {
    const displayPath = getRelativePath(editor, d.file);
    editor.setStatus(
      editor.t("status.jumped_to", {
        file: displayPath,
        line: String(d.line),
      })
    );
  },
});

// Get title based on current filter state
function getTitle(): string {
  const filterLabel = showAllFiles
    ? editor.t("panel.all_files")
    : editor.t("panel.current_file");
  return editor.t("panel.header", { filter: filterLabel });
}

// Commands
globalThis.show_diagnostics_panel = async function (): Promise<void> {
  if (isOpen) {
    // Already open - just notify to refresh
    provider.notify();
    return;
  }

  // Capture source context
  sourceBufferId = editor.getActiveBufferId();

  // Show the panel
  await finder.livePanel({
    title: getTitle(),
    provider: provider as FinderProvider<DiagnosticItem>,
    ratio: 0.3,
  });

  isOpen = true;

  // Show count
  const diagnostics = editor.getAllDiagnostics();
  editor.setStatus(
    editor.t("status.diagnostics_count", { count: String(diagnostics.length) })
  );
};

globalThis.diagnostics_close = function (): void {
  finder.close();
  isOpen = false;
  sourceBufferId = null;
  editor.setStatus(editor.t("status.closed"));
};

globalThis.diagnostics_toggle_all = function (): void {
  if (!isOpen) return;

  showAllFiles = !showAllFiles;

  // Update and refresh
  finder.updateTitle(getTitle());
  provider.notify();

  const label = showAllFiles
    ? editor.t("panel.all_files")
    : editor.t("panel.current_file");
  editor.setStatus(editor.t("status.showing", { label }));
};

globalThis.diagnostics_refresh = function (): void {
  if (!isOpen) return;

  provider.notify();
  editor.setStatus(editor.t("status.refreshed"));
};

globalThis.toggle_diagnostics_panel = function (): void {
  if (isOpen) {
    globalThis.diagnostics_close();
  } else {
    globalThis.show_diagnostics_panel();
  }
};

// Event Handlers

// When diagnostics update, notify the provider
globalThis.on_diagnostics_updated = function (_data: {
  uri: string;
  count: number;
}): void {
  if (isOpen) {
    provider.notify();
  }
};

// When a different buffer becomes active, update filter context
globalThis.on_diagnostics_buffer_activated = function (data: {
  buffer_id: number;
}): void {
  if (!isOpen) return;

  // Update source buffer
  sourceBufferId = data.buffer_id;

  // Refresh if not showing all files
  if (!showAllFiles) {
    provider.notify();
    finder.updateTitle(getTitle());
  }
};

// Register event handlers
editor.on("diagnostics_updated", "on_diagnostics_updated");
editor.on("buffer_activated", "on_diagnostics_buffer_activated");

// Mode Definition (for custom keybindings beyond Enter/Escape)
editor.defineMode(
  "diagnostics-extra",
  "diagnostics-results",
  [
    ["a", "diagnostics_toggle_all"],
    ["r", "diagnostics_refresh"],
  ],
  true
);

// Command Registration
editor.registerCommand(
  "%cmd.show_diagnostics_panel",
  "%cmd.show_diagnostics_panel_desc",
  "show_diagnostics_panel",
  "normal"
);

editor.registerCommand(
  "%cmd.toggle_diagnostics_panel",
  "%cmd.toggle_diagnostics_panel_desc",
  "toggle_diagnostics_panel",
  "normal"
);

// Initialization
editor.setStatus(editor.t("status.loaded"));
editor.debug("Diagnostics Panel plugin initialized (using Finder abstraction)");
