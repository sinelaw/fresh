/// <reference path="../types/fresh.d.ts" />

import { PanelManager, NavigationController } from "./lib/index.ts";

/**
 * Diagnostics Panel Plugin (TypeScript)
 *
 * Full diagnostics panel implementation with virtual buffer split view.
 * Provides LSP-like diagnostics display with severity icons and navigation.
 */

// Diagnostic item structure
interface DiagnosticItem {
  severity: "error" | "warning" | "info" | "hint";
  message: string;
  file: string;
  line: number;
  column: number;
}

// Panel and navigation managers
const panel = new PanelManager("*Diagnostics*", "diagnostics-list");
const nav = new NavigationController<DiagnosticItem>({
  itemLabel: "Diagnostic",
  wrap: true,
  onSelectionChange: () => updatePanelContent(),
});

// Severity icons
const severityIcons: Record<string, string> = {
  error: "[E]",
  warning: "[W]",
  info: "[I]",
  hint: "[H]",
};

// Define the diagnostics mode with keybindings
editor.defineMode(
  "diagnostics-list",
  null, // no parent mode
  [
    ["Return", "diagnostics_goto"],
    ["n", "diagnostics_next"],
    ["p", "diagnostics_prev"],
    ["j", "diagnostics_next"],
    ["k", "diagnostics_prev"],
    ["q", "diagnostics_close"],
    ["Escape", "diagnostics_close"],
  ],
  true // read-only
);

// Format a diagnostic for display
function formatDiagnostic(item: DiagnosticItem, index: number): string {
  const icon = severityIcons[item.severity] || "[?]";
  const marker = index === nav.selectedIndex ? ">" : " ";
  return `${marker} ${icon} ${item.file}:${item.line}:${item.column} - ${item.message}\n`;
}

// Build entries for the virtual buffer
function buildPanelEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  const diagnostics = nav.getItems();

  // Header
  entries.push({
    text: "═══ LSP Diagnostics ═══\n",
    properties: { type: "header" },
  });

  if (diagnostics.length === 0) {
    entries.push({
      text: "  No diagnostics available\n",
      properties: { type: "empty" },
    });
  } else {
    // Add each diagnostic
    for (let i = 0; i < diagnostics.length; i++) {
      const diag = diagnostics[i];
      entries.push({
        text: formatDiagnostic(diag, i),
        properties: {
          type: "diagnostic",
          index: i,
          severity: diag.severity,
          location: {
            file: diag.file,
            line: diag.line,
            column: diag.column,
          },
        },
      });
    }
  }

  // Footer with summary
  const errorCount = diagnostics.filter((d) => d.severity === "error").length;
  const warningCount = diagnostics.filter((d) => d.severity === "warning").length;
  entries.push({
    text: `───────────────────────\n`,
    properties: { type: "separator" },
  });
  entries.push({
    text: `Total: ${errorCount} error(s), ${warningCount} warning(s)\n`,
    properties: { type: "summary" },
  });

  return entries;
}

// Update the panel content
function updatePanelContent(): void {
  panel.updateContent(buildPanelEntries());
}

// Generate sample diagnostics for the current file
function generateSampleDiagnostics(): DiagnosticItem[] {
  const bufferId = editor.getActiveBufferId();
  const filePath = editor.getBufferPath(bufferId);

  // Return sample diagnostics
  return [
    {
      severity: "error",
      message: "unused import",
      file: filePath || "unknown.rs",
      line: 1,
      column: 1,
    },
    {
      severity: "warning",
      message: "variable never used",
      file: filePath || "unknown.rs",
      line: 2,
      column: 5,
    },
    {
      severity: "info",
      message: "consider using pattern matching",
      file: filePath || "unknown.rs",
      line: 3,
      column: 10,
    },
  ];
}

// Show diagnostics panel
globalThis.show_diagnostics_panel = async function (): Promise<void> {
  if (panel.isOpen) {
    editor.setStatus("Diagnostics panel already open");
    updatePanelContent();
    return;
  }

  // Generate sample diagnostics and set up navigation
  const diagnostics = generateSampleDiagnostics();
  nav.setItems(diagnostics);

  // Build panel entries and open panel
  try {
    await panel.open({
      entries: buildPanelEntries(),
      ratio: 0.3,
    });

    editor.setStatus(`Diagnostics: ${diagnostics.length} item(s) - Press RET to jump, n/p to navigate, q to close`);
    editor.debug(`Diagnostics panel opened with buffer ID ${panel.bufferId}`);
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    editor.setStatus("Failed to open diagnostics panel");
    editor.debug(`ERROR: createVirtualBufferInSplit failed: ${errorMessage}`);
  }
};

// Hide diagnostics panel
globalThis.hide_diagnostics_panel = function (): void {
  if (!panel.isOpen) {
    editor.setStatus("Diagnostics panel not open");
    return;
  }

  panel.close();
  nav.reset();
  editor.setStatus("Diagnostics panel closed");
};

// Toggle diagnostics panel
globalThis.toggle_diagnostics_panel = function (): void {
  if (panel.isOpen) {
    globalThis.hide_diagnostics_panel();
  } else {
    globalThis.show_diagnostics_panel();
  }
};

// Show diagnostic count
globalThis.show_diagnostics_count = function (): void {
  const diagnostics = nav.getItems();
  const errorCount = diagnostics.filter((d) => d.severity === "error").length;
  const warningCount = diagnostics.filter((d) => d.severity === "warning").length;
  editor.setStatus(`Diagnostics: ${errorCount} errors, ${warningCount} warnings`);
};

// Navigation: go to selected diagnostic
globalThis.diagnostics_goto = function (): void {
  if (nav.isEmpty) {
    editor.setStatus("No diagnostics to jump to");
    return;
  }

  if (panel.sourceSplitId === null) {
    editor.setStatus("Source split not available");
    return;
  }

  const bufferId = panel.bufferId;
  if (bufferId === null) return;

  const props = editor.getTextPropertiesAtCursor(bufferId);

  if (props.length > 0) {
    const location = props[0].location as { file: string; line: number; column: number } | undefined;
    if (location) {
      // Open file in the source split, not the diagnostics split
      editor.openFileInSplit(panel.sourceSplitId, location.file, location.line, location.column || 0);
      editor.setStatus(`Jumped to ${location.file}:${location.line}`);
    } else {
      editor.setStatus("No location info for this diagnostic");
    }
  } else {
    // Fallback: use selected item from navigation
    const diag = nav.selected;
    if (diag) {
      editor.openFileInSplit(panel.sourceSplitId, diag.file, diag.line, diag.column);
      editor.setStatus(`Jumped to ${diag.file}:${diag.line}`);
    }
  }
};

// Navigation: next diagnostic
globalThis.diagnostics_next = function (): void {
  nav.next();
};

// Navigation: previous diagnostic
globalThis.diagnostics_prev = function (): void {
  nav.prev();
};

// Close the diagnostics panel
globalThis.diagnostics_close = function (): void {
  globalThis.hide_diagnostics_panel();
};

// Register commands
editor.registerCommand(
  "Show Diagnostics Panel",
  "Open the diagnostics panel",
  "show_diagnostics_panel",
  "normal"
);

editor.registerCommand(
  "Hide Diagnostics Panel",
  "Close the diagnostics panel",
  "hide_diagnostics_panel",
  "normal"
);

editor.registerCommand(
  "Toggle Diagnostics Panel",
  "Toggle diagnostics panel visibility",
  "toggle_diagnostics_panel",
  "normal"
);

editor.registerCommand(
  "Diagnostics Count",
  "Show count of current diagnostics",
  "show_diagnostics_count",
  "normal"
);

// Plugin initialization
editor.setStatus("Diagnostics Panel plugin loaded (TypeScript)");
editor.debug("Diagnostics Panel plugin initialized - 4 commands registered");
