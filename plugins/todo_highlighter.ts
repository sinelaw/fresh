// TypeScript TODO Highlighter Plugin
// Highlights TODO, FIXME, XXX keywords in source code
// This is a port of the Lua plugin to demonstrate the TypeScript plugin system

interface HighlightConfig {
  enabled: boolean;
  keywords: Array<{
    word: string;
    color: [number, number, number];
  }>;
}

// Plugin configuration
const config: HighlightConfig = {
  enabled: false,  // Start disabled, use Enable or Toggle to activate
  keywords: [
    { word: "TODO", color: [255, 200, 50] },     // Yellow
    { word: "FIXME", color: [255, 100, 100] },   // Red
    { word: "XXX", color: [255, 150, 50] },      // Orange
    { word: "HACK", color: [200, 100, 255] },    // Purple
    { word: "NOTE", color: [100, 200, 255] },    // Blue
  ],
};

// Track which buffers need their overlays refreshed (content changed)
const dirtyBuffers = new Set<number>();

// Process a single line for keyword highlighting
function highlightLine(
  bufferId: number,
  lineNumber: number,
  byteStart: number,
  content: string
): void {
  // Search for keywords
  for (const keyword of config.keywords) {
    let searchStart = 0;
    while (true) {
      const pos = content.indexOf(keyword.word, searchStart);
      if (pos === -1) break;

      // Check if it's a whole word (preceded by non-word char or start)
      const isWordStart = pos === 0 || !/\w/.test(content[pos - 1]);
      const isWordEnd = pos + keyword.word.length >= content.length ||
                        !/\w/.test(content[pos + keyword.word.length]);

      if (isWordStart && isWordEnd) {
        const absoluteStart = byteStart + pos;
        const absoluteEnd = absoluteStart + keyword.word.length;
        const overlayId = `todo-${bufferId}-${lineNumber}-${pos}`;

        editor.addOverlay(
          bufferId,
          overlayId,
          absoluteStart,
          absoluteEnd,
          keyword.color[0],
          keyword.color[1],
          keyword.color[2],
          false  // background color, not underline
        );
      }

      searchStart = pos + 1;
    }
  }
}

// Clear highlights for a buffer
function clearHighlights(bufferId: number): void {
  editor.removeOverlaysByPrefix(bufferId, "todo-");
}

// Handle render-start events (only clear overlays if buffer content changed)
globalThis.onRenderStart = function(data: { buffer_id: number }): void {
  if (!config.enabled) return;

  // Only clear and recreate overlays if the buffer content changed
  if (dirtyBuffers.has(data.buffer_id)) {
    clearHighlights(data.buffer_id);
    dirtyBuffers.delete(data.buffer_id);
  }
};

// Handle lines_changed events (batched for efficiency)
globalThis.onLinesChanged = function(data: {
  buffer_id: number;
  lines: Array<{
    line_number: number;
    byte_start: number;
    byte_end: number;
    content: string;
  }>;
}): void {
  if (!config.enabled) return;

  // Process all changed lines
  for (const line of data.lines) {
    highlightLine(data.buffer_id, line.line_number, line.byte_start, line.content);
  }
};

// Handle buffer content changes - mark buffer as needing overlay refresh
globalThis.onAfterInsert = function(data: { buffer_id: number }): void {
  dirtyBuffers.add(data.buffer_id);
};

globalThis.onAfterDelete = function(data: { buffer_id: number }): void {
  dirtyBuffers.add(data.buffer_id);
};

// Handle buffer close events
globalThis.onBufferClosed = function(data: { buffer_id: number }): void {
  dirtyBuffers.delete(data.buffer_id);
};

// Register hooks
editor.on("render_start", "onRenderStart");
editor.on("lines_changed", "onLinesChanged");
editor.on("after-insert", "onAfterInsert");
editor.on("after-delete", "onAfterDelete");
editor.on("buffer_closed", "onBufferClosed");

// Plugin commands
globalThis.todoHighlighterEnable = function(): void {
  config.enabled = true;
  // Clear seen_lines so next render processes all visible lines
  const bufferId = editor.getActiveBufferId();
  editor.refreshLines(bufferId);
  editor.setStatus("TODO Highlighter: Enabled");
};

globalThis.todoHighlighterDisable = function(): void {
  config.enabled = false;
  const bufferId = editor.getActiveBufferId();
  clearHighlights(bufferId);
  editor.setStatus("TODO Highlighter: Disabled");
};

globalThis.todoHighlighterToggle = function(): void {
  config.enabled = !config.enabled;
  const bufferId = editor.getActiveBufferId();
  if (config.enabled) {
    // Clear seen_lines so next render processes all visible lines
    editor.refreshLines(bufferId);
  } else {
    clearHighlights(bufferId);
  }
  editor.setStatus(`TODO Highlighter: ${config.enabled ? "Enabled" : "Disabled"}`);
};

globalThis.todoHighlighterShowKeywords = function(): void {
  const keywords = config.keywords.map(k => k.word).join(", ");
  editor.setStatus(`TODO Keywords: ${keywords}`);
};

// Register commands
editor.registerCommand(
  "TODO Highlighter: Enable",
  "Enable TODO keyword highlighting",
  "todoHighlighterEnable",
  "normal"
);

editor.registerCommand(
  "TODO Highlighter: Disable",
  "Disable TODO keyword highlighting",
  "todoHighlighterDisable",
  "normal"
);

editor.registerCommand(
  "TODO Highlighter: Toggle",
  "Toggle TODO keyword highlighting",
  "todoHighlighterToggle",
  "normal"
);

editor.registerCommand(
  "TODO Highlighter: Show Keywords",
  "Show currently tracked keywords",
  "todoHighlighterShowKeywords",
  "normal"
);

// Initialization
editor.setStatus("TODO Highlighter plugin loaded (TypeScript)");
editor.debug("TODO Highlighter initialized with keywords: " + config.keywords.map(k => k.word).join(", "));
