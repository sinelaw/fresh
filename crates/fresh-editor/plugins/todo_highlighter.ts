/// <reference path="./lib/fresh.d.ts" />
// TypeScript TODO Highlighter Plugin
// Highlights TODO, FIXME, XXX keywords in source code
// Uses targeted overlay invalidation for efficient updates on edits
const editor = getEditor();


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

// Namespace for all TODO highlighter overlays
const NAMESPACE = "todo";

// Process a single line for keyword highlighting
function highlightLine(
  bufferId: number,
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

        // Add overlay with namespace for efficient batch removal
        editor.addOverlay(
          bufferId,
          NAMESPACE,
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

// Clear highlights for a buffer using namespace
function clearHighlights(bufferId: number): void {
  editor.clearNamespace(bufferId, NAMESPACE);
}

// Handle lines_changed events (batched for efficiency)
// This is called for lines that need (re)processing
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

  // Process all changed lines and create overlays for them
  for (const line of data.lines) {
    highlightLine(data.buffer_id, line.byte_start, line.content);
  }
};

// Handle buffer content changes - clear only affected overlays
// The editor will automatically re-send the affected lines via lines_changed
globalThis.onAfterInsert = function(data: {
  buffer_id: number;
  position: number;
  text: string;
  affected_start: number;
  affected_end: number;
}): void {
  if (!config.enabled) return;

  // Clear only overlays that overlap with the insertion range
  // These overlays may now span corrupted content (e.g., "TODO" -> "TOxDO")
  // The affected lines will be re-sent via lines_changed with correct content
  editor.clearOverlaysInRange(data.buffer_id, data.affected_start, data.affected_end);
};

globalThis.onAfterDelete = function(data: {
  buffer_id: number;
  start: number;
  end: number;
  deleted_text: string;
  affected_start: number;
  deleted_len: number;
}): void {
  if (!config.enabled) return;

  // Clear overlays that overlapped with the deleted range
  // Overlays that were entirely within the deleted range are already gone
  // (their markers were deleted), but overlays that spanned the deletion
  // boundary may now be incorrect
  // Use a slightly expanded range to catch boundary cases
  const clearStart = data.affected_start > 0 ? data.affected_start - 1 : 0;
  const clearEnd = data.affected_start + 1;
  editor.clearOverlaysInRange(data.buffer_id, clearStart, clearEnd);
};

// Handle buffer close events
globalThis.onBufferClosed = function(data: { buffer_id: number }): void {
  // No cleanup needed - overlays are automatically cleaned up with the buffer
};

// Register hooks
editor.on("lines_changed", "onLinesChanged");
editor.on("after_insert", "onAfterInsert");
editor.on("after_delete", "onAfterDelete");
editor.on("buffer_closed", "onBufferClosed");

// Plugin commands
globalThis.todoHighlighterEnable = function(): void {
  config.enabled = true;
  // Refresh lines so next render processes all visible lines
  const bufferId = editor.getActiveBufferId();
  editor.refreshLines(bufferId);
  editor.setStatus(editor.t("status.enabled"));
};

globalThis.todoHighlighterDisable = function(): void {
  config.enabled = false;
  const bufferId = editor.getActiveBufferId();
  clearHighlights(bufferId);
  editor.setStatus(editor.t("status.disabled"));
};

globalThis.todoHighlighterToggle = function(): void {
  config.enabled = !config.enabled;
  const bufferId = editor.getActiveBufferId();
  if (config.enabled) {
    // Refresh lines so next render processes all visible lines
    editor.refreshLines(bufferId);
  } else {
    clearHighlights(bufferId);
  }
  editor.setStatus(config.enabled ? editor.t("status.enabled") : editor.t("status.disabled"));
};

globalThis.todoHighlighterShowKeywords = function(): void {
  const keywords = config.keywords.map(k => k.word).join(", ");
  editor.setStatus(editor.t("status.keywords", { keywords }));
};

// Register commands
editor.registerCommand(
  "%cmd.enable",
  "%cmd.enable_desc",
  "todoHighlighterEnable",
  "normal"
);

editor.registerCommand(
  "%cmd.disable",
  "%cmd.disable_desc",
  "todoHighlighterDisable",
  "normal"
);

editor.registerCommand(
  "%cmd.toggle",
  "%cmd.toggle_desc",
  "todoHighlighterToggle",
  "normal"
);

editor.registerCommand(
  "%cmd.show_keywords",
  "%cmd.show_keywords_desc",
  "todoHighlighterShowKeywords",
  "normal"
);

// Initialization
editor.setStatus(editor.t("status.loaded"));
editor.debug("TODO Highlighter initialized with keywords: " + config.keywords.map(k => k.word).join(", "));
