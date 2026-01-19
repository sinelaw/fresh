/// <reference path="./lib/fresh.d.ts" />

// TypeScript Color Highlighter Plugin
// Highlights color codes in source code with a colored swatch
// Supports: #RGB, #RRGGBB, rgb(), rgba(), hsl(), hsla(), Color::Rgb()
const editor = getEditor();


interface ColorHighlighterConfig {
  enabled: boolean;
}

// Plugin configuration
const config: ColorHighlighterConfig = {
  enabled: false, // Start disabled, use Enable or Toggle to activate
};

// Track which buffers need their virtual texts refreshed (content changed)
const dirtyBuffers = new Set<number>();

// Color block character for display
const COLOR_BLOCK = "█";

// Parse a hex color string to RGB
function parseHexColor(hex: string): [number, number, number] | null {
  // Remove # prefix
  hex = hex.replace(/^#/, "");

  let r: number, g: number, b: number;

  if (hex.length === 3) {
    // #RGB format
    r = parseInt(hex[0] + hex[0], 16);
    g = parseInt(hex[1] + hex[1], 16);
    b = parseInt(hex[2] + hex[2], 16);
  } else if (hex.length === 6 || hex.length === 8) {
    // #RRGGBB or #RRGGBBAA format
    r = parseInt(hex.substring(0, 2), 16);
    g = parseInt(hex.substring(2, 4), 16);
    b = parseInt(hex.substring(4, 6), 16);
  } else {
    return null;
  }

  if (isNaN(r) || isNaN(g) || isNaN(b)) {
    return null;
  }

  return [r, g, b];
}

// Parse rgb() or rgba() color to RGB
function parseRgbColor(match: string): [number, number, number] | null {
  const rgbMatch = match.match(/rgba?\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)/);
  if (!rgbMatch) {
    return null;
  }

  const r = parseInt(rgbMatch[1], 10);
  const g = parseInt(rgbMatch[2], 10);
  const b = parseInt(rgbMatch[3], 10);

  if (r > 255 || g > 255 || b > 255) {
    return null;
  }

  return [r, g, b];
}

// Convert HSL to RGB
function hslToRgb(h: number, s: number, l: number): [number, number, number] {
  // Normalize h to 0-360, s and l to 0-1
  h = h % 360;
  if (h < 0) h += 360;
  s = Math.max(0, Math.min(1, s / 100));
  l = Math.max(0, Math.min(1, l / 100));

  const c = (1 - Math.abs(2 * l - 1)) * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = l - c / 2;

  let r = 0, g = 0, b = 0;

  if (h >= 0 && h < 60) {
    r = c; g = x; b = 0;
  } else if (h >= 60 && h < 120) {
    r = x; g = c; b = 0;
  } else if (h >= 120 && h < 180) {
    r = 0; g = c; b = x;
  } else if (h >= 180 && h < 240) {
    r = 0; g = x; b = c;
  } else if (h >= 240 && h < 300) {
    r = x; g = 0; b = c;
  } else {
    r = c; g = 0; b = x;
  }

  return [
    Math.round((r + m) * 255),
    Math.round((g + m) * 255),
    Math.round((b + m) * 255),
  ];
}

// Parse hsl() or hsla() color to RGB
function parseHslColor(match: string): [number, number, number] | null {
  const hslMatch = match.match(/hsla?\s*\(\s*(-?\d+(?:\.\d+)?)\s*,\s*(\d+(?:\.\d+)?)%\s*,\s*(\d+(?:\.\d+)?)%/);
  if (!hslMatch) {
    return null;
  }

  const h = parseFloat(hslMatch[1]);
  const s = parseFloat(hslMatch[2]);
  const l = parseFloat(hslMatch[3]);

  return hslToRgb(h, s, l);
}

// Parse Rust Color::Rgb(r, g, b) to RGB
function parseRustRgbColor(match: string): [number, number, number] | null {
  const rustMatch = match.match(/Color::Rgb\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)/);
  if (!rustMatch) {
    return null;
  }

  const r = parseInt(rustMatch[1], 10);
  const g = parseInt(rustMatch[2], 10);
  const b = parseInt(rustMatch[3], 10);

  if (r > 255 || g > 255 || b > 255) {
    return null;
  }

  return [r, g, b];
}

// Color patterns to match
const colorPatterns = [
  {
    // Hex colors: #RGB, #RRGGBB, #RRGGBBAA
    regex: /#([0-9a-fA-F]{3}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})\b/g,
    parse: parseHexColor,
  },
  {
    // CSS rgb() and rgba()
    regex: /rgba?\s*\(\s*\d+\s*,\s*\d+\s*,\s*\d+\s*(?:,\s*[\d.]+\s*)?\)/g,
    parse: parseRgbColor,
  },
  {
    // CSS hsl() and hsla()
    regex: /hsla?\s*\(\s*-?\d+(?:\.\d+)?\s*,\s*\d+(?:\.\d+)?%\s*,\s*\d+(?:\.\d+)?%\s*(?:,\s*[\d.]+\s*)?\)/g,
    parse: parseHslColor,
  },
  {
    // Rust Color::Rgb(r, g, b)
    regex: /Color::Rgb\s*\(\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\)/g,
    parse: parseRustRgbColor,
  },
];

// Process a single line for color highlighting
function highlightLine(
  bufferId: number,
  lineNumber: number,
  byteStart: number,
  content: string
): void {
  // Search for color patterns
  for (const pattern of colorPatterns) {
    // Reset regex lastIndex
    pattern.regex.lastIndex = 0;

    let match;
    while ((match = pattern.regex.exec(content)) !== null) {
      const matchText = match[0];
      const pos = match.index;
      const color = pattern.parse(matchText);

      if (color) {
        const absolutePos = byteStart + pos;
        const virtualTextId = `color-${bufferId}-${lineNumber}-${pos}`;

        // Add virtual text with color swatch before the color code
        editor.addVirtualText(
          bufferId,
          virtualTextId,
          absolutePos,
          COLOR_BLOCK + " ",
          color[0],
          color[1],
          color[2],
          true, // before the character
          false // use_bg - use foreground color for the block character
        );
      }
    }
  }
}

// Clear color highlights for a buffer
function clearHighlights(bufferId: number): void {
  editor.removeVirtualTextsByPrefix(bufferId, "color-");
}

// Handle render-start events (only clear virtual texts if buffer content changed)
globalThis.onColorRenderStart = function(data: { buffer_id: number }): void {
  if (!config.enabled) return;

  // Only clear and recreate virtual texts if the buffer content changed
  if (dirtyBuffers.has(data.buffer_id)) {
    clearHighlights(data.buffer_id);
    dirtyBuffers.delete(data.buffer_id);
  }
};

// Handle lines_changed events (batched for efficiency)
globalThis.onColorLinesChanged = function(data: {
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

// Handle buffer content changes - mark buffer as needing virtual text refresh
globalThis.onColorAfterInsert = function(data: { buffer_id: number }): void {
  dirtyBuffers.add(data.buffer_id);
};

globalThis.onColorAfterDelete = function(data: { buffer_id: number }): void {
  dirtyBuffers.add(data.buffer_id);
};

// Handle buffer close events
globalThis.onColorBufferClosed = function(data: { buffer_id: number }): void {
  dirtyBuffers.delete(data.buffer_id);
};

// Register hooks
editor.on("render_start", "onColorRenderStart");
editor.on("lines_changed", "onColorLinesChanged");
editor.on("after_insert", "onColorAfterInsert");
editor.on("after_delete", "onColorAfterDelete");
editor.on("buffer_closed", "onColorBufferClosed");

// Plugin commands
globalThis.colorHighlighterEnable = function(): void {
  config.enabled = true;
  // Refresh lines so next render processes all visible lines
  const bufferId = editor.getActiveBufferId();
  editor.refreshLines(bufferId);
  editor.setStatus(editor.t("status.enabled"));
};

globalThis.colorHighlighterDisable = function(): void {
  config.enabled = false;
  const bufferId = editor.getActiveBufferId();
  clearHighlights(bufferId);
  editor.setStatus(editor.t("status.disabled"));
};

globalThis.colorHighlighterToggle = function(): void {
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

// Register commands
editor.registerCommand(
  "%cmd.enable",
  "%cmd.enable_desc",
  "colorHighlighterEnable",
  null
);

editor.registerCommand(
  "%cmd.disable",
  "%cmd.disable_desc",
  "colorHighlighterDisable",
  null
);

editor.registerCommand(
  "%cmd.toggle",
  "%cmd.toggle_desc",
  "colorHighlighterToggle",
  null
);

// Initialization
editor.debug("Color Highlighter initialized - supports hex, rgb, hsl, and Rust Color::Rgb");
