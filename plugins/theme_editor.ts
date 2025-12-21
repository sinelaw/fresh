/// <reference path="../types/fresh.d.ts" />

/**
 * Theme Editor Plugin - Interactive color theme editor
 *
 * Provides a visual interface for editing Fresh's color themes with:
 * - Organized display of all theme color fields by section
 * - Inline color swatches showing the actual colors
 * - Color picker supporting both RGB values and named colors
 * - Copy from built-in themes to use as starting point
 * - Save as new theme name
 * - Easy option to set as default theme
 */

// =============================================================================
// Types and Schema
// =============================================================================

type RGB = [number, number, number];

/**
 * Named colors supported by Fresh themes
 */
const NAMED_COLORS: Record<string, RGB> = {
  "Black": [0, 0, 0],
  "Red": [255, 0, 0],
  "Green": [0, 128, 0],
  "Yellow": [255, 255, 0],
  "Blue": [0, 0, 255],
  "Magenta": [255, 0, 255],
  "Cyan": [0, 255, 255],
  "Gray": [128, 128, 128],
  "DarkGray": [169, 169, 169],
  "LightRed": [255, 128, 128],
  "LightGreen": [144, 238, 144],
  "LightYellow": [255, 255, 224],
  "LightBlue": [173, 216, 230],
  "LightMagenta": [255, 128, 255],
  "LightCyan": [224, 255, 255],
  "White": [255, 255, 255],
};

/**
 * Special colors that use the terminal's default (preserves transparency)
 * These don't have RGB values - they tell the terminal to use its native color
 */
const SPECIAL_COLORS = ["Default", "Reset"];

const NAMED_COLOR_LIST = Object.keys(NAMED_COLORS);
const ALL_COLOR_NAMES = [...NAMED_COLOR_LIST, ...SPECIAL_COLORS];

/**
 * Color value - either RGB array or named color string
 */
type ColorValue = RGB | string;

/**
 * Theme section definition
 */
interface ThemeSection {
  name: string;
  displayName: string;
  description: string;
  fields: ThemeFieldDef[];
}

/**
 * Theme field definition with metadata
 */
interface ThemeFieldDef {
  key: string;
  displayName: string;
  description: string;
  section: string;
}

/**
 * Theme field with current value
 */
interface ThemeField {
  def: ThemeFieldDef;
  value: ColorValue;
  path: string;
  depth: number;
  isSection: boolean;
  expanded?: boolean;
}

// =============================================================================
// Theme Field Metadata
// =============================================================================

const THEME_SECTIONS: ThemeSection[] = [
  {
    name: "editor",
    displayName: "Editor",
    description: "Main editor area colors",
    fields: [
      { key: "bg", displayName: "Background", description: "Editor background color", section: "editor" },
      { key: "fg", displayName: "Foreground", description: "Default text color", section: "editor" },
      { key: "cursor", displayName: "Cursor", description: "Cursor color", section: "editor" },
      { key: "inactive_cursor", displayName: "Inactive Cursor", description: "Cursor color in unfocused splits", section: "editor" },
      { key: "selection_bg", displayName: "Selection Background", description: "Selected text background", section: "editor" },
      { key: "current_line_bg", displayName: "Current Line Background", description: "Background of the line containing cursor", section: "editor" },
      { key: "line_number_fg", displayName: "Line Number Foreground", description: "Line number text color", section: "editor" },
      { key: "line_number_bg", displayName: "Line Number Background", description: "Line number gutter background", section: "editor" },
    ],
  },
  {
    name: "ui",
    displayName: "UI Elements",
    description: "User interface colors (tabs, menus, status bar, etc.)",
    fields: [
      { key: "tab_active_fg", displayName: "Active Tab Foreground", description: "Active tab text color", section: "ui" },
      { key: "tab_active_bg", displayName: "Active Tab Background", description: "Active tab background color", section: "ui" },
      { key: "tab_inactive_fg", displayName: "Inactive Tab Foreground", description: "Inactive tab text color", section: "ui" },
      { key: "tab_inactive_bg", displayName: "Inactive Tab Background", description: "Inactive tab background color", section: "ui" },
      { key: "tab_separator_bg", displayName: "Tab Separator", description: "Tab bar separator color", section: "ui" },
      { key: "tab_close_hover_fg", displayName: "Tab Close Hover", description: "Tab close button hover color", section: "ui" },
      { key: "tab_hover_bg", displayName: "Tab Hover Background", description: "Tab hover background color", section: "ui" },
      { key: "menu_bg", displayName: "Menu Background", description: "Menu bar background", section: "ui" },
      { key: "menu_fg", displayName: "Menu Foreground", description: "Menu bar text color", section: "ui" },
      { key: "menu_active_bg", displayName: "Menu Active Background", description: "Active menu item background", section: "ui" },
      { key: "menu_active_fg", displayName: "Menu Active Foreground", description: "Active menu item text color", section: "ui" },
      { key: "menu_dropdown_bg", displayName: "Menu Dropdown Background", description: "Dropdown menu background", section: "ui" },
      { key: "menu_dropdown_fg", displayName: "Menu Dropdown Foreground", description: "Dropdown menu text color", section: "ui" },
      { key: "menu_highlight_bg", displayName: "Menu Highlight Background", description: "Highlighted menu item background", section: "ui" },
      { key: "menu_highlight_fg", displayName: "Menu Highlight Foreground", description: "Highlighted menu item text color", section: "ui" },
      { key: "menu_border_fg", displayName: "Menu Border", description: "Menu border color", section: "ui" },
      { key: "menu_separator_fg", displayName: "Menu Separator", description: "Menu separator line color", section: "ui" },
      { key: "menu_hover_bg", displayName: "Menu Hover Background", description: "Menu item hover background", section: "ui" },
      { key: "menu_hover_fg", displayName: "Menu Hover Foreground", description: "Menu item hover text color", section: "ui" },
      { key: "menu_disabled_fg", displayName: "Menu Disabled Foreground", description: "Disabled menu item text color", section: "ui" },
      { key: "menu_disabled_bg", displayName: "Menu Disabled Background", description: "Disabled menu item background", section: "ui" },
      { key: "status_bar_fg", displayName: "Status Bar Foreground", description: "Status bar text color", section: "ui" },
      { key: "status_bar_bg", displayName: "Status Bar Background", description: "Status bar background color", section: "ui" },
      { key: "prompt_fg", displayName: "Prompt Foreground", description: "Command prompt text color", section: "ui" },
      { key: "prompt_bg", displayName: "Prompt Background", description: "Command prompt background", section: "ui" },
      { key: "prompt_selection_fg", displayName: "Prompt Selection Foreground", description: "Prompt selected text color", section: "ui" },
      { key: "prompt_selection_bg", displayName: "Prompt Selection Background", description: "Prompt selection background", section: "ui" },
      { key: "popup_border_fg", displayName: "Popup Border", description: "Popup window border color", section: "ui" },
      { key: "popup_bg", displayName: "Popup Background", description: "Popup window background", section: "ui" },
      { key: "popup_selection_bg", displayName: "Popup Selection Background", description: "Popup selected item background", section: "ui" },
      { key: "popup_text_fg", displayName: "Popup Text Foreground", description: "Popup window text color", section: "ui" },
      { key: "suggestion_bg", displayName: "Suggestion Background", description: "Autocomplete suggestion background", section: "ui" },
      { key: "suggestion_selected_bg", displayName: "Suggestion Selected Background", description: "Selected suggestion background", section: "ui" },
      { key: "help_bg", displayName: "Help Background", description: "Help panel background", section: "ui" },
      { key: "help_fg", displayName: "Help Foreground", description: "Help panel text color", section: "ui" },
      { key: "help_key_fg", displayName: "Help Key Foreground", description: "Help keybinding text color", section: "ui" },
      { key: "help_separator_fg", displayName: "Help Separator", description: "Help panel separator color", section: "ui" },
      { key: "help_indicator_fg", displayName: "Help Indicator Foreground", description: "Help indicator text color", section: "ui" },
      { key: "help_indicator_bg", displayName: "Help Indicator Background", description: "Help indicator background", section: "ui" },
      { key: "inline_code_bg", displayName: "Inline Code Background", description: "Inline code block background", section: "ui" },
      { key: "split_separator_fg", displayName: "Split Separator", description: "Split pane separator color", section: "ui" },
      { key: "split_separator_hover_fg", displayName: "Split Separator Hover", description: "Split separator hover color", section: "ui" },
      { key: "scrollbar_track_fg", displayName: "Scrollbar Track", description: "Scrollbar track color", section: "ui" },
      { key: "scrollbar_thumb_fg", displayName: "Scrollbar Thumb", description: "Scrollbar thumb color", section: "ui" },
      { key: "scrollbar_track_hover_fg", displayName: "Scrollbar Track Hover", description: "Scrollbar track hover color", section: "ui" },
      { key: "scrollbar_thumb_hover_fg", displayName: "Scrollbar Thumb Hover", description: "Scrollbar thumb hover color", section: "ui" },
      { key: "compose_margin_bg", displayName: "Compose Margin Background", description: "Compose mode margin background", section: "ui" },
      { key: "semantic_highlight_bg", displayName: "Semantic Highlight Background", description: "Word under cursor highlight", section: "ui" },
      { key: "terminal_bg", displayName: "Terminal Background", description: "Embedded terminal background (use Default for transparency)", section: "ui" },
      { key: "terminal_fg", displayName: "Terminal Foreground", description: "Embedded terminal default text color", section: "ui" },
    ],
  },
  {
    name: "search",
    displayName: "Search",
    description: "Search result highlighting colors",
    fields: [
      { key: "match_bg", displayName: "Match Background", description: "Search match background color", section: "search" },
      { key: "match_fg", displayName: "Match Foreground", description: "Search match text color", section: "search" },
    ],
  },
  {
    name: "diagnostic",
    displayName: "Diagnostics",
    description: "LSP diagnostic colors (errors, warnings, etc.)",
    fields: [
      { key: "error_fg", displayName: "Error Foreground", description: "Error message text color", section: "diagnostic" },
      { key: "error_bg", displayName: "Error Background", description: "Error highlight background", section: "diagnostic" },
      { key: "warning_fg", displayName: "Warning Foreground", description: "Warning message text color", section: "diagnostic" },
      { key: "warning_bg", displayName: "Warning Background", description: "Warning highlight background", section: "diagnostic" },
      { key: "info_fg", displayName: "Info Foreground", description: "Info message text color", section: "diagnostic" },
      { key: "info_bg", displayName: "Info Background", description: "Info highlight background", section: "diagnostic" },
      { key: "hint_fg", displayName: "Hint Foreground", description: "Hint message text color", section: "diagnostic" },
      { key: "hint_bg", displayName: "Hint Background", description: "Hint highlight background", section: "diagnostic" },
    ],
  },
  {
    name: "syntax",
    displayName: "Syntax Highlighting",
    description: "Code syntax highlighting colors",
    fields: [
      { key: "keyword", displayName: "Keyword", description: "Language keywords (if, for, fn, etc.)", section: "syntax" },
      { key: "string", displayName: "String", description: "String literals", section: "syntax" },
      { key: "comment", displayName: "Comment", description: "Code comments", section: "syntax" },
      { key: "function", displayName: "Function", description: "Function names", section: "syntax" },
      { key: "type", displayName: "Type", description: "Type names", section: "syntax" },
      { key: "variable", displayName: "Variable", description: "Variable names", section: "syntax" },
      { key: "constant", displayName: "Constant", description: "Constants and literals", section: "syntax" },
      { key: "operator", displayName: "Operator", description: "Operators (+, -, =, etc.)", section: "syntax" },
    ],
  },
];

// =============================================================================
// State Management
// =============================================================================

interface ThemeEditorState {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null;
  sourceSplitId: number | null;
  sourceBufferId: number | null;
  /** Current theme data */
  themeData: Record<string, unknown>;
  /** Original theme data (for change detection) */
  originalThemeData: Record<string, unknown>;
  /** Theme name */
  themeName: string;
  /** Theme file path (null for new themes) */
  themePath: string | null;
  /** Expanded sections */
  expandedSections: Set<string>;
  /** Visible fields */
  visibleFields: ThemeField[];
  /** Selected field index */
  selectedIndex: number;
  /** Whether there are unsaved changes */
  hasChanges: boolean;
  /** Available built-in themes */
  builtinThemes: string[];
}

const state: ThemeEditorState = {
  isOpen: false,
  bufferId: null,
  splitId: null,
  sourceSplitId: null,
  sourceBufferId: null,
  themeData: {},
  originalThemeData: {},
  themeName: "custom",
  themePath: null,
  expandedSections: new Set(["editor", "syntax"]),
  visibleFields: [],
  selectedIndex: 0,
  hasChanges: false,
  builtinThemes: [],
};

// =============================================================================
// Color Definitions for UI
// =============================================================================

const colors = {
  sectionHeader: [255, 200, 100] as RGB,   // Gold
  fieldName: [200, 200, 255] as RGB,       // Light blue
  defaultValue: [150, 150, 150] as RGB,    // Gray
  customValue: [100, 255, 100] as RGB,     // Green
  description: [120, 120, 120] as RGB,     // Dim gray
  modified: [255, 255, 100] as RGB,        // Yellow
  footer: [100, 100, 100] as RGB,          // Gray
  colorBlock: [200, 200, 200] as RGB,      // Light gray for color swatch outline
};

// Color block character for swatches
const COLOR_BLOCK = "██";

// =============================================================================
// Mode Definition
// =============================================================================

editor.defineMode(
  "theme-editor",
  "normal",
  [
    ["Return", "theme_editor_edit_color"],
    ["Space", "theme_editor_edit_color"],
    ["Tab", "theme_editor_toggle_section"],
    ["c", "theme_editor_copy_from_builtin"],
    ["n", "theme_editor_set_name"],
    ["s", "theme_editor_save"],
    ["S", "theme_editor_save_as"],
    ["d", "theme_editor_set_as_default"],
    ["q", "theme_editor_close"],
    ["Escape", "theme_editor_close"],
    ["r", "theme_editor_reload"],
    ["?", "theme_editor_show_help"],
  ],
  true // read-only
);

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Calculate UTF-8 byte length of a string
 */
function getUtf8ByteLength(str: string): number {
  let length = 0;
  for (let i = 0; i < str.length; i++) {
    const code = str.charCodeAt(i);
    if (code < 0x80) {
      length += 1;
    } else if (code < 0x800) {
      length += 2;
    } else if (code < 0xD800 || code >= 0xE000) {
      length += 3;
    } else {
      i++;
      length += 4;
    }
  }
  return length;
}

/**
 * Deep clone an object
 */
function deepClone<T>(obj: T): T {
  return JSON.parse(JSON.stringify(obj));
}

/**
 * Check if two values are deeply equal
 */
function deepEqual(a: unknown, b: unknown): boolean {
  return JSON.stringify(a) === JSON.stringify(b);
}

/**
 * Parse a color value to RGB
 */
function parseColorToRgb(value: ColorValue): RGB | null {
  if (Array.isArray(value) && value.length === 3) {
    return value as RGB;
  }
  if (typeof value === "string") {
    return NAMED_COLORS[value] || null;
  }
  return null;
}

/**
 * Convert RGB to hex string
 */
function rgbToHex(r: number, g: number, b: number): string {
  const toHex = (n: number) => n.toString(16).padStart(2, '0').toUpperCase();
  return `#${toHex(r)}${toHex(g)}${toHex(b)}`;
}

/**
 * Parse hex string to RGB
 */
function hexToRgb(hex: string): RGB | null {
  const match = hex.match(/^#?([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})$/);
  if (match) {
    return [
      parseInt(match[1], 16),
      parseInt(match[2], 16),
      parseInt(match[3], 16),
    ];
  }
  return null;
}

/**
 * Format a color value for display (as hex)
 */
function formatColorValue(value: ColorValue): string {
  if (Array.isArray(value)) {
    return rgbToHex(value[0], value[1], value[2]);
  }
  return String(value);
}

/**
 * Check if a color is a named color (including special colors like Default/Reset)
 */
function isNamedColor(value: ColorValue): boolean {
  return typeof value === "string" && (value in NAMED_COLORS || SPECIAL_COLORS.includes(value));
}

/**
 * Get nested value from object
 */
function getNestedValue(obj: Record<string, unknown>, path: string): unknown {
  const parts = path.split(".");
  let current: unknown = obj;
  for (const part of parts) {
    if (current === null || current === undefined) return undefined;
    current = (current as Record<string, unknown>)[part];
  }
  return current;
}

/**
 * Set nested value in object
 */
function setNestedValue(obj: Record<string, unknown>, path: string, value: unknown): void {
  const parts = path.split(".");
  let current: Record<string, unknown> = obj;
  for (let i = 0; i < parts.length - 1; i++) {
    const part = parts[i];
    if (!(part in current) || typeof current[part] !== "object") {
      current[part] = {};
    }
    current = current[part] as Record<string, unknown>;
  }
  current[parts[parts.length - 1]] = value;
}

/**
 * Find themes directory
 */
function findThemesDir(): string {
  const cwd = editor.getCwd();
  const candidates = [
    editor.pathJoin(cwd, "themes"),
  ];

  for (const path of candidates) {
    if (editor.fileExists(path)) {
      return path;
    }
  }

  return candidates[0];
}

/**
 * Load list of available built-in themes
 */
async function loadBuiltinThemes(): Promise<string[]> {
  const themesDir = findThemesDir();
  try {
    const entries = editor.readDir(themesDir);
    return entries
      .filter(e => e.is_file && e.name.endsWith(".json"))
      .map(e => e.name.replace(".json", ""));
  } catch {
    return ["dark", "light", "high-contrast", "dracula", "nord", "solarized-dark"];
  }
}

/**
 * Load a theme file
 */
async function loadThemeFile(name: string): Promise<Record<string, unknown> | null> {
  const themesDir = findThemesDir();
  const themePath = editor.pathJoin(themesDir, `${name}.json`);

  try {
    const content = await editor.readFile(themePath);
    return JSON.parse(content);
  } catch {
    editor.debug(`Failed to load theme: ${name}`);
    return null;
  }
}

/**
 * Get user themes directory
 * Uses XDG_CONFIG_HOME if set, otherwise falls back to $HOME/.config
 */
function getUserThemesDir(): string {
  // Check XDG_CONFIG_HOME first (standard on Linux)
  const xdgConfig = editor.getEnv("XDG_CONFIG_HOME");
  if (xdgConfig) {
    return editor.pathJoin(xdgConfig, "fresh", "themes");
  }

  // Fall back to $HOME/.config
  const home = editor.getEnv("HOME");
  if (home) {
    return editor.pathJoin(home, ".config", "fresh", "themes");
  }

  return editor.pathJoin(editor.getCwd(), "themes");
}

// =============================================================================
// Field Building
// =============================================================================

/**
 * Build visible fields list based on expanded sections
 */
function buildVisibleFields(): ThemeField[] {
  const fields: ThemeField[] = [];

  for (const section of THEME_SECTIONS) {
    const expanded = state.expandedSections.has(section.name);

    // Section header
    fields.push({
      def: {
        key: section.name,
        displayName: section.displayName,
        description: section.description,
        section: section.name,
      },
      value: [0, 0, 0], // Placeholder
      path: section.name,
      depth: 0,
      isSection: true,
      expanded,
    });

    // Section fields
    if (expanded) {
      for (const fieldDef of section.fields) {
        const path = `${section.name}.${fieldDef.key}`;
        const value = getNestedValue(state.themeData, path) as ColorValue || [128, 128, 128];

        fields.push({
          def: fieldDef,
          value,
          path,
          depth: 1,
          isSection: false,
        });
      }
    }
  }

  return fields;
}

// =============================================================================
// UI Building
// =============================================================================

/**
 * Build display entries for virtual buffer
 */
function buildDisplayEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Title
  const modifiedMarker = state.hasChanges ? " [modified]" : "";
  entries.push({
    text: `━━━ Theme Editor: ${state.themeName}${modifiedMarker} ━━━\n`,
    properties: { type: "title" },
  });

  if (state.themePath) {
    entries.push({
      text: `File: ${state.themePath}\n`,
      properties: { type: "file-path" },
    });
  } else {
    entries.push({
      text: "New theme (not yet saved)\n",
      properties: { type: "file-path" },
    });
  }

  entries.push({
    text: "\n",
    properties: { type: "blank" },
  });

  // Fields
  state.visibleFields = buildVisibleFields();

  for (let i = 0; i < state.visibleFields.length; i++) {
    const field = state.visibleFields[i];
    const indent = "  ".repeat(field.depth);

    if (field.isSection) {
      // Section header
      const icon = field.expanded ? "▼" : ">";
      entries.push({
        text: `${indent}${icon} ${field.def.displayName}\n`,
        properties: {
          type: "section",
          path: field.path,
          index: i,
          expanded: field.expanded,
        },
      });

      // Section description
      entries.push({
        text: `${indent}  // ${field.def.description}\n`,
        properties: { type: "description", path: field.path },
      });
    } else {
      // Field description (before the field)
      entries.push({
        text: `${indent}    // ${field.def.description}\n`,
        properties: { type: "description", path: field.path },
      });

      // Color field with swatch
      const colorStr = formatColorValue(field.value);

      entries.push({
        text: `${indent}  ${field.def.displayName}: ${colorStr}\n`,
        properties: {
          type: "field",
          path: field.path,
          index: i,
          colorValue: field.value,
        },
      });
    }

    entries.push({
      text: "\n",
      properties: { type: "blank" },
    });
  }

  // Footer
  entries.push({
    text: "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
    properties: { type: "separator" },
  });
  entries.push({
    text: "↑/↓: navigate | RET/SPC: edit color | TAB: expand/collapse\n",
    properties: { type: "footer" },
  });
  entries.push({
    text: "c: copy from builtin | n: set name | s: save | S: save as | d: set as default | q: quit\n",
    properties: { type: "footer" },
  });

  return entries;
}

/**
 * Helper to add a colored overlay
 */
function addColorOverlay(
  bufferId: number,
  start: number,
  end: number,
  color: RGB,
  bold: boolean = false
): void {
  editor.addOverlay(bufferId, "theme", start, end, color[0], color[1], color[2], false, bold, false);
}

/**
 * Check if a field path represents a background color
 */
function isBackgroundColorField(path: string): boolean {
  // Check if path ends with .bg or contains _bg
  // e.g., "editor.bg", "editor.selection_bg", "ui.tab_active_bg"
  return path.endsWith(".bg") || path.includes("_bg");
}

/**
 * Check if a color is a special color (Default/Reset)
 */
function isSpecialColor(value: ColorValue): boolean {
  return typeof value === "string" && SPECIAL_COLORS.includes(value);
}

/**
 * Add color swatches using virtual text
 */
function addColorSwatches(): void {
  if (state.bufferId === null) return;

  // Clear existing swatches
  editor.removeVirtualTextsByPrefix(state.bufferId, "theme-swatch-");

  const entries = buildDisplayEntries();
  let byteOffset = 0;

  for (const entry of entries) {
    const props = entry.properties as Record<string, unknown>;

    if (props.type === "field" && props.colorValue) {
      const colorValue = props.colorValue as ColorValue;
      const path = props.path as string;

      // Find position after the field name colon
      const colonIdx = entry.text.indexOf(":");
      if (colonIdx >= 0) {
        const swatchPos = byteOffset + getUtf8ByteLength(entry.text.substring(0, colonIdx + 2));
        const swatchId = `theme-swatch-${path}`;

        if (isSpecialColor(colorValue)) {
          // For Default/Reset, show a placeholder indicator
          editor.addVirtualText(
            state.bufferId,
            swatchId,
            swatchPos,
            "∅ ",  // Empty set symbol to indicate "use default"
            150,   // Gray color for the indicator
            150,
            150,
            true,
            false
          );
        } else {
          const rgb = parseColorToRgb(colorValue);
          if (rgb) {
            const useBg = isBackgroundColorField(path);

            // Add swatch with a trailing space included in the text
            editor.addVirtualText(
              state.bufferId,
              swatchId,
              swatchPos,
              useBg ? "   " : COLOR_BLOCK + " ",  // Include trailing space in swatch text
              rgb[0],
              rgb[1],
              rgb[2],
              true,
              useBg  // use as background color
            );
          }
        }
      }
    }

    byteOffset += getUtf8ByteLength(entry.text);
  }
}

/**
 * Apply syntax highlighting
 */
function applyHighlighting(): void {
  if (state.bufferId === null) return;

  const bufferId = state.bufferId;
  editor.clearNamespace(bufferId, "theme");

  const entries = buildDisplayEntries();
  let byteOffset = 0;

  for (const entry of entries) {
    const text = entry.text;
    const textLen = getUtf8ByteLength(text);
    const props = entry.properties as Record<string, unknown>;
    const entryType = props.type as string;

    if (entryType === "title") {
      addColorOverlay(bufferId, byteOffset, byteOffset + textLen, colors.sectionHeader, true);
    } else if (entryType === "file-path") {
      addColorOverlay(bufferId, byteOffset, byteOffset + textLen, colors.description);
    } else if (entryType === "description") {
      addColorOverlay(bufferId, byteOffset, byteOffset + textLen, colors.description);
    } else if (entryType === "section") {
      addColorOverlay(bufferId, byteOffset, byteOffset + textLen, colors.sectionHeader, true);
    } else if (entryType === "field") {
      // Field name - light blue
      const colonPos = text.indexOf(":");
      if (colonPos > 0) {
        const nameEnd = byteOffset + getUtf8ByteLength(text.substring(0, colonPos));
        addColorOverlay(bufferId, byteOffset, nameEnd, colors.fieldName);

        // Value - custom color (green)
        const valueStart = nameEnd + getUtf8ByteLength(": ");
        addColorOverlay(bufferId, valueStart, byteOffset + textLen, colors.customValue);
      }
    } else if (entryType === "separator" || entryType === "footer") {
      addColorOverlay(bufferId, byteOffset, byteOffset + textLen, colors.footer);
    }

    byteOffset += textLen;
  }

  // Add color swatches
  addColorSwatches();
}

/**
 * Update display
 */
function updateDisplay(): void {
  if (state.bufferId === null) return;

  const entries = buildDisplayEntries();
  editor.setVirtualBufferContent(state.bufferId, entries);
  applyHighlighting();
}

// =============================================================================
// Field Editing
// =============================================================================

/**
 * Get field at cursor position
 */
function getFieldAtCursor(): ThemeField | null {
  if (state.bufferId === null) return null;

  const props = editor.getTextPropertiesAtCursor(state.bufferId);
  if (props.length > 0 && typeof props[0].index === "number") {
    const index = props[0].index as number;
    if (index >= 0 && index < state.visibleFields.length) {
      return state.visibleFields[index];
    }
  }

  return null;
}

/**
 * Start color editing prompt
 */
function editColorField(field: ThemeField): void {
  const currentValue = formatColorValue(field.value);

  // Use startPromptWithInitial to pre-fill with current value
  editor.startPromptWithInitial(`${field.def.displayName} (#RRGGBB or named): `, `theme-color-${field.path}`, currentValue);

  // Build suggestions with named colors and current value
  const suggestions: PromptSuggestion[] = [
    {
      text: currentValue,
      description: "(current)",
      value: currentValue,
    },
  ];

  // Add special colors first (Default/Reset for terminal transparency)
  for (const name of SPECIAL_COLORS) {
    suggestions.push({
      text: name,
      description: "Use terminal's native color",
      value: name,
    });
  }

  // Add named colors as suggestions with hex format
  for (const name of NAMED_COLOR_LIST) {
    const rgb = NAMED_COLORS[name];
    const hexValue = rgbToHex(rgb[0], rgb[1], rgb[2]);
    suggestions.push({
      text: name,
      description: hexValue,
      value: name,
    });
  }

  editor.setPromptSuggestions(suggestions);
}

/**
 * Parse color input from user
 */
function parseColorInput(input: string): ColorValue | null {
  input = input.trim();

  // Check for special colors (Default/Reset - use terminal's native color)
  if (SPECIAL_COLORS.includes(input)) {
    return input;
  }

  // Check for named color
  if (input in NAMED_COLORS) {
    return input;
  }

  // Try to parse as hex color #RRGGBB
  const hexResult = hexToRgb(input);
  if (hexResult) {
    return hexResult;
  }

  // Try to parse as RGB array [r, g, b]
  const rgbMatch = input.match(/^\[?\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\]?$/);
  if (rgbMatch) {
    const r = parseInt(rgbMatch[1], 10);
    const g = parseInt(rgbMatch[2], 10);
    const b = parseInt(rgbMatch[3], 10);

    if (r >= 0 && r <= 255 && g >= 0 && g <= 255 && b >= 0 && b <= 255) {
      return [r, g, b];
    }
  }

  return null;
}

// =============================================================================
// Prompt Handlers
// =============================================================================

/**
 * Handle color prompt confirmation
 */
globalThis.onThemeColorPromptConfirmed = function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (!args.prompt_type.startsWith("theme-color-")) return true;

  const path = args.prompt_type.replace("theme-color-", "");
  const newValue = parseColorInput(args.input);

  if (newValue !== null) {
    setNestedValue(state.themeData, path, newValue);
    state.hasChanges = !deepEqual(state.themeData, state.originalThemeData);
    updateDisplay();
    editor.setStatus(`Updated ${path}`);
  } else {
    editor.setStatus("Invalid color format. Use #RRGGBB, [r,g,b], or named color.");
  }

  return true;
};

/**
 * Handle theme name prompt
 */
globalThis.onThemeNamePromptConfirmed = function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (args.prompt_type !== "theme-name") return true;

  const name = args.input.trim();
  if (name) {
    state.themeName = name;
    state.themeData.name = name;
    state.hasChanges = true;
    updateDisplay();
    editor.setStatus(`Theme name set to: ${name}`);
  }

  return true;
};

/**
 * Handle copy from builtin prompt
 */
globalThis.onThemeCopyPromptConfirmed = async function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "theme-copy-builtin") return true;

  const themeName = args.input.trim();
  const themeData = await loadThemeFile(themeName);

  if (themeData) {
    state.themeData = deepClone(themeData);
    state.themeName = `${themeName}-custom`;
    state.themeData.name = state.themeName;
    state.themePath = null; // New theme, not saved yet
    state.hasChanges = true;
    updateDisplay();
    editor.setStatus(`Copied from ${themeName}. Edit and save as new theme.`);
  } else {
    editor.setStatus(`Failed to load theme: ${themeName}`);
  }

  return true;
};

/**
 * Handle save as prompt
 */
globalThis.onThemeSaveAsPromptConfirmed = async function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "theme-save-as") return true;

  const name = args.input.trim();
  if (name) {
    state.themeName = name;
    state.themeData.name = name;
    await saveTheme(name);
  }

  return true;
};

/**
 * Handle set as default prompt
 */
globalThis.onThemeSetDefaultPromptConfirmed = async function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "theme-set-default") return true;

  const themeName = args.input.trim();
  if (themeName) {
    await setThemeAsDefault(themeName);
  }

  return true;
};

/**
 * Handle prompt cancellation
 */
globalThis.onThemePromptCancelled = function(args: { prompt_type: string }): boolean {
  if (!args.prompt_type.startsWith("theme-")) return true;
  editor.setStatus("Cancelled");
  return true;
};

// Register prompt handlers
editor.on("prompt_confirmed", "onThemeColorPromptConfirmed");
editor.on("prompt_confirmed", "onThemeNamePromptConfirmed");
editor.on("prompt_confirmed", "onThemeCopyPromptConfirmed");
editor.on("prompt_confirmed", "onThemeSaveAsPromptConfirmed");
editor.on("prompt_confirmed", "onThemeSetDefaultPromptConfirmed");
editor.on("prompt_cancelled", "onThemePromptCancelled");

// =============================================================================
// Theme Operations
// =============================================================================

/**
 * Save theme to file
 */
async function saveTheme(name?: string): Promise<boolean> {
  const themeName = name || state.themeName;
  const userThemesDir = getUserThemesDir();

  // Ensure themes directory exists
  if (!editor.fileExists(userThemesDir)) {
    try {
      // Create directory via shell command
      await editor.spawnProcess("mkdir", ["-p", userThemesDir]);
    } catch (e) {
      editor.setStatus(`Failed to create themes directory: ${e}`);
      return false;
    }
  }

  const themePath = editor.pathJoin(userThemesDir, `${themeName}.json`);

  try {
    state.themeData.name = themeName;
    const content = JSON.stringify(state.themeData, null, 2);
    await editor.writeFile(themePath, content);

    state.themePath = themePath;
    state.themeName = themeName;
    state.originalThemeData = deepClone(state.themeData);
    state.hasChanges = false;
    updateDisplay();

    editor.setStatus(`Theme saved to ${themePath}`);
    return true;
  } catch (e) {
    editor.setStatus(`Failed to save theme: ${e}`);
    return false;
  }
}

/**
 * Set a theme as the default in config and apply it immediately
 */
async function setThemeAsDefault(themeName: string): Promise<void> {
  try {
    // Use the editor API to apply and persist the theme
    editor.applyTheme(themeName);
    editor.setStatus(`Theme "${themeName}" applied and saved as default.`);
  } catch (e) {
    editor.setStatus(`Failed to apply theme: ${e}`);
  }
}

/**
 * Create a default/empty theme
 */
function createDefaultTheme(): Record<string, unknown> {
  return {
    name: "custom",
    editor: {
      bg: [30, 30, 30],
      fg: [212, 212, 212],
      cursor: [82, 139, 255],
      inactive_cursor: [100, 100, 100],
      selection_bg: [38, 79, 120],
      current_line_bg: [40, 40, 40],
      line_number_fg: [100, 100, 100],
      line_number_bg: [30, 30, 30],
    },
    ui: {
      tab_active_fg: "Yellow",
      tab_active_bg: "Blue",
      tab_inactive_fg: "White",
      tab_inactive_bg: "DarkGray",
      tab_separator_bg: "Black",
      status_bar_fg: "White",
      status_bar_bg: "DarkGray",
      prompt_fg: "White",
      prompt_bg: "Black",
      prompt_selection_fg: "White",
      prompt_selection_bg: [58, 79, 120],
      popup_border_fg: "Gray",
      popup_bg: [30, 30, 30],
      popup_selection_bg: [58, 79, 120],
      popup_text_fg: "White",
      suggestion_bg: [30, 30, 30],
      suggestion_selected_bg: [58, 79, 120],
      help_bg: "Black",
      help_fg: "White",
      help_key_fg: "Cyan",
      help_separator_fg: "DarkGray",
      help_indicator_fg: "Red",
      help_indicator_bg: "Black",
      split_separator_fg: [100, 100, 100],
      terminal_bg: "Default",
      terminal_fg: "Default",
    },
    search: {
      match_bg: [100, 100, 20],
      match_fg: [255, 255, 255],
    },
    diagnostic: {
      error_fg: "Red",
      error_bg: [60, 20, 20],
      warning_fg: "Yellow",
      warning_bg: [60, 50, 0],
      info_fg: "Blue",
      info_bg: [0, 30, 60],
      hint_fg: "Gray",
      hint_bg: [30, 30, 30],
    },
    syntax: {
      keyword: [86, 156, 214],
      string: [206, 145, 120],
      comment: [106, 153, 85],
      function: [220, 220, 170],
      type: [78, 201, 176],
      variable: [156, 220, 254],
      constant: [79, 193, 255],
      operator: [212, 212, 212],
    },
  };
}

// =============================================================================
// Cursor Movement Handler
// =============================================================================

globalThis.onThemeEditorCursorMoved = function(data: {
  buffer_id: number;
  cursor_id: number;
  old_position: number;
  new_position: number;
}): void {
  if (state.bufferId === null || data.buffer_id !== state.bufferId) return;

  applyHighlighting();

  const field = getFieldAtCursor();
  if (field) {
    editor.setStatus(field.def.description);
  }
};

editor.on("cursor_moved", "onThemeEditorCursorMoved");

// =============================================================================
// Public Commands
// =============================================================================

/**
 * Open the theme editor
 */
globalThis.open_theme_editor = async function(): Promise<void> {
  if (state.isOpen) {
    editor.setStatus("Theme editor already open");
    return;
  }

  editor.setStatus("Loading theme editor...");

  // Save context
  state.sourceSplitId = editor.getActiveSplitId();
  state.sourceBufferId = editor.getActiveBufferId();

  // Load available themes
  state.builtinThemes = await loadBuiltinThemes();

  // Create default theme data
  state.themeData = createDefaultTheme();
  state.originalThemeData = deepClone(state.themeData);
  state.themeName = "custom";
  state.themePath = null;
  state.hasChanges = false;

  // Build initial entries
  const entries = buildDisplayEntries();

  // Create virtual buffer in current split (no new split)
  const bufferId = await editor.createVirtualBuffer({
    name: "*Theme Editor*",
    mode: "theme-editor",
    read_only: true,
    entries: entries,
    show_line_numbers: false,
    show_cursors: true,
    editing_disabled: true,
  });

  if (bufferId !== null) {
    state.isOpen = true;
    state.bufferId = bufferId;
    state.splitId = null;

    editor.setContext("theme-editor", true);

    applyHighlighting();
    editor.setStatus("Theme Editor | c: copy from builtin | s: save | d: set as default | q: quit");
  } else {
    editor.setStatus("Failed to open theme editor");
  }
};

/**
 * Close the theme editor
 */
globalThis.theme_editor_close = function(): void {
  if (!state.isOpen) return;

  if (state.hasChanges) {
    editor.setStatus("Warning: Unsaved changes discarded");
  }

  editor.setContext("theme-editor", false);

  // Close the buffer (this will switch to another buffer in the same split)
  if (state.bufferId !== null) {
    editor.closeBuffer(state.bufferId);
  }

  // Reset state
  state.isOpen = false;
  state.bufferId = null;
  state.splitId = null;
  state.themeData = {};
  state.originalThemeData = {};
  state.hasChanges = false;

  editor.setStatus("Theme editor closed");
};

/**
 * Edit color at cursor
 */
globalThis.theme_editor_edit_color = function(): void {
  const field = getFieldAtCursor();
  if (!field) {
    editor.setStatus("No field selected");
    return;
  }

  if (field.isSection) {
    theme_editor_toggle_section();
    return;
  }

  editColorField(field);
};

/**
 * Toggle section expansion
 */
globalThis.theme_editor_toggle_section = function(): void {
  const field = getFieldAtCursor();
  if (!field || !field.isSection) {
    editor.setStatus("Not a section");
    return;
  }

  if (state.expandedSections.has(field.path)) {
    state.expandedSections.delete(field.path);
  } else {
    state.expandedSections.add(field.path);
  }

  updateDisplay();
};

/**
 * Copy from a built-in theme
 */
globalThis.theme_editor_copy_from_builtin = function(): void {
  editor.startPrompt("Copy from theme: ", "theme-copy-builtin");

  const suggestions: PromptSuggestion[] = state.builtinThemes.map(name => ({
    text: name,
    description: "Built-in theme",
    value: name,
  }));

  editor.setPromptSuggestions(suggestions);
};

/**
 * Set theme name
 */
globalThis.theme_editor_set_name = function(): void {
  editor.startPrompt("Theme name: ", "theme-name");

  editor.setPromptSuggestions([{
    text: state.themeName,
    description: "(current)",
    value: state.themeName,
  }]);
};

/**
 * Save theme
 */
globalThis.theme_editor_save = async function(): Promise<void> {
  if (!state.hasChanges && state.themePath) {
    editor.setStatus("No changes to save");
    return;
  }

  await saveTheme();
};

/**
 * Save theme as (new name)
 */
globalThis.theme_editor_save_as = function(): void {
  editor.startPrompt("Save theme as: ", "theme-save-as");

  editor.setPromptSuggestions([{
    text: state.themeName,
    description: "(current)",
    value: state.themeName,
  }]);
};

/**
 * Set current theme as default
 */
globalThis.theme_editor_set_as_default = function(): void {
  editor.startPrompt("Set default theme: ", "theme-set-default");

  // Suggest current theme and all builtins
  const suggestions: PromptSuggestion[] = [];

  if (state.themeName && state.themePath) {
    suggestions.push({
      text: state.themeName,
      description: "(current)",
      value: state.themeName,
    });
  }

  for (const name of state.builtinThemes) {
    suggestions.push({
      text: name,
      description: "Built-in",
      value: name,
    });
  }

  editor.setPromptSuggestions(suggestions);
};

/**
 * Reload theme
 */
globalThis.theme_editor_reload = async function(): Promise<void> {
  if (state.themePath) {
    const themeName = state.themeName;
    const themeData = await loadThemeFile(themeName);
    if (themeData) {
      state.themeData = deepClone(themeData);
      state.originalThemeData = deepClone(themeData);
      state.hasChanges = false;
      updateDisplay();
      editor.setStatus("Theme reloaded from file");
    }
  } else {
    state.themeData = createDefaultTheme();
    state.originalThemeData = deepClone(state.themeData);
    state.hasChanges = false;
    updateDisplay();
    editor.setStatus("Theme reset to defaults");
  }
};

/**
 * Show help
 */
globalThis.theme_editor_show_help = function(): void {
  editor.setStatus(
    "Keys: ↑/↓ navigate | RET/SPC edit | TAB expand | c copy | n name | s save | S save-as | d default | q quit"
  );
};

// =============================================================================
// Command Registration
// =============================================================================

// Main command to open theme editor
editor.registerCommand(
  "Edit Theme",
  "Open the theme color editor",
  "open_theme_editor",
  "normal"
);

// Context-specific commands
editor.registerCommand(
  "Theme: Close Editor",
  "Close the theme editor",
  "theme_editor_close",
  "normal,theme-editor"
);

editor.registerCommand(
  "Theme: Edit Color",
  "Edit the selected color",
  "theme_editor_edit_color",
  "normal,theme-editor"
);

editor.registerCommand(
  "Theme: Toggle Section",
  "Expand or collapse section",
  "theme_editor_toggle_section",
  "normal,theme-editor"
);

editor.registerCommand(
  "Theme: Copy From Built-in",
  "Copy colors from a built-in theme",
  "theme_editor_copy_from_builtin",
  "normal,theme-editor"
);

editor.registerCommand(
  "Theme: Set Name",
  "Set the theme name",
  "theme_editor_set_name",
  "normal,theme-editor"
);

editor.registerCommand(
  "Theme: Save",
  "Save the theme",
  "theme_editor_save",
  "normal,theme-editor"
);

editor.registerCommand(
  "Theme: Save As",
  "Save theme with a new name",
  "theme_editor_save_as",
  "normal,theme-editor"
);

editor.registerCommand(
  "Theme: Set As Default",
  "Set a theme as the default",
  "theme_editor_set_as_default",
  "normal,theme-editor"
);

editor.registerCommand(
  "Theme: Reload",
  "Reload theme from file",
  "theme_editor_reload",
  "normal,theme-editor"
);

editor.registerCommand(
  "Theme: Show Help",
  "Show theme editor help",
  "theme_editor_show_help",
  "normal,theme-editor"
);

// =============================================================================
// Plugin Initialization
// =============================================================================

editor.setStatus("Theme Editor plugin loaded");
editor.debug("Theme Editor plugin initialized - Use 'Edit Theme' command to open");
