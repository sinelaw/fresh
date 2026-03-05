/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


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
 *
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
// Theme Schema (loaded dynamically from Rust)
// =============================================================================

/**
 * Cached theme sections loaded from the API.
 * This is populated on first use and reflects the actual theme structure from Rust.
 */
let cachedThemeSections: ThemeSection[] | null = null;

/**
 * Load theme sections from the Rust API.
 * Parses the raw JSON Schema and resolves $ref references.
 * Uses i18n keys for localized display names.
 */
function loadThemeSections(): ThemeSection[] {
  if (cachedThemeSections !== null) {
    return cachedThemeSections;
  }

  const schema = editor.getThemeSchema() as {
    $defs?: Record<string, Record<string, unknown>>;
    properties?: Record<string, unknown>;
  };
  const defs = schema.$defs || {};

  // Helper to resolve $ref and get the referenced schema
  const resolveRef = (refStr: string): Record<string, unknown> | null => {
    // $ref format: "#/$defs/TypeName"
    const prefix = "#/$defs/";
    if (refStr.startsWith(prefix)) {
      const typeName = refStr.slice(prefix.length);
      return defs[typeName] as Record<string, unknown> || null;
    }
    return null;
  };

  const sections: ThemeSection[] = [];
  const properties = schema.properties || {};

  // Section ordering
  const sectionOrder = ["editor", "ui", "search", "diagnostic", "syntax"];

  for (const [sectionName, sectionSchema] of Object.entries(properties)) {
    // Skip "name" field - it's not a color section
    if (sectionName === "name") continue;

    const sectionObj = sectionSchema as Record<string, unknown>;
    const sectionDesc = (sectionObj.description as string) || "";

    // Resolve $ref to get the actual type definition
    const refStr = sectionObj.$ref as string | undefined;
    const resolvedSchema = refStr ? resolveRef(refStr) : sectionObj;
    if (!resolvedSchema) continue;

    const sectionProps = resolvedSchema.properties as Record<string, unknown> || {};
    const fields: ThemeFieldDef[] = [];

    for (const [fieldName, fieldSchema] of Object.entries(sectionProps)) {
      const fieldObj = fieldSchema as Record<string, unknown>;
      const fieldDesc = (fieldObj.description as string) || "";

      // Generate i18n keys from field names
      const i18nName = `field.${fieldName}`;
      const i18nDesc = `field.${fieldName}_desc`;

      fields.push({
        key: fieldName,
        displayName: editor.t(i18nName) || fieldDesc || fieldName,
        description: editor.t(i18nDesc) || fieldDesc,
        section: sectionName,
      });
    }

    // Sort fields alphabetically (use simple comparison to avoid ICU issues in QuickJS)
    fields.sort((a, b) => (a.key < b.key ? -1 : a.key > b.key ? 1 : 0));

    // Generate i18n keys for section
    const sectionI18nName = `section.${sectionName}`;
    const sectionI18nDesc = `section.${sectionName}_desc`;

    sections.push({
      name: sectionName,
      displayName: editor.t(sectionI18nName) || sectionDesc || sectionName,
      description: editor.t(sectionI18nDesc) || sectionDesc,
      fields,
    });
  }

  // Sort sections in logical order
  sections.sort((a, b) => {
    const aIdx = sectionOrder.indexOf(a.name);
    const bIdx = sectionOrder.indexOf(b.name);
    return (aIdx === -1 ? 99 : aIdx) - (bIdx === -1 ? 99 : bIdx);
  });

  cachedThemeSections = sections;
  return cachedThemeSections;
}

/**
 * Get theme sections (loads from API if not cached)
 */
function getThemeSections(): ThemeSection[] {
  return loadThemeSections();
}

// =============================================================================
// State Management
// =============================================================================

interface ThemeEditorState {
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
  /** Pending save name for overwrite confirmation */
  pendingSaveName: string | null;
  /** Whether current theme is a built-in (requires Save As) */
  isBuiltin: boolean;
  /** Saved cursor field path (for restoring after prompts) */
  savedCursorPath: string | null;
  /** Whether to close the editor after a successful save */
  closeAfterSave: boolean;
}

/**
 * Check if the theme editor is currently open.
 * Uses a stateless approach by checking if the buffer actually exists.
 * This handles cases where the buffer was closed externally (e.g., Ctrl+W).
 */
function isThemeEditorOpen(): boolean {
  if (state.bufferId === null) {
    return false;
  }
  // Check if the buffer actually exists
  const buffers = editor.listBuffers();
  const exists = buffers.some(b => b.id === state.bufferId);

  // If buffer doesn't exist, reset our stale state
  if (!exists) {
    editor.debug(`Theme editor buffer ${state.bufferId} no longer exists, resetting state`);
    state.bufferId = null;
    state.splitId = null;
    state.themeData = {};
    state.originalThemeData = {};
    state.hasChanges = false;
  }

  return exists;
}

const state: ThemeEditorState = {
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
  pendingSaveName: null,
  isBuiltin: false,
  savedCursorPath: null,
  closeAfterSave: false,
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
  selectionBg: [50, 50, 80] as RGB,        // Dark blue-gray for selected field
};

// =============================================================================
// Keyboard Shortcuts (defined once, used in mode and i18n)
// =============================================================================

/**
 * Keyboard shortcuts for the theme editor.
 * These are defined once and used both in the mode definition and in the UI hints.
 */
const SHORTCUTS = {
  open: "C-o",
  save: "C-s",
  save_as: "C-S-s",
  delete: "C-d",
  close: "Esc",
  reload: "C-r",
  help: "F1",
};

// =============================================================================
// Mode Definition
// =============================================================================

editor.defineMode(
  "theme-editor",
  "normal",
  [
    // Navigation (standard keys that don't conflict with typing)
    ["Return", "theme_editor_edit_color"],
    ["Space", "theme_editor_edit_color"],
    ["Tab", "theme_editor_nav_next_section"],
    ["S-Tab", "theme_editor_nav_prev_section"],
    ["Up", "theme_editor_nav_up"],
    ["Down", "theme_editor_nav_down"],
    ["Escape", "theme_editor_close"],
    [SHORTCUTS.help, "theme_editor_show_help"],

    // Ctrl+ shortcuts (match common editor conventions)
    [SHORTCUTS.open, "theme_editor_open"],
    [SHORTCUTS.save, "theme_editor_save"],
    [SHORTCUTS.save_as, "theme_editor_save_as"],
    [SHORTCUTS.delete, "theme_editor_delete"],
    [SHORTCUTS.reload, "theme_editor_reload"],
    ["C-h", "theme_editor_show_help"],  // Alternative help key
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
  try {
    editor.debug("[theme_editor] loadBuiltinThemes: calling editor.getBuiltinThemes()");
    const rawThemes = editor.getBuiltinThemes();
    editor.debug(`[theme_editor] loadBuiltinThemes: got rawThemes type=${typeof rawThemes}`);
    // getBuiltinThemes returns a JSON string, need to parse it
    const builtinThemes = typeof rawThemes === "string"
      ? JSON.parse(rawThemes) as Record<string, string>
      : rawThemes as Record<string, string>;
    editor.debug(`[theme_editor] loadBuiltinThemes: parsed ${Object.keys(builtinThemes).length} themes`);
    return Object.keys(builtinThemes);
  } catch (e) {
    editor.debug(`[theme_editor] Failed to load built-in themes list: ${e}`);
    throw e;
  }
}

/**
 * Load theme data by name from the in-memory theme registry.
 * Works for all theme types (builtin, user, package) — no file I/O needed.
 */
function loadThemeFile(name: string): Record<string, unknown> | null {
  try {
    const data = editor.getThemeData(name);
    return data as Record<string, unknown> | null;
  } catch (e) {
    editor.debug(`[theme_editor] Failed to load theme data for '${name}': ${e}`);
    return null;
  }
}

// =============================================================================
// Field Building
// =============================================================================

/**
 * Build visible fields list based on expanded sections
 */
function buildVisibleFields(): ThemeField[] {
  const fields: ThemeField[] = [];
  const themeSections = getThemeSections();

  for (const section of themeSections) {
    const expanded = state.expandedSections.has(section.name);

    // Section header - displayName and description are already translated in getThemeSections()
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

        // fieldDef displayName and description are already translated in getThemeSections()
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
  const modifiedMarker = state.hasChanges ? " " + editor.t("panel.modified") : "";
  entries.push({
    text: `━━━ ${editor.t("panel.title", { name: state.themeName })}${modifiedMarker} ━━━\n`,
    properties: { type: "title" },
  });

  if (state.themePath) {
    entries.push({
      text: `${editor.t("panel.file", { path: state.themePath })}\n`,
      properties: { type: "file-path" },
    });
  } else {
    entries.push({
      text: editor.t("panel.new_theme") + "\n",
      properties: { type: "file-path" },
    });
  }

  // Key hints at the top (moved from footer)
  entries.push({
    text: editor.t("panel.nav_hint") + "\n",
    properties: { type: "footer" },
  });
  entries.push({
    text: editor.t("panel.action_hint", SHORTCUTS) + "\n",
    properties: { type: "footer" },
  });

  entries.push({
    text: "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
    properties: { type: "separator" },
  });

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

      // Color field with swatch characters (X for fg preview, space for bg preview)
      const colorStr = formatColorValue(field.value);

      entries.push({
        text: `${indent}  ${field.path} - ${field.def.displayName}: X  ${colorStr}\n`,
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

  return entries;
}

/**
 * Helper to add a colored overlay (foreground color)
 * addOverlay signature: (bufferId, namespace, start, end, r, g, b, underline, bold, italic, bg_r, bg_g, bg_b, extend_to_line_end)
 */
function addColorOverlay(
  bufferId: number,
  start: number,
  end: number,
  color: RGB,
  bold: boolean = false
): void {
  editor.addOverlay(bufferId, "theme", start, end, { fg: color, bold });
}

/**
 * Helper to add a background highlight overlay
 */
function addBackgroundHighlight(
  bufferId: number,
  start: number,
  end: number,
  bgColor: RGB
): void {
  editor.addOverlay(bufferId, "theme-selection", start, end, { bg: bgColor, extendToLineEnd: true });
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
 * Apply syntax highlighting
 */
function applyHighlighting(): void {
  if (state.bufferId === null) return;

  const bufferId = state.bufferId;
  editor.clearNamespace(bufferId, "theme");
  editor.clearNamespace(bufferId, "theme-selection");

  const entries = buildDisplayEntries();
  let byteOffset = 0;

  // Get current field at cursor to highlight it
  const currentField = getFieldAtCursor();
  const currentFieldPath = currentField?.path;

  for (const entry of entries) {
    const text = entry.text;
    const textLen = getUtf8ByteLength(text);
    const props = entry.properties as Record<string, unknown>;
    const entryType = props.type as string;
    const entryPath = props.path as string | undefined;

    // Add selection highlight for current field/section
    if (currentFieldPath && entryPath === currentFieldPath && (entryType === "field" || entryType === "section")) {
      addBackgroundHighlight(bufferId, byteOffset, byteOffset + textLen, colors.selectionBg);
    }

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

        // Color the swatch characters with the field's actual color
        // Text format: "FieldName: X  #RRGGBB" (X=fg, space=bg)
        const colorValue = props.colorValue as ColorValue;
        const rgb = parseColorToRgb(colorValue);
        if (rgb) {
          // "X" is at colon + 2 (": " = 2 bytes), and is 1 byte
          const swatchFgStart = nameEnd + getUtf8ByteLength(": ");
          const swatchFgEnd = swatchFgStart + 1; // "X" is 1 byte
          addColorOverlay(bufferId, swatchFgStart, swatchFgEnd, rgb);

          // First space after "X" is the bg swatch, 1 byte
          const swatchBgStart = swatchFgEnd;
          const swatchBgEnd = swatchBgStart + 1;
          // Use background color for the space
          editor.addOverlay(bufferId, "theme", swatchBgStart, swatchBgEnd, { bg: rgb });
        }

        // Value (hex code) - custom color (green)
        // Format: ": X  #RRGGBB" - value starts after "X  " (X + 2 spaces)
        const valueStart = nameEnd + getUtf8ByteLength(": X  ");
        addColorOverlay(bufferId, valueStart, byteOffset + textLen, colors.customValue);
      }
    } else if (entryType === "separator" || entryType === "footer") {
      addColorOverlay(bufferId, byteOffset, byteOffset + textLen, colors.footer);
    }

    byteOffset += textLen;
  }
}

/**
 * Update display (preserves cursor position)
 */
function updateDisplay(): void {
  if (state.bufferId === null) return;

  // Save current field path before updating
  const currentPath = getCurrentFieldPath();

  const entries = buildDisplayEntries();
  editor.setVirtualBufferContent(state.bufferId, entries);
  applyHighlighting();

  // Restore cursor to the same field if possible
  if (currentPath) {
    moveCursorToField(currentPath);
  }
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
 * Get field by path
 */
function getFieldByPath(path: string): ThemeField | null {
  return state.visibleFields.find(f => f.path === path) || null;
}

/**
 * Build color suggestions for a field
 */
function buildColorSuggestions(field: ThemeField): PromptSuggestion[] {
  const currentValue = formatColorValue(field.value);
  const suggestions: PromptSuggestion[] = [
    { text: currentValue, description: editor.t("suggestion.current"), value: currentValue },
  ];

  // Add special colors (Default/Reset for terminal transparency)
  for (const name of SPECIAL_COLORS) {
    suggestions.push({ text: name, description: editor.t("suggestion.terminal_native"), value: name });
  }

  // Add named colors with hex format
  for (const name of NAMED_COLOR_LIST) {
    const rgb = NAMED_COLORS[name];
    const hexValue = rgbToHex(rgb[0], rgb[1], rgb[2]);
    suggestions.push({ text: name, description: hexValue, value: name });
  }

  return suggestions;
}

/**
 * Start color editing prompt
 */
function editColorField(field: ThemeField): void {
  const currentValue = formatColorValue(field.value);
  editor.startPromptWithInitial(
    editor.t("prompt.color_input", { field: field.def.displayName }),
    `theme-color-${field.path}`,
    currentValue
  );
  editor.setPromptSuggestions(buildColorSuggestions(field));
}

interface ParseColorResult {
  value?: ColorValue;
  error?: string;
}

/**
 * Parse color input from user with detailed error messages
 */
function parseColorInput(input: string): ParseColorResult {
  input = input.trim();

  if (!input) {
    return { error: "empty" };
  }

  // Check for special colors (Default/Reset - use terminal's native color)
  if (SPECIAL_COLORS.includes(input)) {
    return { value: input };
  }

  // Check for named color
  if (input in NAMED_COLORS) {
    return { value: input };
  }

  // Try to parse as hex color #RRGGBB
  if (input.startsWith("#")) {
    const hex = input.slice(1);
    if (hex.length !== 6) {
      return { error: "hex_length" };
    }
    if (!/^[0-9A-Fa-f]{6}$/.test(hex)) {
      return { error: "hex_invalid" };
    }
    const hexResult = hexToRgb(input);
    if (hexResult) {
      return { value: hexResult };
    }
  }

  // Try to parse as RGB array [r, g, b]
  const rgbMatch = input.match(/^\[?\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\]?$/);
  if (rgbMatch) {
    const r = parseInt(rgbMatch[1], 10);
    const g = parseInt(rgbMatch[2], 10);
    const b = parseInt(rgbMatch[3], 10);
    if (r > 255 || g > 255 || b > 255) {
      return { error: "rgb_range" };
    }
    return { value: [r, g, b] };
  }

  // Unknown format
  return { error: "unknown" };
}

// =============================================================================
// Prompt Handlers
// =============================================================================

/**
 * Find best matching color name for partial input
 */
function findMatchingColor(input: string): string | null {
  const lower = input.toLowerCase();
  // First try exact match
  for (const name of Object.keys(NAMED_COLORS)) {
    if (name.toLowerCase() === lower) return name;
  }
  for (const name of SPECIAL_COLORS) {
    if (name.toLowerCase() === lower) return name;
  }
  // Then try prefix match
  for (const name of Object.keys(NAMED_COLORS)) {
    if (name.toLowerCase().startsWith(lower)) return name;
  }
  for (const name of SPECIAL_COLORS) {
    if (name.toLowerCase().startsWith(lower)) return name;
  }
  // Then try contains match
  for (const name of Object.keys(NAMED_COLORS)) {
    if (name.toLowerCase().includes(lower)) return name;
  }
  return null;
}

/**
 * Handle color prompt confirmation
 */
function onThemeColorPromptConfirmed(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (!args.prompt_type.startsWith("theme-color-")) return true;

  const path = args.prompt_type.replace("theme-color-", "");
  const field = getFieldByPath(path);
  if (!field) return true;

  const result = parseColorInput(args.input);

  if (result.value !== undefined) {
    // Valid color - apply it
    setNestedValue(state.themeData, path, result.value);
    state.hasChanges = !deepEqual(state.themeData, state.originalThemeData);

    const entries = buildDisplayEntries();
    if (state.bufferId !== null) {
      editor.setVirtualBufferContent(state.bufferId, entries);
      applyHighlighting();
    }
    moveCursorToField(path);
    editor.setStatus(editor.t("status.updated", { path }));
  } else {
    // Invalid input - try to find a matching color name
    const matchedColor = findMatchingColor(args.input);
    if (matchedColor) {
      // Found a match - reopen prompt with the matched value
      editor.startPromptWithInitial(
        editor.t("prompt.color_input", { field: field.def.displayName }),
        `theme-color-${path}`,
        matchedColor
      );
      // Rebuild suggestions
      const suggestions: PromptSuggestion[] = buildColorSuggestions(field);
      editor.setPromptSuggestions(suggestions);
      editor.setStatus(editor.t("status.autocompleted", { value: matchedColor }));
    } else {
      // No match found - reopen prompt with original input
      editor.startPromptWithInitial(
        editor.t("prompt.color_input", { field: field.def.displayName }),
        `theme-color-${path}`,
        args.input
      );
      const suggestions: PromptSuggestion[] = buildColorSuggestions(field);
      editor.setPromptSuggestions(suggestions);

      const errorKey = `error.color_${result.error}`;
      editor.setStatus(editor.t(errorKey, { input: args.input }));
    }
  }

  return true;
}
registerHandler("onThemeColorPromptConfirmed", onThemeColorPromptConfirmed);

/**
 * Handle open theme prompt (both builtin and user themes)
 */
async function onThemeOpenPromptConfirmed(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "theme-open") return true;

  const value = args.input.trim();

  // Parse the value to determine if it's user or builtin
  let isBuiltin = false;
  let themeName = value;

  if (value.startsWith("user:")) {
    themeName = value.slice(5);
    isBuiltin = false;
  } else if (value.startsWith("builtin:")) {
    themeName = value.slice(8);
    isBuiltin = true;
  } else {
    // Fallback: check if it's a builtin theme
    isBuiltin = state.builtinThemes.includes(value);
  }

  const themeData = loadThemeFile(themeName);
  if (themeData) {
    state.themeData = deepClone(themeData);
    state.originalThemeData = deepClone(themeData);
    state.themeName = themeName;
    state.themePath = null;
    state.isBuiltin = isBuiltin;
    state.hasChanges = false;
    updateDisplay();
    const statusKey = isBuiltin ? "status.opened_builtin" : "status.loaded";
    editor.setStatus(editor.t(statusKey, { name: themeName }));
  } else {
    editor.setStatus(editor.t("status.load_failed", { name: themeName }));
  }

  return true;
}
registerHandler("onThemeOpenPromptConfirmed", onThemeOpenPromptConfirmed);

/**
 * Handle save as prompt
 */
async function onThemeSaveAsPromptConfirmed(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "theme-save-as") return true;

  const name = args.input.trim();
  if (name) {
    // Reject names that match a built-in theme
    if (state.builtinThemes.includes(name)) {
      editor.setStatus(editor.t("error.name_is_builtin", { name }));
      theme_editor_save_as();
      return true;
    }

    // Check if a user theme file already exists with this name
    if (editor.themeFileExists(name)) {
      // Store pending save name for overwrite confirmation
      state.pendingSaveName = name;
      editor.startPrompt(editor.t("prompt.overwrite_confirm", { name }), "theme-overwrite-confirm");
      const suggestions: PromptSuggestion[] = [
        { text: editor.t("prompt.overwrite_yes"), description: "", value: "overwrite" },
        { text: editor.t("prompt.overwrite_no"), description: "", value: "cancel" },
      ];
      editor.setPromptSuggestions(suggestions);
      return true;
    }

    state.themeName = name;
    state.themeData.name = name;
    const restorePath = state.savedCursorPath;
    state.savedCursorPath = null;
    await saveTheme(name, restorePath);
  } else {
    state.savedCursorPath = null;
  }

  return true;
}
registerHandler("onThemeSaveAsPromptConfirmed", onThemeSaveAsPromptConfirmed);

/**
 * Handle prompt cancellation
 */
function onThemePromptCancelled(args: { prompt_type: string }) : boolean {
  if (!args.prompt_type.startsWith("theme-")) return true;

  // Clear saved state on cancellation
  state.savedCursorPath = null;
  state.pendingSaveName = null;
  state.closeAfterSave = false;

  editor.debug(editor.t("status.cancelled"));
  return true;
}
registerHandler("onThemePromptCancelled", onThemePromptCancelled);

/**
 * Handle initial theme selection prompt (when opening editor)
 */
async function onThemeSelectInitialPromptConfirmed(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  editor.debug(`[theme_editor] onThemeSelectInitialPromptConfirmed called with: ${JSON.stringify(args)}`);
  if (args.prompt_type !== "theme-select-initial") {
    editor.debug(`[theme_editor] prompt_type mismatch, expected 'theme-select-initial', got '${args.prompt_type}'`);
    return true;
  }
  editor.debug(`[theme_editor] prompt_type matched, processing selection...`);

  const value = args.input.trim();

  // Parse the value to determine if it's user or builtin
  let isBuiltin = false;
  let themeName = value;

  if (value.startsWith("user:")) {
    themeName = value.slice(5);
    isBuiltin = false;
  } else if (value.startsWith("builtin:")) {
    themeName = value.slice(8);
    isBuiltin = true;
  } else {
    // Fallback: check if it's a builtin theme
    isBuiltin = state.builtinThemes.includes(value);
  }

  editor.debug(editor.t("status.loading"));

  const themeData = loadThemeFile(themeName);
  if (themeData) {
    state.themeData = deepClone(themeData);
    state.originalThemeData = deepClone(themeData);
    state.themeName = themeName;
    state.themePath = null;
    state.isBuiltin = isBuiltin;
    state.hasChanges = false;
  } else {
    editor.setStatus(`Failed to load theme '${themeName}'`);
    return true;
  }

  // Now open the editor with loaded theme
  editor.debug(`[theme_editor] About to call doOpenThemeEditor()`);
  await doOpenThemeEditor();
  editor.debug(`[theme_editor] doOpenThemeEditor() completed`);

  return true;
}
registerHandler("onThemeSelectInitialPromptConfirmed", onThemeSelectInitialPromptConfirmed);

// Register prompt handlers
editor.on("prompt_confirmed", "onThemeSelectInitialPromptConfirmed");
editor.on("prompt_confirmed", "onThemeColorPromptConfirmed");
editor.on("prompt_confirmed", "onThemeOpenPromptConfirmed");
editor.on("prompt_confirmed", "onThemeSaveAsPromptConfirmed");
editor.on("prompt_confirmed", "onThemeDiscardPromptConfirmed");
editor.on("prompt_confirmed", "onThemeOverwritePromptConfirmed");
editor.on("prompt_confirmed", "onThemeDeletePromptConfirmed");
editor.on("prompt_cancelled", "onThemePromptCancelled");

// =============================================================================
// Theme Operations
// =============================================================================

/**
 * Save theme to file
 * @param name - Theme name to save as
 * @param restorePath - Optional field path to restore cursor to after save
 */
async function saveTheme(name?: string, restorePath?: string | null): Promise<boolean> {
  // Normalize theme name: lowercase, replace underscores/spaces with hyphens
  // (must match Rust's normalize_theme_name so config name matches filename)
  const themeName = (name || state.themeName).toLowerCase().replace(/[_ ]/g, "-");

  try {
    // Build a complete theme object from all known fields.
    // This ensures we always write every field, even if state.themeData
    // is missing some (e.g. package theme that failed to load fully).
    const completeTheme: Record<string, unknown> = { name: themeName };
    const sections = getThemeSections();
    for (const section of sections) {
      const sectionData: Record<string, unknown> = {};
      for (const field of section.fields) {
        const path = `${section.name}.${field.key}`;
        const value = getNestedValue(state.themeData, path);
        if (value !== undefined) {
          sectionData[field.key] = value;
        }
      }
      completeTheme[section.name] = sectionData;
    }

    const content = JSON.stringify(completeTheme, null, 2);
    const savedPath = editor.saveThemeFile(themeName, content);

    state.themePath = savedPath;
    state.themeName = themeName;
    state.isBuiltin = false; // After saving, it's now a user theme
    state.originalThemeData = deepClone(state.themeData);
    state.hasChanges = false;

    // Update display
    const entries = buildDisplayEntries();
    if (state.bufferId !== null) {
      editor.setVirtualBufferContent(state.bufferId, entries);
      applyHighlighting();
    }

    // Restore cursor position if provided
    if (restorePath) {
      moveCursorToField(restorePath);
    }

    // Reload themes and apply the new/updated theme atomically
    editor.reloadAndApplyTheme(themeName);
    editor.setStatus(editor.t("status.saved_and_applied", { name: themeName }));

    if (state.closeAfterSave) {
      state.closeAfterSave = false;
      doCloseEditor();
    }

    return true;
  } catch (e) {
    state.closeAfterSave = false;
    editor.setStatus(editor.t("status.save_failed", { error: String(e) }));
    return false;
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

function onThemeEditorCursorMoved(data: {
  buffer_id: number;
  cursor_id: number;
  old_position: number;
  new_position: number;
}): void {
  if (state.bufferId === null || data.buffer_id !== state.bufferId) return;

  applyHighlighting();

  const field = getFieldAtCursor();
  if (field) {
    editor.debug(field.def.description);
  }
}
registerHandler("onThemeEditorCursorMoved", onThemeEditorCursorMoved);

editor.on("cursor_moved", "onThemeEditorCursorMoved");

/**
 * Handle buffer_closed event to reset state when buffer is closed by any means
 */
function onThemeEditorBufferClosed(data: {
  buffer_id: number;
}): void {
  if (state.bufferId !== null && data.buffer_id === state.bufferId) {
    // Reset state when our buffer is closed
    state.bufferId = null;
    state.splitId = null;
    state.themeData = {};
    state.originalThemeData = {};
    state.hasChanges = false;
  }
}
registerHandler("onThemeEditorBufferClosed", onThemeEditorBufferClosed);

editor.on("buffer_closed", "onThemeEditorBufferClosed");

/**
 * Handle theme_inspect_key hook: open the theme editor at a specific key
 */
async function onThemeInspectKey(data: {
  theme_name: string;
  key: string;
}): Promise<void> {
  // If already open, focus and navigate to the key
  if (isThemeEditorOpen()) {
    if (state.bufferId !== null) {
      editor.showBuffer(state.bufferId);
    }
    const section = data.key.split(".")[0];
    if (!state.expandedSections.has(section)) {
      state.expandedSections.add(section);
    }
    updateDisplay();
    moveCursorToField(data.key);
    return;
  }

  // Save context
  state.sourceSplitId = editor.getActiveSplitId();
  state.sourceBufferId = editor.getActiveBufferId();
  state.builtinThemes = await loadBuiltinThemes();

  // Auto-load the current theme (builtin or user)
  const isBuiltin = state.builtinThemes.includes(data.theme_name);
  const themeData = loadThemeFile(data.theme_name);
  if (themeData) {
    state.themeData = deepClone(themeData);
    state.originalThemeData = deepClone(themeData);
    state.themeName = data.theme_name;
    state.themePath = null;
    state.isBuiltin = isBuiltin;
    state.hasChanges = false;
  } else {
    editor.setStatus(`Failed to load theme '${data.theme_name}'`);
    return;
  }

  // Expand the target section
  const section = data.key.split(".")[0];
  state.expandedSections.add(section);

  // Open editor and navigate
  await doOpenThemeEditor();
  moveCursorToField(data.key);
}
registerHandler("onThemeInspectKey", onThemeInspectKey);

editor.on("theme_inspect_key", "onThemeInspectKey");

// =============================================================================
// Smart Navigation - Skip Non-Selectable Lines
// =============================================================================

interface SelectableEntry {
  byteOffset: number;
  valueByteOffset: number; // Position at the value (after "field: ")
  index: number;
  isSection: boolean;
  path: string;
}

/**
 * Get byte offsets for all selectable entries (fields and sections)
 */
function getSelectableEntries(): SelectableEntry[] {
  const entries = buildDisplayEntries();
  const selectableEntries: SelectableEntry[] = [];
  let byteOffset = 0;

  for (const entry of entries) {
    const props = entry.properties as Record<string, unknown>;
    const entryType = props.type as string;
    const path = (props.path as string) || "";

    // Only fields and sections are selectable (they have index property)
    if ((entryType === "field" || entryType === "section") && typeof props.index === "number") {
      // For fields, calculate position at the color value (after "FieldName: X  ")
      let valueByteOffset = byteOffset;
      if (entryType === "field") {
        const colonIdx = entry.text.indexOf(":");
        if (colonIdx >= 0) {
          // Position at the hex value, after ": X  " (colon + space + X + 2 spaces = 5 chars)
          valueByteOffset = byteOffset + getUtf8ByteLength(entry.text.substring(0, colonIdx + 5));
        }
      }

      selectableEntries.push({
        byteOffset,
        valueByteOffset,
        index: props.index as number,
        isSection: entryType === "section",
        path,
      });
    }

    byteOffset += getUtf8ByteLength(entry.text);
  }

  return selectableEntries;
}

/**
 * Get the current selectable entry index based on cursor position
 */
function getCurrentSelectableIndex(): number {
  if (state.bufferId === null) return -1;

  const props = editor.getTextPropertiesAtCursor(state.bufferId);
  if (props.length > 0 && typeof props[0].index === "number") {
    return props[0].index as number;
  }
  return -1;
}

/**
 * Get the current field path at cursor
 */
function getCurrentFieldPath(): string | null {
  if (state.bufferId === null) return null;

  const props = editor.getTextPropertiesAtCursor(state.bufferId);
  if (props.length > 0 && typeof props[0].path === "string") {
    return props[0].path as string;
  }
  return null;
}

/**
 * Move cursor to a field by path (positions at value for fields)
 */
function moveCursorToField(path: string): void {
  if (state.bufferId === null) return;

  const selectableEntries = getSelectableEntries();
  for (const entry of selectableEntries) {
    if (entry.path === path) {
      // Use valueByteOffset for fields, byteOffset for sections
      const targetOffset = entry.isSection ? entry.byteOffset : entry.valueByteOffset;
      editor.setBufferCursor(state.bufferId, targetOffset);
      return;
    }
  }
}

/**
 * Navigate to the next selectable field/section
 */
function theme_editor_nav_down() : void {
  if (state.bufferId === null) return;

  const selectableEntries = getSelectableEntries();
  const currentIndex = getCurrentSelectableIndex();

  // Find next selectable entry after current
  for (const entry of selectableEntries) {
    if (entry.index > currentIndex) {
      // Use valueByteOffset for fields, byteOffset for sections
      const targetOffset = entry.isSection ? entry.byteOffset : entry.valueByteOffset;
      editor.setBufferCursor(state.bufferId, targetOffset);
      return;
    }
  }

  // Already at last selectable, stay there
  editor.debug(editor.t("status.at_last_field"));
}
registerHandler("theme_editor_nav_down", theme_editor_nav_down);

/**
 * Navigate to the previous selectable field/section
 */
function theme_editor_nav_up() : void {
  if (state.bufferId === null) return;

  const selectableEntries = getSelectableEntries();
  const currentIndex = getCurrentSelectableIndex();

  // Find previous selectable entry before current
  for (let i = selectableEntries.length - 1; i >= 0; i--) {
    const entry = selectableEntries[i];
    if (entry.index < currentIndex) {
      // Use valueByteOffset for fields, byteOffset for sections
      const targetOffset = entry.isSection ? entry.byteOffset : entry.valueByteOffset;
      editor.setBufferCursor(state.bufferId, targetOffset);
      return;
    }
  }

  // Already at first selectable, stay there
  editor.debug(editor.t("status.at_first_field"));
}
registerHandler("theme_editor_nav_up", theme_editor_nav_up);

/**
 * Navigate to next element (Tab) - includes both fields and sections
 */
function theme_editor_nav_next_section() : void {
  if (state.bufferId === null) return;

  const selectableEntries = getSelectableEntries();
  const currentIndex = getCurrentSelectableIndex();

  // Find next selectable entry after current
  for (const entry of selectableEntries) {
    if (entry.index > currentIndex) {
      // Use valueByteOffset for fields, byteOffset for sections
      const targetOffset = entry.isSection ? entry.byteOffset : entry.valueByteOffset;
      editor.setBufferCursor(state.bufferId, targetOffset);
      return;
    }
  }

  // Wrap to first entry
  if (selectableEntries.length > 0) {
    const entry = selectableEntries[0];
    const targetOffset = entry.isSection ? entry.byteOffset : entry.valueByteOffset;
    editor.setBufferCursor(state.bufferId, targetOffset);
  }
}
registerHandler("theme_editor_nav_next_section", theme_editor_nav_next_section);

/**
 * Navigate to previous element (Shift+Tab) - includes both fields and sections
 */
function theme_editor_nav_prev_section() : void {
  if (state.bufferId === null) return;

  const selectableEntries = getSelectableEntries();
  const currentIndex = getCurrentSelectableIndex();

  // Find previous selectable entry before current
  for (let i = selectableEntries.length - 1; i >= 0; i--) {
    const entry = selectableEntries[i];
    if (entry.index < currentIndex) {
      // Use valueByteOffset for fields, byteOffset for sections
      const targetOffset = entry.isSection ? entry.byteOffset : entry.valueByteOffset;
      editor.setBufferCursor(state.bufferId, targetOffset);
      return;
    }
  }

  // Wrap to last entry
  if (selectableEntries.length > 0) {
    const entry = selectableEntries[selectableEntries.length - 1];
    const targetOffset = entry.isSection ? entry.byteOffset : entry.valueByteOffset;
    editor.setBufferCursor(state.bufferId, targetOffset);
  }
}
registerHandler("theme_editor_nav_prev_section", theme_editor_nav_prev_section);

// =============================================================================
// Public Commands
// =============================================================================

/**
 * Open the theme editor - prompts user to select theme first
 */
async function open_theme_editor() : Promise<void> {
  editor.debug("[theme_editor] open_theme_editor called");
  if (isThemeEditorOpen()) {
    editor.debug("[theme_editor] already open, focusing");
    // Focus the existing theme editor split
    if (state.splitId !== null) {
      editor.focusSplit(state.splitId);
    }
    editor.debug(editor.t("status.already_open"));
    return;
  }

  editor.debug("[theme_editor] saving context");
  // Save context
  state.sourceSplitId = editor.getActiveSplitId();
  state.sourceBufferId = editor.getActiveBufferId();

  editor.debug("[theme_editor] loading builtin themes...");
  // Load available themes
  state.builtinThemes = await loadBuiltinThemes();
  editor.debug(`[theme_editor] loaded ${state.builtinThemes.length} builtin themes`);

  // Get current theme name from config
  const config = editor.getConfig() as Record<string, unknown>;
  const currentThemeName = (config?.theme as string) || "dark";

  // Prompt user to select which theme to edit
  editor.startPrompt(editor.t("prompt.select_theme_to_edit"), "theme-select-initial");

  const suggestions: PromptSuggestion[] = [];

  // Add user themes first (from themes directory)
  const userThemesDir = editor.getThemesDir();
  try {
    const entries = editor.readDir(userThemesDir);
    for (const e of entries) {
      if (e.is_file && e.name.endsWith(".json")) {
        const name = e.name.replace(".json", "");
        const isCurrent = name === currentThemeName;
        suggestions.push({
          text: name,
          description: isCurrent ? editor.t("suggestion.user_theme_current") : editor.t("suggestion.user_theme"),
          value: `user:${name}`,
        });
      }
    }
  } catch {
    // No user themes directory
  }

  // Add built-in themes
  for (const name of state.builtinThemes) {
    const isCurrent = name === currentThemeName;
    suggestions.push({
      text: name,
      description: isCurrent ? editor.t("suggestion.builtin_theme_current") : editor.t("suggestion.builtin_theme"),
      value: `builtin:${name}`,
    });
  }

  // Sort suggestions to put current theme first
  suggestions.sort((a, b) => {
    const aIsCurrent = a.description.includes("current");
    const bIsCurrent = b.description.includes("current");
    if (aIsCurrent && !bIsCurrent) return -1;
    if (!aIsCurrent && bIsCurrent) return 1;
    return 0;
  });

  editor.setPromptSuggestions(suggestions);
}
registerHandler("open_theme_editor", open_theme_editor);

/**
 * Actually open the theme editor with loaded theme data
 */
async function doOpenThemeEditor(): Promise<void> {
  editor.debug("[theme_editor] doOpenThemeEditor: building display entries");
  // Build initial entries
  const entries = buildDisplayEntries();
  editor.debug(`[theme_editor] doOpenThemeEditor: built ${entries.length} entries`);

  // Create virtual buffer in current split (no new split)
  editor.debug("[theme_editor] doOpenThemeEditor: calling createVirtualBuffer...");
  const result = await editor.createVirtualBuffer({
    name: "*Theme Editor*",
    mode: "theme-editor",
    readOnly: true,
    entries: entries,
    showLineNumbers: false,
    showCursors: true,
    editingDisabled: true,
  });
  const bufferId = result.bufferId;
  editor.debug(`[theme_editor] doOpenThemeEditor: createVirtualBuffer returned bufferId=${bufferId}`);
  editor.debug(`[theme_editor] doOpenThemeEditor: checking if bufferId !== null...`);

  if (bufferId !== null) {
    editor.debug(`[theme_editor] doOpenThemeEditor: bufferId is not null, setting state...`);
    state.bufferId = bufferId;
    state.splitId = null;

    editor.debug(`[theme_editor] doOpenThemeEditor: calling applyHighlighting...`);
    applyHighlighting();
    editor.debug(`[theme_editor] doOpenThemeEditor: applyHighlighting completed`);
    editor.debug(`[theme_editor] doOpenThemeEditor: calling setStatus...`);
    editor.debug(editor.t("status.ready"));
    editor.debug(`[theme_editor] doOpenThemeEditor: completed successfully`);
  } else {
    editor.setStatus(editor.t("status.open_failed"));
  }
}

/**
 * Close the theme editor
 */
function theme_editor_close() : void {
  if (!isThemeEditorOpen()) return;

  if (state.hasChanges) {
    // Show confirmation prompt before closing with unsaved changes
    editor.startPrompt(editor.t("prompt.discard_confirm"), "theme-discard-confirm");
    const suggestions: PromptSuggestion[] = [
      { text: editor.t("prompt.discard_no"), description: "", value: "keep" },
      { text: editor.t("prompt.discard_save"), description: "", value: "save" },
      { text: editor.t("prompt.discard_yes"), description: "", value: "discard" },
    ];
    editor.setPromptSuggestions(suggestions);
    return;
  }

  doCloseEditor();
}
registerHandler("theme_editor_close", theme_editor_close);

/**
 * Actually close the editor (called after confirmation or when no changes)
 */
function doCloseEditor(): void {
  // Close the buffer (this will switch to another buffer in the same split)
  if (state.bufferId !== null) {
    editor.closeBuffer(state.bufferId);
  }

  // Reset state
  state.bufferId = null;
  state.splitId = null;
  state.themeData = {};
  state.originalThemeData = {};
  state.hasChanges = false;

  editor.debug(editor.t("status.closed"));
}

/**
 * Handle discard confirmation prompt
 */
function onThemeDiscardPromptConfirmed(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (args.prompt_type !== "theme-discard-confirm") return true;

  const response = args.input.trim().toLowerCase();
  if (response === "discard" || args.selected_index === 2) {
    editor.setStatus(editor.t("status.unsaved_discarded"));
    doCloseEditor();
  } else if (response === "save" || args.selected_index === 1) {
    state.closeAfterSave = true;
    theme_editor_save();
  } else {
    editor.debug(editor.t("status.cancelled"));
  }

  return false;
}
registerHandler("onThemeDiscardPromptConfirmed", onThemeDiscardPromptConfirmed);

/**
 * Edit color at cursor
 */
function theme_editor_edit_color() : void {
  const field = getFieldAtCursor();
  if (!field) {
    editor.debug(editor.t("status.no_field"));
    return;
  }

  if (field.isSection) {
    theme_editor_toggle_section();
    return;
  }

  editColorField(field);
}
registerHandler("theme_editor_edit_color", theme_editor_edit_color);

/**
 * Toggle section expansion
 */
function theme_editor_toggle_section() : void {
  const field = getFieldAtCursor();
  if (!field || !field.isSection) {
    editor.debug(editor.t("status.not_section"));
    return;
  }

  if (state.expandedSections.has(field.path)) {
    state.expandedSections.delete(field.path);
  } else {
    state.expandedSections.add(field.path);
  }

  updateDisplay();
}
registerHandler("theme_editor_toggle_section", theme_editor_toggle_section);

/**
 * Open a theme (builtin or user) for editing
 */
function theme_editor_open() : void {
  editor.startPrompt(editor.t("prompt.open_theme"), "theme-open");

  const suggestions: PromptSuggestion[] = [];

  // Add user themes first (from themes directory)
  const userThemesDir = editor.getThemesDir();
  try {
    const entries = editor.readDir(userThemesDir);
    for (const e of entries) {
      if (e.is_file && e.name.endsWith(".json")) {
        const name = e.name.replace(".json", "");
        suggestions.push({
          text: name,
          description: editor.t("suggestion.user_theme"),
          value: `user:${name}`,
        });
      }
    }
  } catch {
    // No user themes directory
  }

  // Add built-in themes
  for (const name of state.builtinThemes) {
    suggestions.push({
      text: name,
      description: editor.t("suggestion.builtin_theme"),
      value: `builtin:${name}`,
    });
  }

  editor.setPromptSuggestions(suggestions);
}
registerHandler("theme_editor_open", theme_editor_open);

/**
 * Save theme
 */
async function theme_editor_save() : Promise<void> {
  // Save cursor path for restoration after save
  state.savedCursorPath = getCurrentFieldPath();

  // Built-in themes require Save As
  if (state.isBuiltin) {
    editor.setStatus(editor.t("status.builtin_requires_save_as"));
    theme_editor_save_as();
    return;
  }

  // If theme has never been saved (no path), trigger "Save As" instead
  if (!state.themePath) {
    theme_editor_save_as();
    return;
  }

  if (!state.hasChanges) {
    editor.debug(editor.t("status.no_changes"));
    return;
  }

  // Check for name collision if name has changed since last save
  const expectedPath = editor.pathJoin(editor.getThemesDir(), `${state.themeName}.json`);

  if (state.themePath !== expectedPath && editor.themeFileExists(state.themeName)) {
    // File exists with this name - ask for confirmation
    editor.startPrompt(editor.t("prompt.overwrite_confirm", { name: state.themeName }), "theme-overwrite-confirm");
    const suggestions: PromptSuggestion[] = [
      { text: editor.t("prompt.overwrite_yes"), description: "", value: "overwrite" },
      { text: editor.t("prompt.overwrite_no"), description: "", value: "cancel" },
    ];
    editor.setPromptSuggestions(suggestions);
    return;
  }

  await saveTheme(undefined, state.savedCursorPath);
}
registerHandler("theme_editor_save", theme_editor_save);

/**
 * Handle overwrite confirmation prompt
 */
async function onThemeOverwritePromptConfirmed(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "theme-overwrite-confirm") return true;

  const response = args.input.trim().toLowerCase();
  if (response === "overwrite" || args.selected_index === 0) {
    // Use pending name if set (from Save As), otherwise use current name
    const nameToSave = state.pendingSaveName || state.themeName;
    state.themeName = nameToSave;
    state.themeData.name = nameToSave;
    state.pendingSaveName = null;
    const restorePath = state.savedCursorPath;
    state.savedCursorPath = null;
    await saveTheme(nameToSave, restorePath);
  } else {
    state.pendingSaveName = null;
    state.savedCursorPath = null;
    state.closeAfterSave = false;
    editor.debug(editor.t("status.cancelled"));
  }

  return false;
}
registerHandler("onThemeOverwritePromptConfirmed", onThemeOverwritePromptConfirmed);

/**
 * Save theme as (new name)
 */
function theme_editor_save_as() : void {
  // Save cursor path for restoration after save (if not already saved by theme_editor_save)
  if (!state.savedCursorPath) {
    state.savedCursorPath = getCurrentFieldPath();
  }

  editor.startPrompt(editor.t("prompt.save_as"), "theme-save-as");

  editor.setPromptSuggestions([{
    text: state.themeName,
    description: editor.t("suggestion.current"),
    value: state.themeName,
  }]);
}
registerHandler("theme_editor_save_as", theme_editor_save_as);

/**
 * Reload theme
 */
async function theme_editor_reload() : Promise<void> {
  if (state.themePath) {
    const themeName = state.themeName;
    const themeData = loadThemeFile(themeName);
    if (themeData) {
      state.themeData = deepClone(themeData);
      state.originalThemeData = deepClone(themeData);
      state.hasChanges = false;
      updateDisplay();
      editor.setStatus(editor.t("status.reloaded"));
    }
  } else {
    state.themeData = createDefaultTheme();
    state.originalThemeData = deepClone(state.themeData);
    state.hasChanges = false;
    updateDisplay();
    editor.setStatus(editor.t("status.reset"));
  }
}
registerHandler("theme_editor_reload", theme_editor_reload);

/**
 * Show help
 */
function theme_editor_show_help() : void {
  editor.debug(editor.t("status.help"));
}
registerHandler("theme_editor_show_help", theme_editor_show_help);

/**
 * Delete the current user theme
 */
function theme_editor_delete() : void {
  // Can only delete saved user themes
  if (!state.themePath) {
    editor.setStatus(editor.t("status.cannot_delete_unsaved"));
    return;
  }

  // Show confirmation dialog
  editor.startPrompt(editor.t("prompt.delete_confirm", { name: state.themeName }), "theme-delete-confirm");
  const suggestions: PromptSuggestion[] = [
    { text: editor.t("prompt.delete_yes"), description: "", value: "delete" },
    { text: editor.t("prompt.delete_no"), description: "", value: "cancel" },
  ];
  editor.setPromptSuggestions(suggestions);
}
registerHandler("theme_editor_delete", theme_editor_delete);

/**
 * Handle delete confirmation prompt
 */
async function onThemeDeletePromptConfirmed(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "theme-delete-confirm") return true;

  const value = args.input.trim();
  if (value === "delete" || value === editor.t("prompt.delete_yes")) {
    if (state.themeName) {
      try {
        // Delete the theme file by name
        await editor.deleteTheme(state.themeName);
        const deletedName = state.themeName;

        // Reset to default theme
        state.themeData = createDefaultTheme();
        state.originalThemeData = deepClone(state.themeData);
        state.themeName = "custom";
        state.themePath = null;
        state.hasChanges = false;
        updateDisplay();

        editor.setStatus(editor.t("status.deleted", { name: deletedName }));
      } catch (e) {
        editor.setStatus(editor.t("status.delete_failed", { error: String(e) }));
      }
    }
  } else {
    editor.debug(editor.t("status.cancelled"));
  }

  return true;
}
registerHandler("onThemeDeletePromptConfirmed", onThemeDeletePromptConfirmed);

// =============================================================================
// Command Registration
// =============================================================================

// Main command to open theme editor (always available - no context restriction)
editor.registerCommand(
  "%cmd.edit_theme",
  "%cmd.edit_theme_desc",
  "open_theme_editor"
);

// Buffer-scoped commands - only visible when a buffer with mode "theme-editor" is focused
// The core automatically checks the focused buffer's mode against command contexts
editor.registerCommand("%cmd.close_editor", "%cmd.close_editor_desc", "theme_editor_close", "theme-editor");
editor.registerCommand("%cmd.edit_color", "%cmd.edit_color_desc", "theme_editor_edit_color", "theme-editor");
editor.registerCommand("%cmd.toggle_section", "%cmd.toggle_section_desc", "theme_editor_toggle_section", "theme-editor");
editor.registerCommand("%cmd.open_theme", "%cmd.open_theme_desc", "theme_editor_open", "theme-editor");
editor.registerCommand("%cmd.save", "%cmd.save_desc", "theme_editor_save", "theme-editor");
editor.registerCommand("%cmd.save_as", "%cmd.save_as_desc", "theme_editor_save_as", "theme-editor");
editor.registerCommand("%cmd.reload", "%cmd.reload_desc", "theme_editor_reload", "theme-editor");
editor.registerCommand("%cmd.show_help", "%cmd.show_help_desc", "theme_editor_show_help", "theme-editor");
editor.registerCommand("%cmd.delete_theme", "%cmd.delete_theme_desc", "theme_editor_delete", "theme-editor");
editor.registerCommand("%cmd.nav_up", "%cmd.nav_up_desc", "theme_editor_nav_up", "theme-editor");
editor.registerCommand("%cmd.nav_down", "%cmd.nav_down_desc", "theme_editor_nav_down", "theme-editor");
editor.registerCommand("%cmd.nav_next", "%cmd.nav_next_desc", "theme_editor_nav_next_section", "theme-editor");
editor.registerCommand("%cmd.nav_prev", "%cmd.nav_prev_desc", "theme_editor_nav_prev_section", "theme-editor");

// =============================================================================
// Plugin Initialization
// =============================================================================

editor.debug("Theme Editor plugin initialized - Use 'Edit Theme' command to open");
