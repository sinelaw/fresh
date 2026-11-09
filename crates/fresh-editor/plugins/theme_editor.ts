/// <reference path="./lib/fresh.d.ts" />
import { hintBar, parseHintString, WidgetPanel } from "./lib/index.ts";
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
type ModifierValue = string[];
type ThemeFieldValue = ColorValue | ModifierValue | boolean;
/** Note: a `text` field's value is a string, which `ColorValue` covers. */

// =============================================================================
// Layout Constants & Panel Types
// =============================================================================

const LEFT_WIDTH = 38;
/** The tree panel's share of the editor's width. */
const TREE_RATIO = 0.45;
const RIGHT_WIDTH_CONST = 61;
function RIGHT_WIDTH(): number { return RIGHT_WIDTH_CONST; }

type PickerFocusTarget =
  | { type: "attributes" }
  | { type: "hex-input" }
  | { type: "named-colors"; index: number }
  | { type: "palette"; row: number; col: number };

// =============================================================================
// Named Color Grid (for picker panel)
// =============================================================================

const NAMED_COLORS_PER_ROW = 6;
const NAMED_COLOR_GRID: Array<Array<{ display: string; value: string; rgb: OverlayColorSpec | null }>> = [
  [
    { display: "Black", value: "Black", rgb: "Black" },
    { display: "Red", value: "Red", rgb: "Red" },
    { display: "Green", value: "Green", rgb: "Green" },
    { display: "Yellow", value: "Yellow", rgb: "Yellow" },
    { display: "Blue", value: "Blue", rgb: "Blue" },
    { display: "Magenta", value: "Magenta", rgb: "Magenta" },
  ],
  [
    { display: "Cyan", value: "Cyan", rgb: "Cyan" },
    { display: "Gray", value: "Gray", rgb: "Gray" },
    { display: "DkGray", value: "DarkGray", rgb: "DarkGray" },
    { display: "LtRed", value: "LightRed", rgb: "LightRed" },
    { display: "LtGreen", value: "LightGreen", rgb: "LightGreen" },
    { display: "LtYellw", value: "LightYellow", rgb: "LightYellow" },
  ],
  [
    { display: "LtBlue", value: "LightBlue", rgb: "LightBlue" },
    { display: "LtMag", value: "LightMagenta", rgb: "LightMagenta" },
    { display: "LtCyan", value: "LightCyan", rgb: "LightCyan" },
    { display: "White", value: "White", rgb: "White" },
    { display: "Default", value: "Default", rgb: null },
    { display: "Reset", value: "Reset", rgb: null },
  ],
];

// =============================================================================
// Extended Color Palette
// =============================================================================

const PALETTE_COLS = 12;
const PALETTE_ROWS = 4;
const PALETTE_LIGHTNESSES = [25, 40, 60, 75];

function hslToRgb(h: number, s: number, l: number): RGB {
  s /= 100;
  l /= 100;
  const c = (1 - Math.abs(2 * l - 1)) * s;
  const x = c * (1 - Math.abs((h / 60) % 2 - 1));
  const m = l - c / 2;
  let r = 0, g = 0, b = 0;
  if (h < 60) { r = c; g = x; }
  else if (h < 120) { r = x; g = c; }
  else if (h < 180) { g = c; b = x; }
  else if (h < 240) { g = x; b = c; }
  else if (h < 300) { r = x; b = c; }
  else { r = c; b = x; }
  return [
    Math.round((r + m) * 255),
    Math.round((g + m) * 255),
    Math.round((b + m) * 255),
  ];
}

let cachedPalette: RGB[][] | null = null;

function getExtendedPalette(): RGB[][] {
  if (cachedPalette) return cachedPalette;
  const palette: RGB[][] = [];
  for (let row = 0; row < PALETTE_ROWS; row++) {
    const rowColors: RGB[] = [];
    for (let col = 0; col < PALETTE_COLS; col++) {
      const hue = col * 30;
      rowColors.push(hslToRgb(hue, 80, PALETTE_LIGHTNESSES[row]));
    }
    palette.push(rowColors);
  }
  cachedPalette = palette;
  return palette;
}

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
  kind: FieldKind;
  /** A color that carries text attributes (`{color, modifier}` on disk),
   * edited from the picker's Attributes row. */
  styled?: boolean;
  /** A `group`'s fields: a nested object with fixed keys
   * (`chrome.nerd_font`, `file_icons.default`). */
  children?: ThemeFieldDef[];
  /** A `map`'s per-entry fields: an object keyed by the user
   * (`file_icons.by_extension`), each entry the same shape. */
  entry?: ThemeFieldDef[];
  /** Which built-in table a map's or group's values fall back to, for
   * showing what an unset value inherits (`file_icons` only). */
  builtin?: string;
  /** What an unset value draws: the schema's `default`. */
  defaultValue?: unknown;
  /** How a map's keys are read when the theme loads (the schema's
   * `x-key-normalize`), so the editor writes them the same way. */
  keyNormalize?: string;
}

/**
 * What a field edits: a color, a color's text attributes, or one of the
 * `chrome` section's structural options — an on/off toggle, or the text of a
 * separator glyph.
 */
type FieldKind =
  | "color" | "modifier" | "toggle" | "text"
  // Containers: a nested object with fixed keys, a user-keyed map, one of
  // a map's entries, and the row that adds one.
  | "group" | "map" | "entry" | "add";

/**
 * Theme field with current value
 */
interface ThemeField {
  def: ThemeFieldDef;
  value: ThemeFieldValue;
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
 * Whether a property schema entry refers to `ColorDef` — directly via
 * `$ref` or wrapped inside an `anyOf` (the schema generator wraps
 * `Option<ColorDef>` as `anyOf: [{$ref: ColorDef}, {type: null}]`).
 * Returns false for unrelated $refs such as `ModifierDef`.
 */
function fieldRefersToColorDef(fieldObj: Record<string, unknown>): boolean {
  return refOf(fieldObj)?.endsWith("/ColorDef") ?? false;
}

/**
 * Whether a property schema refers to `StyledColorDef` — a value that is
 * either a bare color or a `{color, modifier}` bundle — directly or wrapped
 * in an `anyOf` (every theme key is optional, so the schema generator wraps
 * it as `anyOf: [{$ref: StyledColorDef}, {type: null}]`). Such a field is a
 * color whose attributes are edited on its picker panel.
 */
function fieldRefersToStyledColorDef(fieldObj: Record<string, unknown>): boolean {
  return refOf(fieldObj)?.endsWith("/StyledColorDef") ?? false;
}

/** The `$ref` a property schema entry points at, directly or as the
 * non-null half of an `anyOf`. */
function refOf(fieldObj: Record<string, unknown>): string | null {
  if (typeof fieldObj["$ref"] === "string") return fieldObj["$ref"] as string;
  const anyOf = fieldObj["anyOf"];
  if (Array.isArray(anyOf)) {
    for (const v of anyOf) {
      const r = v && typeof v === "object" ? (v as Record<string, unknown>)["$ref"] : undefined;
      if (typeof r === "string") return r;
    }
  }
  return null;
}

/** Whether a property schema entry is a string (possibly nullable). Theme
 * colors are `$ref`s, so only the `chrome` section's separators match. */
function fieldIsString(fieldObj: Record<string, unknown>): boolean {
  const t = fieldObj["type"];
  return t === "string" || (Array.isArray(t) && t.includes("string"));
}

/** Whether a property schema entry is a boolean (possibly nullable). */
function fieldIsBoolean(fieldObj: Record<string, unknown>): boolean {
  const t = fieldObj["type"];
  return t === "boolean" || (Array.isArray(t) && t.includes("boolean"));
}

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
  const sectionOrder = ["editor", "ui", "chrome", "file_icons", "search", "diagnostic", "syntax"];

  for (const [sectionName, sectionSchema] of Object.entries(properties)) {
    // Skip top-level fields that aren't color sections: `name` is the
    // theme's identity and `extends` is the inheritance pointer (a string,
    // not a section). Without this check, the plugin would emit
    // `"extends": {}` when serializing back out, which fails to round-trip
    // as a `ThemeFile`.
    if (sectionName === "name" || sectionName === "extends") continue;

    const sectionObj = sectionSchema as Record<string, unknown>;
    const sectionDesc = (sectionObj.description as string) || "";

    // Resolve $ref to get the actual type definition
    const refStr = sectionObj.$ref as string | undefined;
    const resolvedSchema = refStr ? resolveRef(refStr) : sectionObj;
    if (!resolvedSchema) continue;

    const sectionProps = resolvedSchema.properties as Record<string, unknown> || {};
    const fields: ThemeFieldDef[] = [];

    // Build a field def from a field key + kind, resolving its i18n labels.
    const makeFieldDef = (
      key: string,
      kind: FieldKind,
      fallbackDesc: string,
    ): ThemeFieldDef => {
      // Numbered indent-rainbow fields share one translated template. Keeping
      // the level as data avoids twelve near-identical locale keys and lets
      // translators place the number where their language expects it.
      const indentRainbow = /^indent_rainbow_([1-6])$/.exec(key);
      const i18nKey = indentRainbow ? "indent_rainbow" : key;
      const params = indentRainbow ? { level: indentRainbow[1] } : undefined;
      // A missing translation comes back as the key itself; treat it as none.
      const tr = (k: string, p?: Record<string, string>): string | undefined => {
        const s = editor.t(k, p);
        return s && s !== k ? s : undefined;
      };
      // Every color key has a synthetic attributes row. One without a
      // translation of its own is named after its color field.
      const colorKey = kind === "modifier" ? key.replace(/_modifier$/, "") : undefined;
      const colorName = colorKey ? (tr(`field.${colorKey}`) ?? colorKey) : undefined;
      const displayName = tr(`field.${i18nKey}`, params)
        ?? (colorName ? tr("field.attributes_of", { field: colorName }) : undefined)
        ?? (fallbackDesc || key);
      const description = tr(`field.${i18nKey}_desc`, params)
        ?? (colorName ? tr("field.attributes_of_desc", { field: colorName }) : undefined)
        ?? fallbackDesc;
      return {
        key,
        displayName,
        description,
        section: sectionName,
        kind,
      };
    };

    // The fields of one object schema, recursing into nested objects
    // (groups) and keyed maps. Everything the editor can edit is found here
    // from the schema, so a new theme option needs no plugin change.
    const fieldsOf = (props: Record<string, unknown>, builtin?: string): ThemeFieldDef[] => {
      const out: ThemeFieldDef[] = [];
      for (const [fieldName, fieldSchema] of Object.entries(props)) {
        const fieldObj = fieldSchema as Record<string, unknown>;
        const fieldDesc = (fieldObj.description as string) || "";
        // The chrome options: booleans are toggles, strings are glyphs.
        if (fieldIsBoolean(fieldObj)) {
          out.push({ ...makeFieldDef(fieldName, "toggle", fieldDesc), defaultValue: fieldObj["default"] });
          continue;
        }
        if (fieldIsString(fieldObj)) {
          out.push({ ...makeFieldDef(fieldName, "text", fieldDesc), defaultValue: fieldObj["default"] });
          continue;
        }
        const styled = fieldRefersToStyledColorDef(fieldObj);
        if (styled || fieldRefersToColorDef(fieldObj)) {
          out.push({ ...makeFieldDef(fieldName, "color", fieldDesc), styled, builtin });
          continue;
        }
        // A keyed map: `{type: object, additionalProperties: {$ref: T}}`.
        const additional = fieldObj["additionalProperties"] as Record<string, unknown> | undefined;
        const entryRef = additional && typeof additional["$ref"] === "string"
          ? resolveRef(additional["$ref"] as string) : null;
        if (entryRef && entryRef.properties) {
          const table = builtin ? `${builtin}.${fieldName}` : undefined;
          out.push({
            ...makeFieldDef(fieldName, "map", fieldDesc),
            entry: fieldsOf(entryRef.properties as Record<string, unknown>, table),
            builtin: table,
            keyNormalize: fieldObj["x-key-normalize"] as string | undefined,
          });
          continue;
        }
        // A nested object: `$ref` to a struct, possibly `anyOf [.., null]`.
        const nested = refOf(fieldObj);
        const nestedSchema = nested ? resolveRef(nested) : null;
        if (nestedSchema && nestedSchema.properties) {
          const table = builtin ? `${builtin}.${fieldName}` : undefined;
          out.push({
            ...makeFieldDef(fieldName, "group", fieldDesc),
            children: fieldsOf(nestedSchema.properties as Record<string, unknown>, table),
            builtin: table,
          });
        }
      }
      // Sorted for a stable tree (simple comparison: no ICU in QuickJS).
      out.sort((a, b) => (a.key < b.key ? -1 : a.key > b.key ? 1 : 0));
      return out;
    };
    fields.push(...fieldsOf(sectionProps, sectionName === "file_icons" ? "file_icons" : undefined));


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
  /** Theme display name */
  themeName: string;
  /** Theme registry key (for lookups) */
  themeKey: string;
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
  /** All themes from registry: key → {name, pack} */
  themeRegistry: Map<string, {name: string; pack: string}>;
  /** Keys of builtin themes (empty pack) */
  builtinKeys: Set<string>;
  /** Pending save name for overwrite confirmation */
  pendingSaveName: string | null;
  /** Whether current theme is a built-in (requires Save As) */
  isBuiltin: boolean;
  /** Saved cursor field path (for restoring after prompts) */
  savedCursorPath: string | null;
  /** Whether to close the editor after a successful save */
  closeAfterSave: boolean;
  /** Whether the Save As prompt has been pre-filled (to distinguish first vs second Enter) */
  saveAsPreFilled: boolean;
  /** Which panel has focus */
  focusPanel: "tree" | "picker";
  /** Focus target within picker panel */
  pickerFocus: PickerFocusTarget;
  /** Filter text for tree */
  filterText: string;
  /** Whether filter input is active */
  filterActive: boolean;
  /** First visible tree line index for virtual scrolling */
  treeScrollOffset: number;
  /** Cached viewport height */
  viewportHeight: number;
  /** Cached viewport width */
  viewportWidth: number;
  /** Buffer group ID (when using buffer groups) */
  groupId: number | null;
  /** Widget panel rendering the footer hint bar. Created when the
   * buffer-group view opens. The tree and picker panels still use
   * `setPanelContent` because they own the focus model (header
   * row, separator, optional filter line, then selectable section /
   * field rows) which doesn't map cleanly to a single List today. */
  footerPanel: WidgetPanel | null;
  /** Panel buffer IDs keyed by panel name */
  panelBuffers: Record<string, number>;
  /** The theme the editor was running when the theme editor opened (its
   * config key), restored when the editor closes without saving. */
  runningThemeKey: string | null;
  /** The theme JSON last handed to `previewTheme`, so an unchanged theme is
   * not re-applied on every redraw. */
  previewedJson: string | null;
  /** The pane the theme editor opened beside the user's code, closed with
   * the editor. */
  editorPaneId: number | null;
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
    endPreview();
    state.bufferId = null;
    state.splitId = null;
    state.themeData = {};
    state.originalThemeData = {};
    state.hasChanges = false;
    state.focusPanel = "tree";
    state.selectedIndex = 0;
    state.treeScrollOffset = 0;
    state.filterText = "";
    state.filterActive = false;
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
  themeName: "",
  themeKey: "",
  themePath: null,
  expandedSections: new Set(["editor", "syntax"]),
  visibleFields: [],
  selectedIndex: 0,
  hasChanges: false,
  themeRegistry: new Map(),
  builtinKeys: new Set(),
  pendingSaveName: null,
  isBuiltin: false,
  savedCursorPath: null,
  closeAfterSave: false,
  saveAsPreFilled: false,
  focusPanel: "tree",
  pickerFocus: { type: "hex-input" },
  filterText: "",
  filterActive: false,
  treeScrollOffset: 0,
  viewportHeight: 40,
  viewportWidth: 120,
  groupId: null,
  footerPanel: null,
  panelBuffers: {},
  runningThemeKey: null,
  previewedJson: null,
  editorPaneId: null,
};

// =============================================================================
// Color Definitions for UI
// =============================================================================

/**
 * UI palette for the theme editor's own chrome.
 *
 * Each role maps to a theme-key path (e.g. `"syntax.keyword"`). The plugin
 * passes these strings straight through to the core as
 * `OverlayColorSpec::ThemeKey`, and the core's renderer resolves them
 * against the *currently-active* theme on every frame
 * (see `OverlayFace::from_options` → `OverlayFace::ThemedStyle`, and the
 * render-time lookup in `split_rendering.rs`). That means the theme editor
 * inherits the host theme's look and automatically picks up theme switches
 * without the plugin having to rebuild its overlays or even be notified —
 * the `resolve_theme_key` lookup runs with the new theme on the next render.
 *
 * Important: we only use keys that the theme defines as readable on
 * `editor.bg` (since that's the background the theme editor draws over).
 * That rules out `ui.menu_*`, `ui.tab_*`, etc. — those are designed for
 * their own bg pairs (e.g. `ui.menu_active_fg` on `ui.menu_active_bg`) and
 * will clash or go invisible when drawn on `editor.bg` (notoriously in
 * high-contrast, where `ui.menu_active_fg` is pure black). So we only pull
 * from `editor.*` and `syntax.*`, and lean on bold + distinct syntax roles
 * to give each UI element its own visual identity.
 *
 * We don't need a client-side fallback chain: the core resolves every key a
 * theme leaves out (from its base theme, or from the key's fallback key), so
 * `resolve_theme_key` always returns a value for any key listed here — a stub
 * theme file can omit them.
 */
const colors = {
  sectionHeader: "syntax.keyword",
  fieldName:     "editor.fg",
  customValue:   "syntax.string",
  description:   "syntax.comment",
  footer:        "editor.line_number_fg",
  selectionBg:   "editor.selection_bg",
  divider:       "editor.line_number_fg",
  header:        "syntax.keyword",
  pickerLabel:   "editor.fg",
  pickerValue:   "syntax.constant",
  pickerFocusBg: "editor.selection_bg",
  filterText:    "syntax.function",
} as const satisfies Record<string, OverlayColorSpec>;

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
  [
    // Navigation
    ["Return", "theme_editor_enter"],
    ["Space", "theme_editor_enter"],
    ["Tab", "theme_editor_focus_tab"],
    ["S-Tab", "theme_editor_focus_shift_tab"],
    ["Up", "theme_editor_nav_up"],
    ["Down", "theme_editor_nav_down"],
    ["PageUp", "theme_editor_page_up"],
    ["PageDown", "theme_editor_page_down"],
    ["Left", "theme_editor_nav_left"],
    ["Right", "theme_editor_nav_right"],
    ["Escape", "theme_editor_escape"],
    ["/", "theme_editor_filter"],
    [SHORTCUTS.help, "theme_editor_show_help"],

    // Ctrl+ shortcuts (match common editor conventions)
    [SHORTCUTS.open, "theme_editor_open"],
    [SHORTCUTS.save, "theme_editor_save"],
    [SHORTCUTS.save_as, "theme_editor_save_as"],
    [SHORTCUTS.delete, "theme_editor_delete"],
    [SHORTCUTS.reload, "theme_editor_reload"],
    ["Delete", "theme_editor_reset"],
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
 * Convert a color value to an OverlayColorSpec for rendering.
 * Named colors (e.g. "Yellow") are sent as strings so the editor renders them
 * using native ANSI color codes, matching the actual theme rendering.
 * RGB arrays are passed through directly.
 */
function colorValueToOverlaySpec(value: ColorValue): OverlayColorSpec | null {
  if (Array.isArray(value) && value.length === 3) {
    return value as RGB;
  }
  if (typeof value === "string") {
    // For recognized named colors, send the name directly so the editor
    // uses native ANSI rendering (matching actual theme output)
    if (NAMED_COLORS[value] !== undefined) {
      return value;
    }
    return null;
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

function formatModifierValue(value: ModifierValue): string {
  return value.length === 0 ? "normal" : value.join(",");
}

/**
 * Check if a color is a named color (including special colors like Default/Reset)
 */
function isNamedColor(value: ColorValue): boolean {
  return typeof value === "string" && (value in NAMED_COLORS || SPECIAL_COLORS.includes(value));
}

/** A map key as one path segment: map keys are the user's (`Cargo.toml`)
 * and may hold the dot paths are split on. */
function encodeKey(key: string): string {
  return key.replace(/%/g, "%25").replace(/\./g, "%2E");
}

function decodeKey(part: string): string {
  return part.replace(/%2E/g, ".").replace(/%25/g, "%");
}

function pathParts(path: string): string[] {
  return path.split(".").map(decodeKey);
}

/** Remove a value, leaving its parents in place. */
function deleteNestedValue(obj: Record<string, unknown>, path: string): void {
  const parts = pathParts(path);
  let current: unknown = obj;
  for (let i = 0; i < parts.length - 1; i++) {
    if (!current || typeof current !== "object") return;
    current = (current as Record<string, unknown>)[parts[i]];
  }
  if (current && typeof current === "object") {
    delete (current as Record<string, unknown>)[parts[parts.length - 1]];
  }
}

/**
 * Get nested value from object
 */
function getNestedValue(obj: Record<string, unknown>, path: string): unknown {
  const parts = pathParts(path);
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
  const parts = pathParts(path);
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

// =============================================================================
// Style bundling
//
// On disk a styled key (every syntax key, and a few others such as
// `search.current_match_fg`) is either a bare color (`[r,g,b]` / `"Named"`)
// or a `{color, modifier}` bundle. Internally the editor works with a flat
// model — a color at `<key>` and its attributes at `<key>_modifier` — so the
// whole field pipeline treats attributes like any other value. These two
// helpers bridge the on-disk bundle and the flat in-memory form.
// =============================================================================

const MODIFIER_SUFFIX = "_modifier";

/** The sections whose values are colors (bare, or `{color, modifier}`
 * bundles), read off the schema: a section every field of which is a color.
 * Only these are bundled — `chrome` and `file_icons` hold other shapes, and
 * a `file_icons` entry has a `color` of its own without being a bundle. */
function colorSections(): string[] {
  return getThemeSections()
    .filter(sec => sec.fields.length > 0 && sec.fields.every(f => f.kind === "color"))
    .map(sec => sec.name);
}

/** Expand on-disk `{color, modifier}` bundles into flat color + `_modifier`,
 * in the color sections. */
function expandStyleBundles(themeData: Record<string, unknown>): void {
  for (const name of colorSections()) {
    const section = themeData[name];
    if (!section || typeof section !== "object" || Array.isArray(section)) continue;
    const obj = section as Record<string, unknown>;
    for (const [key, value] of Object.entries(obj)) {
      // A bundle is a plain object; a bare color is an array or string.
      if (!value || typeof value !== "object" || Array.isArray(value)) continue;
      const bundle = value as Record<string, unknown>;
      if (!("color" in bundle)) continue;
      obj[key] = bundle.color;
      if (Array.isArray(bundle.modifier) && bundle.modifier.length > 0) {
        obj[`${key}${MODIFIER_SUFFIX}`] = bundle.modifier;
      }
    }
  }
}

/**
 * Collapse a flat section (color + `<key>_modifier`) back to the on-disk
 * form: a `{color, modifier}` bundle when attributes are present, otherwise
 * the bare color. A `_modifier` key with no color key beside it is a key of
 * its own (e.g. `editor.selection_modifier`) and is kept as is.
 */
function collapseStyleBundles(section: Record<string, unknown>): Record<string, unknown> {
  const out: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(section)) {
    if (key.endsWith(MODIFIER_SUFFIX) && key.slice(0, -MODIFIER_SUFFIX.length) in section) {
      continue;
    }
    const modifier = section[`${key}${MODIFIER_SUFFIX}`];
    if (Array.isArray(modifier) && modifier.length > 0) {
      out[key] = { color: value, modifier };
    } else {
      out[key] = value;
    }
  }
  return out;
}

/**
 * Load theme registry and populate state.themeRegistry + state.builtinKeys.
 *
 * Themes come pre-merged from the Rust registry (builtins + user dir +
 * packages + bundles), keyed by canonical registry key with `_key`/`_pack`
 * metadata baked in — same data the native `select_theme` prompt sees.
 */
async function loadThemeRegistry(): Promise<void> {
  const themes = editor.getAllThemes() as Record<string, Record<string, unknown>>;

  state.themeRegistry = new Map();
  state.builtinKeys = new Set();

  for (const [key, data] of Object.entries(themes)) {
    const name = (data?.name as string) || key;
    const pack = (data?._pack as string) || "";

    state.themeRegistry.set(key, { name, pack });

    // Builtin themes have an empty pack; user themes start with "user"
    if (!pack || (!pack.startsWith("user") && !pack.startsWith("pkg"))) {
      state.builtinKeys.add(key);
    }
  }

  editor.debug(`[theme_editor] loadThemeRegistry: loaded ${state.themeRegistry.size} themes (${state.builtinKeys.size} builtin)`);
}

/**
 * Load theme data by key from the in-memory theme registry.
 */
function loadThemeFile(key: string): Record<string, unknown> | null {
  try {
    const data = editor.getThemeData(key) as Record<string, unknown> | null;
    if (data) expandStyleBundles(data);
    return data;
  } catch (e) {
    editor.debug(`[theme_editor] Failed to load theme data for '${key}': ${e}`);
    return null;
  }
}

// =============================================================================
// Field Building
// =============================================================================

const TREE_ROW_TYPES = ["tree-field", "tree-section", "tree-group", "tree-entry", "tree-add"];

/** Whether a row is a header that expands and collapses. */
function isContainer(field: ThemeField): boolean {
  return field.isSection || ["group", "map", "entry"].includes(field.def.kind);
}

/** Whether a field's value may be removed, so it inherits again: anything
 * under `chrome` or `file_icons`, which a theme names only to change. The
 * color sections' keys stay, so a saved theme keeps naming them all. */
function isResettable(field: ThemeField): boolean {
  return /^(chrome|file_icons)\./.test(field.path) && field.def.kind !== "add" && field.def.kind !== "map";
}

/** Whether a field's own name matches the filter. */
function nameMatches(def: ThemeFieldDef, filter: string): boolean {
  return def.key.toLowerCase().includes(filter) || def.displayName.toLowerCase().includes(filter);
}

/** Whether a field at `path`, or anything under it, matches the filter: a
 * group's children, a map's entries by key. */
function treeMatches(def: ThemeFieldDef, path: string, filter: string): boolean {
  if (nameMatches(def, filter)) return true;
  if (def.kind === "group") {
    return (def.children ?? []).some(c => treeMatches(c, `${path}.${encodeKey(c.key)}`, filter));
  }
  if (def.kind === "map") {
    return mapKeys(path).some(k => k.toLowerCase().includes(filter));
  }
  return false;
}

/** The keys a map holds in the theme being edited. */
function mapKeys(path: string): string[] {
  const stored = getNestedValue(state.themeData, path);
  return stored && typeof stored === "object" ? Object.keys(stored as object).sort() : [];
}

/** What an unset value draws: the built-in icon's glyph or color at the
 * same path, or the schema's default for the option. */
function inheritedValue(def: ThemeFieldDef, path: string): unknown {
  return builtinValue(path) ?? def.defaultValue;
}

/** The default a leaf falls back to when nothing names it at all. */
function fallbackValue(def: ThemeFieldDef): ThemeFieldValue {
  return def.kind === "modifier" ? [] : def.kind === "toggle" ? true : def.kind === "text" ? "" : [128, 128, 128];
}

/**
 * Append the rows for `defs` under `parent`: leaves with their values, and
 * containers (groups, maps, map entries) as headers whose rows follow while
 * they are expanded. Expansion is remembered by path in
 * `state.expandedSections`; a filter opens every container on its way to a
 * match.
 */
function pushFields(out: ThemeField[], defs: ThemeFieldDef[], parent: string, depth: number, filter: string): void {
  for (const def of defs) {
    const path = `${parent}.${encodeKey(def.key)}`;
    if (filter && !treeMatches(def, path, filter)) continue;
    const stored = getNestedValue(state.themeData, path);
    if (def.kind === "group" || def.kind === "map") {
      const expanded = state.expandedSections.has(path) || !!filter;
      out.push({ def, value: [], path, depth, isSection: false, expanded });
      if (!expanded) continue;
      // A container whose own name matched shows all it holds; otherwise
      // only what matched inside it.
      const inner = filter && !nameMatches(def, filter) ? filter : "";
      if (def.kind === "group") {
        pushFields(out, def.children ?? [], path, depth + 1, inner);
        continue;
      }
      // A map: one header per entry the theme has, then the row that adds one.
      const entries = mapKeys(path).filter(k => !inner || k.toLowerCase().includes(inner));
      for (const key of entries) {
        const entryPath = `${path}.${encodeKey(key)}`;
        const entryDef: ThemeFieldDef = {
          key,
          displayName: key,
          description: def.description,
          section: def.section,
          kind: "entry",
          children: def.entry,
          builtin: def.builtin,
        };
        const open = state.expandedSections.has(entryPath);
        out.push({ def: entryDef, value: [], path: entryPath, depth: depth + 1, isSection: false, expanded: open });
        if (open) pushFields(out, def.entry ?? [], entryPath, depth + 2, "");
      }
      out.push({
        def: { key: "+", displayName: "Add…", description: def.description, section: def.section, kind: "add", builtin: def.builtin },
        value: [],
        path: `${path}.+`,
        depth: depth + 1,
        isSection: false,
      });
      continue;
    }
    // An unset value edits from what it draws — the built-in icon's glyph
    // or color, the option's default — so confirming a prompt or a picker
    // unchanged never writes something else.
    const value = stored ?? inheritedValue(def, path) ?? fallbackValue(def);
    out.push({ def, value: value as ThemeFieldValue, path, depth, isSection: false });
  }
}

/**
 * Build visible fields list based on expanded sections
 */
function buildVisibleFields(): ThemeField[] {
  const fields: ThemeField[] = [];
  const themeSections = getThemeSections();
  const filter = state.filterText.toLowerCase();

  for (const section of themeSections) {
    // A section with nothing the editor can edit is left out of the tree;
    // Save still writes it back untouched.
    if (section.fields.length === 0) continue;
    const expanded = state.expandedSections.has(section.name);

    // When filtering, check if section or any of its fields match
    if (filter) {
      const sectionMatches = section.name.toLowerCase().includes(filter) ||
        section.displayName.toLowerCase().includes(filter);
      const anyFieldMatches = section.fields.some(f =>
        treeMatches(f, `${section.name}.${encodeKey(f.key)}`, filter));
      if (!sectionMatches && !anyFieldMatches) continue;
    }

    // Section header
    fields.push({
      def: {
        key: section.name,
        displayName: section.displayName,
        description: section.description,
        section: section.name,
        kind: "color",
      },
      value: [0, 0, 0], // Placeholder
      path: section.name,
      depth: 0,
      isSection: true,
      expanded,
    });

    // Section fields: all of them when the section's own name matched.
    if (expanded) {
      const sectionMatches = section.name.toLowerCase().includes(filter)
        || section.displayName.toLowerCase().includes(filter);
      pushFields(fields, section.fields, section.name, 1, filter && !sectionMatches ? filter : "");
    }
  }

  return fields;
}

// =============================================================================
// UI Building
// =============================================================================

// =============================================================================
// Tree Panel Builder (Left)
// =============================================================================

interface TreeLine {
  text: string;
  type: string;
  index?: number;
  path?: string;
  selected?: boolean;
  colorValue?: ColorValue;
  fieldKind?: FieldKind;
  /** A map entry's icon glyph, drawn in `colorValue`. */
  glyph?: string;
}

/** The tree panel's width: its share of the pane the editor opened in
 * (`listSplits` knows that pane's width; the viewport is the terminal's). */
function treeWidth(): number {
  const splits = editor.listSplits();
  // The tree panel itself, when the group's panels are listed; else the
  // pane the editor opened in, of which the tree takes its share.
  const tree = splits.find(sp => sp.bufferId === state.panelBuffers["tree"]);
  if (tree) return Math.max(30, tree.width - 2);
  // The editor opens in a horizontal split, as wide as the pane it split
  // from — which the snapshot may hold before it holds the new pane.
  const pane = splits.find(sp => sp.splitId === state.editorPaneId)
    ?? splits.find(sp => sp.splitId === state.sourceSplitId);
  const width = pane ? pane.width : state.viewportWidth;
  return Math.max(30, Math.floor(width * TREE_RATIO) - 2);
}

/** The built-in file icons (`editor.getBuiltinFileIcons`), fetched once. */
let builtinIcons: Record<string, unknown> | null = null;

/** What an unset `file_icons` value falls back to: the built-in icon's
 * glyph or color at the same path, if the table has one. */
function builtinValue(path: string): unknown {
  if (!path.startsWith("file_icons.")) return undefined;
  if (builtinIcons === null) {
    builtinIcons = (editor.getBuiltinFileIcons() as Record<string, unknown>) ?? {};
  }
  const value = getNestedValue({ file_icons: builtinIcons }, path);
  // A folder icon has no color of its own: it takes the explorer's
  // directory color (`view::file_icons::resolve_folder`).
  if (value === null && /^file_icons\.folder(_open)?\.color$/.test(path)) {
    return getNestedValue(state.themeData, "ui.file_explorer_directory_fg") ?? undefined;
  }
  return value ?? undefined;
}

/** One row of the tree for a field below a section header. */
function fieldLine(field: ThemeField, i: number, isSelected: boolean, width: number): TreeLine {
  const sel = isSelected && state.focusPanel === "tree" ? "▸" : " ";
  const indent = "  ".repeat(Math.max(0, field.depth - 1));
  const kind = field.def.kind;
  const base = { index: i, path: field.path, selected: isSelected, fieldKind: kind };
  if (kind === "group" || kind === "map") {
    const icon = field.expanded ? "▼" : ">";
    return { ...base, text: `  ${indent}${sel}${icon} ${field.def.key}`, type: "tree-group" };
  }
  if (kind === "entry") {
    const icon = field.expanded ? "▼" : ">";
    // The entry's icon as it will be drawn: its own glyph or the built-in.
    const glyph = (getNestedValue(state.themeData, `${field.path}.glyph`) as string | undefined)
      ?? (builtinValue(`${field.path}.glyph`) as string | undefined) ?? "";
    const color = (getNestedValue(state.themeData, `${field.path}.color`) as ColorValue | undefined)
      ?? (builtinValue(`${field.path}.color`) as ColorValue | undefined);
    return {
      ...base,
      text: `  ${indent}${sel}${icon} ${field.def.key}  ${glyph}`,
      type: "tree-entry",
      glyph,
      colorValue: color ?? undefined,
    };
  }
  if (kind === "add") {
    return { ...base, text: `  ${indent}${sel} + Add…`, type: "tree-add" };
  }

  // A leaf. Layout: indent, marker, name padded, swatch/marker, value.
  // The name takes what the value (`#RRGGBB +bold,italic`) leaves.
  const valueW = Math.max(9, Math.min(24, Math.floor(width * 0.35)));
  const nameW = Math.max(8, width - valueW - 9 - indent.length);
  const name = field.def.key.length > nameW ? field.def.key.slice(0, nameW - 1) + "…" : field.def.key;
  const stored = getNestedValue(state.themeData, field.path);
  const inherited = stored === undefined ? inheritedValue(field.def, field.path) : undefined;
  // Where an unset value's draw comes from: a built-in icon, or the
  // option's default.
  const source = builtinValue(field.path) !== undefined ? "built-in" : "default";
  let valueText: string;
  if (kind === "toggle") {
    valueText = (field.value ? "on" : "off") + (stored === undefined ? ` (${source})` : "");
  } else if (kind === "text") {
    valueText = stored === undefined
      ? (inherited !== undefined ? `${JSON.stringify(inherited)} (${source})` : "(unset)")
      : JSON.stringify(field.value);
  } else if (stored === undefined && kind === "color") {
    valueText = inherited ? `${formatColorValue(inherited as ColorValue)} (${source})` : "(unset)";
  } else {
    valueText = formatColorValue(field.value as ColorValue);
    // A styled color's attributes ride on its row: `#RRGGBB +bold,italic`.
    const mods = getNestedValue(state.themeData, `${field.path}${MODIFIER_SUFFIX}`) as ModifierValue | undefined;
    if (field.def.styled && mods && mods.length > 0) valueText += ` +${mods.join(",")}`;
  }
  const valueStr = valueText.length > valueW ? valueText.slice(0, valueW - 1) + "…" : valueText;
  const swatch = kind === "color"
    ? (stored !== undefined ? field.value as ColorValue : (inherited as ColorValue | undefined))
    : undefined;
  // No swatch for a color with nothing to show: a gray block would read as
  // a color the theme picked.
  const marker = kind === "toggle"
    ? (field.value ? " ●" : " ○")
    : kind === "text" ? "Ab" : swatch === undefined || swatch === null ? "··" : "██";
  return {
    ...base,
    text: `  ${indent}${sel} ${name.padEnd(nameW)} ${marker} ${valueStr}`,
    type: "tree-field",
    colorValue: swatch ?? undefined,
  };
}

function buildTreeLines(): TreeLine[] {
  const lines: TreeLine[] = [];
  // Asked of the host once per build, not once per row.
  const width = treeWidth();

  // Header
  const modMarker = state.hasChanges ? " [*]" : "";
  lines.push({
    text: `Theme Editor: ${state.themeName}${modMarker}`,
    type: "header",
  });

  // Separator (adapt to panel width)
  lines.push({ text: "─".repeat(Math.max(10, width - 2)), type: "separator" });

  // Filter
  if (state.filterText) {
    lines.push({
      text: `Filter: [${state.filterText}]`,
      type: "filter",
    });
    lines.push({ text: "─".repeat(Math.max(10, width - 2)), type: "separator" });
  }

  // Build visible fields
  state.visibleFields = buildVisibleFields();

  // Clamp selectedIndex
  if (state.selectedIndex >= state.visibleFields.length) {
    state.selectedIndex = Math.max(0, state.visibleFields.length - 1);
  }

  for (let i = 0; i < state.visibleFields.length; i++) {
    const field = state.visibleFields[i];
    const isSelected = i === state.selectedIndex;

    if (field.isSection) {
      const icon = field.expanded ? "▼" : ">";
      const sel = isSelected && state.focusPanel === "tree" ? "▸" : " ";
      lines.push({
        text: `${sel}${icon} ${field.def.displayName}`,
        type: "tree-section",
        index: i,
        path: field.path,
        selected: isSelected,
      });
    } else {
      lines.push(fieldLine(field, i, isSelected, width));
    }
  }

  return lines;
}

// =============================================================================
// Picker Panel Builder (Right)
// =============================================================================

interface PickerLine {
  text: string;
  type: string;
  namedRow?: number;
  paletteRow?: number;
  previewLineIdx?: number;
}

function buildPickerLines(): PickerLine[] {
  const lines: PickerLine[] = [];
  const field = state.visibleFields[state.selectedIndex];

  if (!field || isContainer(field) || field.def.kind === "add") {
    // A header (section, group, map, map entry) or the add row: what it is,
    // and what Enter and Delete do there.
    if (field) {
      lines.push({ text: `${field.def.displayName}`, type: "picker-title" });
      lines.push({ text: `"${field.def.description}"`, type: "picker-desc" });
    } else {
      lines.push({ text: "No field selected", type: "picker-title" });
    }
    lines.push({ text: "─".repeat(RIGHT_WIDTH() - 2), type: "picker-separator" });
    const hint = !field || field.isSection
      ? "Select a color field to edit"
      : field.def.kind === "add"
        ? "Enter: add an entry (built-in names are suggested)"
        : field.def.kind === "entry"
          ? "Enter: open. Delete: remove this entry"
          : isResettable(field)
            ? "Enter: open. Delete: reset all of it to Fresh's defaults"
            : "Enter: open";
    lines.push({ text: hint, type: "picker-desc" });
    return lines;
  }

  // Field title
  lines.push({ text: `${field.path} - ${field.def.displayName}`, type: "picker-title" });
  lines.push({ text: `"${field.def.description}"`, type: "picker-desc" });
  lines.push({ text: "─".repeat(RIGHT_WIDTH() - 2), type: "picker-separator" });

  if (field.def.kind === "modifier") {
    const modifiers = field.value as ModifierValue;
    lines.push({ text: `Attributes: ${formatModifierValue(modifiers)}`, type: "picker-hex" });
    lines.push({ text: "", type: "picker-blank" });
    lines.push({ text: "Available: bold, italic, underlined, dim, reversed", type: "picker-label" });
    lines.push({ text: "Press Enter or Space to edit the comma-separated list.", type: "picker-desc" });
    lines.push({ text: "Use an empty value or 'normal' to clear attributes.", type: "picker-desc" });
    return lines;
  }

  if (field.def.kind === "text") {
    lines.push({ text: `Text: ${JSON.stringify(field.value)}`, type: "picker-hex" });
    lines.push({ text: "", type: "picker-blank" });
    lines.push({ text: "Press Enter or Space to type it, or pick a glyph:", type: "picker-desc" });
    // An icon's glyph is picked from the built-in icons; a chrome glyph
    // from separators and caps.
    const choices = field.path.startsWith("file_icons.")
      ? builtinGlyphSuggestions().slice(0, 24).map(sg => sg.value ?? "")
      : SEPARATOR_GLYPHS.map(g => g.value || "∅");
    lines.push({ text: ` ${choices.join("  ")}`, type: "picker-label" });
    lines.push({ text: "The editor shows each change live, before saving.", type: "picker-desc" });
    return lines;
  }

  if (field.def.kind === "toggle") {
    lines.push({ text: `Value: ${field.value ? "on" : "off"}`, type: "picker-hex" });
    lines.push({ text: "", type: "picker-blank" });
    lines.push({ text: "Press Enter or Space to toggle.", type: "picker-desc" });
    lines.push({ text: "Off removes the element and the cells it takes.", type: "picker-desc" });
    lines.push({ text: "The editor shows each change live, before saving.", type: "picker-desc" });
    return lines;
  }

  // Color value display
  const isNamed = typeof field.value === "string" && NAMED_COLORS[field.value] !== undefined;
  if (isNamed) {
    lines.push({ text: `Color: ${field.value} (terminal native)`, type: "picker-hex" });
  } else {
    const colorStr = formatColorValue(field.value as ColorValue);
    const rgb = parseColorToRgb(field.value as ColorValue);
    let valueLine = `Hex: ${colorStr}`;
    if (rgb) {
      valueLine += `     RGB: ${rgb[0]}, ${rgb[1]}, ${rgb[2]}`;
    }
    lines.push({ text: valueLine, type: "picker-hex" });
  }
  // A styled color's attributes, on the color's own panel rather than a
  // second row in the tree (↑ from the value, Enter to edit).
  if (field.def.styled) {
    const mods = (getNestedValue(state.themeData, `${field.path}${MODIFIER_SUFFIX}`) ?? []) as ModifierValue;
    lines.push({ text: `Attributes: ${formatModifierValue(mods)}`, type: "picker-attrs" });
  }

  lines.push({ text: "", type: "picker-blank" });

  // Named Colors section
  lines.push({ text: "Named Colors:", type: "picker-label" });
  for (let row = 0; row < NAMED_COLOR_GRID.length; row++) {
    let rowText = "";
    for (const item of NAMED_COLOR_GRID[row]) {
      rowText += " " + item.display.padEnd(8);
    }
    lines.push({ text: rowText, type: "picker-named-row", namedRow: row });
  }

  lines.push({ text: "", type: "picker-blank" });

  // Extended Color Palette section
  lines.push({ text: "Color Palette:", type: "picker-label" });
  const palette = getExtendedPalette();
  for (let row = 0; row < PALETTE_ROWS; row++) {
    let rowText = "";
    for (let col = 0; col < PALETTE_COLS; col++) {
      rowText += (col === 0 ? " " : " ") + "██";
    }
    lines.push({ text: rowText, type: "picker-palette-row", paletteRow: row });
  }

  return lines;
}

// =============================================================================
// UI Building - Two Panel Merge
// =============================================================================

/**
 * Compute inline style for a left-panel tree entry
 */
function styleForLeftEntry(item: TreeLine | undefined): { style?: Partial<OverlayOptions>; inlineOverlays?: InlineOverlay[] } {
  if (!item) return {};
  const type = item.type;
  if (type === "header") {
    return { style: { fg: colors.header, bold: true } };
  } else if (type === "separator") {
    return { style: { fg: colors.divider } };
  } else if (type === "filter") {
    return { style: { fg: colors.filterText } };
  } else if (type === "tree-section") {
    return { style: { fg: colors.sectionHeader, bold: true } };
  } else if (type === "tree-group") {
    return { style: { fg: colors.sectionHeader } };
  } else if (type === "tree-add") {
    return { style: { fg: colors.description } };
  } else if (type === "tree-entry") {
    // The entry's icon in the color it will be drawn in.
    const text = " " + item.text;
    const glyph = item.glyph ?? "";
    const at = glyph ? text.lastIndexOf(glyph) : -1;
    const spec = item.colorValue !== undefined ? colorValueToOverlaySpec(item.colorValue) : null;
    if (at < 0 || !spec) return { style: { fg: colors.fieldName } };
    const start = getUtf8ByteLength(text.substring(0, at));
    return {
      inlineOverlays: [
        { start: 0, end: start, style: { fg: colors.fieldName } },
        { start, end: start + getUtf8ByteLength(glyph), style: { fg: spec } },
      ],
    };
  } else if (type === "tree-field") {
    const inlines: InlineOverlay[] = [];
    const text = " " + item.text; // matches leftText construction below
    // The value runs to the end of the row's own text: rows are as wide as
    // the tree panel, which is not a fixed width.
    const paddedLen = getUtf8ByteLength(text);
    const colorValue = item.colorValue;
    const swatchIdx = colorValue !== undefined ? text.indexOf("██") : -1;

    // For the swatch, use the native color representation to ensure it matches
    // how the theme actually renders: named colors (e.g. "Yellow") should use
    // the terminal's native ANSI color, not an RGB approximation.
    const swatchColor: OverlayColorSpec | null = colorValue !== undefined
      ? (typeof colorValue === "string" && NAMED_COLORS[colorValue] !== undefined
        ? colorValue as OverlayColorSpec  // Send named color string directly
        : parseColorToRgb(colorValue))    // Send RGB for array values
      : null;

    if (swatchColor && swatchIdx >= 0) {
      const swatchStart = getUtf8ByteLength(text.substring(0, swatchIdx));
      const swatchEnd = swatchStart + getUtf8ByteLength("██");
      // Non-overlapping segments: fieldName | swatch | value
      inlines.push({ start: 0, end: swatchStart, style: { fg: colors.fieldName } });
      inlines.push({ start: swatchStart, end: swatchEnd, style: { fg: swatchColor, bg: swatchColor } });
      const valueStart = swatchEnd + getUtf8ByteLength(" ");
      if (valueStart < paddedLen) {
        inlines.push({ start: valueStart, end: paddedLen, style: { fg: colors.customValue } });
      }
      return { inlineOverlays: inlines };
    }
    // No swatch — use entry-level style
    return { style: { fg: colors.fieldName } };
  }
  return {};
}

/**
 * Compute inline style for a right-panel picker entry
 */
function styleForRightEntry(item: PickerLine | undefined): { style?: Partial<OverlayOptions>; inlineOverlays?: InlineOverlay[] } {
  if (!item) return {};
  const type = item.type;
  if (type === "picker-title") {
    return { style: { fg: colors.header, bold: true } };
  } else if (type === "picker-desc") {
    return { style: { fg: colors.description } };
  } else if (type === "picker-separator") {
    return { style: { fg: colors.divider } };
  } else if (type === "picker-hex") {
    return { style: { fg: colors.pickerValue } };
  } else if (type === "picker-attrs") {
    return { style: { fg: colors.pickerLabel } };
  } else if (type === "picker-label") {
    return { style: { fg: colors.pickerLabel } };
  } else if (type === "picker-named-row") {
    const namedRow = item.namedRow!;
    const gridRow = NAMED_COLOR_GRID[namedRow];
    if (!gridRow) return {};
    const inlines: InlineOverlay[] = [];
    // Entry text is " " + item.text. Byte positions are relative to that.
    const bytePos = getUtf8ByteLength(" "); // the prepended " "
    let innerPos = 0;
    for (let col = 0; col < gridRow.length; col++) {
      const cellItem = gridRow[col];
      const cellText = " " + cellItem.display.padEnd(8);
      const cellLen = getUtf8ByteLength(cellText);
      const cellStart = bytePos + getUtf8ByteLength(item.text.substring(0, innerPos));
      const cellEnd = cellStart + cellLen;
      if (cellItem.rgb) {
        inlines.push({ start: cellStart, end: cellEnd, style: { fg: cellItem.rgb }, properties: { namedCol: col } });
      } else {
        inlines.push({ start: cellStart, end: cellEnd, style: { fg: colors.pickerLabel }, properties: { namedCol: col } });
      }
      innerPos += cellText.length;
    }
    return { inlineOverlays: inlines.length > 0 ? inlines : undefined };
  } else if (type === "picker-palette-row") {
    const paletteRow = item.paletteRow!;
    const palette = getExtendedPalette();
    const rowColors = palette[paletteRow];
    if (!rowColors) return {};
    const inlines: InlineOverlay[] = [];
    // entry text = " " + item.text
    // item.text = " ██ ██ ██..." (starts with " " then "██" pairs)
    let bytePos = getUtf8ByteLength(" "); // prepended " "
    let innerPos = 0;
    for (let col = 0; col < PALETTE_COLS; col++) {
      const prefix = col === 0 ? " " : " ";
      innerPos += prefix.length;
      bytePos = getUtf8ByteLength(" ") + getUtf8ByteLength(item.text.substring(0, innerPos));
      const swatchLen = getUtf8ByteLength("██");
      const rgb = rowColors[col];
      inlines.push({ start: bytePos, end: bytePos + swatchLen, style: { fg: rgb, bg: rgb }, properties: { paletteCol: col } });
      innerPos += 2; // "██" is 2 JS chars
    }
    return { inlineOverlays: inlines.length > 0 ? inlines : undefined };
  }
  return {};
}

function buildDisplayEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  const allLeftLines = buildTreeLines();
  const rightLines = buildPickerLines();

  // Virtual scrolling: only show a viewport-sized slice of tree lines
  // Reserve 2 rows for status bar + hints, 1 for possible scroll indicator
  const treeVisibleRows = Math.max(8, state.viewportHeight - 2);

  // Adjust scroll offset to keep selectedIndex visible
  // Find which line index in allLeftLines corresponds to selectedIndex
  let selectedLineIdx = -1;
  for (let i = 0; i < allLeftLines.length; i++) {
    if (allLeftLines[i].index === state.selectedIndex && allLeftLines[i].selected) {
      selectedLineIdx = i;
      break;
    }
  }
  if (selectedLineIdx >= 0) {
    if (selectedLineIdx < state.treeScrollOffset) {
      state.treeScrollOffset = selectedLineIdx;
    }
    if (selectedLineIdx >= state.treeScrollOffset + treeVisibleRows) {
      state.treeScrollOffset = selectedLineIdx - treeVisibleRows + 1;
    }
  }
  // Clamp scroll offset
  const maxOffset = Math.max(0, allLeftLines.length - treeVisibleRows);
  if (state.treeScrollOffset > maxOffset) state.treeScrollOffset = maxOffset;
  if (state.treeScrollOffset < 0) state.treeScrollOffset = 0;

  const leftLines = allLeftLines.slice(state.treeScrollOffset, state.treeScrollOffset + treeVisibleRows);

  // Add scroll indicators if tree is scrollable
  const canScrollUp = state.treeScrollOffset > 0;
  const canScrollDown = state.treeScrollOffset + treeVisibleRows < allLeftLines.length;

  const maxRows = Math.max(leftLines.length, rightLines.length, 8);
  let byteOffset = 0;

  for (let i = 0; i < maxRows; i++) {
    const leftItem = leftLines[i];
    const rightItem = rightLines[i];

    // Left side (padded to LEFT_WIDTH)
    const leftText = leftItem ? (" " + leftItem.text) : "";
    const leftPadded = leftText.padEnd(LEFT_WIDTH);
    const leftStyle = styleForLeftEntry(leftItem);
    entries.push({
      text: leftPadded,
      properties: {
        type: leftItem?.type || "blank",
        index: leftItem?.index,
        path: leftItem?.path,
        selected: leftItem?.selected,
        colorValue: leftItem?.colorValue,
      },
      style: leftStyle.style,
      inlineOverlays: leftStyle.inlineOverlays,
    });
    byteOffset += getUtf8ByteLength(leftPadded);

    // Divider
    const dividerText = "│";
    entries.push({ text: dividerText, properties: { type: "divider" }, style: { fg: colors.divider } });
    byteOffset += getUtf8ByteLength(dividerText);

    // Right side
    const rightText = rightItem ? (" " + rightItem.text) : "";
    const rightStyle = styleForRightEntry(rightItem);
    entries.push({
      text: rightText,
      properties: {
        type: rightItem?.type || "blank",
        namedRow: rightItem?.namedRow,
        paletteRow: rightItem?.paletteRow,
        previewLineIdx: rightItem?.previewLineIdx,
      },
      style: rightStyle.style,
      inlineOverlays: rightStyle.inlineOverlays,
    });
    byteOffset += getUtf8ByteLength(rightText);

    // Newline
    entries.push({ text: "\n", properties: { type: "newline" } });
    byteOffset += 1;
  }

  // Status bar (full width)
  entries.push({
    text: "─".repeat(100) + "\n",
    properties: { type: "status-separator" },
    style: { fg: colors.divider },
  });

  // Context-sensitive key hints
  let hints: string;
  const scrollHint = canScrollUp || canScrollDown
    ? ` [${canScrollUp ? "▲" : " "}${canScrollDown ? "▼" : " "}]`
    : "";
  if (state.focusPanel === "tree") {
    hints = " ↑↓ Navigate  Tab Switch Panel  Enter Edit  /Filter  Ctrl+S Save  Esc Close" + scrollHint;
  } else {
    hints = " ↑↓←→ Navigate  Tab Switch Panel  Enter Apply  Esc Back to Tree" + scrollHint;
  }
  entries.push({
    text: hints + "\n",
    properties: { type: "status-bar" },
    style: { fg: colors.footer },
  });

  return entries;
}

/**
 * Helper to add a colored overlay (foreground color)
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
  bgColor: OverlayColorSpec
): void {
  editor.addOverlay(bufferId, "theme-sel", start, end, { bg: bgColor });
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
 * Apply selection-only highlighting (cheap — just the selected row background
 * and picker focus highlights). Only touches the "theme-selection" namespace.
 */
function applySelectionHighlighting(cachedEntries?: TextPropertyEntry[]): void {
  if (state.bufferId === null) return;

  const bufferId = state.bufferId;
  editor.clearNamespace(bufferId, "theme-sel");

  const entries = cachedEntries || buildDisplayEntries();
  let byteOffset = 0;

  for (const entry of entries) {
    const text = entry.text;
    const textLen = getUtf8ByteLength(text);
    const props = entry.properties as Record<string, unknown>;
    const entryType = props.type as string;

    if (TREE_ROW_TYPES.includes(entryType)) {
      if (props.selected as boolean) {
        // For tree-field entries, split the highlight around the swatch (██) bytes
        // so the inline swatch color overlay (fg+bg) isn't overridden by selection bg
        const swatchIdx = text.indexOf("██");
        if (entryType === "tree-field" && swatchIdx >= 0) {
          const swatchByteStart = byteOffset + getUtf8ByteLength(text.substring(0, swatchIdx));
          const swatchByteEnd = swatchByteStart + getUtf8ByteLength("██");
          addBackgroundHighlight(bufferId, byteOffset, swatchByteStart, colors.selectionBg);
          addBackgroundHighlight(bufferId, swatchByteEnd, byteOffset + textLen, colors.selectionBg);
        } else {
          addBackgroundHighlight(bufferId, byteOffset, byteOffset + textLen, colors.selectionBg);
        }
      }
    } else if (entryType === "picker-hex") {
      if (state.focusPanel === "picker" && state.pickerFocus.type === "hex-input") {
        addBackgroundHighlight(bufferId, byteOffset, byteOffset + textLen, colors.pickerFocusBg);
      }
    } else if (entryType === "picker-attrs") {
      if (state.focusPanel === "picker" && state.pickerFocus.type === "attributes") {
        addBackgroundHighlight(bufferId, byteOffset, byteOffset + textLen, colors.pickerFocusBg);
      }
    } else if (entryType === "picker-named-row") {
      if (state.focusPanel === "picker" && state.pickerFocus.type === "named-colors") {
        const namedRow = props.namedRow as number;
        const gridRow = NAMED_COLOR_GRID[namedRow];
        if (gridRow) {
          let pos = byteOffset + getUtf8ByteLength(" ");
          for (let col = 0; col < gridRow.length; col++) {
            const item = gridRow[col];
            const cellText = " " + item.display.padEnd(8);
            const cellLen = getUtf8ByteLength(cellText);
            const flatIdx = namedRow * NAMED_COLORS_PER_ROW + col;
            if (state.pickerFocus.index === flatIdx) {
              editor.addOverlay(bufferId, "theme-sel", pos, pos + cellLen, { bg: colors.pickerFocusBg });
            }
            pos += cellLen;
          }
        }
      }
    } else if (entryType === "picker-palette-row") {
      if (state.focusPanel === "picker" && state.pickerFocus.type === "palette") {
        const paletteRow = props.paletteRow as number;
        const palette = getExtendedPalette();
        const rowColors = palette[paletteRow];
        if (rowColors) {
          let pos = byteOffset + getUtf8ByteLength(" ");
          for (let col = 0; col < PALETTE_COLS; col++) {
            const prefix = col === 0 ? " " : " ";
            pos += getUtf8ByteLength(prefix);
            const swatchLen = getUtf8ByteLength("██");
            if (state.pickerFocus.row === paletteRow && state.pickerFocus.col === col) {
              editor.addOverlay(bufferId, "theme-sel", pos, pos + swatchLen, {
                bg: [255, 255, 255],
                fg: rowColors[col],
              });
            }
            pos += swatchLen;
          }
        }
      }
    }

    byteOffset += textLen;
  }

}

// Guard to suppress cursor_moved handler during programmatic updates
let isUpdatingDisplay = false;

/**
 * Full display update — rebuilds content and all overlays.
 * Use for structural changes (open, section toggle, color edit, filter).
 */
// --- Buffer Group panel content builders ---

function buildTreePanelEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  const allLeftLines = buildTreeLines();
  for (const item of allLeftLines) {
    const leftStyle = styleForLeftEntry(item);
    entries.push({
      text: " " + item.text + "\n",
      properties: {
        type: item.type,
        index: item.index,
        path: item.path,
        selected: item.selected,
        colorValue: item.colorValue,
      },
      style: leftStyle.style,
      inlineOverlays: leftStyle.inlineOverlays,
    });
  }
  return entries;
}

function buildPickerPanelEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  const rightLines = buildPickerLines();
  for (const item of rightLines) {
    const rightStyle = styleForRightEntry(item);
    entries.push({
      text: " " + item.text + "\n",
      properties: {
        type: item.type,
        namedRow: item.namedRow,
        paletteRow: item.paletteRow,
        previewLineIdx: item.previewLineIdx,
      },
      style: rightStyle.style,
      inlineOverlays: rightStyle.inlineOverlays,
    });
  }
  return entries;
}

/** Render the footer hints into the footer widget panel. The hint
 * string follows `parseHintString`'s convention (tokens separated
 * by two spaces; each token is `<keys>:<label>` or `<keys> <label>`)
 * so the host's HintBar styles the keys with `ui.help_key_fg`
 * consistently with every other plugin's footer. */
function renderThemeEditorFooter(): void {
  if (state.footerPanel === null) return;
  const hintString =
    "↑↓:Navigate  Tab:Switch Panel  Enter:Edit  Del:Reset  /:Filter  Ctrl+S:Save  Esc:Close";
  state.footerPanel.set(hintBar(parseHintString(hintString)));
}

function updateDisplay(): void {
  isUpdatingDisplay = true;
  previewEdits();

  // Always refresh viewport dimensions
  const viewport = editor.getViewport();
  if (viewport) {
    state.viewportHeight = viewport.height;
    state.viewportWidth = viewport.width;
  }

  // Buffer group mode: write to each panel separately
  if (state.groupId !== null) {
    editor.setPanelContent(state.groupId, "tree", buildTreePanelEntries());
    editor.setPanelContent(state.groupId, "picker", buildPickerPanelEntries());
    renderThemeEditorFooter();

    // Keep the selected tree row in view. The plugin's `selectedIndex`
    // navigation doesn't move the buffer cursor, so without this the
    // core-driven panel viewport would stay at the top even after many
    // Down-arrow presses, causing the `▸` marker to scroll off-screen
    // for sections with many fields.
    const treeBufferId = state.panelBuffers["tree"];
    if (typeof treeBufferId === "number") {
      const treeLines = buildTreeLines();
      let selectedLine = -1;
      for (let i = 0; i < treeLines.length; i++) {
        if (treeLines[i].index === state.selectedIndex && treeLines[i].selected) {
          selectedLine = i;
          break;
        }
      }
      if (selectedLine >= 0) {
        editor.scrollBufferToLine(treeBufferId, selectedLine);
      }
    }

    isUpdatingDisplay = false;
    return;
  }

  if (state.bufferId === null) {
    isUpdatingDisplay = false;
    return;
  }

  const entries = buildDisplayEntries();

  // Clear selection overlays BEFORE replacing content to prevent stale
  // theme-sel markers from having wrong positions after the buffer replace
  editor.clearNamespace(state.bufferId, "theme-sel");

  editor.setVirtualBufferContent(state.bufferId, entries);

  // Selection highlights use a separate namespace via addOverlay (dynamic, position-dependent)
  applySelectionHighlighting(entries);
  isUpdatingDisplay = false;
}

// =============================================================================
// Field Editing
// =============================================================================

/**
 * Get field at cursor position (uses state.selectedIndex)
 */
function getFieldAtCursor(): ThemeField | null {
  if (state.selectedIndex >= 0 && state.selectedIndex < state.visibleFields.length) {
    return state.visibleFields[state.selectedIndex];
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
  const currentValue = formatColorValue(field.value as ColorValue);
  const suggestions: PromptSuggestion[] = [
    // Namespaced: the current value is often one of the named colours
    // listed below it, and the two rows are different rows.
    { id: "current", text: currentValue, description: editor.t("suggestion.current"), value: currentValue },
  ];

  // Add special colors (Default/Reset for terminal transparency)
  for (const name of SPECIAL_COLORS) {
    suggestions.push({ id: `special:${name}`, text: name, description: editor.t("suggestion.terminal_native"), value: name });
  }

  // Add named colors (terminal native - no hex shown since actual color depends on terminal)
  for (const name of NAMED_COLOR_LIST) {
    suggestions.push({ id: `named:${name}`, text: name, description: editor.t("suggestion.terminal_native"), value: name });
  }

  return suggestions;
}

/**
 * Start color editing prompt
 */
function editColorField(field: ThemeField): void {
  if (field.def.kind === "modifier") {
    editModifierField(field);
    return;
  }
  if (field.def.kind === "toggle") {
    toggleField(field);
    return;
  }
  if (field.def.kind === "text") {
    editTextField(field);
    return;
  }
  const currentValue = formatColorValue(field.value as ColorValue);
  editor.startPromptWithInitial(
    editor.t("prompt.color_input", { field: field.def.displayName }),
    `theme-color-${field.path}`,
    currentValue
  );
  editor.setPromptSuggestions(buildColorSuggestions(field));
}

/** The attributes of a styled color, as the field the modifier prompt
 * edits (`<path>_modifier`). */
function attributesField(field: ThemeField): ThemeField {
  return {
    def: { ...field.def, key: `${field.def.key}${MODIFIER_SUFFIX}`, kind: "modifier" },
    value: (getNestedValue(state.themeData, `${field.path}${MODIFIER_SUFFIX}`) ?? []) as ModifierValue,
    path: `${field.path}${MODIFIER_SUFFIX}`,
    depth: field.depth,
    isSection: false,
  };
}

/** Separator glyphs offered for a `chrome` text option. */
const SEPARATOR_GLYPHS: Array<{ value: string; description: string }> = [
  { value: "│", description: "Thin vertical line" },
  { value: "┃", description: "Heavy vertical line" },
  { value: "▏", description: "Left one-eighth block" },
  { value: "▕", description: "Right one-eighth block" },
  { value: "·", description: "Middle dot" },
  { value: "•", description: "Bullet" },
  { value: "◆", description: "Diamond" },
  { value: "╱", description: "Diagonal" },
  { value: "⟩", description: "Angle bracket" },
  { value: "\u{e0b1}", description: "Powerline thin arrow (needs a Nerd Font)" },
  { value: " ", description: "One space" },
  { value: "", description: "Nothing" },
];

/** Edit a `chrome` separator: free text, with glyphs to pick from. */
function editTextField(field: ThemeField): void {
  editor.startPromptWithInitial(
    `${field.def.displayName} (any text):`,
    `theme-text-${field.path}`,
    String(field.value ?? ""),
  );
  // An icon's glyph: the built-in icons' glyphs, named by what uses them.
  // A chrome glyph: separators and caps.
  const suggestions = field.path.startsWith("file_icons.")
    ? builtinGlyphSuggestions()
    : SEPARATOR_GLYPHS.map(g => ({
      id: `glyph:${g.value}`,
      text: g.value === "" ? "(none)" : g.value === " " ? "(space)" : g.value,
      description: g.description,
      value: g.value,
    }));
  editor.setPromptSuggestions(suggestions);
}

/** Every distinct built-in icon glyph, described by the files it marks. */
function builtinGlyphSuggestions(): PromptSuggestion[] {
  builtinValue("file_icons.default");
  const users = new Map<string, string[]>();
  const add = (glyph: unknown, who: string) => {
    if (typeof glyph !== "string" || !glyph) return;
    users.set(glyph, [...(users.get(glyph) ?? []), who]);
  };
  const icons = builtinIcons ?? {};
  for (const table of ["by_extension", "by_name"]) {
    const t = (icons[table] ?? {}) as Record<string, { glyph?: string }>;
    for (const [k, v] of Object.entries(t)) add(v.glyph, table === "by_extension" ? `.${k}` : k);
  }
  for (const one of ["default", "folder", "folder_open"]) {
    add((icons[one] as { glyph?: string } | undefined)?.glyph, one);
  }
  return [...users.entries()].map(([glyph, who]) => ({
    id: `icon:${glyph}`,
    text: glyph,
    description: who.slice(0, 6).join(" ") + (who.length > 6 ? " …" : ""),
    value: glyph,
  }));
}

/** The map definition a map path names, from the schema. */
function mapDefAt(mapPath: string): ThemeFieldDef | undefined {
  const [sectionName, ...rest] = pathParts(mapPath);
  let defs = getThemeSections().find(sec => sec.name === sectionName)?.fields ?? [];
  let def: ThemeFieldDef | undefined;
  for (const part of rest) {
    def = defs.find(d => d.key === part);
    defs = def?.children ?? [];
  }
  return def?.kind === "map" ? def : undefined;
}

/** A map key the way the theme loader reads it (the schema's
 * `x-key-normalize`): file names in lower case, extensions in lower case
 * without their dot. Writing it that way keeps two spellings of one key
 * from being two entries that collapse into one on load. */
function normalizeMapKey(mapPath: string, key: string): string {
  switch (mapDefAt(mapPath)?.keyNormalize) {
    case "lowercase":
      return key.toLowerCase();
    case "extension":
      return key.replace(/^\.+/, "").toLowerCase();
    default:
      return key;
  }
}

/** Add an entry to a keyed map (`file_icons.by_extension`): ask for its
 * key, offering the built-in table's keys the theme does not override yet. */
function addMapEntry(field: ThemeField): void {
  const mapPath = field.path.slice(0, -2); // drop ".+"
  editor.startPrompt(`New entry in ${pathParts(mapPath).slice(-1)[0]}:`, `theme-add-${mapPath}`);
  const existing = new Set(mapKeys(mapPath).map(k => normalizeMapKey(mapPath, k)));
  const table = (builtinValue(mapPath) ?? {}) as Record<string, { glyph?: string }>;
  editor.setPromptSuggestions(
    Object.entries(table)
      .filter(([k]) => !existing.has(normalizeMapKey(mapPath, k)))
      .map(([k, v]) => ({ id: `key:${k}`, text: k, description: `${v.glyph ?? ""} built-in`, value: k })),
  );
}

/** Remove the selected value, so Fresh's default draws it again — the
 * option's default, or the built-in icon. A map entry or a group goes
 * whole. (The editor works on the fully resolved theme and saves it whole,
 * so a removed value does not fall back to another theme's.) */
function theme_editor_reset(): void {
  if (state.focusPanel !== "tree") return;
  const field = getFieldAtCursor();
  if (!field || !isResettable(field)) {
    editor.setStatus("Only chrome and file icon values can be reset");
    return;
  }
  // Only the value itself: resettable values have no `_modifier` companion,
  // and in a map that name is another, user-named entry.
  deleteNestedValue(state.themeData, field.path);
  state.hasChanges = !deepEqual(state.themeData, state.originalThemeData);
  updateDisplay();
  editor.setStatus(`Reset ${field.path.split(".").map(decodeKey).join(".")}`);
}
registerHandler("theme_editor_reset", theme_editor_reset);

/** Flip a `chrome` on/off option. */
function toggleField(field: ThemeField): void {
  setNestedValue(state.themeData, field.path, !field.value);
  state.hasChanges = !deepEqual(state.themeData, state.originalThemeData);
  updateDisplay();
  editor.setStatus(editor.t("status.updated", { path: field.path }));
}

const VALID_MODIFIERS = ["bold", "italic", "underlined", "dim", "reversed"] as const;

function editModifierField(field: ThemeField): void {
  const currentValue = (field.value as ModifierValue).join(", ");
  editor.startPromptWithInitial(
    `Text attributes for ${field.def.displayName} (comma-separated):`,
    `theme-modifier-${field.path}`,
    currentValue
  );
  editor.setPromptSuggestions(VALID_MODIFIERS.map(value => ({
    id: value,
    text: value,
    description: "Toggleable text attribute",
    value,
  })));
}

function parseModifierInput(input: string): { value?: ModifierValue; error?: string } {
  const normalized = input.trim().toLowerCase();
  if (!normalized || normalized === "normal" || normalized === "none") return { value: [] };
  const values = normalized.split(/[\s,]+/).filter(Boolean).map(value => {
    if (value === "underline") return "underlined";
    if (value === "reverse") return "reversed";
    return value;
  });
  const invalid = values.find(value => !(VALID_MODIFIERS as readonly string[]).includes(value));
  if (invalid) return { error: invalid };
  return { value: [...new Set(values)] };
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


/**
 * Handle open theme prompt (both builtin and user themes)
 */


/**
 * Handle save as prompt
 */


/**
 * Handle prompt cancellation
 */


/**
 * Handle initial theme selection prompt (when opening editor)
 */


// Register prompt handlers
editor.on("prompt_confirmed", (args) => {
  if (!args.prompt_type.startsWith("theme-add-")) return true;
  const mapPath = args.prompt_type.replace("theme-add-", "");
  const key = normalizeMapKey(mapPath, args.input.trim());
  if (!key) return true;
  const entryPath = `${mapPath}.${encodeKey(key)}`;
  if (getNestedValue(state.themeData, entryPath) === undefined) {
    setNestedValue(state.themeData, entryPath, {});
  }
  // Open the new entry, so its glyph and color are the next rows.
  state.expandedSections.add(mapPath);
  state.expandedSections.add(entryPath);
  state.hasChanges = !deepEqual(state.themeData, state.originalThemeData);
  updateDisplay();
  moveCursorToField(entryPath);
  return true;
});

editor.on("prompt_confirmed", (args) => {
  if (!args.prompt_type.startsWith("theme-text-")) return true;
  const path = args.prompt_type.replace("theme-text-", "");
  // Taken as typed: a separator's spaces are part of it.
  setNestedValue(state.themeData, path, args.input);
  state.hasChanges = !deepEqual(state.themeData, state.originalThemeData);
  updateDisplay();
  moveCursorToField(path);
  editor.setStatus(editor.t("status.updated", { path }));
  return true;
});

editor.on("prompt_confirmed", (args) => {
  if (!args.prompt_type.startsWith("theme-modifier-")) return true;

  const path = args.prompt_type.replace("theme-modifier-", "");
  const result = parseModifierInput(args.input);
  if (result.value !== undefined) {
    setNestedValue(state.themeData, path, result.value);
    state.hasChanges = !deepEqual(state.themeData, state.originalThemeData);
    updateDisplay();
    // The attributes ride on their color's row.
    moveCursorToField(path.replace(/_modifier$/, ""));
    editor.setStatus(editor.t("status.updated", { path }));
  } else {
    // Ask again, on the color the attributes belong to (they have no row
    // of their own).
    const color = getFieldByPath(path.replace(/_modifier$/, ""));
    editor.setStatus(`Unknown text attribute '${result.error}'`);
    if (color) editModifierField(attributesField(color));
  }
  return true;
});

editor.on("prompt_confirmed", async (args) => {
  editor.debug(`[theme_editor] onThemeSelectInitialPromptConfirmed called with: ${JSON.stringify(args)}`);
  if (args.prompt_type !== "theme-select-initial") {
    editor.debug(`[theme_editor] prompt_type mismatch, expected 'theme-select-initial', got '${args.prompt_type}'`);
    return true;
  }
  editor.debug(`[theme_editor] prompt_type matched, processing selection...`);

  const key = args.input.trim();
  const isBuiltin = state.builtinKeys.has(key);
  const entry = state.themeRegistry.get(key);
  const themeName = entry?.name || key;

  editor.debug(editor.t("status.loading"));

  const themeData = loadThemeFile(key);
  if (themeData) {
    state.themeData = deepClone(themeData);
    state.originalThemeData = deepClone(themeData);
    state.themeName = themeName;
    state.themeKey = key;
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
});
editor.on("prompt_confirmed", (args) => {
  if (!args.prompt_type.startsWith("theme-color-")) return true;

  const path = args.prompt_type.replace("theme-color-", "");
  const field = getFieldByPath(path);
  if (!field) return true;

  const result = parseColorInput(args.input);

  if (result.value !== undefined) {
    // Valid color - apply it
    setNestedValue(state.themeData, path, result.value);
    state.hasChanges = !deepEqual(state.themeData, state.originalThemeData);

    updateDisplay();
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
});
editor.on("prompt_confirmed", (args) => {
  if (args.prompt_type !== "theme-open") return true;

  const key = args.input.trim();
  const isBuiltin = state.builtinKeys.has(key);
  const entry = state.themeRegistry.get(key);
  const themeName = entry?.name || key;

  const themeData = loadThemeFile(key);
  if (themeData) {
    state.themeData = deepClone(themeData);
    state.originalThemeData = deepClone(themeData);
    state.themeName = themeName;
    state.themeKey = key;
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
});
editor.on("prompt_confirmed", async (args) => {
  if (args.prompt_type !== "theme-save-as") return true;

  const name = args.input.trim();
  if (name) {
    // If user accepted a suggestion without typing, pre-fill the prompt so they can edit the name
    if (args.selected_index !== null && !state.saveAsPreFilled) {
      state.saveAsPreFilled = true;
      editor.startPromptWithInitial(editor.t("prompt.save_as"), "theme-save-as", name);
      editor.setPromptSuggestions([{
        id: "current",
        text: state.themeName,
        description: state.isBuiltin
          ? editor.t("suggestion.current_builtin")
          : editor.t("suggestion.current"),
        value: state.themeName,
      }]);
      return true;
    }
    state.saveAsPreFilled = false;

    // Reject names that match a built-in theme
    if (state.builtinKeys.has(name)) {
      editor.startPromptWithInitial(editor.t("prompt.save_as_builtin_error"), "theme-save-as", name);
      editor.setPromptSuggestions([{
        id: "current",
        text: state.themeName,
        description: state.isBuiltin
          ? editor.t("suggestion.current_builtin")
          : editor.t("suggestion.current"),
        value: state.themeName,
      }]);
      return true;
    }

    // Check if a user theme file already exists with this name
    if (editor.themeFileExists(name)) {
      // Store pending save name for overwrite confirmation
      state.pendingSaveName = name;
      editor.startPrompt(editor.t("prompt.overwrite_confirm", { name }), "theme-overwrite-confirm");
      const suggestions: PromptSuggestion[] = [
        { id: "overwrite", text: editor.t("prompt.overwrite_yes"), description: "", value: "overwrite" },
        { id: "cancel", text: editor.t("prompt.overwrite_no"), description: "", value: "cancel" },
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
});
editor.on("prompt_confirmed", (args) => {
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
});
editor.on("prompt_confirmed", async (args) => {
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
});
editor.on("prompt_confirmed", async (args) => {
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
        state.themeName = "";
        state.themeKey = "";
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
});
editor.on("prompt_cancelled", (args) => {
  if (!args.prompt_type.startsWith("theme-")) return true;

  // Clear saved state on cancellation
  state.savedCursorPath = null;
  state.pendingSaveName = null;
  state.closeAfterSave = false;
  state.filterActive = false;

  editor.debug(editor.t("status.cancelled"));
  return true;
});

// =============================================================================
// Theme Operations
// =============================================================================

/**
 * Save theme to file
 * @param name - Theme name to save as
 * @param restorePath - Optional field path to restore cursor to after save
 */
/**
 * The theme being edited, in theme-file form: what Save writes and what the
 * live preview loads. `state.themeData` comes from the registry fully
 * resolved, so it already names every key; sections the schema does not
 * know are left out.
 */
function buildThemeFile(themeName: string): Record<string, unknown> {
  const completeTheme: Record<string, unknown> = { name: themeName };
  const colors = colorSections();
  for (const section of getThemeSections()) {
    const stored = state.themeData[section.name];
    if (!stored || typeof stored !== "object" || Array.isArray(stored)) continue;
    // Every value the section holds — colors and their attributes, chrome
    // options and glyph sets, icon maps — as edited. Only the color
    // sections re-bundle color + attributes into one on-disk value.
    const data = deepClone(stored as Record<string, unknown>);
    completeTheme[section.name] = colors.includes(section.name)
      ? collapseStyleBundles(data)
      : data;
  }
  return completeTheme;
}

/**
 * Live preview: render the whole editor — menu, explorer, tabs, gutter,
 * status bar, the `chrome` options — with the theme as edited, through the
 * editor's own renderer (`editor.previewTheme`). Nothing is saved; closing
 * without saving restores `state.runningThemeKey`.
 */
function previewEdits(): void {
  if (!state.themeName || Object.keys(state.themeData).length === 0) return;
  const json = JSON.stringify(buildThemeFile(state.themeName));
  if (json === state.previewedJson) return;
  state.previewedJson = json;
  editor.previewTheme(json);
}

/** End the live preview: put back the theme the editor was running —
 * through `previewTheme` as well, so restoring it does not rewrite the
 * user's config the way `applyTheme` (which persists its choice) would. */
function endPreview(): void {
  if (state.previewedJson !== null && state.runningThemeKey) {
    // The config may spell the theme `builtin://dark` or `my-theme.json`;
    // the registry knows it by key or by name.
    const key = state.runningThemeKey;
    const bare = key.replace(/^builtin:\/\//, "").replace(/^.*\//, "").replace(/\.json$/, "");
    const running = editor.getThemeData(key) ?? editor.getThemeData(bare);
    if (running) {
      editor.previewTheme(JSON.stringify(running));
    } else {
      editor.applyTheme(state.runningThemeKey);
    }
  }
  state.previewedJson = null;
  state.runningThemeKey = null;
}

async function saveTheme(name?: string, restorePath?: string | null): Promise<boolean> {
  // Normalize theme name: lowercase, replace underscores/spaces with hyphens
  // (must match Rust's normalize_theme_name so config name matches filename)
  const themeName = (name || state.themeName).toLowerCase().replace(/[_ ]/g, "-");

  if (!themeName) {
    editor.setStatus(editor.t("status.save_failed", { error: "No theme name" }));
    return false;
  }

  try {
    const content = JSON.stringify(buildThemeFile(themeName), null, 2);
    const savedPath = editor.saveThemeFile(themeName, content);

    state.themePath = savedPath;
    state.themeName = themeName;
    // Use the portable theme name as the key. The registry resolves names
    // via its name-fallback, and config persistence strips the absolute
    // `file://` prefix for user themes (see apply_theme in Rust).
    state.themeKey = themeName;
    state.isBuiltin = false; // After saving, it's now a user theme
    state.originalThemeData = deepClone(state.themeData);
    state.hasChanges = false;

    // Update display
    updateDisplay();

    // Restore cursor position if provided
    if (restorePath) {
      moveCursorToField(restorePath);
    }

    // Reload themes and apply the new/updated theme atomically. The saved
    // theme is now the running one, so closing keeps it.
    editor.reloadAndApplyTheme(themeName);
    state.runningThemeKey = themeName;
    state.previewedJson = null;
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



editor.on("cursor_moved", (data) => {
  if (state.bufferId === null) return;
  // Accept cursor_moved events for any of the buffer group's panels
  // (tree, picker, footer). With buffer groups each panel is its own
  // buffer, so clicks in the picker fire cursor_moved for the picker
  // buffer — not the tree buffer (state.bufferId). We must handle
  // events for all of them so picker clicks (named colors, palette,
  // hex) still update selection/colors.
  const groupBufferIds = Object.values(state.panelBuffers || {});
  const isGroupBuffer =
    data.buffer_id === state.bufferId || groupBufferIds.includes(data.buffer_id);
  if (!isGroupBuffer) return;
  if (isUpdatingDisplay) return;

  const props = data.text_properties || [];
  if (props.length === 0) return;

  const entryType = props[0].type as string | undefined;

  // Tree field/section click — update selection and refresh display
  if (TREE_ROW_TYPES.includes(entryType ?? "") && typeof props[0].index === "number") {
    const index = props[0].index as number;
    if (index >= 0 && index < state.visibleFields.length) {
      state.selectedIndex = index;
      state.focusPanel = "tree";
      // A click on any header (section, group, map, entry) toggles it.
      if (entryType === "tree-section" || entryType === "tree-group" || entryType === "tree-entry") {
        theme_editor_toggle_section();
        return;
      }
      updateDisplay();
      return;
    }
  }

  // Picker named color click — find which column via inline overlay properties
  if (entryType === "picker-named-row" && typeof props[0].namedRow === "number") {
    const namedRow = props[0].namedRow as number;
    // Look for namedCol property from inline overlay
    const colProp = props.find(p => typeof p.namedCol === "number");
    const clickedCol = colProp ? (colProp.namedCol as number) : 0;
    state.focusPanel = "picker";
    state.pickerFocus = { type: "named-colors", index: namedRow * NAMED_COLORS_PER_ROW + clickedCol };
    applyPickerColor();
    return;
  }

  // Picker palette click — find which column via inline overlay properties
  if (entryType === "picker-palette-row" && typeof props[0].paletteRow === "number") {
    const paletteRow = props[0].paletteRow as number;
    // Look for paletteCol property from inline overlay
    const colProp = props.find(p => typeof p.paletteCol === "number");
    const clickedCol = colProp ? (colProp.paletteCol as number) : 0;
    state.focusPanel = "picker";
    state.pickerFocus = { type: "palette", row: paletteRow, col: clickedCol };
    applyPickerColor();
    return;
  }

  // Picker hex click — open hex editing prompt
  if (entryType === "picker-hex") {
    state.focusPanel = "picker";
    state.pickerFocus = { type: "hex-input" };
    applyPickerColor();
    return;
  }

  applySelectionHighlighting();
});


editor.on("resize", (data) => {
  if (state.bufferId === null) return;
  state.viewportHeight = data.height;
  state.viewportWidth = data.width;
  updateDisplay();
});

/**
 * Handle buffer_closed event to reset state when buffer is closed by any means
 */


editor.on("buffer_closed", (data) => {
  if (state.bufferId !== null && data.buffer_id === state.bufferId) {
    // Reset state when our buffer is closed
    state.bufferId = null;
    state.splitId = null;
    state.themeData = {};
    state.originalThemeData = {};
    state.hasChanges = false;
    state.focusPanel = "tree";
    state.pickerFocus = { type: "hex-input" };
    state.filterText = "";
    state.filterActive = false;
    state.selectedIndex = 0;
    state.treeScrollOffset = 0;
  }
});

/**
 * Handle theme_inspect_key hook: open the theme editor at a specific key
 */


editor.on("theme_inspect_key", async (data) => {
  // If already open, focus and navigate to the key
  if (isThemeEditorOpen()) {
    if (state.bufferId !== null) {
      editor.showBuffer(state.bufferId);
    }
    const section = data.key.split(".")[0];
    if (!state.expandedSections.has(section)) {
      state.expandedSections.add(section);
    }
    moveCursorToField(data.key);
    return;
  }

  // Save context
  state.sourceSplitId = editor.getActiveSplitId();
  state.sourceBufferId = editor.getActiveBufferId();
  await loadThemeRegistry();

  // Auto-load the current theme (data.theme_name is the config key)
  const themeKey = data.theme_name;
  const isBuiltin = state.builtinKeys.has(themeKey);
  const entry = state.themeRegistry.get(themeKey);
  const themeName = entry?.name || themeKey;
  const themeData = loadThemeFile(themeKey);
  if (themeData) {
    state.themeData = deepClone(themeData);
    state.originalThemeData = deepClone(themeData);
    state.themeName = themeName;
    state.themeKey = themeKey;
    state.themePath = null;
    state.isBuiltin = isBuiltin;
    state.hasChanges = false;
  } else {
    editor.setStatus(`Failed to load theme '${themeName}'`);
    return;
  }

  // Expand the target section
  const section = data.key.split(".")[0];
  state.expandedSections.add(section);

  // Open editor and navigate
  await doOpenThemeEditor();
  moveCursorToField(data.key);
});

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

    // Only tree rows are selectable (they have index property)
    if (TREE_ROW_TYPES.includes(entryType) && typeof props.index === "number") {
      selectableEntries.push({
        byteOffset,
        valueByteOffset: byteOffset,
        index: props.index as number,
        isSection: entryType === "tree-section",
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

  // Find the field by path in visibleFields and update selectedIndex
  for (let i = 0; i < state.visibleFields.length; i++) {
    if (state.visibleFields[i].path === path) {
      state.selectedIndex = i;
      updateDisplay();
      return;
    }
  }
}

/**
 * Navigate up - context-dependent on focus panel
 */
function theme_editor_nav_down() : void {
  if (state.bufferId === null) return;

  if (state.focusPanel === "tree") {
    if (state.selectedIndex < state.visibleFields.length - 1) {
      state.selectedIndex++;
      updateDisplay();
    }
  } else {
    navigatePickerVertical(1);
  }
}
registerHandler("theme_editor_nav_down", theme_editor_nav_down);

function theme_editor_nav_up() : void {
  if (state.bufferId === null) return;

  if (state.focusPanel === "tree") {
    if (state.selectedIndex > 0) {
      state.selectedIndex--;
      updateDisplay();
    }
  } else {
    navigatePickerVertical(-1);
  }
}
registerHandler("theme_editor_nav_up", theme_editor_nav_up);

/**
 * Page Down - jump a page of items in the tree
 */
function theme_editor_page_down() : void {
  if (state.bufferId === null) return;

  if (state.focusPanel === "tree") {
    const pageSize = Math.max(1, Math.max(8, state.viewportHeight - 2) - 1);
    state.selectedIndex = Math.min(state.visibleFields.length - 1, state.selectedIndex + pageSize);
    updateDisplay();
  }
}
registerHandler("theme_editor_page_down", theme_editor_page_down);

/**
 * Page Up - jump a page of items in the tree
 */
function theme_editor_page_up() : void {
  if (state.bufferId === null) return;

  if (state.focusPanel === "tree") {
    const pageSize = Math.max(1, Math.max(8, state.viewportHeight - 2) - 1);
    state.selectedIndex = Math.max(0, state.selectedIndex - pageSize);
    updateDisplay();
  }
}
registerHandler("theme_editor_page_up", theme_editor_page_up);

/**
 * Navigate left/right - for picker grid navigation
 */
function theme_editor_nav_left() : void {
  if (state.focusPanel === "picker") {
    navigatePickerHorizontal(-1);
  }
}
registerHandler("theme_editor_nav_left", theme_editor_nav_left);

function theme_editor_nav_right() : void {
  if (state.focusPanel === "picker") {
    navigatePickerHorizontal(1);
  }
}
registerHandler("theme_editor_nav_right", theme_editor_nav_right);

/**
 * Tab - switch focus between panels
 */
function theme_editor_focus_tab() : void {
  if (state.focusPanel === "tree") {
    state.focusPanel = "picker";
    state.pickerFocus = { type: "hex-input" };
  } else {
    state.focusPanel = "tree";
  }
  updateDisplay();
}
registerHandler("theme_editor_focus_tab", theme_editor_focus_tab);

/**
 * Shift-Tab - reverse switch focus
 */
function theme_editor_focus_shift_tab() : void {
  if (state.focusPanel === "picker") {
    state.focusPanel = "tree";
  } else {
    state.focusPanel = "picker";
    state.pickerFocus = { type: "hex-input" };
  }
  updateDisplay();
}
registerHandler("theme_editor_focus_shift_tab", theme_editor_focus_shift_tab);

/**
 * Enter key - context-dependent action
 */
function theme_editor_enter() : void {
  if (state.focusPanel === "tree") {
    const field = getFieldAtCursor();
    if (!field) return;
    if (isContainer(field)) {
      theme_editor_toggle_section();
    } else if (field.def.kind === "add") {
      addMapEntry(field);
    } else {
      editColorField(field);
    }
  } else {
    applyPickerColor();
  }
}
registerHandler("theme_editor_enter", theme_editor_enter);

/**
 * Escape - context-dependent
 */
function theme_editor_escape() : void {
  if (state.focusPanel === "picker") {
    state.focusPanel = "tree";
    updateDisplay();
  } else if (state.filterText) {
    state.filterText = "";
    state.filterActive = false;
    updateDisplay();
  } else {
    theme_editor_close();
  }
}
registerHandler("theme_editor_escape", theme_editor_escape);

/**
 * / key - activate filter
 */
function theme_editor_filter() : void {
  state.filterActive = true;
  editor.startPromptWithInitial(
    "Filter fields:",
    "theme-filter",
    state.filterText
  );
}
registerHandler("theme_editor_filter", theme_editor_filter);

/**
 * Handle filter prompt confirmation
 */


editor.on("prompt_confirmed", (args) => {
  if (args.prompt_type !== "theme-filter") return true;

  state.filterText = args.input.trim();
  state.filterActive = false;
  state.selectedIndex = 0;
  state.treeScrollOffset = 0;
  updateDisplay();
  return true;
});

/**
 * Navigate picker vertically (between sections: hex, named-colors, palette)
 */
function navigatePickerVertical(dir: number): void {
  const pf = state.pickerFocus;
  if (dir > 0) {
    if (pf.type === "attributes") {
      state.pickerFocus = { type: "hex-input" };
    } else if (pf.type === "hex-input") {
      state.pickerFocus = { type: "named-colors", index: 0 };
    } else if (pf.type === "named-colors") {
      const row = Math.floor(pf.index / NAMED_COLORS_PER_ROW);
      const col = pf.index % NAMED_COLORS_PER_ROW;
      if (row < NAMED_COLOR_GRID.length - 1) {
        state.pickerFocus = { type: "named-colors", index: (row + 1) * NAMED_COLORS_PER_ROW + col };
      } else {
        state.pickerFocus = { type: "palette", row: 0, col: Math.min(col, PALETTE_COLS - 1) };
      }
    } else if (pf.type === "palette") {
      if (pf.row < PALETTE_ROWS - 1) {
        state.pickerFocus = { type: "palette", row: pf.row + 1, col: pf.col };
      }
    }
  } else {
    if (pf.type === "hex-input") {
      const field = getFieldAtCursor();
      if (field && field.def.styled) state.pickerFocus = { type: "attributes" };
    } else if (pf.type === "palette") {
      if (pf.row > 0) {
        state.pickerFocus = { type: "palette", row: pf.row - 1, col: pf.col };
      } else {
        const col = Math.min(pf.col, NAMED_COLORS_PER_ROW - 1);
        state.pickerFocus = { type: "named-colors", index: (NAMED_COLOR_GRID.length - 1) * NAMED_COLORS_PER_ROW + col };
      }
    } else if (pf.type === "named-colors") {
      const row = Math.floor(pf.index / NAMED_COLORS_PER_ROW);
      const col = pf.index % NAMED_COLORS_PER_ROW;
      if (row > 0) {
        state.pickerFocus = { type: "named-colors", index: (row - 1) * NAMED_COLORS_PER_ROW + col };
      } else {
        state.pickerFocus = { type: "hex-input" };
      }
    }
  }
  updateDisplay();
}

/**
 * Navigate picker horizontally (within named-colors or palette grids)
 */
function navigatePickerHorizontal(dir: number): void {
  const pf = state.pickerFocus;
  if (pf.type === "named-colors") {
    const row = Math.floor(pf.index / NAMED_COLORS_PER_ROW);
    const col = pf.index % NAMED_COLORS_PER_ROW;
    const newCol = col + dir;
    if (newCol >= 0 && newCol < NAMED_COLORS_PER_ROW) {
      const totalItems = NAMED_COLOR_GRID.length * NAMED_COLORS_PER_ROW;
      const newIdx = row * NAMED_COLORS_PER_ROW + newCol;
      if (newIdx < totalItems) {
        state.pickerFocus = { type: "named-colors", index: newIdx };
      }
    }
  } else if (pf.type === "palette") {
    const newCol = pf.col + dir;
    if (newCol >= 0 && newCol < PALETTE_COLS) {
      state.pickerFocus = { type: "palette", row: pf.row, col: newCol };
    }
  }
  updateDisplay();
}

/**
 * Apply the currently focused picker color to the selected field
 */
function applyPickerColor(): void {
  const field = getFieldAtCursor();
  if (!field || field.isSection) return;
  if (field.def.kind === "modifier") {
    editModifierField(field);
    return;
  }
  if (field.def.kind === "toggle") {
    toggleField(field);
    return;
  }
  if (field.def.kind === "text") {
    editTextField(field);
    return;
  }

  const pf = state.pickerFocus;
  let newColor: ColorValue | null = null;

  if (pf.type === "attributes") {
    editModifierField(attributesField(field));
    return;
  }
  if (pf.type === "hex-input") {
    editColorField(field);
    return;
  } else if (pf.type === "named-colors") {
    const row = Math.floor(pf.index / NAMED_COLORS_PER_ROW);
    const col = pf.index % NAMED_COLORS_PER_ROW;
    if (row < NAMED_COLOR_GRID.length && col < NAMED_COLOR_GRID[row].length) {
      const item = NAMED_COLOR_GRID[row][col];
      newColor = item.value;
    }
  } else if (pf.type === "palette") {
    const palette = getExtendedPalette();
    if (pf.row < PALETTE_ROWS && pf.col < PALETTE_COLS) {
      newColor = palette[pf.row][pf.col];
    }
  }

  if (newColor !== null) {
    setNestedValue(state.themeData, field.path, newColor);
    state.hasChanges = !deepEqual(state.themeData, state.originalThemeData);
    updateDisplay();
    editor.setStatus(editor.t("status.updated", { path: field.path }));
  }
}


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

  editor.debug("[theme_editor] loading themes...");
  // Load available themes
  await loadThemeRegistry();
  editor.debug(`[theme_editor] loaded ${state.themeRegistry.size} themes (${state.builtinKeys.size} builtin)`);

  // Get current theme key from config
  const config = editor.getConfig() as Record<string, unknown>;
  const currentThemeKey = (config?.theme as string) || "dark";
  // What the live preview replaces until the theme is saved.
  state.runningThemeKey = currentThemeKey;
  state.previewedJson = null;

  // Prompt user to select which theme to edit
  editor.startPrompt(editor.t("prompt.select_theme_to_edit"), "theme-select-initial");

  const suggestions: PromptSuggestion[] = [];

  // Build suggestions from theme registry (user themes first, then builtins)
  const userSuggestions: PromptSuggestion[] = [];
  const builtinSuggestions: PromptSuggestion[] = [];
  for (const [key, {name}] of state.themeRegistry) {
    const isCurrent = key === currentThemeKey || name === currentThemeKey;
    const isBuiltin = state.builtinKeys.has(key);
    const desc = isBuiltin
      ? (isCurrent ? editor.t("suggestion.builtin_theme_current") : editor.t("suggestion.builtin_theme"))
      : (isCurrent ? editor.t("suggestion.user_theme_current") : editor.t("suggestion.user_theme"));
    const suggestion = { id: key, text: name, description: desc, value: key };
    if (isBuiltin) {
      builtinSuggestions.push(suggestion);
    } else {
      userSuggestions.push(suggestion);
    }
  }
  suggestions.push(...userSuggestions, ...builtinSuggestions);

  // Sort suggestions to put current theme first
  suggestions.sort((a, b) => {
    const aIsCurrent = (a.description ?? "").includes("current");
    const bIsCurrent = (b.description ?? "").includes("current");
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
  // Initialize viewport dimensions
  const vp = editor.getViewport();
  if (vp) {
    state.viewportHeight = vp.height;
    state.viewportWidth = vp.width;
  }
  state.treeScrollOffset = 0;

  // Open below the user's code rather than over it: the live preview
  // (`previewEdits`) re-themes the real editor, so the code pane above is
  // the preview of the gutter, syntax, tabs and explorer.
  state.editorPaneId = null;
  try {
    // Stacked, not side by side: the editor's two panels need the width,
    // and a dozen rows of code are enough to show what an edit does.
    const pane = await editor.splitWindow({ direction: "horizontal", place: "after", ratio: 0.4 });
    state.editorPaneId = pane.splitId;
  } catch (e) {
    editor.debug(`[theme_editor] could not open beside the code: ${e}`);
  }

  // Create buffer group with layout: horizontal split (tree | picker) + footer
  const layout = JSON.stringify({
    type: "split",
    direction: "v",
    ratio: 0.95,
    first: {
      type: "split",
      direction: "h",
      ratio: TREE_RATIO,
      first: { type: "scrollable", id: "tree" },
      second: { type: "scrollable", id: "picker" },
    },
    second: { type: "fixed", id: "footer", height: 1 },
  });

  const groupResult = await editor.createBufferGroup("*Theme Editor*", "theme-editor", layout);
  state.groupId = groupResult.groupId;
  state.panelBuffers = groupResult.panels;
  state.bufferId = groupResult.panels["tree"]; // representative buffer
  state.splitId = null;

  // Mount the footer as a widget panel (HintBar). Tree + picker
  // panels still use `setPanelContent` (see updateDisplay).
  const footerBufferId = groupResult.panels["footer"];
  if (typeof footerBufferId === "number") {
    state.footerPanel = new WidgetPanel(footerBufferId);
  }

  if (state.bufferId !== null) {
    // Set initial content for all panels
    updateDisplay();
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
      { id: "keep", text: editor.t("prompt.discard_no"), description: "", value: "keep" },
      { id: "save", text: editor.t("prompt.discard_save"), description: "", value: "save" },
      { id: "discard", text: editor.t("prompt.discard_yes"), description: "", value: "discard" },
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
  endPreview();
  // Close the buffer group (or fall back to single buffer close).
  // Closing the buffer group tears down the footer buffer too,
  // implicitly dropping the widget panel rendering into it.
  state.footerPanel = null;
  if (state.groupId !== null) {
    editor.closeBufferGroup(state.groupId);
    state.groupId = null;
    state.panelBuffers = {};
  } else if (state.bufferId !== null) {
    editor.closeBuffer(state.bufferId);
  }

  if (state.editorPaneId !== null) {
    editor.closeSplit(state.editorPaneId);
    state.editorPaneId = null;
  }

  // Reset state
  state.bufferId = null;
  state.splitId = null;
  state.themeData = {};
  state.originalThemeData = {};
  state.hasChanges = false;
  state.focusPanel = "tree";
  state.pickerFocus = { type: "hex-input" };
  state.filterText = "";
  state.filterActive = false;
  state.selectedIndex = 0;

  editor.debug(editor.t("status.closed"));
}

/**
 * Handle discard confirmation prompt
 */


/**
 * Edit color at cursor
 */
function theme_editor_edit_color() : void {
  const field = getFieldAtCursor();
  if (!field) {
    editor.debug(editor.t("status.no_field"));
    return;
  }

  if (isContainer(field)) {
    theme_editor_toggle_section();
    return;
  }
  if (field.def.kind === "add") {
    addMapEntry(field);
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
  if (!field || !isContainer(field)) {
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

  // Build suggestions from theme registry (user themes first, then builtins)
  const userSuggestions: PromptSuggestion[] = [];
  const builtinSuggestions: PromptSuggestion[] = [];
  for (const [key, {name}] of state.themeRegistry) {
    const isBuiltin = state.builtinKeys.has(key);
    const desc = isBuiltin ? editor.t("suggestion.builtin_theme") : editor.t("suggestion.user_theme");
    const suggestion = { id: key, text: name, description: desc, value: key };
    if (isBuiltin) {
      builtinSuggestions.push(suggestion);
    } else {
      userSuggestions.push(suggestion);
    }
  }
  suggestions.push(...userSuggestions, ...builtinSuggestions);

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
      { id: "overwrite", text: editor.t("prompt.overwrite_yes"), description: "", value: "overwrite" },
      { id: "cancel", text: editor.t("prompt.overwrite_no"), description: "", value: "cancel" },
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


/**
 * Save theme as (new name)
 */
function theme_editor_save_as() : void {
  // Save cursor path for restoration after save (if not already saved by theme_editor_save)
  if (!state.savedCursorPath) {
    state.savedCursorPath = getCurrentFieldPath();
  }

  state.saveAsPreFilled = false;
  editor.startPrompt(editor.t("prompt.save_as"), "theme-save-as");

  editor.setPromptSuggestions([{
    id: "current",
    text: state.themeName,
    description: state.isBuiltin
      ? editor.t("suggestion.current_builtin")
      : editor.t("suggestion.current"),
    value: state.themeName,
  }]);
}
registerHandler("theme_editor_save_as", theme_editor_save_as);

/**
 * Reload theme
 */
async function theme_editor_reload() : Promise<void> {
  if (state.themePath) {
    const themeData = loadThemeFile(state.themeKey);
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
    { id: "delete", text: editor.t("prompt.delete_yes"), description: "", value: "delete" },
    { id: "cancel", text: editor.t("prompt.delete_no"), description: "", value: "cancel" },
  ];
  editor.setPromptSuggestions(suggestions);
}
registerHandler("theme_editor_delete", theme_editor_delete);

/**
 * Handle delete confirmation prompt
 */


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
editor.registerCommand("%cmd.nav_left", "%cmd.nav_left_desc", "theme_editor_nav_left", "theme-editor");
editor.registerCommand("%cmd.nav_right", "%cmd.nav_right_desc", "theme_editor_nav_right", "theme-editor");
editor.registerCommand("%cmd.focus_tab", "%cmd.focus_tab_desc", "theme_editor_focus_tab", "theme-editor");
editor.registerCommand("%cmd.focus_shift_tab", "%cmd.focus_shift_tab_desc", "theme_editor_focus_shift_tab", "theme-editor");
editor.registerCommand("%cmd.enter", "%cmd.enter_desc", "theme_editor_enter", "theme-editor");
editor.registerCommand("%cmd.escape", "%cmd.escape_desc", "theme_editor_escape", "theme-editor");
editor.registerCommand("%cmd.filter", "%cmd.filter_desc", "theme_editor_filter", "theme-editor");

// =============================================================================
// Plugin Initialization
// =============================================================================

editor.debug("Theme Editor plugin initialized - Use 'Edit Theme' command to open");
