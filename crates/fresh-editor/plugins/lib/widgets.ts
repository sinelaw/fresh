/**
 * Plugin widget library — declarative UI for Fresh plugins.
 *
 * Plugins describe panel content as a `WidgetSpec` tree. The host owns
 * rendering, theming, and (in later phases) hit-testing, focus, and
 * keymaps. This module provides:
 *
 *   - Type re-exports from the generated `fresh.d.ts` so plugins import
 *     `WidgetSpec` / `HintEntry` from one place.
 *   - Builder helpers (`row`, `col`, `hintBar`, `raw`) that produce the
 *     correct discriminated-union shape.
 *   - A `WidgetPanel` class that wraps the
 *     `mountWidgetPanel` / `updateWidgetPanel` / `unmountWidgetPanel`
 *     IPC trio with mount-once-then-update semantics.
 *   - `parseHintString(s)` — parses the legacy `Tab:section  Esc:close`
 *     string format used by today's plugin i18n bundles into
 *     `HintEntry[]`.
 *
 * See `docs/internal/plugin-widget-library-design.md`.
 *
 * @example
 *   import { WidgetPanel, hintBar, col, raw, parseHintString } from "./lib/widgets.ts";
 *
 *   const panel = new WidgetPanel(bufferId);
 *   panel.set(col(
 *     raw(myExistingEntries),
 *     hintBar(parseHintString(editor.t("panel.help"))),
 *   ));
 *   // …later, on every state change:
 *   panel.set(col(raw(newEntries), hintBar(myHints)));
 *   // …on close:
 *   panel.unmount();
 */

/// <reference path="./fresh.d.ts" />

// `fresh.d.ts` declares HintEntry / WidgetSpec / TextPropertyEntry as
// ambient globals (it is not an ES module). Re-export the relevant
// type names locally so plugin code can write
// `import type { WidgetSpec } from "./lib/widgets.ts"` without dipping
// into the ambient namespace directly.
export type WidgetSpec = globalThis.WidgetSpec;
export type HintEntry = globalThis.HintEntry;
export type ButtonKind = globalThis.ButtonKind;
export type WidgetAction = globalThis.WidgetAction;
export type WidgetMutation = globalThis.WidgetMutation;
export type TreeNode = globalThis.TreeNode;
export type StyledSegment = globalThis.StyledSegment;
export type TextPropertyEntry = globalThis.TextPropertyEntry;
type InlineOverlay = globalThis.InlineOverlay;
type OverlayOptions = globalThis.OverlayOptions;

// =============================================================================
// Builder helpers — preferred over hand-writing `{ kind: "row", ... }`.
// =============================================================================

/** Horizontal layout. Children laid out left-to-right; inline-sized
 * children collapse into a single line. See §3 of the design doc. */
export function row(...children: WidgetSpec[]): WidgetSpec {
  return { kind: "row", children };
}

/** Vertical layout. Children stacked top-to-bottom. */
export function col(...children: WidgetSpec[]): WidgetSpec {
  return { kind: "col", children };
}

/** Keyboard-hint footer. Renders `<keys> <label>` per entry, with the
 * keys portion styled by the `ui.help_key_fg` theme key.
 *
 * Replaces the per-plugin hand-rolled help row. */
export function hintBar(entries: HintEntry[]): WidgetSpec {
  return { kind: "hintBar", entries };
}

/** Imperative-virtual-buffer escape hatch. Wraps an existing
 * `TextPropertyEntry[]` (the same shape `setVirtualBufferContent`
 * already accepts) so a plugin can migrate its panel one widget at a
 * time. */
export function raw(entries: TextPropertyEntry[], key?: string): WidgetSpec {
  return { kind: "raw", entries, key };
}

/** Build a `TextPropertyEntry` from a sequence of styled segments.
 *
 * The plugin describes row content structurally — each segment is a
 * piece of text plus optional `style` and optional nested
 * `overlays`. The host concatenates the segments and emits one
 * inline overlay per styled segment plus the segment's nested
 * overlays shifted by the segment's start position; both happen in
 * Rust against the final text, so the plugin never names byte or
 * codepoint offsets between segments.
 *
 * Use `padToChars` / `truncateToChars` to constrain the entry's
 * total width — both are applied AFTER segment concatenation (so
 * `padToChars: 80` pads the full row to 80 codepoints, regardless
 * of how the segments split it).
 *
 * For freeform overlays inside a single segment (e.g. highlighting
 * pattern matches inside a context string), pass them via the
 * segment's `overlays` field with `unit: "char"`. */
export function styledRow(
  segments: StyledSegment[],
  options?: {
    padToChars?: number;
    truncateToChars?: number;
    properties?: Record<string, unknown>;
    style?: Partial<OverlayOptions>;
    inlineOverlays?: InlineOverlay[];
  },
): TextPropertyEntry {
  // Build the entry by spreading only set fields. The plugin
  // bridge converts JS `undefined` to JSON `null` when an object
  // key is present, which then fails to deserialize as the
  // matching `Option<…>` / `Vec<…>` field on the host. Omitting
  // the key entirely lets serde fall back to `#[serde(default)]`.
  const entry: TextPropertyEntry = { text: "", segments };
  if (options?.padToChars !== undefined) entry.padToChars = options.padToChars;
  if (options?.truncateToChars !== undefined) entry.truncateToChars = options.truncateToChars;
  if (options?.properties !== undefined) entry.properties = options.properties;
  if (options?.style !== undefined) entry.style = options.style;
  if (options?.inlineOverlays !== undefined) entry.inlineOverlays = options.inlineOverlays;
  return entry;
}

/** Boolean toggle, rendered as `[v] label` / `[ ] label`.
 * Pass `focused: true` to highlight (the host will own focus once
 * the keymap layer is wired). */
export function toggle(
  checked: boolean,
  label: string,
  options?: { focused?: boolean; key?: string },
): WidgetSpec {
  return {
    kind: "toggle",
    checked,
    label,
    focused: options?.focused ?? false,
    key: options?.key,
  };
}

/** Action button, rendered as `[ Label ]`. `intent` controls visual
 * emphasis: `"normal"` (default) → no override, `"primary"` → bold,
 * `"danger"` → error theme key.
 *
 * `disabled: true` paints the button with `ui.menu_disabled_fg`,
 * drops it from the Tab cycle, and makes clicks no-ops — for actions
 * that aren't currently available against the surrounding state.
 * The button still occupies its layout cell, so flipping `disabled`
 * doesn't reshuffle the surrounding row. */
export function button(
  label: string,
  options?: {
    focused?: boolean;
    intent?: ButtonKind;
    key?: string;
    disabled?: boolean;
  },
): WidgetSpec {
  return {
    kind: "button",
    label,
    focused: options?.focused ?? false,
    intent: options?.intent ?? "normal",
    key: options?.key,
    disabled: options?.disabled ?? false,
  };
}

/** Horizontal spacer of fixed column count. In a `Row` it produces
 * `cols` spaces; at the top level or in a `Col` it produces a
 * short blank line. */
export function spacer(cols: number, key?: string): WidgetSpec {
  return { kind: "spacer", cols, flex: false, key };
}

/** Flex horizontal spacer — fills remaining row width
 * (`panel_width - sum(non-flex children)`). Use to right-align a
 * trailing widget: `row(label, flexSpacer(), button)`. With
 * multiple flex spacers in one row the leftover splits evenly. */
export function flexSpacer(key?: string): WidgetSpec {
  return { kind: "spacer", cols: 0, flex: true, key };
}

/** Vertical list of pre-rendered rows with host-managed selection
 * styling, click routing, and **virtual scrolling**. Plugin passes
 * the full dataset of items + a `visibleRows` count; the widget
 * owns scroll offset as instance state (keyed by `key`) and
 * auto-clamps it to keep `selectedIndex` in view. Plugins never
 * compute scroll math.
 *
 * Click on a row fires `widget_event` with `eventType: "select"` and
 * `payload: { index, key }` where `index` is the *absolute* index
 * into `items` (not the visible-window index).
 *
 * `key` is required for any List that should preserve scroll across
 * re-renders. Lists without a key reset to scroll=0 each render. */
export function list(options: {
  items: TextPropertyEntry[];
  itemKeys?: string[];
  selectedIndex?: number;
  visibleRows: number;
  /** Whether Tab / Shift+Tab lands focus on this list. Default
   * true (matches other tabbable widgets). Set to false in
   * picker-style layouts where the filter input stays focused
   * and Up/Down forward to the list via host smart-keys —
   * skipping the list in the Tab cycle keeps focus jumping
   * straight between filter and action buttons. */
  focusable?: boolean;
  key?: string;
}): WidgetSpec {
  return {
    kind: "list",
    items: options.items,
    itemKeys: options.itemKeys ?? [],
    selectedIndex: options.selectedIndex ?? -1,
    visibleRows: options.visibleRows,
    focusable: options.focusable ?? true,
    key: options.key,
  };
}

/** Construct one node in a `Tree` widget's flat-list spec. The
 * plugin emits a depth-first traversal of its hierarchy, one
 * `treeNode(...)` per node, plus a parallel `itemKeys` array for
 * stable per-row identifiers. `depth` controls indent (`depth * 2`
 * spaces); `hasChildren: true` renders a disclosure glyph (`▶`/`▼`)
 * with a click-to-expand hit area in the indent column. The host
 * filters out descendants of collapsed nodes when rendering. */
export function treeNode(
  text: TextPropertyEntry,
  options?: { depth?: number; hasChildren?: boolean; checked?: boolean },
): TreeNode {
  // `checked` is intentionally Optional<bool>, not a default-false
  // boolean: omitting it (== undefined here) maps to host-side
  // `None`, which means "no checkbox glyph". Per-node opt-in keeps
  // checkable trees mixing checkbox-bearing rows with rows that
  // shouldn't render one (e.g. a header that doesn't itself have
  // a meaningful checked state).
  const node: TreeNode = {
    text,
    depth: options?.depth ?? 0,
    hasChildren: options?.hasChildren ?? false,
  };
  if (options?.checked !== undefined) {
    node.checked = options.checked;
  }
  return node;
}

/** Hierarchical tree with host-managed expand/collapse, selection,
 * scrolling, and click routing.
 *
 * The plugin emits its hierarchy as a flat list of `TreeNode`s
 * (depth-first); the host filters out descendants of collapsed
 * nodes at render time. **Toggling expansion is host-owned** —
 * `Right`/`Left` arrow keys and disclosure clicks update host
 * instance state without the plugin re-emitting. Plugins that
 * need to react to expansion changes listen for
 * `widget_event` `eventType: "expand"`.
 *
 * Click on the disclosure column → `expand` event. Click on the
 * row body → `select` event. Enter/Space on the focused tree →
 * `activate` event with the currently-selected node. Up/Down move
 * selection through the visible (un-collapsed) flat list.
 *
 * `key` is required for any Tree that should preserve scroll +
 * selection + expansion across re-renders. */
export function tree(options: {
  nodes: TreeNode[];
  itemKeys?: string[];
  selectedIndex?: number;
  visibleRows: number;
  /** Initial expanded keys; subsequent expansion changes are
   * host-owned and don't read this field. Use
   * `panel.setExpandedKeys(...)` to override host state after
   * mount. */
  expandedKeys?: string[];
  /** When true, every node with `checked: true | false` renders
   * a `[v]` / `[ ]` glyph and emits a `toggle` hit area. Click on
   * the glyph fires `widget_event` `eventType: "toggle"` with
   * `payload: { key, index, checked: <new> }`; the plugin updates
   * its model and pushes the new state back via
   * `panel.setCheckedKeys(...)`. */
  checkable?: boolean;
  key?: string;
}): WidgetSpec {
  return {
    kind: "tree",
    nodes: options.nodes,
    itemKeys: options.itemKeys ?? [],
    selectedIndex: options.selectedIndex ?? -1,
    visibleRows: options.visibleRows,
    expandedKeys: options.expandedKeys ?? [],
    checkable: options.checkable ?? false,
    key: options.key,
  };
}

/** Text input — single-line (`rows: 1`, default) or multi-line
 * (`rows >= 2`). The host owns `value` and `cursorByte` as instance
 * state once the widget renders for the first time; multi-line
 * widgets also own a vertical scroll offset.
 *
 * Single-line (`rows: 1`) renders as `[value]` (or `Label: [value]`
 * if `label` is provided), with `fieldWidth` giving a constant
 * visible width — short values pad with trailing spaces, long
 * values head-truncate with `…` so the tail (where the cursor
 * usually is) stays visible. Smart-key dispatch: `Enter` advances
 * focus; `Up`/`Down` are no-ops; `Home`/`End` jump to the start /
 * end of the whole value.
 *
 * Multi-line (`rows >= 2`) renders as a `rows`-tall block, padded
 * with blanks when `value` is shorter. Smart-key dispatch differs:
 * `Enter` inserts a newline; `Up`/`Down` move between lines;
 * `Home`/`End` are line-relative; long lines tail-truncate with `…`
 * per-line. Plugins that want `Enter` to submit can intercept the
 * key in their own mode binding and call
 * `panel.command(focusAdvance(1))` instead.
 *
 * `key` is required for any text widget that should preserve its
 * value, cursor, and scroll across re-renders.
 *
 * Prefer the `textInput()` / `textArea()` helpers below when the
 * intent is unambiguous — they call this with the right `rows`. */
export function text(
  options: {
    value?: string;
    cursorByte?: number;
    focused?: boolean;
    label?: string;
    placeholder?: string;
    /** Number of visible rows of editing region. `1` (default) =
     * single-line behaviour; `>= 2` = multi-line behaviour. */
    rows?: number;
    /** Visible column width. `0` (default) = auto-fit (single-line)
     * or panel width (multi-line). */
    fieldWidth?: number;
    /** Single-line soft cap on visible chars after the
     * `fieldWidth` pad. `0` = no cap. Ignored when `rows >= 2`. */
    maxVisibleChars?: number;
    /** Stretch the visible field to fill the enclosing
     * container's width. Overrides `fieldWidth` when set:
     * the renderer sizes the bracketed region to
     * `panelWidth - label_overhead - bracket_overhead`. Pair
     * with `labeledSection(...)` to get a uniformly full-width
     * fieldset look. */
    fullWidth?: boolean;
    /** Initial completion candidates. Use the
     * `setCompletions(widgetKey, items)` mutation for live
     * updates — the spec field seeds the host's instance state
     * on first render only. Empty = no popup. See
     * `WidgetSpec::Text.completions` (Rust) for the rendering
     * + keyboard semantics. */
    completions?: string[];
    key?: string;
  } = {},
): WidgetSpec {
  return {
    kind: "text",
    value: options.value ?? "",
    cursorByte: options.cursorByte ?? -1,
    focused: options.focused ?? false,
    label: options.label ?? "",
    placeholder: options.placeholder,
    rows: options.rows ?? 1,
    fieldWidth: options.fieldWidth ?? 0,
    maxVisibleChars: options.maxVisibleChars ?? 0,
    fullWidth: options.fullWidth ?? false,
    completions: options.completions ?? [],
    key: options.key,
  };
}

/** Multi-line text widget. Thin wrapper over `text({ rows, ... })`
 * for ergonomic intent — renders as a `rows`-tall block with
 * Enter-inserts-newline / Up-Down-line-nav semantics. Default `rows`
 * is 5; pass `rows: N` to override. */
export function textArea(
  options: {
    value?: string;
    cursorByte?: number;
    focused?: boolean;
    label?: string;
    placeholder?: string;
    /** Visible rows of editing area; default 5. */
    rows?: number;
    /** Visible column width; `0` = use panel width. */
    fieldWidth?: number;
    fullWidth?: boolean;
    key?: string;
  } = {},
): WidgetSpec {
  return text({
    ...options,
    rows: options.rows ?? 5,
  });
}

/** Single-line text widget. Thin wrapper over `text({ rows: 1,
 * ... })` matching the historical `textInput(value, opts)`
 * signature. Renders as `[value]` (or `Label: [value]` if `label`
 * is provided), with Enter-advances-focus semantics. */
export function textInput(
  value: string,
  options?: {
    cursorByte?: number;
    focused?: boolean;
    label?: string;
    placeholder?: string;
    /** Soft truncation cap (legacy). Prefer `fieldWidth`. */
    maxVisibleChars?: number;
    /** Constant visible width inside the brackets. */
    fieldWidth?: number;
    /** See `text({ fullWidth })`. */
    fullWidth?: boolean;
    key?: string;
  },
): WidgetSpec {
  return text({
    value,
    cursorByte: options?.cursorByte,
    focused: options?.focused,
    label: options?.label,
    placeholder: options?.placeholder,
    rows: 1,
    fieldWidth: options?.fieldWidth,
    maxVisibleChars: options?.maxVisibleChars,
    fullWidth: options?.fullWidth,
    key: options?.key,
  });
}

/** Reserve a rectangle in the layout for the host to natively
 * render the editor window identified by `windowId`. The widget
 * itself emits `rows` blank lines so layout reserves the space;
 * the host's paint path then overlays the live window UI (split
 * tree, terminals, syntax highlighting, decorations) into the
 * reserved rectangle.
 *
 * `windowId` of 0 (or any unknown id) renders the placeholder
 * blanks without dispatching the per-window paint. */
export function windowEmbed(options: {
  windowId: number;
  rows: number;
  key?: string;
}): WidgetSpec {
  return {
    kind: "windowEmbed",
    windowId: options.windowId,
    rows: options.rows,
    key: options.key,
  };
}

/** Group a single child widget inside a rounded, thin border
 * with `label` printed as a top-left legend (HTML
 * `<fieldset>` semantics). The host renders three rows:
 *
 *     ╭─ Label ──────────────────╮
 *     │ <child rendered content> │
 *     ╰──────────────────────────╯
 *
 * The section always spans the parent's available width. The
 * child is rendered with the inner width (parent width minus
 * 4 columns of border + padding), so child widgets that honour
 * `fullWidth: true` size themselves to fill the inner area.
 * Focus, hit areas and cursor positions bubble up from the
 * child unchanged, shifted by the border offset. */
export function labeledSection(options: {
  label?: string;
  child: WidgetSpec;
  /** When this section is a Block child of a Row, request a
   * specific share of the row's width as a percentage (1..=100).
   * Out-of-range values fall back to the equal-split default.
   * Useful for picker-style layouts: a narrow list pane next to
   * a wide preview pane. */
  widthPct?: number;
  key?: string;
}): WidgetSpec {
  return {
    kind: "labeledSection",
    label: options.label ?? "",
    child: options.child,
    widthPct: options.widthPct,
    key: options.key,
  };
}

/** Float `child` over the rest of the layout instead of consuming
 * vertical space. Place inside a `Col` at the position where you
 * want the overlay to anchor — at paint time the child renders
 * at that row but DOES NOT push the rows below it down. Used for
 * dropdown completions, tooltips, hover popups — anything that
 * should appear next to a focused widget without reflowing the
 * panel each time it shows or hides.
 *
 * Hit-testing: overlays paint on top, so clicks inside the
 * overlay's region go to the overlay (not what's underneath).
 * Tab cycle: the child IS walked for tabbable keys — give it a
 * `key` if you want focus to reach it, or leave it keyless to
 * keep it out of the cycle (the typical popup case). */
export function overlay(
  child: WidgetSpec,
  options?: { key?: string },
): WidgetSpec {
  return {
    kind: "overlay",
    child,
    key: options?.key,
  };
}

// =============================================================================
// HintEntry parsing — for the legacy `Tab:section  Esc:close` format
// shipped in existing plugin i18n bundles.
// =============================================================================

/** Parse a hint string of the form `<keys>:<label>  <keys>:<label> ...`.
 *
 * The separator between entries defaults to two-or-more spaces (matching
 * what existing i18n bundles use). The separator between keys and label
 * within an entry is a colon.
 *
 * Empty input yields an empty array. Entries without a colon are
 * preserved with empty label. */
export function parseHintString(
  s: string,
  options?: { entrySep?: RegExp; keyLabelSep?: string },
): HintEntry[] {
  if (!s) return [];
  const entrySep = options?.entrySep ?? /\s{2,}/;
  const keyLabelSep = options?.keyLabelSep ?? ":";
  const parts = s.split(entrySep).filter((p) => p.length > 0);
  return parts.map((part) => {
    const idx = part.indexOf(keyLabelSep);
    if (idx < 0) {
      return { keys: part, label: "" };
    }
    return {
      keys: part.slice(0, idx).trim(),
      label: part.slice(idx + keyLabelSep.length).trim(),
    };
  });
}

// =============================================================================
// WidgetPanel — mount-once-update-many wrapper around the IPC trio.
// =============================================================================

/** A handle to a mounted widget panel. Construct one per virtual
 * buffer that should host widget-rendered content; call `set(spec)`
 * on every render; call `unmount()` when the buffer is closed.
 *
 * The first `set()` issues `mountWidgetPanel`; subsequent calls
 * issue `updateWidgetPanel`. Idempotent re-mount is guaranteed by the
 * host (see `WidgetRegistry::mount`). */
export class WidgetPanel {
  private mounted = false;
  private readonly panelId: number;
  private readonly bufferId: number;

  constructor(bufferId: number, panelId?: number) {
    this.bufferId = bufferId;
    this.panelId = panelId ?? allocatePanelId();
  }

  /** Returns the plugin-allocated panel id, useful for routing
   * widget events back through `editor.on("widget_event", ...)`. */
  id(): number {
    return this.panelId;
  }

  /** Render or re-render the panel against the given spec.
   * Cheap to call on every state change; the host reconciles. */
  set(spec: WidgetSpec): boolean {
    // deno-lint-ignore no-explicit-any
    const editor = (globalThis as any).editor;
    if (!this.mounted) {
      this.mounted = true;
      return editor.mountWidgetPanel(this.panelId, this.bufferId, spec);
    }
    return editor.updateWidgetPanel(this.panelId, spec);
  }

  /** Tear down the panel. The plugin retains ownership of the
   * underlying virtual buffer. Subsequent `set()` calls re-mount. */
  unmount(): boolean {
    if (!this.mounted) return true;
    this.mounted = false;
    // deno-lint-ignore no-explicit-any
    const editor = (globalThis as any).editor;
    return editor.unmountWidgetPanel(this.panelId);
  }

  /** Route a key/nav action to the focused widget in this panel.
   * The host computes the result on the focused widget's kind and
   * fires `widget_event` as appropriate. See `WidgetAction` for
   * the action shapes. */
  command(action: WidgetAction): boolean {
    // deno-lint-ignore no-explicit-any
    const editor = (globalThis as any).editor;
    return editor.widgetCommand(this.panelId, action);
  }

  /** Apply a targeted mutation in place — the IPC fast path.
   * Use instead of `set(spec)` when only one widget changes;
   * the host applies the mutation directly and re-renders
   * without re-transmitting the full spec. See `WidgetMutation`
   * for the shapes. The typed wrappers below cover the common
   * cases. */
  mutate(mutation: WidgetMutation): boolean {
    // deno-lint-ignore no-explicit-any
    const editor = (globalThis as any).editor;
    return editor.widgetMutate(this.panelId, mutation);
  }

  /** Set a `TextInput`'s value (and optionally cursor byte).
   * Mutates host instance state; doesn't re-transmit the full
   * spec. */
  setValue(widgetKey: string, value: string, cursorByte?: number): boolean {
    return this.mutate({ kind: "setValue", widgetKey, value, cursorByte });
  }

  /** Set a `Toggle`'s checked state. */
  setChecked(widgetKey: string, checked: boolean): boolean {
    return this.mutate({ kind: "setChecked", widgetKey, checked });
  }

  /** Set a `List`'s selected index. */
  setSelectedIndex(widgetKey: string, index: number): boolean {
    return this.mutate({ kind: "setSelectedIndex", widgetKey, index });
  }

  /** Update a Text widget's completion popup candidates. Empty
   * `items` closes the popup; non-empty opens it and resets the
   * host-managed selection to index 0. The host repaints the
   * popup on its own; the plugin doesn't need to follow up with
   * an `update(spec)` call. */
  setCompletions(
    widgetKey: string,
    items: Array<string | { value: string; kind?: string }>,
  ): boolean {
    return this.mutate({ kind: "setCompletions", widgetKey, items });
  }

  /** Replace a `List`'s items + parallel `itemKeys`. */
  setItems(
    widgetKey: string,
    items: TextPropertyEntry[],
    itemKeys: string[] = [],
  ): boolean {
    return this.mutate({ kind: "setItems", widgetKey, items, itemKeys });
  }

  /** Replace a `Tree`'s expanded-keys instance state. The host
   * normally owns expansion (Right/Left arrows + disclosure
   * clicks); use this when a non-user action drives expansion
   * (e.g. "expand all", reveal-on-search). */
  setExpandedKeys(widgetKey: string, keys: string[]): boolean {
    return this.mutate({ kind: "setExpandedKeys", widgetKey, keys });
  }

  /** Stamp `checked` onto every node in the named `Tree` whose
   * `itemKey` appears in `keys`. Used by the `toggle` event
   * handler to push the post-click state back without a full spec
   * re-emit. Nodes whose existing `checked` is `undefined` (no
   * checkbox glyph) are unchanged. */
  setCheckedKeys(widgetKey: string, checked: boolean, keys: string[]): boolean {
    return this.mutate({ kind: "setCheckedKeys", widgetKey, checked, keys });
  }

  /** Append `newNodes` (and parallel `newItemKeys`) to an existing
   * `Tree`'s node list — the streaming-friendly counterpart to
   * `setItems`. Existing selection, scroll, and expansion state are
   * preserved; the renderer simply has more tail to paint on the next
   * cycle. Cheap relative to a full spec re-emit for plugins that
   * stream large result sets (e.g. a project-wide grep). */
  appendTreeNodes(
    widgetKey: string,
    newNodes: TreeNode[],
    newItemKeys: string[] = [],
  ): boolean {
    return this.mutate({
      kind: "appendTreeNodes",
      widgetKey,
      newNodes,
      newItemKeys,
    });
  }

  /** Replace the entries of a `Raw` widget identified by `widgetKey`.
   *
   * Use this when a small piece of panel chrome (a label, a header,
   * a status line) needs to change but the rest of the spec is
   * unchanged — calling `set(...)` to push the whole spec just to
   * flip a few characters would force a `js_to_json` walk of every
   * other widget (and every `Tree` node) in the panel, which can
   * block the JS thread for hundreds of ms on large panels. */
  setRawEntries(widgetKey: string, entries: TextPropertyEntry[]): boolean {
    return this.mutate({ kind: "setRawEntries", widgetKey, entries });
  }

  /** Set the panel's focused widget by key. Passing a key that isn't
   * a current tabbable is harmless — the next render clamps focus to
   * the first tabbable. */
  setFocusKey(widgetKey: string): boolean {
    return this.mutate({ kind: "setFocusKey", widgetKey });
  }
}

// =============================================================================
// FloatingWidgetPanel — mount-once-update-many wrapper for centered
// floating overlays (no virtual buffer required).
// =============================================================================

/** A handle to a floating widget panel — a modal-ish overlay
 * rendered in a centered frame on top of the editor, dimming the
 * background. Unlike `WidgetPanel`, no virtual buffer is needed;
 * the host owns the rect and paints the spec inside it.
 *
 * `mount({ widthPct, heightPct })` mounts the panel and renders
 * the spec; `update(spec)` re-renders against the previous instance
 * state; `unmount()` tears it down. The host routes keys to the
 * focused widget automatically while a floating panel is up: Esc
 * unmounts and fires a `widget_event` "cancel"; Tab / Enter /
 * arrows / Backspace / printable chars route through the same
 * smart-key dispatch as `WidgetPanel.command(key(...))`. */
export class FloatingWidgetPanel {
  private mounted = false;
  private readonly panelId: number;

  constructor(panelId?: number) {
    this.panelId = panelId ?? allocatePanelId();
  }

  /** Returns the plugin-allocated panel id, useful for routing
   * widget events back through `editor.on("widget_event", ...)`. */
  id(): number {
    return this.panelId;
  }

  /** Mount the panel as a centered overlay sized by `widthPct` /
   * `heightPct` (percent of terminal, clamped 1..=100). Cheap to
   * call repeatedly with a new spec — re-mounting replaces the
   * existing panel. */
  mount(
    spec: WidgetSpec,
    options: { widthPct?: number; heightPct?: number } = {},
  ): boolean {
    // deno-lint-ignore no-explicit-any
    const editor = (globalThis as any).editor;
    const wp = options.widthPct ?? 60;
    const hp = options.heightPct ?? 40;
    this.mounted = true;
    return editor.mountFloatingWidget(this.panelId, spec, wp, hp);
  }

  /** Re-render the panel against the given spec; instance state on
   * keyed widgets is preserved. No-op when not mounted. */
  update(spec: WidgetSpec): boolean {
    if (!this.mounted) return false;
    // deno-lint-ignore no-explicit-any
    const editor = (globalThis as any).editor;
    return editor.updateFloatingWidget(this.panelId, spec);
  }

  /** Tear down the panel and let the editor return to its normal
   * key/click routing. */
  unmount(): boolean {
    if (!this.mounted) return true;
    this.mounted = false;
    // deno-lint-ignore no-explicit-any
    const editor = (globalThis as any).editor;
    return editor.unmountFloatingWidget(this.panelId);
  }

  /** Route a key/nav action to the focused widget. The host
   * automatically routes keystrokes while a floating panel is up,
   * so plugins rarely need to call this directly — it's exposed
   * for symmetry with `WidgetPanel`. */
  command(action: WidgetAction): boolean {
    // deno-lint-ignore no-explicit-any
    const editor = (globalThis as any).editor;
    return editor.widgetCommand(this.panelId, action);
  }

  /** Apply a targeted mutation in place — the IPC fast path. */
  mutate(mutation: WidgetMutation): boolean {
    // deno-lint-ignore no-explicit-any
    const editor = (globalThis as any).editor;
    return editor.widgetMutate(this.panelId, mutation);
  }

  setValue(widgetKey: string, value: string, cursorByte?: number): boolean {
    return this.mutate({ kind: "setValue", widgetKey, value, cursorByte });
  }

  setChecked(widgetKey: string, checked: boolean): boolean {
    return this.mutate({ kind: "setChecked", widgetKey, checked });
  }

  setSelectedIndex(widgetKey: string, index: number): boolean {
    return this.mutate({ kind: "setSelectedIndex", widgetKey, index });
  }

  /** Update a Text widget's completion popup candidates. Empty
   * `items` closes the popup; non-empty opens it and resets the
   * host-managed selection to index 0. */
  setCompletions(
    widgetKey: string,
    items: Array<string | { value: string; kind?: string }>,
  ): boolean {
    return this.mutate({ kind: "setCompletions", widgetKey, items });
  }

  setItems(
    widgetKey: string,
    items: TextPropertyEntry[],
    itemKeys: string[] = [],
  ): boolean {
    return this.mutate({ kind: "setItems", widgetKey, items, itemKeys });
  }

  setExpandedKeys(widgetKey: string, keys: string[]): boolean {
    return this.mutate({ kind: "setExpandedKeys", widgetKey, keys });
  }

  setCheckedKeys(widgetKey: string, checked: boolean, keys: string[]): boolean {
    return this.mutate({ kind: "setCheckedKeys", widgetKey, checked, keys });
  }

  /** Set the panel's focused widget by key. Passing a key that isn't
   * a current tabbable is harmless — the next render clamps focus to
   * the first tabbable. */
  setFocusKey(widgetKey: string): boolean {
    return this.mutate({ kind: "setFocusKey", widgetKey });
  }
}

// =============================================================================
// WidgetAction builders — convenience wrappers around `panel.command(...)`.
// Plugin's mode bindings call these for keys handled by the widget layer.
// =============================================================================

/** Cycle focus through the panel's tabbable widgets. `delta=+1`
 * for Tab, `-1` for Shift+Tab. Wraps at the ends. */
export function focusAdvance(delta: number): WidgetAction {
  return { kind: "focusAdvance", delta };
}

/** Activate the focused widget (Enter on Button → "activate"
 * event; Enter on Toggle → "toggle" event). No-op for other
 * widget kinds. */
export function activate(): WidgetAction {
  return { kind: "activate" };
}

/** Move the focused List's selection by `delta`. Plugin listens
 * for `widget_event` "select" to mirror back into its model. */
export function selectMove(delta: number): WidgetAction {
  return { kind: "selectMove", delta };
}

/** Apply a non-printable editing key to the focused TextInput:
 * `"Backspace"`, `"Delete"`, `"Left"`, `"Right"`, `"Home"`,
 * `"End"`. Fires `widget_event` "change" with the new value +
 * cursorByte. */
export function textInputKey(key: string): WidgetAction {
  return { kind: "textInputKey", key };
}

/** Append printable text at the focused TextInput's cursor.
 * Fires `widget_event` "change" with the new value + cursorByte.
 * Used for the `mode_text_input` fall-through path. */
export function textInputChar(text: string): WidgetAction {
  return { kind: "textInputChar", text };
}

/** Smart-key dispatch — routes the keystroke to the right widget
 * action based on the focused widget's kind. Plugin's mode bindings
 * use this rather than picking the right action themselves: bind
 * Tab/Shift+Tab/Enter/Space/Backspace/Delete/Left/Right/Up/Down/
 * Home/End all to one handler that calls `panel.command(key("Tab"))`.
 *
 * See `WidgetAction::Key` (Rust) for the full dispatch table. */
export function key(name: string): WidgetAction {
  return { kind: "key", key: name };
}

// =============================================================================
// Panel-id allocation. Plugin-side counter; need only be unique per
// plugin instance (the host doesn't interpret the value).
// =============================================================================

let nextPanelId = 1;
function allocatePanelId(): number {
  // Bias high so plugin-allocated ids don't collide with the
  // editor's internal panel-id space if it ever uses small ints.
  const id = nextPanelId++;
  return 0x1000_0000 + id;
}
