// inty type declarations for the scene protocol: the JSON the Fresh web UI
// receives from its Rust bridge over the `/ws` WebSocket (and `GET /state`).
//
// Declarations only; never executed. Load with `inty --lib <this file>`.
//
// Conventions, from the serde attributes on the Rust side:
//   - `Option<T>` without `skip_serializing_if`  ->  `T | Null`
//   - a field with `skip_serializing_if`         ->  `name?: T`
//   - `#[serde(tag = "kind")]` enums             ->  union of records, each
//     carrying its `kind` (or `type`) literal
//   - every integer/float width                  ->  `Number`
// Objects are closed rows: each alias lists exactly the fields Rust sends.

// ───────────────────────────── wire messages ─────────────────────────────

// src/webui/mod.rs `WsSession::send_hello` / `push_diff`.
/** type ServerMessage = HelloMessage | FrameMessage */

// src/webui/mod.rs `WsSession::send_hello`: full scene, sent once per connection.
/** type HelloMessage = { type: "hello", seq: Number, scene: Scene } */

// src/webui/mod.rs `WsSession::push_diff`: `changed` maps a path to the value
// that replaces it wholesale. Paths: "w", "h", "theme", "clipboard",
// "windowId", "regions.<key>", "regions.panes.<i>", "regions.panes.len".
/** type FrameMessage = { type: "frame", seq: Number, changed: Dict<FrameValue> } */

// Every value a `changed` entry can hold (src/webui/mod.rs `WsSession::diff`):
// a top-level scene field, any `Regions` field, or one `Pane`.
// "regions.panes.len" is a Number.
/** type FrameValue =
    Null | Number | Boolean | SceneTheme | Clipboard | Rect | Number[]
    | MenuEntry[] | Dropdown | StatusBar | FileExplorer | Pane | Separator[]
    | Popup[] | TreeView | NativeView | Palette | TrustDialog | ContextMenu
    | AuxModal | KeybindingEditor | Settings | CursorPos | Poll */

// ───────────────────────────── scene root ─────────────────────────────

// src/webui/mod.rs `scene_json` + `ClipboardSync::attach` (`build_scene`).
// `clipboard` is attached for every browser scene (WS hello/frames and
// GET /state); only the parity harness's direct `scene_json` lacks it.
/** type Scene = {
    w: Number,
    h: Number,
    windowId: Number,
    regions: Regions,
    theme: SceneTheme,
    clipboard: Clipboard
} */

// src/webui/mod.rs `ClipboardSync::attach`.
/** type Clipboard = { seq: Number, text: String } */

// src/webui/mod.rs `scene_json` (`theme`). Colors via `color_css`, which
// returns None for `Color::Reset`.
/** type SceneTheme = {
    name: String,
    bg: String | Null,
    fg: String | Null,
    accent: String | Null,
    muted: String | Null,
    selectionBg: String | Null,
    menuBg: String | Null,
    menuFg: String | Null,
    menuHi: String | Null,
    menuHighlightFg: String | Null,
    popupBg: String | Null,
    popupFg: String | Null,
    border: String | Null,
    statusBg: String | Null,
    statusFg: String | Null,
    tabActiveBg: String | Null
} */

// src/webui/mod.rs `scene_json` (`regions`). The menu fields are
// `view/scene.rs MenuView` spread in; `tree` and `native` are always objects.
// NOTE: the JS `PATH_REGION` table (75-app.js) also names a `widgets` region
// that Rust never sends.
/** type Regions = {
    menubar: Rect | Null,
    menus: MenuEntry[],
    menuOpen: Number | Null,
    menuHighlight: Number | Null,
    submenuPath: Number[],
    dropdown: Dropdown | Null,
    statusbar: StatusBar | Null,
    fileExplorer: FileExplorer | Null,
    panes: Pane[],
    separators: Separator[],
    popups: Popup[],
    tree: TreeView,
    native: NativeView,
    palette: Palette | Null,
    trustDialog: TrustDialog | Null,
    contextMenu: ContextMenu | Null,
    auxModal: AuxModal | Null,
    keybindingEditor: KeybindingEditor | Null,
    settings: Settings | Null,
    cursor: CursorPos | Null,
    poll: Poll
} */

// view/scene.rs `RectView` (also mod.rs `rect_json`, native.rs rect json!).
/** type Rect = { x: Number, y: Number, w: Number, h: Number } */

// src/webui/mod.rs `scene_json` (`cursor`): the hardware caret cell.
/** type CursorPos = { x: Number, y: Number } */

// src/webui/mod.rs `scene_json` (`poll`): tick-pacing hint.
/** type Poll = { active: Boolean } */

// src/webui/mod.rs `scene_json` (`separators`).
/** type Separator = { vertical: Boolean, x: Number, y: Number, len: Number } */

// ───────────────────────────── panes & cells ─────────────────────────────

// src/webui/mod.rs `cells_json`: one styled run of cell text.
/** type CellRun = {
    t: String,
    fg: String | Null,
    bg: String | Null,
    b: Boolean,
    i: Boolean,
    u: Boolean,
    r: Boolean
} */

// src/webui/mod.rs `cells_json`: one screen row.
/** type CellRow = CellRun[] */

// src/webui/mod.rs `scene_json` (inline `json!` per pane).
/** type Pane = {
    leaf: Number,
    buffer: Number | Null,
    content: Rect,
    gutterWidth: Number,
    gutter: CellRow[] | Null,
    cells: CellRow[],
    tabBar: Rect | Null,
    tabs: Tab[],
    vscroll: Rect,
    thumbStart: Number,
    thumbEnd: Number,
    vscrollMarkers: ScrollbarMarker[]
} */

// src/webui/mod.rs `scrollbar_markers_json`.
/** type ScrollbarMarker = { row: Number, color: String } */

// view/scene.rs `TabView`.
/** type Tab = {
    bufferId: Number | Null,
    label: String,
    active: Boolean,
    modified: Boolean,
    rect: Rect,
    closeRect: Rect
} */

// view/scene.rs `TabBarView`. Not sent as a unit: its fields travel as
// `Pane.tabBar` (= `bar`) and `Pane.tabs`.
/** type TabBar = { bar: Rect | Null, tabs: Tab[] } */

// ───────────────────────────── menu ─────────────────────────────

// view/scene.rs `MenuView`. Not sent as a unit: spread into `Regions`.
/** type Menu = {
    menus: MenuEntry[],
    menuOpen: Number | Null,
    menuHighlight: Number | Null,
    submenuPath: Number[],
    dropdown: Dropdown | Null
} */

// view/scene.rs `MenuEntry`.
/** type MenuEntry = {
    label: String,
    visible: Boolean,
    x: Number | Null,
    w: Number | Null,
    items: MenuItem[]
} */

// view/scene.rs `MenuItemView` (tag = "kind", lowercase).
/** type MenuItem =
    { kind: "action", label: String, action: String, args?: Dict<JsonValue>,
      accel: String | Null, enabled: Boolean, checked: Boolean | Null }
    | { kind: "sep" }
    | { kind: "submenu", label: String, items: MenuItem[] }
    | { kind: "label", label: String } */

// view/scene.rs `DropdownView`.
/** type Dropdown = {
    rect: Rect | Null,
    items: ItemArea[],
    submenus: SubmenuArea[],
    submenuBoxes: SubmenuBoxArea[]
} */

// view/scene.rs `ItemArea`.
/** type ItemArea = { index: Number, rect: Rect } */

// view/scene.rs `SubmenuArea`.
/** type SubmenuArea = { depth: Number, index: Number, rect: Rect } */

// view/scene.rs `SubmenuBoxArea`.
/** type SubmenuBoxArea = { depth: Number, rect: Rect } */

// ───────────────────────────── status bar ─────────────────────────────

// view/scene.rs `StatusSegment`. `name`: "lsp" | "warning" | "language" |
// "encoding" | "lineEnding" | "remote" | "trust" | "message" |
// "terminalRestart" | "plugin" | "text" (per the Rust doc comment).
/** type StatusSegment = {
    name: String,
    key: String | Null,
    text: String,
    x: Number,
    w: Number,
    side: "left" | "right"
} */

// view/scene.rs `StatusView`.
/** type StatusBar = { rect: Rect, segments: StatusSegment[] } */

// ───────────────────────────── palette / prompt ─────────────────────────────

// view/scene.rs `SuggestionView`.
/** type Suggestion = {
    text: String,
    description: String | Null,
    keybinding: String | Null,
    disabled: Boolean
} */

// view/scene.rs `SearchOptionView`.
/** type SearchOption = {
    name: String,
    label: String,
    shortcut: String | Null,
    active: Boolean,
    x: Number,
    w: Number
} */

// view/scene.rs `SearchOptionsView`.
/** type SearchOptions = { row: Number, options: SearchOption[] } */

// view/scene.rs `PaletteView`, plus `previewCells` (inserted when
// `previewRect` is non-empty) and `browser` (`file_browser_view`, inserted
// when present) in src/webui/mod.rs `scene_json`.
/** type Palette = {
    query: String,
    message: String,
    promptType: "quickopen" | "livegrep" | "search" | "openfile" | "saveas" | "goto" | "input",
    overlay: Boolean,
    title: String,
    status: String,
    selected: Number | Null,
    scrollStart: Number,
    visibleCount: Number,
    total: Number,
    outerRect: Rect | Null,
    listRect: Rect | Null,
    previewRect?: Rect,
    suggestions: Suggestion[],
    toolbar?: WidgetSpec,
    toolbarFocus?: String,
    searchOptions?: SearchOptions,
    previewCells?: CellRow[],
    browser?: FileBrowser
} */

// view/scene.rs `FileBrowserRowView`.
/** type FileBrowserRow = {
    index: Number,
    row: Number,
    name: String,
    isDir: Boolean,
    isSymlink: Boolean,
    size: String,
    modified: String,
    selected: Boolean,
    matchesFilter: Boolean
} */

// view/scene.rs `FileBrowserToggleView`.
/** type FileBrowserToggle = {
    name: String,
    label: String,
    shortcut: String | Null,
    active: Boolean,
    x: Number,
    y: Number,
    w: Number
} */

// view/scene.rs `FileBrowserShortcutView`.
/** type FileBrowserShortcut = {
    index: Number,
    label: String,
    description: String,
    selected: Boolean,
    x: Number,
    y: Number,
    w: Number
} */

// view/scene.rs `FileBrowserColumnView`.
/** type FileBrowserColumn = {
    name: String,
    label: String,
    active: Boolean,
    ascending: Boolean,
    x: Number,
    y: Number,
    w: Number
} */

// view/scene.rs `FileBrowserView` (sent as `Palette.browser`).
/** type FileBrowser = {
    rect: Rect,
    listRect: Rect,
    scrollbarRect: Rect,
    path: String,
    toggles: FileBrowserToggle[],
    shortcuts: FileBrowserShortcut[],
    columns: FileBrowserColumn[],
    rows: FileBrowserRow[],
    scrollOffset: Number,
    visibleRows: Number,
    total: Number,
    selected: Number | Null,
    activeSection: "navigation" | "files",
    loading: Boolean,
    error: String | Null,
    thumbStart: Number,
    thumbEnd: Number
} */

// ───────────────────────────── popups ─────────────────────────────

// view/scene.rs `PopupItemView`.
/** type PopupItem = {
    text: String,
    detail: String | Null,
    icon: String | Null,
    disabled: Boolean
} */

// view/scene.rs `PopupSpanView`: like `CellRun`, but colors and false
// flags are omitted. Typed `name: T?` (reads as undefined when omitted)
// rather than `name?: T`: inty gives an alias's optional field one presence
// for all its values, which the popup renderer's reads would pin to present.
/** type PopupSpan = {
    t: String,
    fg: String?,
    bg: String?,
    b: Boolean?,
    i: Boolean?,
    u: Boolean?,
    r: Boolean?
} */

// view/scene.rs `PopupContentView` (tag = "type", lowercase).
/** type PopupContent =
    { type: "list", items: PopupItem[], selected: Number }
    | { type: "lines", lines: PopupSpan[][] } */

// view/scene.rs `ScenePopup`.
/** type Popup = {
    kind: "completion" | "hover" | "action" | "list" | "text",
    title: String | Null,
    description: String | Null,
    rect: Rect,
    contentRect: Rect,
    scrollOffset: Number,
    content: PopupContent
} */

// ───────────────────────────── file explorer ─────────────────────────────

// view/scene.rs `FileRow`.
/** type FileRow = { name: String, depth: Number, isDir: Boolean, expanded: Boolean } */

// view/scene.rs `FileExplorerView`.
/** type FileExplorer = {
    rect: Rect,
    gripRect: Rect | Null,
    title: String,
    scrollOffset: Number,
    viewportHeight: Number,
    selected: Number | Null,
    viewportRows: Number[],
    rows: FileRow[]
} */

// ───────────────────────────── trust dialog ─────────────────────────────

// view/scene.rs `TrustOptionView`.
/** type TrustOption = {
    label: String,
    description: String,
    selected: Boolean,
    data: String,
    rect: Rect
} */

// view/scene.rs `TrustDialogView`.
/** type TrustDialog = {
    dialog: Rect,
    title: String,
    path: String,
    triggers: String,
    cancellable: Boolean,
    options: TrustOption[],
    ok: Rect,
    okLabel: String,
    quit: Rect,
    quitLabel: String
} */

// ───────────────────────────── display-list tree ─────────────────────────────

// view/scene.rs `TreeSurfaceView`. (The Rust doc comment omits "pane", which
// `tree_view` does push.)
/** type TreeSurface = {
    kind: "dock" | "floating" | "confirm" | "sidebar" | "pane",
    x: Number,
    y: Number,
    w: Number,
    h: Number,
    anchored: Boolean,
    section?: Number
} */

// view/scene.rs `MarkView`.
/** type TreeMark = { at: Number, color: String, full?: Boolean } */

// view/scene.rs `TreeItemView`.
// `kind` is every string `tree_view`'s `match &item.draw` produces (72-tree.js
// draws the overflow kinds through its `default` branch).
/** type TreeItem = {
    surface: Number,
    interior?: Boolean,
    page?: Boolean,
    layer?: Boolean,
    key?: String,
    kind: "fill" | "wash" | "rule" | "border" | "overflow-before" | "overflow-after"
        | "scrollbar" | "scrim" | "lines" | "selectable" | "host",
    x: Number,
    y: Number,
    w: Number,
    h: Number,
    ox: Number,
    oy: Number,
    fg?: String,
    bg?: String,
    fgKey?: String,
    bgKey?: String,
    classes?: String,
    bold?: Boolean,
    italic?: Boolean,
    underline?: Boolean,
    dim?: Boolean,
    lines?: String[],
    border?: String,
    thumb?: [Number, Number],
    horizontal?: Boolean,
    marks?: TreeMark[]
} */

// view/scene.rs `TreeView` (`regions.tree`).
/** type TreeView = {
    surfaces: TreeSurface[],
    items: TreeItem[],
    cursor?: [Number, Number]
} */

// ───────────────────────────── native panels ─────────────────────────────

// view/native.rs `key_json`: a tree key (Key::Str | Key::Int | Key::Pair).
/** type TreeKey = String | Number | { ns: String, i: Number } */

// view/native.rs `NativeView` (`regions.native`).
/** type NativeView = { confirm: ConfirmCard | Null, panels: NativePanel[] } */

// view/native.rs `NativePanelView`. `rect`/`frame` are `json!` rects;
// `spec` is the panel's `WidgetSpec` run through `styled_spec`.
// view/native.rs `PageView`: a page's window, in content rows, and the row
// each keyed widget starts on (`[row, key]`).
/** type PageView = { offset: Number, window: Number, content: Number, marks: [Number, String][] } */

/** type NativePanel = {
    kind: "dock" | "floating" | "sidebar" | "pane",
    section?: Number,
    pane?: Number,
    rect: Rect,
    spec: WidgetSpec,
    instances: Dict<WidgetInstance>,
    focusKey: String,
    keyboard: Boolean,
    anchored: Boolean,
    frame?: Rect,
    title?: String,
    closeKey?: TreeKey,
    page?: PageView
} */

// view/native.rs `ConfirmCardView`.
/** type ConfirmCard = {
    title: String,
    body: String,
    detail: String,
    buttons: ConfirmButton[],
    key: TreeKey
} */

// view/native.rs `ConfirmButtonView`. `label` is [before, accelerator, after].
/** type ConfirmButton = {
    label: [String, String, String],
    destructive: Boolean,
    selected: Boolean,
    hovered: Boolean,
    escape: Boolean,
    key: TreeKey
} */

// view/native.rs `instance_json` over fresh-editor-core `WidgetInstanceState`.
// Rust sends one of several untagged shapes (list/radio: selectedIndex;
// text: textValue..completionNavigated; tree: selectedIndex, expandedKeys;
// number: numberValue; dropdown: selectedIndex, dropdownOpen, highlight;
// dual list: included..includedCursor). With no tag to narrow on, it is
// declared as one row whose fields are each present in some shape.
/** type WidgetInstance = {
    selectedIndex?: Number,
    textValue?: String,
    cursorByte?: Number,
    completions?: String[],
    completionSelected?: Number,
    completionNavigated?: Boolean,
    expandedKeys?: String[],
    numberValue?: Number,
    dropdownOpen?: Boolean,
    highlight?: Number | Null,
    included?: String[],
    activeIncluded?: Boolean,
    availableCursor?: Number,
    includedCursor?: Number
} */

// ───────────────────────────── widget specs (fresh-core) ─────────────────────────────

// fresh-core/src/api.rs `WidgetSpec` (tag = "kind", camelCase variants and
// fields). `#[serde(default)]` fields without a skip are always sent.
/** type WidgetSpec =
    { kind: "row", children: WidgetSpec[], key?: String, wrap: Boolean, justifyEnd: Boolean }
    | { kind: "col", children: WidgetSpec[], key?: String }
    | { kind: "hintBar", entries: HintEntry[], key?: String }
    | { kind: "toggle", checked: Boolean, label: String, focused: Boolean,
        indeterminate: Boolean, labelFirst: Boolean, labelWidth: Number,
        mnemonic?: String, key?: String }
    | { kind: "number", value: Number, min?: Number, max?: Number, step: Number,
        integer: Boolean, percent: Boolean, label?: String, focused: Boolean,
        labelWidth: Number, key?: String }
    | { kind: "dropdown", options: String[], selectedIndex: Number, label?: String,
        focused: Boolean, labelWidth: Number, open: Boolean, scrollOffset: Number,
        key?: String }
    | { kind: "radio", options: String[], selectedIndex: Number, label?: String,
        focused: Boolean, labelWidth: Number, key?: String }
    | { kind: "dualList", options: DualListOption[], included: String[],
        excluded: String[], label?: String, focused: Boolean, activeIncluded: Boolean,
        availableCursor: Number, includedCursor: Number, hint?: String,
        visibleRows: Number, key?: String }
    | { kind: "button", label: String, focused: Boolean, intent: ButtonKind,
        key?: String, disabled: Boolean, focusable: Boolean, bare: Boolean,
        fullWidth: Boolean, hoverStyle?: OverlayOptions, style?: OverlayOptions }
    | { kind: "spacer", cols: Number, flex: Boolean, key?: String }
    | { kind: "divider", ch: String, style?: OverlayOptions, key?: String }
    | { kind: "list", items: TextPropertyEntry[], itemSpecs?: WidgetSpec[],
        itemKeys: String[], selectedIndex: Number, visibleRows?: Number,
        focusable: Boolean, typeAhead: Boolean, key?: String }
    | { kind: "tree", nodes: TreeNode[], itemKeys: String[], selectedIndex: Number,
        visibleRows?: Number, expandedKeys: String[], checkable: Boolean,
        itemHeight: Number, cardBorders: Boolean, indentCols: Number,
        toggleOnClick: Boolean, columns?: TableColumn[], key?: String }
    | { kind: "text", value: String, cursorByte: Number, focused: Boolean,
        label?: String, placeholder?: String, rows: Number, fieldWidth: Number,
        maxVisibleChars: Number, fullWidth: Boolean, completions?: CompletionItem[],
        completionsVisibleRows: Number, minRows: Number, maxRows: Number,
        blockCaret: Boolean, selStart: Number, selEnd: Number, labelWidth: Number,
        readOnly: Boolean, markdown: Boolean, combo: Boolean, key?: String }
    | { kind: "labeledSection", label: String, child: WidgetSpec, widthPct?: Number,
        widthCols?: Number, key?: String, hoverStyle?: OverlayOptions }
    | { kind: "windowEmbed", windowId: Number, rows: Number, key?: String }
    | { kind: "label", text: String, style?: OverlayOptions, labelWidth: Number,
        segments?: StyledSegment[], wrap: Boolean, elide: Elide }
    | { kind: "raw", entries: TextPropertyEntry[], key?: String }
    | { kind: "overlay", child: WidgetSpec, key?: String }
    | { kind: "component", child: WidgetSpec, key?: String }
    | { kind: "popup", child: WidgetSpec, key?: String, anchor?: [Number, Number],
        screenSpace: Boolean } */

// fresh-core/src/api.rs `HintEntry`.
/** type HintEntry = { keys: String, label: String } */

// fresh-core/src/api.rs `ButtonKind` (camelCase unit variants).
/** type ButtonKind = "normal" | "primary" | "danger" */

// fresh-core/src/api.rs `Elide` (camelCase unit variants).
/** type Elide = "none" | "tail" | "head" */

// fresh-core/src/api.rs `OverlayColorSpec` (untagged): [r, g, b] or a theme key.
/** type OverlayColor = [Number, Number, Number] | String */

// fresh-core/src/api.rs `OverlayOptions`, plus `fgCss`/`bgCss`, which
// view/native.rs `resolve_styles` adds to every `style`/`hoverStyle` object in
// a native panel's spec (never in `Palette.toolbar`).
/** type OverlayOptions = {
    fg?: OverlayColor,
    bg?: OverlayColor,
    underline: Boolean,
    bold: Boolean,
    italic: Boolean,
    strikethrough: Boolean,
    extendToLineEnd: Boolean,
    reversed: Boolean,
    fgOnCollisionOnly: Boolean,
    url?: String,
    fgCss?: String,
    bgCss?: String
} */

// fresh-core/src/api.rs `CompletionItem`.
/** type CompletionItem = { value: String, kind?: String } */

// fresh-core/src/api.rs `DualListOption`.
/** type DualListOption = { value: String, label: String } */

// fresh-core/src/api.rs `TreeNode`.
/** type TreeNode = {
    text: TextPropertyEntry,
    depth: Number,
    hasChildren: Boolean,
    flush?: Boolean,
    draggable?: Boolean,
    checked?: Boolean,
    extraLines?: TextPropertyEntry[],
    windowAnchor?: TextWindowAnchor,
    action?: String,
    cells?: TableCell[]
} */

// fresh-core/src/api.rs `TableCell`.
/** type TableCell = { text: String, style?: OverlayOptions } */

// fresh-core/src/api.rs `TableColumn`.
/** type TableColumn = { title: String, elide: Elide, maxWidth: Number } */

// fresh-core/src/api.rs `TextWindowAnchor`.
/** type TextWindowAnchor = { pinned: Number, start: Number, len: Number } */

// fresh-core/src/text_property.rs `TextPropertyEntry`.
/** type TextPropertyEntry = {
    text: String,
    properties: Dict<JsonValue>,
    style?: OverlayOptions,
    inlineOverlays?: InlineOverlay[],
    segments?: StyledSegment[],
    padToChars?: Number,
    truncateToChars?: Number
} */

// fresh-core/src/text_property.rs `StyledSegment`.
/** type StyledSegment = { text: String, style?: OverlayOptions, overlays?: InlineOverlay[] } */

// fresh-core/src/text_property.rs `InlineOverlay`. `unit` is omitted when "byte".
/** type InlineOverlay = {
    start: Number,
    end: Number,
    style: OverlayOptions,
    properties?: Dict<JsonValue>,
    unit?: OffsetUnit
} */

// fresh-core/src/text_property.rs `OffsetUnit`.
/** type OffsetUnit = "byte" | "char" */

// serde_json::Value (MenuItem `args`, text-property `properties`).
/** type JsonValue = Null | Boolean | Number | String | JsonValue[] | Dict<JsonValue> */

// ───────────────────────────── modals ─────────────────────────────

// view/scene.rs `ContextMenuView`.
/** type ContextMenu = {
    kind: "fileExplorer" | "newTab" | "tab" | "closeSplit",
    x: Number,
    y: Number,
    highlighted: Number,
    items: String[]
} */

// view/scene.rs `AuxLine`.
/** type AuxLine = { text: String, selected: Boolean } */

// view/scene.rs `AuxModalView`.
/** type AuxModal = {
    kind: "eventDebug" | "themeInfo",
    title: String,
    rect: Rect | Null,
    lines: AuxLine[],
    footer: String | Null
} */

// view/scene.rs `KbSearchView`.
/** type KbSearch = {
    active: Boolean,
    focused: Boolean,
    mode: "text" | "recordKey",
    query: String,
    keyDisplay: String
} */

// view/scene.rs `KbRow` (tag = "type", camelCase).
/** type KbRow =
    { type: "section", name: String, collapsed: Boolean, count: Number, selected: Boolean }
    | { type: "binding", key: String, action: String, description: String,
        context: String, source: "keymap" | "custom" | "plugin" | "", selected: Boolean } */

// view/scene.rs `KbEditDialog`. `focusArea`: 0 key, 1 action, 2 context, 3 buttons.
/** type KbEditDialog = {
    title: String,
    focusArea: Number,
    keyDisplay: String,
    keyCapturing: Boolean,
    actionText: String,
    actionError: String | Null,
    autocomplete: String[],
    autocompleteSelected: Number | Null,
    context: String,
    contextOptions: String[],
    conflicts: String[],
    saveFocused: Boolean,
    cancelFocused: Boolean
} */

// view/scene.rs `KbConfirm`.
/** type KbConfirm = { buttons: String[], selected: Number } */

// view/scene.rs `KeybindingEditorView`.
/** type KeybindingEditor = {
    title: String,
    configPath: String,
    keymaps: String[],
    search: KbSearch,
    contextFilter: String,
    contextFiltered: Boolean,
    sourceFilter: String,
    sourceFiltered: Boolean,
    count: String,
    hasChanges: Boolean,
    rows: KbRow[],
    selected: Number,
    scrollOffset: Number,
    viewport: Number,
    showingHelp: Boolean,
    editDialog: KbEditDialog | Null,
    confirm: KbConfirm | Null
} */

// ───────────────────────────── settings ─────────────────────────────

// view/scene.rs `SettingControlView` (tag = "kind", camelCase variants).
// The enum's rename_all renames variants only; fields are camelCase only on
// the variants that say so (dualList, map, objectArray).
/** type SettingControl =
    { kind: "toggle", checked: Boolean }
    | { kind: "number", value: Number, min: Number | Null, max: Number | Null,
        integer: Boolean, percent: Boolean }
    | { kind: "dropdown", selected: Number, options: String[], open: Boolean }
    | { kind: "text", value: String, editing: Boolean, placeholder: String }
    | { kind: "textList", items: String[], focused: Number | Null }
    | { kind: "dualList", included: String[], available: String[],
        includedCursor: Number, availableCursor: Number,
        activeColumn: "included" | "available" }
    | { kind: "map", entries: MapEntry[], column: String | Null, noAdd: Boolean,
        focused: Number | Null, addFocused: Boolean }
    | { kind: "objectArray", entries: String[], focused: Number | Null, addFocused: Boolean }
    | { kind: "json", value: String }
    | { kind: "complex", type_name: String } */

// view/scene.rs `MapEntryView`.
/** type MapEntry = { key: String, display: String } */

// view/scene.rs `SettingItemView`.
/** type SettingItem = {
    index: Number,
    path: String,
    name: String,
    description: String | Null,
    section: String | Null,
    sectionStart: Boolean,
    modified: Boolean,
    readOnly: Boolean,
    nullable: Boolean,
    isNull: Boolean,
    selected: Boolean,
    control: SettingControl
} */

// view/scene.rs `SettingsCategoryView`.
/** type SettingsCategory = {
    index: Number,
    name: String,
    selected: Boolean,
    expandable: Boolean,
    expanded: Boolean,
    sections: String[],
    nested: Boolean
} */

// view/scene.rs `SettingsSearchResultView`.
/** type SettingsSearchResult = { name: String, category: String } */

// view/scene.rs `EntryDialogView`.
/** type EntryDialog = {
    title: String,
    isNew: Boolean,
    items: SettingItem[],
    selectedItem: Number,
    focusOnButtons: Boolean,
    focusedButton: Number,
    noDelete: Boolean
} */

// view/scene.rs `SettingsView`.
/** type Settings = {
    title: String,
    focus: "categories" | "settings" | "footer",
    targetLayer: String,
    categories: SettingsCategory[],
    items: SettingItem[],
    footerButtons: String[],
    footerSelected: Number,
    searchActive: Boolean,
    searchQuery: String,
    searchResults: SettingsSearchResult[],
    searchSelected: Number,
    entryDialog: EntryDialog | Null,
    showingHelp: Boolean,
    showingConfirm: Boolean,
    showingReset: Boolean
} */
