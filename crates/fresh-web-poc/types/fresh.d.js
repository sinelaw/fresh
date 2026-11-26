// Types shared by the web client's modules, for the inty checker
// (`../typecheck.sh`). Declarations only: nothing here runs.

// An edit primitive over byte offsets (ot.js); an edit is a sequence of them.
/** type Op = {t: "ins", pos: Number, text: String} | {t: "del", pos: Number, len: Number} */
// An op as a document's log has it: with the newlines it inserted or
// deleted (`doc.ops`).
/** type LoggedOp =
 *   | {t: "ins", pos: Number, text: String, nl: Number}
 *   | {t: "del", pos: Number, len: Number, nl: Number}
 */

// A colour as plugins and themes give it: a theme key ("syntax.keyword"), a
// CSS colour, a colour name, "Indexed:N", or [r, g, b].
/** type Color = String | Number[] */

// A syntax span of a line (highlight.js), in UTF-16 indices; `k` is a theme
// key.
/** type Highlight = {s: Number, e: Number, k: String, bold: Boolean, italic: Boolean} */

// A style as drawn: CSS colours ("" for none) and attributes. `mono` is
// inline code in a document; `markup` revealed Markdown source, drawn dim;
// `wavy` a diagnostic's underline colour ("" for none).
/** type Drawn = {
 *   fg: String,
 *   bg: String,
 *   bold: Boolean,
 *   italic: Boolean,
 *   underline: Boolean,
 *   strike: Boolean,
 *   mono: Boolean,
 *   markup: Boolean,
 *   wavy: String,
 * } */

// A syntax span of a line: UTF-16 [s, e) and its finished style.
/** type Span = {s: Number, e: Number, style: Drawn} */

// A byte range [start, end).
/** type Range = {start: Number, end: Number} */
// A fold hiding a line, and the last line it keeps.
/** type Hidden = {fold: Range, keep: Number} */
// A screen place: row and column.
/** type XY = {r: Number, x: Number} */
// Mouse presses in a row: the last one's time and client place, and how
// many there were.
/** type Clicks = {t: Number, x: Number, y: Number, n: Number} */
// What a sash does (sash.js): measure as a drag starts (its context `C`),
// apply a move of so many pixels, finish, and put the default size back.
/** type SashOptions<C> = {
 *   begin: () => C,
 *   drag: (C, Number) => Undefined,
 *   end: () => Undefined,
 *   reset: () => Undefined,
 * } */
// A block selection's corner: a line and a screen column (perhaps past
// the line's end).
/** type Corner = {line: Number, col: Number} */
// A view's options: its compose width in columns (none: the setting's),
// whether lines wrap, and whether line numbers show.
/** type ViewOptions = {composeWidth: Number | Null, wrap: Boolean, lineNumbers: Boolean} */

// A buffer's held lines (a BufferHandle's lines()): their text, their byte
// starts, and the document line of the first (null without a line index).
/** type HeldLines = {lines: String[], starts: Number[], firstLine: Number | Null} */

// ---------------------------------------------------------------- records
//
// A record lists every field, at each place one is made. Only a field that
// may have no value is nullable (`T | Null`, written `null`); any other has
// a real default (`bold: false`, `when: "always"`). No spread overrides a
// field of one.

// ---------------------------------------------------------------- decorations
// (host.js's DecorationSet). `when` is "always", or the view mode a
// decoration applies in ("source" or "compose").

// A decoration's style: its colours (none: the text's own) and the
// attributes it turns on. `mono` is inline code in a compose view;
// `extendToLineEnd` carries an overlay's background to the row's end.
/** type Style = {
 *   fg: Color | Null,
 *   bg: Color | Null,
 *   bold: Boolean,
 *   italic: Boolean,
 *   underline: Boolean,
 *   strike: Boolean,
 *   mono: Boolean,
 *   extendToLineEnd: Boolean,
 * } */

// `mark`: the line is marked on the scrollbar in the style's background;
// `above`: drawn over the selection and the occurrence highlight.
/** type OverlayOptions = {when: String, mark: Boolean, above: Boolean} */

// `text` is drawn instead of the range; `reveal`: the range shown as source
// while a cursor is inside it.
/** type ConcealOptions = {text: String, style: Style, reveal: Range | Null, when: String} */

// `side`: "before" or "after" the character at the position; `edge`: at the
// row's right edge instead.
/** type InlineOptions = {style: Style, side: String, edge: Boolean, when: String} */

// A whole row of text above or below a line; rows at one line go by `order`.
/** type LineOptions = {text: String, style: Style, above: Boolean, order: Number, when: String} */

// Document structure a compose view draws: `type` is "heading" (`level`),
// "paragraph", "item" (`depth`, `marker`), "quote" (`depth`), "code"
// (`lang`), "table" (`align`: per column "left", "center", "right" or
// null), "rule", and inside a table "row" (`header`, `delimiter`) and
// "cell". A field a type doesn't use is at its default (0, "", [], false).
/** type Block = {
 *   type: String,
 *   level: Number,
 *   depth: Number,
 *   marker: String,
 *   lang: String,
 *   align: (String | Null)[],
 *   header: Boolean,
 *   delimiter: Boolean,
 * } */

/** type GutterOptions = {glyph: String, color: Color, title: String} */

// The decorations a buffer holds (js/decor.js), each in a plugin's
// namespace `ns`: the options above, placed.
/** type Overlay = {
 *   ns: String,
 *   start: Number,
 *   end: Number,
 *   style: Style,
 *   when: String,
 *   mark: Boolean,
 *   above: Boolean,
 * } */
/** type Conceal = {
 *   ns: String,
 *   start: Number,
 *   end: Number,
 *   text: String,
 *   style: Style,
 *   reveal: Range | Null,
 *   when: String,
 * } */
/** type InlineText = {
 *   ns: String,
 *   pos: Number,
 *   text: String,
 *   style: Style,
 *   side: String,
 *   edge: Boolean,
 *   when: String,
 * } */
/** type VirtualLine = {
 *   ns: String,
 *   pos: Number,
 *   text: String,
 *   style: Style,
 *   above: Boolean,
 *   order: Number,
 *   when: String,
 * } */
/** type DocNode = {
 *   ns: String,
 *   start: Number,
 *   end: Number,
 *   type: String,
 *   level: Number,
 *   depth: Number,
 *   marker: String,
 *   lang: String,
 *   align: (String | Null)[],
 *   header: Boolean,
 *   delimiter: Boolean,
 *   when: String,
 * } */
/** type GutterMark = {
 *   ns: String,
 *   start: Number,
 *   end: Number,
 *   glyph: String,
 *   color: Color,
 *   title: String,
 * } */

// ---------------------------------------------------------------- navigation

// Where a file opens (`fresh.buffers.open`): at `line` (1-based; null: where
// its view is) and `column` (UTF-16, 1-based), in `place` ("tab", "split"
// or "right"), as the split's preview tab (`preview` true), in a tab of its
// own (false) or as it is shown already (null); `focus: false` leaves the
// keyboard where it is (a list previewing what it selects).
/** type OpenOptions = {
 *   line: Number | Null,
 *   column: Number,
 *   place: String,
 *   preview: Boolean | Null,
 *   focus: Boolean,
 * } */

// A view's cursor: its position, its selection's anchor (null: no
// selection) and the column vertical motion aims for (null: its own).
/** type Cursor = {pos: Number, anchor: Number | Null, goalX: Number | Null} */

// The layout a workspace's page remembers (session.js): each split's
// tabs (a file by its path, an untitled document by its number) with
// their cursor and scroll and the one shown, the splits' directions and
// ratios, the active split, and whether the sidebar and terminal show.
// What a remembered tab shows: a file, an untitled document, a terminal.
/** type TabRef =
 *   | {kind: "file", path: String}
 *   | {kind: "untitled", n: Number}
 *   | {kind: "terminal", term: Number}
 */
/** type SavedTab = {ref: TabRef, pos: Number, top: Number, preview: Boolean} */
/** type SavedNode =
 *   | {kind: "pane", tabs: SavedTab[], active: String | Null}
 *   | {kind: "split", dir: String, ratio: Number, a: SavedNode, b: SavedNode} */
/** type SavedLayout = {tree: SavedNode, active: Number, sidebar: Boolean} */
/** type SessionData = {layout: SavedLayout | Null, recent: String[], sizes: Dict<Number>} */

// The editor's settings: those on or off by name (`flags`), the auto-save
// delay, the columns ruled, a compose view's width, the keymap, the theme
// and the zoom.
/** type Settings = {
 *   flags: Dict<Boolean>,
 *   auto_save_seconds: Number,
 *   rulers: Number[],
 *   page_width: Number,
 *   keymap: String,
 *   theme: String,
 *   zoom: Number,
 * } */

// A cursor moved (`setCursor`, `select`): `center` scrolls it to the middle.
/** type CursorOptions = {center: Boolean} */

// ---------------------------------------------------------------- panels

// A panel (`fresh.ui.panel`) in `place`: "bottom" (the dock), "sidebar",
// "left" (a column before the sidebar), "right", "bar" (above the editor)
// or "dialog"; `onClose` runs when it is closed.
/** type PanelOptions = {
 *   id: String,
 *   title: String,
 *   place: String,
 *   onClose: (() => Undefined) | Null,
 * } */

// ---------------------------------------------------------------- panel UI
//
// What a panel shows (ui.js): a tree of elements, each built by its
// kind's function from its props, every prop given (start from the kind's
// defaults: `button({ ...BUTTON, onClick }, "Save")`). A `key` names a
// control across renders: its focus, caret and (a list's or a tree's)
// selection, scroll and expansion are kept by it. A list of at most one
// is a prop that may be absent.

/** type Point = {x: Number, y: Number} */

/** type BoxProps = {class: String} */
/** type TextProps = {
 *   class: String,
 *   dim: Boolean,
 *   mono: Boolean,
 *   bold: Boolean,
 *   color: String[],
 *   title: String,
 * } */
// `field`: part of a box beside it (a ▾ for its values), drawn as the box is.
/** type ButtonProps = {
 *   primary: Boolean,
 *   field: Boolean,
 *   disabled: Boolean,
 *   title: String,
 *   onClick: (DomEvent<El>) => Undefined,
 * } */
// A compact on/off button, as in find bars: [Aa] [ab] [.*].
/** type ToggleProps = {
 *   key: String,
 *   on: Boolean,
 *   title: String,
 *   onChange: (Boolean) => Undefined,
 * } */
// `onKey` true: the key is taken (nothing else happens); `list`: the list
// the arrow keys move; `onEnter` gets the text and the key.
/** type InputProps = {
 *   key: String,
 *   value: String,
 *   placeholder: String,
 *   grow: Boolean,
 *   list: String[],
 *   onInput: (String) => Undefined,
 *   onEnter: (String, DomEvent<El>) => Undefined,
 *   onKey: (DomEvent<El>) => Boolean,
 * } */
// Text of several lines (Enter is a new line), `rows` tall.
/** type TextareaProps = {
 *   key: String,
 *   rows: Number,
 *   value: String,
 *   placeholder: String,
 *   onInput: (String) => Undefined,
 *   onKey: (DomEvent<El>) => Boolean,
 * } */
/** type CheckboxProps = {
 *   key: String,
 *   checked: Boolean,
 *   disabled: Boolean,
 *   title: String,
 *   onChange: (Boolean) => Undefined,
 * } */
/** type SelectOption = {value: String, label: String} */
/** type SelectProps = {
 *   key: String,
 *   value: String,
 *   options: SelectOption[],
 *   onChange: (String) => Undefined,
 * } */

// A list of `items` (virtualised: any number), each row drawn by `render`;
// the keys move its selection. `how` an item was selected: "mouse" (a
// press) or "key" (the arrows); activated: "click" (`activateOnClick`, the
// first click of one or more), "double" or "key" (Enter); the event says
// which modifiers were held. `header` rows are skipped by the keys;
// `marked` rows are in a multiple selection (drawn as selected). A tree's
// rows (`depth` given) keep the rows of the folders holding the top
// visible row pinned at the top. `onKey` gets the selected item (none in
// an empty list). `drop`: rows can be dragged onto rows (or empty space
// below them: none).
/** type ListDrop<T> = {canDrop: (T, T[]) => Boolean, onDrop: (T, T[]) => Undefined} */
/** type ListProps<T> = {
 *   key: String,
 *   class: String,
 *   items: T[],
 *   render: (T, Number) => UINode,
 *   selected: Number[],
 *   activateOnClick: Boolean,
 *   depth: ((T) => Number)[],
 *   header: (T) => Boolean,
 *   marked: (T) => Boolean,
 *   onSelect: (T, Number, String, DomEvent<El>) => Undefined,
 *   onActivate: (T, Number, String, DomEvent<El>) => Undefined,
 *   onContext: (T, Number, Point) => Undefined,
 *   onKey: (DomEvent<El>, T[], Number) => Undefined,
 *   drop: ListDrop<T>[],
 * } */
// A list as ui.js draws it: its rows by index (`count` of them).
/** type RowDrop = {
 *   canDrop: (Number, Number[]) => Boolean,
 *   onDrop: (Number, Number[]) => Undefined,
 * } */
/** type Rows = {
 *   key: String,
 *   class: String,
 *   count: Number,
 *   row: (Number) => UINode,
 *   selected: Number[],
 *   activateOnClick: Boolean,
 *   depth: ((Number) => Number)[],
 *   header: (Number) => Boolean,
 *   marked: (Number) => Boolean,
 *   onSelect: (Number, String, DomEvent<El>) => Undefined,
 *   onActivate: (Number, String, DomEvent<El>) => Undefined,
 *   onContext: (Number, Point) => Undefined,
 *   onKey: (DomEvent<El>, Number) => Undefined,
 *   drop: RowDrop[],
 * } */

// A tree of nodes holding the plugin's `data` (a leaf has no children, a
// folder may have none; `open` at first); which are open is kept by id.
/** type TreeNode<D> = {
 *   id: String,
 *   label: UINode,
 *   children: TreeNode<D>[],
 *   open: Boolean,
 *   leaf: Boolean,
 *   data: D,
 * } */
/** type TreeProps<D> = {
 *   key: String,
 *   nodes: TreeNode<D>[],
 *   onActivate: (D, TreeNode<D>) => Undefined,
 *   onSelect: (D, TreeNode<D>) => Undefined,
 *   onContext: (D, TreeNode<D>, Point) => Undefined,
 *   onKey: (DomEvent<El>, D[]) => Undefined,
 * } */
// A tree as ui.js draws it: each node with what it does.
/** type Branch = {
 *   id: String,
 *   label: UINode,
 *   children: Branch[],
 *   open: Boolean,
 *   leaf: Boolean,
 *   activate: () => Undefined,
 *   select: () => Undefined,
 *   context: (Point) => Undefined,
 *   key: (DomEvent<El>) => Undefined,
 * } */

/** type UINode =
 *   | {kind: "row", props: BoxProps, children: UINode[]}
 *   | {kind: "col", props: BoxProps, children: UINode[]}
 *   | {kind: "spacer"}
 *   | {kind: "text", props: TextProps, text: String}
 *   | {kind: "markdown", text: String}
 *   | {kind: "button", props: ButtonProps, label: String}
 *   | {kind: "toggle", props: ToggleProps, label: String}
 *   | {kind: "checkbox", props: CheckboxProps, label: String}
 *   | {kind: "input", props: InputProps}
 *   | {kind: "textarea", props: TextareaProps}
 *   | {kind: "select", props: SelectProps}
 *   | {kind: "list", rows: Rows}
 *   | {kind: "tree", key: String, nodes: Branch[], none: (DomEvent<El>) => Undefined} */

// What the UI kit asks of the editor (ui.js): the keyboard back to it,
// and its panes redrawn (the dock took or gave room).
/** type UIKitEditor = {focus: () => Undefined, redraw: () => Undefined} */

// A button in a panel's head: its icon, tooltip and what it does.
/** type PanelAction = {icon: String, title: String, run: () => Undefined} */
// A panel (`fresh.ui.panel`): `render` shows a tree of elements in it;
// `focus` gives the keyboard to the control keyed `key`, `focusFirst` to
// its first control; `open` while it is on screen.
/** type UIPanel = {
 *   render: (UINode) => Undefined,
 *   focus: (String) => Undefined,
 *   focusFirst: () => Undefined,
 *   actions: (PanelAction[]) => Undefined,
 *   close: () => Undefined,
 *   show: () => Undefined,
 *   open: Boolean,
 * } */

// ---------------------------------------------------------------- buffers

// A piece of a virtual buffer's text: `props` are the plugin's own (read
// back with `propsAt`), `style` overlays it.
/** type Entry = {text: String, props: Dict<String>, style: Style | Null} */

// A virtual buffer (`fresh.buffers.createVirtual`): `mode` names the keys
// that apply in it (`fresh.modes.define`), `keep` keeps it while no view
// shows it, `place` shows it ("tab", "split" or "right"; null: not shown).
/** type VirtualOptions = {
 *   name: String,
 *   entries: Entry[],
 *   mode: String | Null,
 *   readOnly: Boolean,
 *   language: String | Null,
 *   keep: Boolean,
 *   place: String | Null,
 *   lineNumbers: Boolean,
 * } */

// ---------------------------------------------------------------- handles
//
// What a plugin holds of a buffer, a view and a buffer's decorations
// (host.js): functions, each reading or changing the application's own as
// it is when called.

// A buffer's decoration set (`decorations(name)`; each plugin names its
// own): `gutters()` and `overlays()` are its marks as edits moved them.
/** type DecorationSet = {
 *   clear: () => Undefined,
 *   clearRange: (Number, Number) => Undefined,
 *   overlay: (Number, Number, Style, OverlayOptions) => Undefined,
 *   conceal: (Number, Number, ConcealOptions) => Undefined,
 *   inline: (Number, String, InlineOptions) => Undefined,
 *   line: (Number, LineOptions) => Undefined,
 *   node: (Number, Number, Block, String) => Undefined,
 *   gutter: (Number, GutterOptions) => Undefined,
 *   gutters: () => {pos: Number, glyph: String}[],
 *   overlays: () => Range[],
 * } */

// A buffer: `path()` is "" while untitled; `whole()`: its whole text is
// held (an ordinary file, or a virtual buffer); `text(start, end)` is null
// where the text is not held; `lines()` the held lines; `propsAt(pos)` a
// virtual buffer's entries' props there; `setFoldRanges` the foldable
// blocks as document line ranges (a language server's; folding uses
// indentation where none are given).
/** type BufferHandle = {
 *   id: Number,
 *   isVirtual: Boolean,
 *   path: () => String,
 *   name: () => String,
 *   language: () => String | Null,
 *   length: () => Number,
 *   modified: () => Boolean,
 *   whole: () => Boolean,
 *   text: (Number, Number) => String | Null,
 *   textAsync: (Number, Number) => Promise<String>,
 *   lines: () => HeldLines | Null,
 *   edit: (Op[]) => Undefined,
 *   insert: (Number, String) => Undefined,
 *   delete: (Number, Number) => Undefined,
 *   replace: (Number, Number, String) => Undefined,
 *   save: () => Promise<Undefined>,
 *   decorations: (String) => DecorationSet,
 *   views: () => ViewHandle[],
 *   setEntries: (Entry[]) => Undefined,
 *   propsAt: (Number) => Dict<String>[],
 *   setFoldRanges: (LspFold[]) => Undefined,
 *   render: () => Undefined,
 * } */

// A view of a buffer: its primary cursor and selection, its mode
// ("source" or "compose") and a compose view's width (null: the
// setting's). `setCursor` and `select` (the caret at the end) resolve once
// the text there is loaded and the view shows it.
/** type ViewHandle = {
 *   buffer: BufferHandle,
 *   cursor: () => Number,
 *   selection: () => Range | Null,
 *   mode: () => String,
 *   composeWidth: () => Number | Null,
 *   setMode: (String) => Undefined,
 *   setComposeWidth: (Number | Null) => Undefined,
 *   setCursor: (Number, CursorOptions) => Promise<Undefined>,
 *   select: (Number, Number, CursorOptions) => Promise<Undefined>,
 * } */

// What a buffer event says of a change: its ops ("buffer.changed"), and
// whether it was made here.
/** type BufferChange = {ops: Op[], local: Boolean} */

// ---------------------------------------------------------------- protocol

// A document as the server describes it (`doc.open`, `doc.saved`).
/** type DocInfo = {
 *   doc: Number,
 *   path: String | Null,
 *   untitled: Number | Null,
 *   len: Number,
 *   version: Number,
 *   lines: Number | Null,
 *   large: Boolean,
 *   modified: Boolean,
 *   language: String | Null,
 *   encoding: String,
 *   line_ending: String,
 *   readonly: Boolean,
 * } */

// A buffer's replica of a server document (buffer.js): the log version
// seen; this client's edits sent and not yet sequenced (a batch at most),
// and those not yet sent; the log as sequenced lately (for a window
// fetched late); the window being fetched (at most one).
/** type LogEntry = {version: Number, ops: LoggedOp[]} */
/** type Replica = {
 *   doc: Number,
 *   version: Number,
 *   inflight: Op[][],
 *   pending: Op[],
 *   recent: LogEntry[],
 *   fetching: Promise<Undefined>[],
 * } */
// A plugin's entry in its buffer: its bytes, properties and style.
/** type EntryRange = {start: Number, end: Number, props: Dict<String>, style: Style | Null} */
// Where a buffer's text comes from: a server document (its replica), a
// plugin's entries, or a terminal (its tab has no text).
/** type Source =
 *   | {kind: "file", replica: Replica}
 *   | {kind: "virtual", ranges: EntryRange[]}
 *   | {kind: "terminal", term: Number} */

// A language server's diagnostic, at byte offsets.
/** type Diagnostic = {
 *   start: Number,
 *   end: Number,
 *   severity: Number,
 *   message: String,
 *   source: String | Null,
 * } */

// ---------------------------------------------------------------- language servers

// What `fresh.lspRequest` answers, one shape per method (src/lsp_shapes.rs).
// Positions are the protocol's: lines and UTF-16 columns.
/** type LspPos = {line: Number, character: Number} */
/** type LspRange = {start: LspPos, end: LspPos} */
// definition, declaration, typeDefinition, implementation, references:
/** type LspLocation = {path: String, range: LspRange} */
// documentSymbol (the range is the name's):
/** type LspSymbol = {
 *   name: String,
 *   kind: Number,
 *   container: String | Null,
 *   range: LspRange,
 *   children: LspSymbol[],
 * } */
// workspace/symbol:
/** type LspWorkspaceSymbol = {
 *   name: String,
 *   kind: Number,
 *   container: String | Null,
 *   path: String,
 *   range: LspRange,
 * } */
// formatting, and each file's edits in a rename or a code action:
/** type LspTextEdit = {range: LspRange, newText: String} */
/** type LspFileEdit = {path: String, edits: LspTextEdit[]} */
// A command, its arguments as JSON text: the server's own, sent back as
// they came (workspace/executeCommand).
/** type LspCommand = {command: String, arguments: String} */
// codeAction:
/** type LspAction = {
 *   title: String,
 *   kind: String | Null,
 *   edit: LspFileEdit[] | Null,
 *   command: LspCommand | Null,
 * } */
// completion:
/** type LspCompletion = {label: String, detail: String | Null, insert: String} */
// signatureHelp (the active signature; null for none):
/** type LspSignature = {label: String, documentation: String | Null} */
// inlayHint:
/** type LspHint = {position: LspPos, label: String, paddingLeft: Boolean, paddingRight: Boolean} */
// foldingRange (document lines):
/** type LspFold = {start: Number, end: Number} */

// A menu's item (js/menu.js: action, actionOf, submenu, SEPARATOR): an
// action runs `run` (its `keys` shown beside it; `checked` true or false
// gives the row a check; `disabled` greys it out), a submenu opens `items`.
/** type MenuItem =
 *   | {kind: "action", label: String, keys: String | Null, checked: Boolean | Null, disabled: Boolean, run: () => Undefined}
 *   | {kind: "submenu", label: String, items: MenuItem[]}
 *   | {kind: "separator"} */

// A DOM element whose element-valued members (its children, its parent,
// `closest`) are elements again.
/** type El = Element<El> */

// A menu (js/menu.js `makeMenu`), built, then shown: at a point
// (`openAt`), under or beside a menu bar title (`openUnder`, true:
// beside), dropping from a control (`openBelow`, true: ending at its
// right edge), or as a submenu at (x, y), left of the third value when
// there is no room (`putAt`). `from`: a submenu's row in its parent.
/** type Menu = {
 *   el: El,
 *   from: Number | Null,
 *   enter: () => Undefined,
 *   close: () => Undefined,
 *   closeSub: () => Undefined,
 *   contains: (El) => Boolean,
 *   putAt: (Number, Number, Number) => Undefined,
 *   openAt: (Number, Number) => Undefined,
 *   openUnder: (DOMRect, Boolean) => Undefined,
 *   openBelow: (DOMRect, Boolean) => Undefined,
 * } */

// A skin as the server lists it (`skin.list`, src/skins.rs): what it
// extends, its frame ("full" or "bezel"), its scale (null: the base's),
// its furniture stylesheet and theme, and whether it is built in or the
// user's ("builtin", "user").
/** type SkinInfo = {
 *   id: String,
 *   name: String,
 *   description: String,
 *   author: String,
 *   extends: String | Null,
 *   frame: String,
 *   scale: Number | Null,
 *   furniture: String | Null,
 *   theme: String | Null,
 *   keys: String,
 *   source: String,
 * } */

// Pick from a list (`fresh.pick`): `value` is the text typed at first;
// `onChange` runs as an item is selected (a preview), `onAccept` with the
// item chosen, `onCancel` when it is given up (Escape, or Enter with
// nothing left to pick).
/** type PickItem<T> = {label: String, detail: String, value: T} */
/** type PickOptions<T> = {
 *   title: String,
 *   value: String,
 *   items: PickItem<T>[],
 *   onAccept: (PickItem<T>) => Undefined,
 *   onChange: (PickItem<T>) => Undefined,
 *   onCancel: () => Undefined,
 * } */
// Ask for text (`fresh.input`): `value` the text at first (all selected
// when `select`), `hint` shown under it.
/** type AskOptions = {title: String, value: String, hint: String, select: Boolean} */

// A key of a buffer mode (`fresh.defineMode`): `run` when it is pressed
// in a buffer of that mode.
/** type ModeKey = {key: String, run: () => Undefined} */

// A row a palette mode lists (`fresh.paletteMode`): `run` when it is
// chosen, `preview` when it is selected; a row with neither is a note.
// Where a view was, to go back to: remembered for Go Back, or restored.
/** type Spot = {remember: () => Undefined, restore: () => Undefined} */

/** type PaletteItem = {
 *   label: String,
 *   detail: String | Null,
 *   run: (() => Undefined) | Null,
 *   preview: (() => Undefined) | Null,
 * } */

// A workspace as the orchestrator lists it (src/session.rs: entry,
// status; src/terminal.rs: a terminal).
/** type TermInfo = {
 *   id: Number,
 *   name: String,
 *   command: String | Null,
 *   agent: Boolean,
 *   resume: String | Null,
 *   title: String,
 *   state: String,
 *   bells: Number,
 * } */
/** type WorkspaceStatus = {
 *   trusted: Boolean,
 *   docs: Number,
 *   modified: Number,
 *   terminals: TermInfo[],
 *   procs: Number,
 *   errors: Number,
 *   warnings: Number,
 *   branch: String | Null,
 *   viewers: Number,
 *   worked: Number,
 *   attention: String | Null,
 * } */
/** type Worktree = {repo: String, branch: String} */
// The document formats the server knows (`doc.formats`): languages,
// encodings (a name and what it is) and line endings.
/** type DocFormats = {
 *   languages: String[],
 *   encodings: {name: String, description: String}[],
 *   line_endings: String[],
 * } */
// A machine a workspace is on (src/authority.rs HostSpec): this one,
// another over ssh (with an identity file and more ssh options), a
// container, or the remote agent here.
/** type HostSpec =
 *   | {kind: "local"}
 *   | {kind: "ssh", target: String, identity: String | Null, options: String[]}
 *   | {kind: "container", id: String}
 *   | {kind: "agent"} */
/** type WorkspaceEntry = {
 *   id: Number,
 *   name: String,
 *   label: String | Null,
 *   root: String,
 *   host: String,
 *   folder: String,
 *   pinned: Boolean,
 *   created: Number,
 *   worktree: Worktree | Null,
 *   status: WorkspaceStatus | Null,
 *   error: String | Null,
 * } */

// A command's argument: what the palette passes after a prefix, a
// plugin's own (a theme's key); none for most.
/** type CommandArg = String | Undefined */
// A command: its title, what it does, where it came from (a plugin's
// name, "core"), and whether the palette leaves it out.
/** type Command = {
 *   title: String,
 *   run: (CommandArg) => Promise<Undefined>,
 *   source: String,
 *   hidden: Boolean,
 * } */
// A command as the plugin API lists it.
/** type CommandInfo = {id: String, title: String, source: String, hidden: Boolean} */
// The palette's rows for commands (`key`: its keys, "" for none) and open
// buffers (`stored`: the server keeps it; `path` "" for none).
/** type PaletteCommand = {title: String, key: String, source: String, run: () => Undefined} */
/** type PaletteBuffer = {
 *   name: String,
 *   path: String,
 *   modified: Boolean,
 *   stored: Boolean,
 *   show: () => Undefined,
 * } */
// What skins ask of the editor (skins.js): report a failure, say
// something, measure the cell again, put an editor theme on, pick from a
// list, and count the open files and their problems.
/** type SkinsEditor = {
 *   report: (String) => Undefined,
 *   say: (String) => Undefined,
 *   remeasure: () => Undefined,
 *   useTheme: (String) => Promise<Undefined>,
 *   pick: (PickOptions<String>) => Undefined,
 *   counts: () => {files: Number, errors: Number, warnings: Number},
 * } */
// What macros ask of the editor (macros.js): say something, a command's
// keys and title, redraw the status bar, run a command, type text, pick a
// register, and show a text.
/** type MacrosEditor = {
 *   say: (String) => Undefined,
 *   keyLabel: (String) => String,
 *   updateStatus: () => Undefined,
 *   run: (String, CommandArg) => Promise<Undefined>,
 *   insert: (String, Boolean) => Undefined,
 *   pick: (PickOptions<String>) => Undefined,
 *   titleOf: (String) => String,
 *   showText: (String, String) => Undefined,
 * } */
// What the menu bar asks of the editor (menubar.js): a command's title
// (none: no such command), its keys, run it, whether a toggle is on (a
// "setting" or a view "state"), and focus.
/** type MenuEditor = {
 *   title: (String) => String | Null,
 *   keyLabel: (String) => String,
 *   run: (String) => Undefined,
 *   isOn: ("setting" | "state" | "read", String) => Boolean,
 *   focus: () => Undefined,
 * } */
// What terminals ask of the editor (terminal.js): to take a key pressed
// in a terminal (true: it did), and that a terminal's label changed or a
// terminal appeared (another client opened it).
/** type TerminalsEditor = {
 *   key: (DomEvent<El>) => Boolean,
 *   changed: (Number) => Undefined,
 *   appeared: (Number) => Undefined,
 * } */
// What the palette asks of the editor (palette.js): where the active view
// is, go to a line (from 0), the commands, buffers, root and recent files
// it lists, open a file (at a line and column, from 1), and focus.
/** type PaletteEditor = {
 *   here: () => Spot[],
 *   goto: (Number) => Undefined,
 *   commands: () => PaletteCommand[],
 *   buffers: () => PaletteBuffer[],
 *   root: () => String,
 *   recent: () => String[],
 *   open: (String) => Undefined,
 *   openAt: (String, Number, Number) => Undefined,
 *   focus: () => Undefined,
 * } */

// An operation's outcome (rpc.js `attempt`): its answer, or the server's
// refusal (`code`: its stable reason, `detail`: its words).
/** type Outcome<T> = {kind: "ok", value: T} | {kind: "refused", code: String, detail: String} */

// What the server pushes over the event stream.
// A terminal's output (from stream offset `at`), its exit, its new size.
/** type TermEvent =
 *   | {ev: "term.data", term: Number, at: Number, data: String}
 *   | {ev: "term.exit", term: Number}
 *   | {ev: "term.size", term: Number, cols: Number, rows: Number} */
// Edits the server sequenced, as a document's log pushes them.
/** type DocOps = {
 *   ev: "doc.ops",
 *   doc: Number,
 *   version: Number,
 *   client: Number,
 *   ops: LoggedOp[],
 *   len: Number,
 *   modified: Boolean,
 * } */
/** type ServerEvent =
 *   | DocOps
 *   | {ev: "doc.saved", doc: Number, info: DocInfo}
 *   | {ev: "doc.renamed", doc: Number, info: DocInfo}
 *   | {ev: "doc.info", doc: Number, info: DocInfo}
 *   | {ev: "doc.deleted", doc: Number, info: DocInfo}
 *   | {ev: "doc.reloaded", doc: Number, info: DocInfo, lost: Boolean}
 *   | {ev: "doc.changed_on_disk", doc: Number}
 *   | {ev: "lsp.status", status: Dict<Boolean>}
 *   | {ev: "workspace.added", workspace: WorkspaceEntry}
 *   | {ev: "workspace.changed", workspace: WorkspaceEntry}
 *   | {ev: "workspace.removed", workspace: Number}
 *   | {ev: "workspace.status", workspace: Number, status: WorkspaceStatus}
 *   | {ev: "orchestrator.folders", folders: String[]}
 *   | {ev: "doc.indexing", doc: Number, done: Number, total: Number}
 *   | {ev: "doc.indexed", doc: Number, lines: Number, version: Number}
 *   | {ev: "lsp.diagnostics", doc: Number, version: Number, items: Diagnostic[]}
 *   | TermEvent
 *   | {ev: "proc.exit", proc: Number, code: Number, stdout: String, stderr: String}
 *   | {ev: "fs.changed", watch: Number, path: String}
 *   | {ev: "workspace.trusted", lsp: String[]}
 */
