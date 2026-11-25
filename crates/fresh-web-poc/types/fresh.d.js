// Types shared by the web client's modules, for the inty checker
// (`../typecheck.sh`). Declarations only: nothing here runs.

// An edit primitive over byte offsets (ot.js); an edit is a sequence of them.
/** type Op = {t: "ins", pos: Number, text: String} | {t: "del", pos: Number, len: Number} */

// A colour as plugins and themes give it: a theme key ("syntax.keyword"), a
// CSS colour, a colour name, "Indexed:N", or [r, g, b].
/** type Color = String | Number[] */

// A syntax span of a line (highlight.js), in UTF-16 indices; `k` is a theme
// key.
/** type Highlight = {s: Number, e: Number, k: String, bold: Boolean, italic: Boolean} */

// A byte range [start, end).
/** type Range = {start: Number, end: Number} */

// A buffer's held lines (a BufferHandle's lines()): their text, their byte
// starts, and the document line of the first (null without a line index).
/** type Lines = {lines: String[], starts: Number[], firstLine: Number | Null} */

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
/** type Overlay = {ns: String, start: Number, end: Number, style: Style, when: String, mark: Boolean, above: Boolean} */
/** type Conceal = {ns: String, start: Number, end: Number, text: String, style: Style, reveal: Range | Null, when: String} */
/** type InlineText = {ns: String, pos: Number, text: String, style: Style, side: String, edge: Boolean, when: String} */
/** type VirtualLine = {ns: String, pos: Number, text: String, style: Style, above: Boolean, order: Number, when: String} */
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
/** type GutterMark = {ns: String, start: Number, end: Number, glyph: String, color: Color, title: String} */

// ---------------------------------------------------------------- navigation

// Where a file opens (`fresh.buffers.open`): at `line` (1-based; null: where
// its view is) and `column` (UTF-16, 1-based), in `place` ("tab", "split"
// or "right"), as the split's preview tab (`preview` true), in a tab of its
// own (false) or as it is shown already (null); `focus: false` leaves the
// keyboard where it is (a list previewing what it selects).
/** type OpenOptions = {line: Number | Null, column: Number, place: String, preview: Boolean | Null, focus: Boolean} */

// A view's cursor: its position, its selection's anchor (null: no
// selection) and the column vertical motion aims for (null: its own).
/** type Cursor = {pos: Number, anchor: Number | Null, goalX: Number | Null} */

// The layout a workspace's page remembers (session.js): each split's
// tabs (a file by its path, an untitled document by its number) with
// their cursor and scroll and the one shown, the splits' directions and
// ratios, the active split, and whether the sidebar and terminal show.
/** type SavedTab = {path: String | Null, untitled: Number | Null, pos: Number, top: Number, preview: Boolean} */
/** type SavedNode =
 *   | {kind: "pane", tabs: SavedTab[], active: String | Null}
 *   | {kind: "split", dir: String, ratio: Number, a: SavedNode, b: SavedNode} */
/** type SavedLayout = {tree: SavedNode, active: Number, sidebar: Boolean, terminal: Boolean} */
/** type SessionData = {layout: SavedLayout | Null, recent: String[], sizes: Dict<Number>} */

// A cursor moved (`setCursor`, `select`): `center` scrolls it to the middle.
/** type CursorOptions = {center: Boolean} */

// ---------------------------------------------------------------- panels

// A panel (`fresh.ui.panel`) in `place`: "bottom" (the dock), "sidebar",
// "left" (a column before the sidebar), "right", "bar" (above the editor)
// or "dialog"; `onClose` runs when it is closed.
/** type PanelOptions = {id: String, title: String, place: String, onClose: (() => Undefined) | Null} */

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
 *   lines: () => Lines | Null,
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
 *   path: String,
 *   len: Number,
 *   version: Number,
 *   lines: Number | Null,
 *   large: Boolean,
 *   modified: Boolean,
 *   language: String | Null,
 *   encoding: String,
 *   readonly: Boolean,
 * } */

// A language server's diagnostic, at byte offsets.
/** type Diagnostic = {start: Number, end: Number, severity: Number, message: String, source: String | Null} */

// ---------------------------------------------------------------- language servers

// What `fresh.lspRequest` answers, one shape per method (src/lsp_shapes.rs).
// Positions are the protocol's: lines and UTF-16 columns.
/** type LspPos = {line: Number, character: Number} */
/** type LspRange = {start: LspPos, end: LspPos} */
// definition, declaration, typeDefinition, implementation, references:
/** type LspLocation = {path: String, range: LspRange} */
// documentSymbol (the range is the name's):
/** type LspSymbol = {name: String, kind: Number, container: String | Null, range: LspRange, children: LspSymbol[]} */
// workspace/symbol:
/** type LspWorkspaceSymbol = {name: String, kind: Number, container: String | Null, path: String, range: LspRange} */
// formatting, and each file's edits in a rename or a code action:
/** type LspTextEdit = {range: LspRange, newText: String} */
/** type LspFileEdit = {path: String, edits: LspTextEdit[]} */
// A command, its arguments as JSON text: the server's own, sent back as
// they came (workspace/executeCommand).
/** type LspCommand = {command: String, arguments: String} */
// codeAction:
/** type LspAction = {title: String, kind: String | Null, edit: LspFileEdit[] | Null, command: LspCommand | Null} */
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
/** type PaletteItem = {
 *   label: String,
 *   detail: String | Null,
 *   run: (() => Undefined) | Null,
 *   preview: (() => Undefined) | Null,
 * } */

// A workspace as the orchestrator lists it (src/session.rs: entry,
// status; src/terminal.rs: a terminal).
/** type TermInfo = {id: Number, name: String, command: String | Null, agent: Boolean, resume: String | Null, title: String, state: String, bells: Number} */
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

// What the server pushes over the event stream.
/** type ServerEvent =
 *   | {ev: "doc.ops", doc: Number, version: Number, client: Number, ops: Op[], len: Number, modified: Boolean}
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
 *   | {ev: "term.data", term: Number, at: Number, data: String}
 *   | {ev: "term.exit", term: Number}
 *   | {ev: "term.size", term: Number, cols: Number, rows: Number}
 *   | {ev: "proc.exit", proc: Number, code: Number, stdout: String, stderr: String}
 *   | {ev: "fs.changed", watch: Number, path: String}
 *   | {ev: "workspace.trusted", lsp: String[]}
 */
