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

// A row a palette mode lists (`fresh.paletteMode`): `run` when it is
// chosen, `preview` when it is selected; a row with neither is a note.
/** type PaletteItem = {
 *   label: String,
 *   detail: String | Null,
 *   run: (() => Undefined) | Null,
 *   preview: (() => Undefined) | Null,
 * } */

// What the server pushes over the event stream.
/** type ServerEvent =
 *   | {ev: "doc.ops", doc: Number, version: Number, client: Number, ops: Op[], len: Number, modified: Boolean}
 *   | {ev: "doc.saved", doc: Number, info: DocInfo}
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
