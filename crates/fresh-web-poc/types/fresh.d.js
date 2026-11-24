// Types shared by the web client's modules, for the inty checker
// (`../typecheck.sh`). Declarations only: nothing here runs.

// An edit primitive over byte offsets (ot.js); an edit is a sequence of them.
/** type Op = {t: "ins", pos: Number, text: String} | {t: "del", pos: Number, len: Number} */

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
