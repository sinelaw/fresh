// The typed surface of js/00-ffi.js, the one part of the web UI the inty
// checker does not read (web-ui/typecheck.sh leaves it out and loads this
// file instead). Each declaration says what the checked code may assume;
// keep the list short and each implementation a line or two.

// `target.addEventListener(type, fn, options)`. Member functions in
// types/dom.d.js take no options argument (see the note on `El` there).
/** const listen: <T>(target: T, type: String, fn: (UiEvent) => Undefined, options: Dict<Boolean>) => Undefined */
const listen;

// A value decoded from the server's JSON, given the type its use needs.
// The checker can't see which type a frame's changed path carries (the path
// is a string), so applyFrame says it; nothing checks it at run time, just
// as nothing checks what `JSON.parse` returns.
/** const jsonAs: <A, B>(A) => B */
const jsonAs;

// `window.fresh = hook`: the test surface web-ui/test/drive.mjs reads.
/** const exposeTestHook: <T>(T) => Undefined */
const exposeTestHook;

// `n.toFixed(digits)`: inty's stdlib has no methods on numbers.
/** const toFixed: (Number, Number) => String */
const toFixed;


// ---- web skins (js/15-theme.js) ----------------------------------------------
// Untrusted JSON (the served skin index, the browser's stored skins): an
// opaque value read through checked accessors. `jsonGet` of a missing key, or
// of a non-object, is `undefined` (kind "undefined"); `jsonKind` is "object",
// "array", "string", "number", "boolean", "null" or "undefined".
/** type Json = {} */
/** const jsonParse: (String) => Json */
const jsonParse;
/** const jsonKind: (Json) => String */
const jsonKind;
/** const jsonGet: (Json, String) => Json */
const jsonGet;
/** const jsonItems: (Json) => Json[] */
const jsonItems;
/** const jsonStr: (Json) => String | Null */
const jsonStr;
// The `:root{}` rule of <style id="editor-theme"> (shell.html).
/** const editorThemeDecl: () => CSSStyle */
const editorThemeDecl;
// A file `<input>`'s chosen files (`webkitRelativePath` is "" unless a folder
// was picked), and one read as a `data:` URL.
/** type File = {name: String, size: Number, type: String, webkitRelativePath: String, text: () => Promise<String>} */
/** const inputFiles: (El) => File[] */
const inputFiles;
/** const fileDataUrl: (File) => Promise<String> */
const fileDataUrl;
// HTML parsed in an inert <template> (no script runs, nothing loads): its
// top-level nodes, for the furniture sanitiser to copy from.
/** const parseInertHtml: (String) => El[] */
const parseInertHtml;
