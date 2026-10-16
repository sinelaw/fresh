// The browser surface the web UI uses, for the inty type checker.
//
// Loaded by web-ui/typecheck.sh with `--lib`, after inty's own stdlib, and
// declares the same names (`document`, `window`, …) again: a later
// declaration shadows the stdlib's. The stdlib types every element as one
// generic row whose children, parent and event targets are left open, and
// its `style` has no properties; this file types exactly what the web UI
// touches, with one recursive element type, so a typo in a property name
// or a missing null check is an error.
//
// Never executed: `const x;` without an initializer only declares a type.

// ---- elements -------------------------------------------------------------

// The inline style properties the web UI writes or reads. Every value is a
// CSS string: write `px + "px"`, not a number.
/** type CSSStyle = {
    left: String, top: String, right: String, bottom: String,
    width: String, height: String,
    minWidth: String, minHeight: String, maxHeight: String,
    display: String, position: String, flex: String,
    justifyContent: String, gridTemplateColumns: String, boxSizing: String,
    padding: String, paddingLeft: String, paddingTop: String, paddingBottom: String,
    marginLeft: String, marginTop: String, marginBottom: String,
    borderTop: String, borderBottom: String,
    color: String, background: String,
    fontWeight: String, fontStyle: String, textDecoration: String, lineHeight: String,
    transform: String, transition: String, clipPath: String,
    animationDelay: String, pointerEvents: String,
    setProperty: (String, String) => Undefined,
    removeProperty: (String) => String,
    getPropertyValue: (String) => String
} */

/** type ClassList = {
    add: (String) => Undefined,
    remove: (String) => Undefined,
    toggle: (String, Boolean) => Boolean,
    contains: (String) => Boolean
} */

// The `data-*` attributes the web UI sets, by their dataset names. One that
// was never set reads as `undefined`.
/** type DataSet = {
    region: String?, popoverRegion: String?, name: String?, theme: String?,
    caret: String?, composing: String?, wfocus: String?,
    fg: String?, bg: String?, bold: String?, class: String?,
    index: String?, key: String?, section: String?, slot: String?,
    surface: String?, surfaceKind: String?, interior: String?, page: String?, layer: String?,
    cellLeft: String?, wkey: String?
} */

/** type DOMRect = {
    top: Number, right: Number, bottom: Number, left: Number,
    width: Number, height: Number, x: Number, y: Number
} */

// One type for every node the UI handles: HTML and SVG elements, and the
// text nodes inside them (`nodeType` 1 or 3; `data` is a text node's text). A text node has no children,
// style or class list; nothing here reads those of one.
// Member functions take a fixed number of arguments: inty fixes an optional
// parameter of a member function at its first call, so `classList.add(a, b)`
// is two calls, `append` takes text (`appendChild` an element), and a
// listener with options is added through `listen` (js/00-ffi.js).
/** type El = {
    id: String, className: String, textContent: String, innerHTML: String,
    title: String, tabIndex: Number, nodeType: Number,
    data: String,
    value: String, type: String, placeholder: String, autocomplete: String,
    readOnly: Boolean, disabled: Boolean, checked: Boolean,
    selectionStart: Number, selectionEnd: Number,
    style: CSSStyle, classList: ClassList, dataset: DataSet,
    children: El[], childNodes: El[], childElementCount: Number,
    firstChild: El | Null, lastChild: El | Null, parentElement: El | Null,
    isConnected: Boolean,
    offsetWidth: Number, offsetHeight: Number, offsetLeft: Number,
    clientWidth: Number, clientHeight: Number,
    scrollTop: Number, scrollLeft: Number, scrollWidth: Number, scrollHeight: Number,
    appendChild: (El) => El,
    append: (String) => Undefined,
    remove: () => Undefined,
    cloneNode: (Boolean) => El,
    contains: (El) => Boolean,
    closest: (String) => El | Null,
    matches: (String) => Boolean,
    querySelector: (String) => El | Null,
    querySelectorAll: (String) => El[],
    getBoundingClientRect: () => DOMRect,
    scrollIntoView: ({block: String}) => Undefined,
    focus: ({preventScroll: Boolean}) => Undefined,
    blur: () => Undefined,
    setSelectionRange: (Number, Number) => Undefined,
    setAttribute: (String, String) => Undefined,
    getAttribute: (String) => String | Null,
    removeAttribute: (String) => Undefined,
    addEventListener: (String, (UiEvent) => Undefined) => Undefined,
    removeEventListener: (String, (UiEvent) => Undefined) => Undefined,
    onclick: (UiEvent) => Undefined,
    onmousedown: (UiEvent) => Undefined,
    onmousemove: (UiEvent) => Undefined,
    onmouseenter: (UiEvent) => Undefined,
    getContext: (String) => CanvasContext
} */

/** type CanvasContext = {
    font: String,
    measureText: (String) => {width: Number}
} */

// ---- events ----------------------------------------------------------------

/** type Touch = {identifier: Number, clientX: Number, clientY: Number} */
/** type DataTransfer = {
    getData: (String) => String,
    setData: (String, String) => Undefined
} */

// Every listener gets this one shape: the fields of the mouse, wheel,
// keyboard, input, composition, touch and clipboard events together. Read
// only the fields the event you listen for has.
/** type UiEvent = {
    type: String, target: El, currentTarget: El, timeStamp: Number,
    preventDefault: () => Undefined, stopPropagation: () => Undefined,
    clientX: Number, clientY: Number, button: Number, buttons: Number, detail: Number,
    deltaX: Number, deltaY: Number,
    key: String, code: String, repeat: Boolean, isComposing: Boolean,
    shiftKey: Boolean, ctrlKey: Boolean, metaKey: Boolean, altKey: Boolean,
    data: String | Null, inputType: String,
    touches: Touch[], changedTouches: Touch[],
    clipboardData: DataTransfer | Null
} */

// ---- document and window ---------------------------------------------------

/** type Range = {
    commonAncestorContainer: El, startContainer: El, endContainer: El,
    startOffset: Number, endOffset: Number,
    intersectsNode: (El) => Boolean
} */
/** type Selection = {
    isCollapsed: Boolean, rangeCount: Number,
    getRangeAt: (Number) => Range,
    removeAllRanges: () => Undefined,
    toString: () => String
} */

/** type MediaQueryList = {
    matches: Boolean,
    addEventListener: (String, (UiEvent) => Undefined) => Undefined
} */

/** const document: {
    body: El, documentElement: El, activeElement: El | Null,
    getElementById: (String) => El | Null,
    querySelector: (String) => El | Null,
    createElement: (String) => El,
    createTextNode: (String) => El,
    addEventListener: (String, (UiEvent) => Undefined) => Undefined,
    removeEventListener: (String, (UiEvent) => Undefined) => Undefined
} */
const document;

/** const window: {
    innerWidth: Number, innerHeight: Number,
    addEventListener: (String, (UiEvent) => Undefined) => Undefined,
    removeEventListener: (String, (UiEvent) => Undefined) => Undefined,
    getSelection: () => Selection | Null,
    matchMedia: (String) => MediaQueryList
} */
const window;

/** const matchMedia: (String) => MediaQueryList */
const matchMedia;
/** const getComputedStyle: (El) => {getPropertyValue: (String) => String} */
const getComputedStyle;
/** const devicePixelRatio: Number */
const devicePixelRatio;
/** const requestAnimationFrame: ((Number) => Undefined) => Number */
const requestAnimationFrame;
/** const setTimeout: (() => Undefined, Number) => Number */
const setTimeout;
/** const clearTimeout: (Number) => Undefined */
const clearTimeout;

// `clipboard` is missing outside a secure context (plain http on a LAN).
/** const navigator: {
    clipboard: {writeText: (String) => Promise<Undefined>} | Undefined
} */
const navigator;

// `Intl.Segmenter`, for grapheme clusters. `segment` returns an iterable of
// `{segment}` records, not an array: read it with `Array.from(it, fn)`.
/** type Segments = {} */
/** type Segmenter = {segment: (String) => Segments} */
/** const Intl: {Segmenter: (Undefined, {granularity: String}) => Segmenter} */
const Intl;

// `new Set(values)`: the stdlib declares only `new Set()`.
/** const Set: <T>(values?: T[]) => Set<T> */
const Set;

// ---- network ---------------------------------------------------------------

// The server only sends text frames, so a message's `data` is a string.
/** type Socket = {
    readyState: Number,
    send: (String) => Undefined,
    onmessage: ({data: String}) => Undefined,
    onclose: () => Undefined
} */
/** const WebSocket: (String) => Socket */
const WebSocket;
