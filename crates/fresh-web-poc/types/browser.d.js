// Browser APIs the web client uses that inty's built-in DOM declarations
// leave out, and the vendored xterm.js. Declarations only: nothing here runs.

// Server-Sent Events.
/** type EventSource = {
 *   onmessage: ({data: String}) => Undefined,
 *   onerror: ({type: String}) => Undefined,
 *   close: () => Undefined,
 * } */
/** const EventSource: new (url: String) => EventSource */
const EventSource;

// The window's size in CSS pixels.
/** const innerWidth: Number */
const innerWidth;
/** const innerHeight: Number */
const innerHeight;

// The vendored xterm.js (vendor/xterm.js, its fit addon): what the
// terminal panel uses of it. `E` is the element it opens in.
/** type XtermTheme = {
 *   background: String,
 *   foreground: String,
 *   cursor: String,
 *   selectionBackground: String,
 * } */
/** type XtermOptions = {
 *   fontFamily: String,
 *   fontSize: Number,
 *   cursorBlink: Boolean,
 *   allowProposedApi: Boolean,
 *   scrollback: Number,
 *   theme: XtermTheme,
 *   minimumContrastRatio: Number,
 * } */
/** type XtermFit = {fit: () => Undefined} */
/** type Xterm<E, K> = {
 *   options: XtermOptions,
 *   open: (E) => Undefined,
 *   write: (String | Uint8Array) => Undefined,
 *   focus: () => Undefined,
 *   dispose: () => Undefined,
 *   loadAddon: (XtermFit) => Undefined,
 *   onData: ((String) => Undefined) => Undefined,
 *   onBinary: ((String) => Undefined) => Undefined,
 *   onResize: (({cols: Number, rows: Number}) => Undefined) => Undefined,
 *   onTitleChange: ((String) => Undefined) => Undefined,
 *   attachCustomKeyEventHandler: ((K) => Boolean) => Undefined,
 * } */
/** const Terminal: <E, K>new (options: XtermOptions) => Xterm<E, K> */
const Terminal;
/** const FitAddon: {FitAddon: new () => XtermFit} */
const FitAddon;

// A message channel (hl.js runs its slices as separate tasks through one).
/** type MessagePort = {
 *   onmessage: ({data: Null}) => Undefined,
 *   postMessage: (Null) => Undefined,
 * } */
/** type MessageChannel = {port1: MessagePort, port2: MessagePort} */
/** const MessageChannel: new () => MessageChannel */
const MessageChannel;
