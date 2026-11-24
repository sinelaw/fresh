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
