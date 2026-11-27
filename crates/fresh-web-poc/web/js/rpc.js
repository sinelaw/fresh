// Transport to the data server: requests as JSON POSTs, pushed events over
// one Server-Sent Events stream.

// This page's client id: none until `hello` gives it one.
/** const session: {client: Number | Null} */
export const session = { client: null };
// The client id, once `hello` gave it (every request but `hello` is made
// after).
/** function clientId() => Number */
export function clientId() {
  const c = session.client;
  if (c === null) throw new Error("no client id yet: hello comes first");
  return c;
}

// A refused operation: `code` is the server's stable reason (see the
// README's protocol section), `detail` its message for people, `message`
// both with the operation, as an Error's message reads.
export class RpcError {
  constructor(op, error) {
    this.op = op;
    this.code = error.code;
    this.detail = error.message;
    this.message = `${op}: ${error.message}`;
  }
  // Its text: the server's own words (`message` says the operation too).
  toString() { return this.detail; }
}

// What to say of a failure: a refusal's own words, an error's message,
// or the value thrown.
export function failure(e) { return String(e); }

// Operations that change documents or files. A client's are sent one at a
// time, each once the one before it is answered, so the server applies them
// in the order the client made them (a close and the reopen after it, an
// edit and the save after it). Reads, searches and language-server requests
// go at once.
const ORDERED = new Set([
  "doc.new", "doc.open", "doc.close", "doc.edit", "doc.save", "doc.save_as", "doc.reload", "doc.set",
  "fs.write", "fs.create", "fs.rename", "fs.delete", "fs.copy", "workspace.trust",
]);
// Settles once the last ordered operation is answered (or refused).
let ordered = Promise.resolve(undefined);

// Operation `op`: its answer, or (thrown) an RpcError.
export function rpc(op, args) {
  return attempt(op, args).then((o) => {
    if (o.kind === "refused") throw new RpcError(op, { code: o.code, message: o.detail });
    return o.value;
  });
}

// Operation `op`, a refusal being an answer too: `{kind: "ok", value}`,
// or `{kind: "refused", code, detail}` with the server's reason (for the
// callers that act on it).
export function attempt(op, args) {
  if (!ORDERED.has(op)) return send(op, args);
  const p = ordered.then(() => send(op, args));
  ordered = p.then(() => undefined).catch(() => undefined);
  return p;
}
/** function answered<T>(value: T) => Outcome<T> */
function answered(value) { return { kind: "ok", value }; }
/** function refused<T>(code: String, detail: String) => Outcome<T> */
function refused(code, detail) { return { kind: "refused", code, detail }; }

// The answer is `{ok: result}` or `{error: {code, message}}`.
async function send(op, args) {
  const r = await fetch("/api", {
    method: "POST",
    body: JSON.stringify({ ...args, op, client: session.client }),
  });
  const j = await r.json();
  const e = j.error;
  return e ? refused(e.code, e.message) : answered(j.ok);
}

/** function connectEvents(onEvent: (ServerEvent) => Undefined) => EventSource */
export function connectEvents(onEvent) {
  const es = new EventSource(`/events?client=${clientId()}`);
  es.onmessage = (m) => { onEvent(JSON.parse(m.data)); };
  return es;
}
