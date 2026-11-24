// Transport to the data server: requests as JSON POSTs, pushed events over
// one Server-Sent Events stream.

export const session = { client: 0 };

// A refused operation: `code` is the server's stable reason (see the
// README's protocol section), `detail` its message for people.
export class RpcError extends Error {
  constructor(op, error) {
    super(`${op}: ${error.message}`);
    this.op = op;
    this.code = error.code;
    this.detail = error.message;
  }
}

// Operations that change documents or files. A client's are sent one at a
// time, each once the one before it is answered, so the server applies them
// in the order the client made them (a close and the reopen after it, an
// edit and the save after it). Reads, searches and language-server requests
// go at once.
const ORDERED = new Set([
  "doc.open", "doc.close", "doc.edit", "doc.save", "doc.reload",
  "fs.write", "fs.create", "fs.rename", "fs.delete", "fs.copy", "workspace.trust",
]);
let ordered = Promise.resolve();

export function rpc(op, args = {}) {
  if (!ORDERED.has(op)) return send(op, args);
  const p = ordered.then(() => send(op, args));
  ordered = p.catch(() => {});
  return p;
}

// The answer is `{ok: result}` or `{error: {code, message}}`.
async function send(op, args) {
  const r = await fetch("/api", {
    method: "POST",
    body: JSON.stringify({ ...args, op, client: session.client }),
  });
  const j = await r.json();
  if (j.error) throw new RpcError(op, j.error);
  return j.ok;
}

/** function connectEvents(onEvent: (ServerEvent) => Undefined) => EventSource */
export function connectEvents(onEvent) {
  const es = new EventSource(`/events?client=${session.client}`);
  es.onmessage = (m) => onEvent(JSON.parse(m.data));
  return es;
}
