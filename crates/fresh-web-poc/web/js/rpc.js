// Transport to the data server: requests as JSON POSTs, pushed events over
// one Server-Sent Events stream.

export const session = { client: 0 };

export async function rpc(op, args = {}) {
  const r = await fetch("/api", {
    method: "POST",
    body: JSON.stringify({ ...args, op, client: session.client }),
  });
  const j = await r.json();
  if (j.error) throw new Error(`${op}: ${j.error}`);
  return j.ok;
}

// Synchronous request, for the few plugin APIs that are synchronous in the
// editor (readFile, fileExists, readDir). Deprecated in browsers but works.
export function rpcSync(op, args = {}) {
  const x = new XMLHttpRequest();
  x.open("POST", "/api", false);
  x.send(JSON.stringify({ ...args, op, client: session.client }));
  const j = JSON.parse(x.responseText);
  if (j.error) throw new Error(`${op}: ${j.error}`);
  return j.ok;
}

export function connectEvents(onEvent) {
  const es = new EventSource(`/events?client=${session.client}`);
  es.onmessage = (m) => onEvent(JSON.parse(m.data));
  return es;
}
