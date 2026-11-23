// The core's API (docs/internal/web-ui.md, crates/fresh-editor/src/webui/api.rs).
// One WebSocket: requests answered by replies, events pushed on change.
// (web-ui/js is concatenated in filename order into one <script>; all files
// share one scope.)
"use strict";

const api = (() => {
  let ws = null, nextId = 0, open = false;
  const pending = new Map();
  const handlers = new Map();
  const queue = [];

  function connect() {
    const proto = location.protocol === "https:" ? "wss:" : "ws:";
    ws = new WebSocket(`${proto}//${location.host}/api`);
    ws.onopen = () => {
      open = true;
      document.body.classList.remove("disconnected");
      for (const m of queue.splice(0)) ws.send(m);
      emit("open", {});
    };
    ws.onmessage = (m) => {
      const v = JSON.parse(m.data);
      if (v.re !== undefined) {
        const p = pending.get(v.re);
        pending.delete(v.re);
        if (!p) return;
        if (v.error !== undefined) p.reject(new Error(v.error)); else p.resolve(v);
        return;
      }
      if (v.ev) emit(v.ev, v);
    };
    ws.onclose = () => {
      open = false;
      document.body.classList.add("disconnected");
      for (const p of pending.values()) p.reject(new Error("disconnected"));
      pending.clear();
      emit("close", {});
      setTimeout(connect, 1000);
    };
  }

  function emit(ev, v) {
    for (const f of handlers.get(ev) || []) {
      try { f(v); } catch (e) { console.error(`[fresh] ${ev} handler`, e); }
    }
  }

  // The largest message the server reads (its `build` event says); a
  // larger one would cost the connection, so it is refused here instead.
  let maxMessage = 16 << 20;
  function raw(obj) {
    const s = JSON.stringify(obj);
    // UTF-8 is at most 3 bytes per UTF-16 unit: count only when it could matter.
    if (s.length * 3 > maxMessage && new TextEncoder().encode(s).length > maxMessage) {
      throw new Error(`too large to send (over ${Math.floor(maxMessage / 1048576)} MiB)`);
    }
    if (open) ws.send(s); else queue.push(s);
  }

  return {
    connect,
    /** Send a command; resolves with its reply. */
    call(op, args = {}) {
      const id = ++nextId;
      return new Promise((resolve, reject) => {
        pending.set(id, { resolve, reject });
        try { raw({ id, op, ...args }); } catch (e) { pending.delete(id); reject(e); }
      });
    },
    /** Send a command whose reply nobody waits for. */
    send(op, args = {}) {
      try { raw({ op, ...args }); } catch (e) { showError(e); }
    },
    get maxMessage() { return maxMessage; },
    set maxMessage(n) { if (n > 0) maxMessage = n; },
    on(ev, f) {
      if (!handlers.has(ev)) handlers.set(ev, []);
      handlers.get(ev).push(f);
    },
  };
})();

// What the core has told us, by topic. Each topic's handler re-renders the
// part of the UI it feeds.
const core = {
  root: "",
  layout: null,
  buffers: [],
  status: {},
  menus: [],
  ask: null,
  popups: [],
  panels: [],
  settings: null,
  theme: {},
};

for (const t of ["layout", "buffers", "status", "menus", "ask", "popups", "panels", "settings", "theme"]) {
  api.on(t, (v) => { core[t] = v[t]; });
}
// The client this page carries is the server's own build (build.rs stamps
// both). A page left open while the editor was upgraded reconnects to a
// server it was not built for, so it reloads into the one that server serves.
const PAGE_BUILD = (document.querySelector('meta[name="fresh-build"]') || {}).content || "";
api.on("build", (v) => {
  if (PAGE_BUILD && v.build !== PAGE_BUILD) location.reload();
  api.maxMessage = v.maxMessage;
});

// The project root: the active workspace's, so it changes when the
// Orchestrator switches workspaces.
api.on("root", (v) => { core.root = v.root; });

// ---- small DOM helpers ----------------------------------------------------
function el(tag, cls, text) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text !== undefined && text !== null) e.textContent = text;
  return e;
}
function div(cls) { return el("div", cls); }
function esc(s) {
  return String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
}
function bufferById(id) { return core.buffers.find((b) => b.id === id); }
function baseName(p) { return String(p || "").split(/[\\/]/).filter(Boolean).pop() || p; }
function joinPath(dir, name) { return dir.endsWith("/") ? dir + name : dir + "/" + name; }
