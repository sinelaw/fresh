// The plugin runtime, in the client.
//
// Each of the editor's bundled plugins (compiled from its TypeScript by the
// editor's own compiler) runs here unchanged, in a scope of its own, against
// an `editor` object implementing the plugin API over this client's state:
// its buffers, views, decorations and UI. What only the server can do —
// files, processes, watches — goes to the server. An API this proof of
// concept does not implement answers like a no-op and is logged once.

import { rpc, rpcSync } from "./rpc.js";
import { byteLen } from "./ot.js";
import { stringWidth, charWidth } from "./layout.js";

const warned = new Set();

export class Plugins {
  constructor(app) {
    this.app = app;
    this.hooks = new Map(); // event -> [{plugin, h}]
    this.commands = new Map(); // name -> {plugin, handler, description, source}
    this.modes = new Map(); // name -> {bindings: Map(key -> handler), plugin, readOnly}
    this.loaded = [];
    this.statusItems = new Map(); // token -> {title, values: Map(buffer -> value)}
    this.procs = new Map(); // proc id -> resolve
    this.watches = new Map(); // watch id -> plugin
    this.log = [];
  }

  async loadAll() {
    const list = await (await fetch("/plugins")).json();
    for (const p of list) {
      if (p.error) { this.note(`plugin ${p.name} failed to compile: ${p.error}`); continue; }
      this.load(p);
    }
  }

  note(msg) { this.log.push(msg); console.log(`[plugins] ${msg}`); }

  load({ name, code, i18n }) {
    const plugin = { name, handlers: new Map(), i18n: i18n || {} };
    const api = this.makeApi(plugin);
    const editor = new Proxy(api, {
      get: (t, k) => {
        if (k in t) return t[k];
        if (typeof k === "symbol" || k === "then") return undefined;
        return (...args) => {
          const key = `${name}:${String(k)}`;
          if (!warned.has(key)) { warned.add(key); this.note(`${name}: editor.${String(k)} is not implemented in the proof of concept`); }
          return undefined;
        };
      },
    });
    plugin.editor = editor;
    // The plugin's globals: handlers registered by name land here.
    const scope = new Proxy({}, {
      get: (t, k) => (k in t ? t[k] : globalThis[k]),
      set: (t, k, v) => { t[k] = v; if (typeof v === "function") plugin.handlers.set(k, v); return true; },
      has: () => true,
    });
    plugin.scope = scope;
    const registerHandler = (n, fn) => { plugin.handlers.set(n, fn); };
    try {
      // The plugin's code runs in a block of its own, so its own top-level
      // `const editor = getEditor()` shadows the one provided, as it shadows
      // the global in the editor's runtime.
      const fn = new Function("__editor", "getEditor", "registerHandler", "globalThis", `const editor = __editor;\n{\n${code}\n}`);
      fn(editor, () => editor, registerHandler, scope);
      this.loaded.push(plugin);
      this.note(`loaded ${name}`);
    } catch (e) {
      this.note(`plugin ${name} failed to load: ${e.stack || e}`);
    }
  }

  handler(plugin, h) {
    if (typeof h === "function") return h;
    return plugin.handlers.get(h) || null;
  }

  call(plugin, h, ...args) {
    const fn = this.handler(plugin, h);
    if (!fn) { this.note(`${plugin.name}: no handler ${h}`); return; }
    try {
      const r = fn(...args);
      if (r && typeof r.catch === "function") r.catch((e) => this.note(`${plugin.name}: ${h}: ${e.stack || e}`));
      return r;
    } catch (e) {
      this.note(`${plugin.name}: ${typeof h === "string" ? h : "handler"}: ${e.stack || e}`);
    }
  }

  fire(event, data) {
    for (const { plugin, h } of this.hooks.get(event) || []) this.call(plugin, h, data);
  }

  runCommand(name) {
    const c = this.commands.get(name);
    if (c) this.call(c.plugin, c.handler);
  }

  t(plugin, key, args) {
    const tbl = plugin.i18n?.en || {};
    let s = tbl[key] ?? key;
    if (args) for (const [k, v] of Object.entries(args)) s = s.split(`%{${k}}`).join(String(v));
    return s;
  }
  resolveName(plugin, s) {
    return typeof s === "string" && s.startsWith("%") ? this.t(plugin, s.slice(1)) : s;
  }

  processHandle(program, args, cwd) {
    let resolve;
    const result = new Promise((r) => (resolve = r));
    const h = {
      processId: 0,
      result,
      then: (a, b) => result.then(a, b),
      kill: async () => (h.processId ? (await rpc("proc.kill", { proc: h.processId })).killed : false),
    };
    rpc("proc.run", { program, args: args || [], cwd: cwd || undefined }).then((r) => {
      h.processId = r.proc;
      this.procs.set(r.proc, resolve);
    }, (e) => resolve({ code: -1, stdout: "", stderr: String(e) }));
    return h;
  }

  onEvent(ev) {
    if (ev.ev === "proc.exit") {
      const done = this.procs.get(ev.proc);
      if (done) { this.procs.delete(ev.proc); done({ code: ev.code, stdout: ev.stdout, stderr: ev.stderr, exit_code: ev.code }); }
      else setTimeout(() => this.onEvent(ev), 20);
    } else if (ev.ev === "fs.changed") {
      this.fire("path_changed", { handle: ev.watch, path: ev.path });
    }
  }

  // ------------------------------------------------------------------ API

  makeApi(plugin) {
    const app = this.app;
    const P = this;
    const buf = (id) => app.buffers.get(id);
    const view = () => app.activePane?.view || null;
    const viewOf = (id) => {
      const v = view();
      if (v && v.buf.id === id) return v;
      for (const p of app.panes()) for (const x of p.views) if (x.buf.id === id) return x;
      return null;
    };
    const touch = (id) => app.renderBuffer(id);
    const style = (o) => o || {};
    const api = {
      // ---- identity and logging
      t: (k, a) => P.t(plugin, k, a),
      debug: (m) => console.debug(`[${plugin.name}] ${m}`),
      info: (m) => console.info(`[${plugin.name}] ${m}`),
      warn: (m) => console.warn(`[${plugin.name}] ${m}`),
      error: (m) => { console.error(`[${plugin.name}] ${m}`); app.setStatus(m); },
      setStatus: (m) => app.setStatus(m),
      stringWidth: (s) => stringWidth(s),
      utf8ByteLength: (s) => byteLen(s),
      setScrollbarMarkers: () => true,
      setScrollbarMarkersInRange: () => true,
      clearScrollbarMarkers: () => true,
      charWidth: (c) => charWidth(c),
      delay: (ms) => new Promise((r) => setTimeout(r, ms)),
      flush: () => Promise.resolve(),
      getConfig: () => app.config,
      getPluginConfig: () => null,
      defineConfigMap: (_n, o) => o?.default || {},
      definePluginConfig: () => ({}),
      getGlobalState: (k) => { try { const v = localStorage.getItem(`plugin:${plugin.name}:${k}`); return v == null ? undefined : JSON.parse(v); } catch { return undefined; } },
      setGlobalState: (k, v) => { try { if (v == null) localStorage.removeItem(`plugin:${plugin.name}:${k}`); else localStorage.setItem(`plugin:${plugin.name}:${k}`, JSON.stringify(v)); } catch {} return true; },
      getCwd: () => app.root,
      getDataDir: () => "/tmp/fresh-poc-data",
      getTempDir: () => "/tmp",
      getScreenSize: () => ({ width: Math.floor(innerWidth / app.metrics.cw), height: Math.floor(innerHeight / app.metrics.lh) }),

      // ---- paths and files (the server's)
      pathJoin: (...parts) => parts.filter((p) => p !== "").join("/").replace(/\/+/g, "/"),
      pathDirname: (p) => p.replace(/\/[^/]*$/, "") || "/",
      pathBasename: (p) => p.split("/").pop(),
      pathExtname: (p) => { const m = /\.[^./]*$/.exec(p); return m ? m[0] : ""; },
      pathIsAbsolute: (p) => p.startsWith("/"),
      localPath: (p) => p,
      authorityPath: (p) => p,
      readFile: (p) => { try { return rpcSync("fs.read", { path: p }).text; } catch { return null; } },
      writeFile: (p, text) => { try { rpcSync("fs.write", { path: p, text }); return true; } catch { return false; } },
      fileExists: (p) => { try { return rpcSync("fs.stat", { path: p }).exists; } catch { return false; } },
      readDir: (p) => { try { return rpcSync("fs.list", { path: p }).entries.map((e) => ({ name: e.name, is_dir: e.dir, is_file: !e.dir })); } catch { return []; } },
      createDir: () => true,
      watchPath: async (p) => { const { watch } = await rpc("fs.watch", { path: p }); P.watches.set(watch, plugin); return watch; },
      unwatchPath: (id) => { rpc("fs.unwatch", { watch: id }); return true; },
      runOnTarget: (program, args, cwd) => P.processHandle(program, args, cwd),
      spawnProcess: (program, args, cwd) => P.processHandle(program, args, cwd),
      spawnHostProcess: (program, args, cwd) => P.processHandle(program, args, cwd),

      // ---- commands, hooks, modes, actions
      registerCommand: (name, description, handler, context, options) => {
        P.commands.set(P.resolveName(plugin, name), { plugin, handler, description: P.resolveName(plugin, description), source: plugin.name });
        return true;
      },
      unregisterCommand: (name) => P.commands.delete(name),
      on: (event, h) => {
        if (!P.hooks.has(event)) P.hooks.set(event, []);
        P.hooks.get(event).push({ plugin, h });
        return true;
      },
      off: (event, h) => {
        const l = P.hooks.get(event) || [];
        P.hooks.set(event, l.filter((x) => !(x.plugin === plugin && x.h === h)));
        return true;
      },
      defineMode: (name, bindings, readOnly) => {
        const m = new Map();
        for (const [k, h] of bindings || []) m.set(k, h);
        P.modes.set(name, { plugin, bindings: m, readOnly: !!readOnly });
        return true;
      },
      executeAction: (name) => app.runAction(name),
      executeActions: (list) => { for (const a of list || []) for (let i = 0; i < (a.count || 1); i++) app.runAction(a.action); return true; },

      // ---- buffers and views
      getActiveBufferId: () => view()?.buf.id ?? 0,
      getActiveSplitId: () => app.activePane?.id ?? 0,
      listSplits: () => app.panes().map((p) => ({ id: p.id, buffer_id: p.view?.buf.id ?? 0, active: p === app.activePane })),
      listBuffers: () => [...app.buffers.values()].map((b) => app.bufferInfo(b)),
      getBufferInfo: (id) => { const b = buf(id); return b ? app.bufferInfo(b) : null; },
      getBufferPath: (id) => buf(id)?.path || "",
      getBufferLength: (id) => buf(id)?.len ?? 0,
      isBufferModified: (id) => !!buf(id)?.modified,
      getBufferText: (id, s = 0, e) => { const b = buf(id); return b ? b.textAsync(s, e ?? b.len) : Promise.resolve(""); },
      getCursorPosition: () => view()?.cursor ?? 0,
      getPrimaryCursor: () => {
        const v = view();
        if (!v) return null;
        const L = v.buf.text;
        const li = L ? L.indexOf(v.cursor) : -1;
        return { position: v.cursor, selection: v.selection(), line: L && L.firstLine != null && li >= 0 ? L.firstLine + li : null };
      },
      getAllCursors: () => { const v = view(); return v ? [{ position: v.cursor, selection: v.selection() }, ...v.extraCursors.map((c) => ({ position: c, selection: null }))] : []; },
      getCursorLine: () => { const v = view(); const L = v?.buf.text; return L ? (L.firstLine ?? 0) + L.indexOf(v.cursor) : 0; },
      getViewport: () => {
        const p = app.activePane;
        const v = p?.view;
        if (!v) return null;
        const g = p.geometry(v);
        const L = v.buf.text;
        return { topByte: v.top, topLine: L && L.firstLine != null ? L.firstLine + L.indexOf(v.top) : null, leftColumn: 0, width: g.cols, height: g.rows };
      },
      getTextPropertiesAtCursor: (id) => { const b = buf(id); const v = viewOf(id); return b && b.propsAt && v ? b.propsAt(v.cursor) : []; },
      insertText: (id, pos, text) => { const b = buf(id); if (!b) return false; app.applyEdit(b, [{ t: "ins", pos, text }]); return true; },
      deleteRange: (id, s, e) => { const b = buf(id); if (!b || e <= s) return false; app.applyEdit(b, [{ t: "del", pos: s, len: e - s }]); return true; },
      insertAtCursor: (text) => { const v = view(); if (!v) return false; app.applyEdit(v.buf, [{ t: "ins", pos: v.cursor, text }]); return true; },
      openFile: (path, line, column) => { app.openFile(path, { line, column }); return true; },
      openFileInSplit: (_s, path, line, column) => { app.openFile(path, { line, column }); return true; },
      closeBuffer: (id) => { app.closeBuffer(id); return true; },
      setViewMode: (id, mode) => { const v = viewOf(id); if (v) { v.compose = mode === "compose"; touch(id); } return true; },
      setLayoutHints: (id, _split, hints) => { const v = viewOf(id); if (v) { v.composeWidth = hints?.composeWidth ?? null; touch(id); } return true; },
      setLineNumbersDefault: (id, on) => { const v = viewOf(id); if (v) { v.lineNumbers = on; touch(id); } return true; },
      setLineNumbers: (id, on) => { const v = viewOf(id); if (v) { v.lineNumbers = on; touch(id); } return true; },
      setLineWrap: (id, _s, on) => { const v = viewOf(id); if (v) { v.wrap = on; touch(id); } return true; },
      setFoldIndicators: () => true,
      setIndentationGuide: () => true,
      refreshLines: (id) => { const b = buf(id); if (b) { b.offered = new Set(); touch(id); } return true; },

      // ---- decorations
      addOverlay: (id, ns, s, e, o) => { const b = buf(id); if (!b) return false; b.decor.addOverlay(ns, s, e, style(o)); touch(id); return true; },
      clearNamespace: (id, ns) => { buf(id)?.decor.clearNamespace(ns); touch(id); return true; },
      clearAllOverlays: (id) => { const b = buf(id); if (b) b.decor.overlays = []; touch(id); return true; },
      clearOverlaysInRange: (id, s, e) => { buf(id)?.decor.clearOverlaysInRange(s, e); touch(id); return true; },
      clearOverlaysInRangeForNamespace: (id, ns, s, e) => { buf(id)?.decor.clearOverlaysInRange(s, e, ns); touch(id); return true; },
      removeOverlay: (id, h) => { buf(id)?.decor.removeOverlay(h); touch(id); return true; },
      addConceal: (id, ns, start, end, replacement, activation, scopeStart, scopeEnd, composeOnly, replacementOverlays) => {
        const b = buf(id);
        if (!b) return false;
        b.decor.addConceal({ ns, start, end, replacement, activation: activation || null, scopeStart: scopeStart ?? null, scopeEnd: scopeEnd ?? null, composeOnly: !!composeOnly, replacementOverlays: replacementOverlays || null });
        touch(id);
        return true;
      },
      clearConcealNamespace: (id, ns) => { buf(id)?.decor.clearConcealNamespace(ns); touch(id); return true; },
      clearConcealsInRange: (id, s, e) => { buf(id)?.decor.clearConcealsInRange(s, e); touch(id); return true; },
      addSoftBreak: (id, ns, pos, indent, activation, scopeStart, scopeEnd, prefix) => {
        buf(id)?.decor.addSoftBreak({ ns, pos, indent, activation: activation || null, scopeStart: scopeStart ?? null, scopeEnd: scopeEnd ?? null, prefix: prefix || null });
        touch(id);
        return true;
      },
      clearSoftBreakNamespace: (id, ns) => { buf(id)?.decor.clearSoftBreakNamespace(ns); touch(id); return true; },
      clearSoftBreaksInRange: (id, s, e) => { buf(id)?.decor.clearSoftBreaksInRange(s, e); touch(id); return true; },
      addVirtualText: (id, vid, pos, text, r, g, b, before, useBg) => {
        buf(id)?.decor.addVirtualText({ id: vid, pos, text, before, style: useBg ? { bg: [r, g, b] } : { fg: [r, g, b] } });
        touch(id);
        return true;
      },
      addVirtualTextStyled: (id, vid, pos, text, o, before) => {
        buf(id)?.decor.addVirtualText({ id: vid, pos, text, before, style: { fg: o?.fg, bg: o?.bg, bold: o?.bold, italic: o?.italic }, padToColumn: o?.padToColumn ?? null, composeOnly: !!o?.composeOnly, ns: o?.namespace });
        touch(id);
        return true;
      },
      removeVirtualText: (id, vid) => { buf(id)?.decor.removeVirtualText(vid); touch(id); return true; },
      removeVirtualTextsByPrefix: (id, p) => { buf(id)?.decor.removeVirtualTextsByPrefix(p); touch(id); return true; },
      clearVirtualTexts: (id) => { buf(id)?.decor.clearVirtualTexts(); touch(id); return true; },
      clearVirtualTextNamespace: (id, ns) => { buf(id)?.decor.clearVirtualTextNamespace(ns); touch(id); return true; },
      clearVirtualTextsInRange: (id, p, s, e) => { buf(id)?.decor.clearVirtualTextsInRange(p, s, e); touch(id); return true; },
      addVirtualLine: (id, pos, text, o, above, ns, priority) => {
        buf(id)?.decor.addVirtualLine({ pos, text, style: o || {}, above: !!above, ns, priority: priority || 0, composeOnly: !!o?.composeOnly });
        touch(id);
        return true;
      },
      clearVirtualLinesInRange: (id, ns, s, e) => { buf(id)?.decor.clearVirtualLinesInRange(ns, s, e); touch(id); return true; },
      createMarker: (id, key, start, end, payload) => { buf(id)?.decor.markers.set(key, { start, end, payload }); return true; },
      updateMarker: (id, key, payload) => { const m = buf(id)?.decor.markers.get(key); if (!m) return false; m.payload = payload; return true; },
      deleteMarker: (id, key) => !!buf(id)?.decor.markers.delete(key),
      getMarker: (id, key) => { const m = buf(id)?.decor.markers.get(key); return m ? { id: key, ...m } : null; },
      queryMarkers: (id, s, e) => [...(buf(id)?.decor.markers || [])].filter(([, m]) => m.start < e && m.end > s || (m.start === m.end && m.start >= s && m.start < e)).map(([k, m]) => ({ id: k, ...m })),

      // ---- virtual buffers
      createVirtualBuffer: async (o) => {
        const b = app.createVirtualBuffer(o);
        return { bufferId: b.id, splitId: null };
      },
      createVirtualBufferInSplit: async (o) => {
        const b = app.createVirtualBuffer(o);
        const pane = app.splitWith(b, o.ratio ?? 0.5);
        return { bufferId: b.id, splitId: pane.id };
      },
      createVirtualBufferInExistingSplit: async (o) => {
        const b = app.createVirtualBuffer(o);
        return { bufferId: b.id, splitId: app.activePane.id };
      },
      setVirtualBufferContent: (id, entries) => { const b = buf(id); if (!b || !b.setEntries) return false; b.setEntries(entries); touch(id); return true; },

      // ---- prompts and status bar
      startPrompt: (label, type) => { app.prompt.open({ label, type, plugin: true }); return true; },
      startPromptWithInitial: (label, type, initial) => { app.prompt.open({ label, type, plugin: true, initial }); return true; },
      setPromptSuggestions: (s, sel) => { app.prompt.setSuggestions(s, sel); return true; },
      setPromptInputSync: () => true,
      registerStatusBarElement: (token, title) => { P.statusItems.set(token, { title, values: new Map() }); return true; },
      setStatusBarValue: (id, token, value) => { const it = P.statusItems.get(token); if (it) { it.values.set(id, value); app.updateStatus(); } return true; },
    };
    return api;
  }
}

export { byteLen };
