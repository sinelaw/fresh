// The plugin API. Plugins are ES modules in `web/plugins/`, each exporting
// `activate(fresh)`; the client's own features (search, git, LSP panels,
// markdown compose, the explorer...) are written against the same API.
//
//   fresh.commands.register("git.blame", { title: "Git: Toggle Blame", hidden: false, run })
//   fresh.keys.bind("C-S-g", "git.log")
//   fresh.events.on("buffer.changed", ({ buffer, ops }) => ...)
//   const deco = buffer.decorations("compose")   // this plugin's set
//   deco.conceal(s, e, { text: "•", style, reveal: { start, end }, when: "always" })
//   fresh.ui.panel({ id, title, place: "bottom", onClose: null }).render(h("list", {...}))
//   await fresh.host.run("git", ["log", "--oneline"])
//
// Events: "buffer.opened", "buffer.closed", "buffer.changed" ({buffer, ops,
// local}), "buffer.saved", "buffer.activated", "cursor.moved" ({view}),
// "view.resized" ({view}), "app.ready".

import { rpc } from "./rpc.js";
import { h } from "./ui.js";
import { byteLen } from "./ot.js";
import { stringWidth } from "./layout.js";

// A plugin's decorations in one buffer, cleared and replaced as a unit or by
// range. Positions are byte offsets; every one moves with edits.
class DecorationSet {
  constructor(buffer, ns) {
    this.buffer = buffer;
    this.ns = ns;
  }
  get d() { return this.buffer.decor; }
  clear() { this.d.clearNamespace(this.ns); }
  clearRange(start, end) { this.d.clearNamespaceRange(this.ns, start, end); }
  // Style [start, end) (`style`: a Style; `opts`: OverlayOptions, see
  // types/fresh.d.js).
  overlay(start, end, style, opts) { this.d.add("overlays", { ns: this.ns, start, end, style, when: opts.when, mark: opts.mark, above: opts.above }); }
  // Draw `opts.text` instead of [start, end) (ConcealOptions). A conceal
  // over a newline joins the next line onto this row; in a compose view,
  // text "\n" is a line break, and a revealed range is drawn dimmed.
  conceal(start, end, opts) {
    this.d.add("conceals", { ns: this.ns, start, end, text: opts.text, style: opts.style, reveal: opts.reveal, when: opts.when });
  }
  // `text` drawn at `pos` (InlineOptions).
  inline(pos, text, opts) {
    this.d.add("inlines", { ns: this.ns, pos, text, style: opts.style, side: opts.side, edge: opts.edge, when: opts.when });
  }
  // A whole row of text above or below the line holding `pos` (LineOptions).
  line(pos, opts) {
    this.d.add("lines", { ns: this.ns, pos, above: opts.above, text: opts.text, style: opts.style, when: opts.when, order: opts.order });
  }
  // Document structure over the lines of [start, end), drawn by a compose
  // view as typography (`block`: a Block). Inside a table, "cell" nodes
  // split their row at their starts. Paragraph, item and quote lines are
  // joined (each line break drawn as a space) and wrapped; code keeps its
  // lines. A line in no node is a paragraph of its own.
  node(start, end, block, when) {
    this.d.add("nodes", { ns: this.ns, start, end, ...block, when });
  }
  // A mark in the gutter on the line holding `pos`.
  // (GutterOptions; the git gutter's glyph is "▎").
  gutter(pos, opts) { this.d.add("gutters", { ns: this.ns, start: pos, end: pos, glyph: opts.glyph, color: opts.color, title: opts.title }); }
  // This set's gutter marks where they are now (edits move them): [{pos, glyph}].
  gutters() { return this.d.gutters.filter((g) => g.ns === this.ns).map((g) => ({ pos: g.start, glyph: g.glyph })); }
  // This set's overlays where they are now: [{start, end}].
  overlays() { return this.d.overlays.filter((o) => o.ns === this.ns).map((o) => ({ start: o.start, end: o.end })); }
}

class BufferHandle {
  constructor(app, buf, plugin) {
    this.app = app;
    this.b = buf;
    this.plugin = plugin;
  }
  get id() { return this.b.id; }
  get path() { return this.b.path; }
  get name() { return this.b.name; }
  get language() { return this.b.language; }
  get length() { return this.b.len; }
  get modified() { return !!this.b.modified; }
  get isVirtual() { return this.b.isVirtual; }
  // The whole text is held (an ordinary file, or a virtual buffer).
  get whole() { return !!this.b.full; }
  // Held text in [start, end), or null when it is not held.
  text(start = 0, end = this.b.len) { return this.b.textRange(start, end); }
  textAsync(start = 0, end = this.b.len) { return this.b.textAsync(start, end); }
  // The held lines: {lines, starts, firstLine}.
  lines() { const L = this.b.text; return L ? { lines: L.lines, starts: L.starts, firstLine: L.firstLine } : null; }
  edit(ops) { this.app.applyEdit(this.b, ops); }
  insert(pos, text) { this.edit([{ t: "ins", pos, text }]); }
  delete(start, end) { if (end > start) this.edit([{ t: "del", pos: start, len: end - start }]); }
  replace(start, end, text) { const ops = []; if (end > start) ops.push({ t: "del", pos: start, len: end - start }); if (text) ops.push({ t: "ins", pos: start, text }); this.edit(ops); }
  save() { return this.app.saveBuffer(this.b); }
  decorations(name) { return new DecorationSet(this.b, `${this.plugin}:${name}`); }
  views() { return [...this.b.views].map((v) => new ViewHandle(this.app, v, this.plugin)); }
  setEntries(entries) { this.b.setEntries(entries); this.app.renderBuffer(this.b.id); }
  propsAt(pos) { return this.b.propsAt(pos); }
  // Foldable blocks, as document line ranges [{start, end}] (a language
  // server's folding ranges); folding uses indentation where none are given.
  setFoldRanges(ranges) { this.b.foldRanges = ranges; }
  render() { this.app.renderBuffer(this.b.id); }
}

class ViewHandle {
  constructor(app, v, plugin) {
    this.app = app;
    this.v = v;
    this.plugin = plugin;
  }
  get buffer() { return new BufferHandle(this.app, this.v.buf, this.plugin); }
  get cursor() { return this.v.cursors[0].pos; }
  get cursors() { return this.v.cursors.map((c) => ({ pos: c.pos, anchor: c.anchor })); }
  get selection() { return this.v.selection(); }
  get mode() { return this.v.mode; }
  get options() { return { ...this.v.opts }; }
  // {mode: "source"|"compose", composeWidth, wrap, lineNumbers}
  setOptions(o) { Object.assign(this.v.opts, o); if (o.mode) this.v.mode = o.mode; this.app.renderBuffer(this.v.buf.id); }
  // Both resolve once the text there is loaded and the view shows it.
  // (`o`: CursorOptions, types/fresh.d.js.)
  setCursor(pos, o) { return this.app.moveTo(this.v, pos, { center: o.center, extend: false, history: false }); }
  // Select [start, end), the caret at `end`.
  async select(start, end, o) {
    await this.app.moveTo(this.v, start, { center: false, extend: false, history: false });
    await this.app.moveTo(this.v, end, { center: o.center, extend: true, history: false });
  }
  // Columns of text the view shows across (its own width).
  get width() { return this.v.pane.geometry(this.v).width; }
  get visibleLines() { return this.v.pane.visibleLineRange(this.v); }
}

export class Host {
  constructor(app) {
    this.app = app;
    this.handlers = new Map(); // event -> [fn]
    this.plugins = [];
  }

  emit(event, data) {
    for (const f of this.handlers.get(event) || []) {
      try { f(data); } catch (e) { this.app.report(`${event}: ${e.stack || e}`); }
    }
  }

  // Activate `plugins` ([{name, activate}], see plugins/index.js) in order;
  // one that throws is reported and the rest still load.
  async load(plugins) {
    for (const { name, activate } of plugins) {
      try {
        await activate(this.api(name));
        this.plugins.push(name);
      } catch (e) {
        this.app.report(`plugin ${name}: ${e.stack || e}`);
      }
    }
  }

  buffer(b, name) { return b ? new BufferHandle(this.app, b, name) : null; }
  view(v, name) { return v ? new ViewHandle(this.app, v, name) : null; }

  api(name) {
    const app = this.app;
    const host = this;
    const wrapEvent = (data) => {
      const out = { ...data };
      if (data.buffer) out.buffer = host.buffer(data.buffer, name);
      if (data.view) out.view = host.view(data.view, name);
      return out;
    };
    return {
      h,
      workspace: { get root() { return app.root; }, get host() { return app.hostName; }, get trusted() { return app.trusted; } },
      settings: app.settings,
      // Remember a change a plugin made to `settings` (in the browser) and redraw.
      saveSettings: () => app.saveSettings(),
      commands: {
        // `spec`: {title, hidden, run}; a hidden command is left out of the palette.
        register: (id, spec) => app.commands.set(id, { title: spec.title, run: spec.run, source: name, hidden: spec.hidden }),
        run: (id, arg) => app.run(id, arg),
        all: () => [...app.commands].map(([id, c]) => ({ id, title: c.title, source: c.source, hidden: c.hidden })),
      },
      keys: {
        bind: (key, command) => app.bindKey(key, command),
        for: (command) => app.keyFor(command),
        // The first key for a command as people write it ("Ctrl+Shift+K").
        label: (command) => app.keyLabel(command),
        // The effective keymap: {key: command}.
        all: () => app.keymap(),
        // The user's own bindings (saved in the browser), over the defaults.
        user: () => ({ ...app.userKeys }),
        setUser: (key, command) => { if (command) app.userKeys[key] = command; else delete app.userKeys[key]; app.saveKeys(); },
        // The next key pressed, as the keymap names it.
        capture: () => app.captureKey(),
      },
      events: {
        on: (event, f) => {
          if (!host.handlers.has(event)) host.handlers.set(event, []);
          host.handlers.get(event).push((d) => f(wrapEvent(d)));
        },
      },
      buffers: {
        active: () => host.buffer(app.view()?.buf, name),
        all: () => [...app.buffers.values()].map((b) => host.buffer(b, name)),
        byPath: (p) => host.buffer([...app.buffers.values()].find((b) => b.path === p), name),
        // `at`: OpenOptions (types/fresh.d.js).
        open: async (path, at) => { const v = await app.openFile(path, at); return v ? host.buffer(v.buf, name) : null; },
        // The buffer for a file, without showing it.
        load: async (path) => host.buffer(await app.loadFile(path), name),
        // A VirtualOptions (types/fresh.d.js): {name, entries: [{text, props, style}], mode, readOnly, language, keep, place, lineNumbers}
        createVirtual: (o) => host.buffer(app.createVirtual(o), name),
        show: (buffer, place) => app.showBuffer(buffer.b, place, { preview: null, focus: true }),
        close: (buffer) => app.closeBuffer(buffer.b),
      },
      views: {
        // Scroll two buffers' views together through a line mapping:
        // aToB(line of a) -> line of b, bToA the other way. Returns the
        // function that unlinks them.
        linkScroll: (a, b, aToB, bToA) => app.linkScroll(a.b, b.b, aToB, bToA),
        active: () => host.view(app.view(), name),
        all: () => app.panes().flatMap((p) => p.views).map((v) => host.view(v, name)),
      },
      // A palette mode for a prefix: fresh.palette.provide("@", {hint, items(q)})
      palette: { provide: (prefix, o) => app.prompt.provide(prefix, o) },
      // A row at the end of a menu bar menu: fresh.menus.add("Go", {command, label})
      menus: { add: (menu, row) => app.menubar.add(menu, row) },
      // Keys for a buffer kind: fresh.modes.define("git-log", { Enter: fn, q: fn })
      modes: { define: (mode, keys) => app.modes.set(mode, keys) },
      ui: {
        // A PanelOptions (types/fresh.d.js).
        panel: (o) => app.ui.panel(o),
        // A context menu: items [{label, run}] (null: separator) at {x, y}.
        menu: (items, at) => app.ui.menu(items, at),
        // Pick from a list: {title, items: [{label, detail, value}], onAccept, onChange}
        pick: (o) => app.prompt.open(o),
        // Ask for text: {title, value} -> Promise<string|null>
        input: (o) => app.prompt.ask(o),
        // Ask yes or no: (title, yes label) -> Promise<boolean>
        confirm: (title, yes) => app.confirm(title, yes),
        // Ask with a few answers: (title, [{label, value}]) -> Promise<value|null>
        choose: (title, answers) => app.choose(title, answers),
        status: (id) => app.statusItem(id),
        message: (text) => app.setStatus(text),
        popup: (text, at) => app.popup(text, at),
        // Colours: {"--var": css} for the page's colour variables (see
        // style.css; theme keys resolve to them).
        theme: (vars) => app.applyTheme(vars),
      },
      host: {
        run: (program, args = [], opts = {}) => app.runProcess(program, args, opts),
        watch: (path, f) => app.watch(path, f),
        read: async (path) => (await rpc("fs.read", { path })).text,
        write: (path, text) => rpc("fs.write", { path, text }),
        list: async (path) => (await rpc("fs.list", { path })).entries,
        stat: (path) => rpc("fs.stat", { path }),
        create: (path, dir) => rpc("fs.create", { path, dir }),
        rename: (from, to) => rpc("fs.rename", { from, to }),
        remove: (path) => rpc("fs.delete", { path }),
        copy: (from, to) => rpc("fs.copy", { from, to }),
        find: async (query) => (await rpc("fs.find", { query })).files,
        search: (query, o = {}) => rpc("search.project", { query, ...o }),
        // The next (or previous) occurrence of `query` in a document, found by
        // the server: how a huge file is searched.
        searchDocument: async (buffer, query, from, backward) => (await rpc("doc.search", { doc: buffer.b.doc, query, from, backward })).pos,
        // The editor's themes: [{key, name}], and one fully resolved.
        themes: async () => (await rpc("theme.list", {})).themes,
        theme: (key) => rpc("theme.get", { key }),
      },
      lsp: {
        available: (buffer) => app.lspAvailable(buffer.b),
        request: (buffer, method, params = {}, pos) => rpc("lsp.call", { doc: buffer.b.doc, method, params, pos }),
        // LSP (line, character) to a byte offset in a held buffer.
        offset: (buffer, line, character) => app.lspOffset(buffer.b, line, character),
        // A byte offset in a held buffer as an LSP {line, character} (UTF-16).
        position: (buffer, pos) => app.lspPosition(buffer.b, pos),
        diagnostics: (buffer) => buffer.b.decor.diags.map((d) => ({ ...d })),
        // The languages with a server on the host, and which servers run: {language: running}.
        languages: () => [...(app.lspLanguages || [])],
        status: () => rpc("lsp.status", {}),
        // A request about the workspace (`workspace/symbol`) to `language`'s
        // running server.
        requestWorkspace: (language, method, params = {}) => rpc("lsp.call", { language, method, params }),
        // Stop a language's server (until restarted), or start it again;
        // both resolve with {language: running}.
        stop: (language) => rpc("lsp.stop", { language }),
        restart: (language) => rpc("lsp.restart", { language }),
      },
      // Text shown when the pointer rests on a position: f(buffer, pos) -> string|null.
      hover: { provide: (f) => app.hoverProvide((b, p) => f(host.buffer(b, name), p)) },
      text: { byteLen, width: stringWidth },
    };
  }
}
