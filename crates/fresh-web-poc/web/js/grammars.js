// Plugin grammars (`registerGrammar`): `.sublime-syntax` sources plugins
// add, each for a language and its file extensions. Every registration
// builds a new grammar set, the server's with every registered grammar, in
// the grammar worker (grammar-worker.js: a second instance of the client's
// WebAssembly module, so the main thread never waits on a build), one
// build at a time in the order they were asked for. The set built goes to
// the engine (Highlighting.swap), which reopens every buffer in it. A
// grammar that does not build is reported to the plugin that registered
// it, and the engine keeps the set it has.

import { found } from "./maybe.js";

// What travels to the worker and back (grammar-worker.js).
/** function startMsg(module: WasmModule, packdump: Uint8Array) => ToGrammarWorker */
function startMsg(module, packdump) { return { kind: "start", module, packdump }; }
/** function buildMsg(id: Number, grammars: GrammarSource[]) => ToGrammarWorker */
function buildMsg(id, grammars) { return { kind: "build", id, grammars }; }

// The name a grammar's syntax goes by: its YAML's top-level `name`, else
// the name it was built under (its language), as the build names it.
/** function syntaxName(source: String, language: String) => String */
function syntaxName(source, language) {
  const m = /^name:[ \t]*(.*?)[ \t]*$/m.exec(source);
  const named = m === null ? "" : m[1].replace(/^(["'])(.*)\1$/, "$2");
  return named === "" ? language : named;
}

/** function grammar(language: String, source: String, extensions: String[]) => Grammar */
function grammar(language, source, extensions) {
  return { language, source, extensions: extensions.map((e) => e.replace(/^\./, "")), syntax: syntaxName(source, language) };
}

// A build the worker is asked for, and who waits on it.
/** type Build = {resolve: (Uint8Array) => Undefined, reject: (Error) => Undefined} */

export class Grammars {
  // `highlighting`: the engine, once it is loaded.
  constructor(highlighting) {
    /** this.highlighting: Promise<Highlighting> */
    this.highlighting = highlighting;
    /** this.worker: Worker<ToGrammarWorker, FromGrammarWorker>[] */
    this.worker = []; // made at the first build (a list of at most one)
    /** this.builds: Map<Number, Build> */
    this.builds = new Map();
    this.nextBuild = 1;
    // The last registration, which the next one waits for.
    /** this.turn: Promise<Undefined> */
    this.turn = Promise.resolve(undefined);
  }

  // Add (or replace) the grammar for `language`, for files with
  // `extensions`: resolves once buffers are coloured with it, rejects with
  // why it did not build.
  /** method register(language: String, source: String, extensions: String[]) => Promise<Undefined> */
  register(language, source, extensions) {
    const g = grammar(language, source, extensions);
    const run = this.turn.then(async () => {
      const hl = await this.highlighting;
      const set = [...hl.grammars.filter((x) => x.language !== language), g];
      const packdump = await this.build(hl, set);
      await hl.swap(packdump, set);
    });
    this.turn = run.catch(() => undefined);
    return run;
  }

  // The grammar set of `set` built onto the server's.
  /** method build(hl: Highlighting, set: Grammar[]) => Promise<Uint8Array> */
  build(hl, set) {
    const worker = this.workerFor(hl);
    const id = this.nextBuild++;
    return new Promise((resolve, reject) => {
      this.builds.set(id, { resolve, reject });
      worker.postMessage(buildMsg(id, set.map((g) => ({ name: g.language, source: g.source }))));
    });
  }

  /** method workerFor(hl: Highlighting) => Worker<ToGrammarWorker, FromGrammarWorker> */
  workerFor(hl) {
    for (const w of this.worker) return w;
    const w = new Worker("/js/grammar-worker.js", { type: "module" });
    w.onmessage = (e) => {
      /** const m: FromGrammarWorker */
      const m = e.data;
      for (const b of found(this.builds.get(m.id))) {
        this.builds.delete(m.id);
        if (m.kind === "built") b.resolve(m.packdump);
        else b.reject(new Error(m.why));
      }
    };
    // A worker that fails fails every build it was given; the next build
    // starts another.
    w.onerror = (e) => {
      const builds = [...this.builds.values()];
      this.builds.clear();
      this.worker = [];
      for (const b of builds) b.reject(new Error(`the grammar worker failed: ${e.message}`));
    };
    w.postMessage(startMsg(hl.engine.w.module, hl.packdump));
    this.worker = [w];
    return w;
  }
}
