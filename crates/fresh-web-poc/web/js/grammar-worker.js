// The grammar worker: compiles syntax sets off the main thread, in a second
// instance of the client's WebAssembly module (wasm.js), so that adding a
// plugin's grammar (hundreds of milliseconds) never stalls typing. A build
// is a pure function: the server's syntax set and every registered grammar
// in, a syntax set out, for the engine to load (grammars.js asks; hl.js
// loads it).
//
// Messages in: `start` (the compiled module and the server's packdump),
// then `build`s, answered in order by `built` (the set, transferred) or
// `failed` (why: every grammar that did not parse, by name).

import { instantiate, encodeFields, wordField, textField } from "./wasm.js";

// Its answers (FromGrammarWorker, types/fresh.d.js).
/** function builtMsg(id: Number, packdump: Uint8Array) => FromGrammarWorker */
function builtMsg(id, packdump) { return { kind: "built", id, packdump }; }
/** function failedMsg(id: Number, why: String) => FromGrammarWorker */
function failedMsg(id, why) { return { kind: "failed", id, why }; }

// The module instance and the set builds start from, once started (a list
// of at most one); builds asked for before then wait on it.
/** let started: Promise<{wasm: Wasm, packdump: Uint8Array}>[] */
let started = [];

// The grammars as `hl_build` reads them: [name][source] each.
/** function sourcesOf(grammars: GrammarSource[]) => Uint8Array */
function sourcesOf(grammars) {
  return encodeFields([wordField(grammars.length), ...grammars.flatMap((g) => [textField(g.name), textField(g.source)])]);
}

/** function build(wasm: Wasm, packdump: Uint8Array, grammars: GrammarSource[]) => Uint8Array */
function build(wasm, packdump, grammars) {
  return wasm.withBytes(packdump, (pp, pn) => wasm.withBytes(sourcesOf(grammars), (sp, sn) => wasm.framed(wasm.x.hl_build(pp, pn, sp, sn))));
}

self.onmessage = (e) => {
  /** const m: ToGrammarWorker */
  const m = e.data;
  if (m.kind === "start") {
    const packdump = m.packdump;
    started = [instantiate(m.module).then((wasm) => ({ wasm, packdump }))];
    return;
  }
  const id = m.id, grammars = m.grammars;
  for (const s of started) {
    s.then((w) => {
      const set = build(w.wasm, w.packdump, grammars);
      self.postMessage(builtMsg(id, set), [set.buffer]);
    }).catch((err) => { self.postMessage(failedMsg(id, err instanceof Error ? err.message : String(err)), []); });
    return;
  }
  self.postMessage(failedMsg(id, "the grammar worker was not started"), []);
};
