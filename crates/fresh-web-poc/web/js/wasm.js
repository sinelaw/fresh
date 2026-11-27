// The client's one WebAssembly module (crates/fresh-highlight-wasm): the
// syntax engine (hl.js), the buffers' marker lists (markers.js) and the
// grapheme clusters text is laid out by (clusters.js). main.js loads it
// once, before anything else; the app starts with it. The grammar worker
// (grammar-worker.js) runs a second instance of the same compiled module.
//
// Its ABI is raw: every argument is a u32; a call that can fail returns a
// code (0: ok) or a pointer to its answer (0: failed), and `hl_error` then
// says why. Answers live in the module's memory until its next call.

const dec = new TextDecoder();
const enc = new TextEncoder();

// The module's exports (crates/fresh-highlight-wasm/src/lib.rs).
/** type WasmExports = {
 *   memory: {buffer: ArrayBuffer},
 *   hl_alloc: (Number) => Number,
 *   hl_free: (Number, Number) => Undefined,
 *   hl_error: () => Number,
 *   hl_init: (Number, Number) => Number,
 *   hl_categories: () => Number,
 *   hl_open: (Number, Number, Number, Number, Number) => Number,
 *   hl_open_declared: (Number, Number, Number) => Number,
 *   hl_regions: (Number, Number, Number) => Number,
 *   hl_build: (Number, Number, Number, Number) => Number,
 *   hl_close: (Number) => Number,
 *   hl_edit: (Number, Number, Number, Number, Number) => Number,
 *   hl_want: (Number, Number, Number) => Number,
 *   hl_work: (Number) => Number,
 *   hl_take: (Number) => Number,
 *   mk_new: () => Number,
 *   mk_drop: (Number) => Number,
 *   mk_point: (Number, Number, Number) => Number,
 *   mk_span: (Number, Number, Number, Number, Number) => Number,
 *   mk_delete: (Number, Number) => Number,
 *   mk_edit: (Number, Number, Number, Number) => Number,
 *   mk_get: (Number, Number) => Number,
 *   mk_query: (Number, Number, Number) => Number,
 *   uc_clusters: (Number, Number) => Number,
 * } */
export class Wasm {
  // `module`: the compiled module `exports` are an instance's of.
  constructor(exports, module) {
    /** this.x: WasmExports */
    this.x = exports;
    /** this.module: WasmModule */
    this.module = module;
  }
  // `n` bytes of the module's memory from `ptr`.
  bytesAt(ptr, n) { return new Uint8Array(this.x.memory.buffer).subarray(ptr, ptr + n); }
  // The little-endian u32 at byte `ptr` of the module's memory.
  u32(ptr) {
    const b = this.bytesAt(ptr, 4);
    return (b[0] | (b[1] << 8) | (b[2] << 16) | (b[3] << 24)) >>> 0;
  }
  // A [len u32][utf8] string at `ptr`.
  /** method str(ptr: Number) => String */
  str(ptr) {
    const n = this.u32(ptr);
    return dec.decode(this.bytesAt(ptr + 4, n));
  }
  // What the last failed call says.
  failed() { return new Error(this.str(this.x.hl_error() >>> 0)); }
  // A call's code: throws what failed.
  check(code) { if (code !== 0) throw this.failed(); }
  // A call's [n u32][n words] answer at `ptr` (0: it failed, thrown), as
  // its words.
  /** method words(ptr: Number) => Uint32Array */
  words(ptr) {
    const at = ptr >>> 0;
    if (at === 0) throw this.failed();
    const n = this.u32(at);
    // The answer buffer is u32-aligned in the module's memory.
    return new Uint32Array(this.x.memory.buffer, at + 4, n).slice();
  }
  // A [len u32][bytes] answer at `ptr` (0: it failed, thrown), copied out.
  /** method framed(ptr: Number) => Uint8Array */
  framed(ptr) {
    const at = ptr >>> 0;
    if (at === 0) throw this.failed();
    return this.bytesAt(at + 4, this.u32(at)).slice();
  }
  // `f(ptr, len)` with `bytes` copied into the module's memory.
  withBytes(bytes, f) {
    const p = this.x.hl_alloc(bytes.length) >>> 0;
    if (p === 0) throw this.failed();
    this.bytesAt(p, bytes.length).set(bytes);
    try { return f(p, bytes.length); } finally { this.x.hl_free(p, bytes.length); }
  }
}

// A list argument's parts (`hl_regions`, `hl_build`): a u32 word, or a
// text (behind its byte length).
/** type Field = {kind: "word", n: Number} | {kind: "text", bytes: Uint8Array} */
/** function wordField(n: Number) => Field */
export function wordField(n) { return { kind: "word", n }; }
/** function textField(s: String) => Field */
export function textField(s) { return { kind: "text", bytes: enc.encode(s) }; }
// Fields as the module reads a list: little-endian words, each text behind
// its length, nothing padded.
/** function encodeFields(fields: Field[]) => Uint8Array */
export function encodeFields(fields) {
  /** const size: (Field) => Number */
  const size = (f) => (f.kind === "word" ? 4 : 4 + f.bytes.length);
  const out = new Uint8Array(fields.reduce((n, f) => n + size(f), 0));
  let at = 0;
  const word = (n) => {
    for (let k = 0; k < 4; k++) out[at + k] = (n >>> (8 * k)) & 255;
    at += 4;
  };
  for (const f of fields) {
    if (f.kind === "word") word(f.n);
    else {
      word(f.bytes.length);
      out.set(f.bytes, at);
      at += f.bytes.length;
    }
  }
  return out;
}

// Fetch and instantiate the module the server serves.
/** function loadWasm() => Promise<Wasm> */
export async function loadWasm() {
  const r = await fetch("/hl/highlight.wasm");
  if (!r.ok) throw new Error(`the server did not serve /hl/highlight.wasm (${r.status})`);
  const { instance, module } = await WebAssembly.instantiate(await r.arrayBuffer(), {});
  return new Wasm(instance.exports, module);
}
// A new instance of a compiled module.
/** function instantiate(module: WasmModule) => Promise<Wasm> */
export async function instantiate(module) {
  const instance = await WebAssembly.instantiate(module, {});
  return new Wasm(instance.exports, module);
}
