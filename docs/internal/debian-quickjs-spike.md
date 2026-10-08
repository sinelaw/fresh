# Plugins on Debian's system QuickJS

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

**Status: IMPLEMENTED (second backend), not the default.** Fresh's plugin
runtime builds on either rquickjs (the default) or the system QuickJS that
Debian ships, chosen at build time. Measurements below are from Debian
testing's `libquickjs` 2025.04.26 on amd64.

**Question.** For a Debian package of Fresh, rquickjs is the blocker. It is not
in Debian, and it bundles quickjs-ng, while Debian ships Bellard's QuickJS
(`libquickjs`, 2025.04.26). Can Fresh run its plugins on the system QuickJS?

**Answer: yes, on amd64.** `crates/fresh-js` is the one crate that names the JS
engine, and it has two backends:

- **rquickjs** (default): plain re-exports of rquickjs.
- **system** (`RUSTFLAGS="--cfg fresh_js_system"`): Fresh's own implementation
  of the same names over Debian's `libquickjs`, through `crates/fresh-quickjs-sys`
  and the proc macros in `crates/fresh-js-macros`.

The plugin runtime builds unchanged on either. Its test suite and the editor's
plugin end-to-end tests pass on the system backend, and a CI job runs them in a
`debian:testing` container with `apt install libquickjs`.

To build and test against the system QuickJS locally (with `libquickjs`
installed, or with `QUICKJS_INCLUDE_DIR`/`QUICKJS_LIB_DIR` pointing at an
unpacked `.deb`):

```sh
RUSTFLAGS="--cfg fresh_js_system" RUSTDOCFLAGS="--cfg fresh_js_system" \
    cargo test -p fresh-js -p fresh-plugin-runtime
```

Set the cfg in `RUSTDOCFLAGS` too: doctests are compiled by rustdoc, which does
not read `RUSTFLAGS`, and would otherwise build `fresh-js` for the rquickjs
backend without rquickjs in the dependency graph.

The backend is a cfg rather than a cargo feature on purpose: CI builds with
`--all-features` on Linux, macOS and Windows, and a feature would make every one
of those jobs need `libquickjs`. The cfg is declared to `check-cfg` in the
workspace lints.

The investigation that led here (the engine comparison and plugin harness)
lives in [`spikes/debian-quickjs/`](../../spikes/debian-quickjs), a separate
Cargo workspace; `spikes/debian-quickjs/run-compat.sh` reruns it.

## The system backend

`fresh-js`'s export list is the contract: the types `Runtime`, `Context`,
`Ctx`, `Value`, `Object`, `Array`, `String`, `Function`, `Persistent`, `Class`,
`Type`, `Error`, `Result`; the traits `FromJs`, `IntoJs`, `JsLifetime` and
`class::Trace`; `function::{Opt, Rest}`, `context::EvalOptions`,
`serde::{from_value, to_value}`; and the `#[class]`/`#[methods]` attributes and
`Trace`/`JsLifetime` derives. The system backend provides exactly those, with
rquickjs's signatures, for the parts Fresh calls. Code outside `fresh-js` names
nothing else, so it compiles against either backend.

What it copies from rquickjs, because Fresh depends on it:

- **Memory model.** A `Ctx<'js>` holds one reference to its `JSContext`, a
  `Value<'js>` holds one reference to its `JSValue` plus a `Ctx`, and the `'js`
  lifetime keeps values from escaping the `Context::with` that produced them. A
  `Persistent` holds a reference against the runtime and must be dropped before
  it, as with rquickjs.
- **Conversion rules and messages.** Which JS types each Rust type accepts,
  numeric range checks ("Underflow"/"Overflow"), `Option` from
  `undefined`/`null`, and the error text plugins and tests match on, such as
  `Error converting from js 'undefined' into type 'string'`. Conversion errors
  raised in a native call surface in JS as `TypeError`s, as with rquickjs.
- **Native-call parameters.** A required parameter missing from the call is an
  error; `Opt<T>` is `None` only when the caller passed fewer arguments; `Rest<T>`
  takes the remainder; a `Ctx` parameter is injected.
- **`#[methods]`.** Every method not marked `#[qjs(skip)]` is exported (private
  ones too), under its camelCase name or `#[qjs(rename)]`.
- **Strict mode.** `eval` is a strict global script by default.
- **Panics.** A panic in a native call is caught at the FFI boundary, carried
  through JS as an exception and resumed once control is back in Rust.

Where it differs:

- **serde goes through JSON** (`JSON.stringify`/`JSON.parse` plus `serde_json`)
  instead of walking values. For the plain data the plugin API exchanges, the
  result is the same; `NaN`, BigInts and lone surrogates are where it is not.
- **Bellard-only shapes are handled.** Long concatenations come back as rope
  strings (`JS_TAG_STRING_ROPE`), which read as ordinary strings.

Each test frees its runtime at the end, and Debian's `libquickjs` keeps
QuickJS's teardown assertion (`JS_FreeRuntime: Assertion
'list_empty(&rt->gc_obj_list)'`), so a reference leak in the backend aborts the
test run. That assertion caught one while the backend was being written (a
handle that leaked its context reference when its value was handed to the
engine).

## Why not just point rquickjs at Debian's library

rquickjs 0.11 is written against quickjs-ng. Bellard's header differs in ways
that compile and then break at runtime:

- 9 functions rquickjs calls do not exist: the `JS_*Proxy*` family,
  `JS_GetTypedArrayType`, `JS_SetPromiseHook`, `JS_GetFunctionProto`,
  `JS_SetDumpFlags` and `JS_AtomToCStringLen`.
- About 20 signatures differ: `JS_NewClassID` takes no runtime, `JS_IsArray` and
  `JS_IsError` take a context, `JS_SetOpaque` returns `void`, and the
  `JS_READ_OBJ_*`/`JS_WRITE_OBJ_*` flag values differ.
- rquickjs hard-codes ~190 atom ids from quickjs-ng's internal atom table. Debian
  does not ship that header, and Bellard's table is different.
- Bellard's engine has a rope-string tag (`JS_TAG_STRING_ROPE = -6`) that
  rquickjs does not know.

## The spike

The spike that preceded the backend built a smaller wrapper and ran every
bundled plugin on both engines. Its results still stand.

### What was built

| Piece | What it does |
| --- | --- |
| `fresh-quickjs-sys` | Runs bindgen against the installed `quickjs.h` at build time and links `libquickjs.a`. `shim.c` exports the header's static-inline helpers and value macros (`JS_FreeValue`, `JS_DupValue`, `JS_NewInt64`, tag reads, …) as real functions, so Rust never encodes the `JSValue` layout. That layout is NaN-boxed on 32-bit targets. The shim also gives one signature for the calls that differ between the two engines. |
| `fresh-quickjs` | A safe wrapper of about 600 lines: `Runtime`, `Context`, `Value`, `eval`/`compile_check`, native functions backed by Rust closures, calls into JS, JSON bridging to `serde_json`, the pending-job loop, a memory limit and an interrupt deadline. |
| `compat/` + `compat_system` / `compat-ng` | The same plugin harness, run once on the system QuickJS and once on quickjs-ng through rquickjs 0.11 (what Fresh ships today). |
| `fresh-parser-js` example `dump_plugins_js` | Writes the JS the runtime actually executes for each bundled plugin. It transpiles and bundles each plugin exactly as the loader does, and also writes out the runtime's JS bootstrap. |

Debian specifics handled by `build.rs`:

- `libquickjs` is static-only.
- It lives in `/usr/lib/<multiarch>/quickjs/`, with the triplet taken from
  `DEB_HOST_MULTIARCH` or `cc -print-multiarch`.
- It has no pkg-config file, so the paths are defaults, overridable through
  `QUICKJS_INCLUDE_DIR` and `QUICKJS_LIB_DIR`.

`bindgen` 0.72 and `cc` 1.2 are both in Debian testing.

### Engine tests (`fresh-quickjs/tests/engine.rs`): 16/16 pass

The tests cover:
- numbers and strings, including non-ASCII text and long concatenations;
- exceptions with name, message and stack, and syntax errors from a compile-only
  check;
- native functions: arguments, return values, `Error`s catchable in JS, and
  panics turned into JS exceptions;
- calling JS from Rust, and a JSON round-trip through serde;
- the runtime's async pattern: a `_xStart` host method returns an id, JS parks a
  promise on it, and the host resolves it through `_resolveCallback` plus the job
  loop;
- one context per plugin, the memory limit, the interrupt deadline, and 1000
  native closures freed with the runtime.

Debian's `libquickjs` keeps QuickJS's own teardown assertion
(`JS_FreeRuntime: Assertion 'list_empty(&rt->gc_obj_list)'`), which fires on a
deliberately leaked reference. A correction: as first written, the spike's
wrapper leaked the context handle of every value it handed to the engine, so
its runtimes were never freed and the assertion never ran for those tests. The
same mistake in the system backend was caught by that assertion; the spike's
wrapper now has the fix too, and its 16 tests still pass with real teardown.

### Bundled plugins on both engines: no differences

Each plugin runs in a fresh runtime:
1. The host is a permissive `Proxy` stub of `editor`. Every call goes through a
   native function, so host calls are counted.
2. The runtime's real JS bootstrap runs first.
3. The plugin runs, wrapped in an IIFE exactly as `execute_js` does.
4. Every command and event handler the plugin registered is called.

| | system QuickJS (Debian) | quickjs-ng (rquickjs 0.11) |
| --- | --- | --- |
| plugins loaded | 49/49 | 49/49 |
| host calls | 2373 | 2373 |
| handlers ok / waiting on a host promise / not defined | 351 / 50 / 2 | 351 / 50 / 2 |
| plugins with any difference | — | **0** |
| full run, release build | ~0.27–0.30 s | ~0.27–0.31 s |

### Built-ins

quickjs-ng has 38 built-in features that Debian's QuickJS lacks. Most are
ES2024/ES2025 additions: `Iterator` and its helpers, the new `Set` methods,
`Promise.try`, `RegExp.escape`, `Array.fromAsync`, `Float16Array`,
`Error.captureStackTrace`, resizable `ArrayBuffer`s, `Map.prototype.getOrInsert`.
It also adds `performance` and `queueMicrotask`.

A precise search of the dumped plugin JS found **no use of any of them**. The
coarse name match in `compare.py` flags 17, and each one checks out as a false
positive. For example, the `Iterator` hits are Rust code inside a string in
`welcome_screen`, and `setTimeout` is the host's own `editor.setTimeout`.

User plugins and `init.ts` are type-checked against `lib: ["ES2020"]`
(`init_script.rs`), so TypeScript already flags those built-ins for authors.

The harness only reaches plugin entry points: the stub never answers host
promises, so code after a handler's first `await` on the host never ran. The
editor's end-to-end plugin tests on the system backend cover those paths.

## What is not covered

- **Only amd64 has run.** The shim is meant to make the 32-bit (NaN-boxed)
  targets work, but nothing has run there yet.
- **No `bundled` mode.** Upstream builds stay on rquickjs; there is no option to
  compile a vendored Bellard QuickJS for non-Debian builds.
- **The rest of the editor's end-to-end suite** (beyond the plugin tests) runs
  only on the default backend in CI.

## What remains for a Debian package

Together with the rest of the Debian plan (ts-rs dev-only, oxc replaced by
esbuild at build time, no tree-sitter, manifest version bumps), this leaves
**no new Rust packages for Debian**: `fresh-editor` builds from what is already
in the archive plus `libquickjs`, with `RUSTFLAGS="--cfg fresh_js_system"`. The
package declares `Built-Using: quickjs` because it links QuickJS statically.
