# Spike: plugins on Debian's system QuickJS

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

**Status: SPIKE.** Nothing here ships. The measurements are from the spike as it
was run (Debian testing's `libquickjs` 2025.04.26, amd64), not live figures.

**Question.** For a Debian package of Fresh, rquickjs is the blocker. It is not
in Debian, and it bundles quickjs-ng, while Debian ships Bellard's QuickJS
(`libquickjs`, 2025.04.26). Can Fresh bind its own crate to the system QuickJS,
and do the plugins still work on that engine?

**Answer so far: yes, on amd64.** The binding builds and links against Debian's
library. It covers what the plugin runtime needs from rquickjs. Every bundled
plugin behaves the same on both engines under the harness below. The open work
is the port itself: moving `fresh-plugin-runtime` off rquickjs onto the
binding.

Everything lives in [`spikes/debian-quickjs/`](../../spikes/debian-quickjs), a
separate Cargo workspace. It needs `libquickjs` to build, so it stays out of the
main workspace and out of CI for now. To rerun everything from a clean checkout
(it fetches Debian's `.deb` if `libquickjs` isn't installed):

```sh
spikes/debian-quickjs/run-compat.sh
```

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

## What was built

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

## Results

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
(`JS_FreeRuntime: Assertion 'list_empty(&rt->gc_obj_list)'`). I checked that it
fires on a deliberately leaked reference. So a refcount bug in the wrapper would
abort the test run instead of passing quietly.

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

## What this does not show

- **Only plugin entry points are exercised.** The stub never answers host
  promises, so code after the first `await` on the host (the 50 waiting handlers)
  never runs on either engine. The full end-to-end suite has to run on the
  ported runtime to cover those paths.
- **Only amd64 was tested.** The shim is meant to make 32-bit (NaN-boxed) targets
  work, but nothing has run there yet.
- **rquickjs's lifetime-branded API is not reproduced.** Values here are
  refcounted handles that keep their context alive. That is simpler and safe. The
  catch: a native closure that captures a `Value` forms a cycle through the JS
  heap and leaks the runtime. The real port needs a rule or a weak handle type
  for that.
- **`bundled` mode** (compiling a vendored QuickJS for non-Debian builds) is not
  in the spike.

## What the port would take

1. Grow `fresh-quickjs` to what `quickjs_backend.rs` uses:
   - `Opt`/`Rest`, and `FromJs`/`IntoJs`-style conversions;
   - one class instance (`JsEditorApi`) with about 100 methods.

   `rquickjs_serde` becomes the JSON bridge above.
2. Have `fresh-plugin-api-macros` generate the method glue. It already parses the
   `#[qjs(rename)]` methods. That removes `#[rquickjs::methods]` and
   rquickjs-macro.
3. Port `quickjs_backend.rs` (15k lines, about 550 rquickjs references, mostly
   `Ctx`/`Value`/`Object`) and `fresh-core`'s `FromJs` impls.
4. Add a `bundled` feature that vendors the same Bellard release, so upstream
   builds and CI run the engine Debian ships. Debian's source package then
   excludes the vendored copy.
5. Move the crates into the main workspace and run the end-to-end suite with
   CI on the system engine.

Together with the rest of the Debian plan (ts-rs dev-only, oxc replaced by
esbuild at build time, no tree-sitter, manifest version bumps), this leaves
**no new Rust packages for Debian**: `fresh-editor` builds from what is already
in the archive plus `libquickjs`. The package declares `Built-Using: quickjs`
because it links QuickJS statically.
