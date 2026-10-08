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

- **64-bit only so far.** The `Debian package` workflow builds and tests the
  package on amd64 and arm64 (native runners). The shim is meant to make the
  32-bit (NaN-boxed) targets, armhf and i386, work too, but nothing has run
  there yet.
- **No `bundled` mode.** Upstream builds stay on rquickjs; there is no option to
  compile a vendored Bellard QuickJS for non-Debian builds.
- **The rest of the editor's end-to-end suite** (beyond the plugin tests) runs
  only on the default backend in CI.

## TypeScript without oxc: esbuild

oxc (Fresh's in-process TypeScript toolchain, a few dozen crates) is not in
Debian either. Debian does ship `esbuild` (a Go binary, `Depends: libc6`), so a
Debian build drops oxc and runs esbuild instead:

- `fresh-parser-js` has a cargo feature `oxc` (default, forwarded by
  `fresh-plugin-runtime` and `fresh-editor`). Without it, the same five
  functions (`transpile_typescript`, `bundle_module`,
  `strip_imports_and_exports`, `syntax_errors`, `emit_isolated_declarations`)
  run `esbuild` (`$FRESH_ESBUILD`, else `esbuild` on `PATH`).
- Every plugin load goes through those functions, so they all take the esbuild
  path: bundled plugins, user plugins, plugins and language packs installed by
  the package manager (`pkg.ts` → `editor.loadPlugin` → the plugin thread's
  loader), `init.ts`, and plugin scripts. `init.ts` syntax checks go through
  `syntax_errors`, with esbuild's positions mapped to 1-based columns.
- Results are cached under `$XDG_CACHE_HOME/fresh/esbuild` (else
  `~/.cache/fresh/esbuild`), keyed on the source, Fresh's version and
  esbuild's version; a bundle's entry records the content hash of every input
  file esbuild read, so editing an imported file invalidates it. A cold
  transform costs one esbuild process (a few milliseconds); a warm start runs
  only `esbuild --version`, once per session.
- Bundles use `--tree-shaking=false` so that functions a plugin registers by
  name (and never references) survive, `--packages=external` for
  `fresh:`-style imports, and the import/export lines esbuild leaves are
  stripped so the result runs as a script, as with oxc.
- `.d.ts` emit (`emit_isolated_declarations`) has no esbuild equivalent and
  returns an error; the plugin runtime skips declaration emit when
  `fresh_parser_js::CAN_EMIT_DECLARATIONS` is false, and the oxc-only
  `api_docs`/`ts_export` modules (type generation for `fresh.d.ts`, a
  development task) are compiled only with `oxc`.
- If esbuild is missing, loading a TypeScript plugin fails with a message that
  says to `apt install esbuild` or set `FRESH_ESBUILD`.

The Debian CI job builds with `--no-default-features` (so no oxc) and runs the
`fresh-parser-js` tests (including ones against the real esbuild), the plugin
runtime's tests, and the editor's plugin end-to-end tests with exactly the
Debian package's features (`runtime,plugins,embed-plugins`).

## The Debian build's dependencies

With the system QuickJS, esbuild in place of oxc, and the Debian feature set
(`runtime,plugins,embed-plugins`), **Debian needs no new Rust packages**:
every dependency of `fresh-editor` is in testing at a version Fresh accepts,
with the features Fresh asks for. Getting there also took:

- **`ts-rs` is optional.** Its `#[derive(TS)]` only feeds the `fresh.d.ts`
  generator. `fresh-core`'s `ts` feature selects the real derive, and the
  plugin runtime's `oxc` feature (which builds the generator) turns it on.
  Without it, `TS` is a no-op derive from `fresh-plugin-api-macros` that
  accepts the same `#[ts(...)]` attributes.
- **Versions aligned with the archive:** `which` 8 and `jsonc-parser` 0.33
  (what Debian has); `nix` `>=0.30, <0.32` and `libloading` `>=0.8, <0.10`
  (Debian has 0.30 and 0.8; upstream builds keep resolving to the newer
  ones); `notify`'s `macos_kqueue` feature, which Debian's package lacks, is
  now requested on macOS only.
- **No `tree-sitter`:** the JavaScript, TypeScript and Templ grammars are not
  in Debian. Indentation falls back to the regex rules.
- **No `http`** is a choice, not a constraint: Debian has everything it needs.
  It is off because it carries the update checker and anonymous telemetry. The
  package manager does not need it (it uses `git`); only installing a single
  theme from a direct file URL, and the `editor.httpFetch` plugin API, return
  an error without it.

`scripts/debian.py check PACKAGES[.xz]` checks this against a Debian
`Packages` index. It walks the Debian build's graph from the workspace crates,
and for every dependency it checks that the archive has the crate at a
matching version with the requested features (crates the archive has bring
their own, archive-consistent dependencies). The Debian CI job runs it against
testing's live index, and also compiles against the exact `nix`,
`libloading` and `jsonc-parser` versions Debian ships, so the widened ranges
stay true. With `tree-sitter` on, it lists the three missing grammars; with
`oxc` on, the 30-odd oxc crates.

## Building from Debian's packaged crates

Debian does not build from crates.io or `Cargo.lock`: it builds offline
against the `librust-*-dev` packages, which unpack into
`/usr/share/cargo/registry`, with cargo's `crates-io` source replaced by that
directory. Cargo then resolves the *whole* workspace from it, every optional,
dev-only and other-target dependency included, before building anything, so
the manifests need the kind of patch a Debian source package carries.
`scripts/debian.py` does both halves:

- `packages PACKAGES [--control]` lists the packages to install: those that
  provide every dependency of every workspace crate, optional and dev ones
  included, that the archive can satisfy. apt brings in their dependencies
  (about 750 packages) and the C libraries the `-sys` crates link, such as
  `libonig-dev`. `--control` prints them as `debian/control` Build-Depends.
- `patch-manifests [REGISTRY]` rewrites the manifests in place. It drops
  dependencies for other targets (Windows, macOS, the rquickjs backend) and
  the optional and dev-dependencies the registry lacks (oxc, ts-rs, the three
  tree-sitter grammars, self-update's `zip` 2), with the feature entries
  naming them. It also leaves out `fresh-gui`, whose wgpu renderer is not
  packaged and which only the optional `gui` feature uses. A required
  dependency the registry lacks is an error.

**`packaging/debian/`** is a Debian source package built this way (its
`README.source` has the details). It is separate from the repository's own
`debian/`, which builds the release `.deb` from crates.io with the default
features:

- `debian/control` lists the `librust-*-dev` Build-Depends that
  `scripts/debian.py packages --control` generates (debcargo-style:
  `librust-<crate>-<semver>+<feature>-dev`, dev-only ones `<!nocheck>`),
  plus `libquickjs`, `dh-cargo`, `python3-tomlkit` and, for the tests,
  `esbuild`. The binary package `Depends: esbuild` and records QuickJS in
  `Static-Built-Using` (`fresh-quickjs-sys`'s build script tells
  `dh-cargo-built-using` that QuickJS is MIT and comes from `libquickjs`).
- `debian/rules` runs `patch-manifests` (restoring the originals on `clean`),
  builds through dh-cargo's cargo wrapper with `--cfg fresh_js_system`, and
  in `dh_auto_test` runs the `fresh-js`, `fresh-parser-js` (against the real
  esbuild) and `fresh-plugin-runtime` tests. Fresh's dev-dependencies are all
  in Debian too, once the test harness's `ctor` accepts 1.x.

The `Debian package` workflow (`.github/workflows/debian-package.yml`) builds
it on every pull request in a `debian:testing` container: an orig tarball from
`git archive`, `apt-get build-dep`, a source and binary `dpkg-buildpackage`,
lintian, then installing the `.deb` and checking an `init.ts` (which runs the
packaged esbuild); the packages are kept as build artifacts. By hand, in a
`debian:testing` container:

```sh
apt-get update && apt-get install -y git dpkg-dev
git clone https://github.com/sinelaw/fresh && cd fresh
rm -rf debian && cp -r packaging/debian debian
apt-get build-dep -y ./
dpkg-buildpackage -us -uc -b
apt-get install -y ../fresh-editor_*.deb
```

Building the package the way Debian does found three more things, now
handled:

- **Build scripts and `--target`.** dh-cargo always passes `--target`, and
  cargo then compiles build scripts without the target's `RUSTFLAGS`, so
  `fresh-quickjs-sys`'s build script never saw `--cfg fresh_js_system`. Its
  build tools now hang off a `system` feature (which `fresh-js` turns on) and
  it reads the cfg at run time from `CARGO_CFG_FRESH_JS_SYSTEM`.
- **No LTO.** dh-cargo adds full debug info (for the `-dbgsym` package) and
  links with GNU ld; with upstream's fat LTO the debug info refers to statics
  LTO dropped, which ld.bfd rejects. The package builds with
  `profile.release.lto=false`.
- **Built-Using.** `dh-cargo-built-using` stops at any static library whose
  license it cannot place. `fresh-quickjs-sys`'s build script declares
  `libquickjs.a` (MIT, from `libquickjs`) and its own C shim (GPL-3+, built
  from this source package).

The resulting package depends on `libc6`, `libgcc-s1`, `libonig5` and
`esbuild`, lists QuickJS in `Static-Built-Using`, installs, and checks an
`init.ts` through the packaged esbuild; lintian reports only
`initial-upload-closes-no-bugs`, until there is an ITP bug.

What the first offline build found that the archive check could not: Debian's
`lsp-types` 0.97.0 is patched to use `fluent-uri` 0.4 instead of the 0.1
upstream uses, and `Uri::scheme()` returns `&Scheme` there instead of
`Option<&Scheme>`. Fresh's three callers now read the scheme from
`Uri::as_str()`, which both have. With that, the build resolves 310 crates,
all from Debian, and compiles. The resulting editor loads all 49 bundled
plugins with no errors, compiled by esbuild and run on Debian's QuickJS
(debug build, cold cache: 263 ms preparing and 314 ms running them; warm
cache: 73 ms preparing).

## What remains for a Debian package

- **Other architectures.** amd64 and arm64 are built in CI; the 32-bit ones
  and Debian's other release architectures (ppc64el, s390x, riscv64, …) are
  untested (see above).
- **The source package.** The plan is one source package that builds Fresh's
  workspace crates (`fresh-core`, `fresh-js`, …) from its own tree, rather
  than packaging each as a `librust-*-dev`; this needs agreeing with the
  Debian Rust team.
- **Process:** an upstream release with this work, an ITP bug, a sponsor, and
  an upload to unstable, from where it migrates to testing.

A sketch of the packaging (not in the upstream `debian/` directory, which
builds the upstream `.deb` with the default features):

```
Build-Depends: debhelper-compat (= 13), dh-cargo, cargo, rustc,
 libquickjs, esbuild, libclang-dev, pkg-config,
 librust-…-dev (the crates in Cargo.toml)
Depends: ${shlibs:Depends}, ${misc:Depends}, esbuild
Built-Using: ${cargo:Built-Using}, quickjs (= <version>)
```

```make
export RUSTFLAGS += --cfg fresh_js_system
export RUSTDOCFLAGS += --cfg fresh_js_system
override_dh_auto_build:
	cargo build --release -p fresh-editor --no-default-features \
	    --features runtime,plugins,embed-plugins
```

`Built-Using: quickjs` is needed because QuickJS is linked statically
(`libquickjs` ships only a static library). `esbuild` is a run-time
dependency because plugins are TypeScript and are compiled when they load;
it is needed at build time only for the tests.
