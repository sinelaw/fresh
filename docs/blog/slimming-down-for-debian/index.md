---
title: "Slimming Fresh down for Debian"
date: 2026-11-02
description: "Debian builds every Rust program from crates already in its archive. Rather than package the crates Fresh needed and Debian lacked, we made Fresh stop needing them: a second JavaScript engine backend on Debian's own QuickJS, esbuild instead of oxc, and 84 fewer crates in the build."
outline: deep
---

# Slimming Fresh down for Debian

In [I hate packaging my software for Linux](../packaging-for-linux/) I explained why Fresh wasn't in Debian: an official package has to build every Rust dependency from Debian's own archive, and I didn't have time to package and maintain the crates Debian was missing.

Fresh now builds that way, and the result is in the Debian process ([ITP #1124035](https://bugs.debian.org/1124035)). We didn't add Fresh's missing crates to Debian. We changed Fresh so the Debian build doesn't need them. This post covers what that took, and what we found along the way.

## The rule

Debian doesn't build from crates.io or from your `Cargo.lock`. A package builds **offline**, against the `librust-*-dev` packages already in the archive. These unpack into `/usr/share/cargo/registry`, and cargo's `crates-io` source is redirected there. If a crate you depend on isn't in the archive, at a version your manifest accepts and with the features you ask for, the build fails. Vendoring isn't allowed.

The usual approach is to package each missing crate yourself, through the Debian Rust team's tooling. That takes care of the build, but then you keep those packages current for every release and every security fix. I didn't want to sign up for that, so we set a different goal:

> **Debian needs no new Rust packages.** Every dependency of the Debian build of Fresh must already be in Debian testing.

## What was missing

The first audit of Fresh's dependency graph against Debian testing found these problems:

| Missing or mismatched | What it does in Fresh |
| --- | --- |
| `rquickjs` (with its bundled quickjs-ng) | Runs every plugin. Fresh's plugin system sits on it. |
| `oxc` (about two dozen crates) | Compiles plugin TypeScript to JavaScript, in-process. |
| `ts-rs` | Generates `fresh.d.ts`, the plugin API's type definitions. |
| `tree-sitter` JavaScript, TypeScript and Templ grammars | Tree-sitter indentation for those languages. |
| `zip` 2 (self-update), `fresh-gui` (wgpu) | The self-updater and the experimental GUI front end. |
| `which`, `jsonc-parser`, `nix`, `libloading`, `ctor` | In Debian, but at older versions than Fresh asked for. |

Most of the smaller items were easy. QuickJS was the hard one.

## Plugins on Debian's QuickJS

Fresh's plugins are TypeScript, run by QuickJS through the `rquickjs` crate. `rquickjs` isn't in Debian, and you can't just point it at Debian's `libquickjs`. Debian ships Fabrice Bellard's original QuickJS, and `rquickjs` 0.11 is written against the **quickjs-ng** fork. The two headers have diverged:

- 9 functions `rquickjs` calls don't exist in Bellard's engine (the `JS_*Proxy*` family, `JS_SetPromiseHook`, and others).
- About 20 signatures differ. For example, `JS_NewClassID` takes no runtime, and `JS_SetOpaque` returns `void`.
- `rquickjs` hard-codes about 190 atom ids from quickjs-ng's internal atom table. Bellard's table is different, and Debian doesn't ship that header anyway.
- Bellard's engine has a rope-string tag (`JS_TAG_STRING_ROPE`) that `rquickjs` doesn't know about.

Most of these differences would compile and then fail at runtime.

### Step 1: one crate names the engine

Fresh's core and plugin runtime named `rquickjs` directly in about **520 places**. So the first step had no visible effect: a new crate, `fresh-js`, became the only one that names the engine. It re-exports an explicit list of `rquickjs` items: `Runtime`, `Ctx`, `Value`, `Object`, `Function`, `Persistent`, `FromJs`/`IntoJs`, the `#[class]`/`#[methods]` macros, serde conversion, and so on. Everything else in Fresh switched to `fresh_js::` paths. That export list became the contract a second backend would have to meet.

### Step 2: a second backend

Building with `RUSTFLAGS="--cfg fresh_js_system"` makes `fresh-js` use a second backend that Fresh implements itself:

- **`fresh-quickjs-sys`** runs bindgen over the installed `quickjs.h` and links Debian's static `libquickjs.a`. A small C shim exports the header's inline helpers and value macros as real functions, so the Rust code never encodes the `JSValue` layout, which is NaN-boxed on 32-bit targets.
- **`fresh-js`'s system backend** provides the same names and signatures as the `rquickjs` re-exports, with `rquickjs`'s memory model (refcounted handles, `'js` lifetimes), its conversion rules, and its error messages, which plugins and tests match on.
- **`fresh-js-macros`** reimplements `#[class]`, `#[methods]` and the `Trace`/`JsLifetime` derives. The 365-method `JsEditorApi` builds on either backend without any source change.

The switch is a `cfg`, not a cargo feature, on purpose. CI builds with `--all-features` on Linux, macOS and Windows, and a feature would have made every one of those jobs install `libquickjs`.

### Did it behave the same?

Before writing the backend, a spike ran **every bundled plugin on both engines** behind the same stub host. Each plugin loaded and then had every command and event handler it registered called:

| | Debian's QuickJS | quickjs-ng (rquickjs) |
| --- | --- | --- |
| plugins loaded | 49/49 | 49/49 |
| host calls | 2373 | 2373 |
| handler outcomes (ok / waiting / undefined) | 351 / 50 / 2 | 351 / 50 / 2 |
| plugins with any difference | — | **0** |

quickjs-ng has 38 built-ins Bellard's engine lacks, mostly ES2024/2025 additions such as iterator helpers, the new `Set` methods and `Promise.try`. A search of the compiled plugin JavaScript found that no plugin uses any of them. User plugins are already type-checked against `ES2020`, so TypeScript flags those built-ins for plugin authors.

There's now a parity test suite that runs against whichever backend is built, and a CI job runs it, the plugin runtime's tests, and the editor's plugin end-to-end tests in a `debian:testing` container.

One bonus: Debian's `libquickjs` keeps QuickJS's teardown assertion, so a leaked reference aborts the test run. That assertion caught a real refcount bug while the backend was being written, and the spike's wrapper turned out to have the same bug.

## TypeScript without oxc

oxc, about two dozen crates, compiles plugin TypeScript in-process. It isn't in Debian. **esbuild** is, and it's a single Go binary that depends only on `libc6`.

`fresh-parser-js` now has an `oxc` cargo feature, on by default. Without it, transpiling, bundling, import stripping and syntax checks run the system `esbuild`, with results cached under `~/.cache/fresh/esbuild`.

That made esbuild a runtime dependency, and esbuild Recommends Node.js. A default `apt install fresh-editor` would have pulled in **about 100 packages**. So we went one step further:

- A **`precompiled-plugins`** feature compiles the bundled plugins while Fresh builds, and embeds the JavaScript, keyed by a hash of the source. At startup, a bundled plugin whose source still matches uses that JavaScript, and nothing is compiled.
- esbuild becomes a build dependency, and the binary package only **Suggests** it, for your own TypeScript (`init.ts`, your plugins). Without it, Fresh tells you to `apt install esbuild`.

A default install now adds `fresh-editor` and `libonig5`, plus `git`, which it recommends. Plugin preparation at startup dropped from **about 200 ms to about 50 ms**, because nothing is compiled.

## The smaller cuts

- **`ts-rs` is optional.** Its derive only feeds the `fresh.d.ts` generator, a development task. Without it, `#[derive(TS)]` is a no-op derive that accepts the same attributes.
- **Version ranges widened, not pinned.** `nix >=0.30, <0.32` and `libloading >=0.8, <0.10` accept Debian's older versions, and upstream builds still resolve to the newer ones. `which` and `jsonc-parser` moved to the versions Debian has. The test harness's `ctor` accepts 1.x.
- **No `tree-sitter` feature.** Its JavaScript, TypeScript and Templ grammars aren't in Debian, so the Debian build turns the feature off. Indentation falls back to Fresh's regex rules, and syntax highlighting comes from syntect, as it does for every other language.
- **No `http`, by choice.** Debian has everything it needs, but it carries the update checker and the anonymous telemetry, and a distro package shouldn't phone home or update itself. The plugin package manager uses `git`, so it still works.
- **No `fresh-gui`.** Its wgpu renderer isn't packaged, and only the optional GUI uses it.

## How much smaller

The same `fresh-editor` crate, with the default features and with the Debian ones (`runtime,plugins,embed-plugins,precompiled-plugins` on the system QuickJS backend), counting the distinct crates in its normal and build dependencies on x86_64 Linux:

| | default build | Debian build |
| --- | --- | --- |
| distinct crates | 325 | **241** |
| `.deb` for amd64 (0.5.7) | 10.9 MB | **9.4 MB** |
| runtime Depends | — | `libc6`, `libgcc-s1`, `libonig5` |

The Debian build drops 91 crates: the five `rquickjs` crates, 23 `oxc_*` crates and their helpers, `ureq` with `rustls`/`ring`/`webpki-roots`, `zip`/`tar`/`xz2`, `ts-rs`, five tree-sitter grammars, and more. It adds 7: `bindgen` and its helpers, `fresh-quickjs-sys` and `fresh-js-macros`. That's 84 fewer crates to audit, and each one that remains already has a Debian maintainer.

## Keeping it true

A promise like "Debian needs no new Rust packages" only lasts as long as something checks it, so CI does:

- **`scripts/debian.py check`** walks the Debian build's dependency graph against a Debian `Packages` index. For every dependency, it checks that the archive has the crate at a matching version with the requested features. CI runs it against testing's live index. If a pull request adds a crate Debian doesn't have, CI fails.
- **`scripts/debian.py patch-manifests`** rewrites the manifests so cargo can resolve the whole workspace from `/usr/share/cargo/registry`. Cargo resolves optional, dev-only and other-target dependencies too, so the dependencies Debian lacks have to come out of the manifests. The script prints each dependency it drops and why.
- **The `Debian package` workflow** builds the package the way a Debian build server would, on every pull request. It runs on `debian:testing` for amd64 and arm64 and on `unstable` for amd64, from an orig tarball, offline, with Debian's `cargo` and `rustc`. It runs lintian and the package's autopkgtests, which load every bundled plugin without esbuild installed.
- **Every release since 0.5.4** attaches the result as `fresh-editor_<ver>-1_<arch>.debian-testing.deb`. The updater and the install script never pick these files.

## What building it the Debian way found

The archive check couldn't catch everything. Actually building the package found more:

- **Debian patches crates.** Debian's `lsp-types` 0.97.0 is patched to use `fluent-uri` 0.4, where `Uri::scheme()` returns `&Scheme` instead of `Option<&Scheme>`. Fresh now reads the scheme from `Uri::as_str()`, which both versions have.
- **Build scripts don't see `RUSTFLAGS` under `--target`.** dh-cargo always passes `--target`, and cargo then compiles build scripts without the target's `RUSTFLAGS`, so the `--cfg` never reached `fresh-quickjs-sys`'s `build.rs`. The script now reads `CARGO_CFG_FRESH_JS_SYSTEM` when it runs.
- **Fat LTO plus full debug info breaks GNU ld.** The debug info for the `-dbgsym` package referred to statics that LTO had dropped. The Debian build turns LTO off. Upstream release builds are unchanged.
- **32-bit rustc runs out of address space.** Salsa's i386 build hit `rustc-LLVM ERROR: out of memory` on the editor crate. On 32-bit targets, the Debian build now keeps line tables only and uses 16 codegen units.
- **Reproducibility.** A rebuilt `.deb` differed only in a timestamp in the install receipt. That timestamp now comes from `SOURCE_DATE_EPOCH`.
- **Licensing.** `debian/copyright` was audited against the whole tree. That found that the Typst grammar derives from Typst's Apache-2.0 TextMate grammar, so it now says so. It also found that the documentation site's footer said "Apache 2.0", when Fresh is GPL-3.0-or-later.

## Where it stands

- The ITP is [#1124035](https://bugs.debian.org/1124035), and the source package has been uploaded to mentors.debian.net.
- The packaging lives on Salsa at [`salsa.debian.org/sinelaw/fresh-editor`](https://salsa.debian.org/sinelaw/fresh-editor), and Salsa CI builds it, runs lintian and autopkgtests, and checks reproducibility.
- **What's left:** a sponsor to upload it to unstable, and agreement with the Debian Rust team on how the source package carries Fresh's own workspace crates (`fresh-core`, `fresh-js`, …).

Unstable keeps moving under us. While I was writing this, Debian unstable replaced `jsonc-parser` 0.33 with 0.34, so Fresh now accepts both. That's what the CI job on unstable is for.

If you run Debian testing or unstable, you can try the package now. Download `fresh-editor_<version>-1_<arch>.debian-testing.deb` from the [latest release](https://github.com/sinelaw/fresh/releases/latest) and install it with `apt install ./fresh-editor_*.debian-testing.deb`. If you're a Debian Developer who sponsors Rust packages, I'd love to hear from you.
