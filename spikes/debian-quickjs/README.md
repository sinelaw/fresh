# Debian QuickJS spike

Fresh's plugin runtime bound to the system QuickJS that Debian ships
(`libquickjs`) instead of rquickjs's bundled quickjs-ng. Findings and results
are in [`docs/internal/debian-quickjs-spike.md`](../../docs/internal/debian-quickjs-spike.md).

- `fresh-quickjs-sys/` — bindgen bindings + C shim over the installed `quickjs.h`
- `fresh-quickjs/` — safe wrapper, engine tests, and the `compat_system` runner
- `compat-ng/` — the same runner on quickjs-ng via rquickjs (today's engine)
- `compat/` — the shared stub host, built-in probe and comparison script

Run everything (fetches Debian's `.deb` when `libquickjs` is not installed):

```sh
./run-compat.sh
```

Or build against an installed or unpacked `libquickjs`:

```sh
QUICKJS_INCLUDE_DIR=/usr/include/quickjs \
QUICKJS_LIB_DIR=/usr/lib/x86_64-linux-gnu/quickjs \
cargo test -p fresh-quickjs
```
