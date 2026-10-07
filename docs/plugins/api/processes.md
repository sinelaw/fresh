<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Processes, Timers & Network

Run programs, wait or repeat on a timer, and fetch over HTTP.

::: v-pre

## Processes

### `isProcessRunning`

Check if a background process is still running: true from
`spawnBackgroundProcess` until its result promise settles or it is
killed.

```typescript
isProcessRunning(processId: number): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `processId` | ID returned from spawnBackgroundProcess |

### `killProcess`

Kill a process by ID (alias for killBackgroundProcess)

Forcibly terminates the process (SIGKILL on Unix). Its
`spawnBackgroundProcess` promise then settles with `exit_code` -1. Returns
true once the kill request has been sent.

```typescript
killProcess(processId: number): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `processId` | `processId` of a spawnBackgroundProcess handle |

### `spawnProcess`

Spawn a process (async, returns request_id)

**No shell is involved.** `command` is executed directly, so quoting,
globbing, `|`, `&&`, `>` and `$VAR` are not interpreted — pass the
program and its arguments already split:

```js
await editor.spawnProcess("gh", ["api", "graphql", "-f", query], repoDir);
```

Wrapping the call in `/bin/sh -lc "…"` to get shell behaviour is
usually a mistake: a login shell sources the user's profile, which is
slow and can block outright.

The child inherits the **editor's** environment, including `PATH`, so
`git`, `gh` and anything else the user can run from their shell
resolves by bare name — no absolute paths needed.

`cwd` is the third argument; without it the child inherits the
editor's working directory, which is not necessarily the workspace you
meant.

Optional 4th argument `stdoutTo: string` pipes the child's stdout
directly into the named file instead of buffering it. The
resolved `SpawnResult.stdout` is empty in that case; the bytes
land on disk for `openFile` to pick up as a file-backed buffer.

```typescript
spawnProcess(command: string, args: string[], cwd?: string, stdoutTo?: string): ProcessHandle<SpawnResult>;
```

### `spawnHostProcess`

Spawn a process on the host regardless of the active authority.

Intended for plugin internals that must run host-side work
(e.g. `devcontainer up`) before installing an authority that
would otherwise route the spawn elsewhere. Same calling shape
as `spawnProcess`.

```typescript
spawnHostProcess(command: string, args: string[], cwd?: string): ProcessHandle<SpawnResult>;
```

### `spawnProcessWait`

Wait for a process to complete and get its result (async)

```typescript
spawnProcessWait(processId: number): Promise<SpawnResult>;
```

### `spawnBackgroundProcess`

Spawn a background process (async, returns request_id which is also process_id)

Unlike `spawnProcess`, which waits for completion, this starts a process
in the background and returns immediately with a handle. Its
`processId` identifies the process; it is the same id that
`onProcessStdout` / `onProcessStderr` payloads carry. The handle is also
a promise that settles with the `BackgroundProcessResult` once the
process exits. Use `handle.kill()` or `killProcess(id)` to terminate the
process later; the promise then settles with `exit_code` -1. Use
`isProcessRunning(id)` to check if it is still running.

```ts
const proc = editor.spawnBackgroundProcess("asciinema", ["rec", "output.cast"]);
// Later...
if (editor.isProcessRunning(proc.processId)) {
  await proc.kill(); // or editor.killProcess(proc.processId)
}
```

```typescript
spawnBackgroundProcess(command: string, args: string[], cwd?: string): ProcessHandle<BackgroundProcessResult>;
```

| Parameter | Description |
|-----------|-------------|
| `command` | Program name (searched in PATH) or absolute path |
| `args` | Command arguments (each array element is one argument) |
| `cwd` | Working directory; omit it to use the editor's working directory |

### `killBackgroundProcess`

Kill a background process

```typescript
killBackgroundProcess(processId: number): boolean;
```

## Timers

### `setInterval`

Run `handlerName` every `intervalMs` milliseconds until cancelled.
Returns a timer id for `clearInterval`.

The alternative — a detached `while (alive) { await editor.delay(ms);
… }` loop — does work, and the bundled dashboard plugin uses one. But
it puts three obligations on you that a timer discharges for free:

1. **A throw anywhere in the loop body ends it, silently.** The loop
   is a detached async function, so the rejection has nowhere to
   surface; the panel simply stops updating, with nothing in the log
   pointing at why. Every `await` inside must be individually
   guarded. A timer handler's throw is caught and logged by the host,
   and the *next* tick still fires.
2. **You must cancel it yourself.** A loop keeps running after its
   plugin is unloaded or reloaded until its own guard notices, so it
   needs a liveness check that survives a reload — an identity check
   (`myBufferId === currentBufferId`), not a boolean, or a reopened
   panel ends up with two loops. Timers are cancelled on unload.
3. **The first iteration is one period late** unless you also do the
   work once before entering the loop.

A loop is still the better shape when each iteration's decision
depends on the last one's result, or when you want a single ticker
driving many items on their own schedules (again: see dashboard.ts,
which ticks at 1s and re-runs a section only once its own TTL has
expired, so cost scales with the sum of the sections' rates rather
than tick-rate × section-count).

The handler is named, not passed as a function, for the same reason
`registerCommand` takes a name: the host invokes it by looking it up
on `globalThis`. Declare it with `registerHandler("myTick", fn)`.

The handler may be `async`; a fire is not awaited, and a slow handler
does not delay the editor. Ticks are *not* queued — if a fire is
still outstanding when the next is due, the next simply happens, so
guard re-entrancy yourself (`if (inFlight) return;`) when a tick can
outlast its period.

Timers are cancelled automatically when the owning plugin is unloaded
or reloaded, so a hot-reload during development does not leave the
previous copy ticking alongside the new one.

`intervalMs` is clamped to a floor (see `MIN_PLUGIN_TIMER_MS` in the
host) so a `0` cannot spin the editor.

```typescript
setInterval(intervalMs: number, handlerName: string): number;
```

### `setTimeout`

Run `handlerName` once, `delayMs` from now. Returns a timer id, so a
pending one-shot can still be cancelled with `clearInterval`.

Same contract as `setInterval` — named handler, host-driven, cancelled
on plugin unload. Use it when the continuation should happen whether
or not the code that scheduled it is still around; use
`await editor.delay(ms)` when you are pausing work you are already
inside of and want to keep the local variables.

```typescript
setTimeout(delayMs: number, handlerName: string): number;
```

### `clearInterval`

Cancel a timer from `setInterval` / `setTimeout`.

Returns `false` when this plugin holds no live timer under that id —
which covers a typo, a double-cancel, and a one-shot that has already
fired. None of those is an error, so none throws.

Only your own timers are cancellable: ids come from a counter shared
across plugins, so accepting an arbitrary id would let one plugin stop
another's refresh.

```typescript
clearInterval(timerId: number): boolean;
```

### `delay`

Delay/sleep (async, returns request_id)

Resolves after `durationMs`. Two things it is very good at:

- a pause inside work you are already inside of — a debounce, a retry
  backoff, a settle before reading state back;
- a **timeout**, by racing it against the real work:
  ```js
  const timedOut = Symbol("timeout");
  const outcome = await Promise.race([
      doTheWork().then(() => "ok"),
      editor.delay(8000).then(() => timedOut),
  ]);
  ```
  which is how the bundled dashboard stops one slow section from
  stalling the panel.

For a *periodic background* task, weigh it against
`editor.setInterval(ms, "handlerName")`. A detached
`while (…) { await editor.delay(ms); … }` loop works, but it dies
silently on the first unguarded throw, is not cancelled when the
plugin unloads, and does its first iteration one period late — see
`setInterval` for when each shape is the right one.

Note the loop keeps running after the plugin that created it is
unloaded or reloaded, until its own guard notices. Gate it on an
identity (`myBufferId === currentBufferId`) rather than a boolean, or
reloading leaves two loops racing.

```typescript
delay(durationMs: number): Promise<void>;
```

| Parameter | Description |
|-----------|-------------|
| `durationMs` | Number of milliseconds to delay |

## Network

### `httpFetch`

Fetch a URL over HTTP(S) and stream the response body into `target_path`.

Resolves with a `SpawnResult`-shaped value: `exit_code` is `0` on a
2xx response (file written), the HTTP status code on non-2xx
(target file untouched), and `-1` on transport errors. `stderr`
carries an error message in the non-success cases; `stdout` is
always empty.

This uses the editor's built-in HTTP client (`ureq`), so plugins
don't need `curl`/`wget` on PATH.

`headers` are extra request headers (`{ "Authorization": "Bearer …" }`).
They travel in the request only: never logged, never on a command line,
which is why a credential goes here rather than through `curl -H`.
Non-string values are ignored.

```typescript
httpFetch(url: string, targetPath: string, headers?: Record<string, string> | null): ProcessHandle<SpawnResult>;
```

:::
