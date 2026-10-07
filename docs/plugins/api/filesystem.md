<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Files, Paths & Environment

Read and write files, work with paths, watch for changes, read the environment, store plugin data, and reach other machines.

::: v-pre

## Files

### `fileExists`

Check if a file exists on the path's filesystem (a window's authority,
or the local host for a `LocalPath`).

The path may be a file or a directory. Use `fileStat` for more detailed
information.

```typescript
fileExists(path: string | LocalPath | WindowPath | AuthorityPath): boolean;
```

### `readFile`

Read file contents from the path's filesystem.

The file is read as a UTF-8 string. Returns `null` if the file does not
exist, cannot be read, or is not valid UTF-8, so binary files cannot be
read this way.

```typescript
readFile(path: string | LocalPath | WindowPath | AuthorityPath): string | null;
```

### `writeFile`

Write file contents to a NEW file on the path's filesystem. Parent
directories are created as needed. Returns false if the path already
exists — use `replaceFile` to replace a file deliberately.

```typescript
writeFile(path: string | LocalPath | WindowPath | AuthorityPath, content: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `content` | UTF-8 string to write |

### `replaceFile`

Write to a file, replacing it if it already exists.

`writeFile` refuses an existing path, which is what its documentation
always promised and what stops a plugin destroying a user's file by
accident. Use this when replacing the file is the actual intent — a
plugin rewriting its own cache or state, or re-exporting a report the
user asked for again. The write is atomic: the content goes to a temp
file which is renamed over the destination, so a reader sees either the
old file or the new one, never a partial one.

```typescript
replaceFile(path: string | LocalPath | WindowPath | AuthorityPath, content: string): boolean;
```

### `readDir`

Read directory contents (returns array of &#123;name, is_file, is_dir})

Entries are in no particular order. Entry names are relative to the
directory; use `pathJoin` to build full paths. Returns an empty array if
the path cannot be read, for example when it is not a directory or
permission is denied.

```ts
const entries = editor.readDir("/home/user");
for (const e of entries) {
  const fullPath = editor.pathJoin("/home/user", e.name);
}
```

```typescript
readDir(path: string | LocalPath | WindowPath | AuthorityPath): DirEntry[];
```

### `createDir`

Create a directory (and all parent directories) recursively on the
path's filesystem. Returns true if the directory was created or already
exists.

```typescript
createDir(path: string | LocalPath | WindowPath | AuthorityPath): boolean;
```

### `fileStat`

Get file stat information

Follows symlinks. Returns `null` for a path that does not exist rather than
throwing. Otherwise returns `{ isFile, isDir, size, readonly }`, with `size`
in bytes.

```typescript
fileStat(path: string | LocalPath | WindowPath | AuthorityPath): unknown;
```

## Paths

### `pathJoin`

Join path components (variadic - accepts multiple string arguments)
Always uses forward slashes for cross-platform consistency (like Node.js path.posix.join)
Empty segments are skipped. If a segment is absolute, earlier segments are
discarded.

Preserves up to 2 leading slashes, which matters on Windows: paths the
editor hands to plugins, such as `editor.getCwd()`, can carry the
`\\?\` prefix (`\\?\C:\...`). After the backslash→slash
normalization the prefix becomes `//?/C:/...`; collapsing the leading
`//` to a single `/` yields `/?/C:/...`, which every filesystem API on
Windows rejects, breaking `findConfig()`-style plugin logic.

```ts
editor.pathJoin("/home", "user", "file.txt"); // "/home/user/file.txt"
editor.pathJoin("relative", "/absolute"); // "/absolute"
```

```typescript
pathJoin(...parts: string[]): string;
```

### `pathDirname`

Get directory name from path.

Returns the parent directory, or an empty string for a root path or a path
with no parent. Does not resolve symlinks or check that the path exists.

```ts
editor.pathDirname("/home/user/file.txt"); // "/home/user"
editor.pathDirname("/"); // ""
```

```typescript
pathDirname(path: string): string;
```

### `pathBasename`

Get file name from path.

Returns the final component of the path, or an empty string for a root path.
Does not strip the file extension; use `pathExtname` for that.

```ts
editor.pathBasename("/home/user/file.txt"); // "file.txt"
editor.pathBasename("/home/user/"); // "user"
```

```typescript
pathBasename(path: string): string;
```

### `pathExtname`

Get file extension.

Returns the extension including the dot, or an empty string if there is
none. Only the last extension is returned, so "archive.tar.gz" gives ".gz".

```ts
editor.pathExtname("file.txt"); // ".txt"
editor.pathExtname("archive.tar.gz"); // ".gz"
editor.pathExtname("Makefile"); // ""
```

```typescript
pathExtname(path: string): string;
```

### `pathIsAbsolute`

Check if path is absolute.

On Unix, a path is absolute if it starts with "/". On Windows, it must start
with a drive letter and separator (such as `C:\`) or be a UNC path.

```typescript
pathIsAbsolute(path: string): boolean;
```

### `fileUriToPath`

Convert a file:// URI to a local file path.
Handles percent-decoding and Windows drive letters.
Returns an empty string if the URI is not a valid file URI.

```typescript
fileUriToPath(uri: string): string;
```

### `pathToFileUri`

Convert a local file path to a file:// URI.
Handles Windows drive letters and special characters.
Returns an empty string if the path cannot be converted.

```typescript
pathToFileUri(path: string): string;
```

### `localPath`

Construct a `LocalPath` — a path that always resolves on the local
editor host, regardless of the active window's authority. Use for
editor-owned state under the config/data dirs.

```typescript
localPath(path: string): LocalPath;
```

### `windowPath`

Construct a `WindowPath` — a path that resolves on a specific window's
authority filesystem, regardless of which window is focused.

```typescript
windowPath(windowId: number, path: string): WindowPath;
```

### `authorityPath`

Construct an `AuthorityPath` — the active window's authority filesystem,
stated explicitly. Routes identically to passing a bare string, but is
self-documenting: bundled plugins use it so bare strings are reserved as
the backward-compatible default for external plugins.

```typescript
authorityPath(path: string): AuthorityPath;
```

## File Watching

### `watchPath`

Register a watch on `path`. Returns a
promise that resolves to a numeric `handle` (also passed
in subsequent `path_changed` event payloads). The promise
rejects when the watch cannot be set up (path missing, kernel limit).

Each change fires a `path_changed` hook with the handle, the changed
path and the change kind. Release the watch with
`unwatchPath(handle)`.

`recursive` defaults to `false`. Non-recursive watches
cover the path itself plus its direct children for
directories.

```typescript
watchPath(path: string, recursive?: boolean): Promise<number>;
```

### `unwatchPath`

Drop a watcher by its handle. Unknown handles are
silently ignored.

```typescript
unwatchPath(handle: number): boolean;
```

## Environment

### `getEnv`

Get an environment variable

```typescript
getEnv(name: string): string | null;
```

### `getCwd`

Get current working directory

Use it as the base for resolving relative paths.

```typescript
getCwd(): string;
```

### `workspaceTrustLevel`

Current Workspace Trust level for the active project: `"restricted"`,
`"trusted"`, or `"blocked"` (empty when unavailable). Plugins that run
repo-controlled work
should treat anything other than `"trusted"` as "do not execute".

```typescript
workspaceTrustLevel(): string;
```

### `envActive`

Whether an environment is currently active (set via `editor.setEnv`).
Lets the env-manager plugin
reflect activation and re-establish its file watch after the restart
that `setEnv` triggers.

```typescript
envActive(): boolean;
```

### `detectedEnv`

The environment the editor detected in the workspace, as a JSON string
(`{name, kind, snippet}`) or empty when none. Detection lives only in
the editor; the env-manager
plugin consumes this result instead of probing the filesystem itself.

```typescript
detectedEnv(): string;
```

### `setEnv`

Activate an environment: set the live env recipe (`snippet` run in
`dir`). Applied to every spawn, re-evaluated on demand — no restart.
Honored only when the workspace is Trusted.

```typescript
setEnv(snippet: string, dir: string | null): void;
```

### `clearEnv`

Deactivate the environment — spawns return to the inherited env.

```typescript
clearEnv(): void;
```

## Plugin Storage

Plugins can't delete, move or overwrite a path they name. There is no
`removePath`, `renamePath` or `copyPath`. The calls below name a *thing*
instead (a staging directory the editor issued, a package, a state entry),
and the editor works out the path itself.

This closes real holes. `removePath` once checked only its top-level
argument, so a symlink inside the target let a recursive delete escape.
`renamePath` had no check and fell back to copy-then-delete, so anything
`removePath` refused could be moved elsewhere and deleted there. Now nothing a
plugin passes decides what gets removed.

Removals a user would notice (replacing or uninstalling a package, deleting a
theme) go to the system trash, so they can be recovered. Staging directories
are the editor's own working space and are deleted outright.

### `scratchCreate`

Create an editor-owned staging directory and return the opaque token
that names it. Write into it with the path `scratchPath` returns, then
either publish it with `installScratch` or drop it with
`scratchDiscard`. `label` only makes the directory recognisable to a
human; it does not decide where the directory goes.

```typescript
scratchCreate(label: string): string | null;
```

### `scratchPath`

The directory a staging token names, or `null` if the token is unknown
or already spent.

```typescript
scratchPath(token: string): string | null;
```

### `scratchDiscard`

Discard a staging directory. The path is looked up from the token, so
an unknown, forged or already-spent token removes nothing.

```typescript
scratchDiscard(token: string): boolean;
```

### `installScratch`

Publish a staging directory as the installed package `<kind>/<name>`,
where `kind` is one of `plugin`, `theme`, `language` or `bundle`. Any
existing install under that name is moved aside, the new package is put
in its place, and only then does the old one go to the system trash, so an
upgrade is recoverable. If the new package cannot be put in place, the old
install is put back, so a failed upgrade never leaves the user without a
package.

`subpath` installs one directory out of the staging tree (a package in
a subdirectory of a cloned monorepo); pass `""` for the whole thing. It
chooses the source only — `kind` and `name` decide where the package
lands. Installing the whole tree spends the token; installing a subpath
leaves it live so the rest can be discarded.

```typescript
installScratch(token: string, kind: string, name: string, subpath: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `name` | Package name; must be a single path component |

### `scratchFromDirectory`

Create a staging directory holding a copy of `from`, and return the
token that names it — how a package installed from a local directory
reaches staging.

`from` is a path on the editor host. Staging directories, installed
packages and plugin state all live there by design, so an install
survives the SSH session that started it going away; there is no
authority-path form of this call, so the argument is a plain path
rather than a `LocalPath | WindowPath | AuthorityPath` union with two
thirds of it rejected at runtime.

The copy can only land in the new staging directory, never on anything
else. Symlinks are recreated as symlinks rather than followed; where the
platform does not allow creating one, the target's contents are copied.

Answers `null` if `from` is not a directory or could not be copied,
having discarded anything it had already staged — so there is never a
half-filled staging directory to clean up.

```typescript
scratchFromDirectory(from: string): string | null;
```

### `uninstallPackage`

Move an installed package to the system trash. Returns false if nothing
is installed under that kind and name.

```typescript
uninstallPackage(kind: string, name: string): boolean;
```

### `stateSet`

Write a namespaced state entry, replacing any previous value. The
editor owns the on-disk layout; a plugin names the entry, not the file.

Writes are atomic. `namespace` and `key` must each be a single path
component, and a key may not be empty or start with a dot; otherwise
nothing is written and false is returned. Use this instead of
hand-rolling a temp-file-and-rename dance in a directory of your own.

```typescript
stateSet(namespace: string, key: string, value: string): boolean;
```

### `stateGet`

Read a namespaced state entry, or `null` if it is unset.

```typescript
stateGet(namespace: string, key: string): string | null;
```

### `stateKeys`

The keys set in a namespace, in no particular order.

```typescript
stateKeys(namespace: string): string[];
```

### `stateDelete`

Clear a namespaced state entry. Returns true if it is gone afterwards,
including when it was already unset.

```typescript
stateDelete(namespace: string, key: string): boolean;
```

### `setGlobalState`

Set plugin-managed global state.
State is automatically isolated per plugin using the plugin's name.
`getGlobalState` returns the new value straight away; `null` or
`undefined` deletes the key. The state is saved to disk as soon as the
editor applies the change, so it survives restarts.

```typescript
setGlobalState(key: string, value: unknown): boolean;
```

### `getGlobalState`

Get plugin-managed global state, as set by `setGlobalState`.
`undefined` if missing.
State is automatically isolated per plugin using the plugin's name.

```typescript
getGlobalState(key: string): unknown;
```

### `setWindowState`

Set per-session state on the **active** session. Same
shape as `setGlobalState` (`getWindowState` returns the new value
straight away; null/undefined deletes), but the state belongs to
the active session and swaps with the rest of session state on
`setActiveWindow`.
Plugins that genuinely want per-project state use this;
Orchestrator itself uses `setGlobalState` because its session
list lives above session boundaries.

The state is per plugin. It follows the session across saves and
restores instead of applying globally.

```typescript
setWindowState(key: string, value: unknown): boolean;
```

### `getWindowState`

Get per-session state from the **active** session.
`undefined` if missing.

The state is per plugin, as written by `setWindowState`.

```typescript
getWindowState(key: string): unknown;
```

## Remote & Authority

### `getAuthorityLabel`

Get the active authority's display label.

Empty means the local (default) authority. A non-empty value
means a plugin-installed or SSH authority is in effect (e.g.
`"Container:abc123def456"` for a devcontainer). Intended as a
simple "am I already attached?" check that survives editor
restarts: after the restart the editor goes through when the
authority changes, it already reports the new label.

```typescript
getAuthorityLabel(): string;
```

### `setAuthority`

Install a new authority via an opaque payload.

The payload is a JS object describing filesystem + spawner +
terminal wrapper + display label. The canonical schema lives in
the `AuthorityPayload` type in `fresh-editor`; plugins should
hand-build objects that match it. Fire-and-forget: returns before the
authority is live and reloads nothing, so follow-up work belongs in an
`authority_changed` handler.

```typescript
setAuthority(payload: AuthorityPayload): boolean;
```

### `clearAuthority`

Restore the default local authority on this window. Same semantics as
`setAuthority`.

```typescript
clearAuthority(): void;
```

### `attachRemoteAgent`

Attach to a remote agent that needs a live connection (an SSH host or a
`kubectl exec` agent in a Kubernetes pod). The connect is asynchronous —
the editor spawns the carrier, bootstraps the agent and builds the
session in the background — and this returns a promise that settles on
the real outcome:

  * resolves once the session (authority + window) is fully
    constructed, so a caller can keep its dialog open until there is a
    real session to show;
  * rejects with the failure reason (e.g. ssh "Could not resolve
    hostname") if the connect or window creation fails — in which case
    no window is created and the editor stays on its current authority.

The payload schema (`RemoteAgentSpec`) lives in `fresh-editor`;
plugins hand-build an object matching it.

```typescript
attachRemoteAgent(payload: RemoteAgentSpec): Promise<void>;
```

### `cancelRemoteAgent`

Cancel any in-flight `attachRemoteAgent` connect — the New-Session
dialog's Cancel. The pending promise rejects with "cancelled" and the
background connect's late result is discarded, so no window is built.
A no-op when nothing is connecting.

```typescript
cancelRemoteAgent(): void;
```

### `setRemoteIndicatorState`

Override the Remote Indicator's displayed state. Plugins call
this to surface lifecycle transitions that the authority layer
doesn't know about yet — "Connecting" while `devcontainer up`
runs, "FailedAttach" after a non-zero exit, etc.

Accepts a tagged JS object:
```ts
editor.setRemoteIndicatorState({ kind: "connecting", label: "Building" });
editor.setRemoteIndicatorState({ kind: "failed_attach", error: "exit 1" });
editor.setRemoteIndicatorState({ kind: "connected", label: "Container:abc" });
editor.setRemoteIndicatorState({ kind: "local" });
```

The override sticks until replaced or cleared via
`clearRemoteIndicatorState`. It survives an authority change but not a
relaunch.

```typescript
setRemoteIndicatorState(state: RemoteIndicatorStatePayload): boolean;
```

### `clearRemoteIndicatorState`

Drop any active Remote Indicator override. Safe to call even
without a prior `setRemoteIndicatorState`.

```typescript
clearRemoteIndicatorState(): void;
```

## Machines

### `openMachine`

Open a machine to read without attaching it to a window.
 `{ kind: "window", window?: number }` borrows a window's own authority.
 `{ kind: "ssh" | "kubectl-exec", ... }` connects to a machine nothing is
 attached to; it is read-only, so `run` rejects. Anything else is an
 `AuthorityPayload`, as `setAuthority` takes.

```typescript
openMachine(spec: {
  kind: "window";
  window?: number;
} | RemoteAgentTransport | AuthorityPayload): Promise<FreshMachine>;
```

### `walkTree`

```typescript
walkTree(root: string, options?: WalkTreeOptions): Promise<WalkTreeResult>;
```

### `readFilePrefixes`

```typescript
readFilePrefixes(requests: {
  path: string;
  maxBytes: number;
}[]): Promise<FilePrefix[]>;
```

### `runOnTarget`

Unlike `spawnHostProcess`, a remote authority runs the command there.

```typescript
runOnTarget(program: string, args?: string[], cwd?: string): Promise<CommandResult>;
```

### `machineEnv`

```typescript
machineEnv(names: string[]): Promise<Record<string, string>>;
```

:::
