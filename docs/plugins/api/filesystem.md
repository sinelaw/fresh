# Filesystem, Path, and Environment API

## File System Operations

### `readFile`

Read entire file contents as UTF-8 string
Throws if file doesn't exist, isn't readable, or isn't valid UTF-8.
For binary files, this will fail. For large files, consider memory usage.

```typescript
readFile(path: string): Promise<string>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | File path (absolute or relative to cwd) |

#### `writeFile`

Write string content to a NEW file. Fails if the path already exists.

This page always said so — "fails if the file already exists to prevent
plugins from accidentally overwriting user data" — but the implementation did
the opposite, writing a temp file and renaming it over whatever was there. A
plugin that trusted the documentation destroyed the user's file. It now
behaves as documented, and replacing a file has to be asked for by name.

```typescript
writeFile(path: string, content: string): Promise<void>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | Destination path (absolute or relative to cwd) |
| `content` | `string` | UTF-8 string to write |

#### `replaceFile`

Write string content to a file, replacing it if it already exists.

```typescript
replaceFile(path: string, content: string): boolean
```

Use this when replacing the file is the actual intent — rewriting your own
cache or state, or re-exporting a report the user asked for again — and
`writeFile` when the file is meant to be new. The write is atomic: the content
goes to a temp file which is renamed over the destination, so a reader sees
either the old file or the new one, never a partial one.

#### `fileExists`

Check if a path exists (file, directory, or symlink)
Does not follow symlinks; returns true for broken symlinks.
Use fileStat for more detailed information.

```typescript
fileExists(path: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | Path to check (absolute or relative to cwd) |

#### `fileStat`

Get metadata about a file or directory
Follows symlinks. Returns exists=false for non-existent paths
rather than throwing. Size is in bytes; directories may report 0.

```typescript
fileStat(path: string): FileStat
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | Path to stat (absolute or relative to cwd) |

#### `readDir`

List directory contents
Returns unsorted entries with type info. Entry names are relative
to the directory (use pathJoin to construct full paths).
Throws on permission errors or if path is not a directory.

```typescript
readDir(path: string): DirEntry[]
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | Directory path (absolute or relative to cwd) |

**Example:**

```typescript
const entries = editor.readDir("/home/user");
for (const e of entries) {
const fullPath = editor.pathJoin("/home/user", e.name);
}
```

### Staging, Packages, and State

Plugins cannot delete, move, or overwrite a path they name. There is no
`removePath`, `renamePath` or `copyPath`; the operations below name a *thing*
— a staging directory the editor issued, a package, a state entry — and the
editor resolves that to a path itself.

This is not paperwork. `removePath` used to check that its target sat under
the temp or config directory, but only its top-level argument: a symlink
inside the target walked its recursive delete straight back out of the fence.
`renamePath` had no fence at all and fell back to copy-then-delete, so
anything `removePath` refused could be moved somewhere it allowed and deleted
from there. Nothing a plugin passes now decides what gets removed.

Removals a user would notice — replacing or uninstalling a package, deleting a
theme — go to the system trash, so they are recoverable. Staging directories
are the editor's own working space and are unlinked outright.

#### `scratchCreate` / `scratchPath` / `scratchDiscard`

Ask for a staging directory, find out where it is, and give it back.

```typescript
scratchCreate(label: string): string | null   // returns an opaque token
scratchPath(token: string): string | null     // the directory to write into
scratchDiscard(token: string): boolean        // remove it
```

`label` only makes the directory recognisable to a human; it does not decide
where the directory goes. `scratchDiscard` looks the path up from the token,
so an unknown, forged, or already-spent token removes nothing.

#### `scratchFromDirectory`

A staging directory holding a copy of a local directory — how a package
installed from a path on disk gets its source.

```typescript
scratchFromDirectory(from: string): string | null   // returns a token
```

`from` is a path on the editor host: staging directories, installed packages
and plugin state all live there by design, so an install survives the SSH
session that started it going away. There is no authority-path form, which is
why this takes a plain path rather than a `LocalPath`.

Reading a path you choose is safe here in a way `copyPath` was not — the
destination is a staging directory the editor just made, so a copy cannot
land on anything else. Symlinks are recreated as symlinks rather than
followed. Returns `null` if `from` is not a directory or could not be copied,
having discarded whatever it had already staged, so there is never a
half-filled staging directory to clean up.

#### `installScratch`

Publish a staging directory as an installed package.

```typescript
installScratch(token: string, kind: string, name: string, subpath: string): boolean
```

`kind` is `plugin`, `theme`, `language` or `bundle`; `name` must be a single
path component. Any existing install under that name goes to the trash first,
then the staging directory is renamed into place — so a failed upgrade never
leaves the user without a package, and a successful one is still recoverable.

`subpath` installs one directory out of the staging tree (a package in a
subdirectory of a cloned monorepo); pass `""` for the whole thing. It selects
the *source* only — `kind` and `name` decide where the package lands.
Installing the whole tree spends the token; installing a subpath leaves it
live so you can discard the rest.

#### `uninstallPackage`

```typescript
uninstallPackage(kind: string, name: string): boolean
```

Moves the installed package to the system trash. Returns false if nothing is
installed under that kind and name.

#### `stateSet` / `stateGet` / `stateKeys` / `stateDelete`

Namespaced key/value storage whose on-disk layout the editor owns.

```typescript
stateSet(namespace: string, key: string, value: string): boolean
stateGet(namespace: string, key: string): string | null
stateKeys(namespace: string): string[]
stateDelete(namespace: string, key: string): boolean
```

Writes are atomic. Namespaces and keys must each be a single safe path
component. Use this instead of hand-rolling a temp-file-and-rename dance in a
directory of your own.

### Environment Operations

#### `getEnv`

Get an environment variable

```typescript
getEnv(name: string): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `name` | `string` | Name of environment variable |

#### `getCwd`

Returns the editor's working directory set at startup. Use as base for resolving relative paths.

```typescript
getCwd(): string
```

### Path Operations

#### `pathJoin`

Join path segments using the OS path separator
Handles empty segments and normalizes separators.
If a segment is absolute, previous segments are discarded.

```typescript
pathJoin(parts: string[]): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `parts` | `string[]` | Path segments to join |

**Example:**

```typescript
pathJoin("/home", "user", "file.txt") // "/home/user/file.txt"
pathJoin("relative", "/absolute") // "/absolute"
```

#### `pathDirname`

Get the parent directory of a path
Returns empty string for root paths or paths without parent.
Does not resolve symlinks or check existence.

```typescript
pathDirname(path: string): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | File or directory path |

**Example:**

```typescript
pathDirname("/home/user/file.txt") // "/home/user"
pathDirname("/") // ""
```

#### `pathBasename`

Get the final component of a path
Returns empty string for root paths.
Does not strip file extension; use pathExtname for that.

```typescript
pathBasename(path: string): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | File or directory path |

**Example:**

```typescript
pathBasename("/home/user/file.txt") // "file.txt"
pathBasename("/home/user/") // "user"
```

#### `pathExtname`

Get the file extension including the dot
Returns empty string if no extension. Only returns the last extension
for files like "archive.tar.gz" (returns ".gz").

```typescript
pathExtname(path: string): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | File path |

**Example:**

```typescript
pathExtname("file.txt") // ".txt"
pathExtname("archive.tar.gz") // ".gz"
pathExtname("Makefile") // ""
```

#### `pathIsAbsolute`

Check if a path is absolute
On Unix: starts with "/". On Windows: starts with drive letter or UNC path.

```typescript
pathIsAbsolute(path: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | Path to check |

### Event/Hook Operations