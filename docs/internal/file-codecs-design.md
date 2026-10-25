# File Codecs: Loading and Saving Through External Tools

> _AI-generated design. Entirely **PLANNED**: nothing here ships. Where it
> describes today's code (the save path, recovery, the authority), the
> source is authoritative._

Purpose: let a file's **stored bytes** pass through a transform on load and on
save, with the transform supplied by **any standard command-line tool**
(`age`, `gpg`, `gzip`, `zstd`, `sops`, …) and declared by **user config or a
plugin**. The motivating case is encryption at rest for the writing mode
([writing-mode-exploration.md](writing-mode-exploration.md)): open
`journal.md.age`, edit Markdown, save, and nothing but ciphertext ever lands
on disk. The same seam covers transparent compression and any other
reversible "envelope" a tool can apply.

The design goal is to add this **without bending the architecture**: one small
trait in the core, one implementation in the app layer, one registry, and no
new branches on "is this file encrypted?" scattered through the editor.

**Naming.** "Format" is already taken twice: *Format Buffer* (the formatter)
and `BufferFormat` (encoding, line ending, BOM). This doc calls the new
thing a **file codec**: user-facing config `file_codecs`, plugin API
`registerFileCodec`, core trait `StorageCodec`.

---

## 1. The answer in one paragraph

A codec is **one more stage of the existing normalize-on-load /
convert-on-save pipeline**, sitting *between* the authority's `FileSystem`
(which keeps moving opaque bytes) and the encoding stage (which keeps
turning bytes into UTF-8). The core sees a pure `bytes → bytes` trait and
knows nothing about processes, secrets or tools. The app layer supplies the
one implementation, `CommandCodec`, which pipes bytes through a declared
argv on the **host**. Because the codec sits *above* every write primitive,
everything the save path does below it (atomic replace, in-place staging,
the sudo temp file, remote `write_file`, the content digest) handles
**ciphertext only, by construction**. The few subsystems that hold buffer
text *outside* the save path (recovery/hot exit, LSP, diff baselines) ask
one question, `is_confidential()`, and follow a fixed rule.

## 2. Prior art

Editors have done this for decades, and the lessons carry over:

- **Emacs `jka-compr` (auto-compression-mode)**: a *declarative table* of
  file-name regexp → compress/uncompress programs and args. Proves that a
  data-only description of external tools is enough for the common cases.
- **Emacs EasyPG (`epa-file`)**: transparent `.gpg` editing, passphrase
  caching for symmetric files, and auto-save inhibited for encrypted
  buffers, because auto-save files are the classic plaintext leak.
- **vim-gnupg**: the same lesson from Vim. Encrypted buffers turn off the
  swap file, undo file and viminfo for that buffer.
- **Git clean/smudge filters and `textconv`**: a filter is a command run
  with stdin → stdout, selected by path pattern. Git splits *selection*
  from *definition*. A repository's `.gitattributes` may name a filter,
  but the command itself lives only in the user's own config, never in
  the clone, so cloning can't run code. §6.1 takes the same stance.

Fresh's counterpart to auto-save is its recovery and hot-exit store, which
writes raw buffer content every couple of seconds. Getting that right is
most of the work (§8).

## 3. Where it sits, and where it doesn't

```
 disk / remote host
        │  opaque stored bytes (ciphertext, gzip, …)
 ┌──────┴───────────────────────────────┐
 │ FileSystem (Authority)               │  unchanged: read_file, atomic replace,
 │                                      │  write_patched, digest, staging, sudo
 └──────┬───────────────────────────────┘
        │  stored bytes
 ┌──────┴───────────────────────────────┐
 │ StorageCodec  (NEW, optional)        │  decode on load / encode on save
 └──────┬───────────────────────────────┘
        │  plain bytes
 ┌──────┴───────────────────────────────┐
 │ Encoding · line ending · BOM         │  unchanged (BufferFormat)
 └──────┬───────────────────────────────┘
        │  UTF-8, LF
   piece tree / TextBuffer
```

### 3.1 Why not a `FileSystem` decorator

Wrapping the authority's filesystem in a `CodecFileSystem` that decodes
matching paths is the obvious way to "plug into the authority API". It
looks clean and is wrong in five ways:

1. **The contract is random access; a codec is whole-file.** `read_range`
   (the lazy-load primitive), `write_patched`'s Copy ops, `set_file_length`,
   append, server-side `search_file` and `content_digest` all assume stored
   bytes *are* content bytes. Over a codec each either lies (sizes,
   offsets, digests) or decodes the whole file on every call.
2. **It sits below the save strategy, so plaintext hits disk anyway.** The
   save path writes a temp file and renames it, or stages a full copy in
   the recovery directory before an in-place write, or hands the content
   to the sudo helper. Those writes go to paths that don't match the codec
   rule. A decorator would see plaintext arrive at `.inplace-…tmp` and pass
   it straight through.
3. **It decodes for everyone.** Every consumer of the authority's filesystem
   (the file explorer preview, finder previews, plugin `readFile`, walks)
   would receive plaintext. Decryption should happen only when the user
   *opens a document*, not whenever something touches the path.
4. **It can't ask the user anything.** A `FileSystem` call is synchronous
   and deep in the stack, with no UI to prompt for a passphrase.
5. **It can't reach git.** Diff baselines read `git show HEAD:path` through
   the spawner, not the filesystem. A pure codec can decode those bytes
   too (§8); a filesystem wrapper never sees them.

So the codec *uses* the authority (stored bytes travel through its
`FileSystem`) without *being part of* it. The authority stays a pure router
of where bytes and processes live.

### 3.2 Why not JavaScript transforms in the plugin runtime

A `decode(bytes) / encode(bytes)` callback in a plugin is tempting, and it is
refused for v1:

- **Plaintext would cross the plugin bridge** into the QuickJS heap, where it
  can't be zeroized and lives until garbage collection. `registerDiffBaseline`
  already set the precedent that file content doesn't cross the bridge.
- **Save and load are synchronous on the editor thread; the plugin runtime is
  another thread with a one-frame command lag.** A save would need a blocking
  cross-thread round-trip, and a plugin that awaits the editor deadlocks it.
- **A hung or crashed plugin would block saving the user's novel.**

Plugins still define codecs: they *register a declaration* (§10) and the host
executes it. A plugin that needs custom logic ships or names a small CLI. If
a JS-backed codec is ever wanted, it is another `StorageCodec`
implementation behind the same core seam; nothing in the core changes.

## 4. The core seam (`fresh-editor-core`)

### 4.1 The trait

```rust
/// How a file's stored bytes become the bytes the buffer edits, and back.
/// Pure and synchronous: an implementation may block (it runs a process),
/// but the core never knows what it does.
pub trait StorageCodec: Send + Sync + fmt::Debug {
    /// Stable id for status, errors, recovery metadata ("age", "gzip").
    fn id(&self) -> &str;
    /// Stored bytes → plain bytes.
    fn decode(&self, stored: &[u8]) -> Result<Zeroizing<Vec<u8>>, CodecError>;
    /// Plain bytes → stored bytes. `Err(ReadOnly)` when the codec has no encoder.
    fn encode(&self, plain: &[u8]) -> Result<Vec<u8>, CodecError>;
    /// Whether the plain bytes are secret (drives §8). Encryption: true.
    fn is_confidential(&self) -> bool;
    /// Whether encode/decode can run now without asking the user
    /// (no secret needed, or the secret is already bound).
    fn is_unattended(&self) -> bool;
}
```

`CodecError` carries the codec id, a short reason (exit code, first stderr
line, "timed out", "tool not found") and never any content.

### 4.2 Where the buffer holds it

`BufferFormat` already answers "how do this file's bytes map to the text I
edit?" (encoding, line ending, BOM, each with a *current* and an
*original* value so a change counts as a modification). The codec is one
more stage of that answer, so it lives there as a `current`/`original`
pair of `Option<Arc<dyn StorageCodec>>`. Changing it ("Encrypt…", "Remove
Encryption", Save As to another codec) is then a format change like
switching encoding: the buffer becomes modified, and a successful save
promotes current to original. No new persistence state is needed.

`TextBuffer::is_confidential()` reads the current codec. Everything in §8
asks this one question.

### 4.3 Load

The load entry points gain a `LoadOptions { large_file_threshold,
normalize_cr, force_text, codec }` in place of their growing parameter
lists. With a codec:

1. **Always a whole-file load.** Lazy chunking is impossible over a
   transform, so the large-file path is never taken. Above the threshold,
   the open asks first, exactly as the non-resynchronizable-encoding case
   already does.
2. `fs.read_file(path)` → `codec.decode(stored)` → the **existing** binary
   and encoding detection, run on the plain bytes → the normal buffer.
3. The persisted size and digest bookkeeping stay those of the **stored**
   file, since that is what the changed-on-disk check compares against.

### 4.4 Save

In `save_to_file`, between building the recipe and writing it:

1. The recipe is built as today (encoding, line ending, BOM all applied).
2. **Invariant:** a codec buffer was loaded whole, so its recipe has no
   Copy ops (those only reference unloaded regions of a lazily loaded
   source). The save asserts this and refuses, rather than encode a recipe
   that points at ciphertext offsets.
3. `plain = recipe.flatten_inserts()`, then `stored = codec.encode(&plain)?`.
4. **Verify (default on when unattended):** `codec.decode(&stored)? ==
   plain`, or the save is refused and the file left untouched. *A file is
   never replaced by something Fresh can't read back.* This catches a
   wrong recipient list, a broken tool, or a tool that writes nothing and
   exits 0.
5. The recipe is replaced with a single Insert of `stored`, and the
   **existing** write path runs unchanged: hot-exit journal, atomic
   replace or staged in-place write, remote `write_file`, digest, and
   `finalize_save`. All of them see only stored bytes.

An encode failure fails the save like a disk-full error does: the buffer
stays modified, the file is untouched, and the error names the codec and
the tool's reason.

## 5. The command codec (`fresh-editor`, app layer)

`CommandCodec` is the only `StorageCodec` implementation in v1. It is a
resolved spec (§6) plus an optional bound secret (§7).

### 5.1 Running the tool

- **No shell, ever.** The spec is an argv array. Placeholders are
  substituted per argument, never by string interpolation into a command
  line. (The formatter's shell-joined command is exactly what this avoids.)
- **On the host, with the editor's own environment.** Not through the
  workspace's spawner, and **not** with the workspace's activated env
  (direnv, venv). A repository's `.envrc` must not be able to put its own
  `age` first on `PATH`. The executable must be a bare name (resolved on the
  editor's `PATH`) or an absolute path; relative paths are rejected. With
  executables and env both out of the workspace's reach, the workspace
  trust gate has nothing to add, and codecs keep working in Restricted
  workspaces.
- **stdin → stdout.** Stored or plain bytes are written on a writer
  thread while stdout and stderr are drained concurrently, the same shape
  the formatter adopted to fix its pipe-buffer hang. The shared logic
  (spawn argv, feed stdin, collect bytes, watchdog, kill) moves into one
  `pipe_command` service used by both, instead of a second copy.
- **Timeout** (default some seconds, configurable per codec). On expiry the
  child is killed and the operation fails.
- **Success means** exit status 0 *and*, for encode, non-empty stdout. stderr
  is kept only for the error message (first meaningful line, length-capped);
  tools don't echo their stdin there.
- **Side files only on the stored side.** Some tools insist on paths.
  `{path}` (the stored file, decode only, local files only) and `{out}` (a
  private, exclusively created 0600 temp file for the *stored* output of
  encode) are allowed, because what they hold is already ciphertext or
  compressed. **There is no placeholder for a plain-text file.** Plain
  bytes only ever travel through pipes.

### 5.2 Why the host, even for remote workspaces

For an SSH or Kubernetes workspace, the stored bytes travel through the
`RemoteFileSystem` and the tool runs **locally**. That is the right default
for encryption: *decrypt where your keys are*. The remote host stores and
moves only ciphertext, and the plaintext exists only in the local editor's
memory. It is also the pragmatic choice: `ProcessSpawner` has no stdin
today, and the remote agent's one-shot `exec` doesn't stream it. `{path}` is
refused for remote files, since the path isn't local; the stdin form works
everywhere.

**Daemon-mode caveat:** the host process is the daemon, whose environment
was captured when it started. Tools that need session variables (a GUI
pinentry's `DISPLAY`/`WAYLAND_DISPLAY`, gpg-agent's socket, `GPG_TTY`)
see the daemon's values, not the attaching
terminal's. Codec errors should say so when a tool fails in a way that
suggests it ("no pinentry", "agent refused").

## 6. Declaring and resolving codecs

### 6.1 Sources: user config and plugins only

```jsonc
// ~/.config/fresh/config.json (User layer)
"file_codecs": {
  "age": {
    "match": ["*.age"],
    "magic": ["age-encryption.org/v1", "-----BEGIN AGE ENCRYPTED FILE-----"],
    "decode": ["age", "--decrypt", "--identity", "~/.config/age/me.key"],
    "encode": ["age", "--encrypt", "--armor", "--recipients-file", "~/.config/age/recipients.txt"]
  }
}
```

A plugin's `registerFileCodec(id, spec)` writes the same shape into the
runtime config, the way `registerLspServer` writes into `lsp`. A codec runs
a program **when a file is opened**, so the **Project layer
(`.fresh/config.json` in a repository) is ignored for `file_codecs`**,
whatever the trust level. So is the Session layer, which lives in the
project's `.fresh/session.json` and can therefore arrive with a clone. Opening a cloned repository must never run a
repository-chosen program. The settings UI shows a project-level entry as
"ignored here" rather than silently dropping it.

### 6.2 Matching

- `match`: file-name globs, the primary selector. A **new** file matching
  `*.age` gets the codec from its name, so the first save encrypts.
- `magic` (optional): leading-byte prefixes, as text or as `hex:1f8b`
  for bytes that aren't valid UTF-8. When present and the file is
  non-empty, a glob match whose bytes don't start with any of them opens
  **raw** with a notice, rather than feeding an unrelated file to the tool.
  (An empty file always matches, so a freshly touched `x.age` works.)
- **Inner name.** Language detection and editorconfig run on the name with
  the matched suffix removed: `journal.md.age` is Markdown. `*.ext` globs
  strip `.ext`; others can set `"inner": "keep"`.
- When several codecs match, the most specific glob wins; a tie is a config
  error reported when the codec is registered, not when a file is opened.
  Codecs don't chain in v1 (`.md.gz.age` is one codec's job, if anyone's).

### 6.3 Save As

The destination decides the codec:

- Plain → `*.age`: the text is encrypted (this *is* "encrypt this file").
- `*.age` → a name that matches no codec, from a confidential buffer:
  **confirm first**: "This writes the decrypted text to disk
  unencrypted." There is also an explicit command, *Save Decrypted As…*,
  so the safe default never blocks a deliberate export.
- Codec to codec: re-encode with the destination's codec.

## 7. Secrets

### 7.1 Modes

| Mode | How the tool gets its secret | Works with |
|---|---|---|
| none (default) | The tool finds it itself: key file, agent, keyring, GUI pinentry, hardware token | `age -i key` (an age or ssh identity file), `gpg` with gpg-agent and a GUI pinentry, `sops` with KMS or age keys |
| `"secret": { "pass": "fd" }` | Fresh prompts (masked) and passes the secret on a dedicated pipe; `{secret_fd}` names it in argv | `gpg --pinentry-mode loopback --passphrase-fd {secret_fd}`, `openssl enc -pass fd:{secret_fd}` |
| `"secret": { "pass": { "env": "NAME" } }` | Same, in an environment variable of that one child | Tools that only read env; weaker (visible to the same user in `/proc`), so documented as the fallback |

**Tools that insist on a terminal are not supported in v1.** That includes
`age -p`, which reads its passphrase from the TTY on purpose, and curses
pinentry. The documented alternatives are an age identity file, `gpg` symmetric in
loopback mode (above), or gpg-agent with a GUI pinentry. A later
`"terminal": true` could run the tool in an integrated-terminal popup;
that is a separate feature with its own UX.

### 7.2 Flow

Load and save are synchronous, and a prompt is not. So the prompt happens
*before* the synchronous operation, using a two-phase shape the editor
already has twice: the non-resynchronizable-encoding confirmation
(open → "needs confirmation" → prompt → confirmed open) and the sudo save
(save → `SudoSaveRequired` → prompt → retry).

- **Open:** resolve the codec. If it needs a secret that isn't cached, the
  open returns `NeedsSecret`, the prompt opens, and submitting retries with
  the secret bound into the `CommandCodec`. On a decode failure the user
  can retry, **Open Raw** (read-only, no codec: saving it can never
  "encrypt" ciphertext twice or write garbage), or cancel. A text buffer
  holding garbage is never opened.
- **First save of a new symmetric file:** prompt twice (enter and confirm),
  with the honest warning: *"Forget this and nobody can recover the
  text."*
- **Cache:** the secret is bound to the buffer's codec instance (zeroized
  on drop), so later saves, verify and encoded recovery (§8) run
  unattended. Closing the buffer drops it. A *Lock* command drops the
  secret and closes the buffer's views; a per-codec `cache` setting
  (`"buffer"`, the default, or `"never"`, which prompts on every save)
  covers stricter users.

## 8. Confidential buffers: every place the text goes

The codec protects the file. A confidential buffer's plaintext also lives in
memory, and several subsystems copy it out. Each one gets a fixed rule:

| Sink | Today | With a confidential codec |
|---|---|---|
| Atomic replace temp, in-place staged copy, sudo temp, remote `write_file`, content digest | Plain buffer bytes | **Ciphertext automatically**: all run below the codec (§4.4). No code change. |
| **Recovery / hot exit** (chunk files in the data dir, written every couple of seconds) | Raw content | **Encoded**: the content is run through `codec.encode` and stored as one opaque chunk, marked with the codec id, and decoded on restore. If the codec can't run unattended yet (a new symmetric file before its first passphrase), only the entry's metadata is kept and the status bar says that unsaved changes to this file won't survive a crash. The encode runs **off the editor thread** at a longer interval than plain buffers, so typing never waits for `gpg`. |
| LSP `didOpen` / `didChange` | Sent to every attached server | **Not attached by default.** A server opts in with `"confidential": true` (a local spell or grammar server). A server on a remote authority never gets it. |
| Diff baselines: "disk", "gitRef", "gitIndex" | Stored bytes compared with the buffer | **Decoded through the same codec** before diffing, since the codec is a pure bytes function. Without this, every line looks changed. When decode can't run unattended, the baseline reports "unavailable", never a garbage diff. |
| File explorer preview, finder preview, plugin `readFile`, find-in-files, `search_file` | Read the file from disk | **Unchanged: they see ciphertext.** Searching an encrypted journal finds nothing on disk, which is correct. Search inside *open* buffers works as usual. |
| Plugins reading buffer text (`getBufferText`) | Allowed | **Allowed.** Plugins are code the user installed and they run in-process; Fresh can't enforce more than that. Documented, with `confidential` exposed on buffer info so well-behaved plugins can choose to skip such buffers. |
| Clipboard, search and replace history | Persisted / OS clipboard | **Unchanged in v1, and documented.** A later setting can keep history from recording entries typed in confidential buffers. |
| Undo history | In memory only (there is no on-disk undo file) | Unchanged. If an undo file is ever added, it falls under the canary test. |

The table is also the test plan's checklist (§12).

## 9. The authority, in summary

- **Bytes:** through the buffer's `FileSystem`, which is the window's
  authority, exactly as today. Local, SSH, container and Kubernetes all
  work, because the codec only ever asks it for whole files.
- **Processes:** on the host, outside the authority's spawners, with the
  editor's own env. Like `spawnHostProcess`, and for the reasons in §5.1
  and §5.2.
- **No authority changes.** No new `FileSystem` methods and no new
  `ProcessSpawner` methods. `Authority` stays opaque and backend-agnostic,
  and nothing in core branches on whether a file is encoded.
- **Devcontainers:** the workspace is bind-mounted, so stored bytes are read
  through the host filesystem and the codec runs on the host: the same path
  as local.

## 10. Plugin API

```ts
type FileCodecSpec = {
  match: string[];                  // file-name globs
  magic?: string[];                 // optional leading-byte prefixes: text, or "hex:…"
  inner?: "strip" | "keep";         // name used for language detection
  decode: string[];                 // argv; stdin = stored, stdout = plain
  encode?: string[];                // argv; stdin = plain, stdout = stored. Absent ⇒ read-only
  secret?: { prompt?: string; pass: "fd" | { env: string }; cache?: "buffer" | "never" };
  confidential?: boolean;           // default true
  verify?: boolean;                 // default true
  timeoutMs?: number;
};

/** Register or replace a codec. Returns false (with a logged reason) on an invalid spec. */
registerFileCodec(id: string, spec: FileCodecSpec): boolean;
unregisterFileCodec(id: string): boolean;

// BufferInfo gains:
//   codec: string | null       — the active codec id
//   confidential: boolean
```

Plus three built-in commands that work with any registered codec:
**Encrypt / Encode As…** (pick a codec, then Save As with its suffix),
**Save Decrypted As…**, and **Lock**. A writing-mode plugin uses
`codec`/`confidential` to show a lock icon. No API carries file content,
and none needs to.

## 11. Examples

```jsonc
"file_codecs": {
  // age, asymmetric: no prompt (the identity file may also be an ssh key)
  "age": {
    "match": ["*.age"],
    "magic": ["age-encryption.org/v1", "-----BEGIN AGE ENCRYPTED FILE-----"],
    "decode": ["age", "-d", "-i", "~/.config/age/me.key"],
    "encode": ["age", "-e", "-a", "-R", "~/.config/age/recipients.txt"]
  },
  // gpg, symmetric with a passphrase Fresh prompts for
  "gpg-pass": {
    "match": ["*.gpg"],
    "decode": ["gpg", "--batch", "--quiet", "--pinentry-mode", "loopback",
               "--passphrase-fd", "{secret_fd}", "--decrypt"],
    "encode": ["gpg", "--batch", "--quiet", "--pinentry-mode", "loopback",
               "--passphrase-fd", "{secret_fd}", "--symmetric", "--cipher-algo", "AES256"],
    "secret": { "prompt": "Passphrase", "pass": "fd" }
  },
  // transparent compression: not confidential, so recovery and LSP behave as usual
  "gzip": {
    "match": ["*.gz"], "magic": ["hex:1f8b"],
    "decode": ["gzip", "-dc"], "encode": ["gzip", "-c", "-n"],
    "confidential": false
  },
  // sops reads the stored file by path
  "sops": {
    "match": ["*.sops.yaml"], "inner": "keep",
    "decode": ["sops", "--decrypt", "{path}"],
    "encode": ["sops", "--encrypt", "--input-type", "yaml", "--output-type", "yaml", "/dev/stdin"]
  }
}
```

(`gzip -n` leaves out the original name and timestamp, which also suits the
writing mode's "no metadata" promise.)

## 12. Testing

- **A reversible codec from coreutils** (`base64` / `base64 -d`) drives
  unit and e2e tests without any crypto tooling in CI. Failure tools are
  `false` (exit 1), `true` (exit 0 with empty output, which encode must
  reject) and `sleep` (timeout).
- **Round-trip property test:** for random content, encodings and line
  endings, load → edit → save → load is the identity on text, and the
  stored file decodes to the expected bytes.
- **The canary test** (the one that matters): open a confidential buffer,
  type a unique canary string, then drive every sink in §8 (recovery save,
  hot exit on quit, an in-place save that stages a copy, the sudo path, a
  failed save, a crash-restore). Afterwards, assert that the canary
  appears **nowhere** under the data directory, the recovery directory,
  the system temp directory or the project directory, except decoded in
  memory. Any new subsystem that writes buffer text fails this test until
  it learns the rule.
- **Verify-on-save:** an encoder that emits valid output for a recipient
  the user can't decrypt (simulated with a decode that fails) must leave
  the original file untouched.
- **Resolution:** project-layer codecs are ignored; relative executable
  paths are rejected; a magic mismatch opens raw; `{path}` on a remote
  file is refused.

## 13. Phasing

1. **The seam.** The `StorageCodec` trait and the `BufferFormat` slot,
   `LoadOptions`, codec-aware load and save with verify, and one app-level
   loader that every open, reload, revert and restore path goes through
   (today each calls the core load functions itself). `file_codecs` config
   (User layer), `CommandCodec` without secrets, the shared `pipe_command`
   service, Save As rules, Open Raw. Recovery for confidential buffers
   stores **metadata only**. Includes the canary test. Encryption with key
   files (age, gpg-agent) is fully usable from this phase.
2. **Secrets and encoded recovery.** Prompt modes, the cache, Lock, and
   off-thread encoded recovery and hot exit.
3. **Plugin API.** `registerFileCodec`, the BufferInfo fields, and the three
   commands, with a bundled example plugin that registers age and gpg
   presets after checking the tools exist.
4. **Reach.** Codec-aware diff baselines, LSP opt-in, and, if wanted,
   terminal-mode tools.

## 14. Open questions

1. **Chaining** (`.md.gz.age`): worth it? It's cheap to add later as a list
   of codecs in the format slot, and nothing above it changes.
2. **Default recovery stance for confidential buffers** after phase 2:
   encoded recovery (crash-safe, ciphertext on disk) or none at all
   (nothing on disk but the file itself)? Encoded is the recommendation; a
   per-codec `"recovery": "encoded" | "off"` lets cautious users choose.
3. **Should `confidential` hide buffer text from plugins** (a host-enforced
   refusal of `getBufferText`)? It would break compose mode and the
   outline for encrypted Markdown, which is most of the writing-mode use.
   The proposal is no: plugins are trusted, and the flag is informational.
4. **Bundled presets:** ship age/gpg codecs enabled by default, or only as
   an opt-in plugin? An editor that runs `gpg` on any `*.gpg` it opens
   without being asked may surprise people, which argues for opt-in.
