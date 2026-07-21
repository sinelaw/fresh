/// <reference path="./lib/fresh.d.ts" />

/**
 * Slang LSP Helper Plugin
 *
 * slangd (the Slang shader language server) answers go-to-definition on
 * builtins — `float3`, `normalize`, etc. — with a synthetic URI such as
 * `slang-synth://core/core.builtin` that has no file on disk. The editor
 * core can't open a non-`file://` URI, so on its own it just reports
 * "external location, no local source file".
 *
 * This plugin makes those targets actually open. It:
 *   1. tells the core it provides the `slang-synth` scheme, so the core
 *      routes such targets here instead of showing the fallback message;
 *   2. on the `lsp_open_external_uri` hook, dumps the requested builtin
 *      module with `slangd --print-builtin-module <module>` — the SAME
 *      binary serving the LSP, so the line numbers in the URI match — and
 *      opens the cached `.slang` file at the target position, read-only.
 *
 * The core stays scheme-agnostic; everything Slang-specific (the scheme
 * string, the `--print-builtin-module` flag, module parsing, caching)
 * lives here. This mirrors the VS Code `TextDocumentContentProvider` and
 * `slang.nvim` (`--print-builtin-module`) approaches.
 */

const editor = getEditor();

const SCHEME = "slang-synth";

interface LspOpenExternalUriData {
  uri: string;
  scheme: string;
  line: number; // 0-indexed (LSP)
  character: number; // 0-indexed (LSP)
  language: string;
  server_name: string;
}

/**
 * `slang-synth://core/core.builtin` → `core`. The authority component is
 * the module name; slangd's `--print-builtin-module` takes exactly that.
 */
function moduleFromUri(uri: string): string | null {
  const rest = uri.slice(`${SCHEME}://`.length);
  const mod = rest.split("/")[0];
  return mod && mod.length > 0 ? mod : null;
}

/**
 * The Slang toolchain version (from `slangc -v`), used to key the cache so
 * a compiler upgrade — which shifts the builtin-module line numbers —
 * doesn't serve stale content. Falls back to null (→ ephemeral temp dir)
 * when `slangc` isn't on PATH.
 */
async function slangVersion(): Promise<string | null> {
  try {
    const r = await editor.spawnProcess("slangc", ["-v"]);
    const v = (r.stdout || r.stderr || "").trim().split(/\s+/)[0];
    return v && /^[0-9]/.test(v) ? v : null;
  } catch (_e) {
    return null;
  }
}

/** Absolute path to cache this module's dumped source at. */
async function cachePathFor(module: string): Promise<string> {
  const version = await slangVersion();
  // Versioned + persistent when we know the version; ephemeral otherwise so
  // an unknown build can never leave permanently-stale content behind.
  const root = version
    ? `${editor.getDataDir()}/slang-builtin/${version}`
    : `${editor.getTempDir()}/fresh-slang-builtin`;
  editor.createDir(editor.localPath(root)); // stdoutTo won't create parent dirs
  return `${root}/${module}.slang`;
}

editor.registerLspUriScheme(SCHEME);

editor.on("lsp_open_external_uri", async (data: LspOpenExternalUriData) => {
  if (data.scheme !== SCHEME) return; // not ours

  // The builtin-module cache is host-side: `getDataDir`/`fileExists`/
  // `createDir` and `stdoutTo` all resolve on the host, while `openFile`
  // reads through the active window's authority filesystem. On the local
  // authority those are the same filesystem, so this is consistent. On a
  // remote/SSH/container authority they diverge — the dump lands host-side
  // but `openFile` would look for it on the remote — so caching + opening
  // can't line up without an authority-aware write, which the plugin API
  // doesn't expose. Rather than open a phantom empty buffer, bail with a
  // clear message on non-local authorities. (Highlighted builtin
  // navigation over a remote authority needs a core primitive that opens
  // in-memory content with a syntax language; tracked as follow-up.)
  if (editor.getAuthorityLabel() !== "") {
    editor.setStatus(
      "slang-lsp: opening builtin modules is only supported on the local authority"
    );
    return;
  }

  const module = moduleFromUri(data.uri);
  if (!module) {
    editor.setStatus(`slang-lsp: could not parse module from ${data.uri}`);
    return;
  }

  const path = await cachePathFor(module);

  if (!editor.fileExists(editor.localPath(path))) {
    editor.setStatus(`slang-lsp: loading builtin module '${module}'…`);
    const res = await editor.spawnProcess(
      "slangd",
      ["--print-builtin-module", module],
      undefined,
      path // stdoutTo: write the dump straight to the cache file
    );
    if (res.exit_code !== 0 || !editor.fileExists(editor.localPath(path))) {
      editor.setStatus(
        `slang-lsp: failed to load builtin module '${module}' (exit ${res.exit_code})`
      );
      return;
    }
  }

  // LSP positions are 0-indexed; openFile is 1-indexed.
  if (!editor.openFile(path, data.line + 1, data.character + 1)) {
    editor.setStatus(`slang-lsp: could not open ${path}`);
    return;
  }
  // It's generated builtin source — don't let it be edited by accident.
  // Resolve read-only by path (not active-buffer id): openFile and this
  // call are FIFO commands, so the buffer exists when this is processed,
  // whereas getActiveBufferId() reads a snapshot that may still be stale.
  editor.markFileReadOnly(path);
});

editor.debug("slang-lsp: Plugin loaded");
