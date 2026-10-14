/// <reference path="./fresh.d.ts" />

/**
 * Resolve a session's working directory to a project identity, so that two
 * checkouts of one repository (or the same repository on two machines) group as one.
 * The ladder, strongest rung first:
 *  1. `repo`: the canonical remote URL from `.git/config`. Merges across machines.
 *  2. `root`: the nearest ancestor holding a `.git`, when there is no remote.
 *  3. `path`: the working directory itself, as the labelled last resort.
 * Every rung uses plain file reads, never `git`: a machine opened by transport may run nothing.
 */

// One-way dependency: `agent_scanner.ts` must not import this module, because
// the plugin loader cannot evaluate a cycle.
import { baseName, type ProjectIdentity } from "./agent_scanner.ts";

// Generous: the remote can sit far down a config with many `[branch]` stanzas.
const GIT_CONFIG_BYTES = 64 * 1024;
const HEAD_BYTES = 512;
const POINTER_BYTES = 4 * 1024;

const MAX_ANCESTORS = 24;

// Ceiling on directories probed per resolution; the rest fall to the `path` rung.
const MAX_PROBE_DIRS = 256;

/** The parent of `path`, or null at a root. */
export function parentDir(path: string, sep: string): string | null {
  const trimmed = path.replace(/[\\/]+$/, "");
  const idx = Math.max(trimmed.lastIndexOf("/"), trimmed.lastIndexOf("\\"));
  if (idx < 0) return null;
  const head = trimmed.slice(0, idx);
  if (head === "") return trimmed.length > 1 ? sep : null;
  if (/^[A-Za-z]:$/.test(head)) return head + sep; // C:\foo -> C:\
  if (/^[\\/]{2}[^\\/]+$/.test(head)) return null; // \\server\share is a root
  return head;
}

/** `path` and every ancestor of it, nearest first. */
export function ancestorsOf(path: string, sep: string, max = MAX_ANCESTORS): string[] {
  const chain: string[] = [];
  let at: string | null = path.replace(/[\\/]+$/, "") || path;
  while (at !== null && chain.length < max) {
    chain.push(at);
    const up: string | null = parentDir(at, sep);
    if (up === null || up === at) break;
    at = up;
  }
  return chain;
}

function isAbsolute(value: string, sep: string): boolean {
  if (sep !== "\\") return value.startsWith("/");
  return value.startsWith("\\\\") || /^[A-Za-z]:[\\/]/.test(value);
}

/**
 * The remote URL from a `.git/config`: `origin`, else the first remote in file order.
 *
 * Parsed by section rather than regex-scanned for `url =`, because a
 * `[url "…"] insteadOf` stanza carries the same key outside any remote.
 */
export function gitConfigRemoteUrl(text: string): string | null {
  const urls = new Map<string, string>();
  let remote: string | null = null;
  for (const raw of text.split("\n")) {
    const line = raw.trim();
    if (line === "" || line.startsWith("#") || line.startsWith(";")) continue;
    if (line.startsWith("[")) {
      const section = /^\[\s*remote\s+"([^"]*)"\s*\]/.exec(line);
      remote = section ? section[1] : null;
      continue;
    }
    if (remote === null) continue;
    const kv = /^url\s*=\s*(.*)$/i.exec(line);
    if (!kv) continue;
    const value = kv[1].trim();
    if (value !== "" && !urls.has(remote)) urls.set(remote, value);
  }
  const origin = urls.get("origin");
  if (origin) return origin;
  for (const value of urls.values()) return value;
  return null;
}

/**
 * A remote URL reduced to host and path, so that https, scp-like and ssh
 * spellings of one repository agree. Scheme, credentials and port are dropped.
 * The id is lower-cased only for a URL with a host; a local path keeps its case.
 */
export function canonicalRepoUrl(url: string): { id: string; label: string } | null {
  let rest = url.trim();
  if (rest === "") return null;

  let host = "";
  const scheme = /^[A-Za-z][A-Za-z0-9+.-]*:\/\//.exec(rest);
  if (scheme) {
    rest = rest.slice(scheme[0].length);
    const at = rest.indexOf("@");
    const firstSlash = rest.indexOf("/");
    if (at >= 0 && (firstSlash < 0 || at < firstSlash)) rest = rest.slice(at + 1);
    const slash = rest.indexOf("/");
    host = slash < 0 ? rest : rest.slice(0, slash);
    rest = slash < 0 ? "" : rest.slice(slash + 1);
    host = host.replace(/:\d+$/, "");
    if (scheme[0].toLowerCase() === "file://") host = "";
  } else {
    // scp-like: a colon before any separator, and not a Windows drive letter.
    const scp = /^([^/\\]*?):(?![\\/])(.*)$/.exec(rest);
    if (scp && !/^[A-Za-z]$/.test(scp[1])) {
      const authority = scp[1];
      const at = authority.indexOf("@");
      host = (at >= 0 ? authority.slice(at + 1) : authority).replace(/:\d+$/, "");
      rest = scp[2];
    } else if (isAbsolute(rest, rest.includes("\\") ? "\\" : "/")) {
      host = "";
    } else {
      // A relative remote (`../sibling.git`) identifies nothing across machines.
      return null;
    }
  }

  const segments = rest
    .split(/[\\/]+/)
    .filter((s) => s !== "" && s !== ".");
  if (segments.length === 0) return null;
  const last = segments.length - 1;
  segments[last] = segments[last].replace(/\.git$/i, "");
  if (segments[last] === "") segments.pop();
  if (segments.length === 0) return null;

  const path = segments.join("/");
  const id = host === "" ? path : `${host.toLowerCase()}/${path.toLowerCase()}`;
  // `owner/name` for a hosted repository, since the name alone is ambiguous.
  const label = host === "" ? segments[segments.length - 1] : segments.slice(-2).join("/");
  return { id, label };
}

/** The target of a `.git` pointer file, as a linked worktree leaves one. */
export function gitDirPointer(text: string): string | null {
  const match = /^\s*gitdir\s*:\s*(.+?)\s*$/m.exec(text);
  return match ? match[1] : null;
}

/**
 * The repository's shared directory, given a linked worktree's own.
 * Git lays a worktree out at `<repo>/.git/worktrees/<name>`, so the shared
 * config is two levels up. Any other shape is returned as-is.
 */
export function commonDirOf(gitdir: string, sep: string): string {
  const parts = gitdir.split(/[\\/]/);
  if (parts.length >= 2 && parts[parts.length - 2] === "worktrees") {
    return parts.slice(0, -2).join(sep);
  }
  return gitdir;
}

/** The branch from a `.git/HEAD`, or the abbreviated commit when detached. */
export function headBranch(text: string): string | undefined {
  const line = text.split("\n", 1)[0]?.trim() ?? "";
  const ref = /^ref:\s*refs\/heads\/(.+)$/.exec(line);
  if (ref) return ref[1].trim() || undefined;
  if (/^[0-9a-f]{40,64}$/i.test(line)) return line.slice(0, 7);
  return undefined;
}

function join(sep: string, ...parts: string[]): string {
  // Adds a separator only where none exists. Collapsing would eat a UNC
  // prefix's second slash; blind joining would turn the root into `//.git`.
  let out = "";
  for (const part of parts) {
    if (part === "") continue;
    if (out === "") out = part;
    else out += /[\\/]$/.test(out) ? part : sep + part;
  }
  return out;
}

/**
 * Resolve every distinct working directory to a project.
 *
 * Batched per directory, not per session: one round trip normally, two when a
 * linked worktree is involved. Never throws; a machine that cannot answer
 * leaves every directory on the `path` rung.
 */
export async function resolveProjects(
  machine: FreshMachine,
  cwds: string[],
): Promise<Map<string, ProjectIdentity>> {
  const sep = machine.platform === "windows" ? "\\" : "/";
  const out = new Map<string, ProjectIdentity>();
  const distinct = [...new Set(cwds.filter((c) => typeof c === "string" && c !== ""))];
  if (distinct.length === 0) return out;

  const fallback = (cwd: string): ProjectIdentity => ({
    kind: "path",
    id: `path:${cwd}`,
    label: baseName(cwd) || cwd,
    root: cwd,
  });

  const chains = new Map<string, string[]>();
  const probeDirs: string[] = [];
  const seenDir = new Set<string>();
  for (const cwd of distinct) {
    const chain = ancestorsOf(cwd, sep);
    chains.set(cwd, chain);
    for (const dir of chain) {
      if (seenDir.has(dir)) continue;
      if (probeDirs.length >= MAX_PROBE_DIRS) break;
      seenDir.add(dir);
      probeDirs.push(dir);
    }
  }

  const text = new Map<string, string>();
  const read = async (
    requests: { path: string; maxBytes: number }[],
  ): Promise<void> => {
    if (requests.length === 0) return;
    const results = await machine.readFilePrefixes(requests);
    // Pair by position when the batch came back whole: the far side may spell a path differently.
    const paired = results.length === requests.length;
    results.forEach((r, i) => {
      if (typeof r.text !== "string") return;
      text.set(paired ? requests[i].path : r.path, r.text);
    });
  };

  try {
    await read(
      probeDirs.flatMap((dir) => [
        { path: join(sep, dir, ".git", "config"), maxBytes: GIT_CONFIG_BYTES },
        { path: join(sep, dir, ".git", "HEAD"), maxBytes: HEAD_BYTES },
        // The pointer file of a linked worktree. Fails harmlessly when `.git` is a directory.
        { path: join(sep, dir, ".git"), maxBytes: POINTER_BYTES },
      ]),
    );

    // Second round, only for linked worktrees.
    const linked = new Map<string, { gitdir: string; common: string }>();
    for (const dir of probeDirs) {
      if (text.has(join(sep, dir, ".git", "config"))) continue;
      const pointer = text.get(join(sep, dir, ".git"));
      const target = pointer ? gitDirPointer(pointer) : null;
      if (!target) continue;
      const gitdir = isAbsolute(target, sep) ? target : join(sep, dir, target);
      linked.set(dir, { gitdir, common: commonDirOf(gitdir, sep) });
    }
    const second = new Map<string, { path: string; maxBytes: number }>();
    for (const { gitdir, common } of linked.values()) {
      second.set(join(sep, common, "config"), {
        path: join(sep, common, "config"),
        maxBytes: GIT_CONFIG_BYTES,
      });
      second.set(join(sep, gitdir, "HEAD"), {
        path: join(sep, gitdir, "HEAD"),
        maxBytes: HEAD_BYTES,
      });
    }
    await read([...second.values()]);

    for (const cwd of distinct) {
      let identity: ProjectIdentity | null = null;
      for (const dir of chains.get(cwd) ?? []) {
        const worktree = linked.get(dir);
        const configAt = worktree
          ? join(sep, worktree.common, "config")
          : join(sep, dir, ".git", "config");
        const headAt = worktree
          ? join(sep, worktree.gitdir, "HEAD")
          : join(sep, dir, ".git", "HEAD");
        const config = text.get(configAt);
        const head = text.get(headAt);
        if (config === undefined && head === undefined && !worktree) continue;
        const branch = head === undefined ? undefined : headBranch(head);
        const url = config === undefined ? null : gitConfigRemoteUrl(config);
        const repo = url === null ? null : canonicalRepoUrl(url);
        identity = repo
          ? { kind: "repo", id: `repo:${repo.id}`, label: repo.label, root: dir, branch }
          : { kind: "root", id: `root:${dir}`, label: baseName(dir) || dir, root: dir, branch };
        break;
      }
      out.set(cwd, identity ?? fallback(cwd));
    }
  } catch {
    // Keep the sessions already reported; only the headings are lost.
    for (const cwd of distinct) if (!out.has(cwd)) out.set(cwd, fallback(cwd));
  }

  return out;
}
