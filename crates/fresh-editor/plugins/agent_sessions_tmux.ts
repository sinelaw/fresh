/// <reference path="./lib/fresh.d.ts" />

/**
 * tmux scanner for the agent-sessions hub.
 *
 * tmux keeps its session model in the server process. The only thing on disk
 * is the server socket at `$TMUX_TMPDIR/tmux-<uid>/<name>` (`/tmp` when
 * unset). Without commands the socket is the whole answer; with them the
 * server is asked for sessions and panes.
 *
 * tmux records no agent identity. A pane is reported as an agent when
 * `pane_current_command` matches a known agent name, which is only a guess.
 * Format output is delimited with `\x1f`, which tmux escapes as `\037`;
 * `splitEscapedFields` undoes that.
 */

import {
  baseName,
  envDirs,
  registerScanner,
  type CollectedSession,
  type ScanContext,
  type ScannerReport,
} from "./lib/agent_scanner.ts";
import {
  AGENT_COMMANDS,
  PANE_FORMAT,
  SESSION_FORMAT,
  splitEscapedFields,
} from "./lib/tmux_format.ts";

const editor = getEditor();

/** Sockets tmux may be listening on. */
async function socketPaths(
  machine: FreshMachine,
  problems: string[],
): Promise<string[]> {
  const tmpdir = (await envDirs(machine, ["TMUX_TMPDIR"], problems))["TMUX_TMPDIR"] ?? "/tmp";

  // `tmux-<uid>` directories are found by listing rather than from a uid: the
  // directory is mode 0700, so this sees only what it may read, and a remote
  // machine may not report a uid at all.
  const dirs = await machine.walkTree(tmpdir, {
    includeDirs: true,
    includeHidden: true,
    maxDepth: 1,
    maxEntries: 2000,
  });
  const sockets: string[] = [];
  for (const entry of dirs.entries) {
    if (entry.kind !== "dir" || !/^tmux-\d+$/.test(entry.rel)) continue;
    const inner = await machine.walkTree(entry.path, {
      includeHidden: true,
      includeDirs: false,
      maxDepth: 1,
      maxEntries: 500,
    });
    // Everything inside `tmux-<uid>/` is a socket. The walk may report it as
    // a file or a symlink depending on the transport.
    for (const sock of inner.entries) sockets.push(sock.path);
  }
  return sockets;
}

/** Ask a running server for its sessions and panes. */
async function liveSessions(
  machine: FreshMachine,
  socket: string,
  problems: string[],
): Promise<CollectedSession[] | null> {
  const list = await machine.run("tmux", ["-S", socket, "list-sessions", "-F", SESSION_FORMAT]);
  if (list.code !== 0) {
    // A gone server is not a failure; a stale socket in `/tmp` is normal and
    // the socket-only fallback covers it.
    const err = list.stderr.trim();
    if (!/no server running/i.test(err)) {
      problems.push(`tmux -S ${socket} list-sessions: ${err || `exit ${list.code}`}`);
    }
    return null;
  }

  // Ids are qualified by socket. A session name is unique only within one
  // server, and a machine can run several.
  const server = baseName(socket);
  const sessions = new Map<string, CollectedSession>();
  for (const line of list.stdout.split("\n")) {
    if (!line.trim()) continue;
    const [id, name, created, attached, path] = splitEscapedFields(line);
    if (!id) continue;
    sessions.set(id, {
      id: `${server}/${name || id}`,
      title: name || id,
      cwd: path || undefined,
      mtime: Number.parseInt(created ?? "", 10) || undefined,
      // tmux reports the number of attached clients.
      attached: (attached ?? "0") !== "0",
      // `-S` because a server on a non-default socket is invisible to a bare
      // `tmux attach`. `-t` by name: the id is only stable while the server lives.
      attach: { program: "tmux", args: ["-S", socket, "attach", "-t", name || id] },
      evidence: [{ locator: socket, saying: "tmux server socket" }],
    });
  }

  // One `list-panes -a` rather than one call per session.
  const panes = await machine.run("tmux", ["-S", socket, "list-panes", "-a", "-F", PANE_FORMAT]);
  const out: CollectedSession[] = [...sessions.values()];
  if (panes.code === 0) {
    for (const line of panes.stdout.split("\n")) {
      if (!line.trim()) continue;
      const [sessionId, windowIndex, windowName, paneId, , paneCwd, paneCommand, paneTitle] =
        splitEscapedFields(line);
      if (!paneCommand || !AGENT_COMMANDS.has(baseName(paneCommand))) continue;
      const session = sessionId ? sessions.get(sessionId) : undefined;
      out.push({
        // The pane id is unique within the server and stable while it lives.
        id: `${session?.id ?? sessionId ?? "tmux"}${paneId ?? ""}`,
        title: `${paneCommand} — ${windowName || windowIndex || ""}`.trim(),
        cwd: paneCwd || session?.cwd,
        attached: session?.attached,
        mtime: session?.mtime,
        // A name match on the foreground process, so weak.
        agent: baseName(paneCommand),
        // tmux attaches to a session; there is no attach-to-pane.
        attach: session?.attach,
        evidence: [
          {
            locator: `${socket} ${paneId ?? ""}`.trim(),
            saying: `pane_current_command is "${paneCommand}"${
              paneTitle ? `, title "${paneTitle}"` : ""
            } — a name match, not an identity`,
          },
        ],
      });
    }
  } else if (panes.stderr.trim()) {
    problems.push(`tmux -S ${socket} list-panes: ${panes.stderr.trim()}`);
  }
  return out;
}

registerScanner({
  id: "tmux",
  displayName: "tmux",

  async scan(machine: FreshMachine, ctx: ScanContext): Promise<ScannerReport> {
    const problems: string[] = [];
    const sockets = await socketPaths(machine, problems);
    if (sockets.length === 0) {
      return { sessions: [], installed: false, problems };
    }

    const sessions: CollectedSession[] = [];
    for (const socket of sockets) {
      const live = ctx.allowCommands
        ? await liveSessions(machine, socket, problems)
        : null;
      if (live) {
        sessions.push(...live);
        continue;
      }
      // Commands unavailable or the server gone: the socket alone is the
      // answer, and a weak one.
      sessions.push({
        id: socket,
        title: baseName(socket),
        evidence: [
          {
            locator: socket,
            saying: "socket exists; server not queried, so the session list is unknown",
          },
        ],
      });
    }
    return { sessions, installed: true, problems };
  },
});

editor.debug("agent-sessions: tmux scanner registered");
