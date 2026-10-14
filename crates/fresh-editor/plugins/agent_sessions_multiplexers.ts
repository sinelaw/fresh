/// <reference path="./lib/fresh.d.ts" />

/**
 * GNU screen and zellij scanners. Both keep sessions in a running server, like
 * tmux: sockets come from the filesystem, real sessions only when a command can run.
 *
 * screen: sockets in `$SCREENDIR`, else `S-<user>/` under `/run/screen`,
 * `/var/run/screen` or `/tmp/screens`, each named `<pid>.<tty>.<host>`.
 * zellij: `zellij list-sessions --no-formatting` prints one session per line
 * with a `(current)` or `(EXITED ...)` suffix.
 */

import {
  baseName,
  envDirs,
  joinPath,
  registerScanner,
  type CollectedSession,
  type ScanContext,
  type ScannerReport,
} from "./lib/agent_scanner.ts";

const editor = getEditor();

registerScanner({
  id: "screen",
  displayName: "GNU screen",

  async scan(machine: FreshMachine): Promise<ScannerReport> {
    const problems: string[] = [];
    const explicit = (await envDirs(machine, ["SCREENDIR"], problems))["SCREENDIR"];
    const dirs: string[] = explicit === undefined ? [] : [explicit];
    // The `S-<user>` name is not built from `$USER`: it is often unset, and a
    // transport-opened machine reports no environment. The parents are walked one
    // level deeper instead; other users' directories are 0700 and get skipped.
    const parents = ["/run/screen", "/var/run/screen", "/tmp/screens"];

    const sessions: CollectedSession[] = [];
    let anyDir = false;
    // `/var/run` is usually a symlink to `/run`, so a socket can be found twice.
    const seen = new Set<string>();
    // An explicit `$SCREENDIR` holds sockets directly; a parent holds `S-<user>/`.
    const roots: { dir: string; depth: number }[] = [
      ...dirs.map((dir) => ({ dir, depth: 1 })),
      ...parents.map((dir) => ({ dir, depth: 2 })),
    ];
    for (const { dir, depth } of roots) {
      const walk = await machine.walkTree(dir, {
        includeHidden: true,
        includeDirs: false,
        maxDepth: depth,
        maxEntries: 500,
      });
      if (walk.entries.length === 0) continue;
      anyDir = true;
      for (const entry of walk.entries) {
        const name = baseName(entry.path);
        if (seen.has(name)) continue;
        seen.add(name);
        // The label drops the pid; the id keeps it because `screen -r` takes the full name.
        const dot = name.indexOf(".");
        sessions.push({
          id: name,
          title: dot >= 0 ? name.slice(dot + 1) : name,
          path: entry.path,
          mtime: entry.mtime,
          attach: { program: "screen", args: ["-r", name] },
          evidence: [{ locator: entry.path, saying: "screen session socket" }],
        });
      }
    }

    return { sessions, installed: anyDir, problems };
  },
});

registerScanner({
  id: "zellij",
  displayName: "zellij",

  async scan(machine: FreshMachine, ctx: ScanContext): Promise<ScannerReport> {
    const problems: string[] = [];
    const runtime = (await envDirs(machine, ["XDG_RUNTIME_DIR"], problems))["XDG_RUNTIME_DIR"];

    // The runtime directory only says zellij has run here; the list comes from the command.
    let installed = false;
    if (runtime !== undefined) {
      const walk = await machine.walkTree(joinPath(machine, runtime, "zellij"), {
        includeDirs: true,
        includeHidden: true,
        maxDepth: 1,
        maxEntries: 200,
      });
      installed = walk.entries.length > 0;
    }

    if (!ctx.allowCommands) {
      return { sessions: [], installed, problems };
    }

    // Asked even when the walk found nothing: a zellij that never ran has no runtime directory.
    let listed;
    try {
      listed = await machine.run("zellij", ["list-sessions", "--no-formatting"]);
    } catch {
      // Could not spawn: zellij is not installed here. Not a problem.
      return { sessions: [], installed, problems };
    }
    if (listed.code !== 0) {
      // zellij exits non-zero with "no active sessions" when there is nothing to list.
      const err = listed.stderr.trim();
      if (/no active .*sessions/i.test(err)) return { sessions: [], installed: true, problems };
      if (err) problems.push(`zellij list-sessions: ${err}`);
      return { sessions: [], installed, problems };
    }

    const sessions: CollectedSession[] = [];
    for (const line of listed.stdout.split("\n")) {
      const trimmed = line.trim();
      if (!trimmed) continue;
      // `<name> [Created ...] (current)` or `(EXITED - ...)`.
      const name = trimmed.split(/\s+/)[0] ?? trimmed;

      // `(current)` is the session this command ran inside; `(EXITED)` is dead;
      // anything else is alive with attachment unknown.
      let attached: boolean | undefined;
      if (trimmed.includes("EXITED")) attached = false;
      else if (trimmed.includes("(current)")) attached = true;

      sessions.push({
        id: name,
        title: name,
        attached,
        // An exited session cannot be attached to.
        attach: attached === false
          ? undefined
          : { program: "zellij", args: ["attach", name] },
        evidence: [
          { locator: "zellij list-sessions --no-formatting", saying: trimmed },
        ],
      });
    }

    return { sessions, installed: true, problems };
  },
});

editor.debug("agent-sessions: screen and zellij scanners registered");
