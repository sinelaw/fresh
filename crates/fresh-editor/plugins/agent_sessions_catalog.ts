/// <reference path="./lib/fresh.d.ts" />

/**
 * Registers one scanner per row of `lib/agent_catalog.ts`. Tools with a
 * layout of their own (tmux, Claude Code, Codex, Herdr, Orca) have their own plugin.
 *
 * Each store costs two round trips regardless of session count: one
 * `walkTree`, then one `readFilePrefixes` for the transcripts kept.
 */

import {
  envDirs,
  firstJsonRecord,
  joinPath,
  pickString,
  readHeaders,
  registerScanner,
  type CollectedSession,
  type Evidence,
  type ScanContext,
  type ScannerReport,
} from "./lib/agent_scanner.ts";
import {
  AGENT_STORES,
  fileRuleAccepts,
  hasExtension,
  sessionIdFor,
  type AgentStore,
} from "./lib/agent_catalog.ts";

const editor = getEditor();

const HEADER_BYTES = 64 * 1024;

/** The directory to walk, and the variable that moved it, if any. */
async function storeRoot(
  machine: FreshMachine,
  store: AgentStore,
): Promise<{ root: string; evidence: Evidence[]; problems: string[] }> {
  const evidence: Evidence[] = [];
  const problems: string[] = [];

  let base = joinPath(machine, machine.home, ...store.baseSegments);
  const env = await envDirs(machine, store.baseEnv, problems);
  for (const name of store.baseEnv) {
    const value = env[name];
    if (value === undefined) continue;
    base = value;
    // Recorded whether or not the directory exists; it is not evidence of an install.
    evidence.push({ locator: name, saying: value });
    break;
  }

  return { root: joinPath(machine, base, ...store.suffix), evidence, problems };
}

async function scanStore(
  machine: FreshMachine,
  store: AgentStore,
  ctx: ScanContext,
): Promise<ScannerReport> {
  const { root, evidence, problems } = await storeRoot(machine, store);

  const walk = await machine.walkTree(root, {
    includeHidden: true,
    includeDirs: false,
    maxDepth: store.maxDepth,
    // One past the cap, so truncation is observed rather than inferred.
    maxEntries: 5001,
  });
  if (walk.entries.length === 0) {
    return { sessions: [], installed: false, problems };
  }
  if (walk.truncated) {
    problems.push(
      `${root}: stopped at the walk limit, so some sessions are not reported`,
    );
  }
  evidence.push({ locator: root, saying: `${store.displayName} session store` });

  const transcripts = walk.entries.filter(
    (entry) =>
      entry.kind !== "dir" &&
      hasExtension(store.extensions, entry.rel) &&
      fileRuleAccepts(store.fileRule, entry.rel),
  );

  if (store.layout === "opaque") {
    for (const entry of transcripts.slice(0, 50)) {
      evidence.push({
        locator: entry.path,
        saying: `${store.displayName} state; format not parsed, so no sessions are reported from it`,
      });
    }
    return { sessions: [], installed: true, problems };
  }

  // Newest first, capped before the read.
  const headers = await readHeaders(machine, transcripts, {
    maxBytes: HEADER_BYTES,
    limit: ctx.maxSessions,
    problems,
  });

  const sessions: CollectedSession[] = headers.map(({ entry, text }) => {
    const record = text ? firstJsonRecord(text) : null;
    return {
      id: sessionIdFor(store.layout, entry.rel),
      cwd: pickString(record, store.cwdFields),
      path: entry.path,
      mtime: entry.mtime,
      evidence: [
        {
          locator: entry.path,
          saying: `${store.displayName} transcript under ${root}`,
        },
      ],
    };
  });

  // Ids must be unique per tool: the consumer keys rows by `tool/id`. A store
  // with a fixed filename per session directory gives every row the same stem,
  // so only the colliding rows are re-keyed by their relative path.
  const byId = new Map<string, CollectedSession[]>();
  for (const s of sessions) {
    const list = byId.get(s.id);
    if (list) list.push(s);
    else byId.set(s.id, [s]);
  }
  for (const [, clashing] of byId) {
    if (clashing.length < 2) continue;
    for (const s of clashing) {
      const entry = headers.find((h) => h.entry.path === s.path)?.entry;
      if (entry) s.id = entry.rel.replace(/\.[^./]+$/, "");
    }
  }

  return { sessions, installed: true, problems: [...problems] };
}

for (const store of AGENT_STORES) {
  registerScanner({
    id: store.id,
    displayName: store.displayName,
    scan: (machine, ctx) => scanStore(machine, store, ctx),
  });
}

editor.debug(`agent-sessions: ${AGENT_STORES.length} catalog scanners registered`);
