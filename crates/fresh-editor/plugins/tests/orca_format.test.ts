/** Orca's orca-data.json, in the shape a real install writes. */
import { orcaWorktreePath, orcaWorktrees, type OrcaState } from "../lib/orca_format.ts";

let failures = 0;
function eq(actual: unknown, expected: unknown, name: string): void {
  const a = JSON.stringify(actual);
  const e = JSON.stringify(expected);
  if (a !== e) {
    console.log(`FAIL ${name}\n  got      ${a}\n  expected ${e}`);
    failures++;
  } else {
    console.log(`ok   ${name}`);
  }
}

const KEY = "22ea1782::/home/u/repos/clips";

// Trimmed from a real profiles/local-default/orca-data.json.
const real: OrcaState = {
  repos: [{ id: "22ea1782", displayName: "clips" }],
  worktreeMeta: {
    [KEY]: { displayName: "", lastActivityAt: 1790191874209, hostId: "local" },
  },
  workspaceSession: {
    tabsByWorktree: {
      [KEY]: [
        { id: "tab-shell" },
        {
          id: "tab-agent",
          launchAgent: "claude",
          aiVaultTitle: { agent: "claude", sessionId: "b4b69504", title: "Status check" },
        },
      ],
    },
    sleepingAgentSessionsByPaneKey: {
      "tab-agent:leaf": {
        tabId: "tab-agent",
        worktreeId: KEY,
        agent: "claude",
        providerSession: { id: "b4b69504" },
      },
    },
  },
};

eq(orcaWorktreePath(KEY), "/home/u/repos/clips", "the path is the key's second half");
eq(orcaWorktreePath("no-separator"), null, "a key of another shape has no path");

eq(orcaWorktrees(real), [{
  id: KEY,
  path: "/home/u/repos/clips",
  name: "clips",
  lastActivityAt: 1790191874209,
  agents: [{ agent: "claude", sessionId: "b4b69504", title: "Status check", tabId: "tab-agent" }],
}], "a real file yields its worktree, named after the repo, with the agent tab");

eq(orcaWorktrees({
  worktreeMeta: { [KEY]: { displayName: "checkout flow" } },
})[0].name, "checkout flow", "Orca's own display name wins");

eq(orcaWorktrees({
  worktreeMeta: { [KEY]: {} },
  workspaceSession: {
    tabsByWorktree: { [KEY]: [{ id: "t", launchAgent: "codex" }] },
  },
})[0].agents, [{ agent: "codex", sessionId: "", tabId: "t" }],
  "a launched agent with no recorded session resumes the newest");

eq(orcaWorktrees({
  worktreeMeta: { [KEY]: {} },
  workspaceSession: {
    sleepingAgentSessionsByPaneKey: {
      p: { tabId: "t", worktreeId: KEY, agent: "claude", providerSession: { id: "s1" }, terminalTitle: "fix" },
    },
  },
})[0].agents, [{ agent: "claude", sessionId: "s1", title: "fix", tabId: "t" }],
  "a session sleeping at quit is found without a tab title");

eq(orcaWorktrees({ worktreeMeta: { [KEY]: { hostId: "ssh-1" } } }), [],
  "a worktree on an SSH target is not on this machine");
eq(orcaWorktrees({}), [], "an empty file has no worktrees");

console.log(failures === 0 ? "\nAll orca format tests passed." : `\n${failures} failed.`);
if (failures > 0) process.exit(1);
