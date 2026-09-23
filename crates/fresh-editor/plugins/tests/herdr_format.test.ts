/** Herdr's session.json, in the shape v0.9.0 writes. */
import { herdrAgentPanes, type HerdrSnapshot, type PaneSnapshot } from "../lib/herdr_format.ts";

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

const ws = (panes: Record<string, PaneSnapshot>): HerdrSnapshot => ({
  version: 3,
  workspaces: [{ id: "w1", custom_name: null, identity_cwd: "/repo", tabs: [{ panes }] }],
});

// A real session.json with claude typed by hand and no integration: cwd only.
eq(herdrAgentPanes(ws({ "1": { cwd: "/repo" } })), [],
  "a process-detected agent is not persisted, so there is nothing to import");

eq(herdrAgentPanes(ws({
  "1": {
    cwd: "/repo/a",
    agent_session: { source: "herdr:claude", agent: "claude", kind: "id", value: "b4b6" },
  },
})), [{
  workspaceId: "w1",
  paneId: "1",
  cwd: "/repo/a",
  title: "claude",
  agent: "claude",
  sessionId: "b4b6",
  saying: "Herdr records this pane's agent session (id: b4b6)",
}], "an integration-reported session resumes by its id");

eq(herdrAgentPanes(ws({
  "1": { agent_session: { source: "someone-else", agent: "claude", kind: "id", value: "x" } },
})), [], "a session from an unofficial source is not trusted");

eq(herdrAgentPanes(ws({
  "1": { agent_session: { source: "herdr:pi", agent: "pi", kind: "path", value: "/t.jsonl" } },
}))[0].sessionId, "", "a transcript path is not a session id");

const managed = herdrAgentPanes(ws({ "1": { managed_agent_kind: "codex", agent_name: "reviewer" } }))[0];
eq([managed.agent, managed.sessionId, managed.title, managed.cwd], ["codex", "", "reviewer", "/repo"],
  "a managed agent resumes its kind's newest session, titled by its name");

eq(herdrAgentPanes(ws({ "1": { agent_name: "reviewer" } })), [],
  "a name alone is a label, not an agent to run");

eq(herdrAgentPanes({
  workspaces: [{ custom_name: "api", panes: { "2": { managed_agent_kind: "claude" } } }],
})[0].workspaceId, "api", "a legacy workspace without an id is keyed by its custom name");

console.log(failures === 0 ? "\nAll herdr format tests passed." : `\n${failures} failed.`);
if (failures > 0) process.exit(1);
