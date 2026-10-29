/** Universal Search's "All workspaces" ordering, grouping and terminal row
 *  names (`lib/workspace_groups.ts`). */
import { groupByWorkspace, orderWorkspaces, terminalRowName } from "../lib/workspace_groups.ts";

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

const windows = [
  { id: 1, label: "main", root: "/w/main" },
  { id: 2, label: "web", root: "/w/web" },
  { id: 3, label: "api", root: "/w/api" },
  { id: 4, label: "box", root: "/w/box", remote: { kind: "ssh", detail: "me@box", connected: true } },
  { id: 5, label: "docs", root: "/w/docs" },
];

const ids = (ws: { id: number }[]) => ws.map((w) => w.id);

eq(
  ids(orderWorkspaces({ id: 3, root: "/w/api" }, windows, [])),
  [3, 1, 2, 5],
  "no dock: current first, then by window id, remote left out",
);
eq(
  ids(orderWorkspaces({ id: 3, root: "/w/api" }, windows, [
    { windowId: 5, name: "docs" },
    { windowId: 3, name: "api" },
    { windowId: 2, name: "web" },
  ])),
  [3, 5, 2, 1],
  "dock order for the others; ones the dock does not list after them",
);
eq(
  orderWorkspaces({ id: 1, root: "/cwd" }, windows, [{ windowId: 1, name: "my-app" }, { windowId: 2, name: "" }])
    .slice(0, 2)
    .map((w) => [w.name, w.root, w.current]),
  [["my-app", "/cwd", true], ["web", "/w/web", false]],
  "dock names win over labels unless empty; the current root is the cwd given",
);
eq(
  ids(orderWorkspaces({ id: 4, root: "/w/box" }, windows, [])),
  [4],
  "from a remote workspace, only it: searches run on its host",
);
eq(
  orderWorkspaces({ id: 9, root: "/gone" }, windows, []).map((w) => [w.id, w.name]),
  [[9, ""], [1, "main"], [2, "web"], [3, "api"], [5, "docs"]],
  "a current window missing from the list still leads",
);

type Row = { text: string; ws?: number };
const ws = orderWorkspaces({ id: 3, root: "/w/api" }, windows, []);
eq(
  groupByWorkspace<Row>(
    [
      { workspace: ws[0], matches: [{ text: "a" }, { text: "b" }] },
      { workspace: ws[1], matches: [] },
      { workspace: ws[2], matches: [{ text: "c" }] },
    ],
    (w, n) => ({ text: `${w.name} · ${n}` }),
    (m, w) => ({ ...m, ws: w.id }),
  ),
  [
    { text: "api · 2" },
    { text: "a", ws: 3 },
    { text: "b", ws: 3 },
    { text: "web · 1" },
    { text: "c", ws: 2 },
  ],
  "one header per workspace with matches, matches tagged, empty groups dropped",
);

const screens = [
  { file: "/d/terminals/x/fresh-terminal-1.txt", name: "claude" },
  { file: "", name: "orphan" },
  { file: "/d/terminals/x/fresh-terminal-2.txt", name: "" },
];
eq(terminalRowName("/d/terminals/x/fresh-terminal-1.txt", screens), "claude", "an open terminal's file: its name");
eq(
  terminalRowName("/d/terminals/x/fresh-terminal-3-closed-17.txt", screens),
  "fresh-terminal-3-closed-17.txt",
  "a closed terminal's file: the file's name",
);
eq(terminalRowName("/d/terminals/x/fresh-terminal-2.txt", screens), "fresh-terminal-2.txt", "an unnamed terminal: the file's name");
eq(terminalRowName("C:\\d\\fresh-terminal-4.txt", []), "fresh-terminal-4.txt", "Windows separators");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
