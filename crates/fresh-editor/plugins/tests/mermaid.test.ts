/** Mermaid diagrams as box-drawing text (`lib/mermaid.ts`). */
import { renderMermaidDiagram } from "../lib/mermaid.ts";

let failures = 0;
function eq(actual: unknown, expected: unknown, name: string): void {
  const a = JSON.stringify(actual, null, 1);
  const e = JSON.stringify(expected, null, 1);
  if (a !== e) {
    console.log(`FAIL ${name}\n  got      ${a}\n  expected ${e}`);
    failures++;
  } else {
    console.log(`ok   ${name}`);
  }
}

/** Terminal columns: two for CJK ideographs, one for everything else here. */
const width = (s: string): number =>
  [...s].reduce((w, ch) => w + (/[一-鿿]/.test(ch) ? 2 : 1), 0);
const render = (src: string) => renderMermaidDiagram(src, 60, width);

eq(render("graph TD\n    A[Start Node] --> B(End Node)"), {
  title: "Mermaid Flowchart (TD)",
  lines: [
    "  ┌────────────┐",
    "  │ Start Node │",
    "  └────────────┘",
    "         │",
    "         ▼",
    "   ╭──────────╮",
    "   │ End Node │",
    "   ╰──────────╯",
  ],
}, "a flowchart edge runs straight down into a box under it");

eq(render("graph TD\n    A[Start Node] --> B(Choice A)\n    A --> C(Choice B)\n    B --> A").lines, [
  "         ┌────────────┐",
  "         │ Start Node │",
  "         └────────────┘",
  "                │",
  "        ┌───────┴───────┐",
  "        ▼               ▼",
  "  ╭──────────╮    ╭──────────╮",
  "  │ Choice A │    │ Choice B │",
  "  ╰──────────╯    ╰──────────╯",
  "  ↺ Choice A ──> Start Node",
], "a branch's arrows land on the centre of each box; a cycle is listed");

eq(render("flowchart TD\n    A[Start] --> B{Is it working?}\n    B -->|Yes| C[Ship it]\n    B -->|No| D[Debug]\n    D --> B").lines, [
  "          ┌───────┐",
  "          │ Start │",
  "          └───────┘",
  "              │",
  "              ▼",
  "     ◇ ────────────── ◇",
  "     │ Is it working? │",
  "     ◇ ────────────── ◇",
  "              │",
  "       ┌──────┴──────┐",
  "       ▼ Yes         ▼ No",
  "  ┌─────────┐    ┌───────┐",
  "  │ Ship it │    │ Debug │",
  "  └─────────┘    └───────┘",
  "  ↺ Debug ──> Is it working?",
], "`-->|label|` names the edge, not the node; each label sits by its own arrow");

eq(render("stateDiagram-v2\n    [*] --> Idle\n    Idle --> Running : start\n    Running --> Idle : stop\n    Running --> [*]").lines, [
  "       ●",
  "       │",
  "       ▼",
  "   ┌──────┐",
  "   │ Idle │",
  "   └──────┘",
  "       │",
  "       ▼ start",
  "  ┌─────────┐",
  "  │ Running │",
  "  └─────────┘",
  "       │",
  "       ▼",
  "       ◉",
  "  ↺ Running ──> Idle [stop]",
], "a state diagram: start and end dots, events by their arrows, a transition back listed");

eq(render("stateDiagram-v2\n    state Moving {\n      [*] --> Slow\n    }").lines.join("\n").includes("Moving"), true,
  "a composite state's own start is the state itself");

eq(render("flowchart TB\n    subgraph Client\n      UI[Web UI]:::blue\n    end\n    UI & CLI --> API\n    style UI fill:#f9f\n    classDef blue fill:#00f\n    click API \"https://example.com\"").lines.slice(0, 3), [
  "  ┌────────┐    ┌─────┐",
  "  │ Web UI │    │ CLI │",
  "  └────────┘    └─────┘",
], "subgraph/style/classDef/click lines draw nothing; :::class is dropped; A & B is two nodes");

const seq = render("sequenceDiagram\n    autonumber\n    A->>+B: Hi\n    loop Every minute\n        B-->>A: Ping\n    end\n    rect rgb(0,0,0)\n    A-xB: Bye\n    end").lines;
eq(seq.some(l => l.includes("1. Hi")) && seq.some(l => l.includes("2. Ping")), true, "autonumber; + activation is not part of the name");
eq(seq.filter(l => l.includes("┄")).length, 2, "a loop block opens and closes with a rule; a rect draws none");
eq(seq.some(l => l.includes("┄ loop Every minute ┄")), true, "the rule is labelled with the block");
eq(seq.some(l => /3\. Bye ─+x│/.test(l)), true, "-x ends in a cross");

eq(render("graph LR\n    A --> B\n    A --> C").title, "Mermaid Flowchart (LR, drawn top-down)",
  "a left-to-right graph that branches is drawn top-down rather than as a misleading chain");

eq(render("graph LR\n    B --> C\n    A --> B").lines[1], "│ A │ ──────> │ B │ ──────> │ C │",
  "a chain is drawn in link order, whatever order its lines come in");
eq(render("graph LR\n    A[(DB)] --> B").lines, [
  "╭────╮",
  "├────┤         ┌───┐",
  "│ DB │ ──────> │ B │",
  "╰────╯         └───┘",
], "a taller box in a row keeps all its rows, its arrow on the label row");
eq(render("graph TD; A-->B").lines.some(l => l.includes("│ B │")), true, "statements after the header's `;`");
eq(render("graph TD\n    style-1 --> B[\"a;b\"]").lines.some(l => l.includes("│ style-1 │")), true,
  "a node id that starts with a keyword is a node");
eq(render("graph TD\n    A --> B[\"a;b\"]").lines.some(l => l.includes("│ a;b │")), true,
  "a quoted label loses its quotes and keeps its `;`");
eq(render("sequenceDiagram\n    participant Customer\n    participant B\n    Customer->>B: hi").lines.slice(0, 4), [
  "┌──────────┐                ┌──────┐",
  "│ Customer │                │ B    │",
  "└────┬─────┘                └──┬───┘",
  "     │                         │",
], "a long first name: the box starts at the edge and its lifeline meets the ┬");
eq(render("sequenceDiagram\n    A->>B: " + "x".repeat(60)).lines.some(l => l.includes("…") && l.endsWith(">│")), true,
  "a message too long for its arrow is shortened");
eq(render("stateDiagram-v2\n    [*] --> M\n    state \"Moving fast\" as M {\n      [*] --> Slow\n    }").lines.join("\n").match(/Moving fast[\s\S]*Slow/) !== null, true,
  "a described composite state scopes its inner start");

const edgesOf = (src: string) => render(src).lines.join("\n");
for (const [src, name] of [
  ["graph LR\n    A -- yes --> B", "-- label -->"],
  ["graph LR\n    A == yes ==> B", "== label ==>"],
  ["graph LR\n    A -. yes .-> B", "-. label .->"],
  ["graph LR\n    A --->|yes| B", "a longer arrow with |label|"],
  ["graph LR\n    A -->| yes | B", "|label| with spaces"],
]) {
  eq(edgesOf(src).includes("── yes ──>") && !edgesOf(src).includes("│ yes"), true, `edge label: ${name}`);
}

eq(render("sequenceDiagram\n    participant A as Alice\n    participant B as Bob\n    A->>B: Ping\n    B-->>A: Pong").lines, [
  " ┌───────┐                 ┌──────┐",
  " │ Alice │                 │ Bob  │",
  " └──┬────┘                 └──┬───┘",
  "    │                         │",
  "    │───────── Ping ─────────>│",
  "    │                         │",
  "    │<- - - - - Pong  - - - - │",
  "    │                         │",
  " ┌──┴────┐                 ┌──┴───┐",
  " │ Alice │                 │ Bob  │",
  " └───────┘                 └──────┘",
], "participant boxes are whole and the lifelines meet their centres");

eq(render("sequenceDiagram\n    participant A as 小明\n    participant B as Bob\n    A->>B: Hi").lines.slice(0, 3), [
  " ┌──────┐                  ┌──────┐",
  " │ 小明 │                  │ Bob  │",
  " └──┬───┘                  └──┬───┘",
], "a wide-character name is measured in columns, so its box stays whole");

eq(render("pie\n    title Browser Share\n    \"Chrome\" : 65\n    \"Firefox\" : 20"), {
  title: "Mermaid: Browser Share",
  lines: [
    "Pie: Browser Share",
    "─────────────────────────────────────",
    "  Chrome  │    65 ( 76.5%) [████████████░░░░]",
    "  Firefox │    20 ( 23.5%) [████░░░░░░░░░░░░]",
  ],
}, "a pie chart, its title on the line after `pie`");

eq(render("classDiagram\n    class Animal {\n      +name\n      +eat()\n    }").lines, [
  "┌──────────┐",
  "│ Animal   │",
  "├──────────┤",
  "│ +name    │",
  "│ +eat()   │",
  "└──────────┘",
], "a class's members, from its `{ ... }` body");

eq(render("classDiagram\n    Animal : +age").lines, [
  "┌──────────┐",
  "│ Animal   │",
  "├──────────┤",
  "│ +age     │",
  "└──────────┘",
], "a member added with `Class : member`");

eq(render("gantt\n    title X").lines, ["  gantt", "  title X"], "an unsupported diagram is shown as its source");
eq(render("gantt\n    title X").title, "Mermaid gantt (not drawn)", "and its title says it is not drawn");
eq(render("---\ntitle: Flow\n---\nflowchart TD\n    A --> B").title, "Mermaid Flowchart (TD)", "front matter is skipped");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
