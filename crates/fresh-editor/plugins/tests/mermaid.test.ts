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
    "  ╭──────────╮",
    "  │ End Node │",
    "  ╰──────────╯",
  ],
}, "a flowchart edge runs straight down into a box under it");

eq(render("graph TD\n    A[Start Node] --> B(Choice A)\n    A --> C(Choice B)\n    B --> A").lines, [
  "  ┌────────────┐",
  "  │ Start Node │",
  "  └────────────┘",
  "         │",
  "        ┌┴──────────────┐",
  "        ▼               ▼",
  "  ╭──────────╮    ╭──────────╮",
  "  │ Choice A │    │ Choice B │",
  "  ╰──────────╯    ╰──────────╯",
  "  ↺ B ──> A",
], "a branch's arrows land on the centre of each box; a cycle is listed");

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

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
