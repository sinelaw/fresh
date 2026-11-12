/**
 * Mermaid diagrams drawn as box-drawing text: flowcharts, sequence diagrams,
 * pie charts, and state and class diagrams.
 *
 * Pure functions with no editor calls, so they can be unit-tested
 * (`tests/mermaid.test.ts`). Text is measured with the `width` the caller
 * passes, which in the editor is its terminal cell width.
 */

/** Columns `text` takes on screen. */
export type Width = (text: string) => number;

/** `s` padded with spaces to `cols` columns, measured by `width`. */
function padTo(s: string, cols: number, width: Width): string {
  return s + " ".repeat(Math.max(0, cols - width(s)));
}


export interface MermaidRenderResult {
  title: string;
  lines: string[];
}

interface FlowNode {
  id: string;
  label: string;
  shape: "box" | "round" | "stadium" | "cylinder" | "diamond" | "circle" | "hexagon" | "flag" | "start" | "end";
}

interface FlowEdge {
  from: string;
  to: string;
  label?: string;
}

function parseNodeDef(raw: string): FlowNode {
  // A `:::className` suffix styles the node; it is not part of its text.
  raw = raw.replace(/:::[\w-]+$/, "").trim();
  const m = raw.match(/^([A-Za-z0-9_\-]+)\s*(?:\(\[([^\]]+)\]\)|\(\(([^)]+)\)\)|\[\(([^)]+)\)\]|\{\{([^}]+)\}\}|\[([^[\]]+)\]|\(([^(]+)\)|\{([^{}]+)\}|>([^\]]+)\])?$/);
  if (!m) return { id: raw, label: raw, shape: "box" };
  const id = m[1];
  // A label may be quoted, to hold characters Mermaid would otherwise read.
  for (let k = 2; k <= 9; k++) {
    if (m[k] !== undefined) m[k] = m[k].trim().replace(/^"(.*)"$/, "$1");
  }
  if (m[2] !== undefined) return { id, label: m[2].trim(), shape: "stadium" };
  if (m[3] !== undefined) return { id, label: m[3].trim(), shape: "circle" };
  if (m[4] !== undefined) return { id, label: m[4].trim(), shape: "cylinder" };
  if (m[5] !== undefined) return { id, label: m[5].trim(), shape: "hexagon" };
  if (m[6] !== undefined) return { id, label: m[6].trim(), shape: "box" };
  if (m[7] !== undefined) return { id, label: m[7].trim(), shape: "round" };
  if (m[8] !== undefined) return { id, label: m[8].trim(), shape: "diamond" };
  if (m[9] !== undefined) return { id, label: m[9].trim(), shape: "flag" };
  return { id, label: id, shape: "box" };
}

function renderNodeBox(node: FlowNode, width: Width): string[] {
  // A state diagram's start and end are a dot, not a box.
  if (node.shape === "start") return ["●"];
  if (node.shape === "end") return ["◉"];
  const lbl = node.label;
  const w = width(lbl);
  const pad = 1;
  const innerW = w + pad * 2;
  const spaces = " ".repeat(pad);
  if (node.shape === "round") {
    return [
      "╭" + "─".repeat(innerW) + "╮",
      "│" + spaces + lbl + spaces + "│",
      "╰" + "─".repeat(innerW) + "╯",
    ];
  }
  if (node.shape === "diamond") {
    return [
      "◇ " + "─".repeat(Math.max(1, innerW - 2)) + " ◇",
      "│" + spaces + lbl + spaces + "│",
      "◇ " + "─".repeat(Math.max(1, innerW - 2)) + " ◇",
    ];
  }
  if (node.shape === "stadium") {
    return [
      "╭" + "─".repeat(innerW) + "╮",
      "(" + spaces + lbl + spaces + ")",
      "╰" + "─".repeat(innerW) + "╯",
    ];
  }
  if (node.shape === "circle") {
    return [
      "╭─" + "─".repeat(innerW) + "─╮",
      "( " + spaces + lbl + spaces + " )",
      "╰─" + "─".repeat(innerW) + "─╯",
    ];
  }
  if (node.shape === "cylinder") {
    // A database: the top's rim drawn as a second line.
    return [
      "╭" + "─".repeat(innerW) + "╮",
      "├" + "─".repeat(innerW) + "┤",
      "│" + spaces + lbl + spaces + "│",
      "╰" + "─".repeat(innerW) + "╯",
    ];
  }
  if (node.shape === "hexagon") {
    return [
      "╱" + "─".repeat(innerW) + "╲",
      "<" + spaces + lbl + spaces + ">",
      "╲" + "─".repeat(innerW) + "╱",
    ];
  }
  return [
    "┌" + "─".repeat(innerW) + "┐",
    "│" + spaces + lbl + spaces + "│",
    "└" + "─".repeat(innerW) + "┘",
  ];
}

/** How a top-down level's boxes are laid out: indented, and apart. */
const BOX_ROW_INDENT = "  ";
const BOX_GAP = "    ";

/** The columns each of a level's boxes spans, as laid out with
 * `BOX_ROW_INDENT` and `BOX_GAP`. */
function boxSpans(boxes: string[][], width: Width, indent = 0): Array<{ start: number; end: number }> {
  const spans: Array<{ start: number; end: number }> = [];
  let x = BOX_ROW_INDENT.length + indent;
  for (const box of boxes) {
    const w = width(box[0]);
    spans.push({ start: x, end: x + w });
    x += w + BOX_GAP.length;
  }
  return spans;
}

const centerOf = (span: { start: number; end: number }): number =>
  span.start + Math.floor((span.end - span.start) / 2);

/** The rows joining boxes centred at the `from` columns to boxes centred at
 * the `to` columns below: a stem down from each source, a bus across when
 * the columns differ, and an arrow down into each target. An edge's label
 * (`labels[i]` for `to[i]`) is written beside its arrowhead, cut short with
 * `…` where it would run into the next arrow. */
function connectorRows(from: number[], to: number[], labels: Array<string | undefined>, width: Width): string[] {
  const all = [...from, ...to];
  const lo = Math.min(...all);
  const hi = Math.max(...all);
  const marks = (cols: number[], ch: string): string => {
    const row = Array<string>(hi + 1).fill(" ");
    for (const c of cols) row[c] = ch;
    return row.join("").trimEnd();
  };
  const arrows = (): string => {
    let row = "";
    const order = to.map((c, i) => ({ c, label: labels[i] })).sort((a, b) => a.c - b.c);
    for (let i = 0; i < order.length; i++) {
      const { c, label } = order[i];
      row = padTo(row, c, width) + "▼";
      if (!label) continue;
      const room = i + 1 < order.length ? order[i + 1].c - c - 3 : Infinity;
      if (room < 2) continue;
      let text = label;
      if (width(text) > room) {
        text = "";
        for (const ch of label) {
          if (width(text + ch) > room - 1) break;
          text += ch;
        }
        text += "…";
      }
      row += " " + text;
    }
    return row;
  };
  if (lo === hi) return [marks(from, "│"), arrows()];
  const bus = Array<string>(hi + 1).fill(" ");
  for (let c = lo; c <= hi; c++) {
    const up = from.includes(c);
    const down = to.includes(c);
    const left = c === lo;
    const right = c === hi;
    if (up && down) bus[c] = left ? "├" : right ? "┤" : "┼";
    else if (up) bus[c] = left ? "└" : right ? "┘" : "┴";
    else if (down) bus[c] = left ? "┌" : right ? "┐" : "┬";
    else bus[c] = "─";
  }
  return [marks(from, "│"), bus.join(""), arrows()];
}

/**
 * Nodes laid out top to bottom, a level per row of boxes: each node goes on
 * the level after the first node that links to it, and the links between
 * neighbouring levels are drawn as connectors. A link back up (a cycle) or
 * across is listed below the diagram with ↺.
 */
function layoutTopDown(nodes: Map<string, FlowNode>, edges: FlowEdge[], width: Width): string[] {
  const outLines: string[] = [];
  // Vertical Top-Down layout: assign levels to prevent node duplication
  const inDegree = new Map<string, number>();
  const outMap = new Map<string, FlowEdge[]>();
  for (const [id] of nodes) {
    inDegree.set(id, 0);
    outMap.set(id, []);
  }
  for (const e of edges) {
    inDegree.set(e.to, (inDegree.get(e.to) || 0) + 1);
    outMap.get(e.from)?.push(e);
  }

  const levels: string[][] = [];
  const levelOf = new Map<string, number>();
  const visited = new Set<string>();

  let currentLevel: string[] = [];
  for (const [id] of nodes) {
    if ((inDegree.get(id) || 0) === 0) {
      currentLevel.push(id);
      visited.add(id);
      levelOf.set(id, 0);
    }
  }
  if (currentLevel.length === 0 && nodes.size > 0) {
    const first = nodes.keys().next().value!;
    currentLevel.push(first);
    visited.add(first);
    levelOf.set(first, 0);
  }

  while (currentLevel.length > 0) {
    levels.push(currentLevel);
    const nextLevel: string[] = [];
    for (const u of currentLevel) {
      for (const e of outMap.get(u) || []) {
        if (!visited.has(e.to)) {
          visited.add(e.to);
          levelOf.set(e.to, levels.length);
          nextLevel.push(e.to);
        }
      }
    }
    if (nextLevel.length === 0 && visited.size < nodes.size) {
      for (const [id] of nodes) {
        if (!visited.has(id)) {
          visited.add(id);
          levelOf.set(id, levels.length);
          nextLevel.push(id);
          break;
        }
      }
    }
    currentLevel = nextLevel;
  }

  // Every level is centred on one axis, so a chain of single nodes runs
  // straight down and a branch spreads evenly under the node it leaves.
  const boxesOf = (ids: string[]) => ids.map(id => renderNodeBox(nodes.get(id)!, width));
  const levelWidth = (ids: string[]) => {
    const spans = boxSpans(boxesOf(ids), width);
    return spans[spans.length - 1].end - BOX_ROW_INDENT.length;
  };
  const axis = Math.floor(Math.max(...levels.map(levelWidth)) / 2);
  const indentOf = (ids: string[]) => axis - Math.floor(levelWidth(ids) / 2);

  for (let lvl = 0; lvl < levels.length; lvl++) {
    const nodeIds = levels[lvl];
    const boxes = boxesOf(nodeIds);
    const maxBoxH = Math.max(...boxes.map(b => b.length));
    const indent = indentOf(nodeIds);

    for (let r = 0; r < maxBoxH; r++) {
      let rowStr = BOX_ROW_INDENT + " ".repeat(indent);
      for (let b = 0; b < boxes.length; b++) {
        const box = boxes[b];
        const line = box[r] || " ".repeat(width(box[0]));
        rowStr += line + (b < boxes.length - 1 ? BOX_GAP : "");
      }
      outLines.push(rowStr);
    }

    if (lvl < levels.length - 1) {
      const nextNodeIds = levels[lvl + 1];
      const crossEdges = edges.filter(e => nodeIds.includes(e.from) && nextNodeIds.includes(e.to));
      const fromSpans = boxSpans(boxes, width, indent);
      const toSpans = boxSpans(boxesOf(nextNodeIds), width, indentOf(nextNodeIds));
      const from = nodeIds.flatMap((id, k) =>
        crossEdges.some(e => e.from === id) ? [centerOf(fromSpans[k])] : []);
      const targetIds = nextNodeIds.filter(id => crossEdges.some(e => e.to === id));
      const targets = nextNodeIds.flatMap((id, k) =>
        crossEdges.some(e => e.to === id) ? [toSpans[k]] : []);
      const labels = targetIds.map(id => crossEdges.find(e => e.to === id && e.label)?.label);
      // One box into one box that lies under its stem: straight down,
      // rather than a jog of a column or two between their centres.
      const to = from.length === 1 && targets.length === 1
        && targets[0].start < from[0] && from[0] < targets[0].end - 1
        ? from
        : targets.map(centerOf);
      if (from.length > 0 && to.length > 0) outLines.push(...connectorRows(from, to, labels, width));
    }
  }

  // Back-edges (cycles or cross-level edges)
  const crossOrBack = edges.filter(e => {
    const lFrom = levelOf.get(e.from);
    const lTo = levelOf.get(e.to);
    return lFrom !== undefined && lTo !== undefined && lTo <= lFrom;
  });
  for (const e of crossOrBack) {
    const lbl = e.label ? ` [${e.label}]` : "";
    const name = (id: string) => nodes.get(id)?.label ?? id;
    outLines.push(`  ↺ ${name(e.from)} ──> ${name(e.to)}${lbl}`);
  }
  return outLines;
}

/** A line's statements: Mermaid lets `;` separate them, even after the
 * header (`graph TD; A-->B`). A `;` inside a label or quotes is text. */
function splitStatements(line: string): string[] {
  const out: string[] = [];
  let depth = 0;
  let quoted = false;
  let start = 0;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (ch === '"') quoted = !quoted;
    else if (quoted) continue;
    else if ("[({".includes(ch)) depth++;
    else if ("])}".includes(ch)) depth = Math.max(0, depth - 1);
    else if (ch === ";" && depth === 0) {
      out.push(line.slice(start, i));
      start = i + 1;
    }
  }
  out.push(line.slice(start));
  return out.map(t => t.trim()).filter(Boolean);
}

function renderMermaidFlowchart(sourceLines: string[], maxWidth: number, width: Width): MermaidRenderResult {
  const rawLines = sourceLines.flatMap(splitStatements);
  const first = rawLines[0].toLowerCase();
  const isLR = /^\S+\s+(lr|rl)\b/.test(first);
  const nodes = new Map<string, FlowNode>();
  const edges: FlowEdge[] = [];

  // A link: an arrow (`-->`, `--->`, `---`, `-.->`, `==>`, `--o`, `--x`,
  // `<-->` …) with an optional `|label|` after it, or a label written
  // inside the link, `-- label -->` / `== label ==>` / `-. label .->`.
  // The inline-label form is tried first, so `-- yes -->` is not read as
  // `--` followed by a node named `yes`.
  const ARROW = "<?(?:-{2,}>|-{3,}|={2,}>|={3,}|-\\.+->|-\\.+-|--[ox]|==[ox])";
  const connRe = new RegExp(
    `(?:--|==|-\\.)\\s+([^|>\\s][^|>]*?)\\s+(?:${ARROW}|\\.-+>|\\.-+)|(?:${ARROW})(?:\\s*\\|([^|]*)\\|)?`,
  );

  for (let i = 1; i < rawLines.length; i++) {
    const line = rawLines[i];
    if (!line || line.startsWith("%%")) continue;
    // Grouping and styling lines say nothing about nodes or edges: `subgraph`
    // and its `end` (the grouping itself is not drawn), `style`, `classDef`,
    // `class`, `click`, `linkStyle`, `direction`.
    if (/^(subgraph|end|style|classDef|class|click|linkStyle|direction)(?=\s|$)/.test(line)) continue;

    // Handle chained connections e.g. A --> B --> C
    const parts: string[] = [];
    const edgeLabels: Array<string | undefined> = [];

    let rem = line;
    while (true) {
      const match = rem.match(connRe);
      if (!match || match.index === undefined) {
        parts.push(rem.trim());
        break;
      }
      parts.push(rem.slice(0, match.index).trim());
      const label = match[1] || match[2] || undefined;
      edgeLabels.push(label ? label.trim() : undefined);
      rem = rem.slice(match.index + match[0].length).trim();
    }

    // `A & B --> C`: each side of a link can name several nodes.
    const group = (part: string): FlowNode[] =>
      part.split(/\s+&\s+/).filter(Boolean).map(parseNodeDef);
    const remember = (node: FlowNode): void => {
      if (!nodes.has(node.id) || node.label !== node.id) nodes.set(node.id, node);
    };
    if (parts.length >= 2) {
      for (let p = 0; p < parts.length - 1; p++) {
        const fromNodes = group(parts[p]);
        const toNodes = group(parts[p + 1]);
        fromNodes.forEach(remember);
        toNodes.forEach(remember);
        for (const f of fromNodes) {
          for (const t of toNodes) edges.push({ from: f.id, to: t.id, label: edgeLabels[p] });
        }
      }
    } else if (parts.length === 1 && parts[0]) {
      for (const single of group(parts[0])) {
        if (!nodes.has(single.id)) nodes.set(single.id, single);
      }
    }
  }

  if (nodes.size === 0) {
    return { title: "Mermaid Flowchart", lines: ["(empty flowchart)"] };
  }

  const outLines: string[] = [];

  // A row of boxes can only show a chain: every node with at most one link
  // in and one out, each link going on to the next box. A branch or a merge
  // drawn as a row would read as a chain it is not, so such a graph is drawn
  // top-down instead, as is a chain too wide for the block.
  const outDeg = new Map<string, number>();
  const inDeg = new Map<string, number>();
  for (const e of edges) {
    outDeg.set(e.from, (outDeg.get(e.from) ?? 0) + 1);
    inDeg.set(e.to, (inDeg.get(e.to) ?? 0) + 1);
  }
  // The chain in order, from the node nothing links to; null when the graph
  // is not one.
  const chain = ((): FlowNode[] | null => {
    if (edges.length !== nodes.size - 1) return null;
    if ([...nodes.keys()].some(id => (outDeg.get(id) ?? 0) > 1 || (inDeg.get(id) ?? 0) > 1)) return null;
    const startId = [...nodes.keys()].find(id => !inDeg.get(id));
    if (startId === undefined) return null;
    const order: FlowNode[] = [];
    for (let id: string | undefined = startId; id !== undefined; id = edges.find(e => e.from === id)?.to) {
      if (order.some(n => n.id === id)) return null;
      order.push(nodes.get(id)!);
    }
    return order.length === nodes.size ? order : null;
  })();
  const rowWidth = [...nodes.values()].reduce((w, n) => w + width(renderNodeBox(n, width)[0]) + 9, 0);
  if (isLR && (chain === null || rowWidth > maxWidth)) {
    return { title: "Mermaid Flowchart (LR, drawn top-down)", lines: layoutTopDown(nodes, edges, width) };
  }

  if (isLR && chain !== null) {
    // A row of boxes, each link an arrow on the boxes' label row. Boxes of
    // different heights (a cylinder has four rows) share that middle row.
    const boxes = chain.map(n => renderNodeBox(n, width));
    const labelRow = (box: string[]) => Math.max(0, box.findIndex(r => r.startsWith("│") || r.startsWith("(") || r.startsWith("<")));
    const above = Math.max(...boxes.map(labelRow));
    const height = Math.max(...boxes.map((b, k) => b.length - labelRow(b) + above));
    const rows: string[] = Array(height).fill("");
    for (let i = 0; i < chain.length; i++) {
      const box = boxes[i];
      const boxW = Math.max(...box.map(width));
      const top = above - labelRow(box);
      for (let r = 0; r < height; r++) rows[r] += padTo(box[r - top] ?? "", boxW, width);
      if (i < chain.length - 1) {
        const label = edges.find(e => e.from === chain[i].id)?.label;
        const arrowText = label ? ` ── ${label} ──> ` : " ──────> ";
        for (let r = 0; r < height; r++) rows[r] += r === above ? arrowText : " ".repeat(width(arrowText));
      }
    }
    outLines.push(...rows.map(r => r.trimEnd()));
  } else {
    outLines.push(...layoutTopDown(nodes, edges, width));
  }

  return { title: isLR ? "Mermaid Flowchart (LR)" : "Mermaid Flowchart (TD)", lines: outLines };
}

function renderMermaidSequence(rawLines: string[], maxWidth: number, width: Width): MermaidRenderResult {
  const participants: string[] = [];
  const partMap = new Map<string, number>();
  const aliasMap = new Map<string, string>();
  const messages: Array<
    | { type: "msg"; from: string; arrow: string; to: string; text: string }
    | { type: "note"; participants: string[]; text: string }
    | { type: "section"; text: string }
  > = [];
  // Open `loop`/`alt`/… blocks, so each `end` closes the right one; a
  // `rect` or `box` only colours or groups and draws nothing.
  const openBlocks: boolean[] = [];
  let autonumber = 0;

  function addPart(id: string, label?: string): void {
    const cleanId = id.trim();
    if (!cleanId) return;
    if (!partMap.has(cleanId)) {
      partMap.set(cleanId, participants.length);
      participants.push(cleanId);
    }
    if (label) aliasMap.set(cleanId, label.trim());
  }

  for (const line of rawLines.slice(1)) {
    if (/^autonumber\b/i.test(line)) { autonumber = 1; continue; }
    if (/^(activate|deactivate|create|destroy|links?|properties|details)\b/i.test(line)) continue;
    const blockMatch = line.match(/^(loop|alt|opt|par|critical|break|rect|box)\b\s*(.*)$/i);
    if (blockMatch) {
      const drawn = !/^(rect|box)$/i.test(blockMatch[1]);
      openBlocks.push(drawn);
      if (drawn) messages.push({ type: "section", text: `${blockMatch[1].toLowerCase()} ${blockMatch[2]}`.trim() });
      continue;
    }
    const branchMatch = line.match(/^(else|and|option)\b\s*(.*)$/i);
    if (branchMatch) {
      messages.push({ type: "section", text: `${branchMatch[1].toLowerCase()} ${branchMatch[2]}`.trim() });
      continue;
    }
    if (/^end$/i.test(line)) {
      if (openBlocks.pop()) messages.push({ type: "section", text: "" });
      continue;
    }
    const pMatch = line.match(/^(?:participant|actor)\s+([A-Za-z0-9_\-]+)(?:\s+as\s+(.+))?/i);
    if (pMatch) {
      addPart(pMatch[1].trim(), pMatch[2] ? pMatch[2].trim() : undefined);
      continue;
    }
    // `A->>B: text`, with any of Mermaid's arrows, and an optional `+`/`-`
    // before the target that activates or deactivates it (not drawn).
    const mMatch = line.match(/^([A-Za-z0-9_\-]+?)\s*(<<-->>|<<->>|-->>|->>|--x|-x|--\)|-\)|-->|->)\s*[+-]?\s*([A-Za-z0-9_\-]+)\s*:\s*(.*)$/);
    if (mMatch) {
      addPart(mMatch[1]);
      addPart(mMatch[3]);
      const text = mMatch[4].trim();
      messages.push({
        type: "msg",
        from: mMatch[1],
        arrow: mMatch[2],
        to: mMatch[3],
        text: autonumber > 0 ? `${autonumber++}. ${text}`.trim() : text,
      });
      continue;
    }
    const noteMatch = line.match(/^note\s+(?:(over|right\s+of|left\s+of)\s+([A-Za-z0-9_\-,\s]+))\s*:\s*(.+)$/i);
    if (noteMatch) {
      const pNames = noteMatch[2].split(",").map(s => s.trim());
      for (const p of pNames) addPart(p);
      messages.push({
        type: "note",
        participants: pNames,
        text: noteMatch[3].trim(),
      });
    }
  }

  if (participants.length === 0) return { title: "Mermaid Sequence", lines: ["(empty sequence diagram)"] };

  const spacing = Math.max(16, Math.min(26, Math.floor((maxWidth - 8) / participants.length)));
  // Each lifeline is where its box's centre falls, and a box never starts
  // left of the block or runs into the box before it, however long the name.
  const boxWidthOf = (id: string) => Math.max(6, width(aliasMap.get(id) || id) + 2);
  const colCenters: number[] = [];
  for (let i = 0; i < participants.length; i++) {
    const half = Math.floor(boxWidthOf(participants[i]) / 2);
    if (i === 0) {
      colCenters.push(Math.max(4, half));
    } else {
      const prev = participants[i - 1];
      const prevEnd = colCenters[i - 1] - Math.floor(boxWidthOf(prev) / 2) + boxWidthOf(prev) + 2;
      colCenters.push(Math.max(colCenters[i - 1] + spacing, prevEnd + 2 + half));
    }
  }

  // A message's text shortened with … to leave room for its arrow.
  const fitLabel = (text: string, cols: number): string => {
    if (width(text) <= cols) return text;
    let out = "";
    for (const ch of text) {
      if (width(out + ch) > cols - 1) break;
      out += ch;
    }
    return out + "…";
  };

  const outLines: string[] = [];

  // Top header boxes
  let topRow0 = "";
  let topRow1 = "";
  let topRow2 = "";
  for (let i = 0; i < participants.length; i++) {
    const id = participants[i];
    const name = aliasMap.get(id) || id;
    const boxW = Math.max(6, width(name) + 2);
    // The lifeline leaves the box at its centre, the column it is drawn in.
    const c = Math.floor(boxW / 2);
    const box = [
      "┌" + "─".repeat(boxW) + "┐",
      "│ " + padTo(name, boxW - 2, width) + " │",
      "└" + "─".repeat(c - 1) + "┬" + "─".repeat(boxW - c) + "┘",
    ];
    const targetPos = Math.max(0, colCenters[i] - Math.floor(boxW / 2));
    topRow0 = padTo(topRow0, targetPos, width) + box[0];
    topRow1 = padTo(topRow1, targetPos, width) + box[1];
    topRow2 = padTo(topRow2, targetPos, width) + box[2];
  }
  outLines.push(topRow0, topRow1, topRow2);

  function makeLifelineRow(): string {
    let row = "";
    for (let i = 0; i < participants.length; i++) {
      row = padTo(row, colCenters[i], width) + "│";
    }
    return row;
  }

  for (const item of messages) {
    if (item.type === "section") {
      // A dotted rule across the lifelines, labelled with the block it opens
      // (`loop Every minute`, `else is well`), or bare where a block ends.
      const start = Math.max(0, colCenters[0] - 3);
      const end = colCenters[participants.length - 1] + 3;
      const label = item.text ? `┄ ${item.text} ` : "";
      const rule = label + "┄".repeat(Math.max(0, end - start - width(label)));
      outLines.push(makeLifelineRow());
      outLines.push(" ".repeat(start) + rule);
      continue;
    }
    outLines.push(makeLifelineRow());
    if (item.type === "note") {
      const indices = item.participants.map(p => partMap.get(p) ?? 0);
      const minCol = Math.min(...indices.map(idx => colCenters[idx]));
      const maxCol = Math.max(...indices.map(idx => colCenters[idx]));
      const noteW = Math.max(width(item.text) + 4, maxCol - minCol + 6);
      const startPos = Math.max(0, minCol - 2);
      outLines.push(" ".repeat(startPos) + "╭" + "─".repeat(noteW - 2) + "╮");
      outLines.push(" ".repeat(startPos) + "│ " + padTo(item.text, noteW - 4, width) + " │");
      outLines.push(" ".repeat(startPos) + "╰" + "─".repeat(noteW - 2) + "╯");
      continue;
    }

    const fromIdx = partMap.get(item.from) ?? 0;
    const toIdx = partMap.get(item.to) ?? 0;
    const c1 = colCenters[fromIdx];
    const c2 = colCenters[toIdx];

    if (c1 < c2) {
      let row = "";
      for (let i = 0; i < fromIdx; i++) row = padTo(row, colCenters[i], width) + "│";
      row = padTo(row, c1, width) + "│";
      const span = c2 - c1 - 1;
      const isDashed = item.arrow.startsWith("--");
      const label = ` ${fitLabel(item.text, Math.max(1, span - 6))} `;
      const dash = isDashed ? "- " : "─";
      let shaft = "";
      while (shaft.length < Math.max(0, span - width(label) - 1)) shaft += dash;
      const half = Math.floor(shaft.length / 2);
      // `-x` / `--x` is a message that is lost: a cross for a head.
      const arrowStr = shaft.slice(0, half) + label + shaft.slice(half) + (item.arrow.endsWith("x") ? "x" : ">");
      row += padTo(arrowStr, span, width) + "│";
      for (let i = toIdx + 1; i < participants.length; i++) row = padTo(row, colCenters[i], width) + "│";
      outLines.push(row);
    } else if (c1 > c2) {
      let row = "";
      for (let i = 0; i < toIdx; i++) row = padTo(row, colCenters[i], width) + "│";
      row = padTo(row, c2, width) + (item.arrow.endsWith("x") ? "│x" : "│<");
      const span = c1 - c2 - 1;
      const isDashed = item.arrow.startsWith("--");
      const label = ` ${fitLabel(item.text, Math.max(1, span - 6))} `;
      const dash = isDashed ? "- " : "─";
      let shaft = "";
      while (shaft.length < Math.max(0, span - width(label) - 2)) shaft += dash;
      const half = Math.floor(shaft.length / 2);
      const arrowStr = shaft.slice(0, half) + label + shaft.slice(half);
      row += padTo(arrowStr, span - 1, width) + "│";
      for (let i = fromIdx + 1; i < participants.length; i++) row = padTo(row, colCenters[i], width) + "│";
      outLines.push(row);
    } else {
      // Self message
      let row = "";
      for (let i = 0; i < fromIdx; i++) row = padTo(row, colCenters[i], width) + "│";
      row = padTo(row, c1, width) + `│──┐ ${item.text}`;
      outLines.push(row);
      let retRow = "";
      for (let i = 0; i < fromIdx; i++) retRow = padTo(retRow, colCenters[i], width) + "│";
      retRow = padTo(retRow, c1, width) + "│<─┘";
      outLines.push(retRow);
    }
  }

  outLines.push(makeLifelineRow());

  // Bottom footer boxes
  let botRow0 = "";
  let botRow1 = "";
  let botRow2 = "";
  for (let i = 0; i < participants.length; i++) {
    const id = participants[i];
    const name = aliasMap.get(id) || id;
    const boxW = Math.max(6, width(name) + 2);
    const c = Math.floor(boxW / 2);
    const box = [
      "┌" + "─".repeat(c - 1) + "┴" + "─".repeat(boxW - c) + "┐",
      "│ " + padTo(name, boxW - 2, width) + " │",
      "└" + "─".repeat(boxW) + "┘",
    ];
    const targetPos = Math.max(0, colCenters[i] - c);
    botRow0 = padTo(botRow0, targetPos, width) + box[0];
    botRow1 = padTo(botRow1, targetPos, width) + box[1];
    botRow2 = padTo(botRow2, targetPos, width) + box[2];
  }
  outLines.push(botRow0, botRow1, botRow2);

  return { title: "Mermaid Sequence", lines: outLines };
}

function renderMermaidPie(rawLines: string[], maxWidth: number, width: Width): MermaidRenderResult {
  let title = "Pie Chart";
  const items: Array<{ label: string; value: number }> = [];

  for (const line of rawLines) {
    const titleMatch = line.match(/^(?:pie\s+)?title\s+(.+)$/i);
    if (titleMatch) {
      title = titleMatch[1].trim().replace(/^["']|["']$/g, "");
      continue;
    }
    const itemMatch = line.match(/^"?([^":]+)"?\s*:\s*([0-9.]+)/);
    if (itemMatch && !line.toLowerCase().startsWith("pie")) {
      items.push({ label: itemMatch[1].trim(), value: parseFloat(itemMatch[2]) });
    }
  }

  const total = items.reduce((sum, it) => sum + it.value, 0);
  const maxLabelLen = Math.max(4, ...items.map(it => width(it.label)));
  const outLines: string[] = [];

  outLines.push(`Pie: ${title}`);
  outLines.push("─".repeat(Math.min(maxWidth - 4, Math.max(36, maxLabelLen + 30))));

  for (const item of items) {
    const pct = total > 0 ? (item.value / total) * 100 : 0;
    const barLen = 16;
    const filled = Math.round((pct / 100) * barLen);
    const bar = "█".repeat(filled) + "░".repeat(barLen - filled);
    const padLabel = item.label + " ".repeat(Math.max(0, maxLabelLen - width(item.label)));
    const valStr = item.value.toString();
    outLines.push(`  ${padLabel} │ ${valStr.padStart(5)} (${pct.toFixed(1).padStart(5)}%) [${bar}]`);
  }

  return { title: `Mermaid: ${title}`, lines: outLines };
}

function renderMermaidState(rawLines: string[], _maxWidth: number, width: Width): MermaidRenderResult {
  const nodes = new Map<string, FlowNode>();
  const edges: FlowEdge[] = [];
  const stateLabels = new Map<string, string>();
  // The composite states (`state Name {`) the line is inside, innermost last.
  const scopes: string[] = [];

  // `[*]` is where its scope starts (as a source) or ends (as a target).
  // Inside a composite state, its start is the state itself, so the
  // diagram reads "Moving → Slow"; each end is a dot of its own.
  const stateNode = (raw: string, asTarget: boolean): FlowNode => {
    const scope = scopes[scopes.length - 1];
    if (raw === "[*]") {
      if (!asTarget) {
        return scope ? { id: scope, label: scope, shape: "box" } : { id: "[*]start", label: "●", shape: "start" };
      }
      const id = `[*]end${scope ? ":" + scope : ""}`;
      return { id, label: "◉", shape: "end" };
    }
    return { id: raw, label: raw, shape: "box" };
  };

  for (let i = 1; i < rawLines.length; i++) {
    const line = rawLines[i].trim();
    if (!line || line.startsWith("%%")) continue;

    // `state "Description" as Id`, optionally opening a composite (`{`).
    const stateMatch = line.match(/^state\s+(?:"([^"]+)"\s+as\s+)?([A-Za-z0-9_\-]+)\s*(\{)?$/i);
    if (stateMatch) {
      if (stateMatch[1]) stateLabels.set(stateMatch[2], stateMatch[1]);
      if (stateMatch[3]) scopes.push(stateMatch[2]);
      continue;
    }
    if (line === "}") {
      scopes.pop();
      continue;
    }
    const labelMatch = line.match(/^([A-Za-z0-9_\-]+)\s*:\s*(.+)$/);
    if (labelMatch && !line.includes("-->")) {
      stateLabels.set(labelMatch[1], labelMatch[2].trim());
      continue;
    }

    const m = line.match(/^(\[\*\]|[A-Za-z0-9_\-]+)\s*-->\s*(\[\*\]|[A-Za-z0-9_\-]+)(?:\s*:\s*(.+))?/);
    if (m) {
      const from = stateNode(m[1], false);
      const to = stateNode(m[2], true);
      for (const n of [from, to]) if (!nodes.has(n.id)) nodes.set(n.id, n);
      edges.push({ from: from.id, to: to.id, label: m[3] ? m[3].trim() : undefined });
    }
  }

  if (edges.length === 0) {
    return { title: "Mermaid State Diagram", lines: ["(empty state diagram)"] };
  }
  for (const [id, label] of stateLabels) {
    const node = nodes.get(id);
    if (node) node.label = label;
  }
  return { title: "Mermaid State Diagram", lines: layoutTopDown(nodes, edges, width) };
}

function renderMermaidClass(rawLines: string[], maxWidth: number, width: Width): MermaidRenderResult {
  const classes = new Map<string, { name: string; members: string[] }>();
  const relations: Array<{ from: string; rel: string; to: string; label?: string }> = [];

  // The class whose `{ ... }` body is open, if any: its lines are members.
  let openClass: { name: string; members: string[] } | null = null;
  for (let i = 1; i < rawLines.length; i++) {
    const line = rawLines[i].trim();
    if (!line || line.startsWith("%%")) continue;

    if (openClass) {
      if (line === "}") openClass = null;
      else openClass.members.push(line);
      continue;
    }

    const relMatch = line.match(/^([A-Za-z0-9_\-]+)\s*(<\|--|--\|>|\*--|--\*|o--|--o|-->|<--)\s*([A-Za-z0-9_\-]+)(?:\s*:\s*(.+))?/);
    if (relMatch) {
      relations.push({ from: relMatch[1], rel: relMatch[2], to: relMatch[3], label: relMatch[4]?.trim() });
      if (!classes.has(relMatch[1])) classes.set(relMatch[1], { name: relMatch[1], members: [] });
      if (!classes.has(relMatch[3])) classes.set(relMatch[3], { name: relMatch[3], members: [] });
      continue;
    }

    const memberMatch = line.match(/^([A-Za-z0-9_\-]+)\s*:\s*(.+)$/);
    if (memberMatch) {
      const cls = classes.get(memberMatch[1]) ?? { name: memberMatch[1], members: [] };
      cls.members.push(memberMatch[2].trim());
      classes.set(memberMatch[1], cls);
      continue;
    }

    const classMatch = line.match(/^class\s+([A-Za-z0-9_\-]+)\s*(\{)?/);
    if (classMatch) {
      if (!classes.has(classMatch[1])) classes.set(classMatch[1], { name: classMatch[1], members: [] });
      if (classMatch[2]) openClass = classes.get(classMatch[1])!;
    }
  }

  const outLines: string[] = [];
  for (const [_, cls] of classes) {
    const nameW = width(cls.name);
    const memberW = cls.members.length ? Math.max(...cls.members.map(m => width(m))) : 0;
    const boxInner = Math.max(10, Math.max(nameW, memberW) + 2);

    outLines.push("┌" + "─".repeat(boxInner) + "┐");
    outLines.push("│ " + padTo(cls.name, boxInner - 2, width) + " │");
    if (cls.members.length > 0) {
      outLines.push("├" + "─".repeat(boxInner) + "┤");
      for (const m of cls.members) {
        outLines.push("│ " + padTo(m, boxInner - 2, width) + " │");
      }
    }
    outLines.push("└" + "─".repeat(boxInner) + "┘");
  }

  for (const r of relations) {
    const lbl = r.label ? ` : ${r.label}` : "";
    outLines.push(`  ${r.from} ${r.rel} ${r.to}${lbl}`);
  }

  if (classes.size === 0) outLines.push("(empty class diagram)");

  return { title: "Mermaid Class Diagram", lines: outLines };
}

export function renderMermaidDiagram(source: string, maxWidth: number, width: Width): MermaidRenderResult {
  let rawLines = source.split(/\r?\n/).map(l => l.trim()).filter(l => l.length > 0 && !l.startsWith("%%"));
  // Front matter (`---` / `title: …` / `---`) configures the diagram; skip it.
  if (rawLines[0] === "---") {
    const close = rawLines.indexOf("---", 1);
    if (close > 0) rawLines = rawLines.slice(close + 1);
  }
  if (rawLines.length === 0) {
    return { title: "Mermaid", lines: ["(empty diagram)"] };
  }

  const firstLine = rawLines[0].toLowerCase();
  if (firstLine.startsWith("graph") || firstLine.startsWith("flowchart")) {
    return renderMermaidFlowchart(rawLines, maxWidth, width);
  }
  if (firstLine.startsWith("sequencediagram")) {
    return renderMermaidSequence(rawLines, maxWidth, width);
  }
  if (firstLine.startsWith("pie")) {
    return renderMermaidPie(rawLines, maxWidth, width);
  }
  if (firstLine.startsWith("statediagram")) {
    return renderMermaidState(rawLines, maxWidth, width);
  }
  if (firstLine.startsWith("classdiagram")) {
    return renderMermaidClass(rawLines, maxWidth, width);
  }

  // A kind this file cannot draw (gantt, erDiagram, journey, …): its source,
  // all of it, under a title that says so.
  const kind = rawLines[0].split(/\s+/)[0];
  return {
    title: `Mermaid ${kind} (not drawn)`,
    lines: rawLines.map(l => "  " + l),
  };
}
