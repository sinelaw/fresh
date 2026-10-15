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


export interface MermaidRenderResult {
  title: string;
  lines: string[];
}

interface FlowNode {
  id: string;
  label: string;
  shape: "box" | "round" | "stadium" | "cylinder" | "diamond" | "circle" | "hexagon" | "flag";
}

interface FlowEdge {
  from: string;
  to: string;
  label?: string;
}

function parseNodeDef(raw: string): FlowNode {
  const m = raw.match(/^([A-Za-z0-9_\-]+)\s*(?:\(\[([^\]]+)\]\)|\(\(([^)]+)\)\)|\[\(([^)]+)\)\]|\{\{([^}]+)\}\}|\[([^[\]]+)\]|\(([^(]+)\)|\{([^{}]+)\}|>([^\]]+)\])?$/);
  if (!m) return { id: raw, label: raw, shape: "box" };
  const id = m[1];
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
      "([" + "─".repeat(innerW) + "])",
      "│" + spaces + lbl + spaces + "│",
      "([" + "─".repeat(innerW) + "])",
    ];
  }
  if (node.shape === "circle") {
    return [
      "((" + "─".repeat(innerW) + "))",
      "│" + spaces + lbl + spaces + "│",
      "((" + "─".repeat(innerW) + "))",
    ];
  }
  if (node.shape === "cylinder") {
    return [
      "⌠" + "─".repeat(innerW) + "⌡",
      "│" + spaces + lbl + spaces + "│",
      "⌡" + "─".repeat(innerW) + "⌠",
    ];
  }
  return [
    "┌" + "─".repeat(innerW) + "┐",
    "│" + spaces + lbl + spaces + "│",
    "└" + "─".repeat(innerW) + "┘",
  ];
}

function renderMermaidFlowchart(rawLines: string[], maxWidth: number, width: Width): MermaidRenderResult {
  const first = rawLines[0].toLowerCase();
  const isLR = first.includes("lr") || first.includes("rl");
  const nodes = new Map<string, FlowNode>();
  const edges: FlowEdge[] = [];

  // Connector regex: -->|label|, -- label -->, -->, ---, -.->, ==>
  const connRe = /(-->|---|-.->|==>|--\s*([^-]+?)\s*-->|-->\|([^|]+)\|)/;

  for (let i = 1; i < rawLines.length; i++) {
    const line = rawLines[i].trim();
    if (!line || line.startsWith("%%")) continue;

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
      const label = match[2] || match[3] || undefined;
      edgeLabels.push(label ? label.trim() : undefined);
      rem = rem.slice(match.index + match[0].length).trim();
    }

    if (parts.length >= 2) {
      for (let p = 0; p < parts.length - 1; p++) {
        const fromNode = parseNodeDef(parts[p]);
        const toNode = parseNodeDef(parts[p + 1]);
        if (!nodes.has(fromNode.id) || fromNode.label !== fromNode.id) nodes.set(fromNode.id, fromNode);
        if (!nodes.has(toNode.id) || toNode.label !== toNode.id) nodes.set(toNode.id, toNode);
        edges.push({ from: fromNode.id, to: toNode.id, label: edgeLabels[p] });
      }
    } else if (parts.length === 1 && parts[0]) {
      const single = parseNodeDef(parts[0]);
      if (!nodes.has(single.id)) nodes.set(single.id, single);
    }
  }

  if (nodes.size === 0) {
    return { title: "Mermaid Flowchart", lines: ["(empty flowchart)"] };
  }

  const outLines: string[] = [];

  if (isLR) {
    // Horizontal layout
    const orderedNodes: Array<{ node: FlowNode; edgeLabel?: string }> = [];
    const seenNodeIds = new Set<string>();

    for (let eIdx = 0; eIdx < edges.length; eIdx++) {
      const e = edges[eIdx];
      const fn = nodes.get(e.from) || { id: e.from, label: e.from, shape: "box" };
      const tn = nodes.get(e.to) || { id: e.to, label: e.to, shape: "box" };
      if (!seenNodeIds.has(fn.id)) {
        orderedNodes.push({ node: fn, edgeLabel: e.label });
        seenNodeIds.add(fn.id);
      } else if (orderedNodes.length > 0 && orderedNodes[orderedNodes.length - 1].node.id === fn.id) {
        orderedNodes[orderedNodes.length - 1].edgeLabel = e.label;
      }
      if (!seenNodeIds.has(tn.id)) {
        orderedNodes.push({ node: tn });
        seenNodeIds.add(tn.id);
      }
    }

    if (orderedNodes.length === 0) {
      for (const node of nodes.values()) {
        orderedNodes.push({ node });
      }
    }

    const row0: string[] = [];
    const row1: string[] = [];
    const row2: string[] = [];

    for (let i = 0; i < orderedNodes.length; i++) {
      const item = orderedNodes[i];
      const box = renderNodeBox(item.node, width);
      const boxW = Math.max(width(box[0]), width(box[1]), width(box[2] || ""));

      row0.push(box[0].padEnd(boxW));
      row1.push(box[1].padEnd(boxW));
      row2.push((box[2] || "").padEnd(boxW));

      if (i < orderedNodes.length - 1) {
        const arrowText = item.edgeLabel ? ` ── ${item.edgeLabel} ──> ` : " ──────> ";
        const arrowW = width(arrowText);
        row0.push(" ".repeat(arrowW));
        row1.push(arrowText);
        row2.push(" ".repeat(arrowW));
      }
    }
    outLines.push(row0.join(""), row1.join(""), row2.join(""));
  } else {
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

    for (let lvl = 0; lvl < levels.length; lvl++) {
      const nodeIds = levels[lvl];
      const boxes = nodeIds.map(id => renderNodeBox(nodes.get(id)!, width));
      const maxBoxH = Math.max(...boxes.map(b => b.length));

      for (let r = 0; r < maxBoxH; r++) {
        let rowStr = "  ";
        for (let b = 0; b < boxes.length; b++) {
          const box = boxes[b];
          const line = box[r] || " ".repeat(width(box[0]));
          rowStr += line + (b < boxes.length - 1 ? "    " : "");
        }
        outLines.push(rowStr);
      }

      if (lvl < levels.length - 1) {
        const nextNodeIds = levels[lvl + 1];
        const crossEdges = edges.filter(e => nodeIds.includes(e.from) && nextNodeIds.includes(e.to));
        const label = crossEdges.find(e => e.label)?.label;
        const lblStr = label ? ` [${label}]` : "";
        if (nodeIds.length === 1 && nextNodeIds.length === 1) {
          const box0W = width(boxes[0][0]);
          const center = 2 + Math.floor(box0W / 2);
          outLines.push(" ".repeat(center) + "│" + lblStr);
          outLines.push(" ".repeat(center) + "▼");
        } else {
          outLines.push("    │" + lblStr);
          outLines.push("    ├───┐");
          outLines.push("    ▼   ▼");
        }
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
      outLines.push(`  ↺ ${e.from} ──> ${e.to}${lbl}`);
    }
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
  > = [];

  function addPart(id: string, label?: string): void {
    const cleanId = id.trim();
    if (!cleanId) return;
    if (!partMap.has(cleanId)) {
      partMap.set(cleanId, participants.length);
      participants.push(cleanId);
    }
    if (label) aliasMap.set(cleanId, label.trim());
  }

  for (const line of rawLines) {
    const pMatch = line.match(/^(?:participant|actor)\s+([A-Za-z0-9_\-]+)(?:\s+as\s+(.+))?/i);
    if (pMatch) {
      addPart(pMatch[1].trim(), pMatch[2] ? pMatch[2].trim() : undefined);
      continue;
    }
    const mMatch = line.match(/^([A-Za-z0-9_]+)\s*(-->>|->>|-->|->|-\)|--\))\s*([A-Za-z0-9_]+)\s*:\s*(.+)$/);
    if (mMatch) {
      addPart(mMatch[1]);
      addPart(mMatch[3]);
      messages.push({
        type: "msg",
        from: mMatch[1],
        arrow: mMatch[2],
        to: mMatch[3],
        text: mMatch[4].trim(),
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
  const colCenters = participants.map((_, i) => 4 + i * spacing);

  const outLines: string[] = [];

  // Top header boxes
  let topRow0 = "";
  let topRow1 = "";
  let topRow2 = "";
  for (let i = 0; i < participants.length; i++) {
    const id = participants[i];
    const name = aliasMap.get(id) || id;
    const boxW = Math.max(6, width(name) + 2);
    const box = [
      "┌" + "─".repeat(boxW) + "┐",
      "│ " + name + " │",
      "└──┬" + "─".repeat(Math.max(0, boxW - 3)) + "┘",
    ];
    const targetPos = Math.max(0, colCenters[i] - Math.floor(boxW / 2));
    topRow0 = topRow0.padEnd(targetPos) + box[0];
    topRow1 = topRow1.padEnd(targetPos) + box[1];
    topRow2 = topRow2.padEnd(targetPos) + box[2];
  }
  outLines.push(topRow0, topRow1, topRow2);

  function makeLifelineRow(): string {
    let row = "";
    for (let i = 0; i < participants.length; i++) {
      row = row.padEnd(colCenters[i]) + "│";
    }
    return row;
  }

  for (const item of messages) {
    outLines.push(makeLifelineRow());
    if (item.type === "note") {
      const indices = item.participants.map(p => partMap.get(p) ?? 0);
      const minCol = Math.min(...indices.map(idx => colCenters[idx]));
      const maxCol = Math.max(...indices.map(idx => colCenters[idx]));
      const noteW = Math.max(width(item.text) + 4, maxCol - minCol + 6);
      const startPos = Math.max(0, minCol - 2);
      outLines.push(" ".repeat(startPos) + "╭" + "─".repeat(noteW - 2) + "╮");
      outLines.push(" ".repeat(startPos) + "│ " + item.text.padEnd(noteW - 4) + " │");
      outLines.push(" ".repeat(startPos) + "╰" + "─".repeat(noteW - 2) + "╯");
      continue;
    }

    const fromIdx = partMap.get(item.from) ?? 0;
    const toIdx = partMap.get(item.to) ?? 0;
    const c1 = colCenters[fromIdx];
    const c2 = colCenters[toIdx];

    if (c1 < c2) {
      let row = "";
      for (let i = 0; i < fromIdx; i++) row = row.padEnd(colCenters[i]) + "│";
      row = row.padEnd(c1) + "│";
      const span = c2 - c1 - 1;
      const isDashed = item.arrow.startsWith("--");
      const label = ` ${item.text} `;
      const dash = isDashed ? "- " : "─";
      let shaft = "";
      while (shaft.length < Math.max(0, span - width(label) - 1)) shaft += dash;
      const half = Math.floor(shaft.length / 2);
      const arrowStr = shaft.slice(0, half) + label + shaft.slice(half) + ">";
      row += arrowStr.padEnd(span) + "│";
      for (let i = toIdx + 1; i < participants.length; i++) row = row.padEnd(colCenters[i]) + "│";
      outLines.push(row);
    } else if (c1 > c2) {
      let row = "";
      for (let i = 0; i < toIdx; i++) row = row.padEnd(colCenters[i]) + "│";
      row = row.padEnd(c2) + "│<";
      const span = c1 - c2 - 1;
      const isDashed = item.arrow.startsWith("--");
      const label = ` ${item.text} `;
      const dash = isDashed ? "- " : "─";
      let shaft = "";
      while (shaft.length < Math.max(0, span - width(label) - 2)) shaft += dash;
      const half = Math.floor(shaft.length / 2);
      const arrowStr = shaft.slice(0, half) + label + shaft.slice(half);
      row += arrowStr.padEnd(span - 1) + "│";
      for (let i = fromIdx + 1; i < participants.length; i++) row = row.padEnd(colCenters[i]) + "│";
      outLines.push(row);
    } else {
      // Self message
      let row = "";
      for (let i = 0; i < fromIdx; i++) row = row.padEnd(colCenters[i]) + "│";
      row = row.padEnd(c1) + `│──┐ ${item.text}`;
      outLines.push(row);
      let retRow = "";
      for (let i = 0; i < fromIdx; i++) retRow = retRow.padEnd(colCenters[i]) + "│";
      retRow = retRow.padEnd(c1) + "│<─┘";
      outLines.push(retRow);
    }
  }

  outLines.push(makeLifelineRow());

  // Bottom footer boxes
  let botRow0 = "";
  let botRow1 = "";
  for (let i = 0; i < participants.length; i++) {
    const id = participants[i];
    const name = aliasMap.get(id) || id;
    const boxW = Math.max(6, width(name) + 2);
    const box = [
      "│ " + name + " │",
      "└" + "─".repeat(boxW) + "┘",
    ];
    const targetPos = Math.max(0, colCenters[i] - Math.floor(boxW / 2));
    botRow0 = botRow0.padEnd(targetPos) + box[0];
    botRow1 = botRow1.padEnd(targetPos) + box[1];
  }
  outLines.push(botRow0, botRow1);

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

function renderMermaidState(rawLines: string[], maxWidth: number, width: Width): MermaidRenderResult {
  const transitions: Array<{ from: string; to: string; event?: string }> = [];
  const stateLabels = new Map<string, string>();

  for (let i = 1; i < rawLines.length; i++) {
    const line = rawLines[i].trim();
    if (!line || line.startsWith("%%")) continue;

    const descMatch = line.match(/^state\s+"([^"]+)"\s+as\s+([A-Za-z0-9_\-]+)/i);
    if (descMatch) {
      stateLabels.set(descMatch[2], descMatch[1]);
      continue;
    }

    const m = line.match(/^(\[\*\]|[A-Za-z0-9_\-]+)\s*-->\s*(\[\*\]|[A-Za-z0-9_\-]+)(?:\s*:\s*(.+))?/);
    if (m) {
      transitions.push({
        from: m[1],
        to: m[2],
        event: m[3] ? m[3].trim() : undefined,
      });
    }
  }

  const outLines: string[] = [];
  const visited = new Set<string>();

  for (let i = 0; i < transitions.length; i++) {
    const tr = transitions[i];
    if (!visited.has(tr.from)) {
      if (tr.from === "[*]") {
        outLines.push("   ● (Start)");
      } else {
        const lbl = stateLabels.get(tr.from) || tr.from;
        outLines.push(...renderNodeBox({ id: tr.from, label: lbl, shape: "box" }, width));
      }
      visited.add(tr.from);
    }

    const eventText = tr.event ? ` [${tr.event}]` : "";
    outLines.push("   │" + eventText);
    outLines.push("   ▼");

    if (i === transitions.length - 1 || !visited.has(tr.to)) {
      if (tr.to === "[*]") {
        outLines.push("   ◎ (End)");
      } else {
        const lbl = stateLabels.get(tr.to) || tr.to;
        outLines.push(...renderNodeBox({ id: tr.to, label: lbl, shape: "box" }, width));
      }
      visited.add(tr.to);
    }
  }

  if (transitions.length === 0) {
    outLines.push("(empty state diagram)");
  }

  return { title: "Mermaid State Diagram", lines: outLines };
}

function renderMermaidClass(rawLines: string[], maxWidth: number, width: Width): MermaidRenderResult {
  const classes = new Map<string, { name: string; members: string[] }>();
  const relations: Array<{ from: string; rel: string; to: string; label?: string }> = [];

  for (let i = 1; i < rawLines.length; i++) {
    const line = rawLines[i].trim();
    if (!line || line.startsWith("%%")) continue;

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

    const classMatch = line.match(/^class\s+([A-Za-z0-9_\-]+)/);
    if (classMatch) {
      if (!classes.has(classMatch[1])) classes.set(classMatch[1], { name: classMatch[1], members: [] });
    }
  }

  const outLines: string[] = [];
  for (const [_, cls] of classes) {
    const nameW = width(cls.name);
    const memberW = cls.members.length ? Math.max(...cls.members.map(m => width(m))) : 0;
    const boxInner = Math.max(10, Math.max(nameW, memberW) + 2);

    outLines.push("┌" + "─".repeat(boxInner) + "┐");
    outLines.push("│ " + cls.name.padEnd(boxInner - 2) + " │");
    if (cls.members.length > 0) {
      outLines.push("├" + "─".repeat(boxInner) + "┤");
      for (const m of cls.members) {
        outLines.push("│ " + m.padEnd(boxInner - 2) + " │");
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
  const rawLines = source.split(/\r?\n/).map(l => l.trim()).filter(l => l.length > 0 && !l.startsWith("%%"));
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

  return {
    title: "Mermaid Diagram",
    lines: rawLines.slice(0, 15).map(l => "  " + l),
  };
}
