/**
 * Properties every plugin source must hold.
 *
 * `tsc` does not catch either of these, and both make the loader refuse the
 * file: a literal NUL byte, and an import cycle. The loader does not erase
 * type-only imports, so `import type` counts as an edge.
 */
import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";

const PLUGINS = join(import.meta.dirname, "..");

let failures = 0;
function check(name: string, ok: boolean, detail: string): void {
  if (ok) {
    console.log(`ok   ${name}`);
  } else {
    console.log(`FAIL ${name}\n  ${detail}`);
    failures++;
  }
}

function sources(): string[] {
  const out: string[] = [];
  for (const entry of readdirSync(PLUGINS, { withFileTypes: true })) {
    if (entry.isFile() && entry.name.endsWith(".ts")) out.push(join(PLUGINS, entry.name));
  }
  for (const entry of readdirSync(join(PLUGINS, "lib"), { withFileTypes: true })) {
    if (entry.isFile() && entry.name.endsWith(".ts")) out.push(join(PLUGINS, "lib", entry.name));
  }
  return out;
}

const offenders: string[] = [];
for (const path of sources()) {
  const bytes = readFileSync(path);
  const at = bytes.indexOf(0);
  if (at >= 0) offenders.push(`${path} (byte ${at})`);
}

check(
  "no plugin source contains a literal NUL byte",
  offenders.length === 0,
  `the loader refuses these files outright:\n  ${offenders.join("\n  ")}`,
);

// ── Import cycles ─────────────────────────────────────────────────

/** Relative specifiers a source imports, resolved to paths. Type-only imports count. */
function importsOf(path: string): string[] {
  const text = readFileSync(path, "utf8");
  const out: string[] = [];
  for (const m of text.matchAll(/\b(?:import|export)\b[^;\n]*?\bfrom\s*["'](\.[^"']+)["']/g)) {
    out.push(resolve(dirname(path), m[1]));
  }
  return out;
}

const graph = new Map<string, string[]>();
for (const path of sources()) graph.set(path, importsOf(path));

/** The first cycle found, as the path it walks, or null. */
function findCycle(): string[] | null {
  const state = new Map<string, "open" | "done">();
  const stack: string[] = [];
  const walk = (node: string): string[] | null => {
    if (state.get(node) === "done") return null;
    if (state.get(node) === "open") return [...stack.slice(stack.indexOf(node)), node];
    state.set(node, "open");
    stack.push(node);
    for (const next of graph.get(node) ?? []) {
      // Only files this scan covers.
      if (!graph.has(next)) continue;
      const found = walk(next);
      if (found) return found;
    }
    stack.pop();
    state.set(node, "done");
    return null;
  };
  for (const node of graph.keys()) {
    const found = walk(node);
    if (found) return found;
  }
  return null;
}

const cycle = findCycle();
check(
  "no plugin source imports itself in a cycle",
  cycle === null,
  `the loader cannot evaluate this cycle:\n  ${(cycle ?? []).map((p) => relative(PLUGINS, p)).join("\n    → ")}`,
);

process.exit(failures === 0 ? 0 : 1);
