#!/usr/bin/env npx ts-node
/**
 * analyze-vscode-api.ts
 *
 * Analyzes VS Code extension source code to extract API usage.
 * Can be run against a single extension directory or a directory of extensions.
 *
 * Usage:
 *   npx ts-node analyze-vscode-api.ts <extensions-dir>
 *   npx ts-node analyze-vscode-api.ts --clone-and-analyze
 */

import { execSync } from "child_process";
import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// ── Top 10 open-source VS Code extensions ──────────────────────────────
const TOP_EXTENSIONS: Record<string, string> = {
  eslint: "https://github.com/microsoft/vscode-eslint.git",
  prettier: "https://github.com/prettier/prettier-vscode.git",
  python: "https://github.com/microsoft/vscode-python.git",
  gitlens: "https://github.com/gitkraken/vscode-gitlens.git",
  "live-server": "https://github.com/ritwickdey/vscode-live-server.git",
  docker: "https://github.com/microsoft/vscode-docker.git",
  "material-icon-theme":
    "https://github.com/PKief/vscode-material-icon-theme.git",
  "path-intellisense":
    "https://github.com/ChristianKohler/PathIntellisense.git",
  "auto-rename-tag":
    "https://github.com/formulahendry/vscode-auto-rename-tag.git",
  "rest-client": "https://github.com/Huachao/vscode-restclient.git",
  cpptools: "https://github.com/microsoft/vscode-cpptools.git",
  "code-runner": "https://github.com/formulahendry/vscode-code-runner.git",
};

// ── Types ──────────────────────────────────────────────────────────────

interface ApiCall {
  namespace: string;
  method: string;
  full: string;
}

interface TypeRef {
  name: string;
  full: string;
}

interface EventSub {
  namespace: string;
  event: string;
  full: string;
}

interface ExtensionAnalysis {
  name: string;
  fileCount: number;
  apiCalls: Map<string, number>; // full api call -> count
  typeRefs: Map<string, number>; // type name -> count
  events: Map<string, number>; // event -> count
  namespaces: Map<string, number>; // namespace -> count
  destructuredImports: Map<string, number>; // import name -> count
  activationEvents: string[];
  contributes: Record<string, number>;
}

interface AggregatedResult {
  methods: Map<string, Set<string>>; // method -> set of extensions
  types: Map<string, Set<string>>;
  events: Map<string, Set<string>>;
  namespaces: Map<string, Set<string>>;
  contributes: Map<string, Set<string>>;
}

// ── File discovery ─────────────────────────────────────────────────────

const EXCLUDE_DIRS = new Set([
  "node_modules",
  ".git",
  "out",
  "dist",
  "__mocks__",
  ".vscode-test",
  "typings",
  ".nyc_output",
  "coverage",
]);

function findSourceFiles(dir: string): string[] {
  const results: string[] = [];

  function walk(currentDir: string) {
    let entries: fs.Dirent[];
    try {
      entries = fs.readdirSync(currentDir, { withFileTypes: true });
    } catch {
      return;
    }
    for (const entry of entries) {
      if (entry.isDirectory()) {
        if (!EXCLUDE_DIRS.has(entry.name)) {
          walk(path.join(currentDir, entry.name));
        }
      } else if (
        entry.isFile() &&
        /\.(ts|js|tsx|jsx)$/.test(entry.name) &&
        !entry.name.endsWith(".d.ts")
      ) {
        results.push(path.join(currentDir, entry.name));
      }
    }
  }

  walk(dir);
  return results;
}

// ── API extraction ─────────────────────────────────────────────────────

// Known VS Code API namespaces (lowercase modules on the vscode object)
const KNOWN_NAMESPACES = new Set([
  "commands",
  "debug",
  "env",
  "extensions",
  "languages",
  "notebooks",
  "scm",
  "tasks",
  "tests",
  "window",
  "workspace",
  "authentication",
  "chat",
  "comments",
  "l10n",
  "lm",
]);

// Patterns for extracting vscode API usage
const PATTERNS = {
  // vscode.namespace.method(...)
  methodCall: /\bvscode\.([a-z][A-Za-z]*)\.([a-zA-Z]+)/g,
  // vscode.TypeName
  typeRef: /\bvscode\.([A-Z][A-Za-z]*)\b/g,
  // vscode.namespace (just the namespace)
  namespace: /\bvscode\.([a-z][A-Za-z]*)\b/g,
  // import { X, Y } from 'vscode'
  destructuredImport: /import\s*\{([^}]+)\}\s*from\s*['"]vscode['"]/g,
  // Event subscriptions: vscode.xxx.onDidXxx or onWillXxx
  event: /\bvscode\.([a-z][A-Za-z]*)\.on(Did|Will)([A-Za-z]+)/g,
};

// Check if a file is likely a test/e2e file
function isTestFile(filePath: string): boolean {
  const lower = filePath.toLowerCase();
  return (
    lower.includes("/test/") ||
    lower.includes("/tests/") ||
    lower.includes("/e2e/") ||
    lower.includes("__tests__") ||
    lower.includes(".test.") ||
    lower.includes(".spec.") ||
    lower.includes("/testing/") ||
    lower.includes("/fixtures/")
  );
}

interface ExtractResult {
  methods: ApiCall[];
  types: TypeRef[];
  events: EventSub[];
  namespaces: string[];
  imports: string[];
  // Destructured usage: when `import { workspace } from 'vscode'` is used,
  // we track `workspace.getConfiguration()` as `vscode.workspace.getConfiguration`
  destructuredMethodCalls: ApiCall[];
}

function extractFromFile(filePath: string): ExtractResult {
  let content: string;
  try {
    content = fs.readFileSync(filePath, "utf-8");
  } catch {
    return {
      methods: [],
      types: [],
      events: [],
      namespaces: [],
      imports: [],
      destructuredMethodCalls: [],
    };
  }

  const methods: ApiCall[] = [];
  const types: TypeRef[] = [];
  const events: EventSub[] = [];
  const namespaces: string[] = [];
  const imports: string[] = [];
  const destructuredMethodCalls: ApiCall[] = [];

  // First, extract destructured imports to know which names come from vscode
  const importedNames = new Set<string>();
  const importPattern = new RegExp(PATTERNS.destructuredImport.source, "g");
  let match: RegExpExecArray | null;
  while ((match = importPattern.exec(content)) !== null) {
    const importNames = match[1].split(",").map((s) => {
      const trimmed = s.trim();
      // Handle "type X" imports
      const withoutType = trimmed.replace(/^type\s+/, "");
      // Handle "X as Y" → take X as the vscode name, Y as the local name
      const parts = withoutType.split(/\s+as\s+/);
      return {
        vscodeName: parts[0].trim(),
        localName: (parts[1] || parts[0]).trim(),
      };
    });
    for (const { vscodeName, localName } of importNames) {
      if (vscodeName) {
        imports.push(vscodeName);
        importedNames.add(localName);
        // Track namespace usage from destructured imports
        if (KNOWN_NAMESPACES.has(vscodeName)) {
          namespaces.push(vscodeName);
        }
      }
    }
  }

  // Track destructured namespace method calls
  // e.g., if `workspace` was imported, find `workspace.getConfiguration(...)`
  for (const imp of imports) {
    if (KNOWN_NAMESPACES.has(imp)) {
      // Find all method calls on this imported namespace
      const nsMethodPattern = new RegExp(
        `\\b${imp}\\.([a-zA-Z]+)`,
        "g"
      );
      let nsMatch: RegExpExecArray | null;
      while ((nsMatch = nsMethodPattern.exec(content)) !== null) {
        destructuredMethodCalls.push({
          namespace: imp,
          method: nsMatch[1],
          full: `vscode.${imp}.${nsMatch[1]}`,
        });
      }
    }
  }

  // Extract vscode.namespace.method calls (only for known namespaces)
  const methodPattern = new RegExp(PATTERNS.methodCall.source, "g");
  while ((match = methodPattern.exec(content)) !== null) {
    const ns = match[1];
    // Filter: only include known VS Code API namespaces
    if (KNOWN_NAMESPACES.has(ns)) {
      methods.push({
        namespace: ns,
        method: match[2],
        full: `vscode.${ns}.${match[2]}`,
      });
    }
  }

  // Extract type references
  const typePattern = new RegExp(PATTERNS.typeRef.source, "g");
  while ((match = typePattern.exec(content)) !== null) {
    types.push({
      name: match[1],
      full: `vscode.${match[1]}`,
    });
  }

  // Extract events (only for known namespaces)
  const eventPattern = new RegExp(PATTERNS.event.source, "g");
  while ((match = eventPattern.exec(content)) !== null) {
    if (KNOWN_NAMESPACES.has(match[1])) {
      events.push({
        namespace: match[1],
        event: `on${match[2]}${match[3]}`,
        full: `vscode.${match[1]}.on${match[2]}${match[3]}`,
      });
    }
  }

  // Extract namespaces (only known ones)
  const nsPattern = new RegExp(PATTERNS.namespace.source, "g");
  while ((match = nsPattern.exec(content)) !== null) {
    if (KNOWN_NAMESPACES.has(match[1])) {
      namespaces.push(match[1]);
    }
  }

  return { methods, types, events, namespaces, imports, destructuredMethodCalls };
}

// ── Per-extension analysis ─────────────────────────────────────────────

function analyzeExtension(name: string, extDir: string): ExtensionAnalysis {
  const allFiles = findSourceFiles(extDir);
  // Separate test files - we'll still analyze them but mark them
  const srcFiles = allFiles.filter((f) => !isTestFile(f));
  const files = srcFiles.length > 0 ? srcFiles : allFiles; // fallback to all if no src found

  const analysis: ExtensionAnalysis = {
    name,
    fileCount: files.length,
    apiCalls: new Map(),
    typeRefs: new Map(),
    events: new Map(),
    namespaces: new Map(),
    destructuredImports: new Map(),
    activationEvents: [],
    contributes: {},
  };

  for (const file of files) {
    const extracted = extractFromFile(file);

    // Merge both vscode.ns.method and destructured ns.method calls
    const allMethods = [...extracted.methods, ...extracted.destructuredMethodCalls];
    for (const m of allMethods) {
      analysis.apiCalls.set(m.full, (analysis.apiCalls.get(m.full) || 0) + 1);
    }
    for (const t of extracted.types) {
      analysis.typeRefs.set(t.full, (analysis.typeRefs.get(t.full) || 0) + 1);
    }
    for (const e of extracted.events) {
      analysis.events.set(e.full, (analysis.events.get(e.full) || 0) + 1);
    }
    for (const ns of extracted.namespaces) {
      analysis.namespaces.set(ns, (analysis.namespaces.get(ns) || 0) + 1);
    }
    for (const imp of extracted.imports) {
      analysis.destructuredImports.set(
        imp,
        (analysis.destructuredImports.get(imp) || 0) + 1
      );
    }
  }

  // Parse package.json
  const pkgPath = path.join(extDir, "package.json");
  if (fs.existsSync(pkgPath)) {
    try {
      const pkg = JSON.parse(fs.readFileSync(pkgPath, "utf-8"));
      analysis.activationEvents = pkg.activationEvents || [];
      const contributes = pkg.contributes || {};
      for (const [key, val] of Object.entries(contributes)) {
        analysis.contributes[key] = Array.isArray(val) ? val.length : 1;
      }
    } catch {
      // ignore parse errors
    }
  }

  return analysis;
}

// ── Aggregation ────────────────────────────────────────────────────────

function aggregate(analyses: ExtensionAnalysis[]): AggregatedResult {
  const result: AggregatedResult = {
    methods: new Map(),
    types: new Map(),
    events: new Map(),
    namespaces: new Map(),
    contributes: new Map(),
  };

  for (const ext of analyses) {
    // For cross-extension counting, we want unique methods per extension
    for (const method of ext.apiCalls.keys()) {
      if (!result.methods.has(method))
        result.methods.set(method, new Set());
      result.methods.get(method)!.add(ext.name);
    }
    for (const type of ext.typeRefs.keys()) {
      if (!result.types.has(type)) result.types.set(type, new Set());
      result.types.get(type)!.add(ext.name);
    }
    for (const event of ext.events.keys()) {
      if (!result.events.has(event))
        result.events.set(event, new Set());
      result.events.get(event)!.add(ext.name);
    }
    for (const ns of ext.namespaces.keys()) {
      if (!result.namespaces.has(ns))
        result.namespaces.set(ns, new Set());
      result.namespaces.get(ns)!.add(ext.name);
    }
    for (const contrib of Object.keys(ext.contributes)) {
      if (!result.contributes.has(contrib))
        result.contributes.set(contrib, new Set());
      result.contributes.get(contrib)!.add(ext.name);
    }
  }

  return result;
}

// ── Reporting ──────────────────────────────────────────────────────────

function sortedByUsage<T>(
  map: Map<string, Set<string>>
): [string, Set<string>][] {
  return [...map.entries()].sort((a, b) => b[1].size - a[1].size);
}

function generateReport(
  analyses: ExtensionAnalysis[],
  agg: AggregatedResult
): string {
  const lines: string[] = [];
  const w = (s = "") => lines.push(s);
  const totalExts = analyses.length;
  const thresholdEssential = Math.max(2, Math.floor(totalExts / 2));

  w("╔═══════════════════════════════════════════════════════════════╗");
  w("║    MINIMAL VS CODE API SURFACE FOR TOP EXTENSION SUPPORT     ║");
  w("╚═══════════════════════════════════════════════════════════════╝");
  w();
  w(`Based on analysis of ${totalExts} top VS Code extensions`);
  w(`Generated: ${new Date().toISOString()}`);
  w();

  // ── NAMESPACES ──
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w("  NAMESPACE USAGE (sorted by # of extensions)");
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w();
  for (const [ns, exts] of sortedByUsage(agg.namespaces)) {
    const bar = "█".repeat(exts.size) + "░".repeat(totalExts - exts.size);
    w(`  vscode.${ns.padEnd(20)} ${bar} ${exts.size}/${totalExts}  [${[...exts].sort().join(", ")}]`);
  }
  w();

  // ── TIER 1: ESSENTIAL ──
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w(
    `  TIER 1: ESSENTIAL METHODS (used by ${thresholdEssential}+ of ${totalExts} extensions)`
  );
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w();
  for (const [method, exts] of sortedByUsage(agg.methods)) {
    if (exts.size >= thresholdEssential) {
      w(`  ${method.padEnd(45)} ${exts.size} exts: ${[...exts].sort().join(", ")}`);
    }
  }
  w();

  // ── TIER 2: COMMON ──
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w(`  TIER 2: COMMON METHODS (used by 2+ extensions)`);
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w();
  for (const [method, exts] of sortedByUsage(agg.methods)) {
    if (exts.size >= 2 && exts.size < thresholdEssential) {
      w(`  ${method.padEnd(45)} ${exts.size} exts: ${[...exts].sort().join(", ")}`);
    }
  }
  w();

  // ── TIER 3: EXTENSION-SPECIFIC ──
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w(`  TIER 3: EXTENSION-SPECIFIC METHODS (used by 1 extension)`);
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w();
  for (const [method, exts] of sortedByUsage(agg.methods)) {
    if (exts.size === 1) {
      w(`  ${method.padEnd(45)} ${[...exts][0]}`);
    }
  }
  w();

  // ── TYPES ──
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w("  TYPE/CLASS USAGE (sorted by # of extensions)");
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w();
  for (const [type, exts] of sortedByUsage(agg.types)) {
    w(`  ${type.padEnd(40)} ${exts.size} exts: ${[...exts].sort().join(", ")}`);
  }
  w();

  // ── EVENTS ──
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w("  EVENT SUBSCRIPTIONS");
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w();
  for (const [event, exts] of sortedByUsage(agg.events)) {
    w(`  ${event.padEnd(50)} ${exts.size} exts: ${[...exts].sort().join(", ")}`);
  }
  w();

  // ── CONTRIBUTES ──
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w("  CONTRIBUTES (package.json) ACROSS EXTENSIONS");
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w();
  for (const [contrib, exts] of sortedByUsage(agg.contributes)) {
    w(`  contributes.${contrib.padEnd(25)} ${exts.size} exts: ${[...exts].sort().join(", ")}`);
  }
  w();

  // ── PER-EXTENSION SUMMARY ──
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w("  PER-EXTENSION API COMPLEXITY");
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w();
  for (const ext of analyses.sort((a, b) => b.apiCalls.size - a.apiCalls.size)) {
    w(
      `  ${ext.name.padEnd(25)} ${ext.apiCalls.size.toString().padStart(3)} methods, ` +
        `${ext.typeRefs.size.toString().padStart(3)} types, ` +
        `${ext.events.size.toString().padStart(2)} events, ` +
        `${Object.keys(ext.contributes).length.toString().padStart(2)} contributes, ` +
        `${ext.fileCount} files`
    );
  }
  w();

  // ── MINIMAL IMPLEMENTATION GUIDE ──
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w("  MINIMAL IMPLEMENTATION GUIDE");
  w("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  w();
  w("  To support the majority of top extensions, implement these");
  w("  namespaces in priority order:");
  w();

  const nsByUsage = sortedByUsage(agg.namespaces);
  for (const [ns, exts] of nsByUsage) {
    // Count unique methods in this namespace
    const methodsInNs = [...agg.methods.entries()].filter(([m]) =>
      m.startsWith(`vscode.${ns}.`)
    );
    w(
      `  ${exts.size >= thresholdEssential ? "★" : "·"} vscode.${ns.padEnd(20)} ` +
        `${exts.size}/${totalExts} exts, ${methodsInNs.length} unique methods`
    );
  }
  w();
  w("  ★ = essential (needed by majority of extensions)");
  w("  · = needed by some extensions");

  return lines.join("\n");
}

function generatePerExtensionReport(ext: ExtensionAnalysis): string {
  const lines: string[] = [];
  const w = (s = "") => lines.push(s);

  w(`═══ ${ext.name} ═══`);
  w(`Files analyzed: ${ext.fileCount}`);
  w();

  w("Activation Events:");
  if (ext.activationEvents.length > 0) {
    for (const e of ext.activationEvents) w(`  ${e}`);
  } else {
    w("  (none / uses *)");
  }
  w();

  w("Contributes:");
  for (const [key, count] of Object.entries(ext.contributes).sort()) {
    w(`  ${key}: ${count} entries`);
  }
  w();

  w("Namespaces (by usage count):");
  for (const [ns, count] of [...ext.namespaces.entries()].sort(
    (a, b) => b[1] - a[1]
  )) {
    w(`  ${count.toString().padStart(5)}x  vscode.${ns}`);
  }
  w();

  w("API Methods (by usage count):");
  for (const [method, count] of [...ext.apiCalls.entries()].sort(
    (a, b) => b[1] - a[1]
  )) {
    w(`  ${count.toString().padStart(5)}x  ${method}`);
  }
  w();

  w("Types/Classes (by usage count):");
  for (const [type, count] of [...ext.typeRefs.entries()].sort(
    (a, b) => b[1] - a[1]
  )) {
    w(`  ${count.toString().padStart(5)}x  ${type}`);
  }
  w();

  w("Event Subscriptions:");
  for (const [event, count] of [...ext.events.entries()].sort(
    (a, b) => b[1] - a[1]
  )) {
    w(`  ${count.toString().padStart(5)}x  ${event}`);
  }
  w();

  w("Destructured Imports:");
  for (const [imp, count] of [...ext.destructuredImports.entries()].sort(
    (a, b) => b[1] - a[1]
  )) {
    w(`  ${count.toString().padStart(5)}x  ${imp}`);
  }

  return lines.join("\n");
}

// ── JSON export for programmatic use ───────────────────────────────────

function toJSON(analyses: ExtensionAnalysis[], agg: AggregatedResult): object {
  return {
    generated: new Date().toISOString(),
    extensionCount: analyses.length,
    namespaces: Object.fromEntries(
      sortedByUsage(agg.namespaces).map(([ns, exts]) => [
        ns,
        {
          extensionCount: exts.size,
          extensions: [...exts].sort(),
          methods: [...agg.methods.entries()]
            .filter(([m]) => m.startsWith(`vscode.${ns}.`))
            .map(([m, e]) => ({
              method: m,
              extensionCount: e.size,
              extensions: [...e].sort(),
            }))
            .sort((a, b) => b.extensionCount - a.extensionCount),
        },
      ])
    ),
    types: Object.fromEntries(
      sortedByUsage(agg.types).map(([t, exts]) => [
        t,
        { extensionCount: exts.size, extensions: [...exts].sort() },
      ])
    ),
    events: Object.fromEntries(
      sortedByUsage(agg.events).map(([e, exts]) => [
        e,
        { extensionCount: exts.size, extensions: [...exts].sort() },
      ])
    ),
    contributes: Object.fromEntries(
      sortedByUsage(agg.contributes).map(([c, exts]) => [
        c,
        { extensionCount: exts.size, extensions: [...exts].sort() },
      ])
    ),
    extensions: analyses.map((ext) => ({
      name: ext.name,
      fileCount: ext.fileCount,
      uniqueMethods: ext.apiCalls.size,
      uniqueTypes: ext.typeRefs.size,
      uniqueEvents: ext.events.size,
      contributeKeys: Object.keys(ext.contributes).length,
      activationEvents: ext.activationEvents,
    })),
  };
}

// ── Clone logic ────────────────────────────────────────────────────────

function cloneExtensions(destDir: string): void {
  fs.mkdirSync(destDir, { recursive: true });

  for (const [name, url] of Object.entries(TOP_EXTENSIONS)) {
    const dest = path.join(destDir, name);
    if (fs.existsSync(dest)) {
      console.log(`  [skip] ${name} (already cloned)`);
      continue;
    }
    console.log(`  [clone] ${name}...`);
    try {
      execSync(`git clone --depth 1 --quiet ${url} ${dest}`, {
        stdio: "pipe",
      });
    } catch (e) {
      console.error(`  [WARN] Failed to clone ${name}, skipping`);
    }
  }
}

// ── Main ───────────────────────────────────────────────────────────────

function main() {
  const args = process.argv.slice(2);
  const scriptDir = __dirname;
  const cloneDir = path.join(scriptDir, "extensions");
  const analysisDir = path.join(scriptDir, "analysis");

  // Clone if requested or if extensions dir doesn't exist
  if (args.includes("--clone-and-analyze") || !fs.existsSync(cloneDir)) {
    console.log("=== Cloning extension repositories ===");
    cloneExtensions(cloneDir);
    console.log();
  }

  const extDir = args.find((a) => !a.startsWith("--")) || cloneDir;

  console.log("=== Analyzing VS Code API usage ===");
  const analyses: ExtensionAnalysis[] = [];

  // Detect if extDir contains extensions or is a single extension
  const subdirs = fs
    .readdirSync(extDir, { withFileTypes: true })
    .filter((d) => d.isDirectory() && !d.name.startsWith("."));

  const hasPkgJson = fs.existsSync(path.join(extDir, "package.json"));

  if (hasPkgJson && subdirs.length < 3) {
    // Single extension
    const name = path.basename(extDir);
    console.log(`  [analyze] ${name}`);
    analyses.push(analyzeExtension(name, extDir));
  } else {
    // Multiple extensions
    for (const dir of subdirs) {
      const fullPath = path.join(extDir, dir.name);
      console.log(`  [analyze] ${dir.name}`);
      analyses.push(analyzeExtension(dir.name, fullPath));
    }
  }

  console.log(`\nAnalyzed ${analyses.length} extensions\n`);

  // Aggregate results
  const agg = aggregate(analyses);

  // Write outputs
  fs.mkdirSync(analysisDir, { recursive: true });

  // Per-extension reports
  for (const ext of analyses) {
    const extOutDir = path.join(analysisDir, ext.name);
    fs.mkdirSync(extOutDir, { recursive: true });
    fs.writeFileSync(
      path.join(extOutDir, "api_usage.txt"),
      generatePerExtensionReport(ext)
    );
  }

  // Combined text report
  const report = generateReport(analyses, agg);
  fs.writeFileSync(path.join(analysisDir, "minimal_api_surface.txt"), report);
  console.log(report);

  // JSON export
  const jsonData = toJSON(analyses, agg);
  fs.writeFileSync(
    path.join(analysisDir, "api_usage.json"),
    JSON.stringify(jsonData, null, 2)
  );

  console.log(`\n=== Reports written to ${analysisDir}/ ===`);
  console.log(`  - Per-extension: ${analysisDir}/<ext>/api_usage.txt`);
  console.log(`  - Summary: ${analysisDir}/minimal_api_surface.txt`);
  console.log(`  - JSON data: ${analysisDir}/api_usage.json`);
}

main();
