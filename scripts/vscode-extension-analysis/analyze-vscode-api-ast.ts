#!/usr/bin/env npx ts-node
/**
 * analyze-vscode-api-ast.ts
 *
 * AST-based analysis of VS Code extension source code.
 * Uses the TypeScript compiler API to precisely extract:
 *   1. vscode API usage (methods, properties, types, events)
 *   2. Node.js builtin usage (fs, child_process, net, etc.)
 *   3. Environment access (process.env, process.cwd, etc.)
 *   4. Network/IO (fetch, http, external HTTP libraries)
 *   5. Shell execution (exec, spawn, etc.)
 *
 * Usage:
 *   npx ts-node analyze-vscode-api-ast.ts <extensions-dir>
 *   npx ts-node analyze-vscode-api-ast.ts --clone-and-analyze
 */

import { execSync } from "child_process";
import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "url";
import * as ts from "typescript";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// ── Top 12 open-source VS Code extensions ──────────────────────────────
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

// ── Categories of side effects ─────────────────────────────────────────

// Node.js builtin modules that represent platform/environment interaction
const NODE_BUILTINS = new Set([
  "fs",
  "fs/promises",
  "path",
  "os",
  "child_process",
  "net",
  "http",
  "https",
  "dgram",
  "dns",
  "tls",
  "crypto",
  "stream",
  "zlib",
  "util",
  "events",
  "buffer",
  "url",
  "querystring",
  "readline",
  "worker_threads",
  "cluster",
  "process",
  "assert",
  "timers",
  "perf_hooks",
  "v8",
]);

// Known VS Code API namespaces
const VSCODE_NAMESPACES = new Set([
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

// Network/HTTP libraries (npm packages)
const NETWORK_PACKAGES = new Set([
  "axios",
  "node-fetch",
  "got",
  "request",
  "superagent",
  "undici",
  "cross-fetch",
  "isomorphic-fetch",
  "bent",
  "needle",
  "phin",
  "ky",
]);

// ── Types ──────────────────────────────────────────────────────────────

interface ImportInfo {
  moduleSpecifier: string; // e.g. 'vscode', 'fs', 'axios'
  importedNames: Map<string, string>; // localName -> originalName
  namespaceImport?: string; // e.g. 'vscode' for `import * as vscode from 'vscode'`
  defaultImport?: string; // e.g. 'axios' for `import axios from 'axios'`
  isTypeOnly: boolean;
}

interface ApiUsage {
  api: string; // e.g. 'vscode.workspace.getConfiguration'
  category: string; // 'vscode' | 'node-builtin' | 'network' | 'process' | 'shell' | 'global'
  file: string;
  line: number;
  isTypeOnly: boolean; // true if used only in type position
}

interface ExtensionAnalysis {
  name: string;
  fileCount: number;
  usages: ApiUsage[];
  // Aggregated
  vscodeApi: Map<string, { count: number; files: Set<string> }>;
  nodeBuiltins: Map<string, { count: number; files: Set<string> }>;
  networkUsage: Map<string, { count: number; files: Set<string> }>;
  processUsage: Map<string, { count: number; files: Set<string> }>;
  shellUsage: Map<string, { count: number; files: Set<string> }>;
  globalSideEffects: Map<string, { count: number; files: Set<string> }>;
  // package.json data
  activationEvents: string[];
  contributes: Record<string, number>;
  dependencies: Record<string, string>;
  vscodeTypes: Map<string, { count: number; files: Set<string> }>;
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
  "test",
  "tests",
  "__tests__",
  "e2e",
  "testing",
  "fixtures",
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

// ── AST Analysis ───────────────────────────────────────────────────────

function analyzeFile(filePath: string): ApiUsage[] {
  let sourceText: string;
  try {
    sourceText = fs.readFileSync(filePath, "utf-8");
  } catch {
    return [];
  }

  const sourceFile = ts.createSourceFile(
    filePath,
    sourceText,
    ts.ScriptTarget.Latest,
    true,
    filePath.endsWith(".tsx") || filePath.endsWith(".jsx")
      ? ts.ScriptKind.TSX
      : undefined
  );

  const usages: ApiUsage[] = [];
  const imports = new Map<string, ImportInfo>(); // localName -> ImportInfo

  // Pass 1: Collect all imports
  function collectImports(node: ts.Node) {
    if (ts.isImportDeclaration(node)) {
      const moduleSpec = (node.moduleSpecifier as ts.StringLiteral).text;
      const isTypeOnly = node.importClause?.isTypeOnly ?? false;
      const importInfo: ImportInfo = {
        moduleSpecifier: moduleSpec,
        importedNames: new Map(),
        isTypeOnly,
      };

      if (node.importClause) {
        // Default import: import foo from 'bar'
        if (node.importClause.name) {
          importInfo.defaultImport = node.importClause.name.text;
          imports.set(node.importClause.name.text, importInfo);
        }

        if (node.importClause.namedBindings) {
          // Namespace import: import * as foo from 'bar'
          if (ts.isNamespaceImport(node.importClause.namedBindings)) {
            importInfo.namespaceImport =
              node.importClause.namedBindings.name.text;
            imports.set(node.importClause.namedBindings.name.text, importInfo);
          }
          // Named imports: import { a, b as c } from 'bar'
          else if (ts.isNamedImports(node.importClause.namedBindings)) {
            for (const spec of node.importClause.namedBindings.elements) {
              const localName = spec.name.text;
              const originalName = spec.propertyName
                ? spec.propertyName.text
                : spec.name.text;
              const specIsTypeOnly = spec.isTypeOnly || isTypeOnly;
              importInfo.importedNames.set(localName, originalName);
              imports.set(localName, {
                ...importInfo,
                isTypeOnly: specIsTypeOnly,
              });
            }
          }
        }
      }
    }

    // Also handle require() calls
    if (
      ts.isCallExpression(node) &&
      ts.isIdentifier(node.expression) &&
      node.expression.text === "require" &&
      node.arguments.length === 1 &&
      ts.isStringLiteral(node.arguments[0])
    ) {
      const moduleSpec = (node.arguments[0] as ts.StringLiteral).text;
      const parent = node.parent;

      if (ts.isVariableDeclaration(parent) && parent.name) {
        if (ts.isIdentifier(parent.name)) {
          // const foo = require('bar')
          const importInfo: ImportInfo = {
            moduleSpecifier: moduleSpec,
            importedNames: new Map(),
            defaultImport: parent.name.text,
            isTypeOnly: false,
          };
          imports.set(parent.name.text, importInfo);
        } else if (ts.isObjectBindingPattern(parent.name)) {
          // const { a, b } = require('bar')
          const importInfo: ImportInfo = {
            moduleSpecifier: moduleSpec,
            importedNames: new Map(),
            isTypeOnly: false,
          };
          for (const elem of parent.name.elements) {
            if (ts.isIdentifier(elem.name)) {
              const localName = elem.name.text;
              const originalName =
                elem.propertyName && ts.isIdentifier(elem.propertyName)
                  ? elem.propertyName.text
                  : localName;
              importInfo.importedNames.set(localName, originalName);
              imports.set(localName, importInfo);
            }
          }
        }
      }
    }

    ts.forEachChild(node, collectImports);
  }

  collectImports(sourceFile);

  // Pass 2: Find all usages of imported names
  function isInTypePosition(node: ts.Node): boolean {
    const parent = node.parent;
    if (!parent) return false;
    return (
      ts.isTypeReferenceNode(parent) ||
      ts.isTypeQueryNode(parent) ||
      ts.isExpressionWithTypeArguments(parent) ||
      ts.isInterfaceDeclaration(parent) ||
      ts.isTypeAliasDeclaration(parent) ||
      (ts.isParameter(parent) && parent.type !== undefined && node === parent.type) ||
      (ts.isPropertyDeclaration(parent) && parent.type !== undefined && node === parent.type) ||
      (ts.isPropertySignature(parent) && parent.type !== undefined && node === parent.type)
    );
  }

  function getAccessChain(node: ts.Node): string[] {
    if (ts.isPropertyAccessExpression(node)) {
      return [...getAccessChain(node.expression), node.name.text];
    }
    if (ts.isIdentifier(node)) {
      return [node.text];
    }
    return [];
  }

  function categorizeModule(moduleSpec: string): string {
    if (moduleSpec === "vscode") return "vscode";
    // Strip node: prefix
    const cleaned = moduleSpec.replace(/^node:/, "");
    if (NODE_BUILTINS.has(cleaned)) return "node-builtin";
    if (NETWORK_PACKAGES.has(moduleSpec)) return "network";
    return "external";
  }

  function addUsage(
    api: string,
    category: string,
    node: ts.Node,
    isTypeOnly: boolean = false
  ) {
    const { line } = sourceFile.getLineAndCharacterOfPosition(
      node.getStart(sourceFile)
    );
    usages.push({
      api,
      category,
      file: filePath,
      line: line + 1,
      isTypeOnly,
    });
  }

  function visitUsages(node: ts.Node) {
    // Property access chains: foo.bar.baz
    if (ts.isPropertyAccessExpression(node)) {
      const chain = getAccessChain(node);
      if (chain.length >= 2) {
        const rootName = chain[0];
        const importInfo = imports.get(rootName);

        if (importInfo) {
          const moduleSpec = importInfo.moduleSpecifier;
          const category = categorizeModule(moduleSpec);
          const typeOnly = isInTypePosition(node) || importInfo.isTypeOnly;

          if (category === "vscode") {
            if (importInfo.namespaceImport === rootName) {
              // vscode.workspace.getConfiguration
              const apiPath = chain.slice(1).join(".");
              addUsage(`vscode.${apiPath}`, "vscode", node, typeOnly);
            } else {
              // Destructured: workspace.getConfiguration
              const originalName =
                importInfo.importedNames.get(rootName) || rootName;
              const apiPath = [originalName, ...chain.slice(1)].join(".");
              addUsage(`vscode.${apiPath}`, "vscode", node, typeOnly);
            }
          } else if (category === "node-builtin") {
            const cleaned = moduleSpec.replace(/^node:/, "");
            if (importInfo.namespaceImport === rootName || importInfo.defaultImport === rootName) {
              const apiPath = chain.slice(1).join(".");
              addUsage(`${cleaned}.${apiPath}`, category, node);

              // Special: child_process.exec/spawn → shell
              if (
                (cleaned === "child_process" || cleaned === "child_process") &&
                ["exec", "execSync", "spawn", "spawnSync", "execFile", "execFileSync", "fork"].includes(chain[1])
              ) {
                addUsage(`${cleaned}.${chain[1]}`, "shell", node);
              }
            } else {
              const originalName =
                importInfo.importedNames.get(rootName) || rootName;
              const apiPath = [originalName, ...chain.slice(1)].join(".");
              addUsage(`${cleaned}.${apiPath}`, category, node);
            }
          } else if (category === "network") {
            const apiPath = chain.join(".");
            addUsage(apiPath, "network", node);
          }
        }

        // Global process.* access
        if (rootName === "process" && !imports.has("process")) {
          const apiPath = chain.join(".");
          addUsage(apiPath, "process", node);
        }
      }
    }

    // Standalone identifier usage (for destructured imports used directly)
    if (
      ts.isIdentifier(node) &&
      !ts.isPropertyAccessExpression(node.parent) // not part of a.b chain
    ) {
      // Skip if this identifier is the right side of a property access
      if (
        ts.isPropertyAccessExpression(node.parent) &&
        node.parent.name === node
      ) {
        // This is the .name part of a.name, skip
      } else {
        const importInfo = imports.get(node.text);
        if (importInfo) {
          const moduleSpec = importInfo.moduleSpecifier;
          const category = categorizeModule(moduleSpec);

          // Type references from vscode (e.g., `Uri` used as a type)
          if (category === "vscode" && isInTypePosition(node)) {
            const originalName =
              importInfo.importedNames.get(node.text) || node.text;
            addUsage(`vscode.${originalName}`, "vscode", node, true);
          }
        }
      }
    }

    // Call expressions for specific globals
    if (ts.isCallExpression(node)) {
      // Global fetch()
      if (ts.isIdentifier(node.expression) && node.expression.text === "fetch") {
        addUsage("globalThis.fetch", "network", node);
      }
      // setTimeout, setInterval, setImmediate
      if (
        ts.isIdentifier(node.expression) &&
        ["setTimeout", "setInterval", "setImmediate", "queueMicrotask"].includes(
          node.expression.text
        )
      ) {
        addUsage(`globalThis.${node.expression.text}`, "global", node);
      }
    }

    // Direct function calls on destructured node builtins
    // e.g., readFileSync() where readFileSync was imported from 'fs'
    if (
      ts.isCallExpression(node) &&
      ts.isIdentifier(node.expression)
    ) {
      const name = node.expression.text;
      const importInfo = imports.get(name);
      if (importInfo) {
        const moduleSpec = importInfo.moduleSpecifier;
        const cleaned = moduleSpec.replace(/^node:/, "");
        const category = categorizeModule(moduleSpec);
        if (category === "node-builtin") {
          const originalName = importInfo.importedNames.get(name) || name;
          addUsage(`${cleaned}.${originalName}`, category, node);

          if (
            cleaned === "child_process" &&
            ["exec", "execSync", "spawn", "spawnSync", "execFile", "execFileSync", "fork"].includes(originalName)
          ) {
            addUsage(`${cleaned}.${originalName}`, "shell", node);
          }
        } else if (category === "network") {
          addUsage(`${moduleSpec}.${name}`, "network", node);
        }
      }
    }

    ts.forEachChild(node, visitUsages);
  }

  visitUsages(sourceFile);

  return usages;
}

// ── Per-extension analysis ─────────────────────────────────────────────

function analyzeExtension(name: string, extDir: string): ExtensionAnalysis {
  const files = findSourceFiles(extDir);

  const analysis: ExtensionAnalysis = {
    name,
    fileCount: files.length,
    usages: [],
    vscodeApi: new Map(),
    nodeBuiltins: new Map(),
    networkUsage: new Map(),
    processUsage: new Map(),
    shellUsage: new Map(),
    globalSideEffects: new Map(),
    activationEvents: [],
    contributes: {},
    dependencies: {},
    vscodeTypes: new Map(),
  };

  for (const file of files) {
    const fileUsages = analyzeFile(file);
    analysis.usages.push(...fileUsages);

    for (const usage of fileUsages) {
      let targetMap: Map<string, { count: number; files: Set<string> }>;

      if (usage.category === "vscode") {
        if (usage.isTypeOnly) {
          targetMap = analysis.vscodeTypes;
        } else {
          targetMap = analysis.vscodeApi;
        }
      } else if (usage.category === "node-builtin") {
        targetMap = analysis.nodeBuiltins;
      } else if (usage.category === "network") {
        targetMap = analysis.networkUsage;
      } else if (usage.category === "process") {
        targetMap = analysis.processUsage;
      } else if (usage.category === "shell") {
        targetMap = analysis.shellUsage;
      } else if (usage.category === "global") {
        targetMap = analysis.globalSideEffects;
      } else {
        continue;
      }

      const existing = targetMap.get(usage.api);
      if (existing) {
        existing.count++;
        existing.files.add(usage.file);
      } else {
        targetMap.set(usage.api, { count: 1, files: new Set([usage.file]) });
      }
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
        analysis.contributes[key] = Array.isArray(val)
          ? (val as unknown[]).length
          : 1;
      }
      analysis.dependencies = {
        ...(pkg.dependencies || {}),
        ...(pkg.devDependencies || {}),
      };
    } catch {
      // ignore
    }
  }

  return analysis;
}

// ── Aggregation ────────────────────────────────────────────────────────

interface AggregatedResult {
  vscodeApi: Map<string, Set<string>>; // api -> set of extensions
  vscodeTypes: Map<string, Set<string>>;
  nodeBuiltins: Map<string, Set<string>>;
  networkUsage: Map<string, Set<string>>;
  processUsage: Map<string, Set<string>>;
  shellUsage: Map<string, Set<string>>;
  globalSideEffects: Map<string, Set<string>>;
  contributes: Map<string, Set<string>>;
}

function aggregate(analyses: ExtensionAnalysis[]): AggregatedResult {
  const result: AggregatedResult = {
    vscodeApi: new Map(),
    vscodeTypes: new Map(),
    nodeBuiltins: new Map(),
    networkUsage: new Map(),
    processUsage: new Map(),
    shellUsage: new Map(),
    globalSideEffects: new Map(),
    contributes: new Map(),
  };

  function addToMap(
    map: Map<string, Set<string>>,
    key: string,
    extName: string
  ) {
    if (!map.has(key)) map.set(key, new Set());
    map.get(key)!.add(extName);
  }

  for (const ext of analyses) {
    for (const api of ext.vscodeApi.keys())
      addToMap(result.vscodeApi, api, ext.name);
    for (const t of ext.vscodeTypes.keys())
      addToMap(result.vscodeTypes, t, ext.name);
    for (const api of ext.nodeBuiltins.keys())
      addToMap(result.nodeBuiltins, api, ext.name);
    for (const api of ext.networkUsage.keys())
      addToMap(result.networkUsage, api, ext.name);
    for (const api of ext.processUsage.keys())
      addToMap(result.processUsage, api, ext.name);
    for (const api of ext.shellUsage.keys())
      addToMap(result.shellUsage, api, ext.name);
    for (const api of ext.globalSideEffects.keys())
      addToMap(result.globalSideEffects, api, ext.name);
    for (const c of Object.keys(ext.contributes))
      addToMap(result.contributes, c, ext.name);
  }

  return result;
}

// ── Report generation ──────────────────────────────────────────────────

function sortedByUsage(
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
  const totalExts = analyses.filter((a) => a.fileCount > 0).length;

  w("# VS Code Extension API & Side Effects Analysis (AST-based)");
  w();
  w(`Analyzed ${analyses.length} extensions (${totalExts} with source code)`);
  w(`Generated: ${new Date().toISOString()}`);
  w();

  // ── vscode API ──
  w("## 1. vscode API Methods & Properties");
  w();
  for (const [api, exts] of sortedByUsage(agg.vscodeApi)) {
    const bar = "█".repeat(exts.size) + "░".repeat(totalExts - exts.size);
    w(
      `  ${bar} ${String(exts.size).padStart(2)}/${totalExts}  ${api.padEnd(55)} [${[...exts].sort().join(", ")}]`
    );
  }
  w();

  // ── vscode Types ──
  w("## 2. vscode Types & Classes (type-position only)");
  w();
  for (const [t, exts] of sortedByUsage(agg.vscodeTypes)) {
    if (exts.size >= 2) {
      w(
        `  ${exts.size}/${totalExts}  ${t.padEnd(45)} [${[...exts].sort().join(", ")}]`
      );
    }
  }
  w();

  // ── Node builtins ──
  w("## 3. Node.js Builtin Usage");
  w();
  for (const [api, exts] of sortedByUsage(agg.nodeBuiltins)) {
    if (exts.size >= 2) {
      w(
        `  ${exts.size}/${totalExts}  ${api.padEnd(45)} [${[...exts].sort().join(", ")}]`
      );
    }
  }
  w();
  w("  (1-extension-only entries omitted, see JSON output)");
  w();

  // ── Process access ──
  w("## 4. Process/Environment Access");
  w();
  for (const [api, exts] of sortedByUsage(agg.processUsage)) {
    w(
      `  ${exts.size}/${totalExts}  ${api.padEnd(45)} [${[...exts].sort().join(", ")}]`
    );
  }
  w();

  // ── Shell execution ──
  w("## 5. Shell/Process Execution");
  w();
  for (const [api, exts] of sortedByUsage(agg.shellUsage)) {
    w(
      `  ${exts.size}/${totalExts}  ${api.padEnd(45)} [${[...exts].sort().join(", ")}]`
    );
  }
  w();

  // ── Network ──
  w("## 6. Network/HTTP Usage");
  w();
  for (const [api, exts] of sortedByUsage(agg.networkUsage)) {
    w(
      `  ${exts.size}/${totalExts}  ${api.padEnd(45)} [${[...exts].sort().join(", ")}]`
    );
  }
  w();

  // ── Globals ──
  w("## 7. Global Side Effects");
  w();
  for (const [api, exts] of sortedByUsage(agg.globalSideEffects)) {
    w(
      `  ${exts.size}/${totalExts}  ${api.padEnd(45)} [${[...exts].sort().join(", ")}]`
    );
  }
  w();

  // ── Contributes ──
  w("## 8. package.json Contributes");
  w();
  for (const [c, exts] of sortedByUsage(agg.contributes)) {
    w(
      `  ${exts.size}/${totalExts}  contributes.${c.padEnd(35)} [${[...exts].sort().join(", ")}]`
    );
  }
  w();

  // ── Per-extension summary ──
  w("## 9. Per-Extension Complexity");
  w();
  w(
    "  Extension              Files  vscodeAPI  Types  Node   Process  Shell  Network  Contributes"
  );
  w(
    "  ─────────────────────  ─────  ────────  ─────  ─────  ───────  ─────  ───────  ───────────"
  );
  for (const ext of analyses.sort(
    (a, b) => b.vscodeApi.size - a.vscodeApi.size
  )) {
    w(
      `  ${ext.name.padEnd(23)} ${String(ext.fileCount).padStart(5)}  ` +
        `${String(ext.vscodeApi.size).padStart(8)}  ` +
        `${String(ext.vscodeTypes.size).padStart(5)}  ` +
        `${String(ext.nodeBuiltins.size).padStart(5)}  ` +
        `${String(ext.processUsage.size).padStart(7)}  ` +
        `${String(ext.shellUsage.size).padStart(5)}  ` +
        `${String(ext.networkUsage.size).padStart(7)}  ` +
        `${String(Object.keys(ext.contributes).length).padStart(11)}`
    );
  }
  w();

  return lines.join("\n");
}

// ── JSON export ────────────────────────────────────────────────────────

function mapToObj(
  map: Map<string, Set<string>>
): Record<string, { count: number; extensions: string[] }> {
  const result: Record<string, { count: number; extensions: string[] }> = {};
  for (const [key, exts] of sortedByUsage(map)) {
    result[key] = { count: exts.size, extensions: [...exts].sort() };
  }
  return result;
}

function detailMapToObj(
  map: Map<string, { count: number; files: Set<string> }>
): Record<string, { count: number; files: string[] }> {
  const result: Record<string, { count: number; files: string[] }> = {};
  for (const [key, val] of [...map.entries()].sort(
    (a, b) => b[1].count - a[1].count
  )) {
    result[key] = { count: val.count, files: [...val.files] };
  }
  return result;
}

function toJSON(
  analyses: ExtensionAnalysis[],
  agg: AggregatedResult
): object {
  return {
    generated: new Date().toISOString(),
    method: "ast",
    extensionCount: analyses.length,
    crossExtension: {
      vscodeApi: mapToObj(agg.vscodeApi),
      vscodeTypes: mapToObj(agg.vscodeTypes),
      nodeBuiltins: mapToObj(agg.nodeBuiltins),
      processUsage: mapToObj(agg.processUsage),
      shellUsage: mapToObj(agg.shellUsage),
      networkUsage: mapToObj(agg.networkUsage),
      globalSideEffects: mapToObj(agg.globalSideEffects),
      contributes: mapToObj(agg.contributes),
    },
    extensions: analyses.map((ext) => ({
      name: ext.name,
      fileCount: ext.fileCount,
      activationEvents: ext.activationEvents,
      contributes: ext.contributes,
      vscodeApi: detailMapToObj(ext.vscodeApi),
      vscodeTypes: detailMapToObj(ext.vscodeTypes),
      nodeBuiltins: detailMapToObj(ext.nodeBuiltins),
      processUsage: detailMapToObj(ext.processUsage),
      shellUsage: detailMapToObj(ext.shellUsage),
      networkUsage: detailMapToObj(ext.networkUsage),
      globalSideEffects: detailMapToObj(ext.globalSideEffects),
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
    } catch {
      console.error(`  [WARN] Failed to clone ${name}, skipping`);
    }
  }
}

// ── Main ───────────────────────────────────────────────────────────────

function main() {
  const args = process.argv.slice(2);
  const cloneDir = path.join(__dirname, "extensions");
  const analysisDir = path.join(__dirname, "analysis");

  if (args.includes("--clone-and-analyze") || !fs.existsSync(cloneDir)) {
    console.log("=== Cloning extension repositories ===");
    cloneExtensions(cloneDir);
    console.log();
  }

  const extDir = args.find((a) => !a.startsWith("--")) || cloneDir;

  console.log("=== Analyzing VS Code extension API usage (AST-based) ===");
  const analyses: ExtensionAnalysis[] = [];

  const subdirs = fs
    .readdirSync(extDir, { withFileTypes: true })
    .filter((d) => d.isDirectory() && !d.name.startsWith("."));

  for (const dir of subdirs) {
    const fullPath = path.join(extDir, dir.name);
    console.log(`  [analyze] ${dir.name}`);
    analyses.push(analyzeExtension(dir.name, fullPath));
  }

  console.log(`\nAnalyzed ${analyses.length} extensions\n`);

  const agg = aggregate(analyses);

  // Write outputs
  fs.mkdirSync(analysisDir, { recursive: true });

  const report = generateReport(analyses, agg);
  fs.writeFileSync(
    path.join(analysisDir, "ast_analysis_report.txt"),
    report
  );
  console.log(report);

  const jsonData = toJSON(analyses, agg);
  fs.writeFileSync(
    path.join(analysisDir, "ast_api_usage.json"),
    JSON.stringify(jsonData, null, 2)
  );

  // Per-extension reports
  for (const ext of analyses) {
    const extOutDir = path.join(analysisDir, ext.name);
    fs.mkdirSync(extOutDir, { recursive: true });

    const extReport: string[] = [];
    const ew = (s = "") => extReport.push(s);

    ew(`# ${ext.name} - Full API & Side Effects Report (AST)`);
    ew(`Files analyzed: ${ext.fileCount}`);
    ew();

    ew("## vscode API");
    for (const [api, info] of [...ext.vscodeApi.entries()].sort(
      (a, b) => b[1].count - a[1].count
    )) {
      ew(`  ${String(info.count).padStart(5)}x  ${api}`);
    }
    ew();

    ew("## vscode Types (type-only)");
    for (const [t, info] of [...ext.vscodeTypes.entries()].sort(
      (a, b) => b[1].count - a[1].count
    )) {
      ew(`  ${String(info.count).padStart(5)}x  ${t}`);
    }
    ew();

    ew("## Node.js Builtins");
    for (const [api, info] of [...ext.nodeBuiltins.entries()].sort(
      (a, b) => b[1].count - a[1].count
    )) {
      ew(`  ${String(info.count).padStart(5)}x  ${api}`);
    }
    ew();

    ew("## Process/Environment");
    for (const [api, info] of [...ext.processUsage.entries()].sort(
      (a, b) => b[1].count - a[1].count
    )) {
      ew(`  ${String(info.count).padStart(5)}x  ${api}`);
    }
    ew();

    ew("## Shell Execution");
    for (const [api, info] of [...ext.shellUsage.entries()].sort(
      (a, b) => b[1].count - a[1].count
    )) {
      ew(`  ${String(info.count).padStart(5)}x  ${api}`);
    }
    ew();

    ew("## Network/HTTP");
    for (const [api, info] of [...ext.networkUsage.entries()].sort(
      (a, b) => b[1].count - a[1].count
    )) {
      ew(`  ${String(info.count).padStart(5)}x  ${api}`);
    }
    ew();

    ew("## Global Side Effects");
    for (const [api, info] of [...ext.globalSideEffects.entries()].sort(
      (a, b) => b[1].count - a[1].count
    )) {
      ew(`  ${String(info.count).padStart(5)}x  ${api}`);
    }
    ew();

    ew("## Activation Events");
    for (const e of ext.activationEvents) ew(`  ${e}`);
    if (ext.activationEvents.length === 0) ew("  (none / uses *)");
    ew();

    ew("## Contributes");
    for (const [key, count] of Object.entries(ext.contributes).sort()) {
      ew(`  ${key}: ${count} entries`);
    }

    fs.writeFileSync(
      path.join(extOutDir, "ast_api_usage.txt"),
      extReport.join("\n")
    );
  }

  console.log(`\n=== Reports written to ${analysisDir}/ ===`);
  console.log(`  - Summary: ${analysisDir}/ast_analysis_report.txt`);
  console.log(`  - JSON data: ${analysisDir}/ast_api_usage.json`);
  console.log(
    `  - Per-extension: ${analysisDir}/<ext>/ast_api_usage.txt`
  );
}

main();
