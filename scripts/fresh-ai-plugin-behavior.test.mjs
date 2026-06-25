import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import path from "node:path";
import vm from "node:vm";

const IS_WINDOWS = process.platform === "win32";
const REPO_ROOT = process.cwd();
const PLUGIN_PATH = "crates/fresh-editor/plugins/azure_ai_chat.ts";

function runCommand(command, args, options = {}) {
  const actualCommand = IS_WINDOWS && command === "npx" ? "cmd.exe" : command;
  const actualArgs = IS_WINDOWS && command === "npx" ? ["/c", "npx", ...args] : args;

  return new Promise((resolve, reject) => {
    const child = spawn(actualCommand, actualArgs, {
      cwd: options.cwd ?? REPO_ROOT,
      shell: false,
      stdio: ["ignore", "pipe", "pipe"],
      env: options.env ?? process.env,
    });

    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk) => { stdout += chunk.toString(); });
    child.stderr.on("data", (chunk) => { stderr += chunk.toString(); });
    child.on("error", reject);
    child.on("close", (code) => {
      if (code === 0) {
        resolve({ stdout, stderr, code });
      } else {
        reject(new Error(`${actualCommand} ${actualArgs.join(" ")} failed with code ${code}\n${stdout}\n${stderr}`));
      }
    });
  });
}

async function getBundledPluginSource() {
  const { stdout, stderr } = await runCommand("npx", [
    "@fresh-editor/fresh-editor",
    "--check-plugin",
    PLUGIN_PATH,
  ]);

  const startMarker = "=== BUNDLED OUTPUT";
  const endMarker = "=== END BUNDLED OUTPUT ===";
  const start = stderr.indexOf(startMarker);
  const end = stderr.indexOf(endMarker);
  if (start < 0 || end < 0 || end <= start) {
    throw new Error("Could not locate bundled plugin output in fresh --check-plugin output.");
  }

  return stdout.trim();
}

function createHarness() {
  const registeredHandlers = new Map();
  const eventHandlers = new Map();
  const commands = [];
  const prompts = [];
  const virtualBuffers = [];
  const statuses = [];
  const debugLogs = [];
  const definedModes = [];
  const files = new Map();
  const deleteCalls = [];
  const insertCalls = [];
  const hostProcessSpawns = [];
  let killCount = 0;

  let activeBufferId = 1;
  let pluginConfig = {};
  let primaryCursor = {
    position: 0,
    selection: { start: 0, end: 0 },
  };
  let bufferInfo = {
    id: 1,
    path: "src/example.ts",
    modified: false,
    length: 0,
    is_virtual: false,
    editing_disabled: false,
    view_mode: "source",
    is_composing_in_any_split: false,
    compose_width: null,
    language: "typescript",
    is_preview: false,
    splits: [1],
  };
  let bufferText = "";
  let lastCurlRequest = null;
  let proxyReply = "Explained result";

  const editor = {
    defineConfigString(name, options) {
      return pluginConfig[name] ?? options.default;
    },
    defineConfigNumber(name, options) {
      return pluginConfig[name] ?? options.default;
    },
    defineConfigInteger(name, options) {
      return pluginConfig[name] ?? options.default;
    },
    getPluginConfig() {
      return pluginConfig;
    },
    registerCommand(name, description, handler, context = null) {
      commands.push({ name, description, handler, context });
      return true;
    },
    on(eventName, handler) {
      const list = eventHandlers.get(eventName) ?? [];
      list.push(handler);
      eventHandlers.set(eventName, list);
    },
    defineMode(name, bindingsArr, readOnly, allowTextInput) {
      definedModes.push({ name, bindingsArr, readOnly, allowTextInput });
      return true;
    },
    debug(message) {
      debugLogs.push(message);
    },
    setStatus(message) {
      statuses.push(message);
    },
    getActiveBufferId() {
      return activeBufferId;
    },
    getBufferInfo(bufferId) {
      return bufferId === activeBufferId ? { ...bufferInfo, length: bufferText.length } : null;
    },
    getPrimaryCursor() {
      return primaryCursor;
    },
    async getBufferText(bufferId, start, end) {
      if (bufferId !== activeBufferId) return "";
      return bufferText.slice(start, end);
    },
    getTempDir() {
      return path.join(REPO_ROOT, ".tmp-tests");
    },
    pathJoin(...parts) {
      return path.join(...parts);
    },
    writeFile(filePath, content) {
      files.set(filePath, content);
      return true;
    },
    readFile(filePath) {
      return files.get(filePath) ?? null;
    },
    async spawnProcess(command, args) {
      if (command !== "curl") {
        return { stdout: "", stderr: `unexpected command: ${command}`, exit_code: 1 };
      }

      const requestArgIndex = args.indexOf("-d");
      const responseArgIndex = args.indexOf("-o");
      const requestFile = args[requestArgIndex + 1].slice(1);
      const responseFile = args[responseArgIndex + 1];
      const requestBody = JSON.parse(files.get(requestFile));
      lastCurlRequest = requestBody;
      files.set(responseFile, JSON.stringify({
        choices: [{ message: { content: proxyReply } }],
      }));
      return { stdout: "200", stderr: "", exit_code: 0 };
    },
    async createVirtualBuffer(options) {
      virtualBuffers.push(options);
      return { bufferId: 101, splitId: 1 };
    },
    deleteRange(bufferId, start, end) {
      if (bufferId !== activeBufferId) return false;
      deleteCalls.push({ bufferId, start, end });
      bufferText = bufferText.slice(0, start) + bufferText.slice(end);
      return true;
    },
    insertText(bufferId, position, text) {
      if (bufferId !== activeBufferId) return false;
      insertCalls.push({ bufferId, position, text });
      bufferText = bufferText.slice(0, position) + text + bufferText.slice(position);
      return true;
    },
    startPrompt(label, promptType) {
      prompts.push({ label, promptType });
      return true;
    },
    fileExists(filePath) {
      return filePath.endsWith("scripts/fresh-ai-proxy.mjs") || filePath.endsWith("scripts\\fresh-ai-proxy.mjs");
    },
    getPluginDir() {
      return path.join(REPO_ROOT, "crates", "fresh-editor", "plugins");
    },
    spawnHostProcess(command, args, cwd) {
      hostProcessSpawns.push({ command, args, cwd });
      const isProbe = args.length === 1 && args[0] === "--version";
      let resolveResult;
      const result = isProbe
        ? Promise.resolve({ stdout: "v24.14.1", stderr: "", exit_code: 0 })
        : new Promise((resolve) => {
          resolveResult = resolve;
        });
      return {
        result,
        then(resolve, reject) {
          return result.then(resolve, reject);
        },
        kill() {
          killCount += 1;
          if (resolveResult) {
            resolveResult({ stdout: "", stderr: "", exit_code: 0 });
          }
          return Promise.resolve(true);
        },
      };
    },
    getEditorMode() {
      return null;
    },
    setEditorMode() {
      return true;
    },
    mountFloatingWidget() {
      return true;
    },
    updateFloatingWidget() {
      return true;
    },
    unmountFloatingWidget() {
      return true;
    },
    widgetMutate() {
      return true;
    },
    widgetCommand() {
      return true;
    },
  };

  const sandbox = {
    console,
    editor,
    getEditor: () => editor,
    registerHandler: (name, fn) => {
      registeredHandlers.set(name, fn);
    },
    globalThis: null,
    setTimeout,
    clearTimeout,
  };
  sandbox.globalThis = sandbox;

  return {
    sandbox,
    editor,
    commands,
    prompts,
    virtualBuffers,
    statuses,
    debugLogs,
    definedModes,
    deleteCalls,
    insertCalls,
    hostProcessSpawns,
    get killCount() {
      return killCount;
    },
    get lastCurlRequest() {
      return lastCurlRequest;
    },
    set proxyReply(value) {
      proxyReply = value;
    },
    setPluginConfig(value) {
      pluginConfig = value;
    },
    setBufferState({ text, selectionStart, selectionEnd, pathName = "src/example.ts", language = "typescript" }) {
      bufferText = text;
      primaryCursor = {
        position: selectionEnd,
        selection: selectionStart === null || selectionEnd === null
          ? null
          : { start: selectionStart, end: selectionEnd },
      };
      bufferInfo = {
        ...bufferInfo,
        path: pathName,
        language,
        length: text.length,
      };
    },
    getBufferText() {
      return bufferText;
    },
    async invokeHandler(name) {
      const handler = registeredHandlers.get(name);
      if (!handler) {
        throw new Error(`Handler not registered: ${name}`);
      }
      return await handler();
    },
    async dispatchEvent(eventName, payload) {
      const handlers = eventHandlers.get(eventName) ?? [];
      for (const handler of handlers) {
        if (typeof handler === "string") {
          const fn = registeredHandlers.get(handler);
          if (!fn) throw new Error(`Event handler not registered: ${handler}`);
          await fn(payload);
        } else {
          await handler(payload);
        }
      }
    },
  };
}

async function run(name, fn) {
  try {
    await fn();
    console.log(`PASS ${name}`);
  } catch (error) {
    console.error(`FAIL ${name}`);
    throw error;
  }
}

const bundledSource = await getBundledPluginSource();
const harness = createHarness();
vm.runInNewContext(bundledSource, harness.sandbox, { filename: "fresh-ai-bundled.js" });

await run("registers Fresh AI command surface", () => {
  const names = harness.commands.map((command) => command.name);
  assert(names.includes("Fresh AI: Open Chat"));
  assert(names.includes("Fresh AI: Explain Selection"));
  assert(names.includes("Fresh AI: Refactor Selection"));
  assert(names.includes("Fresh AI: Generate Tests for Selection"));
  assert(names.includes("Fresh AI: Fix Selection or Error"));
  assert(names.includes("Fresh AI: Ask About Current File"));
  assert(names.includes("Fresh AI: Replace Selection"));
});

await run("explain selection creates an AI output buffer from selected code", async () => {
  harness.setBufferState({
    text: "const value = 1;\nconsole.log(value);\n",
    selectionStart: 0,
    selectionEnd: 16,
  });
  harness.proxyReply = "This code defines a constant.";

  await harness.invokeHandler("fresh_ai_explain_selection");

  assert.equal(harness.virtualBuffers.length > 0, true);
  const latest = harness.virtualBuffers.at(-1);
  assert.equal(latest.name, "*Fresh AI: Explain Selection*");
  assert.match(latest.entries[0].text, /This code defines a constant\./);
  assert.equal(harness.lastCurlRequest.model, "azure-proxy");
  assert.match(harness.lastCurlRequest.messages[1].content, /Selected code:/);
  assert.match(harness.lastCurlRequest.messages[1].content, /const value = 1;/);
});

await run("replace selection prompt rewrites the selected code in place", async () => {
  harness.setBufferState({
    text: "let answer = 41;\n",
    selectionStart: 0,
    selectionEnd: 16,
  });
  harness.proxyReply = "```ts\nlet answer = 42;\n```";

  await harness.invokeHandler("fresh_ai_replace_selection");
  const prompt = harness.prompts.at(-1);
  assert.equal(prompt.promptType, "fresh-ai-replace");

  await harness.dispatchEvent("prompt_confirmed", {
    prompt_type: "fresh-ai-replace",
    input: "make it use the correct value",
    selected_index: null,
  });
  await new Promise((resolve) => setTimeout(resolve, 0));

  assert.equal(harness.deleteCalls.length > 0, true);
  assert.equal(harness.insertCalls.length > 0, true);
  assert.match(harness.getBufferText(), /let answer = 42;/);
});


await run("generate tests creates a test output buffer", async () => {
  harness.setBufferState({
    text: "export function sum(a, b) { return a + b; }\n",
    selectionStart: 0,
    selectionEnd: 41,
    pathName: "src/sum.ts",
    language: "typescript",
  });
  harness.proxyReply = "test('sum adds values', () => expect(sum(1, 2)).toBe(3));";

  await harness.invokeHandler("fresh_ai_generate_tests");

  const latest = harness.virtualBuffers.at(-1);
  assert.equal(latest.name, "*Fresh AI: Generate Tests*");
  assert.match(latest.entries[0].text, /sum adds values/);
  assert.match(harness.lastCurlRequest.messages[1].content, /Generate tests for the selected code/);
});

await run("refactor selection prompts and writes refactored code to a new buffer", async () => {
  harness.setBufferState({
    text: "const data=[1,2,3].map((x)=>x*2);\n",
    selectionStart: 0,
    selectionEnd: 34,
  });
  harness.proxyReply = "const data = [1, 2, 3].map((value) => value * 2);";

  await harness.invokeHandler("fresh_ai_refactor_selection");
  const prompt = harness.prompts.at(-1);
  assert.equal(prompt.promptType, "fresh-ai-refactor");

  await harness.dispatchEvent("prompt_confirmed", {
    prompt_type: "fresh-ai-refactor",
    input: "improve readability",
    selected_index: null,
  });
  await new Promise((resolve) => setTimeout(resolve, 0));

  const latest = harness.virtualBuffers.at(-1);
  assert.equal(latest.name, "*Fresh AI: Refactor Selection*");
  assert.equal(latest.entries[0].text.includes("value * 2"), true);
  assert.match(harness.lastCurlRequest.messages[1].content, /Goal: improve readability/);
});

await run("fix selection or error uses current file context and creates an output buffer", async () => {
  harness.setBufferState({
    text: "function read() {\n  return missingValue;\n}\n",
    selectionStart: null,
    selectionEnd: null,
    pathName: "src/read.ts",
    language: "typescript",
  });
  harness.proxyReply = "function read() {\n  return definedValue;\n}\n\nUpdated the undefined symbol.";

  await harness.invokeHandler("fresh_ai_fix_selection");
  const prompt = harness.prompts.at(-1);
  assert.equal(prompt.promptType, "fresh-ai-fix");

  await harness.dispatchEvent("prompt_confirmed", {
    prompt_type: "fresh-ai-fix",
    input: "ReferenceError at runtime",
    selected_index: null,
  });
  await new Promise((resolve) => setTimeout(resolve, 0));

  const latest = harness.virtualBuffers.at(-1);
  assert.equal(latest.name, "*Fresh AI: Fix Selection or Error*");
  assert.match(latest.entries[0].text, /definedValue/);
  assert.match(harness.lastCurlRequest.messages[1].content, /Additional problem description: ReferenceError at runtime/);
  assert.match(harness.lastCurlRequest.messages[1].content, /File content:/);
});

await run("ask current file prompts against full file context and creates an answer buffer", async () => {
  harness.setBufferState({
    text: "export const enabled = true;\nexport const retries = 3;\n",
    selectionStart: null,
    selectionEnd: null,
    pathName: "src/config.ts",
    language: "typescript",
  });
  harness.proxyReply = "The file enables the feature and sets retries to 3.";

  await harness.invokeHandler("fresh_ai_ask_current_file");
  const prompt = harness.prompts.at(-1);
  assert.equal(prompt.promptType, "fresh-ai-ask-file");

  await harness.dispatchEvent("prompt_confirmed", {
    prompt_type: "fresh-ai-ask-file",
    input: "What does this file configure?",
    selected_index: null,
  });
  await new Promise((resolve) => setTimeout(resolve, 0));

  const latest = harness.virtualBuffers.at(-1);
  assert.equal(latest.name, "*Fresh AI: Ask Current File*");
  assert.match(latest.entries[0].text, /sets retries to 3/);
  assert.match(harness.lastCurlRequest.messages[1].content, /Answer this question about the current file: What does this file configure?/);
});

await run("start and stop local proxy use the host node process", async () => {
  await harness.invokeHandler("fresh_ai_start_proxy");
  assert.equal(harness.hostProcessSpawns.length >= 2, true);
  assert.equal(harness.hostProcessSpawns[0].command, "node");
  assert.equal(harness.hostProcessSpawns[0].args[0], "--version");
  assert.equal(harness.hostProcessSpawns[1].command, "node");
  assert.match(harness.hostProcessSpawns[1].args[0], /fresh-ai-proxy\.mjs$/);

  await harness.invokeHandler("fresh_ai_stop_proxy");
  assert.equal(harness.killCount > 0, true);
});

console.log("Fresh AI plugin behavior checks passed.");


