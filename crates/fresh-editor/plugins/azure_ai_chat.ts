/// <reference path="./lib/fresh.d.ts" />

import {
  button,
  col,
  divider,
  FloatingWidgetPanel,
  hintBar,
  labeledSection,
  raw,
  row,
  textArea,
  type WidgetSpec,
} from "./lib/widgets.ts";

const editor = getEditor();
const CHAT_MODE = "fresh-ai-chat";
const OUTPUT_MODE = "fresh-ai-output";
const DEFAULT_BASE_URL = "http://localhost:8787/v1";
const DEFAULT_MODEL = "azure-proxy";
const DEFAULT_SYSTEM_PROMPT = "You are a helpful coding assistant.";
const DEFAULT_TEMPERATURE = 0.2;
const DEFAULT_MAX_TOKENS = 4096;
const DEFAULT_PROXY_SCRIPT_PATH = "scripts/fresh-ai-proxy.mjs";
const MAX_FILE_CONTEXT_BYTES = 16000;

const PROMPT_TYPE_ASK_FILE = "fresh-ai-ask-file";
const PROMPT_TYPE_REFACTOR = "fresh-ai-refactor";
const PROMPT_TYPE_FIX = "fresh-ai-fix";
const PROMPT_TYPE_REPLACE = "fresh-ai-replace";

editor.defineConfigString("baseUrl", {
  default: DEFAULT_BASE_URL,
  description: "OpenAI-compatible base URL for the local proxy.",
});
editor.defineConfigString("model", {
  default: DEFAULT_MODEL,
  description: "Model name sent to the OpenAI-compatible proxy.",
});
editor.defineConfigString("apiKey", {
  default: "",
  description: "Optional API key for the local proxy. Leave empty if the proxy does not require one.",
});
editor.defineConfigString("systemPrompt", {
  default: DEFAULT_SYSTEM_PROMPT,
  description: "System prompt prepended to every request.",
});
editor.defineConfigString("proxyScriptPath", {
  default: DEFAULT_PROXY_SCRIPT_PATH,
  description: "Proxy script path relative to the Fresh repo root.",
});
editor.defineConfigNumber("temperature", {
  default: DEFAULT_TEMPERATURE,
  minimum: 0,
  maximum: 2,
  description: "Sampling temperature sent to the proxy.",
});
editor.defineConfigInteger("maxTokens", {
  default: DEFAULT_MAX_TOKENS,
  minimum: 1,
  maximum: 32768,
  description: "Maximum completion tokens requested from the proxy.",
});

interface ChatMessage {
  role: "system" | "user" | "assistant";
  content: string;
}

interface ChatState {
  messages: ChatMessage[];
  currentInput: string;
}

interface ProxyConfig {
  baseUrl: string;
  model: string;
  apiKey: string;
  systemPrompt: string;
  proxyScriptPath: string;
  temperature: number;
  maxTokens: number;
}

interface SelectionSnapshot {
  start: number;
  end: number;
}

interface AiContext {
  bufferId: number;
  path: string;
  language: string;
  fileText: string;
  fileTruncated: boolean;
  selection: SelectionSnapshot | null;
  selectedText: string | null;
}

interface PendingPromptAction {
  promptType: string;
  context: AiContext;
}

let chatPanel: FloatingWidgetPanel | null = null;
let previousEditorMode: string | null = null;
let chatState: ChatState = {
  messages: [],
  currentInput: "",
};
let aiBusy = false;
let pendingPromptAction: PendingPromptAction | null = null;
let proxyHandle: ProcessHandle<SpawnResult> | null = null;

function getConfig(): ProxyConfig {
  const cfg = (editor.getPluginConfig() ?? {}) as {
    baseUrl?: string;
    model?: string;
    apiKey?: string;
    systemPrompt?: string;
    proxyScriptPath?: string;
    temperature?: number;
    maxTokens?: number;
  };

  return {
    baseUrl: cfg.baseUrl?.trim() || DEFAULT_BASE_URL,
    model: cfg.model?.trim() || DEFAULT_MODEL,
    apiKey: cfg.apiKey?.trim() || "",
    systemPrompt: cfg.systemPrompt ?? DEFAULT_SYSTEM_PROMPT,
    proxyScriptPath: cfg.proxyScriptPath?.trim() || DEFAULT_PROXY_SCRIPT_PATH,
    temperature: typeof cfg.temperature === "number" ? cfg.temperature : DEFAULT_TEMPERATURE,
    maxTokens: typeof cfg.maxTokens === "number" ? cfg.maxTokens : DEFAULT_MAX_TOKENS,
  };
}

function normalizeBaseUrl(baseUrl: string): string {
  return baseUrl.endsWith("/") ? baseUrl.slice(0, -1) : baseUrl;
}

function ensureTrailingNewline(text: string): string {
  return text.endsWith("\n") ? text : `${text}\n`;
}

function stripMarkdownFence(text: string): string {
  const trimmed = text.trim();
  const fenced = trimmed.match(/^```[\w-]*\n([\s\S]*?)\n```$/);
  return fenced ? fenced[1] : text;
}

function buildChatMessages(): WidgetSpec {
  const entries: TextPropertyEntry[] = [];

  if (chatState.messages.length === 0) {
    entries.push({
      text: "  Fresh AI Chat is ready.\n\n  Commands can explain, refactor, test, fix, and replace code\n  through a local OpenAI-compatible proxy.\n",
      segments: [{
        text: "  Fresh AI Chat is ready.\n\n  Commands can explain, refactor, test, fix, and replace code\n  through a local OpenAI-compatible proxy.\n",
      }],
    });
  } else {
    for (const msg of chatState.messages) {
      const prefix = msg.role === "user" ? "You" : "AI";
      const content = msg.content.split("\n").map((line, index) => {
        return index === 0 ? `${prefix}: ${line}` : `  ${line}`;
      }).join("\n");
      entries.push({
        text: `${content}\n\n`,
        segments: [{ text: `${content}\n\n` }],
      });
    }
  }

  if (aiBusy) {
    entries.push({
      text: "  AI is thinking...\n",
      segments: [{ text: "  AI is thinking...\n" }],
    });
  }

  return raw(entries, "messages");
}

function buildChatPanel(): WidgetSpec {
  return col(
    labeledSection({
      label: "Fresh AI Chat",
      child: col(
        buildChatMessages(),
        divider(),
        row(
          textArea({
            value: chatState.currentInput,
            placeholder: "Type your message...",
            rows: 4,
            fullWidth: true,
            key: "input",
          }),
        ),
        row(
          button("Send", { key: "send", intent: "primary", disabled: aiBusy }),
          button("Clear", { key: "clear", disabled: aiBusy }),
          button("Close", { key: "close" }),
        ),
      ),
    }),
    hintBar([
      { keys: "Ctrl+Enter", label: "Send" },
      { keys: "Esc", label: "Close" },
    ]),
  );
}

function renderPanel(): void {
  if (chatPanel) {
    chatPanel.update(buildChatPanel());
  }
}

async function requestAssistantText(userPrompt: string): Promise<string> {
  const config = getConfig();
  const url = `${normalizeBaseUrl(config.baseUrl)}/chat/completions`;
  const requestBody = {
    model: config.model,
    messages: [
      { role: "system", content: config.systemPrompt },
      { role: "user", content: userPrompt },
    ],
    temperature: config.temperature,
    max_tokens: config.maxTokens,
  };
  const tmpDir = editor.getTempDir();
  const requestFile = editor.pathJoin(tmpDir, "fresh_ai_request.json");
  const responseFile = editor.pathJoin(tmpDir, "fresh_ai_response.json");
  const args = [
    "-sS",
    "-X",
    "POST",
    "-H",
    "Content-Type: application/json",
  ];

  if (config.apiKey.length > 0) {
    args.push("-H", `Authorization: Bearer ${config.apiKey}`);
  }

  args.push(
    "-d",
    `@${requestFile}`,
    "-o",
    responseFile,
    "-w",
    "%{http_code}",
    url,
  );

  if (!editor.writeFile(requestFile, JSON.stringify(requestBody))) {
    throw new Error("Failed to write proxy request payload.");
  }

  const result = await editor.spawnProcess("curl", args, "");
  if (result.exit_code !== 0) {
    throw new Error(`curl failed with exit code ${result.exit_code}: ${result.stderr}`);
  }

  const httpCode = result.stdout.trim();
  const responseBody = editor.readFile(responseFile) ?? "";
  if (httpCode !== "200") {
    throw new Error(`HTTP ${httpCode}\n${responseBody || "No response body"}`);
  }

  const parsed = JSON.parse(responseBody) as {
    choices?: Array<{ message?: { content?: string } }>;
    error?: { message?: string };
  };
  const content = parsed.choices?.[0]?.message?.content?.trim();
  if (!content) {
    throw new Error(parsed.error?.message || "No response received from the proxy.");
  }
  return content;
}

async function withAiBusy<T>(work: () => Promise<T>): Promise<T | null> {
  if (aiBusy) {
    editor.setStatus("Fresh AI is already processing a request.");
    return null;
  }

  aiBusy = true;
  renderPanel();
  try {
    return await work();
  } finally {
    aiBusy = false;
    renderPanel();
  }
}

async function captureContext(requireSelection: boolean): Promise<AiContext | null> {
  const bufferId = editor.getActiveBufferId();
  const bufferInfo = editor.getBufferInfo(bufferId);
  const cursor = editor.getPrimaryCursor();
  if (!bufferInfo || !cursor) {
    editor.setStatus("Fresh AI could not read the active buffer state.");
    return null;
  }

  const fileLimit = Math.min(bufferInfo.length, MAX_FILE_CONTEXT_BYTES);
  const fileText = await editor.getBufferText(bufferId, 0, fileLimit);
  const selection = cursor.selection
    ? { start: cursor.selection.start, end: cursor.selection.end }
    : null;

  if (requireSelection && !selection) {
    editor.setStatus("Fresh AI requires a selection for this command.");
    return null;
  }

  const selectedText = selection
    ? await editor.getBufferText(bufferId, selection.start, selection.end)
    : null;

  return {
    bufferId,
    path: bufferInfo.path || "[untitled]",
    language: bufferInfo.language || "text",
    fileText,
    fileTruncated: bufferInfo.length > fileLimit,
    selection,
    selectedText,
  };
}

function formatFileContext(context: AiContext): string {
  return [
    `Path: ${context.path}`,
    `Language: ${context.language}`,
    context.fileTruncated ? `File excerpt: first ${MAX_FILE_CONTEXT_BYTES} bytes only.` : "File excerpt: complete file.",
    "File content:",
    "```",
    context.fileText,
    "```",
  ].join("\n");
}

function formatSelectionContext(context: AiContext): string {
  if (!context.selection || context.selectedText === null) {
    return formatFileContext(context);
  }

  return [
    `Path: ${context.path}`,
    `Language: ${context.language}`,
    `Selection bytes: ${context.selection.start}-${context.selection.end}`,
    "Selected code:",
    "```",
    context.selectedText,
    "```",
  ].join("\n");
}

async function showOutputBuffer(title: string, content: string): Promise<void> {
  await editor.createVirtualBuffer({
    name: `*Fresh AI: ${title}*`,
    mode: OUTPUT_MODE,
    readOnly: false,
    showLineNumbers: true,
    showCursors: true,
    editingDisabled: false,
    entries: [{ text: ensureTrailingNewline(content) }],
  });
}

function buildExplainPrompt(context: AiContext): string {
  return [
    "Explain the selected code for a developer using Fresh.",
    "Focus on purpose, behavior, and important implementation details.",
    "Use plain prose, not code fences unless absolutely necessary.",
    "",
    formatSelectionContext(context),
  ].join("\n");
}

function buildRefactorPrompt(context: AiContext, instruction: string): string {
  return [
    "Refactor the selected code.",
    `Goal: ${instruction || "Improve clarity, maintainability, and safety while preserving behavior."}`,
    "Return only the refactored code. Do not wrap it in markdown fences.",
    "",
    formatSelectionContext(context),
  ].join("\n");
}

function buildGenerateTestsPrompt(context: AiContext): string {
  return [
    "Generate tests for the selected code.",
    "Prefer small, reliable tests that target the selected behavior directly.",
    "Return only test code.",
    "",
    formatSelectionContext(context),
  ].join("\n");
}

function buildFixPrompt(context: AiContext, instruction: string): string {
  return [
    "Fix the problem in the provided code or error context.",
    `Additional problem description: ${instruction || "None provided. Infer the likely issue from the code."}`,
    "Return corrected code first, followed by a short explanation.",
    "",
    context.selectedText ? formatSelectionContext(context) : formatFileContext(context),
  ].join("\n");
}

function buildAskFilePrompt(context: AiContext, question: string): string {
  return [
    `Answer this question about the current file: ${question}`,
    "If the file excerpt is truncated, mention any uncertainty caused by missing context.",
    "",
    formatFileContext(context),
  ].join("\n");
}

function buildReplacePrompt(context: AiContext, instruction: string): string {
  return [
    "Rewrite the selected code according to this instruction.",
    `Instruction: ${instruction || "Improve the selected code while preserving behavior."}`,
    "Return only the replacement code. Do not use markdown fences.",
    "",
    formatSelectionContext(context),
  ].join("\n");
}

function replaceSelection(context: AiContext, replacementText: string): boolean {
  if (!context.selection) {
    editor.setStatus("Fresh AI requires a selection to replace code.");
    return false;
  }

  const cleaned = stripMarkdownFence(replacementText);
  if (!editor.deleteRange(context.bufferId, context.selection.start, context.selection.end)) {
    editor.setStatus("Fresh AI failed to delete the current selection.");
    return false;
  }
  if (!editor.insertText(context.bufferId, context.selection.start, cleaned)) {
    editor.setStatus("Fresh AI failed to insert the replacement text.");
    return false;
  }
  editor.setStatus("Fresh AI replaced the selected code.");
  return true;
}

function getRepoRoot(): string {
  return editor.pathJoin(editor.getPluginDir(), "..", "..", "..");
}

function getProxyScriptAbsolutePath(): string {
  const config = getConfig();
  return editor.pathJoin(getRepoRoot(), config.proxyScriptPath);
}

async function openChat(): Promise<void> {
  if (chatPanel) {
    chatPanel.unmount();
  }

  previousEditorMode = editor.getEditorMode();
  editor.setEditorMode(CHAT_MODE);
  chatPanel = new FloatingWidgetPanel();
  chatPanel.mount(buildChatPanel(), {
    widthPct: 70,
    heightPct: 70,
    focusMarker: true,
  });
  chatPanel.setFocusKey("input");
}

function closeChat(): void {
  if (chatPanel) {
    chatPanel.unmount();
    chatPanel = null;
  }
  editor.setEditorMode(previousEditorMode);
  previousEditorMode = null;
}

function clearChat(): void {
  chatState.messages = [];
  chatState.currentInput = "";
  chatPanel?.setValue("input", "");
  renderPanel();
}

async function sendCurrentInput(): Promise<void> {
  const input = chatState.currentInput.trim();
  if (!input) return;

  chatState.currentInput = "";
  chatPanel?.setValue("input", "");
  await withAiBusy(async () => {
    chatState.messages.push({ role: "user", content: input });
    renderPanel();
    try {
      const reply = await requestAssistantText(input);
      chatState.messages.push({ role: "assistant", content: reply });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      chatState.messages.push({ role: "assistant", content: `Error: ${message}` });
      editor.setStatus("Fresh AI chat request failed. Check the proxy and settings.");
    }
    renderPanel();
  });
}

async function runSelectionAction(title: string, buildPrompt: (context: AiContext) => string): Promise<void> {
  const context = await captureContext(true);
  if (!context) return;

  await withAiBusy(async () => {
    try {
      const reply = await requestAssistantText(buildPrompt(context));
      await showOutputBuffer(title, reply);
      editor.setStatus(`Fresh AI completed: ${title}`);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      editor.setStatus(`Fresh AI failed: ${message}`);
    }
  });
}

async function explainSelection(): Promise<void> {
  await runSelectionAction("Explain Selection", buildExplainPrompt);
}

async function generateTestsForSelection(): Promise<void> {
  await runSelectionAction("Generate Tests", buildGenerateTestsPrompt);
}

async function startPromptAction(promptType: string, label: string, requireSelection: boolean): Promise<void> {
  const context = await captureContext(requireSelection);
  if (!context) return;
  pendingPromptAction = { promptType, context };
  editor.startPrompt(label, promptType);
}

async function refactorSelectionPrompt(): Promise<void> {
  await startPromptAction(PROMPT_TYPE_REFACTOR, "Fresh AI refactor goal: ", true);
}

async function fixSelectionPrompt(): Promise<void> {
  await startPromptAction(PROMPT_TYPE_FIX, "Fresh AI fix context (optional): ", false);
}

async function askCurrentFilePrompt(): Promise<void> {
  await startPromptAction(PROMPT_TYPE_ASK_FILE, "Ask Fresh AI about current file: ", false);
}

async function replaceSelectionPrompt(): Promise<void> {
  await startPromptAction(PROMPT_TYPE_REPLACE, "Rewrite selected code to: ", true);
}

async function executePromptAction(promptType: string, context: AiContext, input: string): Promise<void> {
  await withAiBusy(async () => {
    try {
      if (promptType === PROMPT_TYPE_REFACTOR) {
        const reply = await requestAssistantText(buildRefactorPrompt(context, input.trim()));
        await showOutputBuffer("Refactor Selection", stripMarkdownFence(reply));
        editor.setStatus("Fresh AI completed: Refactor Selection");
        return;
      }

      if (promptType === PROMPT_TYPE_FIX) {
        const reply = await requestAssistantText(buildFixPrompt(context, input.trim()));
        await showOutputBuffer("Fix Selection or Error", reply);
        editor.setStatus("Fresh AI completed: Fix Selection or Error");
        return;
      }

      if (promptType === PROMPT_TYPE_ASK_FILE) {
        if (!input.trim()) {
          editor.setStatus("Fresh AI needs a question for the current file.");
          return;
        }
        const reply = await requestAssistantText(buildAskFilePrompt(context, input.trim()));
        await showOutputBuffer("Ask Current File", reply);
        editor.setStatus("Fresh AI completed: Ask Current File");
        return;
      }

      if (promptType === PROMPT_TYPE_REPLACE) {
        const reply = await requestAssistantText(buildReplacePrompt(context, input.trim()));
        replaceSelection(context, reply);
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      editor.setStatus(`Fresh AI failed: ${message}`);
    }
  });
}

async function startLocalProxy(): Promise<void> {
  if (proxyHandle) {
    editor.setStatus("Fresh AI proxy is already running.");
    return;
  }

  const proxyScript = getProxyScriptAbsolutePath();
  if (!editor.fileExists(proxyScript)) {
    editor.setStatus(`Fresh AI proxy script not found: ${proxyScript}`);
    return;
  }

  const probe = editor.spawnHostProcess("node", ["--version"]);
  const probeResult = await probe.result;
  if (probeResult.exit_code !== 0) {
    editor.setStatus("Fresh AI could not find a usable Node runtime for the local proxy.");
    return;
  }

  const handle = editor.spawnHostProcess("node", [proxyScript], getRepoRoot());
  proxyHandle = handle;
  editor.setStatus("Fresh AI proxy started on localhost. Configure Azure env vars before sending requests.");
  void handle.result.then((result) => {
    if (proxyHandle === handle) {
      proxyHandle = null;
    }
    if (result.exit_code !== -1 && result.exit_code !== 0) {
      const details = (result.stderr || result.stdout || "").split("\n")[0];
      editor.setStatus(`Fresh AI proxy exited: ${details || `code ${result.exit_code}`}`);
    }
  });
}

async function stopLocalProxy(): Promise<void> {
  if (!proxyHandle) {
    editor.setStatus("Fresh AI proxy is not running from this plugin session.");
    return;
  }

  const handle = proxyHandle;
  proxyHandle = null;
  const killed = await handle.kill();
  editor.setStatus(killed ? "Fresh AI proxy stopped." : "Fresh AI proxy was already stopped.");
}

registerHandler("fresh_ai_open_chat", openChat);
registerHandler("fresh_ai_close_chat", closeChat);
registerHandler("fresh_ai_clear_chat", clearChat);
registerHandler("fresh_ai_send_chat", sendCurrentInput);
registerHandler("fresh_ai_explain_selection", explainSelection);
registerHandler("fresh_ai_refactor_selection", refactorSelectionPrompt);
registerHandler("fresh_ai_generate_tests", generateTestsForSelection);
registerHandler("fresh_ai_fix_selection", fixSelectionPrompt);
registerHandler("fresh_ai_ask_current_file", askCurrentFilePrompt);
registerHandler("fresh_ai_replace_selection", replaceSelectionPrompt);
registerHandler("fresh_ai_start_proxy", startLocalProxy);
registerHandler("fresh_ai_stop_proxy", stopLocalProxy);
registerHandler("fresh_ai_widget_event", (data: HookEventMap["widget_event"]) => {
  if (!chatPanel || data.panel_id !== chatPanel.id()) return;

  if (data.event_type === "change" && data.widget_key === "input") {
    const payload = data.payload as { value?: string };
    chatState.currentInput = payload.value ?? "";
    return;
  }

  if (data.event_type === "activate") {
    if (data.widget_key === "send") {
      void sendCurrentInput();
      return;
    }
    if (data.widget_key === "clear") {
      clearChat();
      return;
    }
    if (data.widget_key === "close") {
      closeChat();
      return;
    }
  }

  if (data.event_type === "cancel") {
    closeChat();
  }
});

editor.on("widget_event", "fresh_ai_widget_event");
editor.on("prompt_confirmed", (args) => {
  if (!pendingPromptAction || args.prompt_type !== pendingPromptAction.promptType) {
    return true;
  }

  const action = pendingPromptAction;
  pendingPromptAction = null;
  void executePromptAction(action.promptType, action.context, args.input);
  return true;
});
editor.on("prompt_cancelled", (args) => {
  if (pendingPromptAction && args.prompt_type === pendingPromptAction.promptType) {
    pendingPromptAction = null;
    editor.setStatus("Fresh AI prompt cancelled.");
  }
  return true;
});

editor.defineMode(CHAT_MODE, [
  ["C-Enter", "fresh_ai_send_chat"],
  ["Escape", "fresh_ai_close_chat"],
], true, true);
editor.defineMode(OUTPUT_MODE, [
  ["q", "close_buffer"],
], false);

editor.registerCommand("Fresh AI: Open Chat", "Open the Fresh AI chat panel", "fresh_ai_open_chat");
editor.registerCommand("Fresh AI: Start Local Proxy", "Start the local Azure compatibility proxy", "fresh_ai_start_proxy");
editor.registerCommand("Fresh AI: Stop Local Proxy", "Stop the local Azure compatibility proxy", "fresh_ai_stop_proxy");
editor.registerCommand("Fresh AI: Explain Selection", "Explain the selected code in a new buffer", "fresh_ai_explain_selection");
editor.registerCommand("Fresh AI: Refactor Selection", "Refactor the selected code into a new buffer", "fresh_ai_refactor_selection");
editor.registerCommand("Fresh AI: Generate Tests for Selection", "Generate tests for the selected code", "fresh_ai_generate_tests");
editor.registerCommand("Fresh AI: Fix Selection or Error", "Fix selected code or current-file issues using AI", "fresh_ai_fix_selection");
editor.registerCommand("Fresh AI: Ask About Current File", "Ask a question about the active file", "fresh_ai_ask_current_file");
editor.registerCommand("Fresh AI: Replace Selection", "Replace the selected code with AI-generated output", "fresh_ai_replace_selection");

editor.debug("Fresh AI assistant plugin loaded");
