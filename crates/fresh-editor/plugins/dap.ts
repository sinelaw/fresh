/// <reference path="./lib/fresh.d.ts" />

/**
 * Debug Adapter Protocol client.
 *
 * Launch requests come from `.vscode/launch.json` (JSONC is accepted). Adapter
 * executables are intentionally configured separately: launch.json describes
 * the debuggee, while `plugins.dap.settings.adapters` describes how Fresh can
 * start each adapter type.
 */
const editor = getEditor();

interface AdapterConfig { type: string; command: string; args?: string[]; cwd?: string }
interface DapSettings {
  launchJson?: string;
  configuration?: string;
  adapters?: AdapterConfig[];
}
interface LaunchConfig {
  name?: string;
  type: string;
  request: "launch" | "attach";
  [key: string]: unknown;
}
interface DapMessage {
  seq: number;
  type: "request" | "response" | "event";
  command?: string;
  event?: string;
  request_seq?: number;
  success?: boolean;
  message?: string;
  body?: any;
  arguments?: any;
}
interface DuplexProcess extends ProcessHandle<BackgroundProcessResult> {
  readonly processId: number;
  write(data: string): boolean;
}
interface Session {
  process: DuplexProcess;
  nextSeq: number;
  pending: Map<number, { resolve: (body: any) => void; reject: (error: Error) => void }>;
  wire: string;
  initializedEvent: boolean;
  launchSent: boolean;
  configured: boolean;
  capabilities: any;
  stoppedThread: number | null;
}

const BREAKPOINT_NS = "dap-breakpoints";
const EXECUTION_NS = "dap-execution";
const settings = (editor.getPluginConfig() || {}) as DapSettings;
let session: Session | null = null;
let stoppedLocation: { path: string; line: number } | null = null;

function loadBreakpoints(): Record<string, number[]> {
  const value = editor.getGlobalState("breakpoints") as Record<string, number[]> | null;
  return value && typeof value === "object" ? value : {};
}
let breakpoints = loadBreakpoints(); // paths -> one-based DAP lines

function persistBreakpoints(): void {
  editor.setGlobalState("breakpoints", breakpoints);
}

function utf8Length(value: string): number {
  let n = 0;
  for (const c of value) {
    const cp = c.codePointAt(0)!;
    n += cp <= 0x7f ? 1 : cp <= 0x7ff ? 2 : cp <= 0xffff ? 3 : 4;
  }
  return n;
}

/** Return a UTF-16 substring containing exactly `bytes` UTF-8 bytes. */
function takeUtf8(value: string, bytes: number): { text: string; units: number } | null {
  let used = 0;
  let units = 0;
  for (const c of value) {
    const cp = c.codePointAt(0)!;
    const width = cp <= 0x7f ? 1 : cp <= 0x7ff ? 2 : cp <= 0xffff ? 3 : 4;
    if (used + width > bytes) return null;
    used += width;
    units += c.length;
    if (used === bytes) return { text: value.slice(0, units), units };
  }
  return bytes === 0 ? { text: "", units: 0 } : null;
}

function send(message: Omit<DapMessage, "seq">): number {
  if (!session) throw new Error("No active debug session");
  const seq = session.nextSeq++;
  const json = JSON.stringify({ seq, ...message });
  const frame = `Content-Length: ${utf8Length(json)}\r\n\r\n${json}`;
  if (!session.process.write(frame)) throw new Error("Debug adapter stdin is closed");
  return seq;
}

function request(command: string, args: any = {}): Promise<any> {
  if (!session) return Promise.reject(new Error("No active debug session"));
  const current = session;
  const seq = send({ type: "request", command, arguments: args });
  return new Promise((resolve, reject) => current.pending.set(seq, { resolve, reject }));
}

function acceptOutput(data: string): void {
  if (!session) return;
  session.wire += data;
  while (session) {
    const headerAt = session.wire.search(/Content-Length\s*:/i);
    if (headerAt < 0) {
      // Retain a short suffix in case the header itself was split.
      if (session.wire.length > 64) session.wire = session.wire.slice(-64);
      return;
    }
    if (headerAt > 0) session.wire = session.wire.slice(headerAt);
    const separator = session.wire.indexOf("\r\n\r\n");
    const altSeparator = session.wire.indexOf("\n\n");
    const headerEnd = separator >= 0 ? separator : altSeparator;
    if (headerEnd < 0) return;
    const separatorLength = separator >= 0 ? 4 : 2;
    const header = session.wire.slice(0, headerEnd);
    const match = /Content-Length\s*:\s*(\d+)/i.exec(header);
    if (!match) {
      session.wire = session.wire.slice(headerEnd + separatorLength);
      continue;
    }
    const payloadStart = headerEnd + separatorLength;
    const payload = takeUtf8(session.wire.slice(payloadStart), Number(match[1]));
    if (!payload) return;
    session.wire = session.wire.slice(payloadStart + payload.units);
    try {
      handleMessage(JSON.parse(payload.text) as DapMessage);
    } catch (error) {
      editor.debug(`[dap] invalid adapter message: ${String(error)}`);
    }
  }
}

function handleMessage(message: DapMessage): void {
  if (!session) return;
  if (message.type === "response") {
    const pending = session.pending.get(message.request_seq!);
    if (!pending) return;
    session.pending.delete(message.request_seq!);
    if (message.success === false) pending.reject(new Error(message.message || "DAP request failed"));
    else pending.resolve(message.body);
    return;
  }
  if (message.type === "event") void handleEvent(message.event || "", message.body || {});
  if (message.type === "request") {
    // Adapters may ask the client to run something in a terminal. Fresh does
    // not silently execute those requests; answer explicitly so they do not
    // wait forever and can fall back to their internal console.
    send({
      type: "response",
      request_seq: message.seq,
      command: message.command,
      success: false,
      message: `Client request '${message.command}' is not supported`,
    });
  }
}

async function handleEvent(event: string, body: any): Promise<void> {
  if (!session) return;
  switch (event) {
    case "initialized":
      session.initializedEvent = true;
      await configureSession();
      break;
    case "stopped":
      session.stoppedThread = Number(body.threadId);
      editor.setStatus(`Debug paused${body.description ? `: ${body.description}` : ""}`);
      await revealTopFrame(session.stoppedThread);
      break;
    case "continued":
      stoppedLocation = null;
      clearExecutionIndicator();
      editor.setStatus("Debug running");
      break;
    case "capabilities":
      session.capabilities = { ...session.capabilities, ...(body.capabilities || {}) };
      break;
    case "output":
      if (body.output) editor.debug(`[dap:${body.category || "output"}] ${String(body.output).trimEnd()}`);
      break;
    case "terminated":
    case "exited":
      endSession(event === "exited" && body.exitCode != null ? `Debuggee exited (${body.exitCode})` : "Debug session ended");
      break;
  }
}

async function revealTopFrame(threadId: number): Promise<void> {
  try {
    const body = await request("stackTrace", { threadId, startFrame: 0, levels: 1 });
    const frame = body?.stackFrames?.[0];
    if (!frame?.source?.path || !frame?.line) return;
    stoppedLocation = { path: String(frame.source.path), line: Number(frame.line) };
    editor.openFile(stoppedLocation.path, stoppedLocation.line, frame.column || 1);
    // `openFile` does not emit `buffer_activated` when the frame is already in
    // the active buffer, so render immediately as well as from the hook below.
    renderIndicators();
    editor.setStatus(`Paused at ${frame.name || "frame"} — ${stoppedLocation.path}:${stoppedLocation.line}`);
  } catch (error) {
    editor.setStatus(`Could not load stack frame: ${String(error)}`);
  }
}

function renderIndicators(bufferId = editor.getActiveBufferId()): void {
  const path = editor.getBufferPath(bufferId);
  editor.clearLineIndicators(bufferId, BREAKPOINT_NS);
  const lines = breakpoints[path] || [];
  if (lines.length) editor.setLineIndicators(bufferId, lines.map((line) => line - 1), BREAKPOINT_NS, "●", 255, 85, 85, 50);
  editor.clearLineIndicators(bufferId, EXECUTION_NS);
  if (stoppedLocation?.path === path) {
    editor.setLineIndicator(bufferId, stoppedLocation.line - 1, EXECUTION_NS, "▶", 80, 250, 123, 100);
  }
}

function clearExecutionIndicator(): void {
  const id = editor.getActiveBufferId();
  editor.clearLineIndicators(id, EXECUTION_NS);
}

async function syncBreakpoints(path: string): Promise<void> {
  if (!session) return;
  const lines = breakpoints[path] || [];
  await request("setBreakpoints", {
    source: { path, name: path.replace(/\\/g, "/").split("/").pop() },
    breakpoints: lines.map((line) => ({ line })),
    sourceModified: false,
  });
}

async function configureSession(): Promise<void> {
  if (!session || session.configured || !session.initializedEvent || !session.launchSent) return;
  session.configured = true;
  try {
    for (const path of Object.keys(breakpoints)) await syncBreakpoints(path);
    if (session.capabilities?.supportsConfigurationDoneRequest === true) {
      await request("configurationDone", {});
    }
  } catch (error) {
    editor.setStatus(`Debug configuration failed: ${String(error)}`);
  }
}

function expand(value: any, variables: Record<string, string>): any {
  if (typeof value === "string") {
    return value.replace(/\$\{(file|workspaceFolder)\}/g, (_, key: string) => variables[key] || "");
  }
  if (Array.isArray(value)) return value.map((item) => expand(item, variables));
  if (value && typeof value === "object") {
    const out: Record<string, unknown> = {};
    for (const [key, item] of Object.entries(value)) out[key] = expand(item, variables);
    return out;
  }
  return value;
}

function readLaunchConfiguration(): { adapter: AdapterConfig; launch: LaunchConfig; root: string } {
  const root = editor.getCwd();
  const launchPath = settings.launchJson || ".vscode/launch.json";
  const absolute = /^(?:[A-Za-z]:[\\/]|\/)/.test(launchPath)
    ? launchPath
    : `${root.replace(/[\\/]$/, "")}/${launchPath}`;
  const text = editor.readFile(absolute);
  if (!text) throw new Error(`No launch configuration at ${absolute}`);
  const document = editor.parseJsonc(text) as { configurations?: LaunchConfig[] };
  const configs = document?.configurations || [];
  const launch = (settings.configuration
    ? configs.find((item) => item.name === settings.configuration)
    : configs[0]);
  if (!launch) throw new Error("launch.json has no matching configuration");
  const adapter = (settings.adapters || []).find((item) => item.type === launch.type);
  if (!adapter?.command) throw new Error(`No DAP adapter command configured for type '${launch.type}'`);
  const file = editor.getBufferPath(editor.getActiveBufferId());
  return {
    adapter: expand(adapter, { file, workspaceFolder: root }),
    launch: expand(launch, { file, workspaceFolder: root }),
    root,
  };
}

async function startDebugging(): Promise<void> {
  if (session) {
    editor.setStatus("A debug session is already active");
    return;
  }
  try {
    const { adapter, launch, root } = readLaunchConfiguration();
    const process = editor.spawnBackgroundProcess(
      adapter.command,
      adapter.args || [],
      adapter.cwd || root,
    ) as DuplexProcess;
    if (typeof process.processId !== "number" || typeof process.write !== "function") {
      throw new Error("This Fresh build does not provide duplex background processes");
    }
    session = {
      process,
      nextSeq: 1,
      pending: new Map(),
      wire: "",
      initializedEvent: false,
      launchSent: false,
      configured: false,
      capabilities: {},
      stoppedThread: null,
    };
    process.result.then((result) => {
      if (session?.process === process) endSession(`Debug adapter exited (${result.exit_code})`);
    });

    session.capabilities = await request("initialize", {
      clientID: "fresh",
      clientName: "Fresh Editor",
      adapterID: launch.type,
      pathFormat: "path",
      linesStartAt1: true,
      columnsStartAt1: true,
      supportsVariableType: true,
      supportsRunInTerminalRequest: false,
    });
    const args: Record<string, unknown> = { ...launch };
    delete args.name;
    delete args.type;
    delete args.request;
    const launchResponse = request(launch.request, args);
    session.launchSent = true;
    await configureSession();
    await launchResponse;
    editor.setStatus(`Debugging: ${launch.name || launch.type}`);
  } catch (error) {
    endSession(`Could not start debugger: ${String(error)}`);
  }
}

function endSession(status: string): void {
  const old = session;
  session = null;
  stoppedLocation = null;
  if (old) {
    for (const pending of old.pending.values()) pending.reject(new Error(status));
    old.pending.clear();
    old.process.kill();
  }
  clearExecutionIndicator();
  editor.setStatus(status);
}

async function stopDebugging(): Promise<void> {
  if (!session) {
    editor.setStatus("No active debug session");
    return;
  }
  try { await request("disconnect", { restart: false, terminateDebuggee: true }); } catch (_) { /* adapter exited */ }
  endSession("Debug session stopped");
}

async function toggleBreakpoint(): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const path = editor.getBufferPath(bufferId);
  const cursor = editor.getPrimaryCursor();
  if (!path || cursor?.line == null) {
    editor.setStatus("Breakpoints require a file-backed buffer");
    return;
  }
  const line = cursor.line + 1;
  const lines = breakpoints[path] || [];
  breakpoints[path] = lines.includes(line) ? lines.filter((item) => item !== line) : [...lines, line].sort((a, b) => a - b);
  if (!breakpoints[path].length) delete breakpoints[path];
  persistBreakpoints();
  renderIndicators(bufferId);
  try { await syncBreakpoints(path); } catch (error) { editor.setStatus(`Could not update breakpoints: ${String(error)}`); }
}

async function control(command: string): Promise<void> {
  if (!session) {
    editor.setStatus("No active debug session");
    return;
  }
  try {
    let threadId = session.stoppedThread;
    if (command === "pause" && threadId == null) {
      const body = await request("threads", {});
      threadId = body?.threads?.[0]?.id ?? null;
    }
    if (threadId == null) throw new Error("The adapter has not reported a thread");
    const args = command === "pause" ? { threadId } : { threadId, singleThread: false };
    await request(command, args);
  } catch (error) {
    editor.setStatus(`Debug ${command} failed: ${String(error)}`);
  }
}

registerHandler("dap_start", startDebugging);
registerHandler("dap_stop", stopDebugging);
registerHandler("dap_toggle_breakpoint", toggleBreakpoint);
registerHandler("dap_continue", () => control("continue"));
registerHandler("dap_next", () => control("next"));
registerHandler("dap_step_in", () => control("stepIn"));
registerHandler("dap_step_out", () => control("stepOut"));
registerHandler("dap_pause", () => control("pause"));

editor.registerCommand("Debug: Start", "Start the configured debug adapter", "dap_start", null);
editor.registerCommand("Debug: Stop", "Terminate the active debug session", "dap_stop", null);
editor.registerCommand("Debug: Toggle Breakpoint", "Toggle a breakpoint on the current line", "dap_toggle_breakpoint", null);
editor.registerCommand("Debug: Continue", "Continue execution", "dap_continue", null);
editor.registerCommand("Debug: Step Over", "Step over the current line", "dap_next", null);
editor.registerCommand("Debug: Step Into", "Step into the current call", "dap_step_in", null);
editor.registerCommand("Debug: Step Out", "Step out of the current call", "dap_step_out", null);
editor.registerCommand("Debug: Pause", "Pause the debuggee", "dap_pause", null);

(editor.on as any)("onProcessStdout", (args: { process_id: number; data: string }) => {
  if (session?.process.processId === args.process_id) acceptOutput(args.data);
});
(editor.on as any)("onProcessStderr", (args: { process_id: number; data: string }) => {
  if (session?.process.processId === args.process_id) editor.debug(`[dap:stderr] ${args.data.trimEnd()}`);
});
editor.on("buffer_activated", (args) => { renderIndicators(args.buffer_id); });
editor.on("after_file_save", (args) => { renderIndicators(args.buffer_id); });

renderIndicators();
