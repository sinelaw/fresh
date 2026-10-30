/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * `run` never rejects, also on a machine that is gone: written for
 * `spawnProcess`, a caller expects a result, and an unhandled rejection
 * stops the plugin. The command runs `echo` on the active window's machine,
 * then on one it has closed, and shows both outcomes:
 * `RUN open=<code>:<stdout> closed=<code>:<has stderr>`.
 */
async function runClosed(): Promise<void> {
  const open = await editor.runOnTarget("echo", ["hi"]);
  const machine = await editor.openMachine({ kind: "window" });
  await machine.close();
  let closed: string;
  try {
    const r = await machine.run("echo", ["hi"]);
    closed = `${r.code}:${r.stderr !== ""}`;
  } catch (e) {
    closed = `rejected`;
  }
  editor.setStatus(`RUN open=${open.code}:${open.stdout.trim()} closed=${closed}`);
}
registerHandler("machine_run_closed", runClosed);

editor.registerCommand(
  "TestRun: Closed machine",
  "Run a command on the active window's machine and on a closed one",
  "machine_run_closed",
  null,
);
