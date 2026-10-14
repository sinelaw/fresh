/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * A keypress handler shaped like a real one: it parks on a host round-trip
 * and does its work in the continuation afterwards.
 *
 * The spin makes that continuation slow enough to observe. It is not a
 * timer the test waits on — nothing in the test is timed; it is the
 * handler being slow, which a real one is free to be.
 */
const SPIN_MS = 250;

/** What the handler writes once it is done. */
const MARK = "PROBE-SETTLED";

async function probe_key(): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  await editor.getLineStartPosition(0);
  const until = Date.now() + SPIN_MS;
  while (Date.now() < until) {
    // deliberately synchronous
  }
  editor.insertText(bufferId, 0, MARK);
}
registerHandler("probe_key", probe_key);

// So the test has something to wait for before pressing the key.
editor.registerCommand(
  "Probe: Run",
  "Run the drain probe handler",
  "probe_key",
  null,
);
