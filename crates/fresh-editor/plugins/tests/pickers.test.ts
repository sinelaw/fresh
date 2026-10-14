/** The shared path picker: Browse… opens a browser, a row walks or picks,
 *  and a pick fills the field and focuses it. */
import { PathPicker, machinePicker, type BrowseSource } from "../lib/pickers.ts";
import type { WidgetEvt } from "../lib/widgets.ts";

let failures = 0;
function eq(actual: unknown, expected: unknown, name: string): void {
  const a = JSON.stringify(actual);
  const e = JSON.stringify(expected);
  if (a !== e) {
    console.log(`FAIL ${name}\n  got      ${a}\n  expected ${e}`);
    failures++;
  } else {
    console.log(`ok   ${name}`);
  }
}

const tree: Record<string, { name: string; git: boolean; file?: boolean }[]> = {
  "/home/u": [{ name: "plain", git: false }, { name: "repo", git: true }],
  "/home/u/plain": [{ name: "id_ed25519", git: false, file: true }],
};
const source: BrowseSource = {
  list: (_m, dir, files) =>
    Promise.resolve({ entries: (tree[dir] ?? []).filter((e) => files || !e.file), error: "" }),
  parentDir: (dir) => dir.replace(/\/[^/]+$/, "") || "/",
  title: (m, dir) => `${m} : ${dir}`,
  t: (k) => k,
};

function harness(files = false) {
  const field = { value: "", cursor: 0 };
  const calls: string[] = [];
  const picker = new PathPicker({
    source,
    key: "path",
    browseKey: "browse",
    listKey: "browse_list",
    label: "Path",
    labelWidth: 10,
    value: () => field,
    start: () => ({ machineKey: "local", dir: "/home/u", files }),
    onPick: (p) => {
      field.value = p;
      field.cursor = p.length;
    },
    render: () => calls.push("render"),
    panel: () => ({
      setFocusKey: (k) => (calls.push(`focus:${k}`), true),
      setValue: (k, v) => (calls.push(`value:${k}=${v}`), true),
    }),
  });
  return { field, calls, picker };
}

const ev = (event_type: string, widget_key: string, payload?: unknown): WidgetEvt =>
  ({ event_type, widget_key, payload } as unknown as WidgetEvt);
const tick = () => new Promise((r) => setTimeout(r, 0));

{
  const { field, calls, picker } = harness();
  eq(picker.handle(ev("activate", "browse")), true, "Browse… is the picker's own event");
  await tick();
  eq(picker.browser?.dir, "/home/u", "it opens where the dialog says");
  eq(calls.includes("focus:browse_list"), true, "and the list takes focus once it has loaded");
  // Rows: "..", then plain/, repo/.
  picker.handle(ev("activate", "browse_list", { index: 1 }));
  await tick();
  eq(picker.browser?.dir, "/home/u/plain", "⏎ on a plain folder goes in");
  eq(picker.up("browse_list"), true, "Backspace in the list goes up");
  await tick();
  eq(picker.browser?.dir, "/home/u", "to the folder above");
  picker.handle(ev("activate", "browse_list", { index: 2 }));
  eq(picker.browser, null, "⏎ on a git folder picks it and closes the browser");
  eq(field.value, "/home/u/repo", "the pick is the field's value");
  eq(calls.slice(-2), ["value:path=/home/u/repo", "focus:path"], "shown in the field, which takes focus");
  eq(picker.handle(ev("change", "other")), false, "another control's event is not the picker's");
}

{
  const { picker, calls } = harness(true);
  picker.toggle();
  await tick();
  picker.toggle();
  eq(picker.browser, null, "Browse… pressed again closes the browser");
  picker.toggle();
  await tick();
  calls.length = 0;
  eq(picker.close(), true, "Esc closes an open browser");
  eq(calls, ["render", "focus:browse"], "and gives Browse… the focus back");
  eq(picker.close(), false, "with nothing open, Esc is the dialog's");
}

{
  const spec = machinePicker({
    options: ["Local", "box"],
    selectedIndex: 1,
    label: "Machine",
    labelWidth: 10,
    key: "machine",
    addKey: "add",
    addLabel: "+ Add machine…",
  }) as unknown as { kind: string; children: { kind: string; key?: string }[] };
  eq(spec.children.map((c) => c.kind), ["dropdown", "spacer", "button"], "the dropdown, then + Add machine… beside it");
}

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
