/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 4. Modal dialog — `mountFloatingWidget` with a WidgetSpec.
// The same machinery as the Orchestrator's "New Folder" dialog: a framed,
// centred panel with real controls (text fields, a checkbox, buttons).
// The host owns typing/focus/Tab; the plugin hears `widget_event`s.
//
// The bundled plugins build specs with helpers from lib/widgets.ts
// (text(), button(), row(), ...); a standalone init.ts writes them out.

const PANEL = 1;          // any id unique within this plugin
const MODE = "ask-dialog"; // keymap for the dialog (Enter = submit)
const form = { issue: "", title: "", urgent: false };

const field = (key: string, label: string, value: string, placeholder: string) => ({
  kind: "text", key, label, value, placeholder, labelWidth: 8, fullWidth: true,
  cursorByte: -1, focused: false, rows: 1, minRows: 0, maxRows: 0, fieldWidth: 0,
  maxVisibleChars: 0, completions: [], blockCaret: false, selStart: -1, selEnd: -1,
  readOnly: false, markdown: false, combo: false,
});
const btn = (key: string, label: string, intent = "normal") => ({
  kind: "button", key, label, intent, focused: false, disabled: false,
  focusable: true, bare: false, fullWidth: false,
});
const spacer = (cols = 0, flex = false) => ({ kind: "spacer", cols, flex });
const rule = { kind: "label", text: "─".repeat(400), labelWidth: 0, wrap: false, elide: "none",
  style: { fg: "ui.menu_disabled_fg" } };

const spec = () => ({
  kind: "col",
  children: [
    spacer(),
    field("issue", "Issue", form.issue, "e.g. 1234"),
    field("title", "Title", form.title, "short description"),
    { kind: "toggle", key: "urgent", checked: form.urgent, label: "Urgent",
      focused: false, indeterminate: false, labelFirst: false, labelWidth: 8 },
    spacer(), rule, spacer(),
    { kind: "row", children: [spacer(0, true), btn("cancel", "Cancel"), spacer(3), btn("ok", "OK", "primary"), spacer(2)] },
  ],
});

function close() { editor.unmountFloatingWidget(PANEL); }
function submit() {
  close();
  editor.setStatus(`Issue=${form.issue} title="${form.title}" urgent=${form.urgent}`);
}

registerHandler("ask_dialog_submit", submit);
editor.defineMode(MODE, [["Enter", "ask_dialog_submit"]], true, true);

registerHandler("ask_modal_dialog", () => {
  Object.assign(form, { issue: "", title: "", urgent: false });
  editor.mountFloatingWidget(PANEL, spec(), 50, 40, false, true, "New issue link", true, false, MODE, "right");
  editor.widgetMutate(PANEL, { kind: "setFocusKey", widgetKey: "issue" });
});

editor.on("widget_event", (e) => {
  if (e.panel_id !== PANEL) return;
  const p = e.payload ?? {};
  if (e.event_type === "change" && (e.widget_key === "issue" || e.widget_key === "title"))
    form[e.widget_key] = String(p.value ?? "");
  else if (e.event_type === "toggle" && e.widget_key === "urgent") {
    form.urgent = typeof p.checked === "boolean" ? p.checked : !form.urgent;
    editor.updateFloatingWidget(PANEL, spec()); // the checkbox is plugin state: re-render it
  }
  else if (e.event_type === "activate" && e.widget_key === "ok") submit();
  else if (e.event_type === "activate" && e.widget_key === "cancel") close();
  // "cancel" event_type (Esc / [×]): the host already unmounted the panel.
});

editor.registerCommand("Ask: Modal Dialog", "Ask via a dialog with fields and buttons", "ask_modal_dialog");
