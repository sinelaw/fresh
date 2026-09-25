/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Window- and buffer-scoped modes beside vi's editor-wide input mode
 * (sinelaw/fresh#3395).
 *
 * - `tim_window_mode` sets a window-scoped editor mode that binds `j`
 *   (status `WINDOW-MODE-J`): it must hold in the window that set it and
 *   nowhere else.
 * - `tim_open_panel` opens a panel buffer, in a split of the active window,
 *   whose own mode binds `j` (status `PANEL-MODE-J`); `tim_close_panel`
 *   closes it. The panel's mode must not reach another window, and closing
 *   it must leave vi as it was.
 * - `tim_report_mode` shows the active window's editor mode in the status
 *   bar (`MODE<n>=<mode>;`, numbered per report), so a test can
 *   wait on screen for another plugin to have set or cleared it.
 */

const WINDOW_MODE = "tim-window-mode";
const PANEL_MODE = "tim-panel-mode";
let panelBuffer: number | null = null;

editor.defineMode(WINDOW_MODE, [["j", "tim_window_j"]], false, false);
editor.defineMode(PANEL_MODE, [["j", "tim_panel_j"]], true, false);

function tim_window_j(): void {
  editor.setStatus("WINDOW-MODE-J");
}
registerHandler("tim_window_j", tim_window_j);

function tim_panel_j(): void {
  editor.setStatus("PANEL-MODE-J");
}
registerHandler("tim_panel_j", tim_panel_j);

function tim_window_mode(): void {
  editor.setEditorMode(WINDOW_MODE);
  editor.setStatus("WINDOW-MODE-ON");
}
registerHandler("tim_window_mode", tim_window_mode);

async function tim_open_panel(): Promise<void> {
  const result = await editor.createVirtualBufferInSplit({
    name: "*TIM Panel*",
    mode: PANEL_MODE,
    readOnly: true,
    ratio: 0.5,
    entries: [{ text: "PANEL-CONTENT\n" }],
  });
  panelBuffer = result.bufferId;
}
registerHandler("tim_open_panel", tim_open_panel);

function tim_close_panel(): void {
  if (panelBuffer === null) return;
  editor.closeBuffer(panelBuffer);
  panelBuffer = null;
}
registerHandler("tim_close_panel", tim_close_panel);

let modeReports = 0;
function tim_report_mode(): void {
  modeReports += 1;
  editor.setStatus(`MODE${modeReports}=${editor.getEditorMode() ?? "none"};`);
}
registerHandler("tim_report_mode", tim_report_mode);

editor.registerCommand("TestInputMode: Window Mode", "", "tim_window_mode", null);
editor.registerCommand("TestInputMode: Open Panel", "", "tim_open_panel", null);
editor.registerCommand("TestInputMode: Close Panel", "", "tim_close_panel", null);
editor.registerCommand("TestInputMode: Report Mode", "", "tim_report_mode", null);
