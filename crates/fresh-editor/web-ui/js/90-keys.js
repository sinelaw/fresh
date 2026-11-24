// The keyboard. The client's own keys open its own screens (settings, the
// explorer, the menus); every other key goes to the core as an editing
// command in the active view — or to whatever has the editor's keyboard (a
// plugin panel, a popup), which the core knows.

const kb = document.getElementById("kb");
// The editor's caret is drawn only while the editor has the keyboard
// (`EditorView.placeCaret`): it follows the keyboard in and out.
for (const [target, ev] of [[kb, "focus"], [kb, "blur"], [window, "focus"], [window, "blur"]]) {
  target.addEventListener(ev, () => { for (const v of views.values()) v.placeCaret(); });
}

function focusKeyboard() {
  const a = document.activeElement;
  // Whatever in a layer over the window has the keys keeps them (a menu, a
  // dialog, the palette): a late call from an earlier click must not take
  // them back.
  if (a && a !== kb && a.closest && a.closest("#overlays, .w-text-input")) return;
  kb.focus({ preventScroll: true });
}

/** Keys the client answers itself. Returns whether it did. */
function clientKey(e) {
  const mod = e.ctrlKey || e.metaKey;
  if (mod && !e.altKey && !e.shiftKey && e.key === ",") { openSettings(); return true; }
  if (mod && !e.altKey && !e.shiftKey && e.key.toLowerCase() === "b") { toggleExplorer(); return true; }
  if (e.key === "F10" && !mod) { if (menubar.open >= 0) closeMenus(); else openMenu(0, true); return true; }
  // A menu opens on the Alt+letter the core's keymap gives it (none while
  // `menu_bar_mnemonics` is off, or when the key is bound to something else);
  // any other Alt+letter is the core's. A terminal keeps its keys.
  if (e.altKey && !mod && /^Key[A-Z]$/.test(e.code) && !(bufferById(core.status && core.status.buffer) || {}).terminal) {
    const i = core.menus.findIndex((m) => m.mnemonic === e.code.slice(3));
    if (i >= 0) { openMenu(i, true); return true; }
  }
  return false;
}

function sendEditorKey(e) {
  const v = activeView();
  const target = !panelHasKeyboard() && v && v.id !== null ? { view: v.id } : {};
  api.send("key", {
    key: e.key, code: e.code,
    ctrl: e.ctrlKey, alt: e.altKey, shift: e.shiftKey, meta: e.metaKey,
    ...target,
  });
}

/** A key for the core: the editor's hidden input's, and a plugin text
 *  field's (its text is the core's; the field only shows it). */
function coreKeydown(e) {
  if (e.isComposing || e.key === "Dead" || e.key === "Process" || e.key === "Unidentified") return;
  if (["Shift", "Control", "Alt", "Meta", "CapsLock"].includes(e.key)) return;
  if (clientKey(e)) { e.preventDefault(); return; }
  // Paste arrives as a `paste` event with the clipboard's text.
  if ((e.ctrlKey || e.metaKey) && !e.altKey && e.key.toLowerCase() === "v") return;
  e.preventDefault();
  sendEditorKey(e);
}
kb.addEventListener("keydown", coreKeydown);
document.addEventListener("keydown", (e) => {
  if (e.target.classList && e.target.classList.contains("w-text-input")) coreKeydown(e);
});

/** Text typed into the editor as text (an input method's result, an
 *  on-screen keyboard's), not as keys. */
function sendText(text) {
  const v = activeView();
  api.send("paste", { text, ...(!panelHasKeyboard() && v && v.id !== null ? { view: v.id } : {}) });
}

// An input method composes in the text field, which sits on the caret; what
// it has so far is shown there, over the text, until it commits.
const preedit = document.getElementById("preedit");
kb.addEventListener("compositionstart", () => {
  kb.dataset.composing = "1";
  preedit.style.left = kb.style.left;
  preedit.style.top = kb.style.top;
});
kb.addEventListener("compositionupdate", (e) => {
  preedit.textContent = e.data || "";
  preedit.hidden = !e.data;
});
kb.addEventListener("compositionend", (e) => {
  delete kb.dataset.composing;
  preedit.hidden = true;
  preedit.textContent = "";
  if (e.data) sendText(e.data);
  kb.value = "";
});

// On-screen keyboards report what they type as input, often with no usable
// key event (`key` is "Unidentified"): take the input itself.
kb.addEventListener("beforeinput", (e) => {
  if (kb.dataset.composing || e.isComposing) return;
  const keyOf = { insertLineBreak: "Enter", insertParagraph: "Enter", deleteContentBackward: "Backspace", deleteContentForward: "Delete" };
  if (e.inputType === "insertText" && e.data) {
    e.preventDefault();
    sendText(e.data);
  } else if (keyOf[e.inputType]) {
    e.preventDefault();
    const v = activeView();
    api.send("key", { key: keyOf[e.inputType], ...(!panelHasKeyboard() && v && v.id !== null ? { view: v.id } : {}) });
  }
});
kb.addEventListener("input", () => { if (!kb.dataset.composing) kb.value = ""; });

/** Paste the system clipboard into `view` (a menu's Paste): the browser's
 *  clipboard when it lets us read it, else the core's own. */
async function pasteFromSystem(view) {
  let text = null;
  try { text = await navigator.clipboard.readText(); } catch (_) {}
  if (text) api.send("paste", { text, view: view.id });
  else api.send("action", { name: "paste", view: view.id });
  focusKeyboard();
}

document.addEventListener("paste", (e) => {
  const a = document.activeElement;
  if (a !== kb && a !== document.body && !(a.classList && a.classList.contains("w-text-input"))) return;
  const text = e.clipboardData && e.clipboardData.getData("text/plain");
  if (!text) return;
  e.preventDefault();
  const v = activeView();
  api.send("paste", { text, ...(!panelHasKeyboard() && v && v.id !== null ? { view: v.id } : {}) });
});

// Keys typed while nothing in particular has focus belong to the editor.
document.addEventListener("keydown", (e) => {
  if (e.target !== document.body) return;
  if (clientKey(e)) { e.preventDefault(); return; }
  kb.focus({ preventScroll: true });
  if (["Shift", "Control", "Alt", "Meta"].includes(e.key)) return;
  e.preventDefault();
  sendEditorKey(e);
});

// The core copied: put it on the system clipboard.
api.on("clipboard", (v) => {
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(v.text).catch(() => {});
  }
});

// ---- boot -------------------------------------------------------------------
(async function boot() {
  if (document.fonts && document.fonts.ready) await document.fonts.ready;
  measureCell();
  applySkin();
  api.connect();
  pollSkinFolders().then(scheduleSkinPoll);
  focusKeyboard();
  // A click anywhere gives the keyboard back to the editor — except in a
  // place that takes keys itself.
  // Not a right-click (it opens a menu that takes the keys) nor anything in
  // a layer over the window.
  document.addEventListener("mouseup", (e) => {
    if (e.button === 2) return;
    if (e.target.closest && e.target.closest("#explorer, #overlays, #menubar, input, select, textarea")) return;
    setTimeout(focusKeyboard, 0);
  });
})();
