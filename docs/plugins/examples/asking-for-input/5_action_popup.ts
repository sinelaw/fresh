/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 5. Action popup — `showActionPopup` with a list of buttons.
// For a choice between a few actions (Yes / No / ...). The pick arrives
// in `action_popup_result`.

registerHandler("ask_action_popup", () => {
  editor.showActionPopup({
    id: "ask-popup",
    title: "Insert license header?",
    message: "This file has no license header. Which one should be added?",
    actions: [
      { id: "mit", label: "MIT" },
      { id: "apache", label: "Apache-2.0" },
      { id: "none", label: "None (Esc)" },
    ],
  });
});

editor.on("action_popup_result", (r) => {
  if (r.popup_id !== "ask-popup") return true;
  editor.setStatus(`You chose: ${r.action_id}`);
  return true;
});

editor.registerCommand("Ask: Action Popup", "Ask via a popup with buttons", "ask_action_popup");
