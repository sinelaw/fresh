/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 5. Popup
// Shows a short message with a few choices.
// The choice comes in action_popup_result. Esc sends "dismissed".

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
  const choice = r.action_id === "dismissed" ? "none" : r.action_id;
  editor.setStatus(`You chose: ${choice}`);
  return true;
});

editor.registerCommand("Ask: Action Popup", "Ask via a popup with buttons", "ask_action_popup");
