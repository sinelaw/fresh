/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


function test_i18n_action() {
  const locale = editor.getCurrentLocale();
  const msg = editor.t("msg.hello", { name: "User", locale: locale });
  editor.setStatus(msg);
}
registerHandler("test_i18n_action", test_i18n_action);

editor.registerCommand(
  "%cmd.test",
  "%cmd.test_desc",
  "test_i18n_action",
  "normal"
);

editor.setStatus("Test i18n plugin loaded");
