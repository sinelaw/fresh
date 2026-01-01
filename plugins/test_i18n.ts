/// <reference path="./lib/fresh.d.ts" />

globalThis.test_i18n_action = function() {
  const locale = editor.getCurrentLocale();
  const msg = editor.t("msg.hello", { name: "User", locale: locale });
  editor.setStatus(msg);
};

editor.registerCommand(
  "%cmd.test",
  "%cmd.test_desc",
  "test_i18n_action",
  "normal"
);

editor.setStatus("Test i18n plugin loaded");
