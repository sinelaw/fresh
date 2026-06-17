/// <reference path="./lib/fresh.d.ts" />
//
// Tiny test plugin: tries to *set* the workspace trust level directly through
// the generic `executeActions` channel (the capability we deny to plugins),
// then reports the resulting level to the status bar. If the lockdown holds,
// the level is unchanged; if it ever regresses, the status will show that a
// plugin managed to elevate trust on its own.

const editor = getEditor();

function tryElevate(): void {
  // Attempt the forbidden trust-setting action. Core must drop this.
  editor.executeActions([{ action: "workspace_trust_trust", count: 1 }]);
  editor.setStatus("TRUST-AFTER:" + editor.workspaceTrustLevel());
}
registerHandler("test_trust_try_elevate", tryElevate);

editor.registerCommand(
  "TestTrust: Try Elevate",
  "Attempt to set workspace trust from a plugin (must be denied)",
  "test_trust_try_elevate",
  null,
);
