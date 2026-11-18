import { test } from "node:test";
import assert from "node:assert/strict";
import { invoicesRouter } from "../../web/routes/invoices.js";

test("invoices router is mounted", () => {
  assert.ok(invoicesRouter);
});
