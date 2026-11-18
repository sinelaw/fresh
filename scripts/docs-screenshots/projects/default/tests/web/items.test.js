import { test } from "node:test";
import assert from "node:assert/strict";
import { itemsRouter } from "../../web/routes/items.js";

test("items router is mounted", () => {
  assert.ok(itemsRouter);
});
