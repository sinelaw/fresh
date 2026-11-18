import { test } from "node:test";
import assert from "node:assert/strict";
import request from "supertest";
import { app } from "../src/app.js";

test("GET /items lists every item", async () => {
  const res = await request(app).get("/items");
  assert.equal(res.status, 200);
  assert.ok(Array.isArray(res.body));
});

test("POST /items/:sku/adjust rejects a missing warehouse", async () => {
  const res = await request(app).post("/items/A-100/adjust").send({ delta: 2 });
  assert.equal(res.status, 400);
});
