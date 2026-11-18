import { Router } from "express";
import { pool } from "../db/pool.js";

export const itemsRouter = Router();

itemsRouter.get("/", async (_req, res) => {
  const { rows } = await pool.query(
    "select sku, name, unit_price from items order by name",
  );
  res.json(rows);
});

itemsRouter.get("/:sku", async (req, res) => {
  const { rows } = await pool.query(
    `select i.sku, i.name, s.warehouse, s.quantity
       from items i join stock s on s.sku = i.sku
      where i.sku = $1`,
    [req.params.sku],
  );
  if (rows.length === 0) return res.status(404).end();
  res.json(rows);
});

itemsRouter.post("/:sku/adjust", async (req, res) => {
  const { warehouse, delta } = req.body;
  if (!warehouse || !Number.isInteger(delta)) {
    return res.status(400).json({ error: "warehouse and integer delta required" });
  }
  await pool.query(
    "update stock set quantity = quantity + $1 where sku = $2 and warehouse = $3",
    [delta, req.params.sku, warehouse],
  );
  res.status(204).end();
});
