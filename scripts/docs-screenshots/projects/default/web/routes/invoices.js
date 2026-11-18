import { Router } from "express";
import { pool } from "../db/pool.js";

export const invoicesRouter = Router();

invoicesRouter.get("/", async (_req, res) => {
  const { rows } = await pool.query("select * from invoices order by issued desc");
  res.json(rows);
});
