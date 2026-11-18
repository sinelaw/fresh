import express from "express";
import { itemsRouter } from "./routes/items.js";
import { invoicesRouter } from "./routes/invoices.js";

const app = express();
app.use(express.json());

app.use("/items", itemsRouter);
app.use("/invoices", invoicesRouter);

const port = Number(process.env.PORT ?? 8080);
app.listen(port, () => {
  console.log(`listening on :${port}`);
});
