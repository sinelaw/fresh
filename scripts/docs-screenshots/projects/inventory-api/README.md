# inventory-api

Stock levels per warehouse, behind a small HTTP API.

| Method | Path | |
| --- | --- | --- |
| GET | `/items` | every item |
| GET | `/items/:sku` | one item, with stock per warehouse |
| POST | `/items/:sku/adjust` | change stock by `delta` in one warehouse |
