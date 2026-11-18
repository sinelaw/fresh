# Design notes

## Money

All amounts are `Decimal`, quantized to cents only when shown or stored.
Tax is computed on the subtotal, never per line.

## Storage

One JSON file per invoice under `~/.invoicing`. Good enough for a
single user; a database comes later.

## Open questions

- Multi-currency invoices?
- Credit notes as negative invoices, or their own type?
