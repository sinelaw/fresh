from collections import defaultdict
from decimal import Decimal

from .models import Invoice


def outstanding(invoices: list[Invoice], open_only: bool = True) -> list[Invoice]:
    """Invoices to chase, oldest first."""
    chosen = [i for i in invoices if not (open_only and i.paid)]
    return sorted(chosen, key=lambda i: i.issued)


def totals_by_client(invoices: list[Invoice], open_only: bool = False) -> dict[str, Decimal]:
    totals: dict[str, Decimal] = defaultdict(Decimal)
    for invoice in invoices:
        if open_only and invoice.paid:
            continue
        totals[invoice.client.name] += invoice.total
    return dict(totals)
