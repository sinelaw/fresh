from datetime import date
from decimal import Decimal

from invoicing.models import Client, Invoice

ACME = Client("Acme", "ops@acme.test", Decimal("100"))


def test_add_uses_client_rate():
    invoice = Invoice("2026-001", ACME, date(2026, 3, 1))
    item = invoice.add("Workshop", Decimal("3"))
    assert item.rate == Decimal("100")


def test_total_includes_tax():
    invoice = Invoice("2026-002", ACME, date(2026, 3, 2))
    invoice.add("Audit", Decimal("10"))
    assert invoice.total == invoice.subtotal + invoice.tax()
