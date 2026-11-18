from datetime import date
from decimal import Decimal

from invoicing.models import Client, Invoice
from invoicing.report import outstanding, totals_by_client

ACME = Client("Acme", "ops@acme.test", Decimal("100"))


def make(number, paid):
    invoice = Invoice(number, ACME, date(2026, 1, int(number[-1])), paid=paid)
    invoice.add("Support", Decimal("1"))
    return invoice


def test_outstanding_skips_paid_by_default():
    invoices = [make("2026-001", True), make("2026-002", False)]
    assert [i.number for i in outstanding(invoices)] == ["2026-002"]


def test_outstanding_can_include_paid():
    invoices = [make("2026-001", True), make("2026-002", False)]
    assert len(outstanding(invoices, open_only=False)) == 2


def test_totals_open_only():
    invoices = [make("2026-001", True), make("2026-002", False)]
    assert totals_by_client(invoices, open_only=True) == {"Acme": Decimal("121.00")}
