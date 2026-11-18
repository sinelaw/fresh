from decimal import Decimal

from invoicing.models import Client, LineItem


def test_line_total_rounds_to_cents():
    item = LineItem("Review", Decimal("1.333"), Decimal("90"))
    assert item.total == Decimal("119.97")


def test_client_defaults_to_euros():
    assert Client("Acme", "ops@acme.test", Decimal("95")).currency == "EUR"
