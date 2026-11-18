from decimal import Decimal

from .models import Invoice

WIDTH = 60


def money(value: Decimal, currency: str) -> str:
    return f"{value:>10,.2f} {currency}"


def render_text(invoice: Invoice) -> str:
    """Render an invoice as a plain-text document."""
    client = invoice.client
    lines = [
        f"INVOICE {invoice.number}".center(WIDTH),
        "",
        f"Billed to: {client.name} <{client.email}>",
        f"Issued:    {invoice.issued:%d %B %Y}",
        "-" * WIDTH,
    ]
    for item in invoice.items:
        label = f"{item.description} ({item.hours} h)"
        lines.append(f"{label:<40}{money(item.total, client.currency)}")
    lines += [
        "-" * WIDTH,
        f"{'Subtotal':<40}{money(invoice.subtotal, client.currency)}",
        f"{'Tax':<40}{money(invoice.tax(), client.currency)}",
        f"{'Total':<40}{money(invoice.total, client.currency)}",
    ]
    return "\n".join(lines)
