import json
from pathlib import Path

from .models import Invoice

STORE = Path.home() / ".invoicing"


def load_invoices() -> list[Invoice]:
    if not STORE.exists():
        return []
    return [Invoice(**json.loads(p.read_text())) for p in sorted(STORE.glob("*.json"))]


def save_invoice(invoice: Invoice) -> None:
    STORE.mkdir(exist_ok=True)
    (STORE / f"{invoice.number}.json").write_text(json.dumps(invoice.__dict__, default=str))
