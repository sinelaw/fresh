import argparse
import csv
from datetime import date
from decimal import Decimal
from pathlib import Path

from .models import Client, Invoice
from .store import save_invoice

CLIENTS = {
    "acme": Client("Acme Corp", "billing@acme.example", Decimal("95")),
    "globex": Client("Globex", "ap@globex.example", Decimal("110"), "USD"),
    "initech": Client("Initech", "finance@initech.example", Decimal("85")),
}


def load_hours(path: Path) -> dict[str, list[tuple[str, Decimal]]]:
    hours: dict[str, list[tuple[str, Decimal]]] = {}
    with path.open(newline="") as f:
        for row in csv.DictReader(f):
            entry = (row["task"], Decimal(row["hours"]))
            hours.setdefault(row["client"], []).append(entry)
    return hours


def main() -> None:
    parser = argparse.ArgumentParser(prog="invoicing")
    parser.add_argument("hours", type=Path, help="CSV of logged hours")
    parser.add_argument("--month", required=True)
    args = parser.parse_args()

    for key, entries in load_hours(args.hours).items():
        invoice = Invoice(f"{args.month}-{key}", CLIENTS[key], date.today())
        for task, spent in entries:
            invoice.add(task, float(spent))
        save_invoice(invoice)


if __name__ == "__main__":
    main()
