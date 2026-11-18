import argparse
from decimal import Decimal

from .models import Client, Invoice
from .store import load_invoices, save_invoice


def main(argv=None):
    parser = argparse.ArgumentParser(prog="invoicing")
    sub = parser.add_subparsers(dest="command", required=True)

    new = sub.add_parser("new", help="start an invoice")
    new.add_argument("client")
    new.add_argument("--rate", type=Decimal, default=Decimal("95"))

    sub.add_parser("list", help="list invoices")

    args = parser.parse_args(argv)
    if args.command == "new":
        client = Client(args.client, f"{args.client}@example.com", args.rate)
        invoice = Invoice(number=next_number(), client=client, issued=today())
        save_invoice(invoice)
        print(f"created {invoice.number}")
    elif args.command == "list":
        for invoice in load_invoices():
            status = "paid" if invoice.paid else "open"
            print(f"{invoice.number}  {invoice.client.name:20} {invoice.total:>10}  {status}")
