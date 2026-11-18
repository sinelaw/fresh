from dataclasses import dataclass, field
from datetime import date
from decimal import Decimal


@dataclass
class Client:
    name: str
    email: str
    hourly_rate: Decimal
    currency: str = "EUR"


@dataclass
class LineItem:
    description: str
    hours: Decimal
    rate: Decimal

    @property
    def total(self) -> Decimal:
        return (self.hours * self.rate).quantize(Decimal("0.01"))


@dataclass
class Invoice:
    number: str
    client: Client
    issued: date
    items: list[LineItem] = field(default_factory=list)
    paid: bool = False

    def add(self, description: str, hours: Decimal) -> LineItem:
        item = LineItem(description, hours, self.client.hourly_rate)
        self.items.append(item)
        return item

    @property
    def subtotal(self) -> Decimal:
        return sum((item.total for item in self.items), Decimal("0"))

    def tax(self, rate: Decimal = Decimal("0.20")) -> Decimal:
        return (self.subtotal * rate).quantize(Decimal("0.01"))

    @property
    def total(self) -> Decimal:
        return self.subtotal + self.tax()
