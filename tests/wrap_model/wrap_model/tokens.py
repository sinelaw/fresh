"""The view-pipeline token stream.

Mirrors `fresh_core::api::ViewTokenWire` one-for-one. Every pass in the
pipeline consumes and produces a `list[Token]`; the wrap machine is the only
thing that decides where a visual row ends.
"""

from __future__ import annotations

from dataclasses import dataclass, replace
from enum import Enum, auto


class Kind(Enum):
    """Discriminant of `ViewTokenWireKind`.

    `TEXT` and `BINARY_BYTE` carry a payload (`Token.text` / `Token.byte`);
    the rest are markers.
    """

    TEXT = auto()
    SPACE = auto()
    NEWLINE = auto()
    BREAK = auto()
    BINARY_BYTE = auto()


@dataclass(frozen=True, slots=True)
class Token:
    """One view token.

    `source_offset` is the absolute buffer byte the token starts at, or None
    for injected content (wrap breaks, hanging indent, virtual text, soft-break
    newlines). "Has a source offset" is the property every downstream pass uses
    to tell real text from injected text, so it is never faked.
    """

    kind: Kind
    source_offset: int | None = None
    text: str = ""
    byte: int = 0
    style: str | None = None

    @staticmethod
    def text_tok(s: str, source_offset: int | None, style: str | None = None) -> Token:
        return Token(Kind.TEXT, source_offset, text=s, style=style)

    @staticmethod
    def space(source_offset: int | None, style: str | None = None) -> Token:
        return Token(Kind.SPACE, source_offset, style=style)

    @staticmethod
    def newline(source_offset: int | None) -> Token:
        return Token(Kind.NEWLINE, source_offset)

    @staticmethod
    def brk() -> Token:
        """A wrap break. Always injected, so never carries a source offset."""
        return Token(Kind.BREAK, None)

    @staticmethod
    def binary(b: int, source_offset: int | None) -> Token:
        return Token(Kind.BINARY_BYTE, source_offset, byte=b)

    def with_offset(self, source_offset: int | None) -> Token:
        return replace(self, source_offset=source_offset)

    def source_len(self) -> int:
        """Bytes of source this token consumes.

        Injected tokens consume none. A `Newline` consumes 1 (CRLF is modelled
        as a single Newline at the `\\r`, matching `build_base_tokens`, with the
        `\\n` skipped — so the caller adds the extra byte).
        """
        if self.source_offset is None:
            return 0
        if self.kind is Kind.TEXT:
            return len(self.text.encode("utf-8"))
        return 1
