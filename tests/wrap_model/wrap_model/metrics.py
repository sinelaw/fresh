"""Operation counters, so complexity claims can be tested rather than argued.

The entire case for this design is about *how much work* an operation does — a
keystroke that rewraps one row instead of 2,500, a frame that builds 50 rows
instead of every row above the viewport. Wall-clock timing is too noisy to
assert on and says nothing about asymptotics, so the model counts the operations
that actually scale instead:

    with measure() as m:
        model.insert(len(model.buffer), "x")
    assert m.rows_emitted <= 2          # not "is it fast", but "is it O(1)"

`tests/test_complexity.py` uses these to pin the scaling laws: cost per keystroke
independent of line length, cost per frame independent of scroll depth and
document size, cost per scroll event constant, cost per cursor move zero.

Counting is global and cheap by design (plain integer increments on a module
singleton). The real implementation would put these behind a feature flag or a
`tracing` counter; here they are always on because the tests are the point.
"""

from __future__ import annotations

from collections.abc import Iterator
from contextlib import contextmanager
from dataclasses import dataclass, fields


@dataclass(slots=True)
class Metrics:
    """Counts of the operations whose growth rate matters."""

    #: Tokens fed to the wrap machine. Proxy for "how much text was considered".
    tokens_fed: int = 0
    #: Visual rows the wrap machine decided the boundaries of.
    rows_emitted: int = 0
    #: Grapheme-cluster width measurements — the profile's hottest leaf.
    width_measurements: int = 0
    #: Bytes pulled out of the buffer.
    bytes_read: int = 0
    #: `LineIterator.next_line` calls.
    lines_read: int = 0
    #: Whole logical lines wrapped from scratch by the index.
    line_builds: int = 0
    #: Incremental repairs attempted.
    repairs: int = 0
    #: Repairs that resynchronised and spliced the tail instead of running on.
    resyncs: int = 0
    #: `ViewLine`s materialised (the per-character mapping work).
    rows_materialized: int = 0
    #: Full index builds.
    index_builds: int = 0

    def reset(self) -> None:
        for f in fields(self):
            setattr(self, f.name, 0)

    def snapshot(self) -> dict[str, int]:
        return {f.name: getattr(self, f.name) for f in fields(self)}

    def __sub__(self, other: Metrics) -> dict[str, int]:
        return {f.name: getattr(self, f.name) - getattr(other, f.name) for f in fields(self)}


#: Module singleton. Instrumentation increments this directly.
CURRENT = Metrics()


def bump(name: str, n: int = 1) -> None:
    setattr(CURRENT, name, getattr(CURRENT, name) + n)


@contextmanager
def measure() -> Iterator[Metrics]:
    """Count operations performed inside the block.

    Yields a `Metrics` holding the *delta*, filled in on exit, so a test reads
    naturally:

        with measure() as m:
            model.render()
        assert m.rows_materialized <= height
    """
    before = Metrics(**CURRENT.snapshot())
    delta = Metrics()
    try:
        yield delta
    finally:
        for name, value in (CURRENT - before).items():
            setattr(delta, name, value)
