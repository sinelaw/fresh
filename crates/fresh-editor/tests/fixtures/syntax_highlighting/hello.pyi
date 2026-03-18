# Python stub file (.pyi) syntax highlighting test
from typing import Optional, List

def greet(name: str) -> str: ...
def process(items: List[int], flag: bool = True) -> Optional[str]: ...

class Config:
    version: str
    enabled: bool
    count: int
    def __init__(self, version: str) -> None: ...
