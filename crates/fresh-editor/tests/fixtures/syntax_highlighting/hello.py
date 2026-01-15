#!/usr/bin/env python3

def greet(name: str) -> str:
    return f"Hello, {name}!"

if __name__ == "__main__":
    message = greet("World")
    print(message)
