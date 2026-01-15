#!/usr/bin/env python3
"""
Convert PNG image to ANSI color text output.
Each pixel becomes a full block character (█) with 24-bit true color.
"""

import sys
from PIL import Image


def png_to_ansi(image_path: str) -> str:
    """Convert a PNG file to ANSI colored text output."""
    img = Image.open(image_path)
    img = img.convert('RGB')

    width, height = img.size
    pixels = img.load()

    output = []

    for y in range(height):
        row = []
        for x in range(width):
            r, g, b = pixels[x, y]
            # Use 24-bit true color ANSI escape code
            # \x1b[38;2;R;G;Bm sets foreground color
            row.append(f'\x1b[38;2;{r};{g};{b}m█')
        output.append(''.join(row) + '\x1b[0m')  # Reset at end of each row

    return '\n'.join(output)


def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <image.png>", file=sys.stderr)
        sys.exit(1)

    image_path = sys.argv[1]

    try:
        result = png_to_ansi(image_path)
        print(result)
    except FileNotFoundError:
        print(f"Error: File '{image_path}' not found", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
