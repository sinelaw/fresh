/**
 * Text measurement shared across the plugin library.
 *
 * The runtime speaks bytes — overlay ranges, syntax-region offsets and
 * prefixes are all UTF-8 byte counts — while JS strings are UTF-16. Every
 * consumer that hands the host an offset needs the conversion, so it lives
 * here rather than being redefined per plugin.
 */

/** UTF-8 byte length of `s`. */
export function byteLength(s: string): number {
  let b = 0;
  for (let i = 0; i < s.length; i++) {
    const code = s.charCodeAt(i);
    if (code <= 0x7f) b += 1;
    else if (code <= 0x7ff) b += 2;
    else if (code >= 0xd800 && code <= 0xdfff) {
      b += 4;
      i++;
    } else b += 3;
  }
  return b;
}

/**
 * `text` broken into lines of at most `width` columns, as `measure` counts
 * them, at spaces where it can: the space a line breaks at is dropped, and a
 * word wider than a line is cut. Iterates by code point, so a character
 * outside the Basic Multilingual Plane is never split.
 */
export function wrapToWidth(text: string, width: number, measure: (s: string) => number): string[] {
  if (width <= 0 || measure(text) <= width) return [text];
  const lines: string[] = [];
  let lineStart = 0;
  let lastSpace = -1;
  let col = 0;
  let i = 0;
  while (i < text.length) {
    const charLen = text.codePointAt(i)! > 0xffff ? 2 : 1;
    const ch = text.slice(i, i + charLen);
    const w = measure(ch);
    if (col + w > width && i > lineStart) {
      if (ch === " ") {
        // The space that overflows ends the line, and goes with it.
        lines.push(text.slice(lineStart, i));
        lineStart = i + 1;
        col = 0;
      } else if (lastSpace > lineStart) {
        lines.push(text.slice(lineStart, lastSpace));
        lineStart = lastSpace + 1;
        col = measure(text.slice(lineStart, i + charLen));
      } else {
        lines.push(text.slice(lineStart, i));
        lineStart = i;
        col = w;
      }
      lastSpace = -1;
      i += charLen;
      continue;
    }
    if (ch === " ") lastSpace = i;
    col += w;
    i += charLen;
  }
  if (lineStart < text.length) lines.push(text.slice(lineStart));
  return lines.length > 0 ? lines : [text];
}
