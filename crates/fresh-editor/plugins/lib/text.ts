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
