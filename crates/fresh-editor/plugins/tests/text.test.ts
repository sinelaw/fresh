/** Text measurement and wrapping (`lib/text.ts`). */
import { wrapToWidth } from "../lib/text.ts";

let failures = 0;
function eq(actual: unknown, expected: unknown, name: string): void {
  const a = JSON.stringify(actual);
  const e = JSON.stringify(expected);
  if (a !== e) {
    console.log(`FAIL ${name}\n  got      ${a}\n  expected ${e}`);
    failures++;
  } else {
    console.log(`ok   ${name}`);
  }
}

/** Terminal columns: two for CJK ideographs and emoji, one otherwise. */
const width = (s: string): number =>
  [...s].reduce((w, ch) => w + (/[一-鿿]|\p{Extended_Pictographic}/u.test(ch) ? 2 : 1), 0);

eq(wrapToWidth("short", 10, width), ["short"], "text that fits is one line");
eq(wrapToWidth("hello world", 5, width), ["hello", "world"], "the space a line overflows at ends it");
eq(wrapToWidth("one two three four", 7, width), ["one two", "three", "four"], "lines break at the last space that fits");
eq(wrapToWidth("abcdefgh", 3, width), ["abc", "def", "gh"], "a word wider than a line is cut");
eq(wrapToWidth("漢字 漢字 漢字", 5, width), ["漢字", "漢字", "漢字"], "wide characters are measured in columns");
eq(wrapToWidth("😀😀😀", 4, width), ["😀😀", "😀"], "a character outside the BMP is never split");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
