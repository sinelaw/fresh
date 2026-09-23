/** tmux format-output escaping. Getting it wrong is silent, so it is pinned here. */
import {
  splitEscapedFields,
} from "../lib/tmux_format.ts";

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

// Built by concatenation so the input is what tmux puts on the wire.
const BS = "\\";
const SEP = BS + "037";

eq(
  splitEscapedFields("one" + SEP + "two" + SEP + "three"),
  ["one", "two", "three"],
  "plain separators split",
);

// tmux doubles a literal backslash; splitting on the escape as a substring shifts fields.
eq(
  splitEscapedFields(
    "$0" + SEP + "0" + SEP + "w" + SEP + "%0" + SEP + "1" + SEP + "/tmp" + SEP +
      "bash" + SEP + "x" + BS + BS + "037y",
  ),
  ["$0", "0", "w", "%0", "1", "/tmp", "bash", "x" + BS + "037y"],
  "an escaped backslash decodes to one backslash and does not split the row",
);

eq(
  splitEscapedFields("$0" + SEP + "0" + SEP + "a" + BS + "011b" + SEP + "%0"),
  ["$0", "0", "a\tb", "%0"],
  "a tab in a window name decodes rather than splitting",
);

// Unknown escapes are passed through rather than dropped.
eq(splitEscapedFields("a" + BS), ["a" + BS], "a lone trailing backslash survives");
eq(splitEscapedFields("a" + BS + "777b"), ["a" + BS + "777b"], "\\777 exceeds a byte, so it is not an escape");
eq(splitEscapedFields("a" + BS + "08b"), ["a" + BS + "08b"], "8 is not an octal digit, so \\08 is not an escape");

eq(
  splitEscapedFields("$0" + SEP + SEP + "x"),
  ["$0", "", "x"],
  "an empty field is preserved rather than collapsed",
);

process.exit(failures === 0 ? 0 : 1);
