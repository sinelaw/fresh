/// <reference path="./lib/fresh.d.ts" />
// Markdown Source Plugin
// List editing for Markdown files:
// - Enter: continues a list item (same bullet, the next number, an unchecked
//   checkbox) with matching indentation; on an empty item, removes the
//   marker instead; on any other indented line, keeps the indentation
// - Tab: on a blank bullet or checkbox item, indents one level (the buffer's
//   tab_size / use_tabs) and cycles the bullet (* -> - -> + -> *)
// - Shift+Tab: on a blank bullet or checkbox item with a level of
//   indentation, removes that level and cycles the bullet back
//   (+ -> - -> * -> +)
// Everywhere else the keys do what they always do.
//
// The plugin only registers these rules (`editor.registerEditRules`); the
// editor applies them itself, as part of the key's own edit, so they keep
// up with fast typing and apply in vi's insert mode too. Disabling the
// plugin turns them off.

const editor = getEditor();

// Language ids the rules apply to. `.md` / `.markdown` files are
// "markdown"; the others are ids a language pack or user config may give
// Markdown dialects.
const LANGUAGES = ["markdown", "multimarkdown", "mdx"];

// A blank list item's text before the cursor: indentation (group 1), a
// bullet (2) and an optional checkbox (3), then only whitespace.
const BLANK_BULLET_ITEM = "^([ \\t]*)([-*+]) (\\[[ xX]\\] )?\\s*$";
// Nothing but whitespace after the cursor.
const BLANK = "^\\s*$";

editor.registerEditRules("lists", {
  languages: LANGUAGES,
  rules: [
    // Enter on an empty item (marker only): remove the marker.
    {
      on: "newline",
      before: "^[ \\t]*(?:[-*+] \\[[ xX]\\] |\\d+\\. |[-*+] )\\s*$",
      after: BLANK,
      replaceBefore: "",
      replaceAfter: "",
    },
    // Enter on a checkbox item: a new unchecked one.
    { on: "newline", before: "^([ \\t]*)([-*+]) \\[[ xX]\\] ", insert: "\n$1$2 [ ] " },
    // Enter on an ordered item: the next number.
    { on: "newline", before: "^([ \\t]*)(\\d+)\\. ", insert: "\n$1${inc:2}. " },
    // Enter on a bullet item: the same bullet.
    { on: "newline", before: "^([ \\t]*)([-*+]) ", insert: "\n$1$2 " },
    // Enter on any other indented line: keep the indentation.
    { on: "newline", before: "^([ \\t]+)", insert: "\n$1" },
    // Tab on a blank bullet item: one level deeper, next bullet.
    {
      on: "indent",
      before: BLANK_BULLET_ITEM,
      after: BLANK,
      replaceBefore: "$1${indent}${cycle:2:*|-|+} $3",
      replaceAfter: "",
    },
    // Shift+Tab on a blank bullet item: one level out, previous bullet.
    // `${dedent}` leaves the rule unapplied when there is no level to take.
    {
      on: "dedent",
      before: BLANK_BULLET_ITEM,
      after: BLANK,
      replaceBefore: "${dedent:1}${cycle:2:+|-|*} $3",
      replaceAfter: "",
    },
  ],
});

editor.debug("markdown_source plugin loaded");
