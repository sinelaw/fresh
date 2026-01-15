# Markdown Compose Mode Test

This is a **beautiful** document that tests the *Markdown compose mode* in Fresh editor.

## Features

The compose mode provides semi-WYSIWYG rendering with:

- Soft breaks for paragraph wrapping
- **Bold text** styling
- *Italic text* styling
- `inline code` highlighting
- [Links to resources](https://example.com)

### Code Blocks

Here's a code example:

```rust
fn main() {
    println!("Hello, Fresh!");
}
```

### Lists and Tasks

1. First ordered item
2. Second ordered item
3. Third item with hard break

Unordered lists work too:

* Item one
* Item two
  * Nested item
* Item three

Task lists:

- [ ] Unchecked task
- [x] Checked task
- [ ] Another task

### Block Quotes

> This is a block quote.
> It can span multiple lines.
>
> And have multiple paragraphs.

### Horizontal Rules

Content above

---

Content below

## Soft vs Hard Breaks

This paragraph demonstrates soft breaks. Each line will flow together when rendered in compose mode, creating a nicely wrapped paragraph that adapts to the terminal width.

This is on the next line but will merge with the previous.

Empty lines create new paragraphs.

Lines ending with two spaces
create hard breaks.

Lines ending with backslash\
also create hard breaks.

## Inline Styles

You can combine **bold** and *italic* for ***bold italic*** text.

Here's ~~strikethrough~~ text.

Mix `code` with **bold** and *italic* freely.

## Links

Check out [Fresh Editor](https://github.com/user/fresh) for more info.

[Reference-style links][1] are also supported.

[1]: https://example.com

## Conclusion

This document tests various Markdown features for the compose mode renderer.
