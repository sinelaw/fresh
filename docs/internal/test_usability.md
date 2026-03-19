# Usability Test Document

This tests all the **seamless canvas** features of the *markdown compose* plugin.

## Emphasis Tests

Here is **bold text** in a sentence.
Here is *italic text* in a sentence.
Here is `inline code` in a sentence.
Here is ~~strikethrough text~~ in a sentence.
Here is ***bold italic text*** in a sentence.
Mix **bold** with *italic* and `code` freely.

## Link Tests

Visit [Fresh Editor](https://github.com/user/fresh) for more.
Check [Example Site](https://example.com) too.
Here's a [link with spaces](https://example.com/path/to/page) in text.
Not a link: ![image alt](https://example.com/img.png) should stay.

## Table Test

| Feature | Status | Notes |
|---------|--------|-------|
| Bold | Done | Working well |
| Italic | Done | Working well |
| Links | Done | Blue underline |
| Tables | Done | Box-drawing |

## Another Table

| Name | Age | City |
|------|-----|------|
| Alice | 30 | NYC |
| Bob | 25 | LA |

## Wrapping Test

This is a very long paragraph that should be soft-wrapped by the compose mode plugin at the configured width, which defaults to 80 characters. The text should flow naturally across multiple visual lines while maintaining proper cursor movement through each visual line.

## List with Long Text

- This is a bullet point with enough text that it should wrap to a second line and demonstrate the hanging indent feature properly
- Short item
- Another long item that goes on and on to test the hanging indent behavior when the line extends past the compose width boundary

## Block Quote

> This is a block quote that has quite a lot of text in it, enough that it should also demonstrate wrapping behavior within the quoted section.

## Code Block

```rust
fn main() {
    println!("Hello, World!");
    let x = 42;
}
```

## Mixed Content

Here's a paragraph with **bold links**: check [this **bold** link](https://example.com).
Also try `code with **no bold**` inside backticks.
And ~~struck [link text](https://url.com) through~~ text.

## Cursor Navigation Test

Line 1
Line 2
Line 3 is a longer line that will need to be wrapped by the compose mode when it exceeds the configured compose width threshold for this particular document.
Line 4
Line 5
