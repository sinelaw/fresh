// Typst syntax highlighting test
#set page(paper: "a4")
#set text(font: "New Computer Modern", size: 12pt)

= Hello World

This is a *bold* and _italic_ test.

#let greet(name) = [Hello, #name!]

#greet("World")

$ E = m c^2 $

#table(
  columns: 2,
  [Column 1], [Column 2],
  [Value 1], [Value 2],
)
