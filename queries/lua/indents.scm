; Indent after opening blocks
[
  (block)
  (function_definition)
  (function_declaration)
  (if_statement)
  (for_statement)
  (while_statement)
  (repeat_statement)
  (do_statement)
  (table_constructor)
] @indent

; Dedent closing delimiters
[
  "end"
  "}"
  "]"
  ")"
  "until"
] @dedent
