; Indent after opening braces
[
  (block)
  (class_body)
  (interface_body)
  (enum_body)
  (array_initializer)
  (switch_block)
] @indent

; Indent after control flow
[
  (if_statement)
  (for_statement)
  (enhanced_for_statement)
  (while_statement)
  (do_statement)
  (try_statement)
  (catch_clause)
  (finally_clause)
  (switch_block_statement_group)
] @indent

; Dedent closing delimiters
[
  "}"
  "]"
  ")"
] @dedent
