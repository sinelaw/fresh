; Indent after opening braces
[
  (compound_statement)
  (switch_block)
  (declaration_list)
  (class_declaration)
  (interface_declaration)
  (trait_declaration)
] @indent

; Indent after control flow
[
  (if_statement)
  (for_statement)
  (foreach_statement)
  (while_statement)
  (do_statement)
  (switch_statement)
  (case_statement)
  (try_statement)
  (catch_clause)
  (finally_clause)
] @indent

; Indent arrays
[
  (array_creation_expression)
] @indent

; Dedent closing delimiters
[
  "}"
  "]"
  ")"
] @dedent
