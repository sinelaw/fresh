; Indent after opening braces
[
  (block)
  (class_declaration)
  (struct_declaration)
  (interface_declaration)
  (enum_declaration)
  (namespace_declaration)
  (switch_section)
  (accessor_list)
  (declaration_list)
  (initializer_expression)
] @indent

; Indent after control flow
[
  (if_statement)
  (for_statement)
  (foreach_statement)
  (while_statement)
  (do_statement)
  (try_statement)
  (catch_clause)
  (finally_clause)
  (using_statement)
  (lock_statement)
] @indent

; Dedent closing delimiters
[
  "}"
  "]"
  ")"
] @dedent
