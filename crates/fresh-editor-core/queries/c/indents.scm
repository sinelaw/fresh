; Indent after opening braces
[
  (compound_statement)
  (struct_specifier)
  (enum_specifier)
  (field_declaration_list)
  (enumerator_list)
  (initializer_list)
] @indent

; Indent after control flow
[
  (if_statement)
  (for_statement)
  (while_statement)
  (do_statement)
  (switch_statement)
  (case_statement)
] @indent

; Dedent closing delimiters
[
  "}"
  "]"
  ")"
] @dedent
