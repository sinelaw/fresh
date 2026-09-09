; Indent after opening braces
[
  (compound_statement)
  (struct_specifier)
  (class_specifier)
  (enum_specifier)
  (field_declaration_list)
  (enumerator_list)
  (initializer_list)
] @indent

; Indent after control flow
[
  (if_statement)
  (for_statement)
  (for_range_loop)
  (while_statement)
  (do_statement)
  (switch_statement)
  (case_statement)
  (try_statement)
  (catch_clause)
] @indent

; Indent namespaces
(namespace_definition) @indent

; Dedent closing delimiters
[
  "}"
  "]"
  ")"
] @dedent
