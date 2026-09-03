; Indent after block-like constructs
[
  (block)
  (struct_type)
  (union_type)
  (enum_type)
  (bit_field_type)
  (procedure_body)
] @indent

; Indent after control flow
[
  (if_statement)
  (for_statement)
  (switch_statement)
  (when_statement)
  (case_statement)
] @indent

; Indent procedure declarations and literals
[
  (procedure_declaration)
  (procedure_literal)
] @indent

; Dedent closing delimiters
[
  "}"
  "]"
  ")"
] @dedent
