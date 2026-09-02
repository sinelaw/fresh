; Indent after GDScript block-introducing statements.
[
  (class_definition)
  (function_definition)
  (if_statement)
  (elif_clause)
  (else_clause)
  (for_statement)
  (while_statement)
  (match_statement)
  (pattern_section)
] @indent

; Indent nested suites and collection literals.
[
  (body)
  (class_body)
  (match_body)
  (array)
  (dictionary)
  (arguments)
  (parameters)
] @indent

; Dedent closing delimiters for bracket continuations.
[
  ")"
  "]"
  "}"
] @dedent

; Leave a completed block after flow-ending statements.
[
  (return_statement)
  (pass_statement)
  (break_statement)
  (continue_statement)
] @dedent_after
