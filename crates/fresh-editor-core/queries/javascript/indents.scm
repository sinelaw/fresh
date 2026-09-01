; Indent after opening braces and blocks
[
  (statement_block)
  (object)
  (object_pattern)
  (class_body)
  (switch_body)
] @indent

; Indent function bodies
[
  (function_declaration)
  (function_expression)
  (arrow_function)
  (method_definition)
] @indent

; Indent control flow statements
[
  (if_statement)
  (for_statement)
  (for_in_statement)
  (while_statement)
  (do_statement)
  (try_statement)
  (catch_clause)
  (finally_clause)
] @indent

; Indent arrays
[
  (array)
  (array_pattern)
] @indent

; Indent switch cases
(switch_case) @indent

; Dedent closing delimiters
[
  "}"
  "]"
  ")"
] @dedent
