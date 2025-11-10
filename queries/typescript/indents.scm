; TypeScript extends JavaScript, so include JS rules
; Indent after opening braces and blocks
[
  (statement_block)
  (object_type)
  (object)
  (object_pattern)
  (class_body)
  (interface_body)
  (enum_body)
  (switch_body)
] @indent

; Indent function bodies
[
  (function_declaration)
  (function_expression)
  (arrow_function)
  (method_definition)
  (method_signature)
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

; Indent arrays and tuples
[
  (array)
  (array_pattern)
  (tuple_type)
] @indent

; Indent switch cases
(switch_case) @indent

; Dedent closing delimiters
[
  "}"
  "]"
  ")"
] @dedent
