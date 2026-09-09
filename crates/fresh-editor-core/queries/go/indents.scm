; Indent after opening braces
[
  (block)
  (literal_value)
  (struct_type)
  (interface_type)
  (field_declaration_list)
  (interface_type)
] @indent

; Indent after control flow
[
  (if_statement)
  (for_statement)
  (expression_switch_statement)
  (type_switch_statement)
  (select_statement)
  (communication_case)
  (expression_case)
  (type_case)
  (default_case)
] @indent

; Indent function declarations
[
  (function_declaration)
  (method_declaration)
  (func_literal)
] @indent

; Dedent closing delimiters
[
  "}"
  "]"
  ")"
] @dedent
