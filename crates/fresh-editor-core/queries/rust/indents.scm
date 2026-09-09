; Indent after opening braces and blocks
[
  (block)
  (struct_expression)
  (enum_variant_list)
  (field_declaration_list)
  (declaration_list)
  (match_block)
  (token_tree)
  (use_list)
] @indent

; Indent inside function definitions
(function_item
  body: (block)) @indent

; Indent inside impl blocks
(impl_item
  body: (declaration_list)) @indent

; Indent match arms
(match_arm) @indent

; Indent array expressions
(array_expression) @indent

; Indent tuple expressions
(tuple_expression) @indent

; Dedent closing delimiters
[
  "}"
  "]"
  ")"
] @dedent

; Keep same indent for these
[
  "where"
  "else"
] @dedent
