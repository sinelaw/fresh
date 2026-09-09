; Indent after compound statements
[
  (function_definition)
  (class_definition)
  (if_statement)
  (elif_clause)
  (else_clause)
  (for_statement)
  (while_statement)
  (with_statement)
  (try_statement)
  (except_clause)
  (finally_clause)
  (match_statement)
  (case_clause)
] @indent

; Indent inside blocks
(block) @indent

; Indent lists, dicts, sets
[
  (list)
  (dictionary)
  (set)
  (tuple)
] @indent

; Indent function/lambda arguments and parameters
[
  (argument_list)
  (parameters)
  (lambda_parameters)
] @indent

; Dedent closing delimiters
[
  ")"
  "]"
  "}"
] @dedent

; Dedent the line *after* a statement that ends the block's straight-line
; flow. Pressing Enter at the end of one of these drops out one level, matching
; VS Code, PyCharm and neovim.
[
  (return_statement)
  (pass_statement)
  (raise_statement)
  (break_statement)
  (continue_statement)
] @dedent_after
