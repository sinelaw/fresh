; Indent after control structures
[
  (if_statement)
  (while_statement)
  (for_statement)
  (case_statement)
  (case_item)
  (function_definition)
  (compound_statement)
] @indent

; Indent inside do...done, then...fi
[
  (do_group)
] @indent

; Dedent on closing keywords
[
  "done"
  "fi"
  "esac"
] @dedent
