; Indent begin - block constructs that introduce a new indentation level

[
  (annotation_directive)
  (array_data_directive)
  (field_definition)
  (method_definition)
  (packed_switch_directive)
  (param_directive)
  (parameter_directive)
  (sparse_switch_directive)
  (subannotation_directive)
  (list)
] @indent.begin

; Indent end - closing keywords that dedent

[
  ".end annotation"
  ".end array-data"
  ".end field"
  ".end method"
  ".end packed-switch"
  ".end param"
  ".end parameter"
  ".end sparse-switch"
  ".end subannotation"
  "}"
] @indent.end

; Branch tokens - lines containing these should be dedented to match their pair

[
  ".end annotation"
  ".end array-data"
  ".end field"
  ".end method"
  ".end packed-switch"
  ".end param"
  ".end parameter"
  ".end sparse-switch"
  ".end subannotation"
  "{"
  "}"
] @indent.branch

; Auto-indent nodes - these should maintain current indentation level

[
  (ERROR)
  (comment)
] @indent.auto
