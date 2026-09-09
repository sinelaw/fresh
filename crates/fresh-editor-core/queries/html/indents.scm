; Indent inside elements
(element) @indent

; Indent inside script and style tags
[
  (script_element)
  (style_element)
] @indent

; Dedent closing tags (handled by element end)
[
  (end_tag)
] @dedent
