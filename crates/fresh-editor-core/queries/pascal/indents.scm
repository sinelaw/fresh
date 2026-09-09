; Indent after begin blocks
(block) @indent

; Indent after procedure/function definitions
[
  (defProc)
  (declProc)
] @indent

; Indent after control flow statements
[
  (if)
  (ifElse)
  (while)
  (repeat)
  (for)
  (foreach)
  (case)
  (try)
  (with)
] @indent

; Indent inside record/object definitions
[
  (declRecord)
  (declClass)
  (declObject)
  (declInterface)
] @indent

; Indent array/record initializers
[
  (arrInitializer)
  (recInitializer)
] @indent

; Indent interface/implementation sections
[
  (interface)
  (implementation)
] @indent

; Dedent closing delimiters
[
  (kEnd)
  "]"
  ")"
] @dedent

; Keep same indent for else/then
[
  (kElse)
  (kThen)
] @dedent
