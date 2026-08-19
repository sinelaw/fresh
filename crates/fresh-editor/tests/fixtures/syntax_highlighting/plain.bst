ENTRY
  { author
    title
    year
  }
  {}
  { label }

STRINGS { s }

FUNCTION {format.title}
{ title empty$
    { "Untitled" }
    { title "t" change.case$ }
  if$
}

READ
ITERATE {call.type$}
