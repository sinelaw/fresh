# Nim syntax highlighting test
import strformat

proc greet(name: string): string =
  fmt"Hello, {name}!"

type
  Config = object
    version: string
    enabled: bool
    count: int

proc main() =
  let message = greet("World")
  echo message

  let items = @[1, 2, 3, 4, 5]
  for item in items:
    echo fmt"Item: {item}"

when isMainModule:
  main()
