// Odin syntax highlighting test
package main

import "core:fmt"
import "core:strings"

greet :: proc(name: string) -> string {
    return fmt.tprintf("Hello, %s!", name)
}

main :: proc() {
    message := greet("World")
    fmt.println(message)

    items := [?]int{1, 2, 3, 4, 5}
    for item in items {
        fmt.printf("Item: %d\n", item)
    }
}
