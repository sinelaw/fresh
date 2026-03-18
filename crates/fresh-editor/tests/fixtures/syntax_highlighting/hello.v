// V syntax highlighting test
module main

import os

fn greet(name string) string {
    return 'Hello, ${name}!'
}

struct Config {
    version string
    enabled bool = true
    count   int = 42
}

fn main() {
    message := greet('World')
    println(message)

    items := [1, 2, 3, 4, 5]
    for item in items {
        println('Item: ${item}')
    }
}
