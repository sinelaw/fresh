// Gleam syntax highlighting test
import gleam/io
import gleam/string
import gleam/list

pub type Config {
  Config(version: String, enabled: Bool, count: Int)
}

pub fn greet(name: String) -> String {
  string.concat(["Hello, ", name, "!"])
}

pub fn main() {
  let message = greet("World")
  io.println(message)

  let items = [1, 2, 3, 4, 5]
  list.each(items, fn(item) {
    io.println(string.concat(["Item: ", string.inspect(item)]))
  })
}
