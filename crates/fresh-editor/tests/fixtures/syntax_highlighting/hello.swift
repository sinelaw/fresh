// Swift syntax highlighting test
import Foundation

func greet(_ name: String) -> String {
    return "Hello, \(name)!"
}

struct Config {
    let version: String
    var enabled: Bool = true
    let count: Int
}

let message = greet("World")
print(message)

let items = [1, 2, 3, 4, 5]
for item in items {
    print("Item: \(item)")
}
