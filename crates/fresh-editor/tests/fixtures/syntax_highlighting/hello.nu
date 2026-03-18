# Nushell syntax highlighting test
def greet [name: string] {
    $"Hello, ($name)!"
}

let config = {
    version: "1.0"
    enabled: true
    count: 42
}

def main [] {
    let message = greet "World"
    print $message

    [1 2 3 4 5] | each { |item|
        print $"Item: ($item)"
    }
}
