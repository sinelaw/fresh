# Julia syntax highlighting test
module Hello

function greet(name::String)::String
    return "Hello, $name!"
end

struct Config
    version::String
    enabled::Bool
    count::Int64
end

function main()
    message = greet("World")
    println(message)

    items = [1, 2, 3, 4, 5]
    for item in items
        println("Item: $item")
    end
end

end # module
