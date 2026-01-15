#!/usr/bin/env lua

-- Greeting function with string formatting
function greet(name)
    return string.format("Hello, %s!", name)
end

-- Table with mixed content
local config = {
    version = "1.0",
    enabled = true,
    count = 42
}

-- Main execution with conditional logic
if arg then
    local message = greet("World")
    print(message)

    for i = 1, 3 do
        print("Iteration: " .. i)
    end
end
