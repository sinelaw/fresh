-- Hello World Plugin
-- Demonstrates basic plugin functionality

-- Register a custom command
editor.register_command({
    name = "Hello World",
    description = "Display a hello world message",
    action = "none",
    contexts = {"normal"}
})

-- Set status message to show plugin loaded
editor.set_status("Hello World plugin loaded!")

-- Log to demonstrate plugin execution
print("Hello World plugin initialized")
