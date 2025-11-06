-- Welcome Plugin
-- Shows a welcome message and registers a test command

-- Show welcome message in status bar
editor.set_status("✨ Plugins are working! Welcome Plugin loaded successfully!")

-- Register a custom command that will appear in command palette (Ctrl+P)
editor.register_command({
    name = "Plugin Demo: Say Hello",
    description = "Test command from the welcome plugin",
    action = "none",
    contexts = {"normal"}
})

-- Register another command
editor.register_command({
    name = "Plugin Demo: Show Status",
    description = "Display a custom status message",
    action = "none",
    contexts = {"normal"}
})

-- Print to logs (visible in /tmp/editor.log)
print("🎉 Welcome plugin initialized successfully!")
print("📝 Registered 2 commands - try Ctrl+P to see them")
