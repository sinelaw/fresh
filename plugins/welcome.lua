-- Welcome Plugin
-- Shows a welcome message and registers test commands

-- Show welcome message in status bar
editor.set_status("✨ Plugins are working! Welcome Plugin loaded successfully!")

-- Register commands that use built-in actions
editor.register_command({
    name = "Plugin Demo: Open Help",
    description = "Open the editor help page (uses built-in action)",
    action = "show_help",
    contexts = {"normal"}
})

editor.register_command({
    name = "Plugin Demo: Save File",
    description = "Save the current file (uses built-in action)",
    action = "save",
    contexts = {"normal"}
})

-- Register commands with custom Lua callbacks
editor.register_command({
    name = "Plugin Demo: Say Hello",
    description = "Show a friendly greeting from Lua",
    action = "plugin_say_hello",
    contexts = {"normal"},
    callback = function()
        editor.set_status("👋 Hello from Lua! The plugin system is working!")
        print("Plugin callback executed: say_hello")
    end
})

editor.register_command({
    name = "Plugin Demo: Show Time",
    description = "Display the current time",
    action = "plugin_show_time",
    contexts = {"normal"},
    callback = function()
        local time = os.date("%H:%M:%S")
        editor.set_status("🕐 Current time: " .. time)
        print("Plugin callback executed: show_time at " .. time)
    end
})

editor.register_command({
    name = "Plugin Demo: Insert Text",
    description = "Insert sample text at cursor position",
    action = "plugin_insert_sample",
    contexts = {"normal"},
    callback = function()
        editor.insert_text(0, 0, "-- Hello from Lua plugin!\n")
        editor.set_status("📝 Sample text inserted by plugin")
        print("Plugin callback executed: insert_sample")
    end
})

-- Print to logs (visible in /tmp/editor.log)
print("🎉 Welcome plugin initialized successfully!")
print("📝 Registered 5 commands - try Ctrl+P to see them!")
print("   - 'Plugin Demo: Open Help' - toggles help screen (built-in action)")
print("   - 'Plugin Demo: Save File' - saves current file (built-in action)")
print("   - 'Plugin Demo: Say Hello' - shows greeting (Lua callback)")
print("   - 'Plugin Demo: Show Time' - displays current time (Lua callback)")
print("   - 'Plugin Demo: Insert Text' - inserts sample text (Lua callback)")
