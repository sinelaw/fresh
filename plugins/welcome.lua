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
    description = "Insert a friendly greeting into the buffer",
    action = "plugin_say_hello",
    contexts = {"normal"},
    callback = function()
        editor.insert("👋 Hello from Lua! The plugin system is working!\n")
        editor.set_status("Inserted greeting at cursor position")
        debug("Plugin callback executed: say_hello")
    end
})

editor.register_command({
    name = "Plugin Demo: Insert Time",
    description = "Insert the current time at cursor position",
    action = "plugin_insert_time",
    contexts = {"normal"},
    callback = function()
        local time = os.date("%H:%M:%S")
        editor.insert("🕐 Current time: " .. time .. "\n")
        editor.set_status("Inserted time at cursor position")
        debug("Plugin callback executed: insert_time at " .. time)
    end
})

editor.register_command({
    name = "Plugin Demo: Insert Comment",
    description = "Insert a sample comment at cursor position",
    action = "plugin_insert_comment",
    contexts = {"normal"},
    callback = function()
        editor.insert("-- This comment was inserted by a Lua plugin!\n")
        editor.set_status("📝 Comment inserted by plugin")
        debug("Plugin callback executed: insert_comment")
    end
})

-- Debug output (goes to temp file, opens in background tab on first debug call)
debug("🎉 Welcome plugin initialized successfully!")
debug("📝 Registered 5 commands - try Ctrl+P to see them!")
debug("   - 'Plugin Demo: Open Help' - toggles help screen (built-in action)")
debug("   - 'Plugin Demo: Save File' - saves current file (built-in action)")
debug("   - 'Plugin Demo: Say Hello' - inserts greeting (Lua callback)")
debug("   - 'Plugin Demo: Insert Time' - inserts current time (Lua callback)")
debug("   - 'Plugin Demo: Insert Comment' - inserts sample comment (Lua callback)")
debug("")
debug("🔍 You're viewing this in the debug log! It opened automatically when the plugin called debug().")
