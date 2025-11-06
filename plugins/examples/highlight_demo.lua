-- Highlight Demo Plugin
-- Demonstrates overlay functionality

-- Register command to add a red highlight
editor.register_command({
    name = "Highlight: Red",
    description = "Add a red highlight overlay to buffer",
    action = "none",
    contexts = {"normal"}
})

-- Register command to add a green highlight
editor.register_command({
    name = "Highlight: Green",
    description = "Add a green highlight overlay to buffer",
    action = "none",
    contexts = {"normal"}
})

-- Register command to add a blue highlight
editor.register_command({
    name = "Highlight: Blue",
    description = "Add a blue highlight overlay to buffer",
    action = "none",
    contexts = {"normal"}
})

-- Example: Add an overlay to buffer 0 at position 0-10
-- editor.add_overlay(buffer_id, overlay_id, start, end, r, g, b, underline)
-- Uncomment to test:
-- editor.add_overlay(0, "demo-red", 0, 10, 255, 0, 0, true)

print("Highlight Demo plugin loaded with 3 commands")
editor.set_status("Highlight Demo plugin ready")
