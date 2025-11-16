-- Diagnostics Panel Plugin
-- Displays LSP diagnostics in a virtual buffer with Emacs-style navigation
--
-- This plugin demonstrates the virtual buffer infrastructure by creating
-- a special buffer that lists diagnostics with embedded source locations.

-- Track if panel is open
local panel_open = false

-- Store current diagnostics data
local diagnostics_data = {}

-- Initialize the diagnostics-list mode
local function setup_mode()
    editor.define_mode({
        name = "diagnostics-list",
        parent = "special",  -- Inherits 'q' to quit, 'g' to refresh
        bindings = {
            ["RET"] = "diagnostics:goto",      -- Jump to diagnostic location
            ["n"] = "diagnostics:next-line",   -- Move to next line
            ["p"] = "diagnostics:prev-line",   -- Move to previous line
        },
        read_only = true
    })
    debug("Registered diagnostics-list mode")
end

-- Format a diagnostic for display
local function format_diagnostic(diag)
    local severity_icon = {
        error = "E",
        warning = "W",
        info = "I",
        hint = "H"
    }

    local icon = severity_icon[diag.severity] or "?"
    return string.format("[%s] %s:%d:%d: %s\n",
        icon, diag.file, diag.line, diag.column, diag.message)
end

-- Build entries from diagnostics data
local function build_entries(mock_diagnostics)
    local entries = {}

    -- Add header
    table.insert(entries, {
        text = "=== LSP Diagnostics ===\n\n",
        properties = {}
    })

    -- Add each diagnostic with embedded properties
    for i, diag in ipairs(mock_diagnostics) do
        local text = format_diagnostic(diag)
        table.insert(entries, {
            text = text,
            properties = {
                diagnostic_index = i,
                file = diag.file,
                line = diag.line,
                column = diag.column,
                severity = diag.severity,
                message = diag.message
            }
        })
    end

    -- Add footer
    table.insert(entries, {
        text = string.format("\nTotal: %d diagnostics", #mock_diagnostics),
        properties = {}
    })

    return entries
end

-- Create the diagnostic panel with content
local function create_panel()
    -- Generate mock diagnostics for demonstration
    -- In a real implementation, these would come from LSP
    local mock_diagnostics = {
        {
            severity = "error",
            file = "src/main.rs",
            line = 42,
            column = 5,
            message = "mismatched types: expected `usize`, found `i32`"
        },
        {
            severity = "warning",
            file = "src/lib.rs",
            line = 156,
            column = 1,
            message = "unused variable: `temp`"
        },
        {
            severity = "error",
            file = "src/editor.rs",
            line = 1024,
            column = 12,
            message = "cannot borrow `*self` as mutable because it is also borrowed as immutable"
        },
        {
            severity = "info",
            file = "src/plugin_api.rs",
            line = 89,
            column = 8,
            message = "consider using `&str` instead of `String`"
        },
        {
            severity = "hint",
            file = "src/buffer.rs",
            line = 203,
            column = 15,
            message = "this function has too many arguments (6/5)"
        }
    }

    diagnostics_data = mock_diagnostics

    -- Build entries with text properties
    local entries = build_entries(mock_diagnostics)

    debug(string.format("Creating diagnostics panel with %d entries", #entries))

    -- Create the virtual buffer with content in one shot
    editor.create_virtual_buffer_with_content({
        name = "*Diagnostics*",
        mode = "diagnostics-list",
        read_only = true,
        entries = entries
    })

    panel_open = true
    editor.set_status(string.format("Diagnostics panel: %d items", #mock_diagnostics))
end

-- Toggle the diagnostics panel
function toggle_diagnostics_panel()
    debug("Toggling diagnostics panel")
    create_panel()
end

-- Jump to the diagnostic at cursor
function goto_diagnostic()
    -- Get the text properties at the current cursor position
    -- This would use the embedded location data to jump to the source
    debug("Goto diagnostic (would jump to source location)")
    editor.set_status("Jump to diagnostic source (not yet implemented)")
end

-- Move to next diagnostic line
function diagnostics_next_line()
    -- Move cursor down
    debug("Next diagnostic")
    editor.set_status("Moving to next diagnostic")
end

-- Move to previous diagnostic line
function diagnostics_prev_line()
    -- Move cursor up
    debug("Previous diagnostic")
    editor.set_status("Moving to previous diagnostic")
end

-- Register commands
editor.register_command({
    name = "Show Diagnostics",
    description = "Show LSP diagnostics in a panel",
    action = "toggle_diagnostics_panel",
    contexts = {"normal"}
})

editor.register_command({
    name = "Goto Diagnostic",
    description = "Jump to diagnostic source location",
    action = "goto_diagnostic",
    contexts = {"normal"}
})

editor.register_command({
    name = "Next Diagnostic Line",
    description = "Move to next diagnostic",
    action = "diagnostics_next_line",
    contexts = {"normal"}
})

editor.register_command({
    name = "Previous Diagnostic Line",
    description = "Move to previous diagnostic",
    action = "diagnostics_prev_line",
    contexts = {"normal"}
})

-- Initialize the mode on load
setup_mode()

debug("Diagnostics panel plugin loaded")
editor.set_status("Diagnostics panel plugin ready - use 'Show Diagnostics' command")
