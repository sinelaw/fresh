-- Diagnostics Panel Plugin
-- Displays LSP diagnostics in a virtual buffer with Emacs-style navigation
--
-- Features:
-- - Opens in a horizontal split below current buffer
-- - Navigate with Up/Down arrows or n/p keys
-- - Jump to diagnostic location with Enter

-- Track the diagnostics panel state
local panel_state = {
    open = false,
    current_index = 1,  -- Currently selected diagnostic (1-indexed)
    diagnostics = {},   -- Current diagnostics data
    header_lines = 2,   -- Number of header lines before first diagnostic
}

-- Initialize the diagnostics-list mode
local function setup_mode()
    editor.define_mode({
        name = "diagnostics-list",
        parent = "special",  -- Inherits 'q' to quit, 'g' to refresh
        bindings = {
            ["RET"] = "diagnostics:goto",      -- Jump to diagnostic location
            ["n"] = "diagnostics:next",        -- Move to next diagnostic
            ["p"] = "diagnostics:prev",        -- Move to previous diagnostic
            ["Down"] = "diagnostics:next",     -- Arrow key support
            ["Up"] = "diagnostics:prev",       -- Arrow key support
            ["j"] = "diagnostics:next",        -- Vim-style
            ["k"] = "diagnostics:prev",        -- Vim-style
        },
        read_only = true
    })
    debug("Registered diagnostics-list mode")
end

-- Format a diagnostic for display
local function format_diagnostic(diag, index, is_selected)
    local severity_icon = {
        error = "E",
        warning = "W",
        info = "I",
        hint = "H"
    }

    local icon = severity_icon[diag.severity] or "?"
    local marker = is_selected and ">" or " "
    return string.format("%s [%s] %s:%d:%d: %s\n",
        marker, icon, diag.file, diag.line, diag.column, diag.message)
end

-- Build entries from diagnostics data
local function build_entries()
    local entries = {}

    -- Add header
    table.insert(entries, {
        text = "=== LSP Diagnostics ===\n\n",
        properties = {}
    })

    -- Add each diagnostic with embedded properties
    for i, diag in ipairs(panel_state.diagnostics) do
        local is_selected = (i == panel_state.current_index)
        local text = format_diagnostic(diag, i, is_selected)
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
        text = string.format("\nTotal: %d diagnostics (selected: %d/%d)",
            #panel_state.diagnostics, panel_state.current_index, #panel_state.diagnostics),
        properties = {}
    })

    return entries
end

-- Generate mock diagnostics (in a real implementation, these would come from LSP)
local function get_diagnostics()
    return {
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
end

-- Create or update the diagnostic panel
local function show_panel()
    -- Load diagnostics
    panel_state.diagnostics = get_diagnostics()
    panel_state.current_index = 1  -- Reset selection to first diagnostic

    if #panel_state.diagnostics == 0 then
        editor.set_status("No diagnostics to display")
        return
    end

    -- Build entries with text properties
    local entries = build_entries()

    debug(string.format("Creating diagnostics panel with %d entries", #entries))

    -- Create the virtual buffer in a horizontal split
    -- The ratio 0.7 means top pane (code) gets 70%, bottom pane (diagnostics) gets 30%
    -- panel_id makes this idempotent - if panel exists, just update its content
    editor.create_virtual_buffer_in_split({
        name = "*Diagnostics*",
        mode = "diagnostics-list",
        read_only = true,
        entries = entries,
        ratio = 0.7,
        panel_id = "diagnostics"  -- Unique ID for idempotent updates
    })

    panel_state.open = true
    editor.set_status(string.format("Diagnostics: %d items - use Up/Down to navigate, Enter to jump",
        #panel_state.diagnostics))
end

-- Toggle the diagnostics panel
function toggle_diagnostics_panel()
    debug("Toggling diagnostics panel")
    show_panel()
end

-- Jump to the diagnostic at the current selection
function goto_diagnostic()
    local diag = panel_state.diagnostics[panel_state.current_index]
    if not diag then
        editor.set_status("No diagnostic selected")
        return
    end

    debug(string.format("Jumping to %s:%d:%d", diag.file, diag.line, diag.column))

    -- Open the file at the diagnostic location
    editor.open_file_at_location(diag.file, diag.line, diag.column)
    editor.set_status(string.format("Jumped to %s:%d:%d - %s",
        diag.file, diag.line, diag.column, diag.message))
end

-- Move to next diagnostic
function diagnostics_next()
    if #panel_state.diagnostics == 0 then
        editor.set_status("No diagnostics")
        return
    end

    panel_state.current_index = panel_state.current_index + 1
    if panel_state.current_index > #panel_state.diagnostics then
        panel_state.current_index = 1  -- Wrap around
    end

    local diag = panel_state.diagnostics[panel_state.current_index]
    editor.set_status(string.format("[%d/%d] %s:%d - %s",
        panel_state.current_index, #panel_state.diagnostics,
        diag.file, diag.line, diag.message))

    -- Rebuild the panel to show the new selection
    local entries = build_entries()
    -- Note: We can't update content of existing buffer yet,
    -- so we recreate the panel (this is a limitation)
    debug(string.format("Selected diagnostic %d", panel_state.current_index))
end

-- Move to previous diagnostic
function diagnostics_prev()
    if #panel_state.diagnostics == 0 then
        editor.set_status("No diagnostics")
        return
    end

    panel_state.current_index = panel_state.current_index - 1
    if panel_state.current_index < 1 then
        panel_state.current_index = #panel_state.diagnostics  -- Wrap around
    end

    local diag = panel_state.diagnostics[panel_state.current_index]
    editor.set_status(string.format("[%d/%d] %s:%d - %s",
        panel_state.current_index, #panel_state.diagnostics,
        diag.file, diag.line, diag.message))

    -- Rebuild the panel to show the new selection
    local entries = build_entries()
    -- Note: We can't update content of existing buffer yet,
    -- so we recreate the panel (this is a limitation)
    debug(string.format("Selected diagnostic %d", panel_state.current_index))
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
    name = "Next Diagnostic",
    description = "Move to next diagnostic",
    action = "diagnostics_next",
    contexts = {"normal"}
})

editor.register_command({
    name = "Previous Diagnostic",
    description = "Move to previous diagnostic",
    action = "diagnostics_prev",
    contexts = {"normal"}
})

-- Initialize the mode on load
setup_mode()

debug("Diagnostics panel plugin loaded")
editor.set_status("Diagnostics panel plugin ready - use 'Show Diagnostics' command")
