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
    buffer_id = nil,    -- Buffer ID for updating content
    source_split_id = nil,  -- Split ID where the code buffer is (for opening files)
}

-- Initialize the diagnostics-list mode
local function setup_mode()
    editor.define_mode({
        name = "diagnostics-list",
        parent = "special",  -- Inherits 'q' to quit, 'g' to refresh
        bindings = {
            ["RET"] = "goto_diagnostic",       -- Jump to diagnostic location
            ["n"] = "diagnostics_next",        -- Move to next diagnostic
            ["p"] = "diagnostics_prev",        -- Move to previous diagnostic
            ["Down"] = "diagnostics_next",     -- Arrow key support
            ["Up"] = "diagnostics_prev",       -- Arrow key support
            ["j"] = "diagnostics_next",        -- Vim-style
            ["k"] = "diagnostics_prev",        -- Vim-style
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
    -- Remember the current split ID (where the code buffer is)
    -- This is where we'll open files when the user selects a diagnostic
    panel_state.source_split_id = editor.get_active_split_id()
    debug(string.format("Storing source split ID: %d", panel_state.source_split_id))

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
        panel_id = "diagnostics",  -- Unique ID for idempotent updates
        show_line_numbers = false,  -- Hide line numbers in diagnostics panel
        show_cursors = false        -- Hide cursor in diagnostics panel
    })

    -- Track the buffer ID (the panel is focused after creation)
    panel_state.buffer_id = editor.get_active_buffer_id()
    panel_state.open = true
    debug(string.format("Diagnostics panel buffer_id: %d", panel_state.buffer_id))

    editor.set_status(string.format("Diagnostics: %d items - use Up/Down to navigate, Enter to jump",
        #panel_state.diagnostics))
end

-- Toggle the diagnostics panel
function toggle_diagnostics_panel()
    debug("Toggling diagnostics panel")

    -- If panel is already open and we're currently in it, do nothing
    if panel_state.open and panel_state.buffer_id then
        local current_buffer_id = editor.get_active_buffer_id()
        if current_buffer_id == panel_state.buffer_id then
            debug("Already in diagnostics panel, doing nothing")
            editor.set_status("Diagnostics panel already focused")
            return
        end
    end

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

    -- Open the file at the diagnostic location in the source split (where code buffer is)
    if panel_state.source_split_id then
        editor.open_file_in_split(panel_state.source_split_id, diag.file, diag.line, diag.column)
    else
        -- Fallback to opening in current split if source_split_id not set
        editor.open_file_at_location(diag.file, diag.line, diag.column)
    end
    editor.set_status(string.format("Jumped to %s:%d:%d - %s",
        diag.file, diag.line, diag.column, diag.message))
end

-- Helper to update the panel display with current selection
local function update_panel_display()
    local entries = build_entries()
    -- Recreate the panel with updated entries
    -- panel_id makes this idempotent - content is updated without creating new split
    editor.create_virtual_buffer_in_split({
        name = "*Diagnostics*",
        mode = "diagnostics-list",
        read_only = true,
        entries = entries,
        ratio = 0.7,
        panel_id = "diagnostics",
        show_line_numbers = false,  -- Keep line numbers hidden
        show_cursors = false        -- Keep cursor hidden
    })
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

    -- Update the panel to show the new selection
    update_panel_display()
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

    -- Update the panel to show the new selection
    update_panel_display()
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
