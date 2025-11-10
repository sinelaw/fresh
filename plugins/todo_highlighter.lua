-- TODO Highlighter Plugin - Robust Implementation
-- Highlights keywords like TODO, FIXME, HACK, NOTE, XXX, and BUG in comments
--
-- This plugin uses the render-line hook for efficient highlighting that scales
-- to huge files. It only scans visible lines and uses the new bulk overlay
-- removal APIs for proper lifecycle management.

local M = {}

-- Configuration
M.config = {
    enabled = false,

    -- Keywords to highlight with their colors (r, g, b, alpha)
    keywords = {
        {pattern = "TODO",  color = {255, 165, 0},   name = "TODO"},   -- Orange
        {pattern = "FIXME", color = {255, 50, 50},   name = "FIXME"},  -- Red
        {pattern = "HACK",  color = {255, 215, 0},   name = "HACK"},   -- Yellow
        {pattern = "NOTE",  color = {80, 200, 120},  name = "NOTE"},   -- Green
        {pattern = "XXX",   color = {255, 100, 255}, name = "XXX"},    -- Magenta
        {pattern = "BUG",   color = {180, 50, 50},   name = "BUG"},    -- Dark Red
    },

    -- Comment patterns to check
    comment_patterns = {
        "//",    -- C++, Rust, JS, etc.
        "#",     -- Python, Ruby, Shell, etc.
        "--",    -- Lua, SQL, etc.
        "/%*",   -- Block comment start
        "%*",    -- Block comment continuation
        "<!%-%-" -- HTML comments
    }
}

-- Track which buffers have been processed this frame
-- We'll clear overlays once per buffer per frame
M.processed_buffers = {}

-- Prefix for all overlay IDs created by this plugin
M.OVERLAY_PREFIX = "todo_hl_"

-- Initialize the plugin
function M.init()
    debug("TODO Highlighter: Initializing plugin")

    -- Register render-line hook
    editor.on("render-line", function(args)
        return M.on_render_line(args)
    end)

    -- Register commands
    M.register_commands()

    debug("TODO Highlighter: Plugin initialized")
end

-- Handle render-line hook
function M.on_render_line(args)
    if not M.config.enabled then
        return true
    end

    local buffer_id = args.buffer_id
    local line_number = args.line_number
    local byte_start = args.byte_start
    local content = args.content

    -- On first line of each buffer per frame, clear old overlays
    if line_number == 1 and not M.processed_buffers[buffer_id] then
        M.clear_buffer_overlays(buffer_id)
        M.processed_buffers[buffer_id] = true
    end

    -- Scan line for keywords
    M.scan_line_for_keywords(buffer_id, line_number, byte_start, content)

    return true
end

-- Clear all overlays for this buffer
function M.clear_buffer_overlays(buffer_id)
    -- Use the new bulk removal API - much more efficient!
    editor.remove_overlays_by_prefix(buffer_id, M.OVERLAY_PREFIX)
end

-- Scan a line for keywords and add overlays
function M.scan_line_for_keywords(buffer_id, line_number, byte_start, content)
    -- Check if this looks like a comment line
    if not M.is_comment_line(content) then
        return
    end

    -- Search for each keyword
    for _, keyword_info in ipairs(M.config.keywords) do
        M.find_and_highlight_keyword(
            buffer_id,
            line_number,
            byte_start,
            content,
            keyword_info
        )
    end
end

-- Check if a line appears to be a comment
function M.is_comment_line(line)
    -- Trim leading whitespace
    local trimmed = line:match("^%s*(.*)$")
    if not trimmed or trimmed == "" then
        return false
    end

    -- Check if line starts with any comment pattern
    for _, pattern in ipairs(M.config.comment_patterns) do
        -- Escape special chars for plain find
        local plain_pattern = pattern:gsub("([%^%$%(%)%%%.%[%]%*%+%-%?])", "%%%1")
        if trimmed:find("^" .. pattern) then
            return true
        end
    end

    return false
end

-- Find all occurrences of a keyword in a line and add overlays
function M.find_and_highlight_keyword(buffer_id, line_number, byte_start, content, keyword_info)
    local pattern = keyword_info.pattern
    local color = keyword_info.color
    local occurrence = 0

    -- Search for all occurrences in the line
    local search_pos = 1
    while true do
        local start_pos, end_pos = content:find(pattern, search_pos, true)
        if not start_pos then
            break
        end

        occurrence = occurrence + 1

        -- Calculate byte positions
        local highlight_start = byte_start + start_pos - 1
        local highlight_end = byte_start + end_pos

        -- Create stable overlay ID
        -- Using line number (not byte position) for stability across edits
        local overlay_id = string.format("%sL%d_%s_O%d",
            M.OVERLAY_PREFIX,
            line_number,
            pattern,
            occurrence
        )

        -- Add overlay (will update existing if ID matches)
        local success, err = pcall(function()
            editor.add_overlay(
                buffer_id,
                overlay_id,
                highlight_start,
                highlight_end,
                color[1], color[2], color[3],
                false -- no underline, use background highlight
            )
        end)

        if not success then
            debug(string.format("TODO Highlighter: Error adding overlay: %s", tostring(err)))
        end

        -- Move search forward
        search_pos = end_pos + 1
    end
end

-- Register plugin commands
function M.register_commands()
    editor.register_command({
        name = "TODO Highlighter: Enable",
        description = "Enable TODO/FIXME/etc highlighting in comments",
        action = "todo_highlight_enable",
        contexts = {"normal"},
        callback = function()
            M.enable()
        end
    })

    editor.register_command({
        name = "TODO Highlighter: Disable",
        description = "Disable TODO highlighting",
        action = "todo_highlight_disable",
        contexts = {"normal"},
        callback = function()
            M.disable()
        end
    })

    editor.register_command({
        name = "TODO Highlighter: Toggle",
        description = "Toggle TODO highlighting on/off",
        action = "todo_highlight_toggle",
        contexts = {"normal"},
        callback = function()
            M.toggle()
        end
    })

    editor.register_command({
        name = "TODO Highlighter: Show Keywords",
        description = "Display list of highlighted keywords",
        action = "todo_highlight_keywords",
        contexts = {"normal"},
        callback = function()
            M.show_keywords()
        end
    })

    editor.register_command({
        name = "TODO Highlighter: Clear All",
        description = "Clear all TODO highlights from active buffer",
        action = "todo_highlight_clear",
        contexts = {"normal"},
        callback = function()
            M.clear_active_buffer()
        end
    })
end

-- Enable highlighting
function M.enable()
    M.config.enabled = true
    M.processed_buffers = {} -- Reset tracking
    editor.set_status("TODO Highlighter: Enabled")
    debug("TODO Highlighter: Enabled")
end

-- Disable highlighting
function M.disable()
    M.config.enabled = false
    M.processed_buffers = {} -- Reset tracking

    -- Clear all highlights from active buffer
    M.clear_active_buffer()

    editor.set_status("TODO Highlighter: Disabled")
    debug("TODO Highlighter: Disabled")
end

-- Toggle highlighting
function M.toggle()
    if M.config.enabled then
        M.disable()
    else
        M.enable()
    end
end

-- Show configured keywords
function M.show_keywords()
    local keyword_list = {}
    for _, kw in ipairs(M.config.keywords) do
        table.insert(keyword_list, kw.name)
    end
    local message = "TODO Highlighter keywords: " .. table.concat(keyword_list, ", ")
    editor.set_status(message)
    debug(message)
end

-- Clear all highlights from the active buffer
function M.clear_active_buffer()
    local buffer_id = editor.get_active_buffer_id()
    if buffer_id then
        M.clear_buffer_overlays(buffer_id)
        editor.set_status("TODO Highlighter: Cleared highlights from buffer")
        debug(string.format("TODO Highlighter: Cleared overlays from buffer %d", buffer_id))
    end
end

-- Initialize the plugin
M.init()

-- Set initial status message
editor.set_status("TODO Highlighter plugin loaded! Use 'TODO Highlighter: Toggle' to enable.")

-- Return module for testing/debugging
return M
