-- TODO Highlighter Plugin - Simple Invalidation Strategy
-- Highlights keywords like TODO, FIXME, HACK, NOTE, XXX, and BUG in comments
--
-- DESIGN: Simple and robust approach
-- 1. On render-line: Scan the line and add overlays
-- 2. On after-insert/delete: Clear ALL overlays (invalidate everything)
-- 3. Natural re-scanning: Only visible lines get scanned during rendering
-- 4. Scrolling handled automatically: New lines trigger render-line hook
--
-- This is similar to VS Code's approach: invalidate on edit, re-scan on render.
-- It's fast (only ~24 visible lines) and correct (no stale overlays).

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

-- Track which lines we've already scanned (to avoid re-scanning same line in one frame)
-- Key: buffer_id, Value: { [line_number] = true, ... }
M.scanned_this_frame = {}

-- Prefix for all overlay IDs created by this plugin
M.OVERLAY_PREFIX = "todo_hl_"

-- Frame counter to detect new frames
M.frame_counter = 0

-- Initialize the plugin
function M.init()
    debug("TODO Highlighter: Initializing plugin (simple invalidation mode)")

    -- Register render-line hook for scanning
    editor.on("render-line", function(args)
        if not M.config.enabled then
            return true
        end

        local buffer_id = args.buffer_id
        local line_number = args.line_number
        local byte_start = args.byte_start
        local content = args.content

        -- Detect frame reset (when line numbers go backwards, we started a new frame)
        if not M.scanned_this_frame[buffer_id] then
            M.scanned_this_frame[buffer_id] = {
                last_line = -1,
                lines = {}
            }
        end

        local frame_data = M.scanned_this_frame[buffer_id]
        if line_number < frame_data.last_line then
            -- New frame started, clear the set
            frame_data.lines = {}
        end
        frame_data.last_line = line_number

        -- Check if we've already scanned this line in this frame
        if frame_data.lines[line_number] then
            return true
        end

        -- Mark as scanned for this frame
        frame_data.lines[line_number] = true

        -- Clear old overlays for this line only
        M.clear_line_overlays(buffer_id, line_number)

        -- Scan and add new overlays
        M.scan_line_for_keywords(buffer_id, line_number, byte_start, content)

        return true
    end)

    -- Register hooks to detect buffer changes - SIMPLE INVALIDATION
    editor.on("after-insert", function(args)
        if not M.config.enabled or not args.buffer_id then
            return true
        end

        local buffer_id = args.buffer_id

        debug(string.format("Insert in buffer %d, invalidating all overlays", buffer_id))

        -- Simple approach: Just clear everything
        M.clear_buffer_overlays(buffer_id)

        -- Reset frame tracking for this buffer
        M.scanned_this_frame[buffer_id] = nil

        return true
    end)

    editor.on("after-delete", function(args)
        if not M.config.enabled or not args.buffer_id then
            return true
        end

        local buffer_id = args.buffer_id

        debug(string.format("Delete in buffer %d, invalidating all overlays", buffer_id))

        -- Simple approach: Just clear everything
        M.clear_buffer_overlays(buffer_id)

        -- Reset frame tracking for this buffer
        M.scanned_this_frame[buffer_id] = nil

        return true
    end)

    -- Register commands
    M.register_commands()

    debug("TODO Highlighter: Plugin initialized")
end

-- Clear overlays for a specific line only
function M.clear_line_overlays(buffer_id, line_number)
    -- Remove overlays that match this line number pattern
    -- Our overlay IDs are formatted as: "todo_hl_L{line}_..."
    local prefix = string.format("%sL%d_", M.OVERLAY_PREFIX, line_number)
    editor.remove_overlays_by_prefix(buffer_id, prefix)
end

-- Clear all overlays for entire buffer
function M.clear_buffer_overlays(buffer_id)
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

        -- Create stable overlay ID using line number
        local overlay_id = string.format("%sL%d_%s_O%d",
            M.OVERLAY_PREFIX,
            line_number,
            pattern,
            occurrence
        )

        -- Add overlay
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
        name = "TODO Highlighter: Refresh",
        description = "Clear and refresh all TODO highlights",
        action = "todo_highlight_refresh",
        contexts = {"normal"},
        callback = function()
            M.refresh_active_buffer()
        end
    })
end

-- Enable highlighting
function M.enable()
    M.config.enabled = true
    M.scanned_this_frame = {}
    editor.set_status("TODO Highlighter: Enabled (simple invalidation mode)")
    debug("TODO Highlighter: Enabled")
end

-- Disable highlighting
function M.disable()
    M.config.enabled = false
    M.scanned_this_frame = {}

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
        M.scanned_this_frame[buffer_id] = nil
        editor.set_status("TODO Highlighter: Cleared highlights from buffer")
        debug(string.format("TODO Highlighter: Cleared overlays from buffer %d", buffer_id))
    end
end

-- Force refresh of active buffer
function M.refresh_active_buffer()
    local buffer_id = editor.get_active_buffer_id()
    if buffer_id then
        M.clear_buffer_overlays(buffer_id)
        M.scanned_this_frame[buffer_id] = nil
        editor.set_status("TODO Highlighter: Buffer marked for refresh")
        debug(string.format("TODO Highlighter: Buffer %d marked for refresh", buffer_id))
    end
end

-- Initialize the plugin
M.init()

-- Set initial status message
editor.set_status("TODO Highlighter plugin loaded! Use 'TODO Highlighter: Toggle' to enable.")

-- Return module for testing/debugging
return M
