-- TODO Highlighter Plugin - Optimized with Marker-Based Overlays
-- Highlights keywords like TODO, FIXME, HACK, NOTE, XXX, and BUG in comments
--
-- DESIGN: Leverages marker-based overlay system for automatic position tracking
-- 1. On render-line: Hash line content, only re-scan if changed
-- 2. On after-insert/delete: Only invalidate if text contains keywords/newlines/comments
-- 3. Markers automatically adjust to text movement - no manual repositioning needed!
--
-- KEY INSIGHT: Overlays use markers that automatically adjust when text is inserted/deleted
-- before them. Combined with content hashing, we get huge performance wins:
--
-- - Simple typing (no keywords): Markers auto-adjust, zero plugin work! ✨
-- - Scrolling: Content hash matches, no re-scanning needed! ✨
-- - Typing keywords/newlines: Full invalidation, hashes cleared, lines re-scanned
--
-- This means:
-- - Zero overhead when scrolling (markers keep overlays positioned correctly)
-- - Zero overhead when typing regular code (markers auto-adjust positions)
-- - Only pays cost when actual TODO keywords appear/disappear in edits

local M = {}

-- Configuration
M.config = {
    enabled = false,

    -- Keywords to highlight with their colors (r, g, b)
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
-- Key: buffer_id, Value: { last_line = N, lines = {[line_number] = true} }
M.scanned_this_frame = {}

-- Track line content hashes to detect actual changes
-- Key: buffer_id, Value: { [line_number] = content_hash }
M.line_content_hashes = {}

-- Prefix for all overlay IDs created by this plugin
M.OVERLAY_PREFIX = "todo_hl_"

-- Simple hash function for strings (djb2 algorithm)
local function hash_string(str)
    local hash = 5381
    for i = 1, #str do
        hash = ((hash * 33) + string.byte(str, i)) % 2147483647
    end
    return hash
end

-- Initialize the plugin
function M.init()
    debug("TODO Highlighter: Initializing (marker-optimized mode)")

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

        -- Check if line content has changed
        if not M.line_content_hashes[buffer_id] then
            M.line_content_hashes[buffer_id] = {}
        end

        local content_hash = hash_string(content)
        local previous_hash = M.line_content_hashes[buffer_id][line_number]

        -- Only re-scan if content actually changed
        if content_hash ~= previous_hash then
            -- Clear existing overlays for this line
            M.clear_line_overlays(buffer_id, line_number)

            -- Scan and add new overlays
            M.scan_line_for_keywords(buffer_id, line_number, byte_start, content)

            -- Update hash
            M.line_content_hashes[buffer_id][line_number] = content_hash
        end

        return true
    end)

    -- Register hooks to detect buffer changes
    -- SMART INVALIDATION: Only invalidate lines that might be affected
    editor.on("after-insert", function(args)
        if not M.config.enabled or not args.buffer_id then
            return true
        end

        local buffer_id = args.buffer_id
        local position = args.position
        local text = args.text

        -- Strategy: Only invalidate if the insertion might create/destroy TODO highlights
        -- Markers will auto-adjust for position changes!

        local needs_rescan = false

        -- Check if inserted text contains TODO keywords
        for _, keyword_info in ipairs(M.config.keywords) do
            if text:find(keyword_info.pattern, 1, true) then
                needs_rescan = true
                debug(string.format("Insert contains keyword '%s', invalidating buffer %d", keyword_info.pattern, buffer_id))
                break
            end
        end

        -- Check if inserted text contains comment markers (might create new comment lines)
        if not needs_rescan then
            for _, pattern in ipairs(M.config.comment_patterns) do
                if text:find(pattern, 1, true) then
                    needs_rescan = true
                    debug(string.format("Insert contains comment marker, invalidating buffer %d", buffer_id))
                    break
                end
            end
        end

        -- Note: We deliberately DON'T invalidate on plain newlines!
        -- When you press Enter on an empty line or split a line without adding keywords,
        -- the markers will automatically adjust to the new positions. The content hash
        -- will detect the change and re-scan only the affected lines on next render.

        if needs_rescan then
            -- Clear everything and let render-line re-scan
            M.clear_buffer_overlays(buffer_id)
            M.scanned_this_frame[buffer_id] = nil
            M.line_content_hashes[buffer_id] = nil
            debug(string.format("Insert triggered rescan of buffer %d", buffer_id))
        else
            -- Markers auto-adjust! No action needed.
            debug(string.format("Simple insert in buffer %d, markers auto-adjust", buffer_id))
        end

        return true
    end)

    editor.on("after-delete", function(args)
        if not M.config.enabled or not args.buffer_id then
            return true
        end

        local buffer_id = args.buffer_id
        local range = args.range
        local deleted_text = args.deleted_text or ""

        -- Strategy: Only invalidate if deletion might destroy TODO highlights

        local needs_rescan = false

        -- Check if deleted text contained TODO keywords
        for _, keyword_info in ipairs(M.config.keywords) do
            if deleted_text:find(keyword_info.pattern, 1, true) then
                needs_rescan = true
                debug(string.format("Delete contains keyword '%s', invalidating buffer %d", keyword_info.pattern, buffer_id))
                break
            end
        end

        -- Check if deleted text contained comment markers
        if not needs_rescan then
            for _, pattern in ipairs(M.config.comment_patterns) do
                if deleted_text:find(pattern, 1, true) then
                    needs_rescan = true
                    debug(string.format("Delete contains comment marker, invalidating buffer %d", buffer_id))
                    break
                end
            end
        end

        -- Note: We deliberately DON'T invalidate on plain newlines!
        -- When you delete lines without removing keywords, markers auto-adjust.
        -- Content hash will detect changes and re-scan affected lines on next render.

        if needs_rescan then
            -- Clear everything and let render-line re-scan
            M.clear_buffer_overlays(buffer_id)
            M.scanned_this_frame[buffer_id] = nil
            M.line_content_hashes[buffer_id] = nil
            debug(string.format("Delete triggered rescan of buffer %d", buffer_id))
        else
            -- Markers auto-adjust! No action needed.
            debug(string.format("Simple delete in buffer %d, markers auto-adjust", buffer_id))
        end

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
        -- The line number in the ID is for organizational purposes only
        -- The actual position is tracked by markers automatically!
        local overlay_id = string.format("%sL%d_%s_O%d",
            M.OVERLAY_PREFIX,
            line_number,
            pattern,
            occurrence
        )

        -- Add overlay with markers
        -- The editor will create markers at these positions
        -- These markers will automatically adjust when text is inserted/deleted before them!
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
    -- Enable command
    function todo_highlight_enable()
        M.enable()
    end

    editor.register_command({
        name = "TODO Highlighter: Enable",
        description = "Enable TODO/FIXME/etc highlighting in comments",
        action = "todo_highlight_enable",
        contexts = {"normal"}
    })

    -- Disable command
    function todo_highlight_disable()
        M.disable()
    end

    editor.register_command({
        name = "TODO Highlighter: Disable",
        description = "Disable TODO highlighting",
        action = "todo_highlight_disable",
        contexts = {"normal"}
    })

    -- Toggle command
    function todo_highlight_toggle()
        M.toggle()
    end

    editor.register_command({
        name = "TODO Highlighter: Toggle",
        description = "Toggle TODO highlighting on/off",
        action = "todo_highlight_toggle",
        contexts = {"normal"}
    })

    -- Show keywords command
    function todo_highlight_keywords()
        M.show_keywords()
    end

    editor.register_command({
        name = "TODO Highlighter: Show Keywords",
        description = "Display list of highlighted keywords",
        action = "todo_highlight_keywords",
        contexts = {"normal"}
    })

    -- Refresh command
    function todo_highlight_refresh()
        M.refresh_active_buffer()
    end

    editor.register_command({
        name = "TODO Highlighter: Refresh",
        description = "Clear and refresh all TODO highlights",
        action = "todo_highlight_refresh",
        contexts = {"normal"}
    })
end

-- Enable highlighting
function M.enable()
    M.config.enabled = true
    M.scanned_this_frame = {}
    M.line_content_hashes = {}
    editor.set_status("TODO Highlighter: Enabled (marker-optimized)")
    debug("TODO Highlighter: Enabled")
end

-- Disable highlighting
function M.disable()
    M.config.enabled = false
    M.scanned_this_frame = {}
    M.line_content_hashes = {}

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
        M.line_content_hashes[buffer_id] = nil
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
        M.line_content_hashes[buffer_id] = nil
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
