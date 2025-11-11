-- TODO Highlighter Plugin - Incremental Update Implementation
-- Highlights keywords like TODO, FIXME, HACK, NOTE, XXX, and BUG in comments
--
-- PERFORMANCE OPTIMIZATIONS:
-- 1. Scans each line once, not every frame
-- 2. Incremental updates: Only re-scans affected lines when text changes
-- 3. Uses position info from insert/delete hooks for localized updates
-- 4. Bulk overlay removal only for affected line range
-- 5. Scales to GB+ files via efficient render-line hook

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

-- Track which buffers have been fully scanned
-- Key: buffer_id, Value: true if fully scanned
M.scanned_buffers = {}

-- Track which lines are "dirty" and need re-scanning
-- Key: buffer_id, Value: { [line_num] = true, ... }
M.dirty_lines = {}

-- Track which lines we've already scanned in this session
-- Key: buffer_id, Value: { [line_num] = true, ... }
M.scanned_lines = {}

-- Prefix for all overlay IDs created by this plugin
M.OVERLAY_PREFIX = "todo_hl_"

-- Average bytes per line (used for position->line estimation)
M.AVG_BYTES_PER_LINE = 80

-- Initialize the plugin
function M.init()
    debug("TODO Highlighter: Initializing plugin (incremental mode)")

    -- Register render-line hook for scanning
    editor.on("render-line", function(args)
        if not M.config.enabled then
            return true
        end

        local buffer_id = args.buffer_id
        local line_number = args.line_number
        local byte_start = args.byte_start
        local content = args.content

        -- Check if this line needs (re-)scanning
        local needs_scan = false

        if not M.scanned_buffers[buffer_id] then
            -- First time scanning this buffer
            needs_scan = true
        elseif M.dirty_lines[buffer_id] and M.dirty_lines[buffer_id][line_number] then
            -- This specific line was marked dirty
            needs_scan = true
        elseif not M.scanned_lines[buffer_id] or not M.scanned_lines[buffer_id][line_number] then
            -- We haven't seen this line yet (new line or scrolled into view)
            needs_scan = true
        end

        if needs_scan then
            -- Remove old overlays for this line only (much faster!)
            M.clear_line_overlays(buffer_id, line_number)

            -- Scan and add new overlays
            M.scan_line_for_keywords(buffer_id, line_number, byte_start, content)

            -- Mark this line as scanned
            if not M.scanned_lines[buffer_id] then
                M.scanned_lines[buffer_id] = {}
            end
            M.scanned_lines[buffer_id][line_number] = true

            -- Clear dirty flag
            if M.dirty_lines[buffer_id] then
                M.dirty_lines[buffer_id][line_number] = nil
            end
        end

        return true
    end)

    -- Detect end of first full scan
    local last_line_number = 0
    local consecutive_decreases = 0
    editor.on("render-line", function(args)
        if not M.config.enabled then
            return true
        end

        -- If line number decreased, we might have completed a frame
        if args.line_number < last_line_number then
            consecutive_decreases = consecutive_decreases + 1
            -- After seeing a decrease, mark buffer as fully scanned
            if consecutive_decreases >= 1 and not M.scanned_buffers[args.buffer_id] then
                M.scanned_buffers[args.buffer_id] = true
                debug(string.format("Buffer %d initial scan complete", args.buffer_id))
            end
        else
            consecutive_decreases = 0
        end

        last_line_number = args.line_number
        return true
    end)

    -- Register hooks to detect buffer changes - INCREMENTAL UPDATES
    editor.on("after-insert", function(args)
        if not M.config.enabled or not args.buffer_id then
            return true
        end

        local buffer_id = args.buffer_id
        local position = args.position
        local text = args.text or ""

        -- Estimate which lines are affected by this insert
        local affected_lines = M.estimate_affected_lines_insert(position, text)

        debug(string.format("Insert at pos %d, marking ~%d lines dirty",
            position, #affected_lines))

        -- Mark only affected lines as dirty
        M.mark_lines_dirty(buffer_id, affected_lines)

        return true
    end)

    editor.on("after-delete", function(args)
        if not M.config.enabled or not args.buffer_id then
            return true
        end

        local buffer_id = args.buffer_id
        local range_start = args.start or 0
        local range_end = args["end"] or range_start
        local deleted_text = args.deleted_text or ""

        -- Estimate which lines are affected by this delete
        local affected_lines = M.estimate_affected_lines_delete(range_start, range_end, deleted_text)

        debug(string.format("Delete range %d-%d, marking ~%d lines dirty",
            range_start, range_end, #affected_lines))

        -- Mark only affected lines as dirty
        M.mark_lines_dirty(buffer_id, affected_lines)

        return true
    end)

    -- Register commands
    M.register_commands()

    debug("TODO Highlighter: Plugin initialized")
end

-- Estimate which lines are affected by an insert operation
function M.estimate_affected_lines_insert(position, text)
    local affected = {}

    -- Calculate approximate line number where insert happened
    local line_num = math.floor(position / M.AVG_BYTES_PER_LINE)

    -- Count newlines in inserted text
    local newline_count = 0
    for _ in text:gmatch("\n") do
        newline_count = newline_count + 1
    end

    -- Mark current line + any new lines created + buffer zone
    -- Buffer zone accounts for line number shifts
    local buffer_zone = 2
    for i = line_num - buffer_zone, line_num + newline_count + buffer_zone do
        if i >= 0 then
            table.insert(affected, i)
        end
    end

    return affected
end

-- Estimate which lines are affected by a delete operation
function M.estimate_affected_lines_delete(range_start, range_end, deleted_text)
    local affected = {}

    -- Calculate approximate line numbers for the deleted range
    local start_line = math.floor(range_start / M.AVG_BYTES_PER_LINE)
    local end_line = math.floor(range_end / M.AVG_BYTES_PER_LINE)

    -- Count newlines in deleted text
    local newline_count = 0
    for _ in deleted_text:gmatch("\n") do
        newline_count = newline_count + 1
    end

    -- Mark affected range + buffer zone
    local buffer_zone = 2
    for i = start_line - buffer_zone, end_line + newline_count + buffer_zone do
        if i >= 0 then
            table.insert(affected, i)
        end
    end

    return affected
end

-- Mark specific lines as dirty (need re-scan)
function M.mark_lines_dirty(buffer_id, line_numbers)
    if not M.dirty_lines[buffer_id] then
        M.dirty_lines[buffer_id] = {}
    end

    for _, line_num in ipairs(line_numbers) do
        M.dirty_lines[buffer_id][line_num] = true
    end
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

    editor.register_command({
        name = "TODO Highlighter: Rescan",
        description = "Force full re-scan of active buffer",
        action = "todo_highlight_rescan",
        contexts = {"normal"},
        callback = function()
            M.rescan_active_buffer()
        end
    })
end

-- Enable highlighting
function M.enable()
    M.config.enabled = true
    M.scanned_buffers = {}
    M.dirty_lines = {}
    M.scanned_lines = {}
    editor.set_status("TODO Highlighter: Enabled (incremental mode)")
    debug("TODO Highlighter: Enabled")
end

-- Disable highlighting
function M.disable()
    M.config.enabled = false
    M.scanned_buffers = {}
    M.dirty_lines = {}
    M.scanned_lines = {}

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
        M.scanned_buffers[buffer_id] = nil
        M.dirty_lines[buffer_id] = nil
        M.scanned_lines[buffer_id] = nil
        editor.set_status("TODO Highlighter: Cleared highlights from buffer")
        debug(string.format("TODO Highlighter: Cleared overlays from buffer %d", buffer_id))
    end
end

-- Force full re-scan of active buffer
function M.rescan_active_buffer()
    local buffer_id = editor.get_active_buffer_id()
    if buffer_id then
        M.scanned_buffers[buffer_id] = nil
        M.dirty_lines[buffer_id] = nil
        M.scanned_lines[buffer_id] = nil
        editor.set_status("TODO Highlighter: Buffer marked for full re-scan")
        debug(string.format("TODO Highlighter: Buffer %d marked for full re-scan", buffer_id))
    end
end

-- Initialize the plugin
M.init()

-- Set initial status message
editor.set_status("TODO Highlighter plugin loaded! Use 'TODO Highlighter: Toggle' to enable.")

-- Return module for testing/debugging
return M
