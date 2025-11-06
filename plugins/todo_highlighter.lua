-- TODO Highlighter Plugin
-- Highlights TODO, FIXME, HACK, NOTE, XXX, and BUG comments in the current buffer
-- Demonstrates buffer query, pattern matching, and overlay APIs

-- Plugin state
local highlighting_enabled = false
local current_overlays = {}

-- Keyword patterns and their colors
local keywords = {
    {pattern = "TODO", color = {r = 255, g = 165, b = 0, a = 50}},   -- Orange
    {pattern = "FIXME", color = {r = 255, g = 0, b = 0, a = 50}},    -- Red
    {pattern = "HACK", color = {r = 255, g = 255, b = 0, a = 50}},   -- Yellow
    {pattern = "NOTE", color = {r = 0, g = 255, b = 0, a = 50}},     -- Green
    {pattern = "XXX", color = {r = 255, g = 0, b = 255, a = 50}},    -- Magenta
    {pattern = "BUG", color = {r = 128, g = 0, b = 0, a = 50}},      -- Dark Red
}

-- Comment patterns for different languages
local comment_patterns = {
    "//",      -- C, C++, Rust, JavaScript, etc.
    "#",       -- Python, Ruby, Shell, etc.
    "--",      -- Lua, Haskell, SQL, etc.
    "/*",      -- C-style block comments
    "*",       -- Inside block comments
    "<!--",    -- HTML, XML
}

-- Clear all existing overlays
local function clear_overlays()
    for _, overlay_id in ipairs(current_overlays) do
        editor.remove_overlay(overlay_id)
    end
    current_overlays = {}
end

-- Find and highlight all keywords in the current buffer
local function highlight_keywords()
    -- Clear existing overlays first
    clear_overlays()

    if not highlighting_enabled then
        return
    end

    -- Get current buffer content
    local buffer_id = editor.get_active_buffer_id()
    local content = editor.get_buffer_content(buffer_id)

    if not content then
        editor.set_status("Cannot access buffer content")
        return
    end

    local highlights_found = 0
    local byte_offset = 0

    -- Process content line by line
    for line in content:gmatch("[^\n]*\n?") do
        local line_without_newline = line:gsub("\n$", "")

        -- Check each keyword
        for _, keyword_info in ipairs(keywords) do
            local pattern = keyword_info.pattern

            -- Find all occurrences of this keyword in the line
            local search_start = 1
            while true do
                local start_pos, end_pos = line_without_newline:find(pattern, search_start, true)
                if not start_pos then
                    break
                end

                -- Check if this keyword is in a comment
                -- Look for comment markers before the keyword on this line
                local line_before_keyword = line_without_newline:sub(1, start_pos - 1)
                local is_in_comment = false
                for _, comment_marker in ipairs(comment_patterns) do
                    if line_before_keyword:find(comment_marker, 1, true) then
                        is_in_comment = true
                        break
                    end
                end

                -- If it's in a comment, highlight it
                if is_in_comment then
                    -- Calculate byte position (UTF-8 safe: we're working with ASCII patterns)
                    local highlight_start = byte_offset + start_pos - 1
                    local highlight_end = byte_offset + end_pos

                    -- Add overlay
                    local overlay_id = editor.add_overlay(
                        buffer_id,
                        highlight_start,
                        highlight_end,
                        keyword_info.color
                    )

                    table.insert(current_overlays, overlay_id)
                    highlights_found = highlights_found + 1
                end

                -- Move to next potential occurrence
                search_start = end_pos + 1
            end
        end

        -- Move byte offset forward by line length (including newline)
        byte_offset = byte_offset + #line
    end

    if highlights_found > 0 then
        editor.set_status(string.format("TODO Highlighter: Found %d keywords", highlights_found))
    else
        editor.set_status("TODO Highlighter: No keywords found")
    end
end

-- Command: Enable highlighting
editor.register_command({
    name = "TODO Highlighter: Enable",
    description = "Enable TODO/FIXME/etc highlighting in current buffer",
    action = "todo_highlight_enable",
    contexts = {"normal"},
    callback = function()
        highlighting_enabled = true
        highlight_keywords()
    end
})

-- Command: Disable highlighting
editor.register_command({
    name = "TODO Highlighter: Disable",
    description = "Disable TODO highlighting and clear overlays",
    action = "todo_highlight_disable",
    contexts = {"normal"},
    callback = function()
    highlighting_enabled = false
        clear_overlays()
        editor.set_status("TODO Highlighter: Disabled")
    end
})

-- Command: Toggle highlighting
editor.register_command({
    name = "TODO Highlighter: Toggle",
    description = "Toggle TODO highlighting on/off",
    action = "todo_highlight_toggle",
    contexts = {"normal"},
    callback = function()
        highlighting_enabled = not highlighting_enabled
        if highlighting_enabled then
            highlight_keywords()
        else
            clear_overlays()
            editor.set_status("TODO Highlighter: Disabled")
        end
    end
})

-- Command: Refresh highlighting
editor.register_command({
    name = "TODO Highlighter: Refresh",
    description = "Re-scan and refresh TODO highlights",
    action = "todo_highlight_refresh",
    contexts = {"normal"},
    callback = function()
        if highlighting_enabled then
            highlight_keywords()
        else
            editor.set_status("TODO Highlighter: Not enabled (use Toggle to enable)")
        end
    end
})

-- Command: Show keyword list
editor.register_command({
    name = "TODO Highlighter: Show Keywords",
    description = "Display list of highlighted keywords",
    action = "todo_highlight_keywords",
    contexts = {"normal"},
    callback = function()
        local keyword_list = {}
        for _, kw in ipairs(keywords) do
            table.insert(keyword_list, kw.pattern)
        end
        local keywords_str = table.concat(keyword_list, ", ")
        editor.set_status("TODO Highlighter keywords: " .. keywords_str)
    end
})

editor.set_status("TODO Highlighter plugin loaded! Use 'TODO Highlighter: Toggle' to start.")
