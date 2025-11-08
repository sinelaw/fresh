-- TODO Highlighter Plugin (Optimized for huge files with render-line hook)
-- Highlights TODO, FIXME, HACK, NOTE, XXX, and BUG comments
--
-- RENDER-LINE HOOK + MARKER-BASED OVERLAYS:
-- - Only scans visible lines during rendering (efficient for huge files)
-- - Marker-based overlays automatically adjust positions when text changes
-- - Uses stable overlay IDs so we don't recreate overlays unnecessarily

-- Plugin state
local highlighting_enabled = false

-- Keyword patterns and their colors
local keywords = {
    {pattern = "TODO", color = {r = 255, g = 165, b = 0, a = 50}},   -- Orange
    {pattern = "FIXME", color = {r = 255, g = 0, b = 0, a = 50}},    -- Red
    {pattern = "HACK", color = {r = 255, g = 255, b = 0, a = 50}},   -- Yellow
    {pattern = "NOTE", color = {r = 0, g = 255, b = 0, a = 50}},     -- Green
    {pattern = "XXX", color = {r = 255, g = 0, b = 255, a = 50}},    -- Magenta
    {pattern = "BUG", color = {r = 128, g = 0, b = 0, a = 50}},      -- Dark Red
}

-- Comment patterns
local comment_patterns = {
    "//", "#", "--", "/*", "*", "<!--"
}

-- Render-line hook: creates/updates overlays for visible keywords
-- Uses stable IDs - add_overlay will update existing overlay if ID already exists
local function on_render_line(args)
    if not highlighting_enabled then
        return true
    end

    local line = args.content
    local line_start_byte = args.byte_start
    local buffer_id = args.buffer_id
    local line_number = args.line_number

    -- Check each keyword
    for _, keyword_info in ipairs(keywords) do
        local pattern = keyword_info.pattern
        local occurrence = 0

        -- Find all occurrences in this line
        local search_start = 1
        while true do
            local start_pos, end_pos = line:find(pattern, search_start, true)
            if not start_pos then
                break
            end

            -- Check if in comment
            local is_in_comment = false
            local line_before = line:sub(1, start_pos - 1)
            for _, comment_marker in ipairs(comment_patterns) do
                if line_before:find(comment_marker, 1, true) then
                    is_in_comment = true
                    break
                end
            end

            if is_in_comment then
                occurrence = occurrence + 1
                local highlight_start = line_start_byte + start_pos - 1
                local highlight_end = line_start_byte + end_pos

                -- Stable ID based on line and pattern (not byte position!)
                -- This way the overlay persists even when byte positions change
                local overlay_id = string.format("todo_%s_L%d_O%d", pattern, line_number, occurrence)

                -- add_overlay updates existing overlay if ID exists, creates new otherwise
                editor.add_overlay(
                    buffer_id,
                    overlay_id,
                    highlight_start,
                    highlight_end,
                    keyword_info.color.r,
                    keyword_info.color.g,
                    keyword_info.color.b,
                    false
                )
            end

            search_start = end_pos + 1
        end
    end

    return true
end

editor.register_command({
    name = "TODO Highlighter: Enable",
    description = "Enable TODO/FIXME/etc highlighting",
    action = "todo_highlight_enable",
    contexts = {"normal"},
    callback = function()
        highlighting_enabled = true
        editor.set_status("TODO Highlighter: Found keywords in visible lines")
    end
})

editor.register_command({
    name = "TODO Highlighter: Disable",
    description = "Disable TODO highlighting",
    action = "todo_highlight_disable",
    contexts = {"normal"},
    callback = function()
        highlighting_enabled = false
        editor.set_status("TODO Highlighter: Disabled")
    end
})

editor.register_command({
    name = "TODO Highlighter: Toggle",
    description = "Toggle TODO highlighting",
    action = "todo_highlight_toggle",
    contexts = {"normal"},
    callback = function()
        highlighting_enabled = not highlighting_enabled
        editor.set_status(highlighting_enabled and "TODO Highlighter: Found keywords in visible lines" or "TODO Highlighter: Disabled")
    end
})

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
        editor.set_status("TODO Highlighter keywords: " .. table.concat(keyword_list, ", "))
    end
})

editor.on("render-line", on_render_line)

debug("TODO Highlighter: Plugin loaded (optimized with render-line hook)")
editor.set_status("TODO Highlighter plugin loaded! Use 'TODO Highlighter: Toggle' to start.")
