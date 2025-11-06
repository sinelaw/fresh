-- Buffer Query API Demo Plugin
-- Demonstrates the new buffer query functions in Phase 2

editor.register_command({
    name = "Query Demo: Show Buffer Info",
    description = "Display information about the current buffer",
    action = "show_buffer_info",
    contexts = {"normal"},
    callback = function()
        local buffer_id = editor.get_active_buffer_id()
        local info = editor.get_buffer_info(buffer_id)

        if info then
            local msg = string.format("Buffer %d: %s (%s, %d bytes)",
                info.id,
                info.path ~= "" and info.path or "[No Name]",
                info.modified and "modified" or "saved",
                info.length
            )
            editor.set_status(msg)
        else
            editor.set_status("No buffer info available")
        end
    end
})

editor.register_command({
    name = "Query Demo: Show Cursor Position",
    description = "Display cursor position and selection info",
    action = "show_cursor_info",
    contexts = {"normal"},
    callback = function()
        local cursor = editor.get_primary_cursor()

        if cursor then
            local msg
            if cursor.selection then
                msg = string.format("Cursor at %d, selection: %d-%d (%d chars)",
                    cursor.position,
                    cursor.selection.start,
                    cursor.selection["end"],
                    cursor.selection["end"] - cursor.selection.start
                )
            else
                msg = string.format("Cursor at byte position %d (no selection)", cursor.position)
            end
            editor.set_status(msg)
        else
            editor.set_status("No cursor info available")
        end
    end
})

editor.register_command({
    name = "Query Demo: Count All Cursors",
    description = "Display the number of active cursors",
    action = "count_cursors",
    contexts = {"normal"},
    callback = function()
        local cursors = editor.get_all_cursors()
        editor.set_status(string.format("Active cursors: %d", #cursors))
    end
})

editor.register_command({
    name = "Query Demo: Show Line Count",
    description = "Display the number of lines in the current buffer",
    action = "show_line_count",
    contexts = {"normal"},
    callback = function()
        local buffer_id = editor.get_active_buffer_id()
        local content = editor.get_buffer_content(buffer_id)

        if content then
            local lines = 1
            for _ in content:gmatch("\n") do
                lines = lines + 1
            end
            editor.set_status(string.format("Buffer has %d lines", lines))
        else
            editor.set_status("No buffer content available")
        end
    end
})

editor.register_command({
    name = "Query Demo: Show Current Line",
    description = "Display the content of line 1",
    action = "show_first_line",
    contexts = {"normal"},
    callback = function()
        local buffer_id = editor.get_active_buffer_id()
        local line = editor.get_line(buffer_id, 1)

        if line then
            editor.set_status(string.format("First line: %s", line))
        else
            editor.set_status("Could not get first line")
        end
    end
})

editor.register_command({
    name = "Query Demo: List All Buffers",
    description = "Show count of open buffers",
    action = "list_all_buffers",
    contexts = {"normal"},
    callback = function()
        local buffers = editor.list_buffers()
        local modified_count = 0

        for _, buf in ipairs(buffers) do
            if buf.modified then
                modified_count = modified_count + 1
            end
        end

        editor.set_status(string.format("Open buffers: %d (%d modified)", #buffers, modified_count))
    end
})

editor.register_command({
    name = "Query Demo: Show Viewport Info",
    description = "Display viewport dimensions and scroll position",
    action = "show_viewport",
    contexts = {"normal"},
    callback = function()
        local vp = editor.get_viewport()

        if vp then
            local msg = string.format("Viewport: %dx%d, top_byte=%d, left_col=%d",
                vp.width, vp.height, vp.top_byte, vp.left_column
            )
            editor.set_status(msg)
        else
            editor.set_status("No viewport info available")
        end
    end
})

editor.set_status("Buffer Query Demo plugin loaded! Try the 'Query Demo' commands.")
