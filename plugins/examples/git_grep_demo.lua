-- Git Grep Demo Plugin
-- Demonstrates using editor.spawn() and editor.open_file() for git operations
--
-- This is a prototype showing how git grep could be implemented as a plugin.
-- A full implementation would need the prompt API (planned for future) to provide
-- interactive search with suggestions.
--
-- Usage: This plugin registers a "Git Grep (Demo)" command that searches for "TODO"
-- and opens the first match.

-- Helper function to parse git grep output
-- Format: file:line:column:content
local function parse_git_grep_line(line)
    -- Match pattern: filename:line:column:content
    local file, line_num, column_num, content = line:match("^([^:]+):(%d+):(%d+):(.*)$")

    if file and line_num and column_num then
        return {
            file = file,
            line = tonumber(line_num),
            column = tonumber(column_num),
            content = content
        }
    end

    return nil
end

-- Register a command that demonstrates git grep functionality
editor.register_command({
    name = "Git Grep (Demo)",
    description = "Demo: Search for 'TODO' in git-tracked files and jump to first match",
    action = "git-grep-demo",
    contexts = {"normal"}
})

-- Register a plugin action to handle the command
-- Note: In the current system, we need to bind this to a keybinding or execute via command palette
-- For now, this demonstrates the concept

editor.set_status("Git Grep Demo plugin loaded. Use command palette (Ctrl+P) to run 'Git Grep (Demo)'")

-- Alternative: Create a function that can be called directly for testing
function git_grep_search(query)
    editor.set_status("Searching for: " .. query)

    -- Spawn git grep asynchronously
    editor.spawn("git", {"grep", "-n", "--column", "-I", "--", query}, function(stdout, stderr, exit_code)
        if exit_code ~= 0 then
            if stdout == "" and stderr == "" then
                editor.set_status("No matches found for: " .. query)
            else
                editor.set_status("Git grep failed: " .. stderr)
            end
            return
        end

        -- Parse results
        local results = {}
        for line in stdout:gmatch("[^\r\n]+") do
            local match = parse_git_grep_line(line)
            if match then
                table.insert(results, match)
            end
        end

        if #results == 0 then
            editor.set_status("No matches found for: " .. query)
            return
        end

        -- For demo purposes, just open the first match
        local first = results[1]
        editor.set_status(string.format("Found %d matches. Opening: %s:%d:%d",
            #results, first.file, first.line, first.column))

        -- Use the new open_file API to jump to the match
        editor.open_file({
            path = first.file,
            line = first.line,
            column = first.column
        })

        -- In a full implementation, we would:
        -- 1. Show all results in a prompt/selection UI
        -- 2. Allow user to navigate through results
        -- 3. Update results as user types (incremental search)
        -- This requires the prompt API which is planned for future work
    end)
end

-- Example: To test this plugin, you can add to your init.lua:
-- git_grep_search("TODO")
--
-- Or create a keybinding that calls this function

debug("Git grep demo plugin loaded successfully")
