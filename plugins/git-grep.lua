-- Git Grep Plugin
-- Full implementation using hook-based prompt API
--
-- This plugin provides git grep functionality as a plugin, demonstrating
-- how the hook-based prompt API can replace hardcoded git operations.

-- State management
local git_grep_results = {}  -- Store parsed results for later use

-- Parse git grep output line
-- Format: file:line:column:content
local function parse_git_grep_line(line)
    local file, line_num, column_num, content = line:match("^([^:]+):(%d+):(%d+):(.*)$")

    if file and line_num and column_num then
        return {
            file = file,
            line = tonumber(line_num),
            column = tonumber(column_num),
            content = content:gsub("^%s+", "")  -- Trim leading whitespace
        }
    end

    return nil
end

-- Parse git grep output into suggestions
local function parse_git_grep_output(stdout)
    local results = {}
    local suggestions = {}

    for line in stdout:gmatch("[^\r\n]+") do
        local match = parse_git_grep_line(line)
        if match then
            table.insert(results, match)

            -- Create suggestion for display
            table.insert(suggestions, {
                text = string.format("%s:%d:%d: %s",
                    match.file, match.line, match.column, match.content),
                description = match.file,
                value = string.format("%s:%d:%d", match.file, match.line, match.column),
                disabled = false,
                keybinding = nil
            })
        end
    end

    return results, suggestions
end

-- Register the Git Grep command
editor.register_command({
    name = "Git Grep",
    description = "Search for text in git-tracked files",
    action = "start_git_grep",  -- Name of global Lua function
    contexts = {"normal"}
})

-- Global function to start git grep
function start_git_grep()
    -- Clear previous results
    git_grep_results = {}

    -- Start the prompt
    editor.start_prompt({
        label = "Git grep: ",
        prompt_type = "git-grep"
    })
end

-- React to prompt input changes
editor.on("prompt-changed", function(args)
    if args.prompt_type ~= "git-grep" then
        return true  -- Not our prompt
    end

    local query = args.input

    -- Don't search for empty queries
    if query == "" or query:match("^%s*$") then
        editor.set_prompt_suggestions({})
        return true
    end

    -- Spawn git grep asynchronously
    editor.spawn("git", {"grep", "-n", "--column", "-I", "--", query},
        function(stdout, stderr, exit_code)
            if exit_code == 0 then
                -- Parse results and update suggestions
                local results, suggestions = parse_git_grep_output(stdout)
                git_grep_results = results

                -- Update prompt with suggestions
                editor.set_prompt_suggestions(suggestions)

                -- Update status
                if #results > 0 then
                    editor.set_status(string.format("Found %d matches", #results))
                else
                    editor.set_status("No matches found")
                end
            elseif exit_code == 1 then
                -- No matches found (git grep returns 1)
                git_grep_results = {}
                editor.set_prompt_suggestions({})
                editor.set_status("No matches found")
            else
                -- Error occurred
                editor.set_status("Git grep error: " .. stderr)
            end
        end)

    return true
end)

-- Handle prompt confirmation (user pressed Enter)
editor.on("prompt-confirmed", function(args)
    if args.prompt_type ~= "git-grep" then
        return true  -- Not our prompt
    end

    -- Check if user selected a suggestion
    if args.selected_index and git_grep_results[args.selected_index + 1] then
        -- Lua is 1-indexed, but selected_index comes from Rust as 0-indexed
        local selected = git_grep_results[args.selected_index + 1]

        -- Open the file at the specific location
        editor.open_file({
            path = selected.file,
            line = selected.line,
            column = selected.column
        })

        editor.set_status(string.format("Opened %s:%d:%d",
            selected.file, selected.line, selected.column))
    else
        -- No selection, maybe user just pressed Enter on empty input
        editor.set_status("No file selected")
    end

    return true
end)

-- Handle prompt cancellation (user pressed Escape)
editor.on("prompt-cancelled", function(args)
    if args.prompt_type ~= "git-grep" then
        return true  -- Not our prompt
    end

    -- Clear results
    git_grep_results = {}
    editor.set_status("Git grep cancelled")

    return true
end)

-- Log that plugin loaded successfully
debug("Git Grep plugin loaded successfully")
debug("Usage: Call start_git_grep() or use command palette 'Git Grep'")
