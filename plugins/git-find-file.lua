-- Git Find File Plugin
-- Full implementation using hook-based prompt API
--
-- This plugin provides git file finding functionality, similar to
-- Ctrl+P in VSCode or fzf file search.

-- State management
local all_files = {}  -- Cache of all git-tracked files
local filtered_files = {}  -- Currently filtered file list

-- Simple fuzzy filter function
-- Returns true if all characters in pattern appear in order in str (case-insensitive)
local function fuzzy_match(str, pattern)
    if pattern == "" then
        return true
    end

    str = str:lower()
    pattern = pattern:lower()

    local str_idx = 1
    local pat_idx = 1

    while str_idx <= #str and pat_idx <= #pattern do
        if str:sub(str_idx, str_idx) == pattern:sub(pat_idx, pat_idx) then
            pat_idx = pat_idx + 1
        end
        str_idx = str_idx + 1
    end

    return pat_idx > #pattern
end

-- Filter files by query using fuzzy matching
local function filter_files(files, query)
    if query == "" or query:match("^%s*$") then
        -- Return all files for empty query (limited to first 100)
        local results = {}
        for i = 1, math.min(100, #files) do
            table.insert(results, files[i])
        end
        return results
    end

    local results = {}
    for _, file in ipairs(files) do
        if fuzzy_match(file, query) then
            table.insert(results, file)

            -- Limit to 100 results for performance
            if #results >= 100 then
                break
            end
        end
    end

    return results
end

-- Convert file list to suggestions format
local function files_to_suggestions(files)
    local suggestions = {}

    for _, file in ipairs(files) do
        table.insert(suggestions, {
            text = file,
            description = nil,
            value = file,
            disabled = false,
            keybinding = nil
        })
    end

    return suggestions
end

-- Load git-tracked files
local function load_git_files()
    editor.spawn("git", {"ls-files"},
        function(stdout, stderr, exit_code)
            if exit_code == 0 then
                -- Parse file list
                all_files = {}
                for line in stdout:gmatch("[^\r\n]+") do
                    if line ~= "" then
                        table.insert(all_files, line)
                    end
                end

                debug(string.format("Loaded %d git-tracked files", #all_files))
            else
                debug("Failed to load git files: " .. stderr)
                all_files = {}
            end
        end)
end

-- Register the Git Find File command
editor.register_command({
    name = "Git Find File",
    description = "Find and open a git-tracked file",
    action = "start_git_find_file",  -- Name of global Lua function
    contexts = {"normal"}
})

-- Global function to start git find file
function start_git_find_file()
    -- Load files if not already loaded
    if #all_files == 0 then
        load_git_files()
    end

    -- Clear filtered results
    filtered_files = {}

    -- Start the prompt
    editor.start_prompt({
        label = "Find file: ",
        prompt_type = "git-find-file"
    })

    -- Show initial file list (first 100)
    filtered_files = filter_files(all_files, "")
    local suggestions = files_to_suggestions(filtered_files)
    editor.set_prompt_suggestions(suggestions)

    if #all_files > 0 then
        editor.set_status(string.format("Showing %d of %d files", #filtered_files, #all_files))
    else
        editor.set_status("Loading git files...")
    end
end

-- React to prompt input changes
editor.on("prompt-changed", function(args)
    if args.prompt_type ~= "git-find-file" then
        return true  -- Not our prompt
    end

    local query = args.input

    -- Filter files by query
    filtered_files = filter_files(all_files, query)
    local suggestions = files_to_suggestions(filtered_files)

    -- Update prompt with filtered suggestions
    editor.set_prompt_suggestions(suggestions)

    -- Update status
    if #filtered_files > 0 then
        editor.set_status(string.format("Showing %d of %d files", #filtered_files, #all_files))
    else
        editor.set_status("No matches found")
    end

    return true
end)

-- Handle prompt confirmation (user pressed Enter)
editor.on("prompt-confirmed", function(args)
    if args.prompt_type ~= "git-find-file" then
        return true  -- Not our prompt
    end

    -- Check if user selected a file
    if args.selected_index and filtered_files[args.selected_index + 1] then
        -- Lua is 1-indexed, but selected_index comes from Rust as 0-indexed
        local selected = filtered_files[args.selected_index + 1]

        -- Open the file
        editor.open_file(selected)
        editor.set_status("Opened " .. selected)
    elseif args.input ~= "" then
        -- Try to open the input as a file path
        editor.open_file(args.input)
        editor.set_status("Opened " .. args.input)
    else
        editor.set_status("No file selected")
    end

    return true
end)

-- Handle prompt cancellation (user pressed Escape)
editor.on("prompt-cancelled", function(args)
    if args.prompt_type ~= "git-find-file" then
        return true  -- Not our prompt
    end

    -- Clear results
    filtered_files = {}
    editor.set_status("Find file cancelled")

    return true
end)

-- Load git files on plugin initialization
load_git_files()

-- Log that plugin loaded successfully
debug("Git Find File plugin loaded successfully")
debug("Usage: Call start_git_find_file() or use command palette 'Git Find File'")
