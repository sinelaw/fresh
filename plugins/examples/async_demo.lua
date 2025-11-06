-- Async Process Demo Plugin
-- Demonstrates spawning external processes asynchronously

editor.register_command({
    name = "Async Demo: Git Status",
    description = "Run git status and show output",
    action = "async_git_status",
    contexts = {"normal"},
    callback = function()
        editor.set_status("Running git status...")

        editor.spawn("git", {"status", "--short"}, function(stdout, stderr, exit_code)
            if exit_code == 0 then
                if stdout == "" or stdout == "\n" then
                    editor.set_status("Git: Working tree clean")
                else
                    -- Count lines
                    local count = 0
                    for _ in stdout:gmatch("\n") do count = count + 1 end
                    editor.set_status(string.format("Git: %d files changed", count))
                end
            else
                editor.set_status("Git status failed: " .. stderr)
            end
        end)
    end
})

editor.register_command({
    name = "Async Demo: Current Directory",
    description = "Show current directory using pwd",
    action = "async_pwd",
    contexts = {"normal"},
    callback = function()
        editor.spawn("pwd", {}, function(stdout, stderr, exit_code)
            if exit_code == 0 then
                local dir = stdout:gsub("\n", "")
                editor.set_status("Current directory: " .. dir)
            else
                editor.set_status("pwd failed")
            end
        end)
    end
})

editor.register_command({
    name = "Async Demo: List Files",
    description = "List files in current directory",
    action = "async_ls",
    contexts = {"normal"},
    callback = function()
        editor.set_status("Listing files...")

        editor.spawn("ls", {"-1"}, function(stdout, stderr, exit_code)
            if exit_code == 0 then
                local count = 0
                for _ in stdout:gmatch("\n") do count = count + 1 end
                editor.set_status(string.format("Found %d files/directories", count))
            else
                editor.set_status("ls failed")
            end
        end)
    end
})

editor.register_command({
    name = "Async Demo: Git Branch",
    description = "Show current git branch",
    action = "async_git_branch",
    contexts = {"normal"},
    callback = function()
        editor.spawn("git", {"branch", "--show-current"}, function(stdout, stderr, exit_code)
            if exit_code == 0 then
                local branch = stdout:gsub("\n", "")
                if branch ~= "" then
                    editor.set_status("Git branch: " .. branch)
                else
                    editor.set_status("Not on any branch (detached HEAD)")
                end
            else
                editor.set_status("Not a git repository")
            end
        end)
    end
})

editor.register_command({
    name = "Async Demo: Echo Test",
    description = "Test with simple echo command",
    action = "async_echo",
    contexts = {"normal"},
    callback = function()
        editor.spawn("echo", {"Hello from async process!"}, function(stdout, stderr, exit_code)
            editor.set_status("Echo output: " .. stdout:gsub("\n", ""))
        end)
    end
})

editor.register_command({
    name = "Async Demo: With Working Dir",
    description = "Run command in /tmp directory",
    action = "async_with_cwd",
    contexts = {"normal"},
    callback = function()
        editor.spawn("pwd", {}, {cwd = "/tmp"}, function(stdout, stderr, exit_code)
            local dir = stdout:gsub("\n", "")
            editor.set_status("Working dir was: " .. dir)
        end)
    end
})

editor.register_command({
    name = "Async Demo: Error Handling",
    description = "Demonstrate error handling with non-existent command",
    action = "async_error",
    contexts = {"normal"},
    callback = function()
        editor.spawn("this_command_does_not_exist", {}, function(stdout, stderr, exit_code)
            if exit_code ~= 0 then
                editor.set_status("Command failed (as expected): " .. stderr)
            else
                editor.set_status("Unexpected success")
            end
        end)
    end
})

editor.set_status("Async Demo plugin loaded! Try the 'Async Demo' commands.")
