/// <reference path="../../types/fresh.d.ts" />

/**
 * Async Process Demo Plugin
 * Demonstrates spawning external processes asynchronously with async/await
 */

// Git status
globalThis.async_git_status = async function(): Promise<void> {
  editor.setStatus("Running git status...");

  try {
    const result = await editor.spawnProcess("git", ["status", "--short"]);
    if (result.exit_code === 0) {
      if (result.stdout === "" || result.stdout === "\n") {
        editor.setStatus("Git: Working tree clean");
      } else {
        const count = result.stdout.split("\n").filter(line => line.trim()).length;
        editor.setStatus(`Git: ${count} files changed`);
      }
    } else {
      editor.setStatus(`Git status failed: ${result.stderr}`);
    }
  } catch (e) {
    editor.setStatus(`Git status error: ${e}`);
  }
};

editor.registerCommand(
  "Async Demo: Git Status",
  "Run git status and show output",
  "async_git_status",
  "normal"
);

// Current directory
globalThis.async_pwd = async function(): Promise<void> {
  try {
    const result = await editor.spawnProcess("pwd");
    if (result.exit_code === 0) {
      const dir = result.stdout.trim();
      editor.setStatus(`Current directory: ${dir}`);
    } else {
      editor.setStatus("pwd failed");
    }
  } catch (e) {
    editor.setStatus(`pwd error: ${e}`);
  }
};

editor.registerCommand(
  "Async Demo: Current Directory",
  "Show current directory using pwd",
  "async_pwd",
  "normal"
);

// List files
globalThis.async_ls = async function(): Promise<void> {
  editor.setStatus("Listing files...");

  try {
    const result = await editor.spawnProcess("ls", ["-1"]);
    if (result.exit_code === 0) {
      const count = result.stdout.split("\n").filter(line => line.trim()).length;
      editor.setStatus(`Found ${count} files/directories`);
    } else {
      editor.setStatus("ls failed");
    }
  } catch (e) {
    editor.setStatus(`ls error: ${e}`);
  }
};

editor.registerCommand(
  "Async Demo: List Files",
  "List files in current directory",
  "async_ls",
  "normal"
);

// Git branch
globalThis.async_git_branch = async function(): Promise<void> {
  try {
    const result = await editor.spawnProcess("git", ["branch", "--show-current"]);
    if (result.exit_code === 0) {
      const branch = result.stdout.trim();
      if (branch !== "") {
        editor.setStatus(`Git branch: ${branch}`);
      } else {
        editor.setStatus("Not on any branch (detached HEAD)");
      }
    } else {
      editor.setStatus("Not a git repository");
    }
  } catch (e) {
    editor.setStatus(`Git branch error: ${e}`);
  }
};

editor.registerCommand(
  "Async Demo: Git Branch",
  "Show current git branch",
  "async_git_branch",
  "normal"
);

// Echo test
globalThis.async_echo = async function(): Promise<void> {
  try {
    const result = await editor.spawnProcess("echo", ["Hello from async process!"]);
    editor.setStatus(`Echo output: ${result.stdout.trim()}`);
  } catch (e) {
    editor.setStatus(`Echo error: ${e}`);
  }
};

editor.registerCommand(
  "Async Demo: Echo Test",
  "Test with simple echo command",
  "async_echo",
  "normal"
);

// With working directory
globalThis.async_with_cwd = async function(): Promise<void> {
  try {
    const result = await editor.spawnProcess("pwd", [], "/tmp");
    const dir = result.stdout.trim();
    editor.setStatus(`Working dir was: ${dir}`);
  } catch (e) {
    editor.setStatus(`pwd error: ${e}`);
  }
};

editor.registerCommand(
  "Async Demo: With Working Dir",
  "Run command in /tmp directory",
  "async_with_cwd",
  "normal"
);

// Error handling
globalThis.async_error = async function(): Promise<void> {
  try {
    const result = await editor.spawnProcess("this_command_does_not_exist");
    if (result.exit_code !== 0) {
      editor.setStatus(`Command failed (as expected): exit ${result.exit_code}`);
    } else {
      editor.setStatus("Unexpected success");
    }
  } catch (e) {
    editor.setStatus(`Command failed with error: ${e}`);
  }
};

editor.registerCommand(
  "Async Demo: Error Handling",
  "Demonstrate error handling with non-existent command",
  "async_error",
  "normal"
);

editor.setStatus("Async Demo plugin loaded! Try the 'Async Demo' commands.");
editor.debug("Async Demo plugin initialized with native async/await support");
