/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Assembly LSP Helper Plugin
 *
 * Server: asm-lsp (github.com/bergercookie/asm-lsp)
 * Covers GAS/NASM/MASM across x86, x86_64, ARM and RISC-V.
 * Fresh routes both the "asm" (Intel/NASM) and "gas" (AT&T/GAS)
 * languages to it.
 * Install via: cargo (Rust package manager)
 *
 * Besides install help, this plugin offers to create a project
 * `.asm-lsp.toml` when an assembly file is opened and no config exists:
 * asm-lsp ignores the LSP languageId and defaults to GAS/x86-64, so
 * without a config NASM/MASM/ARM/RISC-V files get bogus diagnostics.
 * The dialect is guessed from the buffer's language and contents, and
 * the file is only written after the user picks an option.
 */

interface LspServerErrorData {
  language: string;
  server_command: string;
  error_type: string;
  message: string;
}

interface LspStatusClickedData {
  language: string;
  has_error: boolean;
}

interface ActionPopupResultData {
  popup_id: string;
  action_id: string;
}

const ASM_LANGUAGES = ["asm", "gas"];

const INSTALL_COMMANDS = {
  cargo: "cargo install asm-lsp",
  binstall: "cargo binstall asm-lsp",
};

let asmLspError: {
  language: string;
  serverCommand: string;
  message: string;
} | null = null;


editor.on("lsp_server_error", (data) => {
  if (!ASM_LANGUAGES.includes(data.language)) {
    return;
  }

  editor.debug(`asm-lsp: Server error - ${data.error_type}: ${data.message}`);

  asmLspError = {
    language: data.language,
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Assembly LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Assembly LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (!ASM_LANGUAGES.includes(data.language) || !asmLspError) {
    return;
  }

  editor.debug("asm-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "asm-lsp-help",
    title: "Assembly Language Server Not Found",
    message: `"${asmLspError.serverCommand}" provides completion, diagnostics, hover docs for opcodes/registers/directives, and go-to-definition for assembly (GAS, NASM, MASM). Requires Rust's cargo to install.\n\nOptional per-project config: .asm-lsp.toml (choose assembler and instruction set).\nSee: https://github.com/bergercookie/asm-lsp`,
    actions: [
      { id: "copy_cargo", label: `Copy: ${INSTALL_COMMANDS.cargo}` },
      { id: "copy_binstall", label: `Copy: ${INSTALL_COMMANDS.binstall}` },
      { id: "disable", label: "Disable Assembly LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id === "asm-lsp-config-offer") {
    handleConfigOfferResult(data.action_id);
    return;
  }
  if (data.popup_id !== "asm-lsp-help") {
    return;
  }

  editor.debug(`asm-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_cargo":
      editor.setClipboard(INSTALL_COMMANDS.cargo);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.cargo);
      break;

    case "copy_binstall":
      editor.setClipboard(INSTALL_COMMANDS.binstall);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.binstall);
      break;

    case "disable":
      for (const language of ASM_LANGUAGES) {
        editor.disableLspForLanguage(language);
      }
      editor.setStatus("Assembly LSP disabled");
      asmLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`asm-lsp: Unknown action: ${data.action_id}`);
  }
});

// ── .asm-lsp.toml config offer ──────────────────────────────────────────────

const ASSEMBLERS = ["gas", "nasm", "masm"];

let configOfferedThisSession = false;
let pendingOffer: { language: string; arch: string } | null = null;

function projectConfigPath(): string {
  return editor.pathJoin(editor.getCwd(), ".asm-lsp.toml");
}

/// asm-lsp falls back to a user-global config before its defaults; if one
/// exists the user has already made a choice, so don't second-guess it.
/// Mirrors asm-lsp's own lookup: `dirs::config_dir()` (XDG_CONFIG_HOME if
/// set, else ~/.config) plus the macOS ~/Library/Application Support dir.
function globalConfigExists(): boolean {
  const candidates: string[] = [];
  const xdg = editor.getEnv("XDG_CONFIG_HOME");
  const home = editor.getEnv("HOME");
  if (xdg) {
    candidates.push(editor.pathJoin(xdg, "asm-lsp", ".asm-lsp.toml"));
  } else if (home) {
    candidates.push(editor.pathJoin(home, ".config", "asm-lsp", ".asm-lsp.toml"));
  }
  if (home) {
    candidates.push(
      editor.pathJoin(home, "Library", "Application Support", "asm-lsp", ".asm-lsp.toml")
    );
  }
  return candidates.some((path) => editor.fileExists(editor.authorityPath(path)));
}

/// Guess the assembler dialect from the language key and buffer contents.
/// `gas` covers .s/.S; for .asm the MASM directives are distinctive enough
/// to tell it apart from NASM, which is the default for Intel syntax.
function guessAssembler(language: string, text: string): string {
  if (language === "gas") {
    return "gas";
  }
  if (/\b(proc|endp|assume|invoke)\b|\.(model|code|data|stack)\b/i.test(text)) {
    return "masm";
  }
  return "nasm";
}

/// Guess the instruction set from buffer contents. Defaults to x86/x86-64,
/// which is also asm-lsp's own default.
function guessArch(text: string): string {
  if (/\b(adrp|stp|ldp|xzr|wzr|x29|x30|w[0-9]+,)\b/i.test(text)) {
    return "arm64";
  }
  if (/\b(auipc|jalr|ecall|ebreak|addi|sltiu)\b/i.test(text)) {
    return "riscv";
  }
  return "x86/x86-64";
}

function configToml(assembler: string, arch: string): string {
  const lines = [
    "# Generated by Fresh (asm-lsp helper plugin).",
    "# Full reference: https://github.com/bergercookie/asm-lsp",
    "[default_config]",
    `assembler = "${assembler}"`,
    `instruction_set = "${arch}"`,
  ];
  if (assembler !== "gas") {
    lines.push(
      "",
      "[default_config.opts]",
      "# asm-lsp's built-in diagnostics shell out to gcc/clang, which only",
      `# assemble GAS syntax — useless for ${assembler}. Point \`compiler\` at an`,
      "# assembler that understands this dialect to re-enable diagnostics.",
      "default_diagnostics = false"
    );
  }
  lines.push("");
  return lines.join("\n");
}

function handleConfigOfferResult(actionId: string) {
  editor.debug(`asm-lsp: Config offer action - ${actionId}`);
  const offer = pendingOffer;

  if (actionId.startsWith("create:")) {
    const assembler = actionId.slice("create:".length);
    const arch = offer ? offer.arch : "x86/x86-64";
    const path = projectConfigPath();
    if (!editor.writeFile(editor.authorityPath(path), configToml(assembler, arch))) {
      editor.setStatus(`Failed to write ${path}`);
      return;
    }
    editor.setStatus(`Created .asm-lsp.toml (${assembler}, ${arch}) — restarting Assembly LSP`);
    if (offer) {
      editor.restartLspForLanguage(offer.language);
    }
    pendingOffer = null;
    return;
  }

  if (actionId === "never") {
    editor.setGlobalState(`config-offer-never:${editor.getCwd()}`, true);
    editor.setStatus("Won't offer to create .asm-lsp.toml for this project again");
  }
  pendingOffer = null;
}

/// Map a file path to the assembly language key, mirroring the default
/// `languages` config. Decided from the path (like csharp_support does)
/// rather than `getBufferInfo().language`: the plugin state snapshot is
/// updated asynchronously, so right after `after_file_open` fires the
/// buffer's language may not be visible to plugins yet.
function asmLanguageForPath(path: string): string | null {
  const lower = path.toLowerCase();
  if (lower.endsWith(".asm") || lower.endsWith(".nasm")) {
    return "asm";
  }
  if (lower.endsWith(".s")) {
    return "gas";
  }
  return null;
}

async function maybeOfferConfig(bufferId: number, path: string) {
  if (configOfferedThisSession) {
    return;
  }
  const language = asmLanguageForPath(path);
  if (!language) {
    return;
  }
  if (editor.fileExists(editor.authorityPath(projectConfigPath())) || globalConfigExists()) {
    return;
  }
  if (editor.getGlobalState(`config-offer-never:${editor.getCwd()}`) === true) {
    return;
  }
  configOfferedThisSession = true;

  // The content sniff needs the buffer in the plugin snapshot; give the
  // async snapshot a moment to catch up, and fall back to an extension-only
  // guess if it doesn't.
  let sample = "";
  for (let attempt = 0; attempt < 20; attempt++) {
    const info = editor.getBufferInfo(bufferId);
    if (info) {
      sample = await editor.getBufferText(bufferId, 0, Math.min(info.length, 2048));
      break;
    }
    await editor.delay(50);
  }
  const assembler = guessAssembler(language, sample);
  const arch = guessArch(sample);
  pendingOffer = { language, arch };

  const alternates = ASSEMBLERS.filter((a) => a !== assembler);
  editor.showActionPopup({
    id: "asm-lsp-config-offer",
    // Scope the offer to the buffer that triggered it: this is a reaction to
    // opening one assembly file, not a global notification, so it should only
    // show while that buffer is active (and vanish when it closes) rather than
    // floating over every other buffer.
    buffer_id: bufferId,
    title: "Assembly LSP: no .asm-lsp.toml found",
    message:
      `Without a config, asm-lsp assumes GAS syntax on x86/x86-64 — other dialects get wrong diagnostics and docs.\n\n` +
      `Detected for this file: ${assembler} / ${arch}.\nCreate ${projectConfigPath()}?`,
    actions: [
      { id: `create:${assembler}`, label: `Create with detected: ${assembler} / ${arch}` },
      ...alternates.map((a) => ({ id: `create:${a}`, label: `Create with: ${a} / ${arch}` })),
      { id: "dismiss", label: "Not now (ESC)" },
      { id: "never", label: "Don't ask again for this project" },
    ],
  });
}

editor.on("after_file_open", async (data) => {
  await maybeOfferConfig(data.buffer_id, data.path);
});

// Files opened before this plugin finished loading (CLI arguments, session
// restore) never fire `after_file_open` for us — sweep the buffers that are
// already open once the editor is up.
editor.on("plugins_loaded", async () => {
  for (const buffer of editor.listBuffers()) {
    if (buffer.path) {
      await maybeOfferConfig(buffer.id, buffer.path);
    }
    if (configOfferedThisSession) {
      break;
    }
  }
});

editor.debug("asm-lsp: Plugin loaded");
