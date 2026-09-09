/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * LSP Install Help
 *
 * One plugin covering every language server whose only job was to explain
 * itself when it fails to start. Each entry below used to be its own
 * `<language>-lsp.ts` file repeating the same three handlers:
 *
 * - `lsp_server_error`  — remember the failure, put it on the status bar
 * - `lsp_status_clicked` — offer install commands in an action popup
 * - `action_popup_result` — copy a command, or disable the language's LSP
 *
 * Servers that do more than hand out install instructions (rust, asm,
 * slang) keep their own files.
 *
 * Popup ids stay `<id>-lsp-help` so repeated indicator clicks still
 * de-duplicate against the popup already on screen.
 */

/** A row that copies text to the clipboard when chosen. */
interface CopyAction {
  /** Action id reported back in `action_popup_result`. */
  id: string;
  /** Text placed on the clipboard. */
  value: string;
  /** Row label. Defaults to `Copy: <value>`. */
  label?: string;
}

/** Everything that differs between one language server and the next. */
interface ServerHelp {
  /** Stem of the popup id (`<id>-lsp-help`) and of debug lines. */
  id: string;
  /** Human-readable server name, used to build titles and status lines. */
  name: string;
  /** Languages whose LSP events this entry answers. */
  languages: string[];
  /** Languages the "Disable" row turns off. Defaults to `languages`. */
  disables?: string[];
  /** Popup body. `{server}` expands to the command that failed. */
  message: string;
  /** Copy rows, shown above "Disable" and "Dismiss". */
  copies?: CopyAction[];
  /** Popup title. Defaults to `<name> Language Server Not Found`. */
  title?: string;
  /** Status line for `not_found`. `{server}` expands as in `message`. */
  notFoundStatus?: string;
  /** Status line for any other failure. `{message}` expands to the reason. */
  errorStatus?: string;
  /** Status line after disabling. Defaults to `<name> LSP disabled`. */
  disabledStatus?: string;
}

const SERVERS: ServerHelp[] = [
  {
    id: "astro",
    name: "Astro",
    languages: ["astro"],
    message: `"{server}" provides completion, diagnostics, and formatting for Astro components. Powered by the Volar framework.\n\nRequires TypeScript to be installed in your project for full functionality.\nVS Code users: Install the official "Astro" extension.\nSee: https://github.com/withastro/language-tools`,
    copies: [
      { id: "copy_npm", value: "npm install -g @astrojs/language-server" },
      { id: "copy_pnpm", value: "pnpm add -g @astrojs/language-server" },
    ],
  },
  {
    id: "bash",
    name: "Bash",
    languages: ["bash"],
    message: `"{server}" provides code completion, diagnostics, and navigation for shell scripts. Requires Node.js. Copy a command below to install it, or visit https://github.com/bash-lsp/bash-language-server for details. For linting, also consider ShellCheck (https://www.shellcheck.net/).`,
    copies: [
      { id: "copy_npm", value: "npm i -g bash-language-server" },
      { id: "copy_yarn", value: "yarn global add bash-language-server" },
      { id: "copy_pnpm", value: "pnpm add -g bash-language-server" },
    ],
  },
  {
    id: "clangd",
    name: "C/C++",
    languages: ["c", "cpp"],
    message: `"{server}" provides code completion, diagnostics, and navigation for C/C++ files. Copy a command below to install it for your platform.`,
    copies: [
      { id: "copy_apt", value: "sudo apt install clangd" },
      { id: "copy_brew", value: "brew install llvm" },
      { id: "copy_pacman", value: "sudo pacman -S clang" },
    ],
  },
  {
    id: "clojure",
    name: "Clojure",
    languages: ["clojure"],
    message: `"{server}" provides completion, diagnostics, refactoring, and navigation for Clojure/ClojureScript.\n\nNo special project setup needed - it analyzes classpath automatically.\nVS Code users: Install "Calva" (bundles clojure-lsp and nREPL client).\nSee: https://clojure-lsp.io`,
    copies: [
      { id: "copy_brew", value: "brew install clojure-lsp/brew/clojure-lsp-native" },
      { id: "copy_nix", value: "nix-shell -p clojure-lsp" },
      {
        id: "copy_script",
        value:
          "sudo bash < <(curl -s https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)",
        label: "Copy: install script (Linux/macOS)",
      },
    ],
  },
  {
    id: "cmake",
    name: "CMake",
    languages: ["cmake"],
    message: `"{server}" provides code completion, diagnostics, and navigation for CMakeLists.txt files. Requires Python. Copy a command below to install it, or visit https://github.com/regen100/cmake-language-server for details. Alternative: neocmakelsp (https://github.com/Decodetalkers/neocmakelsp).`,
    copies: [
      { id: "copy_pip", value: "pip install cmake-language-server" },
      { id: "copy_pipx", value: "pipx install cmake-language-server" },
    ],
  },
  {
    id: "css",
    name: "CSS",
    languages: ["css"],
    message: `"{server}" provides code completion, diagnostics, and formatting for CSS files. Copy the command below to install it.`,
    copies: [{ id: "copy_npm", value: "npm install -g vscode-langservers-extracted" }],
  },
  {
    id: "dart",
    name: "Dart",
    languages: ["dart"],
    message: `The Dart language server is included with the Dart SDK. Install the Dart SDK (or Flutter SDK) to get LSP support. Visit https://dart.dev/get-dart for platform-specific instructions.`,
    copies: [
      { id: "copy_brew", value: "brew install dart" },
      { id: "copy_apt", value: "sudo apt install dart" },
      { id: "copy_choco", value: "choco install dart-sdk", label: "Copy: choco install dart-sdk (Windows)" },
    ],
  },
  {
    id: "elixir",
    name: "Elixir",
    languages: ["elixir"],
    message: `"{server}" provides completion, diagnostics, go-to-definition, Dialyzer integration, and debugging for Elixir. Requires Elixir and Erlang/OTP.\n\nNew: Expert (https://expert-lsp.org) is the upcoming official Elixir LSP, merging ElixirLS, Lexical, and Next LS.\nVS Code users: Install the "ElixirLS" extension.\nSee: https://github.com/elixir-lsp/elixir-ls`,
    copies: [
      { id: "copy_brew", value: "brew install elixir-ls" },
      { id: "copy_nix", value: "nix-env -iA nixpkgs.elixir-ls" },
    ],
  },
  {
    id: "erlang",
    name: "Erlang",
    languages: ["erlang"],
    message: `"{server}" provides completion, diagnostics, navigation, and code actions for Erlang. Requires Erlang/OTP 24+.\n\nNote: erlang_ls is archived. Consider ELP (Erlang Language Platform) by WhatsApp as the successor: https://github.com/WhatsApp/erlang-language-platform\nConfigure via erlang_ls.config in your project root.\nVS Code users: Install "Erlang LS" or "Erlang Language Platform" extension.\nSee: https://github.com/erlang-ls/erlang_ls`,
    copies: [
      { id: "copy_brew", value: "brew install erlang_ls" },
      { id: "copy_nix", value: "nix-env -iA nixpkgs.erlang-ls" },
    ],
  },
  {
    id: "fsharp",
    name: "F#",
    languages: ["fsharp"],
    message: `"{server}" (FsAutoComplete) provides completion, diagnostics, code actions, and refactoring for F#. Requires .NET SDK.\n\nVS Code users: Install "Ionide-fsharp" (bundles fsautocomplete).\nSee: https://github.com/fsharp/FsAutoComplete`,
    copies: [
      { id: "copy_dotnet", value: "dotnet tool install -g fsautocomplete" },
      { id: "copy_brew", value: "brew install fsautocomplete" },
      { id: "copy_nix", value: "nix-env -iA nixpkgs.fsautocomplete" },
    ],
  },
  {
    id: "gdscript",
    name: "GDScript",
    languages: ["gdscript"],
    // Godot hosts the server itself; the "server command" here is only the
    // connector that reaches it, so the wording differs from the rest.
    title: "GDScript Language Server Unavailable",
    notFoundStatus: `GDScript LSP connector '{server}' not found. Click status bar for help.`,
    errorStatus: "GDScript LSP error. Start Godot with the project open and check port 6005.",
    message: `"{server}" connects Fresh to Godot's built-in GDScript language server at 127.0.0.1:6005. Open the project in Godot, enable the language server in Godot editor settings if needed, and install netcat if the connector is missing.`,
    copies: [
      { id: "copy_macos", value: "brew install netcat" },
      { id: "copy_debian", value: "sudo apt install netcat-openbsd" },
      { id: "copy_arch", value: "sudo pacman -S openbsd-netcat" },
    ],
  },
  {
    id: "gleam",
    name: "Gleam",
    languages: ["gleam"],
    message: `The Gleam language server is built into the Gleam compiler binary. Install Gleam to get LSP support - no separate installation needed.\n\nProvides completion, diagnostics, hover, go-to-definition, and formatting.\nVS Code users: Install the "Gleam" extension.\nSee: https://gleam.run/getting-started/installing/`,
    copies: [
      { id: "copy_brew", value: "brew install gleam" },
      { id: "copy_cargo", value: "cargo install gleam" },
      { id: "copy_nix", value: "nix-env -iA nixpkgs.gleam" },
    ],
  },
  {
    id: "go",
    name: "Go",
    languages: ["go"],
    message: `"{server}" provides code completion, diagnostics, and navigation for Go files. Copy the command below to install it.`,
    copies: [{ id: "copy_go", value: "go install golang.org/x/tools/gopls@latest" }],
  },
  {
    id: "graphql",
    name: "GraphQL",
    languages: ["graphql"],
    message: `"{server}" provides code completion, validation, and hover info for GraphQL schemas and queries. Requires Node.js and a .graphqlrc config. Copy a command below to install it, or visit https://github.com/graphql/graphiql/tree/main/packages/graphql-language-service-cli for details.`,
    copies: [
      { id: "copy_npm", value: "npm i -g graphql-language-service-cli" },
      { id: "copy_yarn", value: "yarn global add graphql-language-service-cli" },
    ],
  },
  {
    id: "haskell",
    name: "Haskell",
    languages: ["haskell"],
    message: `"{server}" (HLS) provides completion, diagnostics, code actions, and refactoring for Haskell. HLS must match your GHC version.\n\nRecommended: Install via GHCup (manages GHC + HLS versions).\nInstall GHCup: curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh\nVS Code users: Install the "Haskell" extension (auto-installs HLS via GHCup).\nSee: https://haskell-language-server.readthedocs.io`,
    copies: [
      { id: "copy_ghcup", value: "ghcup install hls" },
      { id: "copy_brew", value: "brew install haskell-language-server" },
      { id: "copy_nix", value: "nix-env -iA nixpkgs.haskell-language-server" },
    ],
  },
  {
    id: "html",
    name: "HTML",
    languages: ["html"],
    message: `"{server}" provides code completion, diagnostics, and formatting for HTML files. Copy the command below to install it.`,
    copies: [{ id: "copy_npm", value: "npm install -g vscode-langservers-extracted" }],
  },
  {
    id: "java",
    name: "Java",
    languages: ["java"],
    message: `Install jdtls for code completion and diagnostics. Visit https://github.com/eclipse-jdtls/eclipse.jdt.ls#installation`,
    copies: [
      {
        id: "copy_url",
        value: "https://github.com/eclipse-jdtls/eclipse.jdt.ls#installation",
        label: "Copy install URL",
      },
    ],
  },
  {
    id: "json",
    name: "JSON",
    languages: ["json"],
    message: `"{server}" provides code completion, validation, and formatting for JSON files. Copy the command below to install it.`,
    copies: [{ id: "copy_npm", value: "npm install -g vscode-langservers-extracted" }],
  },
  {
    id: "julia",
    name: "Julia",
    languages: ["julia"],
    message: `The Julia language server (LanguageServer.jl) provides completion, diagnostics, formatting, and navigation for Julia. Julia must be installed.\n\nNote: First startup is slow due to Julia's compilation. Consider using PackageCompiler.jl for faster restarts.\nVS Code users: Install the "Julia" extension (auto-installs LanguageServer.jl).\nSee: https://github.com/julia-vscode/LanguageServer.jl`,
    copies: [{ id: "copy_julia", value: 'julia -e \'using Pkg; Pkg.add("LanguageServer")\'' }],
  },
  {
    id: "kotlin",
    name: "Kotlin",
    languages: ["kotlin"],
    message: `"{server}" provides code completion, diagnostics, and navigation for Kotlin files. Requires a JDK (Java 11+). Copy a command below to install it, or visit https://github.com/fwcd/kotlin-language-server for build instructions and releases. For full Kotlin IDE support, consider IntelliJ IDEA or Android Studio.`,
    copies: [
      { id: "copy_brew", value: "brew install kotlin-language-server" },
      { id: "copy_snap", value: "sudo snap install kotlin-language-server --classic" },
      { id: "copy_nix", value: "nix-env -i kotlin-language-server" },
    ],
  },
  {
    id: "latex",
    name: "LaTeX",
    languages: ["latex"],
    message: `Install texlab for code completion and diagnostics. Visit https://github.com/latex-lsp/texlab#installation`,
    copies: [
      {
        id: "copy_url",
        value: "https://github.com/latex-lsp/texlab#installation",
        label: "Copy install URL",
      },
    ],
  },
  {
    id: "lua",
    name: "Lua",
    languages: ["lua"],
    message: `"{server}" (LuaLS) provides code completion, diagnostics, and navigation for Lua files. Copy a command below to install it, or visit https://luals.github.io/#install for platform-specific instructions. Pre-built binaries are also available from https://github.com/LuaLS/lua-language-server/releases.`,
    copies: [
      { id: "copy_brew", value: "brew install lua-language-server" },
      { id: "copy_pacman", value: "sudo pacman -S lua-language-server" },
      { id: "copy_nix", value: "nix-env -i lua-language-server" },
    ],
  },
  {
    id: "marksman",
    name: "Markdown",
    languages: ["markdown"],
    message: `Install marksman for wiki-links and navigation. Visit https://github.com/artempyanykh/marksman#how-to-install`,
    copies: [
      {
        id: "copy_url",
        value: "https://github.com/artempyanykh/marksman#how-to-install",
        label: "Copy install URL",
      },
    ],
  },
  {
    id: "nim",
    name: "Nim",
    languages: ["nim"],
    message: `"{server}" provides completion, diagnostics, hover, and go-to-definition for Nim. Requires Nim and nimble.\n\nInstall Nim via choosenim: curl https://nim-lang.org/choosenim/init.sh -sSf | sh\nVS Code users: Install the "Nim" extension.\nSee: https://github.com/nim-lang/langserver`,
    copies: [
      { id: "copy_nimble", value: "nimble install nimlangserver" },
      { id: "copy_choosenim", value: "choosenim stable && nimble install nimlangserver" },
    ],
  },
  {
    id: "nix",
    name: "Nix",
    languages: ["nix"],
    message: `"{server}" provides completion, diagnostics, go-to-definition, and rename for Nix files.\n\nAlternative: nixd offers richer completions, option evaluation, and flake support.\nNote: rnix-lsp is deprecated.\nVS Code users: Install "Nix IDE" extension.\nSee: https://github.com/oxalica/nil`,
    copies: [
      { id: "copy_nix_profile", value: "nix profile install github:oxalica/nil" },
      { id: "copy_nix_env", value: "nix-env -iA nixpkgs.nil" },
      { id: "copy_nixd", value: "nix profile install nixpkgs#nixd" },
    ],
  },
  {
    id: "nushell",
    name: "Nushell",
    languages: ["nushell"],
    message: `The Nushell LSP server is built into the "nu" binary. Install Nushell to get LSP support. Visit https://www.nushell.sh/book/installation.html for platform-specific instructions.`,
    copies: [
      { id: "copy_cargo", value: "cargo install nu" },
      { id: "copy_brew", value: "brew install nushell" },
      { id: "copy_winget", value: "winget install nushell", label: "Copy: winget install nushell (Windows)" },
    ],
  },
  {
    id: "ocaml",
    name: "OCaml",
    languages: ["ocaml"],
    message: `"{server}" provides completion, diagnostics, type info, and refactoring for OCaml. Built on merlin.\n\nRequires opam (OCaml package manager). Install ocaml-lsp-server in your current opam switch.\nVS Code users: Install the "OCaml Platform" extension.\nSee: https://github.com/ocaml/ocaml-lsp`,
    copies: [
      { id: "copy_opam", value: "opam install ocaml-lsp-server" },
      { id: "copy_nix", value: "nix-env -iA nixpkgs.ocaml-lsp" },
    ],
  },
  {
    id: "odin",
    name: "Odin",
    languages: ["odin"],
    message: `"{server}" (OLS) provides code completion, diagnostics, and navigation for Odin files.\n\nInstallation: https://github.com/DanielGavin/ols`,
  },
  {
    id: "perl",
    name: "Perl",
    languages: ["perl"],
    message: `"{server}" (PerlNavigator) provides completion, diagnostics, navigation, and perlcritic/perltidy integration for Perl.\n\nAlternative: Perl::LanguageServer (older, via CPAN).\nVS Code users: Install the "Perl Navigator" extension.\nSee: https://github.com/bscan/PerlNavigator`,
    copies: [
      { id: "copy_npm", value: "npm install -g perlnavigator-server" },
      { id: "copy_cpan", value: "cpanm Perl::LanguageServer" },
    ],
  },
  {
    id: "php",
    name: "PHP",
    languages: ["php"],
    message: `"{server}" provides code completion, diagnostics, and navigation for PHP files. Requires PHP and Composer. Copy a command below to install it, or visit https://phpactor.readthedocs.io for details. Alternative: Intelephense (https://intelephense.com/) is a popular Node.js-based PHP LSP.`,
    copies: [
      { id: "copy_composer", value: "composer global require phpactor/phpactor" },
      { id: "copy_brew", value: "brew install phpactor" },
      { id: "copy_alt", value: "npm i -g intelephense", label: "Alternative: npm i -g intelephense" },
    ],
  },
  {
    id: "protobuf",
    name: "Protobuf",
    languages: ["protobuf"],
    message: `"{server}" (Buf CLI) provides code completion, diagnostics, linting, and formatting for Protocol Buffer files. Copy a command below to install it, or visit https://buf.build/docs/installation for details.`,
    copies: [
      { id: "copy_brew", value: "brew install bufbuild/buf/buf" },
      { id: "copy_npm", value: "npm i -g @bufbuild/buf" },
      { id: "copy_go", value: "go install github.com/bufbuild/buf/cmd/buf@latest" },
    ],
  },
  {
    id: "python",
    name: "Python",
    languages: ["python"],
    message: `"{server}" provides code completion, diagnostics, and navigation for Python files. Copy a command below to install it, or search online for your platform.`,
    copies: [
      { id: "copy_pipx", value: "pipx install python-lsp-server" },
      { id: "copy_pip", value: "pip install python-lsp-server" },
      { id: "copy_pip_all", value: "pip install 'python-lsp-server[all]'" },
    ],
  },
  {
    id: "r",
    name: "R",
    languages: ["r"],
    message: `The R language server provides completion, diagnostics, hover, formatting, and go-to-definition for R files. It runs as an R script, so R must be installed.\n\nInstall the languageserver R package, then the server runs via: R --vanilla -e 'languageserver::run()'\nVS Code users: Install the "R" extension by REditorSupport.\nSee: https://github.com/REditorSupport/languageserver`,
    copies: [
      { id: "copy_r", value: 'R -e \'install.packages("languageserver")\'' },
      { id: "copy_conda", value: "conda install -c conda-forge r-languageserver" },
    ],
  },
  {
    id: "ruby",
    name: "Ruby",
    languages: ["ruby"],
    message: `"{server}" provides code completion, diagnostics, and navigation for Ruby files. Requires Ruby/RubyGems. Copy a command below to install it, or visit https://solargraph.org/guides/getting-started for details. Alternative: Shopify's ruby-lsp (https://github.com/Shopify/ruby-lsp).`,
    copies: [
      { id: "copy_gem", value: "gem install solargraph" },
      { id: "copy_bundler", value: "bundle add solargraph --group development" },
      { id: "copy_alt", value: "gem install ruby-lsp", label: "Alternative: gem install ruby-lsp" },
    ],
  },
  {
    id: "scala",
    name: "Scala",
    languages: ["scala"],
    message: `"{server}" (Metals) provides completion, diagnostics, refactoring, and debugging for Scala. Requires Java 11+ and Coursier.\n\nSupports sbt, Mill, Gradle, Maven via Bloop/BSP.\nVS Code users: Install "Scala (Metals)" extension.\nNeovim users: Use nvim-metals plugin.\nSee: https://scalameta.org/metals/docs/editors/overview`,
    copies: [
      { id: "copy_coursier", value: "cs install metals" },
      { id: "copy_brew", value: "brew install coursier/formulas/coursier && cs install metals" },
    ],
  },
  {
    id: "solidity",
    name: "Solidity",
    languages: ["solidity"],
    message: `"{server}" (by Nomic Foundation) provides code completion, diagnostics, and navigation for Solidity smart contracts. Requires Node.js. Copy the command below to install it, or visit https://github.com/NomicFoundation/hardhat-vscode for details.`,
    copies: [{ id: "copy_npm", value: "npm i -g @nomicfoundation/solidity-language-server" }],
  },
  {
    id: "sql",
    name: "SQL",
    languages: ["sql"],
    message: `"{server}" provides completion, hover, and diagnostics for SQL files. It requires a config.yml to connect to your database. See: https://github.com/sqls-server/sqls\n\nAlternative: sql-language-server (npm) supports MySQL, PostgreSQL, SQLite.\nVS Code users: Try the SQLTools extension.`,
    copies: [
      { id: "copy_go", value: "go install github.com/sqls-server/sqls@latest" },
      { id: "copy_brew", value: "brew install sqls" },
      { id: "copy_npm", value: "npm install -g sql-language-server" },
    ],
  },
  {
    id: "svelte",
    name: "Svelte",
    languages: ["svelte"],
    message: `"{server}" provides completion, diagnostics, and formatting for Svelte components.\n\nFor TypeScript integration, also install typescript-svelte-plugin in your project.\nUse svelte-check for CI diagnostics.\nVS Code users: Install the "Svelte for VS Code" extension.\nSee: https://github.com/sveltejs/language-tools`,
    copies: [
      { id: "copy_npm", value: "npm install -g svelte-language-server" },
      { id: "copy_pnpm", value: "pnpm add -g svelte-language-server" },
    ],
  },
  {
    id: "swift",
    name: "Swift",
    languages: ["swift"],
    message: `"{server}" provides completion, diagnostics, and navigation for Swift files. It is bundled with the Swift toolchain.\n\nmacOS: Install Xcode Command Line Tools. Use 'xcrun sourcekit-lsp' if not in PATH.\nLinux: Download the Swift toolchain from swift.org.\nFor Xcode projects: Install xcode-build-server for build system integration.\nVS Code users: Install the "Swift" extension.\nSee: https://github.com/swiftlang/sourcekit-lsp`,
    copies: [
      { id: "copy_macos", value: "xcode-select --install" },
      { id: "copy_xbs", value: "brew install xcode-build-server" },
    ],
  },
  {
    id: "tailwindcss",
    name: "Tailwind CSS",
    languages: ["tailwindcss"],
    message: `"{server}" provides class name completion, color previews, hover info, and linting for Tailwind CSS.\n\nRequires Tailwind CSS configured in your project (tailwind.config.js or v4 CSS @import).\nVS Code users: Install "Tailwind CSS IntelliSense" extension.\nSee: https://github.com/tailwindlabs/tailwindcss-intellisense`,
    copies: [
      { id: "copy_npm", value: "npm install -g @tailwindcss/language-server" },
      { id: "copy_pnpm", value: "pnpm add -g @tailwindcss/language-server" },
    ],
  },
  {
    id: "templ",
    name: "Templ",
    languages: ["templ"],
    message: `Install templ for code completion and diagnostics. Visit https://templ.guide/quick-start/installation`,
    copies: [
      {
        id: "copy_url",
        value: "https://templ.guide/quick-start/installation",
        label: "Copy install URL",
      },
    ],
  },
  {
    id: "terraform",
    name: "Terraform",
    languages: ["terraform"],
    message: `"{server}" (by HashiCorp) provides code completion, diagnostics, and navigation for Terraform files. Copy a command below to install it, or visit https://github.com/hashicorp/terraform-ls for details and pre-built binaries.`,
    copies: [
      { id: "copy_brew", value: "brew install hashicorp/tap/terraform-ls" },
      { id: "copy_choco", value: "choco install terraform-ls", label: "Copy: choco install terraform-ls (Windows)" },
      { id: "copy_nix", value: "nix-env -i terraform-ls" },
    ],
  },
  {
    id: "toml",
    name: "TOML",
    languages: ["toml"],
    message: `"{server}" provides code completion, validation, formatting, and schema support for TOML files (Cargo.toml, pyproject.toml, etc.). Copy a command below to install it, or visit https://taplo.tamasfe.dev/cli/installation.html for details.`,
    copies: [
      { id: "copy_cargo", value: "cargo install taplo-cli --locked" },
      { id: "copy_npm", value: "npm i -g @taplo/cli" },
      { id: "copy_brew", value: "brew install taplo" },
    ],
  },
  {
    id: "typescript",
    name: "TypeScript",
    languages: ["typescript", "javascript", "typescriptreact", "javascriptreact"],
    // The React variants share the server, but only the two base languages
    // can be switched off.
    disables: ["typescript", "javascript"],
    disabledStatus: "TypeScript/JavaScript LSP disabled",
    message: `"{server}" provides code completion, diagnostics, and navigation for TypeScript/JavaScript files. Copy a command below to install it, or search online for your platform.`,
    copies: [
      { id: "copy_npm", value: "npm install -g typescript-language-server typescript" },
      { id: "copy_yarn", value: "yarn global add typescript-language-server typescript" },
      { id: "copy_pnpm", value: "pnpm add -g typescript-language-server typescript" },
    ],
  },
  {
    id: "typst",
    name: "Typst",
    languages: ["typst"],
    message: `"{server}" provides code completion, diagnostics, and preview support for Typst documents. Copy a command below to install it, or visit https://github.com/Myriad-Dreamin/tinymist for details and pre-built binaries. Also available as the "Tinymist Typst" VS Code extension.`,
    copies: [
      { id: "copy_cargo", value: "cargo install tinymist" },
      { id: "copy_brew", value: "brew install tinymist" },
      { id: "copy_nix", value: "nix-env -iA nixpkgs.tinymist" },
    ],
  },
  {
    id: "vue",
    name: "Vue",
    languages: ["vue"],
    message: `"{server}" (formerly Volar) provides completion, diagnostics, and refactoring for Vue SFCs. It replaces the deprecated Vetur.\n\nFor TypeScript integration, also install @vue/typescript-plugin.\nVS Code users: Install the "Vue - Official" extension.\nSee: https://github.com/vuejs/language-tools`,
    copies: [
      { id: "copy_npm", value: "npm install -g @vue/language-server" },
      { id: "copy_pnpm", value: "pnpm add -g @vue/language-server" },
    ],
  },
  {
    id: "yaml",
    name: "YAML",
    languages: ["yaml"],
    message: `"{server}" provides code completion, validation, and schema support for YAML files. Requires Node.js. Supports JSON Schema validation and built-in Kubernetes schemas. Copy a command below to install it, or visit https://github.com/redhat-developer/yaml-language-server for details.`,
    copies: [
      { id: "copy_npm", value: "npm i -g yaml-language-server" },
      { id: "copy_yarn", value: "yarn global add yaml-language-server" },
      { id: "copy_pnpm", value: "pnpm add -g yaml-language-server" },
    ],
  },
  {
    id: "zig",
    name: "Zig",
    languages: ["zig"],
    message: `Install zls for code completion and diagnostics. Visit https://github.com/zigtools/zls#installation`,
    copies: [
      {
        id: "copy_url",
        value: "https://github.com/zigtools/zls#installation",
        label: "Copy install URL",
      },
    ],
  },
];

/** Language -> entry, and popup id -> entry, built once at load. */
const byLanguage = new Map<string, ServerHelp>();
const byPopupId = new Map<string, ServerHelp>();
/** Last failure per entry id; cleared when the language's LSP is disabled. */
const failures = new Map<string, { serverCommand: string; message: string }>();

function popupIdFor(help: ServerHelp): string {
  return `${help.id}-lsp-help`;
}

/** Substitute a template placeholder without `String.replace`'s `$` escapes. */
function fill(template: string, placeholder: string, value: string): string {
  return template.split(placeholder).join(value);
}

for (const help of SERVERS) {
  for (const language of help.languages) {
    byLanguage.set(language, help);
  }
  byPopupId.set(popupIdFor(help), help);
}

editor.on("lsp_server_error", (data) => {
  const help = byLanguage.get(data.language);
  if (!help) return;

  editor.debug(`lsp_help: ${help.id}: server error - ${data.error_type}: ${data.message}`);
  failures.set(help.id, { serverCommand: data.server_command, message: data.message });

  if (data.error_type === "not_found") {
    const template =
      help.notFoundStatus ?? `${help.name} LSP server '{server}' not found. Click status bar for help.`;
    editor.setStatus(fill(template, "{server}", data.server_command));
  } else {
    const template = help.errorStatus ?? `${help.name} LSP error: {message}`;
    editor.setStatus(fill(template, "{message}", data.message));
  }
});

editor.on("lsp_status_clicked", (data) => {
  const help = byLanguage.get(data.language);
  if (!help) return;
  const failure = failures.get(help.id);
  if (!failure) return;

  editor.debug(`lsp_help: ${help.id}: status clicked, showing help popup`);

  const actions = (help.copies ?? []).map((copy) => ({
    id: copy.id,
    label: copy.label ?? `Copy: ${copy.value}`,
  }));
  actions.push({ id: "disable", label: `Disable ${help.name} LSP` });
  actions.push({ id: "dismiss", label: "Dismiss (ESC)" });

  editor.showActionPopup({
    id: popupIdFor(help),
    title: help.title ?? `${help.name} Language Server Not Found`,
    message: fill(help.message, "{server}", failure.serverCommand),
    actions,
  });
});

editor.on("action_popup_result", (data) => {
  const help = byPopupId.get(data.popup_id);
  if (!help) return;

  editor.debug(`lsp_help: ${help.id}: action selected - ${data.action_id}`);

  if (data.action_id === "disable") {
    for (const language of help.disables ?? help.languages) {
      editor.disableLspForLanguage(language);
    }
    editor.setStatus(help.disabledStatus ?? `${help.name} LSP disabled`);
    failures.delete(help.id);
    return;
  }

  // Closing without acting on the popup.
  if (data.action_id === "dismiss" || data.action_id === "dismissed") return;

  const copy = (help.copies ?? []).find((candidate) => candidate.id === data.action_id);
  if (!copy) {
    editor.debug(`lsp_help: ${help.id}: unknown action: ${data.action_id}`);
    return;
  }
  editor.setClipboard(copy.value);
  editor.setStatus("Copied: " + copy.value);
});

editor.debug(`lsp_help: plugin loaded (${SERVERS.length} language servers)`);
