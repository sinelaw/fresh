{
  description = "Fresh - A lightweight, fast terminal-based text editor with LSP support and TypeScript plugins";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    crane.url = "github:ipetkov/crane";

    flake-utils.url = "github:numtide/flake-utils";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, crane, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };

        # Use stable Rust from rust-overlay for reproducibility
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
        };

        craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;

        # Common arguments for crane builds
        commonArgs = {
          src = craneLib.cleanCargoSource ./.;
          strictDeps = true;

          # Build inputs needed for compilation
          # Note: On Darwin, frameworks (Security, SystemConfiguration, CoreServices) and
          # libiconv are now provided automatically by the stdenv's default SDK.
          # See: https://discourse.nixos.org/t/the-darwin-sdks-have-been-updated/55295
          buildInputs = with pkgs; [
            # Required for various crates
            openssl
          ];

          nativeBuildInputs = with pkgs; [
            pkg-config
            # For tree-sitter grammars that need C compilation
            clang
          ];

          # Environment variables
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
        };

        # Build dependencies separately for better caching
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        # Build the actual package
        fresh = craneLib.buildPackage (commonArgs // {
          inherit cargoArtifacts;

          # Include runtime assets
          postInstall = ''
            # Copy queries (tree-sitter syntax queries)
            if [ -d queries ]; then
              mkdir -p $out/share/fresh-editor
              cp -r queries $out/share/fresh-editor/
            fi

            # Copy themes
            if [ -d themes ]; then
              cp -r themes $out/share/fresh-editor/
            fi

            # Copy keymaps
            if [ -d keymaps ]; then
              cp -r keymaps $out/share/fresh-editor/
            fi

            # Copy plugins
            if [ -d plugins ]; then
              cp -r plugins $out/share/fresh-editor/
            fi
          '';
        });
      in
      {
        checks = {
          # Build the package as a check
          inherit fresh;

          # Run clippy
          fresh-clippy = craneLib.cargoClippy (commonArgs // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          });

          # Run tests
          fresh-test = craneLib.cargoTest (commonArgs // {
            inherit cargoArtifacts;
          });

          # Check formatting
          fresh-fmt = craneLib.cargoFmt {
            src = craneLib.cleanCargoSource ./.;
          };
        };

        packages = {
          default = fresh;
          fresh = fresh;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = fresh;
        };

        devShells.default = craneLib.devShell {
          # Inherit inputs from the main build
          checks = self.checks.${system};

          # Additional development tools
          packages = with pkgs; [
            # Rust tools (already included via toolchain, but explicit for clarity)
            rustToolchain
            cargo-watch
            cargo-edit

            # For development
            pkg-config
            clang

            # For tree-sitter grammar development
            tree-sitter

            # Useful for debugging
            lldb
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            # Linux-specific dev tools
            valgrind
            strace
          ];

          # Set up environment
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
        };
      });
}
