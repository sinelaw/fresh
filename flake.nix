{
  description = "Fresh - A lightweight, fast terminal-based text editor with LSP support and TypeScript plugins";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    crane.url = "github:ipetkov/crane";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      crane,
      fenix,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        inherit (nixpkgs) lib;
        pkgs = import nixpkgs { inherit system; };

        rustToolchain = fenix.packages.${system}.stable.withComponents [
          "cargo"
          "clippy"
          "rust-src"
          "rustc"
          "rustfmt"
        ];
        craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;

        # Source filtering: include Cargo files plus additional files
        unfilteredRoot = ./.; # The original, unfiltered source
        src = lib.fileset.toSource {
          root = unfilteredRoot;
          fileset = lib.fileset.unions [
            # Default files from crane (Rust and cargo files)
            (craneLib.fileset.commonCargoSources unfilteredRoot)
            # Also keep any javascript files
            (lib.fileset.fileFilter (file: file.hasExt "js") unfilteredRoot)
            # Keep sublime-syntax grammar files (used by include_str! in grammar_registry.rs)
            (lib.fileset.fileFilter (file: file.hasExt "sublime-syntax") unfilteredRoot)
            # Runtime assets in crates/fresh-editor
            ./crates/fresh-editor/keymaps
            ./crates/fresh-editor/locales
            ./crates/fresh-editor/plugins
            ./crates/fresh-editor/queries
            ./crates/fresh-editor/themes
            ./crates/fresh-editor/types
            # Test files
            ./crates/fresh-editor/tests
            # Documentation
            ./docs
          ];
        };

        # Prefetch rusty_v8 static library to avoid network access during build
        # We validate the hash on the compressed download, then decompress for rusty_v8
        librusty_v8 =
          let
            tag = "142.2.0";
            target = pkgs.stdenv.hostPlatform.rust.rustcTarget;
            hashes = {
              x86_64-unknown-linux-gnu = "sha256-xHmofo8wTNg88/TuC2pX2OHDRYtHncoSvSBnTV65o+0=";
              aarch64-unknown-linux-gnu = "sha256-24q6wX8RTRX1tMGqgcz9/wN3Y+hWxM2SEuVrYhECyS8=";
              x86_64-apple-darwin = "sha256-u7fImeadycU1gS5m+m35WZA/G2SOdPrLOMafY54JwY4=";
              aarch64-apple-darwin = "sha256-XvJ7M5XxOzmevv+nPpy/mvEDD1MfHr986bImvDG0o4U=";
            };
          in
          pkgs.stdenv.mkDerivation {
            name = "librusty_v8-${tag}";
            src = pkgs.fetchurl {
              url = "https://github.com/denoland/rusty_v8/releases/download/v${tag}/librusty_v8_release_${target}.a.gz";
              sha256 = hashes.${target};
            };
            nativeBuildInputs = [ pkgs.gzip ];
            dontUnpack = true;
            installPhase = ''
              gzip -d -c $src > $out
            '';
          };

        # Common arguments for crane builds
        commonArgs = {
          inherit src;
          strictDeps = true;

          nativeBuildInputs = with pkgs; [
            pkg-config
            # For tree-sitter grammars that need C compilation
            clang
          ];

          doCheck = false;
        }
        // commonVars;

        commonVars = {
          # Environment variables
          LIBCLANG_PATH = pkgs.lib.makeLibraryPath [
            pkgs.llvmPackages.libclang.lib
          ];
          # Point to prefetched rusty_v8 library to avoid download during build
          RUSTY_V8_ARCHIVE = librusty_v8;
        };

        # Build dependencies separately for better caching
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        # Build the actual package
        fresh = craneLib.buildPackage (
          commonArgs
          // {
            inherit cargoArtifacts;

            # Include runtime assets
            postInstall = ''
              mkdir -p $out/share/fresh-editor
              cp -r crates/fresh-editor/queries $out/share/fresh-editor/
              cp -r crates/fresh-editor/themes $out/share/fresh-editor/
              cp -r crates/fresh-editor/keymaps $out/share/fresh-editor/
              cp -r crates/fresh-editor/plugins $out/share/fresh-editor/
            '';
          }
        );
      in
      {
        checks = {
          # Build the package as a check
          inherit fresh;

          # Run clippy
          fresh-clippy = craneLib.cargoClippy (
            commonArgs
            // {
              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--all-targets -- --deny warnings";
            }
          );

          # Run tests
          fresh-test = craneLib.cargoTest (
            commonArgs
            // {
              inherit cargoArtifacts;
            }
          );

          # Check formatting
          fresh-fmt = craneLib.cargoFmt {
            inherit src;
          };
        };

        packages = {
          inherit fresh;
          default = fresh;
        };

        apps.default = {
          type = "app";
          program = "${fresh}/bin/fresh";
          meta.description = "Text editor for your terminal: easy, powerful and fast";
        };

        devShells.default = craneLib.devShell (
          commonVars
          // {
            # Inherit inputs from the main build
            checks = self.checks.${system};

            # Additional development tools
            packages = with pkgs; [
              rustToolchain
              cargo-watch
              cargo-edit
              rust-analyzer
              vscode-json-languageserver

              tree-sitter

              # Useful for debugging
              lldb
            ];
          }
        );
      }
    );
}
