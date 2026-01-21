{
  description = "Fresh - A lightweight, fast terminal-based text editor with LSP support and TypeScript plugins";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    crane.url = "github:ipetkov/crane";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:NixOS/flake-compat";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;

      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem =
        {
          self',
          pkgs,
          lib,
          system,
          ...
        }:
        let
          cargoToml = lib.importTOML "${src}/Cargo.toml";

          pname = "fresh";
          version = cargoToml.workspace.package.version;

          rust-manifest = pkgs.fetchurl {
            url = "https://static.rust-lang.org/dist/channel-rust-1.91.0.toml";
            hash = "sha256-2eWc3xVTKqg5wKSHGwt1XoM/kUBC6y3MWfKg74Zn+fY=";
          };

          rustToolchain = inputs.fenix.packages.${system}.fromManifestFile rust-manifest;
          craneLib = (inputs.crane.mkLib pkgs).overrideToolchain rustToolchain.defaultToolchain;

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
              ./crates/fresh-editor/docs
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
                hash = hashes.${target};
              };
              nativeBuildInputs = [ pkgs.gzip ];
              dontUnpack = true;
              installPhase = ''
                gzip -d -c $src > $out
              '';
            };

          commonVars = {
            # Environment variables
            LIBCLANG_PATH = pkgs.lib.makeLibraryPath [
              pkgs.llvmPackages.libclang.lib
            ];
            # Point to prefetched rusty_v8 library to avoid download during build
            RUSTY_V8_ARCHIVE = librusty_v8;
          };

          # Common arguments for crane builds
          commonArgs = {
            inherit src pname version;
            strictDeps = true;

            nativeBuildInputs = with pkgs; [
              pkg-config
              # For tree-sitter grammars that need C compilation
              clang
            ];

            doCheck = false;
          }
          // commonVars;

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

              meta.mainProgram = "fresh";
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
            default = self'.packages.fresh;
          };

          overlayAttrs = builtins.removeAttrs self'.packages [ "default" ];

          devShells.default = craneLib.devShell (
            commonArgs
            // {
              # inherit (self') checks;

              # Additional development tools
              buildInputs = with pkgs; [
                rustToolchain.rust-analyzer
                rustToolchain.rust-src
                cargo-watch
                cargo-edit
                vscode-json-languageserver

                tree-sitter

                # Useful for debugging
                lldb
              ];

              RUST_SRC_PATH = "${rustToolchain.rust-src}/lib/rustlib/src/rust/library";
            }
          );
        };
    };
}
