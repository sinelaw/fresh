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

        # Prefetch rusty_v8 static library to avoid network access during build
        # We validate the hash on the compressed download, then decompress for rusty_v8
        librusty_v8 = let
          v8_version = "142.2.0";
          arch = pkgs.stdenv.hostPlatform.rust.rustcTarget;
        in pkgs.stdenv.mkDerivation {
          name = "librusty_v8-${v8_version}";
          src = pkgs.fetchurl {
            url = "https://github.com/denoland/rusty_v8/releases/download/v${v8_version}/librusty_v8_release_${arch}.a.gz";
            sha256 = {
              x86_64-linux = "sha256-xHmofo8wTNg88/TuC2pX2OHDRYtHncoSvSBnTV65o+0=";
              aarch64-linux = "sha256-24q6wX8RTRX1tMGqgcz9/wN3Y+hWxM2SEuVrYhECyS8=";
              x86_64-darwin = "sha256-u7fImeadycU1gS5m+m35WZA/G2SOdPrLOMafY54JwY4=";
              aarch64-darwin = "sha256-XvJ7M5XxOzmevv+nPpy/mvEDD1MfHr986bImvDG0o4U=";
            }.${system} or (throw "Unsupported system: ${system}");
          };
          nativeBuildInputs = [ pkgs.gzip ];
          dontUnpack = true;
          installPhase = ''
            gzip -d -c $src > $out
          '';
        };

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
          # Point to prefetched rusty_v8 library to avoid download during build
          RUSTY_V8_ARCHIVE = librusty_v8;
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
        } // { meta.description = "Fresh terminal-based text editor"; };

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
