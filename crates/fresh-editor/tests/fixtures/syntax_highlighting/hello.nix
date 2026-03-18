# Nix syntax highlighting test
{ pkgs ? import <nixpkgs> {} }:

let
  greeting = "Hello, World!";
  config = {
    version = "1.0";
    enabled = true;
    count = 42;
  };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    rustc
    cargo
    pkg-config
  ];

  shellHook = ''
    echo "${greeting}"
  '';
}
