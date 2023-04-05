{ pkgs ? import <nixpkgs> {} }:

with pkgs;
mkShell {
  buildInputs = [
    gmp
    opam
    pkg-config

    linuxPackages.perf
    flamegraph
  ];
}
