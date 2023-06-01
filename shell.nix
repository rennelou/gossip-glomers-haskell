{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = [
      pkgs.haskell.compiler.ghc924
      pkgs.cabal-install
      pkgs.openjdk
      pkgs.graphviz
      pkgs.gnuplot
    ];
}
