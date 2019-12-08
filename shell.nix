{ pkgs ? import nix/pkgs.nix }:

let
  compiler = "ghc865";
  grin = import ./. { inherit compiler pkgs; };
in

pkgs.haskell.packages."${compiler}".shellFor {
  withHoogle = true;
  packages = p: [ grin ];
  inherit (grin.env) nativeBuildInputs;
}
