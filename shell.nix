{ pkgs ? import nix/pkgs.nix }:

let grin = import ./. { inherit pkgs; };
in pkgs.haskell.packages.ghc844.shellFor {
  withHoogle = true;
  packages = p: [ grin ];
  inherit (grin.env) nativeBuildInputs;
}
