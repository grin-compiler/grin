{ pkgs ? import nix/pkgs.nix }:

let
  compiler = "ghc865";
  hpkg = pkgs.haskell.packages."${compiler}";
  grin = import ./. { inherit compiler pkgs; };
in

hpkg.shellFor {
  withHoogle = true;
  packages = p: [ grin ];
  buildInputs = with hpkg; [ cabal-install ghcid ];
  inherit (grin.env) nativeBuildInputs;
}
