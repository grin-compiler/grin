{ pkgs ? import nix/pkgs.nix }:

let
  hlib = pkgs.haskell.lib;
  hpkg = pkgs.haskell.packages.ghc844;
in

hpkg.developPackage {
  root = ./grin;
  overrides = self: super: {
    Diff = hlib.dontCheck super.Diff;
    llvm-hs = super.llvm-hs.override (oa: {
      llvm-config = pkgs.llvm_7;
    });
    llvm-hs-pretty = hlib.unmarkBroken super.llvm-hs-pretty;
  };
  modifier = drv: hlib.addBuildTools drv [
    (import nix/llvm.nix {})
    pkgs.clang_7
    pkgs.llvm_7
  ];
  returnShellEnv = false;
}
