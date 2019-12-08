{ compiler ? "ghc865", pkgs ? import nix/pkgs.nix }:

let
  hlib = pkgs.haskell.lib;
  hpkg = pkgs.haskell.packages."${compiler}";
  llvm-hs = import ./nix/llvm-hs.nix;
in

hpkg.developPackage {
  root = ./grin;
  overrides = self: super: {
    llvm-hs = hlib.dontCheck (self.callCabal2nix "llvm-hs" "${llvm-hs}/llvm-hs" {
      llvm-config = pkgs.llvm_7;
    });
    llvm-hs-pure = self.callCabal2nix "llvm-hs-pure" "${llvm-hs}/llvm-hs-pure" {};
    llvm-hs-pretty = self.callPackage ./nix/llvm-hs-pretty.nix {};
  };
  modifier = drv: hlib.addBuildTools drv [
    (import nix/llvm.nix {})
    pkgs.clang_7
    pkgs.llvm_7
  ];
  returnShellEnv = false;
}
