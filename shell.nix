let
  sources = import ./nix/sources.nix {};
  nixpkgs = import sources.nixpkgs {};
  pkgs = import ./default.nix;
in
  pkgs.shellFor {
    buildInputs = with nixpkgs.haskellPackages; [
      hlint
      ghcid
    ];

    GRIN_CC = "${nixpkgs.clang_7}/bin/clang";
    GRIN_OPT = "${nixpkgs.llvm_7}/bin/opt";
    GRIN_LLC = "${nixpkgs.llvm_7}/bin/llc";
  }
