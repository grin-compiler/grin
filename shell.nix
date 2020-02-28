let
  pkgs = import ./nix/packages.nix {};
  haskellPkgs = import ./.;
in
  haskellPkgs.shellFor {
    buildInputs = with pkgs.haskellPackages; [
      hlint
      ghcid
    ];

    GRIN_CC = "${pkgs.clang_7}/bin/clang";
    GRIN_OPT = "${pkgs.llvm_7}/bin/opt";
    GRIN_LLC = "${pkgs.llvm_7}/bin/llc";
  }
