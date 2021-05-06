let
  pkgs = import ./default.nix;
in
  pkgs.shellFor {
    buildInputs = with pkgs.haskellPackages; [
      hlint
      ghcid
    ];

    GRIN_CC = "${pkgs.clang_7}/bin/clang";
    GRIN_OPT = "${pkgs.llvm_7}/bin/opt";
    GRIN_LLC = "${pkgs.llvm_7}/bin/llc";
  }
