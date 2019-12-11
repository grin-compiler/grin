let
  pkgs = import ./nix/packages.nix {};
  llvm-links = import ./nix/llvm.nix { inherit pkgs; };
  haskellPkgs = import ./.;
in
  haskellPkgs.shellFor {
    buildInputs = with pkgs.haskellPackages; [
      llvm-links
      hlint
      ghcid
    ];
  }
