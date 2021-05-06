let
  pkgs = import ./nix/haskell.nix;
in
  pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "grin";
      src = ./.;
    };
  }
