let
  spec = builtins.fromJSON (builtins.readFile ./haskell-nix-src.json);
  haskell-nix-src = builtins.fetchGit {
    name = "haskell-lib";
    inherit (spec) url rev;
  };
in
  import haskell-nix-src
