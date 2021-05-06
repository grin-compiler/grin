{
  description = "GRIN - Graph Reduction Intermediate Notation";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    overlays = [ haskellNix.overlay
    (final: prev: {
          # This overlay adds our project to pkgs
          grinProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8104";
            };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        flake = pkgs.grinProject.flake {};
        packageName = "grin";
  in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."grin:exe:grin";

      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      devShell = pkgs.grinProject.shellFor {
        tools = {
          cabal = "latest";
          hlint = "latest";
          haskell-language-server = "latest";
        };
      };
    });
}
