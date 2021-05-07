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
      llvm-overlay = self: super: {
        llvm-config = self.llvm_7;
        GRIN_CC = "${pkgs.clang_7}/bin/clang";
        GRIN_OPT = "${pkgs.llvm_7}/bin/opt";
        GRIN_LLC = "${pkgs.llvm_7}/bin/llc";
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
            # This overlay adds our project to pkgs
            grinProject =
              final.haskell-nix.stackProject' {
                name = "grin";
                src = ./.;
                compiler-nix-name = "ghc865";
              };
            })
          llvm-overlay
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.grinProject.flake {};
      executable = "grin:exe:grin";
      app = flake-utils.lib.mkApp {
        name = "grin";
        exePath = "/bin/grin";
        drv = self.packages.${system}.${executable};
      };
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages.${executable};

      # `nix run`
      apps.grin = app;
      defaultApp = app;

      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      devShell = pkgs.grinProject.shellFor {
        tools = {
          cabal = "latest";
          hlint = "latest";
          haskell-language-server = "latest";
        };

        # Env
        GRIN_CC = "${pkgs.clang_7}/bin/clang";
        GRIN_OPT = "${pkgs.llvm_7}/bin/opt";
        GRIN_LLC = "${pkgs.llvm_7}/bin/llc";
      };
    }
  );
}
