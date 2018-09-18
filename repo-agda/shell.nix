{ fetchFromGitHub ? (import <nixpkgs> {}).fetchFromGitHub
# peg nix packages to specific version
, pkgs ?
    import
      ( fetchFromGitHub
        {
          owner = "NixOS";
          repo = "nixpkgs-channels";
          rev = "696c6bed4e8e2d9fd9b956dea7e5d49531e9d13f";
          sha256 = "1v3yrpj542niyxp0h3kffsdjwlrkvj0mg4ljb85d142gyn3sdzd4";
        }
      ) {}
}:
let
    # extract the Haskell dependencies of a package
    extractHaskellDependencies = (hpkg:
        with builtins;
        let
          isHaskellPkg = x: (isAttrs x) && (x ? pname) && (x ? version) && (x ? env);
          packagesFromDrv = x:
            let
              inputs =
                (x.buildInputs or []) ++
                (x.nativeBuildInputs or []) ++
                (x.propagatedBuildInputs or []) ++
                (x.propagatedNativeBuildInputs or []);

            in
              (filter isHaskellPkg  inputs);
          go1 = s: xs: foldl' go2 s xs;
          go2 = s: x:
            if s ? "${x.pname}"
               then s
               else go1 (s // {"${x.pname}" = x;}) (packagesFromDrv x);
        in assert isAttrs hpkg; attrNames (go1 {} (packagesFromDrv hpkg)));

    # Haskell custom overrides
    haskellPackages = pkgs.haskell.packages.ghc822.override (old:
    { overrides = self: super: with pkgs.haskell.lib;
        {
          free = self.callPackage ../../nix/free.nix {};
          functor-infix = doJailbreak (super.functor-infix);
          llvm-hs = self.callPackage ../../nix/llvm-hs.nix {llvm-config = pkgs.llvm_5;};
          llvm-hs-pure = self.callPackage ../../nix/llvm-hs-pure.nix {};
          llvm-hs-pretty = pkgs.haskell.lib.dontCheck (self.callPackage ../../nix/llvm-hs-pretty.nix {});
          grin = dontHaddock (self.callPackage ../../grin/default.nix {});
        };
    });

    # the grin package
    agda-grin = haskellPackages.callPackage ./. {};
    # grin's dependencies
    agdaGrinDeps = extractHaskellDependencies agda-grin;
    # use a GHC with all the Hakell dependencies and the documentation for them and a Hoogle server to search them
    ghcWith = haskellPackages.ghcWithHoogle (hs: map (x: builtins.getAttr x hs) agdaGrinDeps);

    llc5 = pkgs.runCommand "llc5"
      {
        buildInputs = [pkgs.llvm_5];
      }
      ''
        mkdir -p $out/bin
        cd $out/bin
        ln -s ${pkgs.llvm_5}/bin/llc llc-5.0
      '';
in
# environment setup with all the needed tools
pkgs.runCommand "agda-grin-shell"
      {
        shellHook = ''
          eval $(egrep ^export ${ghcWith}/bin/ghc)
         '';
        buildInputs = [ghcWith llc5 pkgs.llvm_5 haskellPackages.cabal-install];
      }
      "touch $out"
