let
  sources = import ./sources.nix {};
  haskellNix = import sources.haskellNix {};
  llvm-overlay = self: super: {
    llvm-config = self.llvm_7;
  };
  extra-overlays = [ llvm-overlay ];
  pkgs = import
    haskellNix.sources.nixpkgs-2009
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    (haskellNix.nixpkgsArgs // { overlays = haskellNix.nixpkgsArgs.overlays ++ extra-overlays; });
in
  pkgs
