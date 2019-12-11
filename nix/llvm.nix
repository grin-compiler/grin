{ pkgs ? import ./packages.nix {} }:

pkgs.runCommand
  "llvm-7-links"
  { buildInputs = [ pkgs.llvm_7 ]; }
  ''
    mkdir -p $out/bin
    cd $out/bin
    ln -s ${pkgs.llvm_7}/bin/llc llc-7
    ln -s ${pkgs.llvm_7}/bin/opt opt-7
  ''
