{ mkDerivation, array, base, bytestring, directory, filepath
, llvm-hs, llvm-hs-pure, mtl, prettyprinter, tasty, tasty-golden
, tasty-hspec, tasty-hunit, text, transformers, stdenv
}:
mkDerivation {
  pname = "llvm-hs-pretty";
  version = "0.6.1.0";
  sha256 = "12w1rkkaf50jl2vdkyk4xpvjmsxzjbfkdyklaq5p6b8ykw872pda";
  libraryHaskellDepends = [
    array base bytestring llvm-hs-pure prettyprinter text
  ];
  testHaskellDepends = [
    base directory filepath llvm-hs llvm-hs-pure mtl tasty tasty-golden
    tasty-hspec tasty-hunit text transformers
  ];
  description = "A pretty printer for LLVM IR";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
}
