{ mkDerivation, array, base, bytestring, directory, filepath
, llvm-hs, llvm-hs-pure, mtl, pretty-show, stdenv, tasty
, tasty-golden, tasty-hspec, tasty-hunit, text, transformers
, wl-pprint-text
}:
mkDerivation {
  pname = "llvm-hs-pretty";
  version = "0.2.0.0";
  sha256 = "90ce478f6386f836e3b646186c4fe4d72598cc938d8fbb150718a1bbf4f4738c";
  libraryHaskellDepends = [
    array base bytestring llvm-hs-pure text wl-pprint-text
  ];
  testHaskellDepends = [
    base directory filepath llvm-hs llvm-hs-pure mtl pretty-show tasty
    tasty-golden tasty-hspec tasty-hunit text transformers
  ];
  homepage = "https://github.com/llvm-hs/llvm-hs-pretty";
  description = "Pretty printer for LLVM IR";
  license = stdenv.lib.licenses.mit;
}
