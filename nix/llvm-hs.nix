{ mkDerivation, array, attoparsec, base, bytestring, Cabal
, containers, exceptions, llvm-config, llvm-hs-pure, mtl
, pretty-show, QuickCheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, transformers
, utf8-string
}:
mkDerivation {
  pname = "llvm-hs";
  version = "5.1.3";
  sha256 = "ccdac4683f56135ba83ed0883231f686d1784e9bc7f072a34fcf041e0661976b";
  revision = "1";
  editedCabalFile = "0r8xgz6r2miw1l3az343jwz8f9jgzqywxnzl2xhanv05g3i462yh";
  setupHaskellDepends = [ base Cabal containers ];
  libraryHaskellDepends = [
    array attoparsec base bytestring containers exceptions llvm-hs-pure
    mtl template-haskell transformers utf8-string
  ];
  libraryToolDepends = [ llvm-config ];
  testHaskellDepends = [
    base bytestring containers llvm-hs-pure mtl pretty-show QuickCheck
    tasty tasty-hunit tasty-quickcheck temporary transformers
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "General purpose LLVM bindings";
  license = stdenv.lib.licenses.bsd3;
}
