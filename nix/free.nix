{ mkDerivation, base, bifunctors, comonad, containers, distributive
, exceptions, mtl, prelude-extras, profunctors, semigroupoids
, semigroups, stdenv, template-haskell, transformers
, transformers-compat
}:
mkDerivation {
  pname = "free";
  version = "4.12.4";
  sha256 = "c9fe45aae387855626ecb5a0fea6afdb207143cb00af3b1f715d1032d2d08784";
  revision = "2";
  editedCabalFile = "0gmib9bmswrqhl47cp5b871v9f44v9yidzxpljkszy49y9qdf560";
  libraryHaskellDepends = [
    base bifunctors comonad containers distributive exceptions mtl
    prelude-extras profunctors semigroupoids semigroups
    template-haskell transformers transformers-compat
  ];
  homepage = "http://github.com/ekmett/free/";
  description = "Monads for free";
  license = stdenv.lib.licenses.bsd3;
}
