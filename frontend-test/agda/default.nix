{ mkDerivation, Agda, ansi-wl-pprint, base, containers, directory
, filepath, grin, mtl, stdenv, transformers
}:
mkDerivation {
  pname = "agda-grin";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    Agda ansi-wl-pprint base containers directory filepath grin mtl
    transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
