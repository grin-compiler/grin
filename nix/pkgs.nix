let
  pinned = (import <nixpkgs> {}).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs-channels";
    rev    = "ffc604e55790dbf575187738e1c3778231e8f5a9";
    sha256 = "1kw37ab6abkrjn65zhh0jcnfb3bzk1p14d178sp8489kn9gxy12v";
  };
in import pinned {}
