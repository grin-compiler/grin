let
  commit = "2adf2d615cf8f6c6be49a0e54dc9ebf551dcf70f";
  pinned = builtins.fetchTarball {
    name   = "nixpkgs-19.09";
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "0w8zgj38di4ssw6s0bxdb9rgksqfszsa3304863xic4bzsvkql9b";
  };
in import pinned {}
