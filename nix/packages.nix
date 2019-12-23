let
  commit = "4c6e9a553872366e485766fa8b574b54b15957f8";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-19.09";
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "1az9yg2lwwfw3aypi6bdsk7ghld2963vqdp8ajkxciyxdfkaxb3b";
  };
  pkgs = import nixpkgs;
in
  pkgs
