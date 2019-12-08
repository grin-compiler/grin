let
  url = "https://github.com/csabahruska/llvm-hs";
  ref = "llvm-7";
  rev = "868e23a13942703255979369defdb49ac57b6866";
  repo = builtins.fetchGit { inherit url ref rev; };
in
  repo
