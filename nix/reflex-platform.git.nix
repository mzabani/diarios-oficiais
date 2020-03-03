{ bootstrap ? import <nixpkgs> {} }:
let
  # Esse é o reflex-platform v0.5.1.0
  reflex-platform = bootstrap.fetchFromGitHub {
    owner = "reflex-frp";
    repo  = "reflex-platform";
    rev = "5429278830e1555a577f2550e045ce7f7164aa65";
    sha256 = "1lp86cgccmim573rarsjny5vh0ygkfp5afq7006li0k9w2sw2d4c";
  };
in 
  import reflex-platform {}