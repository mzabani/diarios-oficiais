let
  pkgs = import ./pkgs-from-json.nix { json = ./nixos-unstable.json; };
  env = import ./default.nix {};
  lnl7Image = pkgs.dockerTools.pullImage {
    imageName = "lnl7/nix";
    imageDigest = "sha256:068140dbeb7cf8349f789ef2f547bf06c33227dd5c2b3cd9db238d5f57a1fb6a";
    sha256 = "1yjqdcb1ipa978693282cnr24sbk0yihm23xmg68k7ymah7ka9g5";
    finalImageName = "lnl7/nix";
    finalImageTag = "2.2.2";
  };
in pkgs.dockerTools.buildImage {
  name = "diarios-oficiais";
  tag = "latest";

  fromImage = lnl7Image;

  contents = [ pkgs.hello ];
  # runAsRoot = ''
  #   #!${pkgs.stdenv.shell}
  #   echo Iniciando backend do buscador de diários
  #   mkdir -p /data
  # '';

  config = {
    Cmd = [ "${env.shells.ghc}" ];
    ExposedPorts = {
      "8080/tcp" = {};
    };
    # WorkingDir = "/data";
    # Volumes = {
    #   "/data" = {};
    # };
  };
}
