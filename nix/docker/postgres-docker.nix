let
  pkgs = import ../nixpkgs.nix {};
  env = import ../../default.nix {};
  
  lnl7Image = pkgs.dockerTools.pullImage {
    imageName = "lnl7/nix";
    imageDigest = "sha256:068140dbeb7cf8349f789ef2f547bf06c33227dd5c2b3cd9db238d5f57a1fb6a";
    sha256 = "1yjqdcb1ipa978693282cnr24sbk0yihm23xmg68k7ymah7ka9g5";
    finalImageName = "lnl7/nix";
    finalImageTag = "2.2.2";
  };

  postgres-service = import ../postgres-service.nix { postgres = pkgs.postgresql_12; pgdatadir = "/postgres-datadir"; inherit pkgs; };

  init-env = import ../init-env.nix { inherit pkgs; };

  sleep = "${pkgs.coreutils}/bin/sleep";

in pkgs.dockerTools.buildImage {
  name = "diarios-oficiais-postgres-service";
  tag = "latest";

  # fromImage = lnl7Image;

  contents = [ pkgs.coreutils ];
  runAsRoot = ''
    export PATH="/bin/"
  '';

  config = {
    Cmd = [ "${init-env}/bin/init-env" "${postgres-service}/bin/init-postgres" "${sleep} infinity"];
    ExposedPorts = {
      "8080/tcp" = {};
    };
    WorkingDir = "/";
    # User = "";
    # Volumes = {
    #   "/postgres-datadir" = {};
    # };
  };
}
