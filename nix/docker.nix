let
  pkgs = import ./nixpkgs.nix {};
  env = import ../default.nix {};
  lnl7Image = pkgs.dockerTools.pullImage {
    imageName = "lnl7/nix";
    imageDigest = "sha256:068140dbeb7cf8349f789ef2f547bf06c33227dd5c2b3cd9db238d5f57a1fb6a";
    sha256 = "1yjqdcb1ipa978693282cnr24sbk0yihm23xmg68k7ymah7ka9g5";
    finalImageName = "lnl7/nix";
    finalImageTag = "2.2.2";
  };

  postgres-datadir = "";

  init-db = import ./init-env.nix { postgres = pkgs.postgresql_12; rootFolder = "/"; inherit pkgs; };

in pkgs.dockerTools.buildImage {
  name = "diarios-oficiais";
  tag = "latest";

  # fromImage = lnl7Image;

  contents = [ pkgs.coreutils ];
  runAsRoot = ''
    export PATH="/bin/"
  '';

  config = {
    # Cmd = [ "${init-db}/bin/init-db" "/bin/backend" ];
    Cmd = [ "${env.ghc.backend}/bin/backend" ];
    ExposedPorts = {
      "8080/tcp" = {};
    };
    WorkingDir = "/";
    # Volumes = {
    #   "/postgres-datadir" = {};
    # };
  };
}
