{ env-file }:
let
  pkgs = import ../nixpkgs.nix {};
  env = import ../../default.nix { inherit env-file; };

  useradd = "${pkgs.shadow}/bin/useradd";

in pkgs.dockerTools.buildImage {
  name = "diarios-oficiais-backend";
  tag = "latest";

  contents = [ pkgs.coreutils pkgs.iputils pkgs.bash env.ghc.backend ];
  runAsRoot = ''
    export PATH="/bin/"
    ${pkgs.dockerTools.shadowSetup}
    ${useradd} -m -U diarios-backend
    mkdir /tmp
    chmod a+rwx /tmp
  '';

  config = {
    Cmd = "/bin/backend";
    User = "diarios-backend:diarios-backend";
  };
}
