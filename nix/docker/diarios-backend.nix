{ env-file }:
let
  pkgs = import ../nixpkgs.nix {};
  env = import ../../default.nix { inherit env-file; };
  utils = import ../utils.nix {};

  useradd = "${pkgs.shadow}/bin/useradd";

  run-backend-with-certbot = utils.writeShellScriptInBinFolder "run-backend-with-certbot" (builtins.readFile ../../scripts/run-backend-with-certbot.sh);
  run-certbot = utils.writeShellScriptInBinFolder "run-certbot" (builtins.readFile ../../scripts/run-certbot.sh);

in pkgs.dockerTools.buildImage {
  name = "diarios-oficiais-backend";
  tag = "latest";

  contents = [ pkgs.coreutils pkgs.iputils pkgs.bash pkgs.procps pkgs.certbot run-backend-with-certbot run-certbot env.ghc.backend ];
  runAsRoot = ''
    export PATH="/bin/"
    ${pkgs.dockerTools.shadowSetup}
    ${useradd} -m -U diarios-backend
    mkdir /tmp
    chmod a+rwx /tmp
  '';

  config = {
    Cmd = [ "/bin/run-backend-with-certbot" "/bin/run-certbot" "/bin/backend" ];
    User = "diarios-backend:diarios-backend";
  };
}
