{ env-file, local-sql-migrations-dir }:
let
  pkgs = import ../nixpkgs.nix;
  env = import ../../default.nix { };
  utils = import ../utils.nix {};
  sql-migrations-dir = utils.readDockerEnv "SQL_MIGRATIONS_DIR" env-file;
  frontend-dir = utils.readDockerEnv "FRONTEND_DIR" env-file;
  frontend = env.ghcjs.frontend;

  useradd = "${pkgs.shadow}/bin/useradd";

  run-backend-with-certbot = utils.writeShellScriptInBinFolder "run-backend-with-certbot" (builtins.readFile ../../scripts/run-backend-with-certbot.sh);
  run-certbot = utils.writeShellScriptInBinFolder "run-certbot" (builtins.readFile ../../scripts/run-certbot.sh);

in pkgs.dockerTools.buildImage {
  name = "diarios-oficiais-backend";
  tag = "latest";

  contents = [ pkgs.coreutils pkgs.iputils pkgs.bash pkgs.procps pkgs.certbot run-backend-with-certbot run-certbot env.ghc-static.backend ];
  runAsRoot = ''
    export PATH="/bin/"
    ${pkgs.dockerTools.shadowSetup}
    ${useradd} -m -U diarios-backend
    mkdir /tmp
    chmod a+rwx /tmp

    mkdir ${sql-migrations-dir}
    cp ${local-sql-migrations-dir}/*.sql ${sql-migrations-dir}
    chown diarios-backend.diarios-backend ${sql-migrations-dir}

    mkdir ${frontend-dir}
    cp ${frontend}/bin/frontend.jsexe/* ${frontend-dir}
    chown diarios-backend.diarios-backend ${frontend-dir}
  '';

  config = {
    Cmd = [ "/bin/run-backend-with-certbot" "/bin/run-certbot" "/bin/backend-exe" ];
    User = "diarios-backend:diarios-backend";
  };
}
