{ env-file, sql-migrations-folder }:
let
  pkgs = import ../nixpkgs.nix {};
  env = import ../../default.nix { inherit env-file; };
  utils = import ../utils.nix {};

  useradd = "${pkgs.shadow}/bin/useradd";

  db-aplicar-migracao = utils.writeShellScriptInBinFolder "db-aplicar-migracao" (builtins.readFile ../../scripts/aplicar-migracao.sh);
  db-bootstrap = utils.writeShellScriptInBinFolder "bootstrap-db" (builtins.readFile ../../scripts/bootstrap-db.sh);
  db-migrar-tudo = utils.writeShellScriptInBinFolder "db-migrar-tudo" ''
    # Isso aqui e uma cópia barata do target db-update do Makefile.. melhorar essa situação, que está precária..
    /bin/bootstrap-db
    # TODO: Aplicar todas as migrações numa única transação (aplicação pós-deploy precisa de todas ou o deploy deve falhar)
    find /home/db-history-update/sql-migrations -name '*.sql' | sort | xargs -L 1 /bin/db-aplicar-migracao
  '';

in pkgs.dockerTools.buildImage {
  name = "db-history-update";
  tag = "latest";

  contents = [ pkgs.coreutils pkgs.iputils pkgs.bash pkgs.findutils pkgs.postgresql_12 db-bootstrap db-aplicar-migracao db-migrar-tudo ];
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    export PATH="/bin/"
    ${pkgs.dockerTools.shadowSetup}
    ${useradd} -m -U db-history-update
    mkdir /tmp
    chmod a+rwx /tmp
    mkdir /home/db-history-update/sql-migrations
    cp ${sql-migrations-folder}/* /home/db-history-update/sql-migrations/
  '';

  config = {
    Cmd = "/bin/db-migrar-tudo";
    User = "db-history-update:db-history-update";
    Volumes = {
      "/home/db-history-update/sql-migrations" = {};
    };
  };
}
