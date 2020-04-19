{ env-file, local-sql-migrations-dir }:
let
  pkgs = import ../nixpkgs.nix {};
  env = import ../../default.nix { inherit env-file; };
  utils = import ../utils.nix {};
  sql-migrations-dir = utils.readDockerEnv "SQL_MIGRATIONS_DIR" env-file;

  useradd = "${pkgs.shadow}/bin/useradd";

in pkgs.dockerTools.buildImage {
  name = "diarios-fetcher";
  tag = "latest";

  contents = [ pkgs.coreutils pkgs.iputils pkgs.bash pkgs.glibc pkgs.which pkgs.gnugrep pkgs.findutils env.ghc.diarios-fetcher ];
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    export PATH="/bin/"
    ${pkgs.dockerTools.shadowSetup}
    ${useradd} -m -U diarios-fetcher
    mkdir /tmp
    chmod a+rwx /tmp
    # Por que ghc está sendo instalado??
    ls /nix/store | grep "\-ghc\-" | xargs rm -rf

    mkdir ${sql-migrations-dir}
    chown diarios-fetcher.diarios-fetcher ${sql-migrations-dir}
    cp ${local-sql-migrations-dir}/*.sql ${sql-migrations-dir}
  '';

  config = {
    Cmd = "/bin/diarios-fetcher-exe";
    User = "diarios-fetcher:diarios-fetcher";
  };
}
