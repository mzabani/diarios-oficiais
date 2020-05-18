{ env-file, local-sql-migrations-dir }:
let
  pkgs = import ../nixpkgs.nix;
  env = import ../../default.nix { };
  utils = import ../utils.nix {};
  pdftohtml = import ../packages/pdftohtml.nix {};
  sql-migrations-dir = utils.readDockerEnv "SQL_MIGRATIONS_DIR" env-file;

  useradd = "${pkgs.shadow}/bin/useradd";

in pkgs.dockerTools.buildImage {
  name = "diarios-fetcher";
  tag = "latest";

  contents = [ pkgs.coreutils pkgs.iputils pkgs.bash pkgs.glibc pkgs.which pkgs.gnugrep pkgs.findutils pdftohtml env.ghc-static.diarios-fetcher ];
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    export PATH="/bin/"
    ${pkgs.dockerTools.shadowSetup}
    ${useradd} -m -U diarios-fetcher
    mkdir /tmp
    chmod a+rwx /tmp

    mkdir ${sql-migrations-dir}
    chown diarios-fetcher.diarios-fetcher ${sql-migrations-dir}
    cp ${local-sql-migrations-dir}/*.sql ${sql-migrations-dir}
  '';

  config = {
    Cmd = "/bin/diarios-fetcher-exe";
    User = "diarios-fetcher:diarios-fetcher";
  };
}
