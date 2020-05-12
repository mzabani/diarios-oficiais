let
  pkgs = import ../nixpkgs.nix;

  postgres-service = import ../postgres-service.nix { postgres = pkgs.postgresql_12; runInBackground=false; inherit pkgs; };

  useradd = "${pkgs.shadow}/bin/useradd";

in pkgs.dockerTools.buildImage {
  name = "postgresql";
  tag = "latest";

  # Parece que o postgresql não inicializa sem o bash..
  contents = [ pkgs.bash pkgs.coreutils pkgs.postgresql_12 pkgs.iputils ];
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    # Por algum motivo, sem LD_LIBRARY_PATH setado corretamente, o initdb falha..
    export PATH="/bin/"
    export LD_LIBRARY_PATH="/lib/"
    ${pkgs.dockerTools.shadowSetup}
    ${useradd} -m -U postgres
    mkdir /tmp
    chmod a+rwx /tmp
    echo "hosts:     files mymachines dns myhostname" > /etc/nsswitch.conf
  '';

  config = {
    Cmd = [ "${postgres-service}/bin/init-postgres"];
    WorkingDir = "/";
    User = "postgres:postgres";
    Volumes = {
      "/home/postgres/postgres-datadir" = {};
    };
  };
}
