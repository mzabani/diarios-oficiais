{ reflex-platform ? import ./nix/reflex-platform.git.nix {} }:
let
  pkgs = import ./nix/nixpkgs.nix {};
  nixpkgs = pkgs;

  extraBuildInputs = [
    pkgs.postgresql_11
    pkgs.xpdf
    pkgs.automake
    pkgs.docker
  ];

  reflexProj = reflex-platform.project (reflexAttrs: 
    let
      reflexPkgs = reflexAttrs.pkgs;
    in
    {
    name = "diarios-oficiais-site";

    packages = {
      brdocs = ./brdocs;
      diarios-oficiais = ./diarios-oficiais;
      common = ./common;
      backend = ./backend;
      frontend = ./frontend;
    };

    overrides = self: super: {
      # Nosso nixpkgs está com os pacotes que precisamos já definidos através de overlays, mas referenciá-los
      # aqui não funciona direito por algum motivo..
      beam-migrate =  reflexPkgs.haskell.lib.doJailbreak (self.callPackage ./nix/haskell/beam-migrate.nix {});
      beam-postgres = reflexPkgs.haskell.lib.doJailbreak (reflexPkgs.haskell.lib.dontCheck (self.callPackage ./nix/haskell/beam-postgres.nix {}));
      beam-core =     reflexPkgs.haskell.lib.doJailbreak (self.callPackage ./nix/haskell/beam-core.nix {});
    };

    shells = {
      ghc = ["common" "backend" "frontend" "brdocs" "diarios-oficiais" ];
      ghcjs = ["common" "frontend"];
    };
  });
in
  {
    ghcjs = reflexProj.ghcjs;

    ghc = reflexProj.ghc;
    
    shells = {
      ghcjs = reflexProj.shells.ghcjs;

      ghc = reflexProj.shells.ghc.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ extraBuildInputs;

        shellHook = ''
          export PGPORT=5433 # 5432 pode estar sendo usada por algum postgres local
          export PGDATABASE="diariosoficiais"
          export PGHOST="127.0.0.1"
          export PGUSER="diariosapp"

          if [ "$(ls -A postgres-datadir)" ]; then
            echo Postgres datadir não-vazio. Considerando-o inicializado
          else
            initdb --locale=en_US.UTF8 -E UTF8 -U postgres -d postgres-datadir
          fi

          # TODO: Detectar se postgres se está rodando de forma mais inteligente (com pg_ctl, por exemplo)
          if [ ! -f postgres-datadir/postmaster.pid ]; then
                  pg_ctl start -D postgres-datadir -o "-k /tmp/ -e"
          else
            echo Postgres já iniciado
          fi

          echo Postgres rodando na porta $PGPORT. As variáveis de ambiente para usar o psql já foram definidas.
        '';
      });
    };
  }