{ reflex-platform ? import ./nix/reflex-platform.git {} }:
let
  basePkgs = import ./pkgs-from-json.nix { json = ./nixos-unstable.json; };

  # Essa versão do Beam possui funções regexp do Postgres.. mas fica pra outra hora
  # beamSrc = basePkgs.fetchFromGitHub {
  #   owner = "tathougies";
  #   repo = "beam";
  #   rev = "24851f0b1203e329a484aeb3503c5372162efabc";
  #   sha256 = "1zsbi2zamlmfqwgi0jx1j9hayvryvah7i4w1g49nnzblfakysjiy";
  # };

  extraBuildInputs = [
    basePkgs.postgresql_11
    basePkgs.xpdf
    basePkgs.automake
  ];

  deriv = reflex-platform.project ({ pkgs, ... }: {
    name = "diarios-oficiais-site";

    packages = {
      brdocs = ./brdocs;
      diarios-oficiais = ./diarios-oficiais;
      common = ./common;
      backend = ./backend;
      frontend = ./frontend;
    };

    overrides = self: super: {
      beam-migrate = basePkgs.haskellPackages.beam-migrate;
      beam-postgres = basePkgs.haskellPackages.beam-postgres;
      beam-core = basePkgs.haskellPackages.beam-core;
      regex = basePkgs.haskellPackages.regex;
      # beam-migrate = basePkgs.haskell.lib.dontCheck (self.callCabal2nix "beam-migrate" "${beamSrc}/beam-migrate" {});
      # beam-postgres = basePkgs.haskell.lib.dontCheck (self.callCabal2nix "beam-postgres" "${beamSrc}/beam-postgres" {});
      # beam-core = basePkgs.haskell.lib.dontCheck (self.callCabal2nix "beam-core" "${beamSrc}/beam-core" {});
      # dependent-map = basePkgs.haskellPackages.dependent-map;
      # dependent-sum = basePkgs.haskellPackages.dependent-sum;
    };

    shells = {
      ghc = ["common" "backend" "frontend" "brdocs" "diarios-oficiais" ];
      ghcjs = ["common" "frontend"];
    };
  });
in
  {
    ghcjs = deriv.ghcjs;
    
    shells = {
      ghcjs = deriv.shells.ghcjs;

      ghc = deriv.shells.ghc.overrideAttrs (old: {
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