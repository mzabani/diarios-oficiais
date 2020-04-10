{ reflex-platform ? import ./nix/reflex-platform.git.nix {}
, env-file }:
let
  pkgs = import ./nix/nixpkgs.nix {};
  utils = import ./nix/utils.nix {};
  nixpkgs = pkgs;

  extraBuildInputs = [
    pkgs.postgresql_12
    pkgs.xpdf
    pkgs.xdg_utils
    pkgs.automake
    pkgs.docker
    pkgs.docker-compose
    pkgs.docker-machine
    pkgs.evince
    pkgs.gnused
    pkgs.certbot
    pkgs.openssl
  ];

  postgres-service = import ./nix/postgres-service.nix { postgres = pkgs.postgresql_12; runInBackground=true; inherit pkgs; };

  reflexProj = reflex-platform.project (reflexAttrs: 
    let
      reflexPkgs = reflexAttrs.pkgs;
    in
    {
    name = "diarios-oficiais";

    packages = {
      brdocs = ./src/brdocs;
      diarios-fetcher = ./src/diarios-fetcher;
      common = ./src/common;
      backend = ./src/backend;
      frontend = ./src/frontend;
    };

    overrides = self: super: {
      # Nosso nixpkgs está com os pacotes que precisamos já definidos através de overlays, mas referenciá-los
      # aqui não funciona direito por algum motivo..
      beam-migrate =  reflexPkgs.haskell.lib.doJailbreak (self.callPackage ./nix/haskell/beam-migrate.nix {});
      beam-postgres = reflexPkgs.haskell.lib.doJailbreak (reflexPkgs.haskell.lib.dontCheck (self.callPackage ./nix/haskell/beam-postgres.nix {}));
      beam-core =     reflexPkgs.haskell.lib.doJailbreak (self.callPackage ./nix/haskell/beam-core.nix {});
    };

    shells = {
      ghc = ["common" "backend" "frontend" "brdocs" "diarios-fetcher" ];
      ghcjs = ["common" "frontend"];
    };
  });
in
  {
    ghcjs = reflexProj.ghcjs // {
      # O nosso frontend e backend estão em servidores diferentes, e os Requests
      # precisam ir para o endereço certo de acordo com o ambiente desejado (Dev, Produção etc.)
      frontend = reflexProj.ghcjs.frontend.overrideAttrs (old: { BACKENDURL = utils.readDockerEnv "BACKENDURL" env-file; });
    };

    ghc = reflexProj.ghc;
    
    shells = {
      ghcjs = reflexProj.shells.ghcjs;

      ghc = reflexProj.shells.ghc.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ extraBuildInputs;

        shellHook = ''
        source scripts/source-env.sh env/development.env
        ${postgres-service}/bin/init-postgres
        '';
      });
    };
  }