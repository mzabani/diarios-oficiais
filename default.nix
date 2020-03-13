{ reflex-platform ? import ./nix/reflex-platform.git.nix {} }:
let
  pkgs = import ./nix/nixpkgs.nix {};
  nixpkgs = pkgs;

  extraBuildInputs = [
    pkgs.postgresql_12
    pkgs.xpdf
    pkgs.automake
    pkgs.docker
    pkgs.evince
  ];

  init-env = import nix/init-env.nix { inherit pkgs; };
  postgres-service = import nix/postgres-service.nix { postgres = pkgs.postgresql_12; pgdatadir = "./postgres-datadir"; inherit pkgs; };

  reflexProj = reflex-platform.project (reflexAttrs: 
    let
      reflexPkgs = reflexAttrs.pkgs;
    in
    {
    name = "diarios-oficiais";

    packages = {
      brdocs = ./brdocs;
      diarios-fetcher = ./diarios-fetcher;
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
      ghc = ["common" "backend" "frontend" "brdocs" "diarios-fetcher" ];
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
        . ${init-env}/bin/init-env
        ${postgres-service}/bin/init-postgres
        '';
      });
    };
  }