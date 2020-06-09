{ reflex-platform ? import ./nix/reflex-platform.nix }:
let
  pkgs = import ./nix/nixpkgs.nix;
  utils = import ./nix/utils.nix {};
  pdftohtml = import ./nix/packages/pdftohtml.nix {};
  nixpkgs = pkgs;

  shellPkgs = [
    pdftohtml
    pkgs.automake
    pkgs.docker-compose
    pkgs.gnused
    pkgs.certbot
    pkgs.pebble
    pkgs.procps
    pkgs.openssh
    pkgs.cabal2nix
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
      servant-reflex = ./servant-reflex;
    };

    overrides = self: super: {
      beam-migrate =  reflexPkgs.haskell.lib.doJailbreak (self.callPackage ./nix/haskell/beam-migrate.nix {});
      beam-postgres = reflexPkgs.haskell.lib.doJailbreak (reflexPkgs.haskell.lib.dontCheck (self.callPackage ./nix/haskell/beam-postgres.nix {}));
      beam-core =     reflexPkgs.haskell.lib.doJailbreak (self.callPackage ./nix/haskell/beam-core.nix {});
      postgresql-query = reflexPkgs.haskell.lib.dontCheck (reflexPkgs.haskell.lib.markUnbroken super.postgresql-query);
      tmp-postgres = reflexPkgs.haskell.lib.dontCheck (self.callHackage "tmp-postgres" "1.34.1.0" {});
    };

    shells = {
      ghc = ["common" "backend" "frontend" "brdocs" "diarios-fetcher" "servant-reflex" ];
      ghcjs = ["common" "frontend" "servant-reflex" ];
    };
  });
in
  rec {
    ghcjs = reflexProj.ghcjs;

    ghc = reflexProj.ghc;

    ghc-static = builtins.mapAttrs (k: v: pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.lib.dontCheck v)) ghc;
    
    shells = {
      ghcjs = reflexProj.shells.ghcjs;

      ghc = reflexProj.shells.ghc.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ shellPkgs;

        shellHook = ''
        source scripts/source-env.sh ./env/dev/docker.env
        source scripts/source-env.sh ./env/local.env
        ${postgres-service}/bin/init-postgres
        
        # Only run if pebble isn't running yet
        mkdir -p env/dev/data/pebble
        mkdir -p env/dev/data/backend-static-files
        (pgrep pebble > /dev/null) || (pebble -config env/dev/pebble/pebble-config.json >env/dev/data/pebble/pebble-stdout.log 2>&1 &)
        '';
      });
    };
  }