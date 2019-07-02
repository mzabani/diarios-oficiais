let
  basePkgs = import ./pkgs-from-json.nix { json = ./nixos-unstable.json; };

  # Essa versão do Beam possui funções regexp do Postgres.. mas fica pra outra hora
  beamSrc = basePkgs.fetchFromGitHub {
    owner = "tathougies";
    repo = "beam";
    rev = "24851f0b1203e329a484aeb3503c5372162efabc";
    sha256 = "1zsbi2zamlmfqwgi0jx1j9hayvryvah7i4w1g49nnzblfakysjiy";
  };
in
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  name = "concursos-publicos-site";

  packages = {
    brdocs = ../brdocs;
    concursos-publicos = ../concursos-publicos;
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  overrides = self: super: {
    beam-migrate = basePkgs.haskellPackages.beam-migrate;
    beam-postgres = basePkgs.haskellPackages.beam-postgres;
    beam-core = basePkgs.haskellPackages.beam-core;
    # beam-migrate = basePkgs.haskell.lib.dontCheck (self.callCabal2nix "beam-migrate" "${beamSrc}/beam-migrate" {});
    # beam-postgres = basePkgs.haskell.lib.dontCheck (self.callCabal2nix "beam-postgres" "${beamSrc}/beam-postgres" {});
    # beam-core = basePkgs.haskell.lib.dontCheck (self.callCabal2nix "beam-core" "${beamSrc}/beam-core" {});
    # dependent-map = basePkgs.haskellPackages.dependent-map;
    # dependent-sum = basePkgs.haskellPackages.dependent-sum;
  };

  shells = {
    ghc = ["common" "backend" "frontend" "brdocs" "concursos-publicos" ];
    ghcjs = ["common" "frontend"];
  };
})

