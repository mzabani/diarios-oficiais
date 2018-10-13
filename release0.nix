let
  config = {
   packageOverrides = pkgs: rec {
     haskellPackages = pkgs.haskellPackages.override {
       overrides = haskellPackagesNew: haskellPackagesOld: rec {
#         heist = haskellPackagesNew.callPackage haskellPackagesOld.heist { doCheck = false; };
         beam-migrate = haskellPackagesNew.callPackage ../beam/beam-migrate/cabal.nix { };
         beam-core = haskellPackagesNew.callPackage ../beam/beam-core/cabal.nix { };
         beam-postgres = haskellPackagesNew.callPackage ../beam/beam-postgres/cabal.nix { };
         brdocs = haskellPackagesNew.callCabal2nix "brdocs" ../brdocs {};
         concursos-publicos = haskellPackagesNew.callCabal2nix "concursos-publicos" ../concursos-publicos {};
	 concursos-publicos-site = haskellPackagesNew.callCabal2nix "concursos-publicos-site" ./. {};
       };
     };
   };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  pkgs.haskellPackages.concursos-publicos-site
