let
  config = {
   packageOverrides = pkgs: rec {
     haskellPackages = pkgs.haskellPackages.override {
       overrides = haskellPackagesNew: haskellPackagesOld: rec {
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