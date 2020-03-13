{ systemPkgs ? import <nixpkgs> {} }:
let
    repo = systemPkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs-channels";
        rev = "7399c59c4150635b896470078c5f5d18d702061a";
        sha256 = "0vqnfh99358v9ym5z9i3dsfy0l4xxgh9hr278pi1y11gdl092014";
    };
    config = {
            # xpdf tem vulnerabilidades, mas nós não nos importamos com isso no momento
            permittedInsecurePackages = [
                "xpdf-4.02"
            ];
        };
    beamOverlay = self: super: {
        haskellPackages = super.haskellPackages.override {
            overrides = hn: ho: {
                # Os pacotes que fazemos override aqui não parecem estar disponíveis dentro de reflex-platform.project,
                # e parecem ser ignorados mesmo se referenciados lá.. não sei por quê!
                beam-migrate = super.haskell.lib.doJailbreak (hn.callPackage ./haskell/beam-migrate.nix {});
                beam-postgres = super.haskell.lib.doJailbreak (super.haskell.lib.dontCheck (hn.callPackage ./haskell/beam-postgres.nix {}));
                beam-core = super.haskell.lib.doJailbreak (hn.callPackage ./haskell/beam-core.nix {}); # pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.beam-core;
                # regex = pkgs.haskellPackages.regex;
                # attoparsec-time-1 = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.attoparsec-time-1;
                # beam-migrate = pkgs.haskell.lib.dontCheck (self.callCabal2nix "beam-migrate" "${beamSrc}/beam-migrate" {});
                # beam-postgres = pkgs.haskell.lib.dontCheck (self.callCabal2nix "beam-postgres" "${beamSrc}/beam-postgres" {});
                # beam-core = pkgs.haskell.lib.dontCheck (self.callCabal2nix "beam-core" "${beamSrc}/beam-core" {});
                # dependent-map = pkgs.haskellPackages.dependent-map;
                # dependent-sum = pkgs.haskellPackages.dependent-sum;
            };
        };
    };

in import repo {
    inherit config;
    overlays = [ beamOverlay ];
}