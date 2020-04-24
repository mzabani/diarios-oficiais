{ systemPkgs ? import <nixpkgs> {} }:
let
    repo = systemPkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        # rev = "d07543a98cf316ceee7ae3535dc45a385df6b247";
        rev = "f601ab37c2fb7e5f65989a92df383bcd6942567a";
        # sha256 = "1wwg1h7i3xldq56w5gzd1jxzl742bgqymskjlc2ay9aa4kdqzm3a";
        sha256 = "0ikhcmcc29iiaqjv5r91ncgxny2z67bjzkppd3wr1yx44sv7v69s";
    };
    config = {
            # We don't care about xpdf's vulnerabilities for now..
            permittedInsecurePackages = [
                "xpdf-4.02" "pdftohtml-4.02"
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