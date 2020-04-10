{ env-file ? ./env/development.env }:
(import ./default.nix { inherit env-file; }).shells.ghc