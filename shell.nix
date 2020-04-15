{ env-file ? ./env/dev/docker.env }:
(import ./default.nix { inherit env-file; }).shells.ghc