build-frontend:
	nix-build -o frontend-result -A ghcjs.frontend

ghcid-frontend:
	ghcid -W -c "cabal new-repl frontend" -T Main.main

ghcid-backend:
	ghcid -W -c "cabal new-repl backend" -T Main.main

ghcid-diarios:
	ghcid -W -c "cabal new-repl diarios-oficiais-exe"

hoogle:
	hoogle server --local --port=8080

fetch:
	cabal new-run diarios-oficiais-exe -- +RTS -M4096m -RTS fetch

profile-diarios:
	cabal new-build --enable-profiling diarios-oficiais-exe
	cabal new-run diarios-oficiais-exe -- +RTS -4096m -p -T -RTS fetch