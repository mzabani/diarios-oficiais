dev-frontend:
	nix-build -o frontend-result -A ghcjs.frontend

ghcid-frontend:
	ghcid -W -c "cabal new-repl frontend" -T Main.main

ghcid-backend:
	ghcid -W -c "cabal new-repl backend" -T Main.main

hoogle:
	hoogle server --local --port=8080
