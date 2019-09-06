shell:
	@rm -f .ghc.environment.*
	nix-shell -A shells.ghc

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

db-restart:
	dropdb -U postgres --if-exists ${PGDATABASE}
	make db-update

db-update:
	./scripts/bootstrap-db.sh
	# TODO: Aplicar todas as migrações numa única transação (aplicação pós-deploy precisa de todas ou o deploy deve falhar)
	find db-history -name '*.sql' | sort | xargs -L 1 ./scripts/aplicar-migracao.sh