setup-cachix:
	@echo "Isso irá instalar (e sobrepor se você já tiver instalado) o cachix para o seu usuário, além de configurá-lo para usar o mzabani.cachix.org"
	nix-env -iA cachix -f nix/nixpkgs.nix
	cachix use mzabani

shell:
	@rm -f .ghc.environment.*
	nix-shell -A shells.ghc

.PHONY: build-backend
build-backend:
	nix-build -o results/backend -A ghc.backend

.PHONY: build-frontend
build-frontend:
	nix-build -o results/frontend -A ghcjs.frontend

.PHONY: build-diarios-fetcher
build-diarios-fetcher:
	nix-build -o results/diarios-fetcher -A ghc.diarios-fetcher

.PHONY: docker
docker:
	nix-build -o results/docker nix/docker.nix
	@echo "Imagem Docker do backend criada em ./results/docker. Esta imagem inicializa o Fetcher de Diários e o backend do Buscador Web"

run-docker-postgres:
	echo "É necessário fazer 'nix-build -o results/postgres-docker; docker load -i results/postgres-docker;' antes"
	docker run -p 5433:5433 --mount type=bind,source=/home/mzabani/Projects/diarios-oficiais/postgres-datadir,destination=/postgres-datadir -u $(id u) diarios-oficiais-postgres-service

build-all: build-backend build-frontend build-diarios-fetcher docker

.PHONY: ghcid-frontend
ghcid-frontend:
	ghcid -W -c "cabal new-repl frontend --disable-optimization" -T Main.main

.PHONY: ghcid-backend
ghcid-backend:
	ghcid -W -c "cabal new-repl backend --disable-optimization" -T Main.main

.PHONY: ghcid-fetcher
ghcid-fetcher:
	ghcid -W -c "cabal new-repl diarios-fetcher --disable-optimization"

hoogle:
	hoogle server --local --port=8000 2>/dev/null 1>/dev/null &
	xdg-open http://localhost:8000/

fetch:
	cabal new-run diarios-fetcher-exe -- +RTS -M4096m -RTS fetch

profile-diarios:
	cabal new-build --enable-profiling diarios-fetcher-exe
	cabal new-run diarios-fetcher-exe -- +RTS -4096m -p -T -RTS fetch

db-restart:
	dropdb -U postgres --if-exists ${PGDATABASE}
	make db-update

db-update:
	./scripts/bootstrap-db.sh
	# TODO: Aplicar todas as migrações numa única transação (aplicação pós-deploy precisa de todas ou o deploy deve falhar)
	find db-history -name '*.sql' | sort | xargs -L 1 ./scripts/aplicar-migracao.sh