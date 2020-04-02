setup-nix:
	@echo "Isso irá instalar o Nix se você ainda não o tiver instalado"
	@(curl --version || echo "Você precisa do curl instalado")
	nix --version || ((curl -L --proto '=https' --tlsv1.2 https://nixos.org/nix/install | sh); . ~/.nix-profile/etc/profile.d/nix.sh)

setup-cachix:
	@echo "Isso irá instalar o cachix para o seu usuário, além de configurá-lo para usar o mzabani.cachix.org"
	@(cachix --version || nix-env -iA cachix -f nix/nixpkgs.nix)
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

.PHONY: docker-backend
docker-backend:
	nix-build -o results/docker-backend nix/docker/diarios-backend.nix
	docker load -i results/docker-backend
	@echo "Imagem Docker do backend criada e carregada. Esta imagem inicializa o backend do Buscador Web"

.PHONY: docker-fetcher
docker-fetcher:
	nix-build -o results/docker-fetcher nix/docker/diarios-fetcher.nix
	docker load -i results/docker-fetcher
	@echo "Imagem Docker do Fetcher de diários criada e carregada"

.PHONY: docker-postgresql
docker-postgresql:
	nix-build -o results/docker-postgresql nix/docker/postgresql.nix
	docker load -i results/docker-postgresql
	@echo "Imagem Docker do serviço postgresql criada e carregada. Esta imagem inicializa o PostgreSQL"

.PHONY: docker-all
docker-all: docker-backend docker-fetcher docker-postgresql

build-all: build-backend build-frontend build-diarios-fetcher

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