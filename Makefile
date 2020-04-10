# Por padrão buildamos para Produção, a não ser que haja variável de ambiente definida de acordo
NIXBUILD:=nix-build --arg env-file $(if $(LOCAL_DOCKER_ENV_FILE),$(LOCAL_DOCKER_ENV_FILE),./env/production.env)

setup-nix:
	@echo "Isso irá instalar o Nix se você ainda não o tiver instalado"
	@(curl --version || echo "Você precisa do curl instalado")
	nix --version || (curl -L --proto '=https' --tlsv1.2 https://nixos.org/nix/install | sh)

setup-cachix:
	@echo "Isso irá instalar o cachix para o seu usuário, além de configurá-lo para usar o mzabani.cachix.org"
	@(cachix --version || nix-env -iA cachix -f nix/nixpkgs.nix)
	cachix use mzabani

shell:
	rm -f .ghc.environment.*
	nix-shell

.PHONY: nix-build-backend
nix-build-backend:
	${NIXBUILD} -o results/backend -A ghc.backend

.PHONY: nix-build-frontend
nix-build-frontend:
	${NIXBUILD} -o results/frontend -A ghcjs.frontend

.PHONY: nix-build-diarios-fetcher
nix-build-diarios-fetcher:
	${NIXBUILD} -o results/diarios-fetcher -A ghc.diarios-fetcher

.PHONY: docker-backend
docker-backend:
	${NIXBUILD} -o results/docker-backend nix/docker/diarios-backend.nix
	docker load -i results/docker-backend
	@echo "Imagem Docker do backend criada e carregada. Esta imagem inicializa o backend do Buscador Web"

.PHONY: docker-fetcher
docker-fetcher:
	${NIXBUILD} -o results/docker-fetcher nix/docker/diarios-fetcher.nix
	docker load -i results/docker-fetcher
	@echo "Imagem Docker do Fetcher de diários criada e carregada"

.PHONY: docker-postgresql
docker-postgresql:
	${NIXBUILD} -o results/docker-postgresql nix/docker/postgresql.nix
	docker load -i results/docker-postgresql
	@echo "Imagem Docker do serviço postgresql criada e carregada. Esta imagem inicializa o PostgreSQL"

.PHONY: docker-db-history-update
docker-db-history-update:
	${NIXBUILD} -o results/docker-db-history-update nix/docker/db-history-update.nix
	docker load -i results/docker-db-history-update
	@echo "Imagem Docker do aplicador de migrações do DB criada e carregada."

.PHONY: docker-all
docker-all: docker-backend docker-fetcher docker-postgresql docker-db-history-update

.PHONY: run-production
run-production:
	pg_ctl stop || true
	@echo "Isso irá inicializar as imagens Docker da forma mais parecida possível com o que se faz em Produção"
	docker-compose up || true
	pg_ctl start

build-all: nix-build-backend nix-build-frontend nix-build-diarios-fetcher

.PHONY: ghcid-frontend
ghcid-frontend:
	ghcid -W -c "cabal new-repl frontend --disable-optimization"

.PHONY: ghcid-backend
ghcid-backend:
	ghcid -W -c "cabal new-repl backend --disable-optimization"

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