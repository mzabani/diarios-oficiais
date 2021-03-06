NIXBUILD_DOCKER:=nix-build --arg env-file $(if $(DOCKER_BUILD_ENV_FILE),$(DOCKER_BUILD_ENV_FILE),./env/prod/docker.env) --arg local-sql-migrations-dir $(shell ./scripts/get-env.sh LOCAL_SQL_MIGRATIONS_DIR ./env/local.env)

setup-nix:
	@echo "Isso irá instalar o Nix se você ainda não o tiver instalado"
	@(curl --version || echo "Você precisa do curl instalado")
	nix --version || (curl -L --proto '=https' --tlsv1.2 https://nixos.org/nix/install | sh)

setup-cachix:
	@echo "Isso irá instalar o cachix para o seu usuário, além de configurá-lo para usar o mzabani.cachix.org"
	@(cachix --version || nix-env -iA cachix -f https://cachix.org/api/v1/install)
	cachix use mzabani

shell:
	rm -f .ghc.environment.*
	nix-shell

.PHONY: dev-build-frontend
dev-build-frontend:
	nix-build --arg env-file ./env/dev/docker.env -o results/frontend -A ghcjs.frontend
	# nix-shell --arg env-file ./env/dev/docker.env -A shells.ghcjs default.nix --run \
	# 	"cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs build frontend && cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs --installdir=./results/frontend/ install frontend"

.PHONY: test
test:
	cabal test -O0 --test-show-details=streaming all

.PHONY: docker-backend
docker-backend:
	${NIXBUILD_DOCKER} -o results/docker-backend nix/docker/diarios-backend.nix
	docker load -i results/docker-backend

.PHONY: docker-fetcher
docker-fetcher:
	${NIXBUILD_DOCKER} -o results/docker-fetcher nix/docker/diarios-fetcher.nix
	docker load -i results/docker-fetcher

.PHONY: docker-postgresql
docker-postgresql:
	${NIXBUILD_DOCKER} -o results/docker-postgresql nix/docker/postgresql.nix
	docker load -i results/docker-postgresql

.PHONY: docker-all
docker-all: docker-backend docker-fetcher docker-postgresql

.PHONY: nix-build-frontend
nix-build-frontend:
	nix-build --arg env-file ./env/prod/docker.env -o results/frontend -A ghcjs.frontend

.PHONY: nix-build-backend
nix-build-backend:
	nix-build --arg env-file ./env/prod/docker.env -o results/backend -A ghc-static.backend

.PHONY: nix-build-diarios-fetcher
nix-build-diarios-fetcher:
	nix-build --arg env-file ./env/prod/docker.env -o results/diarios-fetcher -A ghc-static.diarios-fetcher

.PHONY: run-certbot
run-certbot:
	./scripts/run-certbot.sh

.PHONY: simul-prod
simul-prod:
	docker-compose -f docker-compose.simul-prod.yaml down
	pg_ctl stop || true
	pkill -x pebble || true
	docker-compose -f docker-compose.simul-prod.yaml up

hoogle:
	pkill -x hoogle || true
	hoogle server --local --port=3000 2>/dev/null 1>/dev/null &
	xdg-open http://localhost:3000/ 2>/dev/null 1>/dev/null &

profile-diarios:
	cabal build --enable-profiling diarios-fetcher-exe
	cabal run diarios-fetcher-exe -- +RTS -4096m -p -T -RTS fetch 2019-01-02 2019-01-03