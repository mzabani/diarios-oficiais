setup-cachix:
	@echo "Isso irá instalar (e sobrepor se você já tiver instalado) o cachix para o seu usuário, além de configurá-lo para usar o mzabani.cachix.org"
	nix-env -iA cachix -f nix/nixpkgs.nix
	cachix use mzabani

shell:
	@rm -f .ghc.environment.*
	nix-shell -A shells.ghc

build-backend:
	nix-build -o results/backend -A ghc.backend

build-frontend:
	nix-build -o results/frontend -A ghcjs.frontend

build-diarios-fetcher:
	nix-build -o results/diarios-oficiais -A ghc.diarios-oficiais

docker:
	nix-build -o results/docker nix/docker.nix
	@echo "Imagem Docker do backend criada em ./results/docker. Esta imagem inicializa o Fetcher de Diários e o backend do Buscador Web"

build-all: build-backend build-frontend build-diarios-fetcher docker

ghcid-frontend:
	ghcid -W -c "cabal new-repl frontend" -T Main.main

ghcid-backend:
	ghcid -W -c "cabal new-repl backend" -T Main.main

ghcid-diarios:
	ghcid -W -c "cabal new-repl diarios-oficiais"

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