# Instruções

1. Instale o Nix.
2. Faça `git clone` e digite `nix-shell -A shells.ghc`
3. Rode `make db-update`. Rode este comando sempre que fizer `git pull` para automaticamente aplicar novas migrações SQL
4. Pronto. `make build-frontend ghcid-backend` e acesse 'http://localhost:8080/index.html' para testar