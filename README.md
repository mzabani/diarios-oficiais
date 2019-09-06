# Instruções

1. Instale o Nix.
2. Faça `git clone` e digite `make shell` para entrar numa shell com todas as dependências necessárias instaladas. Este processo pode demorar **várias horas**.
   - Se algum erro ocorrer neste processo, por favor reporte-o como um bug.
3. Rode `make db-update`. Rode este comando sempre que fizer `git pull` para automaticamente aplicar novas migrações SQL
4. Pronto. Rode `make build-frontend ghcid-backend` e acesse 'http://localhost:8080/index.html' para testar
