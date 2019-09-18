# Instruções

1. Instale o Nix.
2. Faça `git clone` e digite `make shell` para entrar numa shell com todas as dependências necessárias instaladas. Este processo pode demorar **várias horas**.
   - Se algum erro ocorrer neste processo, por favor reporte-o como um bug.
3. Rode `make db-update`. Rode este comando sempre que fizer `git pull` para automaticamente aplicar novas migrações SQL
4. Pronto. Rode `make build-frontend ghcid-backend` e acesse 'http://localhost:8080/index.html' para testar


### Treinar algoritmo de Machine Learning
1. Rode `cabal new-build diarios-oficiais-exe`
2. Escolha um diário na pasta *data/diarios-oficiais/* pelo nome do arquivo (será um hash como por exemplo *treinamento/fa31183804602c61a084ea3af6727e79*). Se não tiver nenhum, rode `make fetch` e aguarde alguns diários serem baixados.
3. Rode `cabal new-exec diarios-oficiais-exe -- treinar fa31183804602c61a084ea3af6727e79`
4. O pdf será convertido e o programa perguntará, para cada bloco, se aquele bloco (um bloco é aproximadamente uma linha do PDF) pertence ao mesmo parágrafo do último bloco analisado, ou se é cabeçalho.  