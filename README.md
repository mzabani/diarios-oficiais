[![Build Status](https://travis-ci.com/mzabani/diarios-oficiais.svg?branch=master)](https://travis-ci.com/mzabani/diarios-oficiais)

# Instruções

Não precisa estar logado como `root` para esta nem nenhuma das etapas abaixo.

1. Instale o Nix através do gerenciador de pacotes da sua distribuição ou executando `make setup-nix`. Siga as instruções após executar este comando para terminar a instalação. 
2. Rode `make setup-cachix` se você não tem um cachix bem recente instalado.  
3. Faça `git clone https://github.com/mzabani/diarios-oficiais.git` e digite `make shell` dentro do repositório para entrar numa shell com todas as dependências necessárias instaladas. Este processo pode demorar **quase uma hora** dependendo de sua conexão com a Internet e você vai precisar de muitos GB livre (talvez até 15GB) em disco por conta de todas as dependências. Nas próximas vezes que executar `make shell` entrará na shell em poucos segundos. Faça isso sempre que for desenvolver.
4. Rode `make db-update`. Rode este comando sempre que fizer `git pull` para automaticamente aplicar novas migrações SQL
5. Digite `make fetch` para baixar diários oficiais (se não não haverá nada para buscar). Pode cancelar o processo com Ctrl+C a hora que quiser se não quiser esperar muito; nenhum diário baixado até lá será perdido.
6. Pronto. Rode `make nix-build-frontend; cabal new-build backend; cabal new-run backend` e acesse 'https://localhost:8080/index.html' para testar

### Simulando ambiente de Produção

Imagens docker são produzidas para Deploy. Você pode executar tudo localmente de forma muito similar a como é feito no ambiente
de produção. Para isso, faça:

1. `make docker-all` para construir todas as imagens Docker e carregá-las para dentro do docker automaticamente.
2. `make nix-build-frontend run-production` para gerar o HTML e Javascript e iniciar todos os contêineres em seguida.
3. Acesse `https://localhost:8080` no Browser e ignore os riscos de segurança. Eles só existem pois o certificado é auto-gerado.
4. Abra o arquivo `index.html` localizado em `/results/frontend/bin/frontend.jsexe/index.html` no seu Browser (esta etapa simula porcamente o hosting do HTML no github.io, que é como é feito atualmente).

### Treinar algoritmo de Machine Learning

NOTA: Ainda não é feito Machine Learning de forma minimamente razoável. Há muito código engessado e eu ainda não arrumei tempo
para fazer algo decente. Por enquanto só é possível ver quanto o algoritmo atual acerta, mas não dá para mudá-lo dinamicamente
com novos dados. Fora isso o algoritmo atual é um monte de código engessado com algumas heurísticas aparentemente razoáveis.

1. Rode `cabal new-build diarios-fetcher-exe`
2. Escolha um diário na pasta */data/diarios-fetcher/* pelo nome do arquivo (será um hash como por exemplo *treinamento/fa31183804602c61a084ea3af6727e79*). Se não tiver nenhum, rode `make fetch` e aguarde alguns diários serem baixados.
3. Rode `cabal new-exec diarios-fetcher-exe -- treinar fa31183804602c61a084ea3af6727e79`
4. O pdf será convertido e o programa perguntará, para cada bloco, se aquele bloco (um bloco é aproximadamente uma linha do PDF) pertence ao mesmo parágrafo do último bloco analisado, ou se é cabeçalho.  
