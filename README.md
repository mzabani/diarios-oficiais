[![Build Status](https://travis-ci.com/mzabani/diarios-oficiais.svg?branch=master)](https://travis-ci.com/mzabani/diarios-oficiais)

# Instruções

1. Instale o Nix através do gerenciador de pacotes da sua distribuição ou executando `bash <(curl https://nixos.org/nix/install)`. Não precisa estar como `root` para esta nem nenhuma das etapas.
2. Rode `make setup-cachix`. 
3. Faça `git clone https://github.com/mzabani/diarios-oficiais.git` e digite `make shell` dentro do repositório para entrar numa shell com todas as dependências necessárias instaladas. Este processo pode demorar **quase uma hora** dependendo de sua conexão com a Internet e você vai precisar de muitos GB livre (talvez até 15GB) em disco por conta de todas as dependências. As próximas vezes que executar `make shell` entrará na shell em poucos segundos. Faça isso sempre que for desenvolver.
4. Rode `make db-update`. Rode este comando sempre que fizer `git pull` para automaticamente aplicar novas migrações SQL
5. Digite `make fetch` para baixar diários oficiais (se não não haverá nada para buscar). Pode cancelar o processo com Ctrl+C a hora que quiser se não quiser esperar muito; nenhum diário baixado até lá será perdido.
6. Pronto. Rode `make build-frontend ghcid-backend` e acesse 'http://localhost:8080/index.html' para testar

### Treinar algoritmo de Machine Learning

NOTA: Ainda não é feito Machine Learning de forma minimamente razoável. Há muito código engessado e eu ainda não arrumei tempo
para fazer algo decente. Por enquanto só é possível ver quanto o algoritmo atual acerta, mas não dá para mudá-lo dinamicamente
com novos dados. Fora isso o algoritmo atual é um monte de código engessado com algumas heurísticas aparentemente razoáveis.

1. Rode `cabal new-build diarios-oficiais-exe`
2. Escolha um diário na pasta */data/diarios-oficiais/* pelo nome do arquivo (será um hash como por exemplo *treinamento/fa31183804602c61a084ea3af6727e79*). Se não tiver nenhum, rode `make fetch` e aguarde alguns diários serem baixados.
3. Rode `cabal new-exec diarios-oficiais-exe -- treinar fa31183804602c61a084ea3af6727e79`
4. O pdf será convertido e o programa perguntará, para cada bloco, se aquele bloco (um bloco é aproximadamente uma linha do PDF) pertence ao mesmo parágrafo do último bloco analisado, ou se é cabeçalho.  