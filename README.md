# Instructions

### 1. Entering the development shell

1. Run `git clone --recursive https://github.com/mzabani/diarios-oficiais.git` and enter the created directory by running `cd diarios-oficiais`.
2. Install Nix through your Distro's package manager or by running `make setup-nix`. Follow the instructions to finish the installation.
3. Run `make setup-cachix`. This will install `cachix` unless you already have it and set up `mzabani.cachix.org` as a Cache server.
4. Run `make shell` inside the repo. This will fetch every required dependency for the full development environment and so can take quite a while depending on your Internet connection. You'll also need lots of disk space (up to 15GB).
5. Now you won't need to run steps 1-3 any more. Always type `make shell` to enter the development shell, and next times it'll be just a few seconds to do so.

### 2. Running the application

1. Run `make fetch` to download the Diários Oficiais (Official Journals) or else there won't be anything to search for. You can stop fetching by typing Ctrl+C any time; no fetched journals will be lost when you do this.
2. Run `make dev-build-frontend; cabal run backend`.
3. Only if this is the very first time you typed `cabal run backend`, a TLS certificate will need to be acquired. Run `make run-certbot` in a separate development shell **without stopping the backend** and wait until it succeeds. Now you can kill the backend and run `cabal run backend` again.
4. Go to 'https://localhost:8083/' to test. Ignore any safety risks; they only exist because the certificate is a "toy" certificate, not signed by a trusted CA.

### 3. Simulating Production locally

Docker images are built for Deployment purposes. You can run it all locally in a very similar fashion to what it's done in Production. In order to do that, follow the instructions below:

1. You'll need Docker configured and running.
2. `make docker-all` to build all Docker images and load them with docker automatically.
3. `make simul-prod` to kill applications which are running locally (such as postgres and pebble) and start all the containers right away.
4. Go to `https://localhost/` in your Browser and ignore safety risks.
5. After you stop `docker-compose`, postgres and pebble won't be started for you. I recommend typing `exit` and `make shell` again to restore the environment.

### 4. Deploying to Production

If you want to run this app in your own instance, you can use some Container Orchestration solution with the produced Docker images, or you can set up a Toy
Production server by (note that this is still a very rough guide, lots of other things need to be set up properly for this to work, such as DNS records for certbot):

1. Edit `/env/prod/docker.env` and change hosts and other info to adapt it to your app.
2. Create a Linux Cloud Compute instance somewhere. It could AWS, GCP, Azure or any other.
3. Install `docker-compose` on that instance and make sure `sshd` is up and running ok. Make sure ports `80` and `443` are open to the Internet.
4. Download the SSH keys necessary to log in and save them to your project's `/env/prod/secrets/LightsailDefaultKey-us-east-1.pem` file.
5. Run `DOCKER_BUILD_ENV_FILE=./env/prod/docker.env make docker-all` to build Docker images for Production.
6. Make sure you're registered at Docker Hub or some other Registry, edit `/scripts/push-to-docker-hub.sh` accordingly and run it.
7. Run `./scripts/prepare-prod-cloud-instance.sh username@yourcloudinstance`. https://yourdomain/ should now be accessible.

### Treinar algoritmo de Machine Learning

NOTA: Ainda não é feito Machine Learning de forma minimamente razoável. Há muito código engessado e eu ainda não arrumei tempo
para fazer algo decente. Por enquanto só é possível ver quanto o algoritmo atual acerta, mas não dá para mudá-lo dinamicamente
com novos dados. Fora isso o algoritmo atual é um monte de código engessado com algumas heurísticas aparentemente razoáveis.

1. Rode `cabal build diarios-fetcher-exe`
2. Escolha um diário na pasta */data/diarios-fetcher/* pelo nome do arquivo (será um hash como por exemplo *treinamento/fa31183804602c61a084ea3af6727e79*). Se não tiver nenhum, rode `make fetch` e aguarde alguns diários serem baixados.
3. Rode `cabal run diarios-fetcher-exe -- treinar fa31183804602c61a084ea3af6727e79`
4. O pdf será convertido e o programa perguntará, para cada bloco, se aquele bloco (um bloco é aproximadamente uma linha do PDF) pertence ao mesmo parágrafo do último bloco analisado, ou se é cabeçalho.  
