#!/usr/bin/env bash

ssh -i env/prod/secrets/LightsailDefaultKey-us-east-1.pem $1 'mkdir -p env/prod diarios-oficiais postgres-datadir'
scp -i env/prod/secrets/LightsailDefaultKey-us-east-1.pem docker-compose.prod.yaml $1:docker-compose.prod.yaml
scp -i env/prod/secrets/LightsailDefaultKey-us-east-1.pem env/prod/docker.env $1:env/prod/docker.env
ssh -i env/prod/secrets/LightsailDefaultKey-us-east-1.pem $1 'docker-compose -f docker-compose.prod.yaml up -d watchtower'