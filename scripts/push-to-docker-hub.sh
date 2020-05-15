#!/usr/bin/env bash

# First the backend, which will run SQL migrations and must continue working.
# The fetcher might break after migrations run, but for now, we'll cope with that
docker login
docker tag diarios-oficiais-backend:latest mzabani/diarios-oficiais-backend:latest
docker push mzabani/diarios-oficiais-backend:latest
docker tag postgresql:latest mzabani/postgresql:latest 
docker push mzabani/postgresql:latest
docker tag diarios-fetcher:latest mzabani/diarios-fetcher:latest 
docker push mzabani/diarios-fetcher:latest