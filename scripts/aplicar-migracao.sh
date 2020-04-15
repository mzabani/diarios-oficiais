#!/usr/bin/env bash

set -e

NOME="$(basename $1)"
HASH_MIGRACAO="$(sha512sum $1 | cut -f 1 -d ' ')"
JA_APLICADA=$(echo "SELECT 1 FROM migracoes_sql WHERE nome_arquivo='${NOME}';" | psql -U postgres -qt $PGDATABASE)
if [ -z "$JA_APLICADA" ]; then
    echo "Aplicando migração SQL $NOME com hash $HASH_MIGRACAO"
    (echo "BEGIN;"; cat $1; printf "\n"; echo "INSERT INTO migracoes_sql (nome_arquivo, hash_arquivo) VALUES ('$NOME', '${HASH_MIGRACAO}'); COMMIT;") | psql -U postgres $PGDATABASE
else
    echo "------------------- Migraçao $1 já aplicada"
fi