#!/bin/sh

set -e

NOME="$(basename $1)"
HASH_MIGRACAO="$(sha512sum $1 | cut -f 1 -d ' ')"
JA_APLICADA=$(echo "SELECT 1 FROM migracoes_sql WHERE nome_arquio='${NOME}';" | psql -U postgres -qt $PGDATABASE)
if [ -z "$JA_APLICADA" ]; then
    (echo "BEGIN;"; cat $1; echo "INSERT INTO migracoes_sql (nome_arquivo, hash_arquivo) VALUES ('$NOME', '${HASH_MIGRACAO}'); COMMIT;") | psql -U postgres $PGDATABASE
else
    echo "------------------- Migraçao $1 já aplicada"
fi;