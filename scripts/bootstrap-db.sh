#!/bin/sh

set -e

echo "SELECT 'CREATE DATABASE ${PGDATABASE};' WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = '${PGDATABASE}')\gexec" | psql -U postgres postgres
echo "SELECT 'CREATE USER ${PGUSER};' WHERE NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = '${PGUSER}')\gexec" | psql -U postgres postgres
echo "GRANT CONNECT ON DATABASE ${PGDATABASE} TO ${PGUSER};" | psql -U postgres postgres
echo "CREATE TABLE IF NOT EXISTS migracoes_sql (
    aplicada_em timestamptz not null default now()
    , nome_arquivo text not null
    , hash_arquivo text not null
    , unique (nome_arquivo)
);

ALTER DEFAULT PRIVILEGES FOR USER ${PGUSER} IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE, REFERENCES, TRIGGER ON TABLES TO ${PGUSER};
ALTER DEFAULT PRIVILEGES FOR USER ${PGUSER} IN SCHEMA public GRANT USAGE, SELECT, UPDATE ON SEQUENCES TO ${PGUSER};
ALTER DEFAULT PRIVILEGES FOR USER ${PGUSER} IN SCHEMA public GRANT EXECUTE ON ROUTINES TO ${PGUSER};
" | psql -U postgres -q $PGDATABASE