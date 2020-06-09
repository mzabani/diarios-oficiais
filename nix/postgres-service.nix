{ postgres, pkgs, runInBackground }:
    let
        utils = import ./utils.nix {};
        
        ls = "${pkgs.coreutils}/bin/ls";
        echo = "${pkgs.coreutils}/bin/echo";
        cat = "${pkgs.coreutils}/bin/cat";
        pg_hba_conf = ../conf/postgresql/pg_hba.conf;
        postgresql_conf = ../conf/postgresql/postgresql.conf;
    in
    utils.writeShellScriptInBinFolder "init-postgres" ''
if [ "$(${ls} -A $PGDATA/*)" ]; then
    ${echo} Postgres datadir not empty. Considering it initialized.
else
    ${postgres}/bin/initdb --locale=C.UTF8 -E UTF8 -U postgres $PGDATA
fi

set +e
${postgres}/bin/pg_ctl status -D $PGDATA -p ${postgres}/bin/postgres
PGCTLSTATUS=$?
set -e

if [ "$PGCTLSTATUS" -eq "0" ]; then
    ${echo} Postgres already initialized.
else
    ${cat} ${pg_hba_conf} > $PGDATA/pg_hba.conf
    ${cat} ${postgresql_conf} > $PGDATA/postgresql.conf
    ${if runInBackground then "${postgres}/bin/postgres 2>/dev/null &" else "exec ${postgres}/bin/postgres"}
fi
''