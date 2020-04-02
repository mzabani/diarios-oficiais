{ postgres, pkgs, runInBackground }:
    let
        writeShellScriptInBinFolder = name: text: pkgs.writeTextFile {
            inherit name;
            executable = true;
            destination = "/bin/${name}";
            text = ''
                #!${pkgs.runtimeShell}
                set -e
                trap "set +e" 0
                ${text}
                set +e
                '';
            checkPhase = ''
                ${pkgs.stdenv.shell} -n $out/bin/${name}
            '';
        };
        
        ls = "${pkgs.coreutils}/bin/ls";
        echo = "${pkgs.coreutils}/bin/echo";
        sleep = "${pkgs.coreutils}/bin/sleep";
        cat = "${pkgs.coreutils}/bin/cat";
        pg_hba_conf = ../conf/postgresql/pg_hba.conf;
        postgresql_conf = ../conf/postgresql/postgresql.conf;
    in
    writeShellScriptInBinFolder "init-postgres" ''
if [ "$(${ls} -A $PGDATA/*)" ]; then
    ${echo} Postgres datadir não-vazio. Considerando-o inicializado
else
    ${postgres}/bin/initdb --locale=C.UTF8 -E UTF8 -U postgres -d $PGDATA
    ${cat} ${pg_hba_conf} > $PGDATA/pg_hba.conf
    ${cat} ${postgresql_conf} > $PGDATA/postgresql.conf
fi

set +e
${postgres}/bin/pg_ctl status -D $PGDATA -p ${postgres}/bin/postgres
PGCTLSTATUS=$?
set -e

if [ "$PGCTLSTATUS" -eq "0" ]; then
    ${echo} Postgres já iniciado
else
    ${postgres}/bin/pg_ctl start -D $PGDATA -p ${postgres}/bin/postgres -o "-k /tmp/ -e -p $PGPORT"
fi

${if runInBackground then ""
    else "trap \"${postgres}/bin/pg_ctl stop -D $PGDATA\" 0; ${sleep} infinity;"}
''