{ postgres, pkgs, pgdatadir }:
    let
        writeShellScriptInBinFolder = name: text: pkgs.writeTextFile {
            inherit name;
            executable = true;
            destination = "/bin/${name}";
            text = ''
                #!${pkgs.runtimeShell}
                set -e
                ${text}
                '';
            checkPhase = ''
                ${pkgs.stdenv.shell} -n $out/bin/${name}
            '';
        };
        
        ls = "${pkgs.coreutils}/bin/ls";
        echo = "${pkgs.coreutils}/bin/echo";
    in
    writeShellScriptInBinFolder "init-postgres" ''
if [ "$(${ls} -A ${pgdatadir})" ]; then
    ${echo} Postgres datadir não-vazio. Considerando-o inicializado
else
    ${postgres}/bin/initdb --locale=en_US.UTF8 -E UTF8 -U postgres -d ${pgdatadir}
fi

# TODO: Detectar se postgres se está rodando de forma mais inteligente (com pg_ctl, por exemplo)
if [ ! -f ${pgdatadir}/postmaster.pid ]; then
    ${postgres}/bin/pg_ctl start -D ${pgdatadir} -p ${postgres}/bin/postgres -o "-k /tmp/ -e -p $PGPORT"
else
    ${echo} Postgres já iniciado
fi
''