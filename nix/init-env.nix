{ pkgs }:
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
    in
    writeShellScriptInBinFolder "init-env" ''
export PGPORT=5433 # 5432 pode estar sendo usada por algum postgres local
export PGDATABASE="diariosoficiais"
export PGHOST="127.0.0.1"
export PGUSER="postgres"
export PGAPPUSER="diariosapp"
''