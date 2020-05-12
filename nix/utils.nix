{ pkgs ? import ./nixpkgs.nix }:
    {
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

        # Esse leitor de dockerEnv certamente não está de acordo com a especificação. Cuidado!
        readDockerEnv = name: file:
            with pkgs.lib;
            let
                env = builtins.readFile file;
                lista0Ou1 = builtins.filter (l: builtins.length l >= 2 && builtins.elemAt l 0 == name) (builtins.map (splitString "=") (builtins.filter (s: ! hasPrefix "#" s) (splitString "\n" env)));
                valor = if builtins.length lista0Ou1 == 0 then "" else builtins.concatStringsSep "=" (builtins.tail (builtins.head lista0Ou1));
            in
                if hasPrefix "\"" valor && hasSuffix "\"" valor
                    then removePrefix "\"" (removeSuffix "\"" valor)
                    else valor;
    }