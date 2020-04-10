let
  pkgs = import ../nixpkgs.nix {};
  env = import ../../default.nix {};

  useradd = "${pkgs.shadow}/bin/useradd";

in pkgs.dockerTools.buildImage {
  name = "diarios-fetcher";
  tag = "latest";

  contents = [ pkgs.coreutils pkgs.iputils pkgs.bash pkgs.xpdf pkgs.glibc pkgs.which pkgs.gnugrep pkgs.findutils env.ghc.diarios-fetcher ];
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    export PATH="/bin/"
    ${pkgs.dockerTools.shadowSetup}
    ${useradd} -m -U diarios-fetcher
    mkdir /tmp
    chmod a+rwx /tmp
    # Por que ghc está sendo instalado??
    ls /nix/store | grep "\-ghc\-" | xargs rm -rf
  '';

  config = {
    Cmd = "/bin/diarios-fetcher-exe";
    User = "diarios-fetcher:diarios-fetcher";
  };
}
