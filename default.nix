{ mkDerivation, base, beam-core, beam-postgres, servant
, servant-server, stdenv, wai, warp
}:
mkDerivation {
  pname = "ConcursosPublicosSite";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base beam-core beam-postgres servant servant-server wai warp
  ];
  homepage = "https://github.com/githubuser/ConcursosPublicosSite#readme";
  license = stdenv.lib.licenses.bsd3;
}
