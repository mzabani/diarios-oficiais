{ mkDerivation, aeson, base, beam-core, beam-postgres
, http-api-data, resource-pool, servant, servant-server, stdenv
, text, wai, warp
}:
mkDerivation {
  pname = "ConcursosPublicosSite";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base beam-core beam-postgres http-api-data resource-pool
    servant servant-server text wai warp
  ];
  homepage = "https://github.com/githubuser/ConcursosPublicosSite#readme";
  license = stdenv.lib.licenses.bsd3;
}
