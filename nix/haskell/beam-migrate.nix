{ mkDerivation, aeson, base, beam-core, bytestring, containers
, deepseq, dependent-map, dependent-sum, fetchgit, free, ghc-prim
, hashable, haskell-src-exts, microlens, mtl, parallel, pqueue
, pretty, scientific, stdenv, text, time, unordered-containers
, uuid-types, vector
}:
mkDerivation {
  pname = "beam-migrate";
  version = "0.4.0.1";
  src = fetchgit {
    url = "https://github.com/tathougies/beam.git";
    sha256 = "19d0iji3dcfnb1db92cnscznjdn45nzqhc6vqqb427rmj64qk8i1";
    rev = "2fa99310557e49b37a59e349032ff7236085b6f8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/beam-migrate; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base beam-core bytestring containers deepseq dependent-map
    dependent-sum free ghc-prim hashable haskell-src-exts microlens mtl
    parallel pqueue pretty scientific text time unordered-containers
    uuid-types vector
  ];
  homepage = "https://travis.athougies.net/projects/beam.html";
  description = "SQL DDL support and migrations support library for Beam";
  license = stdenv.lib.licenses.mit;
}
