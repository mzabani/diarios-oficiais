{ mkDerivation, aeson, base, bytestring, containers, dlist
, fetchgit, free, ghc-prim, hashable, microlens, mtl, network-uri
, scientific, stdenv, tagged, tasty, tasty-hunit, text, time
, vector, vector-sized
}:
mkDerivation {
  pname = "beam-core";
  version = "0.8.1.0";
  src = fetchgit {
    url = "https://github.com/tathougies/beam.git";
    sha256 = "19d0iji3dcfnb1db92cnscznjdn45nzqhc6vqqb427rmj64qk8i1";
    rev = "2fa99310557e49b37a59e349032ff7236085b6f8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/beam-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bytestring containers dlist free ghc-prim hashable
    microlens mtl network-uri scientific tagged text time vector
    vector-sized
  ];
  testHaskellDepends = [
    base bytestring tasty tasty-hunit text time
  ];
  homepage = "http://travis.athougies.net/projects/beam.html";
  description = "Type-safe, feature-complete SQL query and manipulation interface for Haskell";
  license = stdenv.lib.licenses.mit;
}
