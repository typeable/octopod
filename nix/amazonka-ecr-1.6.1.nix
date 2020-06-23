{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, stdenv, tasty, tasty-hunit, text, time, unordered-containers
}:
mkDerivation {
  pname = "amazonka-ecr";
  version = "1.6.1";
  sha256 = "2d0d5dd640f63e11b6009d3b486505e93afd379c5f5738df79582b5eeb6c7358";
  libraryHaskellDepends = [ amazonka-core base ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring tasty tasty-hunit text
    time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon EC2 Container Registry SDK";
  license = stdenv.lib.licenses.mpl20;
}
