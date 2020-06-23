{ mkDerivation, amazonka-core, base, bytestring, conduit
, conduit-extra, directory, exceptions, http-client, http-conduit
, http-types, ini, mmorph, monad-control, mtl, resourcet, retry
, stdenv, tasty, tasty-hunit, text, time, transformers
, transformers-base, transformers-compat, unliftio-core, void
}:
mkDerivation {
  pname = "amazonka";
  version = "1.6.1";
  sha256 = "edb794b7ed0db3f5955ec08ded68b5eca753f62978312c881f3cb0c6eb769180";
  revision = "2";
  editedCabalFile = "171rp3cbgy58lps437c1jfpmi4xsp0z4pral7jh3mybn73l672zm";
  libraryHaskellDepends = [
    amazonka-core base bytestring conduit conduit-extra directory
    exceptions http-client http-conduit http-types ini mmorph
    monad-control mtl resourcet retry text time transformers
    transformers-base transformers-compat unliftio-core void
  ];
  testHaskellDepends = [ base tasty tasty-hunit ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Comprehensive Amazon Web Services SDK";
  license = stdenv.lib.licenses.mpl20;
}
