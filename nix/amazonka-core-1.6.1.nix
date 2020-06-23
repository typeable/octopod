{ mkDerivation, aeson, attoparsec, base, bifunctors, bytestring
, case-insensitive, conduit, conduit-extra, cryptonite
, data-ordlist, deepseq, exceptions, hashable, http-client
, http-conduit, http-types, lens, memory, mtl, QuickCheck
, quickcheck-unicode, resourcet, scientific, semigroups, stdenv
, tagged, tasty, tasty-hunit, tasty-quickcheck, template-haskell
, text, time, transformers, transformers-compat
, unordered-containers, xml-conduit, xml-types
}:
mkDerivation {
  pname = "amazonka-core";
  version = "1.6.1";
  sha256 = "4198f52da9d20338bd6a3a18748d4312d3ff2c06bc84503cb18406251b28a243";
  revision = "1";
  editedCabalFile = "1656dyw6fk3gvph6v3xzvdp3p8xny3ji0gxg7qxvmvn60gj9ricv";
  libraryHaskellDepends = [
    aeson attoparsec base bifunctors bytestring case-insensitive
    conduit conduit-extra cryptonite deepseq exceptions hashable
    http-client http-conduit http-types lens memory mtl resourcet
    scientific semigroups tagged text time transformers
    transformers-compat unordered-containers xml-conduit xml-types
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive conduit data-ordlist
    http-conduit http-types lens QuickCheck quickcheck-unicode tasty
    tasty-hunit tasty-quickcheck template-haskell text time
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Core data types and functionality for Amazonka libraries";
  license = stdenv.lib.licenses.mpl20;
}
