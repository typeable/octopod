{ mkDerivation, aeson, base, bytestring, directory, fast-logger
, http-client, mtl, network-uri, optparse-generic
, postgresql-simple, resource-pool, servant, servant-client
, servant-server, stdenv, temporary, text, transformers
, typed-process, warp
}:
mkDerivation {
  pname = "dm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring directory fast-logger http-client mtl
    network-uri optparse-generic postgresql-simple resource-pool
    servant servant-client servant-server temporary text transformers
    typed-process warp
  ];
  executableHaskellDepends = [
    aeson base bytestring directory fast-logger http-client mtl
    network-uri optparse-generic postgresql-simple resource-pool
    servant servant-client servant-server temporary text transformers
    typed-process warp
  ];
  testHaskellDepends = [
    aeson base bytestring directory fast-logger http-client mtl
    network-uri optparse-generic postgresql-simple resource-pool
    servant servant-client servant-server temporary text transformers
    typed-process warp
  ];
  homepage = "https://github.com/https://github.com/Aviora/dm#readme";
  license = stdenv.lib.licenses.bsd3;
}
