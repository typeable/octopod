{ mkDerivation, aeson, async, base, bytestring, chronos, connection
, data-default-class, deriving-aeson, directory, fast-logger
, filepath, generic-lens, http-api-data, http-client
, http-client-tls, lens, mtl, network-uri, optparse-generic
, postgresql-error-codes, postgresql-simple, resource-pool, servant
, servant-client, servant-client-core, servant-server, stdenv
, temporary, text, tls, transformers, typed-process, warp, warp-tls
, x509, x509-store, x509-validation
}:
mkDerivation {
  pname = "dm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring chronos connection data-default-class
    deriving-aeson directory fast-logger filepath generic-lens
    http-api-data http-client http-client-tls lens mtl network-uri
    optparse-generic postgresql-error-codes postgresql-simple
    resource-pool servant servant-client servant-client-core
    servant-server temporary text tls transformers typed-process warp
    warp-tls x509 x509-store x509-validation
  ];
  executableHaskellDepends = [
    aeson async base bytestring chronos connection data-default-class
    directory fast-logger filepath http-client http-client-tls mtl
    network-uri optparse-generic postgresql-error-codes
    postgresql-simple resource-pool servant servant-client
    servant-client-core servant-server temporary text tls transformers
    typed-process warp warp-tls x509 x509-store x509-validation
  ];
  testHaskellDepends = [
    aeson async base bytestring chronos connection data-default-class
    directory fast-logger filepath http-client http-client-tls mtl
    network-uri optparse-generic postgresql-error-codes
    postgresql-simple resource-pool servant servant-client
    servant-client-core servant-server temporary text tls transformers
    typed-process warp warp-tls x509 x509-store x509-validation
  ];
  homepage = "https://github.com/https://github.com/Aviora/dm#readme";
  license = stdenv.lib.licenses.bsd3;
}
