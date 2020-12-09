{ sources ? import ./nix/sources.nix
, reflex-platform ? sources.reflex-platform
}:
(import reflex-platform { }).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    octopod-common = ./octopod-common;
    octopod-frontend = ./octopod-frontend;
    octopod-backend = ./octopod-backend;
    octo-cli = ./octo-cli;
    octopod-api = ./octopod-api;
  };

  overrides = hself: hsuper: {
    servant-reflex = hsuper.callCabal2nix "servant-reflex" sources.servant-reflex { };
    tabulation = hsuper.callCabal2nix "tabulation" "${sources.obelisk}/lib/tabulation" { };
    obelisk-executable-config-lookup = hsuper.callCabal2nix "obelisk-executable-config-lookup" "${sources.obelisk}/lib/executable-config/lookup" { };
    obelisk-route = hsuper.callCabal2nix "obelisk-route" "${sources.obelisk}/lib/route" { };
    hspec-webdriver = hsuper.callCabal2nix "hspec-webdriver" sources.hspec-webdriver-clone { };
    servant = pkgs.haskell.lib.overrideCabal hsuper.servant (old: {
      postInstall = "";
    });
    servant-websockets = hsuper.callHackageDirect
      {
        pkg = "servant-websockets";
        ver = "2.0.0";
        sha256 = "01bmwg3ysj8gijcqghykxfsd62sqz1pfby2irpzh5ybwyh285pvg";
      } { };
    deriving-aeson = hsuper.callHackageDirect
      {
        pkg = "deriving-aeson";
        ver = "0.2.3";
        sha256 = "0ckwdi9pr4aqp9psag4mdbx30nygxkkpdf21rg9rfz16cz8079j7";
      } { };
    table-layout = hsuper.callHackageDirect
      {
        pkg = "table-layout";
        ver = "0.9.0.1";
        sha256 = "12nllfnh6b5mjda9qxfy192v0r0sx181w9zc9j70kvjdn7hgrb0y";
      } { };
    data-default-instances-base = hsuper.callHackageDirect
      {
        pkg = "data-default-instances-base";
        ver = "0.1.0.1";
        sha256 = "18basdy4qjn246phw008ll9zbi3rpdn6bh2dk0i81a60gsmyn58q";
      } { };
  };

  shells = {
    ghc = [ "octopod-common" "octopod-backend" "octopod-frontend" "octopod-api" "octo-cli" ];
    ghcjs = [ "octopod-common" "octopod-frontend" ];
  };
})
