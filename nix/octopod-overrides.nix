{ lib, sources ? import ./sources.nix }: hself: hsuper:
{
  tabulation = hsuper.callCabal2nix "tabulation" "${sources.obelisk}/lib/tabulation" { };
  obelisk-executable-config-lookup = hsuper.callCabal2nix "obelisk-executable-config-lookup" "${sources.obelisk}/lib/executable-config/lookup" { };
  obelisk-route = hsuper.callCabal2nix "obelisk-route" "${sources.obelisk}/lib/route" { };
  hspec-webdriver = hsuper.callCabal2nix "hspec-webdriver" sources.hspec-webdriver-clone { };
  servant-reflex = lib.doJailbreak (hsuper.callHackageDirect
    {
      pkg = "servant-reflex";
      ver = "0.3.5";
      sha256 = "1cj5b7hl4jhsqxfg8vdw50z8zvfxkj42f41hmyx217w6bv3s3fdb";
    } { }
  );
  servant = hsuper.callHackageDirect
    {
      pkg = "servant";
      ver = "0.18.2";
      sha256 = "0l2k895nxvw2ngr9201g3br6s9zab7mk5mhpjibyg8mxfbv75a8y";
    } { };
  servant-client-core = hsuper.callHackageDirect
    {
      pkg = "servant-client-core";
      ver = "0.18.2";
      sha256 = "1hazxk1laklpm2c65zgkk2gn8mvlp682437071s04bqggk9b59sx";
    } { };
  servant-server = hsuper.callHackageDirect
    {
      pkg = "servant-server";
      ver = "0.18.2";
      sha256 = "1kynxl7qg5z45bhi0k61sxn79xkgnq1z97ccqqs39wjyf45fj5yy";
    } { };
  servant-client = hsuper.callHackageDirect
    {
      pkg = "servant-client";
      ver = "0.18.2";
      sha256 = "0yip2s63ivrlrpficdipq60j2a6czg8agn18lpkkaxf3n55j4jr3";
    } { };
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
  postgresql-simple = hsuper.callHackageDirect
    {
      pkg = "postgresql-simple";
      ver = "0.6.3";
      sha256 = "16kk1bc23gc23cd79c2iy90zh03x5n6b9r2kinjspy7hl9jlzs2a";
    } { };
  concurrent-output = hsuper.callHackageDirect
    {
      pkg = "concurrent-output";
      ver = "1.10.12";
      sha256 = "1ysvahlf6frjfxpv6lwzxn05ps6b4zy1f5xb0yg6z688wbw1f6a7";
    } { };
  hspec-wai = hsuper.callHackageDirect
    {
      pkg = "hspec-wai";
      ver = "0.11.0";
      sha256 = "1kqbk2by70smhd9p1zff6ilimvvxc54dkh2l0wpn03zqvydjnwl2";
    } { };
  patch = lib.doJailbreak (hsuper.callHackageDirect
    {
      pkg = "patch";
      ver = "0.0.3.2";
      sha256 = "10alg64zwx72lz42z9i5hfwdpk0r79zfli57l41akyznj4pwrvgh";
    } { }
  );
  path = hsuper.callHackageDirect
    {
      pkg = "path";
      ver = "0.8.0";
      sha256 = "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb";
    } { };
  path-io = hsuper.callHackageDirect
    {
      pkg = "path-io";
      ver = "1.6.2";
      sha256 = "1s7cahx8cf85kwz9l8cm6x6kvjwsv0spwv78pybsspb1ap5q287p";
    } { };
  optparse-applicative =
    hsuper.callHackageDirect
      {
        pkg = "optparse-applicative";
        ver = "0.15.1.0";
        sha256 = "1mii408cscjvids2xqdcy2p18dvanb0qc0q1bi7234r23wz60ajk";
      } { };
  reflex = lib.dontCheck (lib.doJailbreak hsuper.reflex);
  reflex-dom-core = lib.dontCheck hsuper.reflex-dom-core;
  chronos = lib.doJailbreak hsuper.chronos;
  hedgehog = lib.doJailbreak hsuper.hedgehog;
}
