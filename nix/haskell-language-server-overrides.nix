{ lib, sources ? import ./sources.nix }: hself: hsuper:
{
  haskell-language-server = lib.dontCheck (hsuper.callCabal2nix "haskell-language-server" (sources.haskell-language-server) { });
  hls-hlint-plugin = hsuper.callCabal2nix "hls-hlint-plugin" (sources.haskell-language-server + "/plugins/hls-hlint-plugin") { };
  hls-explicit-imports-plugin = hsuper.callCabal2nix "hls-explicit-imports-plugin" (sources.haskell-language-server + "/plugins/hls-explicit-imports-plugin") { };
  hls-retrie-plugin = hsuper.callCabal2nix "hls-retrie-plugin" (sources.haskell-language-server + "/plugins/hls-retrie-plugin") { };
  hls-plugin-api = hsuper.callCabal2nix "hls-plugin-api" (sources.haskell-language-server + "/hls-plugin-api") { };
  hls-tactics-plugin = hsuper.callCabal2nix "hls-tactics-plugin" (sources.haskell-language-server + "/plugins/tactics") { };
  hls-class-plugin = hsuper.callCabal2nix "hls-class-plugin" (sources.haskell-language-server + "/plugins/hls-class-plugin") { };
  hls-eval-plugin = hsuper.callCabal2nix "hls-eval-plugin" (sources.haskell-language-server + "/plugins/hls-eval-plugin") { };
  hls-haddock-comments-plugin = hsuper.callCabal2nix "hls-haddock-comments-plugin" (sources.haskell-language-server + "/plugins/hls-haddock-comments-plugin") { };
  hls-splice-plugin = hsuper.callCabal2nix "hls-splice-plugin" (sources.haskell-language-server + "/plugins/hls-splice-plugin") { };
  shake-bench = hsuper.callCabal2nix "shake-bench" (sources.haskell-language-server + "/shake-bench") { };
  ghcide = lib.dontCheck (hsuper.callCabal2nix "ghcide" (sources.haskell-language-server + "/ghcide") { });
  hie-compat = hsuper.callCabal2nix "hie-compat" (sources.haskell-language-server + "/hie-compat") { };
  hls-exactprint-utils = hsuper.callCabal2nix "hls-exactprint-utils" (sources.haskell-language-server + "/hls-exactprint-utils") { };
  with-utf8 = (hsuper.callHackageDirect
    {
      pkg = "with-utf8";
      ver = "1.0.2.1";
      sha256 = "1hpqc0ljk1c1vl4671zb290hbvdcjpg66bcxmf1cz8h0vb382xp7";
    } { }
  );
  fourmolu = lib.doJailbreak (hsuper.callHackageDirect
    {
      pkg = "fourmolu";
      ver = "0.3.0.0";
      sha256 = "sha256-SFBwhkXfDArITiBSxGSp2qf8gl+yBpWHglBB5aKeaBU=";
    } { }
  );
  heapsize = lib.doJailbreak (hsuper.callHackageDirect
    {
      pkg = "heapsize";
      ver = "0.3.0.1";
      sha256 = "0c8lqndpbx9ahjrqyfxjkj0z4yhm1zlcn8al0ir4ldlahql2xv3r";
    } { }
  );
  opentelemetry = hsuper.callHackageDirect
    {
      pkg = "opentelemetry";
      ver = "0.6.1";
      sha256 = "08k71z7bns0i6r89nmxqsl00kyksicq619rqy6pf5m7hq1r4zs9m";
    } { };
  ghc-trace-events = hsuper.callHackageDirect
    {
      pkg = "ghc-trace-events";
      ver = "0.1.2.1";
      sha256 = "10vrm7hmg97fn8xf0r79d9vfph0j2s105lsgm0hgqay1qz1x7sp7";
    } { };
  haskell-lsp = hsuper.callHackageDirect
    {
      pkg = "haskell-lsp";
      ver = "0.23.0.0";
      sha256 = "0d9bk1cqkk41frm81j683h2vd1hghl4hlvj8g17690d2qk5pq3c0";
    } { };
  haskell-lsp-types = hsuper.callHackageDirect
    {
      pkg = "haskell-lsp-types";
      ver = "0.23.0.0";
      sha256 = "17mfc2zxkbwipxiy0g3qwqnyp8ds4mrg0z1v7jchcm89hnf8mmmq";
    } { };
  ormolu =
    hsuper.callHackageDirect
      {
        pkg = "ormolu";
        ver = "0.1.4.1";
        sha256 = "07gfag591dsys33q2i80f3afxjqny2zpiq4z35d1ajyp7di73m7z";
      } { };
  ghc-check =
    hsuper.callHackageDirect
      {
        pkg = "ghc-check";
        ver = "0.5.0.1";
        sha256 = "1zlbss7h6infzhhpilvkpk50gxypkb2li8fspi69jlll5l7wqi3d";
      } { };
  implicit-hie-cradle =
    hsuper.callHackageDirect
      {
        pkg = "implicit-hie-cradle";
        ver = "0.3.0.2";
        sha256 = "1fhc8zccd7g7ixka05cba3cd4qf5jvq1zif29bhn593dfkzy89lz";
      } { };
  implicit-hie =
    hsuper.callHackageDirect
      {
        pkg = "implicit-hie";
        ver = "0.1.2.5";
        sha256 = "1l0rz4r4hamvmqlb68a7y4s3n73y6xx76zyprksd0pscd9axznnv";
      } { };
  lsp-test = lib.dontCheck (hsuper.callHackageDirect
    {
      pkg = "lsp-test";
      ver = "0.11.0.7";
      sha256 = "sha256-buHcgRbLIn2zr5GK4gHrKuUiyYJK7Nzqc1y+WosaHJg=";
    } { }
  );
  floskell = lib.dontCheck (
    hsuper.callHackageDirect
      {
        pkg = "floskell";
        ver = "0.10.4";
        sha256 = "0n1gy6yf7lzzh9l67712rr7bjliyifi9xjnc6i9rppiv5adj2xyf";
      } { }
  );
  monad-dijkstra = lib.dontCheck (lib.doJailbreak (
    hsuper.callHackageDirect
      {
        pkg = "monad-dijkstra";
        ver = "0.1.1.2";
        sha256 = "1vyw7az18yy5s9jwi3icq76zvi9myr0y9gja3jaywlykcdn6frsz";
      } { }
  ));
  HsYAML-aeson = lib.doJailbreak (
    hsuper.callHackageDirect
      {
        pkg = "HsYAML-aeson";
        ver = "0.2.0.0";
        sha256 = "0zgcp93y93h7rsg9dv202hf3l6sqr95iadd67lmfclb0npfs640m";
      } { }
  );
  ghc-lib-parser-ex =
    hsuper.callHackageDirect
      {
        pkg = "ghc-lib-parser-ex";
        ver = "8.10.0.16";
        sha256 = "0dp8plj708ss3im6rmp41kpj0df71kjzpw1kqkpn0dhms9yr1g0x";
      } { };
  retrie = lib.doJailbreak (lib.dontCheck (
    hsuper.callHackageDirect
      {
        pkg = "retrie";
        ver = "0.1.1.1";
        sha256 = "0gnp6j35jnk1gcglrymvvn13sawir0610vh0z8ya6599kyddmw7l";
      } { }
  ));
  ghc-lib =
    hsuper.callHackageDirect
      {
        pkg = "ghc-lib";
        ver = "8.10.2.20200916";
        sha256 = "1gx0ijay9chachmd1fbb61md3zlvj24kk63fk3dssx8r9c2yp493";
      } { };
  ghc-lib-parser =
    hsuper.callHackageDirect
      {
        pkg = "ghc-lib-parser";
        ver = "8.10.2.20200916";
        sha256 = "1apm9zn484sm6b8flbh6a2kqnv1wjan4l58b81cic5fc1jsqnyjk";
      } { };
  parser-combinators =
    hsuper.callHackageDirect
      {
        pkg = "parser-combinators";
        ver = "1.2.1";
        sha256 = "1990d6c1zm2wq4w9521bx7l3arg4ly02hq1ass9n19gs273bxx5h";
      } { };
  regex-tdfa =
    hsuper.callHackageDirect
      {
        pkg = "regex-tdfa";
        ver = "1.3.1.0";
        sha256 = "1a0l7kdjzp98smfp969mgkwrz60ph24xy0kh2dajnymnr8vd7b8g";
      } { };
  regex-posix =
    hsuper.callHackageDirect
      {
        pkg = "regex-posix";
        ver = "0.96.0.0";
        sha256 = "0js977ahpz10642sbpb55mw9h01pilai6z201wgkncgkg2d69hl3";
      } { };
  ansi-terminal =
    hsuper.callHackageDirect
      {
        pkg = "ansi-terminal";
        ver = "0.10.3";
        sha256 = "1aa8lh7pl054kz7i59iym49s8w473nhdqgc3pq16cp5v4358hw5k";
      } { };
  ghc-exactprint =
    hsuper.callHackageDirect
      {
        pkg = "ghc-exactprint";
        ver = "0.6.3.2";
        sha256 = "0l9piqqgdi8xd46nj1jizp0r0v526d7f61y05xm8k4aamjaj59d0";
      } { };
  stylish-haskell =
    hsuper.callHackageDirect
      {
        pkg = "stylish-haskell";
        ver = "0.12.2.0";
        sha256 = "1ck8i550rvzbvzrm7dvgir73slai8zmvfppg3n5v4igi7y3jy0mr";
      } { };
  shake = lib.dontCheck (
    hsuper.callHackageDirect
      {
        pkg = "shake";
        ver = "0.19.2";
        sha256 = "03p5hq0sg13ii39j9hf3nvy8v20mkh8czb59djn6zrg7fypy9ivv";
      } { }
  );
  regex-base =
    hsuper.callHackageDirect
      {
        pkg = "regex-base";
        ver = "0.94.0.0";
        sha256 = "0x2ip8kn3sv599r7yc9dmdx7hgh5x632m45ga99ib5rnbn6kvn8x";
      } { };
  extra =
    hsuper.callHackageDirect
      {
        pkg = "extra";
        ver = "1.7.3";
        sha256 = "08j4gg2n5cl7ycr943hmyfimgby0xhf5vp8nwrwflg6lrn1s388c";
      } { };
  quickcheck-instances =
    hsuper.callHackageDirect
      {
        pkg = "quickcheck-instances";
        ver = "0.3.22";
        sha256 = "05mam3x4x7c881bqbq8lcbclmz914yziqh6s04icxzp12zq7c7ks";
      } { };
  apply-refact = lib.doJailbreak (hsuper.callHackageDirect
    {
      pkg = "apply-refact";
      ver = "0.8.2.1";
      sha256 = "0nnprv5lbk7c8w1pa4kywk0cny6prjaml4vnw70s8v6c1r1dx2rx";
    } { }
  );
  hlint =
    hsuper.callHackageDirect
      {
        pkg = "hlint";
        ver = "3.2";
        sha256 = "0y2agj9y4nwhypv9ss97n26mc28pcynfynwilcyj2lwfabhr9pzc";
      } { };
  Diff =
    hsuper.callHackageDirect
      {
        pkg = "Diff";
        ver = "0.4.0";
        sha256 = "1phz4cz7i53jx3d1bj0xnx8vpkk482g4ph044zv5c6ssirnzq3ng";
      } { };
  refinery = lib.dontCheck (
    hsuper.callHackageDirect
      {
        pkg = "refinery";
        ver = "0.3.0.0";
        sha256 = "08s5pw6j3ncz96zfc2j0cna2zbf4vy7045d6jpzmq2sa161qnpgi";
      } { }
  );
  hie-bios = lib.dontCheck (
    hsuper.callHackageDirect
      {
        pkg = "hie-bios";
        ver = "0.7.1";
        sha256 = "137f1dy0fmlrzngwcmgnxghcih7f2rfq5bdnizbwy9534dn4dr42";
      } { }
  );
  test-framework = lib.doJailbreak hsuper.test-framework;
  aeson = lib.dontCheck hsuper.aeson;
  tree-diff = lib.doJailbreak hsuper.tree-diff;
  brittany = hsuper.callHackageDirect
    {
      pkg = "brittany";
      ver = "0.13.1.0";
      sha256 = "172mg0ch2awfzhz8vzvjrfdjylfzawrbgfr5z82l1qzjh6g9z295";
    } { };
}
