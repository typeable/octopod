{ sources ? import ./sources.nix
, compiler ? "ghc865"
, pkgs-expr ? sources.nixpkgs
, static-haskell-nix ? sources.static-haskell-nix
}:
let
  normalPkgs = import pkgs-expr {
    overlays = [
      (self: super: {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            "${compiler}" = super.haskell.packages."${compiler}".override {
              overrides =
                (hself: hsuper: {
                  octo-cli = hsuper.callCabal2nix "octo-cli" ../octo-cli { };
                  octopod-api = hsuper.callCabal2nix "octopod-api" ../octopod-api { };
                  octopod-common = hsuper.callCabal2nix "octopod-common" ../octopod-common { };
                  hspec-wai = hsuper.callHackage "hspec-wai" "0.9.2" { };
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
                });
            };
          };
        };
      })
    ];
  };
  static-haskell = import "${static-haskell-nix}/survey" { inherit normalPkgs compiler; };
in
static-haskell.haskellPackages.octo-cli
