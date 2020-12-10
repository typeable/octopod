{ sources ? import ./sources.nix
, compiler ? "ghc865"
, pkgs-expr ? sources.nixpkgs
, static-haskell-nix ? sources.static-haskell-nix
}:
let
  foldOs = normalPkgs.lib.foldl' normalPkgs.lib.composeExtensions (self: super: { });
  normalPkgs = import pkgs-expr {
    overlays = [
      (self: super: {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            "${compiler}" = super.haskell.packages."${compiler}".override {
              overrides =
                foldOs [
                  (import ./octopod-overrides.nix {
                    inherit (normalPkgs.haskell) lib;
                    inherit sources;
                  }
                  )
                  (hself: hsuper: {
                    octo-cli = hsuper.callCabal2nix "octo-cli" ../octo-cli { };
                    octopod-api = hsuper.callCabal2nix "octopod-api" ../octopod-api { };
                    octopod-common = hsuper.callCabal2nix "octopod-common" ../octopod-common { };
                  })
                ];
            };
          };
        };
      })
    ];
  };
  static-haskell = import "${static-haskell-nix}/survey" { inherit normalPkgs compiler; };
in
static-haskell.haskellPackages.octo-cli
