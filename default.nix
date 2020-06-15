{ sources ? import ./nix/sources.nix
, reflex-platform ? ((import sources.nixpkgs {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "5429278830e1555a577f2550e045ce7f7164aa65";
    sha256 = "1lp86cgccmim573rarsjny5vh0ygkfp5afq7006li0k9w2sw2d4c";
    })
}:
(import reflex-platform {}).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    dm-common = ./dm-common;
    dm-frontend = ./dm-frontend;
    dm-backend = ./dm-backend;
  };

  overrides = hself: hsuper: {
    deriving-aeson = hsuper.callCabal2nix "deriving-aeson" sources.deriving-aeson { };
    servant = pkgs.haskell.lib.overrideCabal hsuper.servant (old: {
      postInstall = "";
    });
  };

  shells = {
    ghc = ["dm-common" "dm-backend" "dm-frontend"];
    ghcjs = ["dm-common" "dm-frontend"];
  };
})

