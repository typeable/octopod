{ sources ? import ./nix/sources.nix
, reflex-platform ? sources.reflex-platform
, hls ? false
}:
(import reflex-platform { }).project ({ pkgs, ... }:
  let foldOs = pkgs.lib.foldl' pkgs.lib.composeExtensions (self: super: { });
  in
  {
    useWarp = true;

    packages = {
      octopod-common = ./octopod-common;
      octopod-frontend = ./octopod-frontend;
      octopod-backend = ./octopod-backend;
      octo-cli = ./octo-cli;
      octopod-api = ./octopod-api;
    };

    overrides = foldOs [
      (import ./nix/haskell-language-server-overrides.nix {
        inherit (pkgs.haskell) lib;
        inherit sources;
      }
      )
      (import ./nix/octopod-overrides.nix {
        inherit (pkgs.haskell) lib;
        inherit sources;
      }
      )
    ];
    shellToolOverrides = ghc: super:
      if hls then { inherit (ghc) haskell-language-server; }
      else { };
    shells = {
      ghc = [ "octopod-common" "octopod-backend" "octopod-frontend" "octopod-api" "octo-cli" ];
      ghcjs = [ "octopod-common" "octopod-frontend" ];
    };
  })
