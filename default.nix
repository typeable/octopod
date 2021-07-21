{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix { }
, pkgs ? import haskellNix.sources.nixpkgs-2105 haskellNix.nixpkgsArgs
, nix-filter ? import sources.nix-filter
}:
let
  hsPkgs = pkgs.haskell-nix.cabalProject {
    src = nix-filter {
      root = ./.;
      name = "octopod";
      include = [
        ./octopod-backend/octopod-backend.cabal
        ./octo-cli/octo-cli.cabal
        ./octopod-api/octopod-api.cabal
        ./octopod-common/octopod-common.cabal
        ./octopod-frontend/octopod-frontend.cabal
        ./cabal.project
      ];
    };

    modules = [
      { packages.octopod-backend.src = ./octopod-backend;
        packages.octo-cli.src = ./octo-cli;
        packages.octopod-api.src = ./octopod-api;
        packages.octopod-frontend.src = ./octopod-frontend;
        packages.octopod-common.src = ./octopod-common;
      }
    ];

    index-state = "2021-07-02T00:00:00Z";
    compiler-nix-name = "ghc8105";
  };
in
hsPkgs // {
  octopod-frontend-pretty =
    pkgs.runCommand "octopod-frontend-pretty"
      { } ''
      mkdir $out
      cp ${./octopod-frontend/index.html} $out/index.html
      cp ${hsPkgs.projectCross.ghcjs.hsPkgs.octopod-frontend.components.exes.frontend}/bin/frontend.jsexe/all.js $out/all.js
    '';
}
