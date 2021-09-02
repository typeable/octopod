{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix { inherit system; }
, pkgsSrc ? import haskellNix.sources.nixpkgs-2105
, pkgs ? pkgsSrc (haskellNix.nixpkgsArgs // { inherit system; })
, nix-filter ? import sources.nix-filter
, system ? builtins.currentSystem
, prod ? false
}:
let
  octopod-css = import ./octopod-css { inherit pkgsSrc; };
  removeOptimizationFlags = x: if prod then x else x // { ghcOptions = [ "-O0" ]; };

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
      {
        ghcOptions = [ "-O2" ];
        dontStrip = false;
        dontPatchELF = false;
        enableDeadCodeElimination = true;
        packages.octopod-backend = removeOptimizationFlags { src = ./octopod-backend; };
        packages.octo-cli = removeOptimizationFlags { src = ./octo-cli; };
        packages.octopod-api = removeOptimizationFlags { src = ./octopod-api; };
        packages.octopod-frontend = removeOptimizationFlags { src = ./octopod-frontend; };
        packages.octopod-common = removeOptimizationFlags { src = ./octopod-common; };
      }
    ];

    index-state = "2021-08-04T00:00:00Z";
    compiler-nix-name = "ghc8105";
  };
in
hsPkgs // {
  octopod-frontend-pretty =
    let frontend = hsPkgs.projectCross.ghcjs.hsPkgs.octopod-frontend.components.exes.frontend;
    in
    pkgs.runCommand "octopod-frontend-pretty"
      { } ''
      mkdir $out
      cp -av ${octopod-css}/* $out
      cp ${./octopod-frontend/index.html} $out/index.html
      cp ${frontend}/bin/frontend.jsexe/all.js $out/all.js
      cp ${frontend}/bin/frontend.jsexe/all.js.externs $out/all.js.externs
    '';
  inherit pkgsSrc;
}
