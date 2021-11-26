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
  insaneOptimizationFlags = [
    "-O2"
    "-fexpose-all-unfoldings"
    "-fspecialise-aggressively"
    "-fsimpl-tick-factor=200"
    "-flate-specialise"
    "-fstatic-argument-transformation"
    "-fsimplifier-phases=4"
    "-fmax-simplifier-iterations=20"
    "-flate-dmd-anal"
  ];
  addLocalOptions = x:
    if prod then x // {
      ghcOptions = [ "-Werror" ] ++ insaneOptimizationFlags;
    }
    else x // { ghcOptions = [ "-O0" ]; };

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
        ghcOptions = insaneOptimizationFlags;
        dontStrip = false;
        dontPatchELF = false;
        enableDeadCodeElimination = true;
        packages.octopod-backend = addLocalOptions (
          if prod
          then {
            src = pkgs.runCommand "octopod-backend-src" { }
              ''
                mkdir -p $out
                cp -r ${./octopod-backend}/* $out
                cp -r ${builtins.path { path = ./.git; name = "dot-git"; }}/ $out/.git/
              '';
            components.exes.octopod-exe = {
              build-tools =
                pkgs.lib.mkForce [ pkgs.buildPackages.buildPackages.gitReallyMinimal ];
              extraSrcFiles = [ ".git/**/*" ];
            };
          }
          else { src = ./octopod-backend; }
        );
        packages.octo-cli = addLocalOptions { src = ./octo-cli; };
        packages.octopod-api = addLocalOptions { src = ./octopod-api; };
        packages.octopod-frontend = addLocalOptions { src = ./octopod-frontend; };
        packages.octopod-common = addLocalOptions { src = ./octopod-common; };
      }
    ];

    index-state = "2021-11-22T00:00:00Z";
    compiler-nix-name = "ghc8107";
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
