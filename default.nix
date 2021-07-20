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
        (nix-filter.inDirectory ./octo-cli)
        (nix-filter.inDirectory ./octopod-backend)
        (nix-filter.inDirectory ./octopod-common)
        (nix-filter.inDirectory ./octopod-frontend)
        (nix-filter.inDirectory ./octopod-api)
        ./cabal.project
      ];
    };
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
