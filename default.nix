{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix { }
, pkgs ? import haskellNix.sources.nixpkgs-2105 haskellNix.nixpkgsArgs
, useWarp ? true
}:
let
  hsPkgs = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "octopod";
      src = ./.;
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
