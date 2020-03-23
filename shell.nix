{ nixpkgs ? import ./nix/. {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
in with pkgs;
stdenv.mkDerivation {
  name = "shell";
  buildInputs = haskellPackages.dm.env.nativeBuildInputs ++ [
    haskellPackages.pretty-show
    haskellPackages.ghcid
    haskellPackages.cabal-install
  ];
}
