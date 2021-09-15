{ sources ? import ./sources.nix
, pkgs ? import sources.nixpkgs { }
}:
pkgs.mkShell {
  packages = [ pkgs.haskellPackages.fourmolu ];
}
