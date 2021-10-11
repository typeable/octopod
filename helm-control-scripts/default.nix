{ sources ? import ../nix/sources.nix
, system ? builtins.currentSystem
, pkgs ? (import sources.nixpkgs { inherit system; }).pkgsCross.musl64
}:
let
  scripts = pkgs.rustPlatform.buildRustPackage {
    name = "helm-control-scripts";

    src = ./.;

    cargoLock = {
      lockFile = ./Cargo.lock;
      outputHashes = {
        "dkregistry-0.5.1-alpha.0" = "sha256:1g8xhznnaqb0ksjm95954ws4a11dm1mh2b9b2zn4g7ibcdbrhv3q";
      };
    };


    nativeBuildInputs = [ pkgs.pkg-config ];

    buildInputs = [ pkgs.openssl.dev ] ++
      pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Security;
    dontStrip = false;
    dontPatchELF = false;
    enableDeadCodeElimination = true;
  };
in
scripts
