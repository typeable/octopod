{ pkgsSrc ? (import ./.. { }).pkgsSrc
, sources ? import ../nix/sources.nix
, nix-filter ? import sources.nix-filter
, system ? builtins.currentSystem
}:
let
  pkgs = pkgsSrc {
    overlays = [ (self: super: { nodejs = self.nodejs-10_x; }) ];
    inherit system;
  };
  production-css = pkgs.mkYarnPackage {
    name = "octopod-css";
    src = nix-filter {
      root = ./.;
      name = "octopod-css";
      include = with nix-filter; [
        (inDirectory ./development)
        (inDirectory ./favicons)
        ./gulpfile.js
        ./package.json
        ./.stylelintrc.json
      ];
    };
    yarnLock = ./yarn.lock;
    buildPhase = "yarn run gulp";
    doDist = false;
    distPhase = " ";
    installPhase = ''
      runHook preInstall

      cp -av deps/octopod-css/production $out

      runHook postInstall
    '';
  };
in
pkgs.runCommand "octopod-production-assets"
{ } ''
  mkdir $out
  cp -av ${./.}/favicons/* $out
  mkdir $out/static
  cp -av ${production-css}/images $out/static
  cp -av ${production-css}/styles $out/static
  cp -av ${production-css}/vendors $out/static
''
