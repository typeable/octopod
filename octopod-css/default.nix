{ pkgsSrc ? sources.nixpkgs
, sources ? import ../nix/sources.nix
, nix-filter ? import sources.nix-filter
}:
(import pkgsSrc {
  overlays = [ (self: super: { nodejs = self.nodejs-10_x; }) ];
}).mkYarnPackage {
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

    cp -R deps/octopod-css/production $out

    runHook postInstall
  '';
}
