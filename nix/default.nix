{ sources ? import ./sources.nix
, octopod-css ? import ../octopod-css { inherit pkgsSrc; }
, migrations ? ../migrations
, system ? builtins.currentSystem
, hsPkgs ? import ./.. { inherit system; }
, pkgs ? hsPkgs.pkgs
, pkgsSrc ? hsPkgs.pkgsSrc
}:
let
  octo-cli = hsPkgs.octo-cli.components.exes.octo;
  octopod-backend = hsPkgs.octopod-backend.components.exes.octopod-exe;

  closurecompiler = (import sources.nixpkgs { inherit system; }).closurecompiler;

  octopod-frontend-ugly =
    let frontend = hsPkgs.octopod-frontend-pretty;
    in
    pkgs.runCommand "octopod-frontend-ugly"
      { } ''
      mkdir $out
      cp -av ${frontend}/* $out
      rm $out/all.js

      ${closurecompiler}/bin/closure-compiler --compilation_level ADVANCED --jscomp_off=checkVars --warning_level QUIET --js ${frontend}/all.js --externs ${frontend}/all.js.externs --js_output_file $out/all.js
    '';

  octopod-server-container = pkgs.dockerTools.buildImage {
    name = "octopod-server-container-slim";
    contents = with pkgs; [
      git
      coreutils
      bash
      openssh
      gnugrep
      cacert
      shadow
      findutils
    ];

    runAsRoot = ''
      mkdir /tmp
      chmod 777 /tmp

      mkdir -p /home/octopod
      useradd octopod -d /home/octopod
      chown octopod.octopod /home/octopod

      mkdir -p /migrations/{deploy,revert,verify}
      cp -av ${migrations}/* /migrations/

      mkdir /www
      cp -av ${octopod-frontend-ugly}/* /www
    '';

    config =
      let entrypoint = "${octopod-backend}/bin/octopod-exe"; in
      {
        Entrypoint = [ entrypoint ];
        Cmd = [
          "--port"
          "4443"
          "--ui-port"
          "4000"
          "--ws-port"
          "4020"
          "--db"
          "host='127.0.0.1' port=5432 user='octopod' password='octopod'"
          "--db-pool-size"
          "10"
        ];
        Env = [
          "PATH=/utils:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
          "USER=octopod"
          "ENTRYPOINT=${entrypoint}"
        ];
      };
  };

  octo-cli-container = pkgs.dockerTools.buildImage {
    name = "octo-cli-container-slim";
    contents = with pkgs; [ cacert ];

    config = let entrypoint = "${octo-cli}/bin/octo"; in
      {
        Entrypoint = [ entrypoint ];
        Env = [
          "ENTRYPOINT=${entrypoint}"
        ];
      };
  };
in
{
  inherit octo-cli-container octopod-server-container octopod-frontend-ugly;
}
