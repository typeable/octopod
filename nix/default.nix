{ sources ? import ./sources.nix
, octopod-css ? ../octopod-css
, migrations ? ../migrations
, system ? builtins.currentSystem
}:
let
  hsPkgs = import ./.. { inherit system; };

  pkgs = hsPkgs.pkgs;

  octo-cli = hsPkgs.octo-cli.components.exes.octo;
  octopod-backend = hsPkgs.octopod-backend.components.exes.octopod-exe;

  terser = (import sources.nixpkgs { inherit system; }).nodePackages.terser;

  octopod-frontend-ugly = pkgs.runCommand "octopod-frontend-ugly"
    { } ''
    mkdir $out
    cp ${hsPkgs.octopod-frontend-pretty}/index.html $out/index.html

    ${terser}/bin/terser ${hsPkgs.octopod-frontend-pretty}/all.js -o $out/all.js -mangle -c
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

      mkdir -p /www/static/{images,styles,vendors/outline}
      cp -av ${octopod-frontend-ugly}/* /www/
      cp -av ${octopod-css}/production/images/* /www/static/images/
      cp -av ${octopod-css}/production/styles/* /www/static/styles/
      cp -av ${octopod-css}/production/vendors/outline/* /www/static/vendors/outline/
      cp -av ${octopod-css}/favicons/* /www/
    '';

    config = {
      Entrypoint = [ "${octopod-backend}/bin/octopod-exe" ];
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
      ];
    };
  };

  octo-cli-container = pkgs.dockerTools.buildImage {
    name = "octo-cli-container-slim";
    contents = with pkgs; [ cacert ];

    config = {
      Entrypoint = [ "${octo-cli}/bin/octo" ];
    };
  };
in
{
  inherit octo-cli-container octopod-server-container octopod-frontend-ugly;
}
