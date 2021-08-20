{ pkgs ? hsPkgs.pkgs
, sources ? import ../nix/sources.nix
, nix-filter ? import sources.nix-filter
, hsPkgs ? import ./.. { }
, migrations ? ../migrations
}:
{
  frontend =
    let

      frontend = hsPkgs.octopod-frontend-pretty;

      frontendConfig = pkgs.writeTextDir "config.json" ''
        {
          "app_url": "http://localhost:8000",
          "ws_url": "ws://localhost:4020",
          "app_auth": "",
          "kubernetes_dashboard_url_template": "https://echo-url.github.io/#"
        }
      '';

      caddyConfig = pkgs.writeText "caddy-config" ''
        http://localhost:8000

        reverse_proxy /api/* localhost:3002

        file_server /config.json {
          root ${frontendConfig}
        }

        file_server {
          root ${frontend}
        }

        log {
          output stdout
          format single_field common_log
        }
      '';

      caddyRun = pkgs.writeScript "run-caddy.sh" ''
        #!${pkgs.bash}/bin/bash

        ${pkgs.caddy}/bin/caddy run --config ${caddyConfig} --adapter caddyfile
      '';
    in
    caddyRun;

  backend =
    let
      echoScript = pkgs.writeScript "echo.sh" ''
        #!${pkgs.bash}/bin/bash

        echo $0 $@
        exit 0
      '';

      failScript = pkgs.writeScript "fail.sh" ''
        #!${pkgs.bash}/bin/bash

        exit 1
      '';

      infoScript = pkgs.writeScript "info.sh" ''
        #!${pkgs.bash}/bin/bash

        echo "key,value"
        echo "key2,value2"

        exit 0
      '';

      writeScript = pkgs.writeScript "write.sh" ''
        #!${pkgs.bash}/bin/bash

        #!/bin/bash

        echo $0 $@ >> ./tmp/calls.txt
        exit 0
      '';

      runBackend = pkgs.writeScript "run-backend.sh" ''
        #!${pkgs.bash}/bin/bash

        ${pkgs.sqitchPg}/bin/sqitch deploy --chdir ${migrations} -t postgresql://octopod:octopod@localhost:5432/octopod

        export PROJECT_NAME="Example Project"
        export BASE_DOMAIN=octopod.example.com
        export NAMESPACE=deployment
        export ARCHIVE_RETENTION=60
        export STATUS_UPDATE_TIMEOUT=600
        export CREATION_COMMAND=${echoScript}
        export UPDATE_COMMAND=${echoScript}
        export UPDATE_ENVS_COMMAND=${echoScript}
        export ARCHIVE_COMMAND=${echoScript}
        export CHECKING_COMMAND=${echoScript}
        export CLEANUP_COMMAND=${echoScript}
        export ARCHIVE_CHECKING_COMMAND=${echoScript}
        export TAG_CHECKING_COMMAND=${echoScript}
        export INFO_COMMAND=${infoScript}
        export NOTIFICATION_COMMAND=${writeScript}
        export POWER_AUTHORIZATION_HEADER="123"
        ${hsPkgs.octopod-backend.components.exes.octopod-exe}/bin/octopod-exe \
            --port 4443 \
            --ui-port 3002 \
            --ws-port 4020 \
            --db "host='127.0.0.1' port=5432 user='octopod' password='octopod'" \
            --db-pool-size 10
      '';
    in
    runBackend;
}
