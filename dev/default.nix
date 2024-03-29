{ pkgs ? hsPkgs.pkgs
, sources ? import ../nix/sources.nix
, nix-filter ? import sources.nix-filter
, hsPkgs ? import ./.. { inherit prod system; }
, migrations ? ../migrations
, octopod-css ? import ../octopod-css { inherit pkgsSrc; }
, pkgsSrc ? hsPkgs.pkgsSrc
, prod ? false
, system ? builtins.currentSystem
}:
let frontendConfig = pkgs.writeTextDir "config.json" ''
  {
    "app_url": "http://localhost:8000",
    "ws_url": "ws://localhost:4020",
    "app_auth": "",
    "kubernetes_dashboard_url_template": "https://echo-url.github.io/#"
  }
'';
in
{
  frontend =
    let
      frontend = hsPkgs.octopod-frontend-pretty;

      caddyConfig = pkgs.writeText "caddy-config" ''
        http://localhost:8000

        header Cache-Control no-cache

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

  caddyForGhcid =
    let
      caddyConfig = pkgs.writeText "caddy-config" ''
        http://localhost:8000

        header Cache-Control no-cache

        reverse_proxy /api/* localhost:3002

        @notStatic {
          not file {
            root ${octopod-css}
          }
          not path /config.json
        }

        reverse_proxy @notStatic localhost:3003

        file_server /config.json {
          root ${frontendConfig}
        }

        file_server {
          root ${octopod-css}
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

        echo "You did something wrong :("
        echo "You did something wrong twice :("

        1>&2 echo "You did something wrong, but you shouldn't see this."

        exit 1
      '';

      infoScript = pkgs.writeScript "info.sh" ''
        #!${pkgs.bash}/bin/bash

        sleep 4

        echo "foo.vat1.fjij.key1,"

        for i in {1..10}
        do
        for j in {1..10}
        do
          echo "foo.vat$i.fjij.key$j,value"
        done
        done

        for i in {1..10}
        do
          echo "fjij.key$i,value"
        done

        exit 0
      '';
      smallInfoScript = pkgs.writeScript "info.sh" ''
        #!${pkgs.bash}/bin/bash

        sleep 4

        for i in {1..10}
        do
          echo "fjij.key$i,value"
        done

        exit 0
      '';

      keysScript = pkgs.writeScript "keys.sh" ''
        #!${pkgs.bash}/bin/bash

        sleep 4

        for i in {1..10}
        do
          echo "foo.vat.fkijf.fjij.key$i"
        done
        for i in {1..10}
        do
          echo "foo.vat.fjij.key$i"
        done

        for i in {1..10}
        do
          echo "fjij.key$i"
        done

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
        export CONFIG_CHECKING_COMMAND=${failScript}
        export INFO_COMMAND=${smallInfoScript}
        export NOTIFICATION_COMMAND=${writeScript}
        export DEPLOYMENT_CONFIG_COMMAND=${infoScript}
        export DEPLOYMENT_KEYS_COMMAND=${keysScript}
        export APPLICATION_CONFIG_COMMAND=${infoScript}
        export APPLICATION_KEYS_COMMAND=${keysScript}
        export UNARCHIVE_COMMAND=${writeScript}
        export POWER_AUTHORIZATION_HEADER="123"
        export CACHE_INVALIDATION_TIME="60"
        export CACHE_UPDATE_TIME="20"
        export CONTROL_SCRIPT_TIMEOUT="10"
        export DEBUG=true
        export PROD_LOGS=false
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
