{ sources ? import ./sources.nix
, octopod-css ? ../octopod-css
, migrations ? "please use '--arg migration <value>'"
}:
let
  hsPkgs = import ./.. { };

  pkgs = hsPkgs.pkgs;

  octo-cli = hsPkgs.octo-cli.components.exes.octo;
  octopod-backend = hsPkgs.octopod-backend.components.exes.octopod-exe;
  octopod-frontend = hsPkgs.octopod-frontend.components.exes.frontend;

  cacert' = pkgs.cacert.overrideAttrs (o: {
    fixupPhase = ''
      cat $out/etc/ssl/certs/* > $out/etc/ssl/certs/ca-certificates.crt
    '';
  });

  octopod-server-container = pkgs.dockerTools.buildImage {
    name = "octopod-server-container-slim";
    contents = with pkgs; [
      octopod-backend
      octopod-frontend
      git
      coreutils
      bash
      openssh
      gnugrep
      cacert'
      shadow
      findutils
    ];

    runAsRoot = ''
      mkdir /tmp
      chmod 777 /tmp

      mkdir -p /home/octopod
      useradd octopod -d /home/octopod
      chown octopod.octopod /home/octopod

      mkdir /app
      cp -av ${octopod-backend}/bin/octopod-exe /app/octopod-exe

      mkdir -p /migrations/{deploy,revert,verify}
      cp -av ${migrations}/* /migrations/

      mkdir /tls /tls_store

      mkdir -p /www/static/{images,styles,vendors/outline}
      cp -av ${octopod-frontend}/bin/frontend.jsexe/* /www/
      cp -av ${octopod-css}/production/images/* /www/static/images/
      cp -av ${octopod-css}/production/styles/* /www/static/styles/
      cp -av ${octopod-css}/production/vendors/outline/* /www/static/vendors/outline/
      cp -av ${octopod-css}/favicons/* /www/
    '';

    config = {
      Entrypoint = [ "/app/octopod-exe" ];
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
        "--tls-cert-path"
        "/tls/server_cert.pem"
        "--tls-key-path"
        "/tls/server_key.pem"
        "--tls-store-path"
        "/tls_store"
      ];
      Env = [
        "PATH=/utils:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        "USER=octopod"
      ];
    };
  };

  octo-cli-container = pkgs.dockerTools.buildImage {
    name = "octo-cli-container-slim";
    contents = with pkgs; [ octo-cli coreutils bash ];

    runAsRoot = ''
      mkdir /app
      cp -av ${octo-cli}/bin/octo /app/octo
    '';

    config = {
      Entrypoint = [ "/app/octo" ];
      Env = [ "EDITOR=${pkgs.vim}/bin/vim" ];
    };
  };
in
{ inherit octo-cli-container octopod-server-container; }
