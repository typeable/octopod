{ sources ? import ./sources.nix
, b2b-helm-pkgs ? import ../b2b-helm { }

, migrations ? "please use '--arg migration <value>'"
, server-cert ? "please use '--arg server-cert <value>'"
, server-key ? "please use '--arg server-key <value>'"
, passwd ? "please use '--arg passwd <value>'"
, ssh-config ? "please use '--arg ssh-config <value>'"
, ssh-known-hosts ? "please use '--arg ssh-known-hosts <value>'"
, deploy-key ? "please use '--arg deploy-key <value>'"
, client-cert ? "please use '--arg client-cert <value>'"
, client-key ? "please use '--arg client-key <value>'"
}:
with {
  overlay = _: pkgs:
    with pkgs; rec {
      dm = haskellPackages.dm-static;

      cacert' = cacert.overrideAttrs (o: {
        fixupPhase = ''
          cat $out/etc/ssl/certs/* > $out/etc/ssl/certs/ca-certificates.crt
        '';
      });

      dms-container = dockerTools.buildImage {
        name = "dms-container-slim";
        contents =
          [ dm git b2b-helm-tool kubernetes-helm2-bin kubectl coreutils bash openssh gnugrep cacert' ];

        runAsRoot = ''
          mkdir /tmp

          mkdir /app
          cp -av ${dm}/bin/dms-exe /app/dms-exe

          mkdir /migrations
          cp -av ${migrations}/* /migrations/

          mkdir /tls
          cp -av ${server-cert} /tls/server_cert.pem
          cp -av ${server-key} /tls/server_key.pem

          mkdir /tls_store
          cp -av ${server-cert} /tls_store/server_cert.pem

          cp -av ${passwd} /etc/passwd

          mkdir -p /root/.ssh
          cp -av ${ssh-config} /root/.ssh/config
          cp -av ${ssh-known-hosts} /root/.ssh/known_hosts
          cp -av ${deploy-key} /root/.ssh/deploy.key
          chown root /root/.ssh/*
          chmod 400 /root/.ssh/*

          mkdir -p /usr/local/bin
          cp ${kubernetes-helm2-bin}/helm /usr/local/bin/
          chmod +x /usr/local/bin/helm
        '';

        config = {
          Entrypoint = [ "/app/dms-exe" ];
          Cmd = [
            "--port"
            "4000"
            "--db"
            "host='127.0.0.1' port=5432 user='dm' password='dm'"
            "--db-pool-size"
            "10"
            "--tls-cert-path"
            "/tls/server_cert.pem"
            "--tls-key-path"
            "/tls/server_key.pem"
            "--tls-store-path"
            "/tls_store"
          ];
        };
      };

      dmc-container = dockerTools.buildImage {
        name = "dmc-container-slim";
        contents = [ dm coreutils bash emacs vim ];

        runAsRoot = ''
          mkdir /app
          cp -av ${dm}/bin/dmc-exe /app/dmc-exe

          cp ${client-cert} /cert.pem
          cp ${client-key} /key.pem
        '';

        config = {
          Entrypoint = [ "/app/dmc-exe" ];
          Env = [ "EDITOR=${vim}/bin/vim" ];
        };
      };

      niv = import sources.niv { };

      b2b-helm-tool = buildGoPackage rec {
        version = "0.1";
        pname = "b2b-helm-tool";
        goPackagePath = "github.com/aviora/b2b-helm";
        src = ../b2b-helm/tool;
        goDeps = ../b2b-helm/tool/deps.nix;
      };

      kubernetes-helm2-bin = fetchzip {
        url = "https://get.helm.sh/helm-v2.16.5-linux-amd64.tar.gz";
        sha256 = "1r91i8gy3zsgwx1fr2n6syspjrqv822ngf54db8xycskv4p5mxxj";
      };

      haskellPackages = pkgs.haskellPackages.override {
        overrides = hself: hsuper: {

          deriving-aeson = hsuper.callPackage (stdenv.mkDerivation ({
            name = "deriving-aeson";
            buildCommand = ''
              ${hsuper.cabal2nix}/bin/cabal2nix file://${sources.deriving-aeson} > $out
            '';
          })) { };

          dm = hsuper.callPackage ../default.nix { };

          dm-static = haskell.lib.justStaticExecutables
            (hsuper.callPackage ../default.nix { });
        };
      };
    };
};
import sources.nixpkgs # and use them again!
{
  overlays = b2b-helm-pkgs.overlays ++ [ overlay ];
  config = { };
}
