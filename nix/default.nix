{ sources ? import ./sources.nix, b2b-helm-pkgs ? import ../b2b-helm { } }:
with {
  overlay = _: pkgs:
    with pkgs; rec {
      dm = haskellPackages.dm-static.overrideAttrs (oldAttrs: {
        installPhase = oldAttrs.installPhase + ''
          mkdir $out/migrations/
          cp -av ./migrations/deploy $out/migrations/
          cp -av ./migrations/revert $out/migrations/
          cp -av ./migrations/verify $out/migrations/
          cp -av ./migrations/sqitch.plan $out/migrations/
        '';
      });

      dms-container = dockerTools.buildImage {
        name = "dms-container-slim";
        contents =
          [ dm git b2b-helm-tool kubernetes-helm2 kubectl coreutils bash ];
        config = {
          Entrypoint = [ "${dm}/bin/dms-exe" ];
          Cmd = [
            "--port"
            "4000"
            "--db"
            "host='127.0.0.1' port=5432 user='dm' password='dm'"
            "--db-pool-size"
            "10"
          ];
          Volumes = { "/migrations" = { }; };
          extraCommands = ''
            cp -av ${dm}/migrations/deploy $out/migrations/
            cp -av ${dm}/migrations/revert $out/migrations/
            cp -av ${dm}/migrations/verify $out/migrations/
            cp -av ${dm}/migrations/sqitch.plan $out/migrations/
          '';
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
