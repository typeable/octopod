{ sources ? import ./sources.nix }:     # import the sources
with
  { overlay = _: pkgs:
      rec {
        dm = haskellPackages.dm-static.overrideAttrs(oldAttrs: {
          installPhase = oldAttrs.installPhase + ''
            mkdir $out/migrations/
            cp -av ./migrations/deploy $out/migrations/
            cp -av ./migrations/revert $out/migrations/
            cp -av ./migrations/verify $out/migrations/
            cp -av ./migrations/sqitch.plan $out/migrations/
          '';
        });

        dms-container = pkgs.dockerTools.buildImage {
          name = "dms-container-slim";
          contents = [
            dm
            b2b-helm-tool
            pkgs.kubernetes-helm2
            pkgs.coreutils
            pkgs.bash
          ];
          config = {
            Cmd = [ "${dm}/bin/dms-exe" ];
            Volumes = {
              "/migrations" = {};
            };
          extraCommands = ''
            cp -av ${dm}/migrations/deploy $out/migrations/
            cp -av ${dm}/migrations/revert $out/migrations/
            cp -av ${dm}/migrations/verify $out/migrations/
            cp -av ${dm}/migrations/sqitch.plan $out/migrations/
          '';
          };

        };

        niv = import sources.niv {};

        b2b-helm-tool = pkgs.buildGoPackage rec {
          version = "0.1";
          pname = "b2b-helm-tool";
          goPackagePath = "github.com/aviora/b2b-helm";
          src = ../b2b-helm/tool;
          goDeps = ../b2b-helm/tool/deps.nix;
        };

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {

            deriving-aeson = hsuper.callPackage(
              pkgs.stdenv.mkDerivation ({
                name = "deriving-aeson";
                buildCommand = ''
                  ${hsuper.cabal2nix}/bin/cabal2nix file://${sources.deriving-aeson} > $out
                '';
              })) {};

            dm = hsuper.callPackage ../default.nix {
            };

            dm-static = pkgs.haskell.lib.justStaticExecutables(
              hsuper.callPackage ../default.nix {}
            );
          };
        };
      };
  };
import sources.nixpkgs                  # and use them again!
  { overlays = [ overlay ] ; config = {}; }
