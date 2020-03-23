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
