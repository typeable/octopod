let
    config = rec {
        packageOverrides = pkgs: rec {
            docker-container = pkgs.dockerTools.buildImage {
                name = "dms-container";
                config.Cmd = [ "${haskellPackages.project}/bin/dms-exe" ];
            };

            docker-container-slim = pkgs.dockerTools.buildImage {
                name = "dms-container-slim";
                config.Cmd = [ "${haskellPackages.project-slim}/bin/dms-exe" ];
            };

        haskellPackages = pkgs.haskellPackages.override {
            overrides = haskellPackagesNew: haskellPackgesOld: rec {
                project = pkgs.haskell.lib.overrideCabal (
                    haskellPackagesNew.callPackage ./default.nix {}
                )
                ( oldDerivation: {}
                );

                project-slim = pkgs.haskell.lib.overrideCabal (
                    pkgs.haskell.lib.justStaticExecutables (
                        haskellPackagesNew.callPackage ./default.nix {}
                    )
                )
                ( oldDerivation: {}
                );
            };
        };
        };

    };

    pkgs = import <nixpkgs> { inherit config; system = "x86_64-linux"; };
in
    {
        docker-container = pkgs.docker-container;
        docker-container-slim = pkgs.docker-container-slim;
    }
