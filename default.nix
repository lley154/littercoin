{ source-repo-override ? { } }:
########################################################################
# default.nix -- The top-level nix build file for littercoin.
#
# This file defines various attributes that are used for building and
# developing littercoin.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   littercoin: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix { inherit source-repo-override; };

  inherit (packages) pkgs littercoin;
  project = littercoin.haskell.project;
in
{
  inherit pkgs littercoin;

  inherit project;
}
