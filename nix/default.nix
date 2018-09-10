######
#
# Author: Brandon Barker
#
######

let
  pkgs    = import <nixpkgs> {};
in rec {
  build   = pkgs.haskellPackages.callPackage ./hico.nix { };
}
