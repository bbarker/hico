######
#
# Author: Brandon Barker
#
######

with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "hico-stack";
  buildInputs = [
    gmp
    pkgconfig
    SDL2
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib::$LD_LIBRARY_PATH
  '';  
}

