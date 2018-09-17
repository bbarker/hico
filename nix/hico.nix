{ mkDerivation, base, bytestring, free, mtl, sdl2, stdenv, text }:
mkDerivation {
  pname = "hico";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    bytestring
    free
    mtl
    optparse-applicative
    sdl2
    text
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/hico#readme";
  license = stdenv.lib.licenses.bsd3;
}
