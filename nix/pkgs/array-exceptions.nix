{ mkDerivation, base, fetchgit, lib, template-haskell }:
mkDerivation {
  pname = "array-exceptions";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/array-exceptions";
    sha256 = "0j8mlj03rgch7mj1z6cckx3j0cgfd8arf4ma7g01f022r0sz8g42";
    rev = "cf901d818d66b75e57af4812c860c0df14907ac2";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base template-haskell ];
  homepage = "https://github.com/riz0id/array-exceptions";
  description = "TODO";
  license = lib.licenses.isc;
}
