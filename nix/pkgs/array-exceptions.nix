{ mkDerivation, base, fetchgit, lib, template-haskell }:
mkDerivation {
  pname = "array-exceptions";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/array-exceptions";
    sha256 = "1pgil9jafv8pzxcr5dayr9a0qp2rc5h9kvaxj93mly294jyvinha";
    rev = "826f44ca30d1eb44a3a93bcff32856c72945038c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base template-haskell ];
  homepage = "https://github.com/riz0id/array-exceptions";
  description = "TODO";
  license = lib.licenses.isc;
}
