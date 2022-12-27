{ mkDerivation, base, fetchgit, lib, template-haskell }:
mkDerivation {
  pname = "array-exceptions";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/array-exceptions";
    sha256 = "0c184s6bkzlb5x1z3xf1ym7fzdalf9whg2s4qjmhiq22wxnvnkb0";
    rev = "fe015ec267b8744d06b85227702d293fece44625";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base template-haskell ];
  homepage = "https://github.com/riz0id/array-exceptions";
  description = "TODO";
  license = lib.licenses.isc;
}
