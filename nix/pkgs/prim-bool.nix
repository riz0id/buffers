{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-bool";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-bool";
    sha256 = "0c02ciibgf11y2q4syr21mbvcwj3026672lx9l06z6rz11kx77ac";
    rev = "40fa894b4082a52c9653c1e12739b0bfdbb12471";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base ghc-prim prim-compat template-haskell
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-bool";
  description = "Unboxed booleans";
  license = lib.licenses.isc;
  mainProgram = "example";
}
