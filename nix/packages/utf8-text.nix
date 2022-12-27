{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, primitive
, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "utf8-text";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/utf8-text";
    sha256 = "0x7wwwzpmxn55zvp95j6plm9jsxa1rb33wcd5s2k4f8lqmy01jhs";
    rev = "1fcbb276669b93b894cb344ec111e8a2bc75aa87";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim primitive template-haskell
  ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/utf8-text";
  description = "TODO";
  license = lib.licenses.isc;
}
