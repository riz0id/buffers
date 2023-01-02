{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, primitive
, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "utf8-text";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/utf8-text";
    sha256 = "0ahhldjm8hafmdmc20x5kyvxabrjb3wbd84alpm8rcdsq36a321z";
    rev = "3c17d0e6879d5f25f08008d66595191f99b6cb28";
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
