{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, primitive
, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "utf8-text";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/utf8-text";
    sha256 = "02zzadwc128wdvxq4w5m6bana92cpsyccvdmgaw7d7psn50axh9s";
    rev = "9a797a203fee504fcde171d4c2e3e76b87d4b932";
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
