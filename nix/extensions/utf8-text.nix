{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        utf8-text = self.callPackage ../packages/utf8-text.nix { };
      });
    };
  };
}