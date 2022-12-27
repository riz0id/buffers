{ ghc }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs) 
    mkShell;

  inherit (pkgs.haskell.packages."${ghc}") 
    array-exceptions
    buffers
    hlint
    haskell-language-server
    showable
    utf8-text;
}