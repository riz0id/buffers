args: 

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions (map (f: f args) [  
      (import exts/array-exceptions.nix)
      (import exts/buffers.nix)
      (import exts/prim-bool.nix)
      (import exts/prim-compat.nix)
      # (import exts/tasty-hedgehog.nix)
      (import exts/utf8-text.nix)
    ]) pkgs pkgs;
}