args: 

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions (map (f: f args) [  
      (import extensions/array-exceptions.nix)
      (import extensions/buffers.nix)
      (import extensions/prim-bool.nix)
      (import extensions/prim-compat.nix)
      (import extensions/utf8-text.nix)
    ]) pkgs pkgs;
}