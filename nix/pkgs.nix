args: 

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions (map (f: f args) [  
      (import extensions/array-exceptions.nix)
      (import extensions/buffers.nix)
    ]) pkgs pkgs;
}