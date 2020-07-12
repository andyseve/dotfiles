let
  pkgs = import <nixos> {};

in
  (pkgs.haskellPackages.callPackage ./default.nix {}).env
