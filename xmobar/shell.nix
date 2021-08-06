let
  pkgs = import <nixos> {};
  unstable = import <unstable> {};
  hspkgs = unstable.haskellPackages;

  haskellDeps = ps: with ps; [
    base
    xmobar
  ];

  ghc = hspkgs.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
  ];
in 
  pkgs.mkShell {
    name = "xmobar";
    buildInputs = nixPackages;
  }
