{ pkgs ? import<nixpkgs> {} }:

pkgs.mkShell {
  name = "c++";

  buildInputs = with pkgs; [
    gnumake
    gcc
    ccls
  ];
}
