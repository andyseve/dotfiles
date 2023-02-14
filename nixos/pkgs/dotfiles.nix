{
  pkgs   ? import <nixpkgs> {},
  lib    ? import pkgs.lib,
  stdenv ? import pkgs.stdenv,
  ...
}:

# Copy files from dotfiles folder to nix-store

with pkgs;
stdenv.mkDerivation {
  pname = "dotfiles";
  version = "1";
  src = ~/dotfiles/bin;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/bin
    install -t $out/bin ./*
    '';
}
