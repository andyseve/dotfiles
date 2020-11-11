{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    unstable.factorio-headless   
  ];

  services.factorio = {
    enable = true;
    package = "unstable.factorio-headless";
    port = "34197";
    game-name = "Stranger's game";
  };
}
