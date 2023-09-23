{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.cudaSupport = true;

  services.xserver.videoDrivers = [ "nvidia" ];
  # hardware.opengl.enable = true;
  # hardware.opengl.driSupport32Bit = true;

  environment.systemPackages = with pkgs; [
    cudatoolkit
    python3Packages.torch
  ];
}
