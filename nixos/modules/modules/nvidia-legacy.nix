{ config, pkgs, ... }:

{
  services.xserver.videoDrivers = [ "nvidiaLegacy390" ];
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;

  environment.systemPackages = with pkgs; [
    linuxPackages.nvidia_x11_legacy390
  ];
}
