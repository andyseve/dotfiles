{ config, pkgs, ... }:

{
  services.xserver.videoDrivers = [ "intel" ];
  services.xserver.deviceSection = ''
    Option "DRI" "2"
    Option "TearFree" "true"
  '';

  #services.xserver.videoDrivers = [ "nvidiaLegacy390" ];
  #hardware.opengl.enable = true;
  #hardware.opengl.driSupport32Bit = true;

  environment.systemPackages = with pkgs; [
    cudatoolkit
    python3Packages.tensorflow-build_2
    python3Packages.pytorch
    linuxPackages.nvidia_x11_legacy390
    vulkan-loader vulkan-validation-layers vulkan-tools
  ];
}
