{ config, pkgs, ... }:

{
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;

  environment.systemPackages = with pkgs; [
    cudatoolkit
    python3Packages.tensorflow-build_2
    python3Packages.pytorch
    linuxPackages.nvidia_x11
    vulkan-loader vulkan-validation-layers vulkan-tools
  ];
}
