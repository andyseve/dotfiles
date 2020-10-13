{ config, pkgs, ... }:

{
  services.xserver.videoDrivers = [ "nvidiaLegacy390" ];
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;

  environment.systemPackages = with pkgs; [
	  cudatoolkit
		python3Packages.tensorflow
		python3Packages.pytorch
    linuxPackages.nvidia_x11_legacy390
		vulkan-loader vulkan-validation-layers vulkan-tools
  ];
}
