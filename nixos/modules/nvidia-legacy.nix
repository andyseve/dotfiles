# Need nvidia-offload for laptop cards
# info: nixos.wiki/wiki/Nvidia
{ config, pkgs, ... }:

{
  #services.xserver.videoDrivers = [ "modesetting" "nvidia" ];
  services.xserver.videoDrivers = [ "nouveau" "modesetting" ];

  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_390;

  hardware.nvidia.prime = {
    sync.enable = true;
    nvidiaBusId = "PCI:4:0:0";
    intelBusId  = "PCI:0:2:0";
  };

  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = [ pkgs.vaapiIntel ];
  };

  #services.xserver.serverLayoutSection = ''
    #Identifier "Multihead"
		#Screen      0  "Screen0" 0 0
		#Screen      1  "Screen1" RightOf "Screen0"
		#InputDevice    "Mouse0" "CorePointer"
		#InputDevice    "Keyboard0" "CoreKeyboard"
    #Option Xinerama "ON"
  #'';

  #services.xserver.videoDrivers = [ "intel" ];
  #services.xserver.deviceSection = ''
    #Option "DRI" "2"
    #Option "TearFree" "true"
  #'';


  environment.systemPackages = with pkgs; [
    cudatoolkit
    python3Packages.tensorflow
    python3Packages.pytorch
  ];
}
