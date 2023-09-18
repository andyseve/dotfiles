# Need nvidia-offload for laptop cards
# info: nixos.wiki/wiki/Nvidia
{ config, lib, pkgs, ... }:

{
  #services.xserver.videoDrivers = [ "modesetting" "nvidia" ];
  services.xserver.videoDrivers = [ "nvidia" ];
  #services.xserver.videoDrivers = lib.mkDefault [ "nouveau" "modesetting" ];

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.legacy_390;
    prime = {
      sync.enable = lib.mkDefault true;
      offload.enable = lib.mkDefault 990 true;
      # Hardware should specify bus id for nvidia and intel
      nvidiaBusId = "PCI:4:0:0";
      intelBusId  = "PCI:0:2:0";
    };
    modesetting.enable = false;
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = [ pkgs.vaapiIntel ];
  };

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override {
      enableHybridCodec = true;
    };
  };

  boot.blacklistedKernelModules = lib.mkDefault [ "nouveau" ];

  #services.xserver.serverLayoutSection = ''
    #Identifier "Multihead"
		#Screen      0  "Screen0" 0 0
		#Screen      1  "Screen1" RightOf "Screen0"
		#InputDevice    "Mouse0" "CorePointer"
		#InputDevice    "Keyboard0" "CoreKeyboard"
    #Option Xinerama "ON"
  #'';

  # Intel driver settings
  #services.xserver.videoDrivers = [ "intel" ];
  #services.xserver.deviceSection = ''
    #Option "DRI" "2"
    #Option "TearFree" "true"
  #'';
}
