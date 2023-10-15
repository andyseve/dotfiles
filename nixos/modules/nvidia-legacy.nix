# Need nvidia-offload for laptop cards
# info: nixos.wiki/wiki/Nvidia

{
  config,
  lib,
  pkgs ? import <nixos>,
  ...
}:

{
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.legacy_390;
    modesetting.enable = true;
    nvidiaSettings =  true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;
    prime = {
      sync.enable = lib.mkDefault true;
      offload.enable = lib.mkDefault false;
    };
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
}
