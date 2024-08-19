# Need nvidia-offload for laptop cards
# info: nixos.wiki/wiki/Nvidia
{ config, lib, pkgs, ... }:

{
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.legacy_390;
    prime = {
      sync.enable = lib.mkForce true;
      offload.enable = lib.mkForce false;
      # Hardware should specify bus id for nvidia and intel
      # Bus Ids are specified in hardware-configuration
    };
    nvidiaSettings = true;
    modesetting.enable = true;
  };


  # nixpkgs.config.packageOverrides = pkgs: {
  #   vaapiIntel = pkgs.vaapiIntel.override {
  #     enableHybridCodec = true;
  #   };
  # };

  # services.xserver.videoDrivers = [ "nouveau" "modesetting" ];
  services.xserver.videoDrivers = [ "nvidia" ];
  boot.initrd.kernelModules = [ "nvidia" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.nvidia_x11_legacy390 ];


  boot.kernelParams = [ "module_blacklist=i915" ];
}
