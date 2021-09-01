# Need nvidia-offload for laptop cards
# info: nixos.wiki/wiki/Nvidia
{ config, pkgs, ... }:

{
  services.xserver.videoDrivers = [ "modesetting" "nvidia" ];

  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_390;

  hardware.nvidia.prime = {
    sync.enable = true;
    nvidiaBusId = "PCI:4:0:0";
    intelBusId  = "PCI:0:2:0";
  };

  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;

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
