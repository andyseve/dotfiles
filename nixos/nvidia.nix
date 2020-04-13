{ config, pkgs, ... }:

{
  services.xserver.videoDrivers = [ "nvidia" ];

  environment.systemPackages = with pkgs; [
    cudatoolkit_10
    python3Packages.tensorflow
    python3Packages.pytorch
  ];
}
