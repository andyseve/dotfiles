{ config, pkgs, ... }:

{
  services.xserver.videoDrivers = [ "nvidia" ];
  # hardware.opengl.enable = true;
  # hardware.opengl.driSupport32Bit = true;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.cudaSupport = true;

  environment.systemPackages = with pkgs; [
    # cudatoolkit
    #python3Packages.tensorflow
    # python3Packages.pytorch
  ];
}
