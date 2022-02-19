{ config, lib, pkgs, ... }:

{
  services.mopidy = {
    enable = true;
    extensionPackages = [
      pkgs.mopidy-ytmusic
    ];
  };
}
