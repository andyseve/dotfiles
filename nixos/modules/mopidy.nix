{ config, lib, pkgs, ... }:

{
  services.mopidy = {
    enable = true;
    extensionPackages = [
      pkgs.mopidy-spotify
      pkgs.mopidy-mopify
      pkgs.mopidy-ytmusic
      pkgs.mopidy-youtube
      pkgs.mopidy-mpd
    ];
  };
}
