{ config, lib, pkgs, ... }:

{
  services.mopidy = {
    enable = true;
    extensionPackages = [
      pkgs.mopidy-mpd
      pkgs.mopidy-iris
      pkgs.mopidy-podcast
      pkgs.mopidy-ytmusic
    ];
    configuration = ''
      [core]
      max_tracklist_length = 10000
      restore_state = true

      [audio]
      output = pulsesink server=127.0.0.1

      [file]
      enabled = true
      media_dirs = 
        /media/storage/Music|Music

      [local]
      enabled = true
      music = /media/storage/Music

      [mpd]
      enabled = true
      hostname = 127.0.0.1

      [youtube]
      enabled = true
      allow_cache = true
      autoplay_enabled = true
    '';
  };
}
