{ config, pkgs, ... }:

{
	environment.systemPackages = with pkgs; [	
    steam
    steam-original
    steam-runtime
  ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  }

	hardware.steam-hardware.enable = true;
}
