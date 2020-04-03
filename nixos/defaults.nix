{ config, pkgs, ... }:

let
	allHieOverlay = (self: super: {
		all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
	});
	unstableOverlay = (self: super: {
		unstable = import (fetchTarball "https://github.com/NixOs/nixpkgs-channels/archive/nixos-unstable.tar.gz") { config = config.nixpkgs.config; };
	});
in
{
  # Select internationalisation properties
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

	fonts = {
		enableFontDir = true;
		fonts = with pkgs; [
			inconsolata
			fira
			fira-mono
			fira-code
			fira-code-symbols
			powerline-fonts
			font-awesome-ttf
			lohit-fonts.marathi
		];

		fontconfig = {
			penultimate.enable = false;
			defaultFonts = {
				monospace = [ "Fira Code" ];
			};
		};
	};

	# NixPkgs Configuration
	nixpkgs.config = {
		allowUnfree = true;
	};
	nixpkgs.overlays = [ allHieOverlay unstableOverlay ];

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
		# Version Control / Archive
		git subversion mercurial bazaar
		unzip zip unrar

		# Debug / Monitor / Analysis
		htop iotop powertop iftop
		ltrace strace
		pciutils usbutils lshw
		smartmontools

		# Networking
		inetutils
		nmap wireshark
		tor openvpn
		wget curl rsync unison

		# Linux shell utils
		zsh bash
		fzf silver-searcher
		neofetch
		tree
		tmux screen

		# Encryption
		gnupg
		
		# CLI programs
		ranger
		weechat
		vim neovim emacs26-nox
		w3m
		youtube-dl
		taskwarrior timewarrior

		
		# Dev Tools
		gnumake cmake
		gcc clang llvm ccls
		(python3.withPackages(ps: with ps; [
			pip virtualenv
			numpy scipy matplotlib
			pylint
		]))
		unstable.haskellPackages.ghc
		unstable.haskellPackages.hoogle
		all-hies.latest
		openjdk nodejs

		# Latex
		texlive.combined.scheme-full

		# Man Pages
		man man-pages
  ];

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
	# PulseAudio Full - bluetooth headsets support
	hardware.pulseaudio.package = pkgs.pulseaudioFull;
	hardware.bluetooth.enable = true;

	# 32 bit support, required for steam
	hardware.opengl.driSupport32Bit = true;
	hardware.pulseaudio.support32Bit = true;
	hardware.steam-hardware.enable = true;

	# backlight using light
	programs.light.enable = true;
}
