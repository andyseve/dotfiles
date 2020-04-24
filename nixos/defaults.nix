{ config, pkgs, ... }:

let
	allHieOverlay = (self: super: {
		all-hies = import <all-hie> {};
	});
	unstableOverlay = (self: super: {
		unstable = import <unstable> { config = config.nixpkgs.config; };
	});
in
{
  # firmware settings
  # additional firmware drivers
  boot.kernelModules = [ "iwlwifi" ];
  # iwlwifi settings
  # needed to stop multiple physical restarts.
  boot.extraModprobeConfig = ''
    options iwlwifi power_save=0 swcrypto=1 11n_disable=8
    options iwlmvm power_scheme=1
    '';
  # logitech
  hardware.logitech.enable=true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # boot settings
  boot.loader.grub.default = "saved";
  boot.plymouth.enable = true;

	# Timezone settings
	time.timeZone = "America/New_York";
	time.hardwareClockInLocalTime = true;
  # location
  location.provider = "geoclue2";

  # Select internationalisation properties
  console = {
    keyMap = "us";
    font = "Lat2-Terminus16";
  };
  i18n.defaultLocale = "en_US.UTF-8";

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

  # Allow non-free drivers
  hardware.enableRedistributableFirmware = true;

	# NixPkgs Configuration
	nixpkgs.config = {
		allowUnfree = true;
	};
	nixpkgs.overlays = [ allHieOverlay unstableOverlay ];

  environment.enableDebugInfo = true;
  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    # Drivers
    firmwareLinuxNonfree

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
		taskwarrior timewarrior tasksh

		
		# Dev Tools
		gnumake cmake
		gcc clang llvm ccls
    (python3.withPackages # installing python3 with packages
			(ps: with ps; [
				pip virtualenv
				pylint
				numpy scipy matplotlib
        jupyter notebook
        mysql-connector
			])
		)
		(unstable.haskellPackages.ghcWithPackages # installing ghc with packges
			(haskellPackages: with haskellPackages; [
				xmonad xmonad-contrib xmonad-extras
				xmobar
        hoogle
			])
		)
		unstable.haskellPackages.hoogle
		all-hies.latest
		openjdk nodejs
    gdb

		# Latex
		texlive.combined.scheme-full

		# Man Pages
		man man-pages
  ];

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
	  # PulseAudio Full - bluetooth headsets support
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };
	hardware.bluetooth.enable = true;

	# 32 bit support, required for steam
	hardware.opengl.driSupport32Bit = true;
	hardware.pulseaudio.support32Bit = true;
	hardware.steam-hardware.enable = true;

	# backlight using light
	programs.light.enable = true;
}
