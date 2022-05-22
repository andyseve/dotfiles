{ config, pkgs, ... }:

{
  # firmware settings
  # additional firmware drivers
  boot.kernelModules = [ "iwlwifi" ];
  # iwlwifi settings
  # needed to stop multiple physical restarts.
  boot.extraModprobeConfig = ''
    options iwlwifi 11n_disable=8
    '';
  # logitech
  hardware.logitech.wireless.enable=true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi = {
    canTouchEfiVariables = true;
  };

  boot.plymouth.enable = true;

  # location
  location.provider = "geoclue2";

  # Select internationalisation properties
  console = {
    keyMap = "us";
    font = "Lat2-Terminus16";
  };
  i18n.defaultLocale = "en_US.UTF-8";

  fonts = {
    fonts = with pkgs; [
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      fira-code
      (nerdfonts.override { fonts = [ "FiraCode" "FiraMono" ]; })
      lohit-fonts.marathi
    ];
  };

  # Allow non-free drivers
  hardware.enableRedistributableFirmware = true;

  # Nix configuration
  nix = {
    autoOptimiseStore = true;

    # Garbage Collector
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

	# NixPkgs Configuration
	nixpkgs.config.allowUnfree = true;

	nixpkgs.overlays = import ../overlays;

  environment.enableDebugInfo = true;
  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    # Drivers
    firmwareLinuxNonfree


		# Version Control / Archive
		git subversion
		unzip zip unrar

		# Debug / Monitor / Analysis
		htop iotop powertop iftop
		ltrace strace
		pciutils usbutils lshw
		smartmontools lm_sensors
    dmidecode

    # Volume
    pavucontrol

		# Networking
    iputils
		nmap wireshark
		tor openvpn
		wget curl rsync
    networkmanager networkmanager-openvpn

		# Linux shell utils
		fzf silver-searcher
		neofetch
		tree
		tmux screen
    pdftk

		# Encryption
		gnupg
    pinentry-gtk2
		
		# CLI programs
		ranger
		weechat
    unstable.neovim # load latest neovim
		w3m
		youtube-dl
		taskwarrior timewarrior
    khal khard # calendars and contacts
    vdirsyncer # vdirsyncer
    fswebcam 
    unstable.pubs # biblography manager
    (pass.withExtensions
      (exts: [
      exts.pass-otp 
      ])
    ) # password manager
    mailutils

		
		# Dev Tools
		gnumake
		gcc ccls

    (python3.withPackages # installing python3 with packages
      (ps: with ps; [
        pylint jedi
        numpy scipy matplotlib
        scikitlearn
        pandas
        jupyter notebook
      ])
    )
    python3Packages.pip
    python3Packages.argcomplete

    unstable.haskellPackages.ghc
    unstable.haskellPackages.hoogle
    unstable.haskellPackages.cabal-install
    unstable.haskellPackages.haskell-language-server

		jdk11 nodejs

		# Latex
		texlive.combined.scheme-full

		# Man Pages
		man man-pages

    # dotfiles
    anish-dotfiles
  ];

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [
    gutenprint
    hplip
    hplipWithPlugin
  ];

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

  programs = {
    # zsh
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    # backlight
    light.enable = true;
    # gnupg
    gnupg.agent = {
      enable = true;
      enableBrowserSocket = true;
      enableSSHSupport = true;
      pinentryFlavor = "gtk2";
    };
  };
}
