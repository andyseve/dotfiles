{ config, pkgs, ... }:

{
  # Xserver settings
	services.xserver = {
		enable = true;
		layout = "us";
		# xkbOptions = "eurosign:e"
		
		# Touchpad
		libinput = {
			enable = true;
			accelProfile = "flat";
			disableWhileTyping = true;
		};

    # Desktop
    desktopManager.xterm.enable = false;
		displayManager.sddm.enable = true;
    displayManager.defaultSession = "none+xmonad";
		windowManager.xmonad = {
			enable = true;
			enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
		};

    # Wacom
    wacom.enable = true;
	};

  hardware.opengl.extraPackages = with pkgs; [ libva ];

	environment.systemPackages = with pkgs; [	
		# WindowManager Core
    haskellPackages.xmobar
		rofi rofi-pass
		picom                            # transparancy
		dunst libnotify                  # notifications
		feh
		xdotool xorg.xmodmap xorg.xrandr
		scrot                            # screenshots
    xclip

    networkmanager_dmenu

    papirus-icon-theme               # Papirus-icons
    numix-solarized-gtk-theme        # Numix theme

		# Desktop Programs
		firefox google-chrome 
		vlc
		zathura
		libreoffice
		gimp inkscape
		alacritty rxvt_unicode-with-plugins
    steam
		unstable.discord unstable.zoom-us unstable.slack
		deluge
		unstable.google-play-music-desktop-player

    # Helper CLI tools
    streamlink
	];

  environment.variables = {
    GTK_THEME = "NumixSolarizedDarkBlue";
    GTK_ICON_THEME = "Papirus-Dark";
  };

  services = {
    physlock = {
      enable = true;
      allowAnyUser = true;
    };
    redshift = {
      enable = true;
      temperature.day = 6500;
      temperature.night = 3000;
    };
    geoclue2.enable = true;
  };
}
