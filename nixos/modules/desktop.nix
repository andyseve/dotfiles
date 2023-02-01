{ config, pkgs, ... }:

{
  # Xserver settings
	services.xserver = {
		enable = true;
		layout = "us";
		# xkbOptions = "eurosign:e"
		
		libinput = {
			enable = true;
      touchpad = {
        accelProfile = "flat";
        disableWhileTyping = true;
      };
		};

    # Desktop
    desktopManager.xterm.enable = false;
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
    trayer
		rofi rofi-pass
		picom                            # transparancy
		dunst libnotify                  # notifications
		feh
    xdotool
    xorg.xmodmap xorg.xrandr xorg.libXinerama
		scrot                            # screenshots
    xclip
    volumeicon                       # volume for trayer

    networkmanager_dmenu

    papirus-icon-theme               # Papirus-icons
    numix-solarized-gtk-theme        # Numix theme

		# Desktop Programs
		firefox google-chrome 
		vlc
		zathura
		gimp inkscape
		alacritty rxvt_unicode-with-plugins
    discord unstable.zoom-us slack
    pavucontrol
		unstable.ytmdesktop
    unstable.dropbox

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
    autorandr.enable = true;
  };

}
