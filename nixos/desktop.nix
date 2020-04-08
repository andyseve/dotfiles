{ configs, pkgs, ... }:

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
		displayManager.slim.enable = true;
		windowManager.xmonad = {
			enable = true;
			enableContribAndExtras = true;
			extraPackages = haskellPackages: [
				haskellPackages.xmonad-contrib
				haskellPackages.xmonad-extras
				haskellPackages.xmonad
			];
		};
		windowManager.default = "xmonad";
	};

	environment.systemPackages = with pkgs; [	
		# WindowManager Core
		unstable.haskellPackages.xmobar
		rofi
		dunst libnotify
		feh
		compton
		xdotool xorg.xmodmap xorg.xrandr
		scrot

		# Desktop Programs
		# modularize these, should only be installed if xserver is set
		firefox google-chrome vivaldi
		vlc
		zathura
		# vimHugeX emacs
		libreoffice
		gimp
		alacritty rxvt_unicode-with-plugins
		steam
		discord zoom-us
		deluge
		google-play-music-desktop-player
	];
}
