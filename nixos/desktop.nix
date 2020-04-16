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
    desktopManager.xterm.enable = false;
		displayManager.sddm.enable = true;
		windowManager.xmonad = {
			enable = true;
			enableContribAndExtras = true;
			extraPackages = haskellPackages: [
				haskellPackages.xmonad-contrib
				haskellPackages.xmonad-extras
				haskellPackages.xmonad
        haskellPackages.xmobar
			];
		};
		windowManager.default = "xmonad";
	};

  hardware.opengl.extraPackages = with pkgs; [ libva ];

	environment.systemPackages = with pkgs; [	
		# WindowManager Core
		unstable.haskellPackages.xmobar
		rofi
		compton
		dunst libnotify
		feh
		xdotool xorg.xmodmap xorg.xrandr
		scrot
    xclip

		# Desktop Programs
		firefox google-chrome vivaldi
		vlc
		zathura
    vimHugeX emacs
		libreoffice
		gimp
		alacritty rxvt_unicode-with-plugins
    steam
		unstable.discord unstable.zoom-us
		deluge
		unstable.google-play-music-desktop-player
	];

  services = {
    phylock = {
      enable = true;
      allowAnyUser = true;
    };
  };
}
