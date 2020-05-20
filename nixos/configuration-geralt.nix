# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
			/etc/nixos/defaults.nix
			/etc/nixos/desktop.nix
      /etc/nixos/nvidia.nix
      /etc/nixos/ssh.nix
    ];

	# Timezone settings
	time.timeZone = "America/New_York";
	time.hardwareClockInLocalTime = true;

  # Defining mount points
	# Mounting Storage
  fileSystems."/media/storage" =
  { device = "/dev/disk/by-label/Storage";
    fsType = "ntfs";
    options = [ "auto" "rw" "exec" "nosuid" "nofail" "user" "uid=1000" "gid=100" ];
  };

  networking.hostName = "geralt"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp4s0.useDHCP = true;

  # Network Proxy
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
	

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # SSHD
  # Add /etc/nixos/ssh.nix to imports

  # Firewall.
  networking.firewall.enable = true;
	networking.firewall.allowPing = false;
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  # Xrandr settings
  services.xserver.xrandrHeads = [ 
    {
      output = "DP-4";
      primary = true;
    }
    {
      output = "HDMI-0";
    }
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
	security.sudo.enable = true;
  users.users.stranger = {
    isNormalUser = true;
		home = "/home/stranger";
		description = "Anish Sevekari";
    extraGroups = [ "wheel" "networkmanager" "video" ]; # Enable ‘sudo’ for the user.
		createHome = true;
		shell = "${pkgs.zsh}/bin/zsh";
  };
	users.users.root.shell = "${pkgs.zsh}/bin/zsh";


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}

