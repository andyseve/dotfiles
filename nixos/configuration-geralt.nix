# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      ./modules/defaults.nix
      ./modules/desktop.nix
      ./modules/nvidia.nix
      ./modules/ssh.nix
      ./modules/users.nix
    ];

	# Timezone settings
	time.timeZone = "America/New_York";
	time.hardwareClockInLocalTime = true;
  services.localtime.enable = true;

  # Defining mount points
	# Mounting Storage
  fileSystems."/media/storage" =
  { device = "/dev/disk/by-label/Storage";
    fsType = "ntfs";
    options = [ "auto" "rw" "nosuid" "nofail" "user" "uid=1000" "gid=100" "exec" "umask=022"];
  };

  networking.hostName = "geralt"; # Define your hostname.
  networking.networkmanager.enable = true;  # Enables wireless support via nm

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
  # Note that ssh works without services.openssh.enable

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

	# power actions
	services.logind = {
		killUserProcesses = false;
		extraConfig = "IdleAction=suspend\nIdleActionSec=300\n";
	};
  
	security.sudo.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # User account info is in ./modules/users.nix

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.11"; # Did you read the comment?
}

