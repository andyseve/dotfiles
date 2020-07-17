# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
			/etc/nixos/modules/users.nix
      /etc/nixos/modules/defaults.nix
      /etc/nixos/modules/desktop.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi = {
    canTouchEfiVariables = true;
  };

  boot.plymouth.enable = true;

	# Timezone settings
	time.timeZone = "America/New_York";
	time.hardwareClockInLocalTime = true;

  # Defining mount points
	# Mounting Home
  fileSystems."/home" =
  { device = "/dev/disk/by-label/home";
    fsType = "ext4";
    options = [ "defaults" ];
  };
	# Mounting Storage
  fileSystems."/media/storage" =
  { device = "/dev/disk/by-label/storage";
    fsType = "ntfs";
    options = [ "auto" "rw" "exec" "nosuid" "nofail" "user" "uid=1000" "gid=100" ];
  };

  networking.hostName = "ziraeal"; # Define your hostname.
  networking.networkmanager.enable = true;  # Enables wireless support via nm

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp1s0.useDHCP = true;

  # Network Proxy
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
	

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # OpenSSH daemon.
  # services.openssh.enable = true;

  # Firewall.
  networking.firewall.enable = true;
	networking.firewall.allowPing = false;
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
	#
  # Define a user account. Don't forget to set a password with ‘passwd’.

	# power actions
	services.logind = {
		killUserProcesses = false;
		lidSwitch = "hibernate";
		lidSwitchExternalPower = "suspend";
		lidSwitchDocked = "ignore";
		extraConfig = "IdleAction=suspend\nIdleActionSec=300\n";
	};


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}

