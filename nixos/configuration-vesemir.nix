# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/modules/defaults.nix
      /etc/nixos/modules/desktop.nix
      /etc/nixos/modules/nvidia-legacy.nix
      /etc/nixos/modules/sound.nix
      /etc/nixos/modules/security.nix
      /etc/nixos/modules/ssh.nix
      /etc/nixos/modules/git.nix
      /etc/nixos/modules/taskserver.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.plymouth.enable = true;

  # Timezone settings
  time.timeZone = "America/New_York";
  time.hardwareClockInLocalTime = false;  

  # Defining mount points
  # Mounting Home
  fileSystems."/home" =
  { device = "/dev/disk/by-label/home";
    fsType = "ext4";
    options = [ "defaults" ];
  };

  networking.hostName = "vesemir"; # Define your hostname.
  networking.networkmanager.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp2s0f1.useDHCP = true;
  networking.interfaces.wlp3s0f0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # Import /etc/nixos/ssh.nix

  # Open ports in the firewall.
  networking.firewall.enable = true;
  networking.firewall.allowPing = false;
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  services.xserver.xrandrHeads = [ 
    {
      output = "eDP-1";
      primary = true;
    }
    {
      output = "DP-1";
    }
  ];

  # Power actions
  services.logind = {
    killUserProcesses = false;
    lidSwitch = "hibernate";
    lidSwitchExternalPower = "ignore";
    lidSwitchDocked = "ignore";
  };


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.11"; # Did you read the comment?
}

