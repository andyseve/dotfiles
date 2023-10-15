# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./modules/defaults.nix
      ./modules/desktop.nix
      ./modules/nvidia-legacy.nix
      ./modules/sound.nix
      ./modules/security.nix
      ./modules/ssh.nix
      ./modules/git.nix
      ./modules/taskserver.nix
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
  networking.interfaces.enp2s0f1.useDHCP = true;
  networking.interfaces.wlp3s0f0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";


  # Enable the OpenSSH daemon.
  # Import /etc/nixos/ssh.nix

  # Open ports in the firewall.
  networking.firewall.enable = true;
  networking.firewall.allowPing = false;
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  # Default monitor configuration
  services.xserver.xrandrHeads = [ 
    {
      output = "HDMI-1-1";
      primary = true;
    }
    {
      output = "eDP-1-1";
    }
  ];

  # Power actions
  services.logind = {
    killUserProcesses = false;
    lidSwitch = "hibernate";
    lidSwitchExternalPower = "ignore";
    lidSwitchDocked = "ignore";
  };

  # Nvidia Prime Settings
  hardware.nvidia.prime = {
      nvidiaBusId = "PCI:4:0:0";
      intelBusId  = "PCI:0:2:0";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "23.05"; # Did you read the comment?
}

