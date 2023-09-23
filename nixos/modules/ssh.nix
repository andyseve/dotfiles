{ config, lib, pkgs, ... }:

{
  # OpenSSH daemon.
  services.openssh = {
    enable = true;
    allowSFTP = true;
    settings.forwardX11 = true;
    settings.logLevel = "VERBOSE";
    ports = [ 22 ];
    settings.permitRootLogin = "no";
    settings.passwordAuthentication = false;

    extraConfig = ''
      # Authentication
      LoginGraceTime 2m
      StrictModes yes
      MaxAuthTries 3
      MaxSessions 5

      PubkeyAuthentication yes
      PermitEmptyPasswords no

      # Config
      PrintLastLog yes
      TCPKeepAlive yes
      '';
    };

  # ssh agent
  # comment if using gpg-agent
  #programs.ssh = {
    #startAgent = true;
    #agentTimeout = "30m";
  #};
}
