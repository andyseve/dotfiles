{
  config,
  lib,
  pkgs ? import <nixos>,
  ...
}:

{
  # OpenSSH daemon.
  services.openssh = {
    enable = true;
    allowSFTP = true;
    settings.X11Forwarding = false;
    settings.LogLevel = "VERBOSE";
    ports = [ 22 ];
    settings.PermitRootLogin = "no";
    settings.PasswordAuthentication = false;

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
  # comment out to use gpg agent
  #programs.ssh = {
    #startAgent = true;
    #agentTimeout = "30m";
  #};
}
