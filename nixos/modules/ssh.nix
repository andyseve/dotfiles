{ config, lib, pkgs, ... }:

{
  # OpenSSH daemon.
  services.openssh = {
		enable = true;
		allowSFTP = true;
		forwardX11 = true;
		logLevel = "VERBOSE";
		ports = [ 22 ];
		permitRootLogin = "no";
		passwordAuthentication = false;
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

}
