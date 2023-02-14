{ config, lib, pkgs, ... }:

{
	services.taskserver = {
		enable = true;
    fqdn = "asevekarlaptop.math.cmu.edu";
    listenHost = "::";
    listenPort = 53589;
    ipLog = true;
    openFirewall = true;

    organisations.asevekar.users = [ "Anish Sevekari" ];
	};
}
