{ config, lib, pkgs, ... }:

{
  users.users.git = {
    isNormalUser = false;
    isSystemUser = true;
    home = "/home/git";
    description = "User that manages git";
    extraGroups = [ "git" ];
    createHome = true;  
		shell = "${pkgs.git}/bin/git-shell";	
    group = "git";
  };
	users.groups.git.members = [ "git" "stranger" ];
}
