{ config, lib, pkgs, ... }:

{
  users.users.git = {
    isNormalUser = false;
    home = "/home/git";
    description = "User that manages git";
    extraGroups = [ "git" ];
    createHome = true;  
		shell = "${pkgs.git}/bin/git-shell";	
  };
	users.groups.git.members = [ "git" "stranger" ];
}
