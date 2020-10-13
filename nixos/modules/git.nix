{ config, lib, pkgs, ... }:

{
  users.users.git = {
    isNormalUser = false;
    home = "/home/git";
    description = "User that manages git";
    extraGroups = [ "git" ];
    createHome = true;    
  };
  users.users.stranger.extraGroups = [ "git" ];
}
