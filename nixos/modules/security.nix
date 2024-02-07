# security and user settings to generate users by default.
{ 
  config,
  lib,
  pkgs,
  ...
}:

{
  security.apparmor.enable = true;
  security.rtkit.enable = true;
  security.polkit.enable = true;
  security.sudo.enable = true;

  users.users.stranger = {
    isNormalUser = true;
    home = "/home/stranger";
    description = "Anish Sevekari";
    extraGroups = [ "wheel" "networkmanager" "video" ]; # Enable ‘sudo’ for the user.
    createHome = true;
    shell = "${pkgs.zsh}/bin/zsh";
  };

  users.users.root.shell = "${pkgs.zsh}/bin/zsh";
}
