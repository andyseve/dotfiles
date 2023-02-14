{ config, pkgs, ... }:

{
  imports = [ <unstable/nixos/modules/services/games/factorio.nix> ];
  disabledModules = [ "services/games/factorio.nix" ];

  services.factorio = {
    enable = true;
    game-password = "ddpv314";
    package = pkgs.unstable.factorio-headless;
    port = 34197;
    game-name = "Stranger's game";
    saveName = "stranger1";
  };
}
