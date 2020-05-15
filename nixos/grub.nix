# This contains example grub config. The disk id is for the laptop.
# Fails to save the last booted option
# If savedefault starts working, then will switch to grub
{ config, lib, pkgs, ... }

{
  # grub settings
  boot.loader.grub = {
    enable = false;
    efiSupport = true;
    device = "nodev";
    #useOSProber = true;
    default = "saved";
    extraPerEntryConfig = ''savedefault=1'';
    extraEntries = ''
      menuentry 'Windows' --class windows {
        insmod part_gpt
        insmod fat
        insmod search_fs_uuid
        insmod chain
        search --fs-uuid --set=root 6858-AFE7
        chainloader /EFI/Microsoft/Boot/bootmgfw.efi
        savedefault=1
      }
    '';
  };
}
