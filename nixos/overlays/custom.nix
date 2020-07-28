self: super:
{
  anish-dotfiles = super.callPackage ../pkgs/dotfiles.nix {};
  anish-pubs = super.callPackage ../pkgs/pubs.nix {};
}
