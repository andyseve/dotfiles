{ mkDerivation, base, lib, xmobar }:
mkDerivation {
  pname = "xmobar-anish";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base xmobar ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
