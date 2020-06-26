{ mkDerivation, base, stdenv, xmobar }:
mkDerivation {
  pname = "my-xmobar";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base xmobar ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
