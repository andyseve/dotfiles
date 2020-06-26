{ stdenv, python3Packages }:

stdenv.mkDerivation rec {
  name = "anish-dotfiles";
  src = /home/stranger/dotfiles;

  phases = [ "installPhase" ];

  propogatedBuildInputs = with python3Packages; [
    beautifulsoup4
  ];

  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/lib
    cp -r $src/bin $out/bin
    cp -r $src/src $out/lib
    ln -s $out/lib/dota_info/dota_info.py $out/bin/dota_info.py
  '';
}
