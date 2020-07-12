let
  pkgs = import <nixos> {};
in 
  pkgs.mkShell {
    name = "python-for-dota_info";

    buildInputs = [
      pkgs.python3
      pkgs.python3Packages.pip
      pkgs.python3Packages.beautifulsoup4
    ];
    
    shellHook = ''
    alias pip="PIP_PREFIX='$(pwd)/_build/pip_packages' \pip"
      export PYTHONPATH="$(pwd)/_build/pip_packages/lib/python3.7/site-packages:$PYTHONPATH"
      unset SOURCE_DATE_EPOCH
    '';
  }


