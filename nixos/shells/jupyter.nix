let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = [
    pkgs.python3
    pkgs.python3Packages.pip

    pkgs.python3Packages.jupyter
    pkgs.python3Packages.notebook

    pkgs.python3Packages.numpy
    pkgs.python3Packages.scipy
    pkgs.python3Packages.matplotlib
    pkgs.python3Packages.pytorch
  ];
  shellHook = ''
    # Tells pip to put packages into $PIP_PREFIX instead of the usual locations.
    # See https://pip.pypa.io/en/stable/user_guide/#environment-variables.
    export PIP_PREFIX=$(pwd)/_build/pip_packages
    export PYTHONPATH="$PIP_PREFIX/${pkgs.python3.sitePackages}:$PYTHONPATH"
    export PATH="$PIP_PREFIX/bin:$PATH"
    unset SOURCE_DATE_EPOCH
    jupyter notebook
  '';
}
