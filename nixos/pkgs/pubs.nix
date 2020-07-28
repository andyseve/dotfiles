{ stdenv, fetchFromGitHub, fetchpatch, python3Packages, pkgs ? import <nixpkgs> {} }:

python3Packages.buildPythonApplication rec {
  pname = "pubs";
  version = "0.8.3";

  src = fetchFromGitHub {
    owner = "pubs";
    repo = "pubs";
    rev = "v${version}";
    sha256 = "0npgsyxj7kby5laznk5ilkrychs3i68y57gphwk48w8k9fvnl3zc";
  };

  #patches = [
    # Approved pull requests after v 8.3:
    # 225
    # 227
  #];

  propagatedBuildInputs = with python3Packages; [
    argcomplete dateutil configobj feedparser bibtexparser pyyaml requests six beautifulsoup4
  ];

  doCheck = false;
  checkInputs = with pkgs; [ git python3Packages.pyfakefs python3Packages.mock python3Packages.ddt ];
  #checkInputs = with python3Packages; [ pyfakefs mock ddt];

  meta = with stdenv.lib; {
    description = "Command-line bibliography manager";
    homepage = "https://github.com/pubs/pubs";
    license = licenses.lgpl3;
    maintainers = with maintainers; [ gebner ];
  };
}
