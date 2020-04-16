#!/run/current-system/sw/bin/bash
sed -i '$d' ~/.taskrc
tw_path=$(nix-build "<nixpkgs>" -A taskwarrior)
echo "include $tw_path/share/doc/task/rc/solarized-dark-256.theme" >> ~/.taskrc
