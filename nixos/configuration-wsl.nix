{
  allowUnfree = true;

  # Installed Packages
  packageOverrides = pkgs: with pkgs; rec {
    anish-pubs = callPackage ./pkgs/pubs.nix;
    myProfile = writeText "my-profile" ''
      export PATH=$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/sbin:/bin:/usr/sbin:/usr/bin
      export MANPATH=$HOME/.nix-profile/share/man:/nix/var/nix/profiles/default/share/man:/usr/share/man
    '';
    myPackages = pkgs.buildEnv {
      name = "anish-packages";
      paths = [
        (runCommand "profile" {} ''
          mkdir -p $out/etc/profile.d
          cp ${myProfile} $out/etc/profile.d/my-profile.sh
        '')
        # Version Control / Archive
        git subversion
        unzip zip unrar

        # Debug / Monitor / Analysis
        htop iotop powertop iftop
        ltrace strace
        pciutils usbutils lshw
        smartmontools lm_sensors

        # Volume
        pavucontrol

        # Networking
        iputils
        #nmap wireshark
        #tor openvpn
        wget curl rsync unison
        #networkmanager networkmanager-openvpn

        # Linux shell utils
        zsh
        fzf silver-searcher
        neofetch
        tree
        tmux screen

        # Encryption
        gnupg
        pinentry-gtk2

        # CLI programs
        ranger
        weechat
        vim neovim emacs26-nox
        w3m
        #youtube-dl
        taskwarrior timewarrior
        khal khard # calendars and contacts
        vdirsyncer # vdirsyncer
        #fswebcam 
        neomutt
        anish-pubs # biblography manager
        pass # password manager


        # Dev Tools
        gnumake
        gcc ccls
        (python3.withPackages # installing python3 with packages
        (ps: with ps; [
          pylint jedi
          numpy scipy matplotlib
          #jupyter notebook
        ])
        )
        python3Packages.pip
        python3Packages.argcomplete
        pypi2nix

        haskellPackages.ghc
        haskellPackages.hoogle
        haskellPackages.hlint
        haskellPackages.cabal-install
        cabal2nix
        # all-hies.latest

        openjdk nodejs

        # Latex
        #texlive.combined.scheme-full

        # Man Pages
        man man-pages
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc" ];
      extraOutputsToInstall = [ "man" "doc" ];
    };
  };
}
