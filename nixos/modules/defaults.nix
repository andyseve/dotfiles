{ config, pkgs, ... }:

{
  # firmware settings
  # additional firmware drivers
  boot.kernelModules = [ "iwlwifi" ];
  # iwlwifi settings
  # needed to stop multiple physical restarts.
  boot.extraModprobeConfig = ''
  options iwlwifi 11n_disable=8
  '';
  # logitech
  hardware.logitech.wireless.enable=true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi = {
    canTouchEfiVariables = true;
  };

  boot.plymouth.enable = true;

  # location
  location.provider = "geoclue2";

  # Select internationalisation properties
  console = {
    keyMap = "us";
    font = "Lat2-Terminus16";
  };
  i18n.defaultLocale = "en_US.UTF-8";

  fonts = {
    fonts = with pkgs; [
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      fira-code
      cascadia-code
      (nerdfonts.override { fonts = [ "FiraCode" "FiraMono" "CascadiaCode" ]; })
      lohit-fonts.marathi
    ];
  };

  # Allow non-free drivers
  hardware.enableRedistributableFirmware = true;

  # Nix configuration
  nix = {
    # build related settings
    settings = {
      auto-optimise-store = true;
      require-sigs = true;
      experimental-features = ["nix-command" "flakes"];
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    # Garbage Collector
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

    # NixPkgs Configuration
    nixpkgs.config.allowUnfree = true;

    nixpkgs.overlays = import ../overlays;

  # environment variables
  environment.sessionVariables = rec {
    XDG_CACHE_HOME  = "\${HOME}/.cache";
    XDG_CONFIG_HOME = "\${HOME}/.config";
    XDG_BIN_HOME    = "\${HOME}/.local/bin";
    XDG_DATA_HOME   = "\${HOME}/.local/share";

    # ZDOTDIR
    ZDOTDIR         = "\${XDG_CONFIG_HOME}/zsh";

    # PATH
    PATH = [
      "\${XDG_BIN_HOME}"
    ];
  };

  environment.enableDebugInfo = true;
  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    # Drivers
    firmwareLinuxNonfree


    # Version Control / Archive
    git 
    unzip zip unrar

    # Debug / Monitor / Analysis
    htop iotop powertop iftop
    ltrace strace
    pciutils usbutils lshw
    smartmontools lm_sensors
    dmidecode

    # Volume
    alsa-utils

    # Networking
    iputils
    tor openvpn
    wget curl rsync
    networkmanager networkmanager-openvpn

    # Linux shell utils
    fzf silver-searcher autojump ripgrep # for grep and search
    neofetch
    tree
    tmux screen
    pdftk
    btop

    # Encryption
    gnupg
    pinentry-gtk2

    # CLI programs
    ranger
    weechat
    unstable.neovim # load latest neovim
    bat #better cat
    taskwarrior timewarrior
    khal khard # calendars and contacts
    unstable.vdirsyncer # vdirsyncer
    fswebcam 
    pubs # biblography manager
    (pass.withExtensions
      (exts: [
        exts.pass-otp 
      ])
    ) # password manager
    unstable.yt-dlp # youtube downloader
    unstable.ytmdl # youtube music downloader and info
    imagemagick
    inkscape


    # Dev Tools
    gnumake
    gcc ccls
    rustc

    python3
    python3Packages.pip
    unstable.python3Packages.argcomplete
    python3Packages.numpy
    python3Packages.scipy
    python3Packages.matplotlib
    python3Packages.scikitlearn
    python3Packages.pandas
    python3Packages.jupyter
    python3Packages.notebook

    unstable.haskellPackages.ghc
    unstable.haskellPackages.hoogle
    unstable.haskellPackages.cabal-install
    unstable.haskellPackages.haskell-language-server

    jdk11 nodejs

    nodePackages.pyright
    sumneko-lua-language-server
    rust-analyzer


    # Latex
    texlive.combined.scheme-full

    # Man Pages
    man man-pages

    # dotfiles
    anish-dotfiles
  ];

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [
    gutenprint
    hplip
    hplipWithPlugin
  ];

  # hardware support
  hardware.bluetooth.enable = true;
    # 32 bit support, required for steam -- check this
    hardware.opengl.driSupport32Bit = true;

    programs = {
    # zsh
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    # backlight
    light.enable = true;
    # gnupg
    gnupg.agent = {
      enable = true;
      enableBrowserSocket = true;
      enableSSHSupport = true;
      pinentryFlavor = "gtk2";
    };
  };
}
